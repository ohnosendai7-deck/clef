;;;; test-gc-model.lisp — tests for the host-modeled GC: SATB barrier,
;;;; cards, marking, ephemerons, finalizers, sweep, evacuation, and the
;;;; deterministic-interleaving model harness.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.

(in-package #:clef-test)

;;; Shorthands for the (unexported, by convention) gc internals — the
;;; existing GC tests use clef/gc:: directly; we do the same.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %gs (name) (intern (string name) :clef/gc)))

(defun %heap (capacity &key (generational nil))
  (funcall (%gs :make-heap) :capacity capacity :generational generational))

(defun %alloc (heap size nslots &key (generation 0))
  (funcall (%gs :alloc-object) heap size nslots :generation generation))

(defun %slots (heap id)
  (aref (funcall (%gs :gc-heap-slots) heap) id))

(defun %marked-p (heap id)
  (= 1 (aref (funcall (%gs :gc-heap-marks) heap) id)))

(defun %allocated-p (heap id)
  (funcall (%gs :object-allocated-p) heap id))

(defun %drive-to-idle (s &key (guard 500))
  "GC-STEP the model state S until the collector is :IDLE again,
accumulating every invariant violation seen on the way."
  (let ((violations '()))
    (loop repeat guard
          until (eq (funcall (%gs :model-state-phase) s) :idle)
          do (setf s (funcall (%gs :gc-step) s))
             (setf violations
                   (append violations (funcall (%gs :check-state) s))))
    (values s violations)))

(defun %scripted-state (heap &key (barrier t))
  (funcall (%gs :%make-model-state)
           :heap heap :phase :idle :barrier barrier))

;;;; ---------------------------------------------------------------
;;;; Write barrier and card table.
;;;; ---------------------------------------------------------------

(deftest gc-model-barrier-logs-old-value
  (let* ((h (%heap 8))
         (a (%alloc h 16 1))
         (x (%alloc h 16 0)))
    (funcall (%gs :set-roots) h (list a))
    (funcall (%gs :mutate) h a 0 x)
    ;; Overwriting NIL logged nothing.
    (is (null (funcall (%gs :gc-heap-mark-stack) h)))
    ;; Overwriting X must log X (Yuasa deletion barrier).
    (funcall (%gs :mutate) h a 0 nil)
    (is (equal (funcall (%gs :gc-heap-mark-stack) h) (list x)))))

(deftest gc-model-card-dirty-old-to-young
  (let* ((h (%heap 8 :generational t))
         (o (%alloc h 16 1 :generation 1))
         (y (%alloc h 16 0))
         (y2 (%alloc h 16 1)))
    (funcall (%gs :mutate) h o 0 y)
    (let* ((ob (aref (funcall (%gs :gc-heap-obj-block) h) o))
           (oline (aref (funcall (%gs :gc-heap-obj-line) h) o))
           (cards (funcall (%gs :card-vector) h ob)))
      (is (= 1 (aref cards oline)) "old->young store must dirty the card"))
    ;; young -> young store must not dirty anything new.
    (let* ((b2 (aref (funcall (%gs :gc-heap-obj-block) h) y2))
           (l2 (aref (funcall (%gs :gc-heap-obj-line) h) y2))
           (cards2 (funcall (%gs :card-vector) h b2)))
      (funcall (%gs :mutate) h y2 0 y)
      (is (zerop (aref cards2 l2)) "young->young store must not dirty"))))

(deftest gc-model-generational-card-marks-young
  ;; A young object stored into an already-scanned old object mid-cycle is
  ;; found via the dirty card at the final mark pause.
  (let* ((h (%heap 8 :generational t))
         (o (%alloc h 16 1 :generation 1))
         (y (%alloc h 16 0)))
    (funcall (%gs :set-roots) h (list o))
    (funcall (%gs :mark-begin) h)
    ;; Drain: O is scanned while its slot is still NIL.
    (loop until (funcall (%gs :mark-step) h :work-budget 8))
    (is (not (%marked-p h y)))
    ;; Mutator installs the old->young edge *after* O was scanned.
    (funcall (%gs :mutate) h o 0 y)
    (funcall (%gs :final-mark-pause) h)
    (is (%marked-p h y) "dirty card must rescue the young object")
    ;; Cards are consumed by the pause.
    (maphash (lambda (b cv)
               (declare (ignore b))
               (is (zerop (reduce #'+ cv)) "cards cleared after pause"))
             (funcall (%gs :gc-heap-cards) h))))

;;;; ---------------------------------------------------------------
;;;; Marking.
;;;; ---------------------------------------------------------------

(deftest gc-model-mark-graph
  ;; Cycle A<->B, shared C (A.s1=B.s1=C), self-loop C.s0=C, dead D.
  (let* ((h (%heap 16))
         (a (%alloc h 16 2))
         (b (%alloc h 16 2))
         (c (%alloc h 16 1))
         (d (%alloc h 16 0)))
    (funcall (%gs :mutate) h a 0 b)
    (funcall (%gs :mutate) h b 0 a)
    (funcall (%gs :mutate) h a 1 c)
    (funcall (%gs :mutate) h b 1 c)
    (funcall (%gs :mutate) h c 0 c)
    (funcall (%gs :set-roots) h (list a))
    (funcall (%gs :mark-cycle) h)
    (is (%marked-p h a))
    (is (%marked-p h b))
    (is (%marked-p h c))
    (is (not (%marked-p h d)) "dead object must stay white")
    (is (equal (funcall (%gs :marked-live-ids) h)
               (sort (list a b c) #'<)))))

(deftest gc-model-mark-incremental
  ;; Stepping with a work budget reaches the same fixpoint as MARK-CYCLE.
  (let* ((h (%heap 16))
         (a (%alloc h 16 2))
         (b (%alloc h 16 2))
         (c (%alloc h 16 1)))
    (funcall (%gs :mutate) h a 0 b)
    (funcall (%gs :mutate) h b 0 a)
    (funcall (%gs :mutate) h a 1 c)
    (funcall (%gs :mutate) h b 1 c)
    (funcall (%gs :mutate) h c 0 c)
    (funcall (%gs :set-roots) h (list a))
    (funcall (%gs :mark-begin) h)
    (let ((total 0) (steps 0))
      (loop (multiple-value-bind (d w)
                (funcall (%gs :mark-step) h :work-budget 1)
              (incf total w)
              (incf steps)
              (is (< steps 100) "marking must terminate")
              (when d (return))))
      (is (> total 0) "mark steps did work"))
    (funcall (%gs :final-mark-pause) h)
    (is (equal (funcall (%gs :marked-live-ids) h)
               (sort (list a b c) #'<)))))

;;;; ---------------------------------------------------------------
;;;; Ephemerons and finalizers.
;;;; ---------------------------------------------------------------

(deftest gc-model-ephemerons
  (let* ((h (%heap 16))
         (r (%alloc h 16 1))
         (k (%alloc h 16 0))
         (v (%alloc h 16 1))
         (w (%alloc h 16 0))
         (dk (%alloc h 16 0))
         (dv (%alloc h 16 0)))
    (funcall (%gs :mutate) h r 0 k)
    (funcall (%gs :mutate) h v 0 w)
    (funcall (%gs :set-roots) h (list r))
    (funcall (%gs :make-ephemeron) h k v)   ; live key -> value survives
    (funcall (%gs :make-ephemeron) h dk dv) ; dead key -> value does not
    (funcall (%gs :mark-cycle) h)
    (is (%marked-p h v) "ephemeron value live iff key live")
    (is (%marked-p h w) "ephemeron value's subgraph survives")
    (is (not (%marked-p h dv)) "dead key must not retain its value")))

(deftest gc-model-ephemeron-chain
  ;; E1(K1,K2) E2(K2,V): marking K1 must propagate through the fixpoint.
  (let* ((h (%heap 16))
         (r (%alloc h 16 1))
         (k1 (%alloc h 16 0))
         (k2 (%alloc h 16 0))
         (v (%alloc h 16 0)))
    (funcall (%gs :mutate) h r 0 k1)
    (funcall (%gs :set-roots) h (list r))
    (funcall (%gs :make-ephemeron) h k1 k2)
    (funcall (%gs :make-ephemeron) h k2 v)
    (funcall (%gs :mark-cycle) h)
    (is (%marked-p h k2) "chained ephemeron key")
    (is (%marked-p h v) "chained ephemeron value")))

(deftest gc-model-finalizer-deferral
  (let* ((h (%heap 8))
         (f (%alloc h 16 1))
         (g (%alloc h 16 0)))
    (funcall (%gs :mutate) h f 0 g)
    (funcall (%gs :register-finalizer) h f 'fin)
    (funcall (%gs :set-roots) h '())
    ;; Cycle 1: F is unreachable but finalizable -> deferred, subgraph kept.
    (funcall (%gs :mark-cycle) h)
    (is (member f (funcall (%gs :gc-heap-pending-finalizers) h)))
    (is (%marked-p h f) "finalizable object kept alive one cycle")
    (is (%marked-p h g) "finalizable object's subgraph kept alive")
    (is (zerop (hash-table-count (funcall (%gs :gc-heap-finalizers) h)))
        "finalizer registration is consumed (runs once)")
    (funcall (%gs :sweep-all) h)
    (is (%allocated-p h f))
    (is (eql (funcall (%gs :pop-pending-finalizer) h) f))
    ;; Cycle 2: unregistered and unreachable -> reclaimed.
    (funcall (%gs :mark-cycle) h)
    (funcall (%gs :sweep-all) h)
    (is (not (%allocated-p h f)) "deferred object reclaimed next cycle")
    (is (not (%allocated-p h g)))))

;;;; ---------------------------------------------------------------
;;;; Sweep.
;;;; ---------------------------------------------------------------

(deftest gc-model-sweep-reclaims
  (let* ((h (%heap 8))
         (r (%alloc h 16 1))
         (g1 (%alloc h 16 0))
         (g2 (%alloc h 16 0)))
    (funcall (%gs :mutate) h r 0 g1) ; then dropped -> garbage
    (funcall (%gs :mutate) h r 0 nil)
    (funcall (%gs :set-roots) h (list r))
    (funcall (%gs :mark-cycle) h)
    (funcall (%gs :sweep-all) h)
    (is (%allocated-p h r))
    (is (not (%allocated-p h g1)) "unreachable object swept")
    (is (not (%allocated-p h g2)))
    (is (= 1 (funcall (%gs :gc-heap-live-count) h)))
    ;; R's block still has free runs -> reusable (size-class availability).
    (is (member (aref (funcall (%gs :gc-heap-obj-block) h) r)
                (funcall (%gs :gc-heap-reusable-blocks) h)))))

(deftest gc-model-sweep-returns-free-blocks
  (let* ((h (%heap 4))                  ; 2 blocks; all objects in block 0
         (a (%alloc h 16 0))
         (b (%alloc h 16 0)))
    (declare (ignore a b))
    (funcall (%gs :set-roots) h '())
    (funcall (%gs :mark-cycle) h)
    (funcall (%gs :sweep-all) h)
    ;; The occupied block became all-free and returned to the region
    ;; (metadata side tables persist; the free list is the occupancy truth).
    (is (member 0 (funcall (%gs :gc-heap-free-blocks) h))
        "all-free block returned to the region free list")
    (is (member 1 (funcall (%gs :gc-heap-free-blocks) h)))
    ;; Allocation still works afterwards.
    (is (integerp (%alloc h 16 0)))))

;;;; ---------------------------------------------------------------
;;;; Evacuation.
;;;; ---------------------------------------------------------------

(deftest gc-model-evac-graph-preserved
  (let* ((h (%heap 8))
         (a (%alloc h 16 1))
         (b (%alloc h 16 1))
         (c (%alloc h 16 0))
         (d (%alloc h 16 0)))           ; dead
    (funcall (%gs :mutate) h a 0 b)
    (funcall (%gs :mutate) h b 0 c)
    (funcall (%gs :set-roots) h (list a))
    (funcall (%gs :mark-cycle) h)
    ;; Selection: the one occupied block is nearly empty -> worst score.
    (let ((cset (funcall (%gs :select-collection-set) h :block-budget 1)))
      (is (= 1 (length cset)))
      (is (= (aref (funcall (%gs :gc-heap-obj-block) h) a) (car cset))))
    ;; Evacuate to completion.
    (loop until (funcall (%gs :evac-step) h :budget 8))
    (is (funcall (%gs :obj-forwarded-p) h a) "forwarding pointer installed")
    (is (funcall (%gs :obj-forwarded-p) h b))
    (is (funcall (%gs :obj-forwarded-p) h c))
    (is (not (funcall (%gs :obj-forwarded-p) h d)) "dead object not copied")
    (funcall (%gs :remap-start) h)
    (loop until (funcall (%gs :remap-step) h :budget 8))
    (let* ((map (funcall (%gs :gc-heap-remap-record) h))
           (a2 (cdr (assoc a map)))
           (b2 (cdr (assoc b map)))
           (c2 (cdr (assoc c map))))
      (is a2) (is b2) (is c2)
      (is (equal (funcall (%gs :gc-heap-roots) h) (list a2))
          "roots re-pointed at the copies")
      (is (eql (aref (%slots h a2) 0) b2) "interior reference remapped")
      (is (eql (aref (%slots h b2) 0) c2))
      ;; Graph preservation: closure from roots == mapped pre-evac live set.
      (is (equal (sort (copy-list (funcall (%gs :reachable-set) h)) #'<)
                 (sort (list a2 b2 c2) #'<)))
      ;; No dangling forwarding pointers.
      (is (zerop (hash-table-count (funcall (%gs :gc-heap-forwards) h))))
      (is (not (%allocated-p h a)) "old ids retired after remap")
      (is (not (%allocated-p h b))))
    ;; Sweep now reclaims the evacuated block wholesale (and the dead D).
    (funcall (%gs :sweep-all) h)
    (is (not (%allocated-p h d)))
    (is (= 3 (funcall (%gs :gc-heap-live-count) h)))))

;;;; ---------------------------------------------------------------
;;;; The model harness.
;;;; ---------------------------------------------------------------

(deftest gc-model-no-violations
  (multiple-value-bind (explored nviolations violations)
      (funcall (%gs :run-model) :max-objects 8 :max-ops 4)
    (is (> explored 50) "model must explore a nontrivial state space")
    (is (zerop nviolations)
        (format nil "~d invariant violations, first: ~a"
                nviolations (car violations)))))

(deftest gc-model-no-violations-deep
  ;; A wider heap and longer programs: reaches every collector phase,
  ;; dirties cards (old->young stores), and evacuates, across several
  ;; collection cycles.
  (multiple-value-bind (explored nviolations violations)
      (funcall (%gs :run-model) :max-objects 12 :max-ops 6)
    (is (> explored 400) "deep model must explore a large state space")
    (is (zerop nviolations)
        (format nil "~d invariant violations, first: ~a"
                nviolations (car violations)))))

(deftest gc-model-negative-no-barrier
  ;; The classic lost-object race the SATB deletion barrier prevents:
  ;; roots = (A C), A.s0 = X.  Mark begins; C is scanned; then the mutator
  ;; deletes A.s0 (X's only snapshot edge) and inserts X under the already
  ;; black C.  Without the barrier X is lost; with it, X is logged.
  (flet ((script (barrier-p)
           (let* ((h (%heap 8))
                  (a (%alloc h 16 1))
                  (x (%alloc h 16 0))
                  (c (%alloc h 16 1)))
             (funcall (%gs :mutate) h a 0 x)
             (funcall (%gs :set-roots) h (list a c))
             (let ((s (%scripted-state h :barrier barrier-p))
                   (violations '()))
               ;; :idle -> :marking (snapshot + grey the roots)
               (setf s (funcall (%gs :gc-step) s))
               ;; scan C (roots are pushed in order, C pops first)
               (setf s (funcall (%gs :gc-step) s))
               (is (eq (funcall (%gs :model-state-phase) s) :marking))
               ;; The racy mutator: delete A.s0, insert X under black C.
               (let ((heap (funcall (%gs :model-state-heap) s)))
                 (if barrier-p
                     (progn (funcall (%gs :mutate) heap a 0 nil)
                            (funcall (%gs :mutate) heap c 0 x))
                     (progn (funcall (%gs :raw-store) heap a 0 nil)
                            (funcall (%gs :raw-store) heap c 0 x))))
               (setf violations
                     (append violations (funcall (%gs :check-state) s)))
               ;; Finish the cycle, collecting violations.
               (multiple-value-bind (sf more) (%drive-to-idle s)
                 (declare (ignore sf))
                 (setf violations (append violations more)))
               violations))))
    (let ((with-barrier (script t))
          (without-barrier (script nil)))
      (is (null with-barrier)
          (format nil "barriered mutator must be clean, got: ~a"
                  (car with-barrier)))
      (is (not (null without-barrier))
          "barrier-less mutator must lose the object (harness catches it)"))))
