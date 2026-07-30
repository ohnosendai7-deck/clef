;;;; model.lisp — deterministic-interleaving model harness for the GC.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; CHESS-style systematic exploration (Musuvathi & Qadeer, "CHESS:
;;;; systematic stress testing of concurrent software", 2007): the
;;;; collector is a pure step function over an explicit state, the mutator
;;;; is a fixed program, and the harness explores, by DFS with a
;;;; visited-state table, every interleaving in which each mutator op is
;;;; preceded by 0, 1, or 2 collector steps.  The SATB/Yuasa barrier model
;;;; follows Yuasa 1990; the heap follows Immix (Blackburn & McKinley
;;;; 2008).  No collector or model-checker source was consulted.
;;;;
;;;; ------------------------------------------------------------------
;;;; EFFECT-DISCIPLINE NOTE (for the eventual solver, DESIGN §6.5)
;;;; ------------------------------------------------------------------
;;;; This file is verification tooling, not collector code: it copies
;;;; heaps, builds canonical state keys, and walks reachability sets.
;;;; Every function here is in the ALLOC row on target and none of it
;;;; runs inside a collection pause.  The *checked* code (mark, sweep,
;;;; evac) carries the alloc-freedom obligations; this file only observes.
;;;;
;;;; Model conventions
;;;; -----------------
;;;;   * The only immediate the harness stores is NIL; any integer in a
;;;;     slot is claimed to be an object reference, so dangling-reference
;;;;     checks may treat any non-allocated integer slot value as a bug.
;;;;   * Roots are a 3-deep register file: each :ALLOC pushes its object
;;;;     and drops the oldest root.  This is what creates garbage.
;;;;   * :STORE operands are interpreted modulo the live/reachable sets,
;;;;     so a fixed program stays meaningful as the heap changes.  Stored
;;;;     values are drawn from objects reachable *now* — a real mutator
;;;;     can only name what it can reach (registers are roots).
;;;;   * Purity: GC-STEP and STEP-MUTATOR copy the state and mutate the
;;;;     copy, so branching exploration never has to undo.

(in-package #:clef/gc)

;;;; ---------------------------------------------------------------
;;;; State and deep copy.
;;;; ---------------------------------------------------------------

(defstruct (model-state (:constructor %make-model-state))
  "One explicit model state: the heap, the collector PHASE
(:idle :marking :evac :remap :sweep), the mutator PROGRAM and program
counter PC, PENDING-ROOTS (root updates staged by the current op, folded
into the heap root set before the op completes), the EVENTS produced by
the step that created this state (:remap-complete, :cycle-complete), the
completed-cycle count, and whether the mutator runs the write barrier."
  heap (phase :idle) program (pc 0) (pending-roots '()) (events '())
  (cycles 0) (barrier t))

(defun %copy-hash (h &optional (value-copier #'identity))
  "Shallow copy of hash table H, mapping VALUE-COPIER over values.
Effects: ALLOC (verification tooling)."
  (let ((out (make-hash-table :test (hash-table-test h))))
    (maphash (lambda (k v) (setf (gethash k out) (funcall value-copier v))) h)
    out))

(defun deep-copy-heap (h)
  "Deep-enough copy of a modeled heap: every side table the collector can
mutate is copied so branches never share mutable structure.  Ephemeron
structs themselves are immutable after registration and shared.  (Named
DEEP-COPY-HEAP so as not to clobber DEFSTRUCT's own copier.)
Effects: ALLOC (verification tooling)."
  (let ((c (%make-gc-heap
            :capacity (gc-heap-capacity h)
            :sizes (copy-seq (gc-heap-sizes h))
            :slots (map 'vector (lambda (v) (and v (copy-seq v)))
                        (gc-heap-slots h))
            :nslots (copy-seq (gc-heap-nslots h))
            :objs (copy-seq (gc-heap-objs h))
            :gens (copy-seq (gc-heap-gens h))
            :free-ids (copy-list (gc-heap-free-ids h))
            :next-id (gc-heap-next-id h)
            :id-gen (gc-heap-id-gen h)
            :live-count (gc-heap-live-count h)
            :roots (copy-list (gc-heap-roots h))
            :region (make-region-meta
                     :base (rm-base (gc-heap-region h))
                     :blocks (copy-seq (rm-blocks (gc-heap-region h))))
            :block-table (%copy-hash
                          (gc-heap-block-table h)
                          (lambda (b)
                            (make-block-meta
                             :size-class (bm-size-class b)
                             :next-free-line (bm-next-free-line b)
                             :lines (make-line-table
                                     :marks (copy-seq
                                             (lt-marks (bm-lines b)))))))
            :obj-block (copy-seq (gc-heap-obj-block h))
            :obj-line (copy-seq (gc-heap-obj-line h))
            :free-blocks (copy-list (gc-heap-free-blocks h))
            :reusable-blocks (copy-list (gc-heap-reusable-blocks h))
            :marks (copy-seq (gc-heap-marks h))
            :mark-stack (copy-list (gc-heap-mark-stack h))
            :ephemerons (copy-list (gc-heap-ephemerons h))
            :finalizers (%copy-hash (gc-heap-finalizers h))
            :pending-finalizers (copy-list (gc-heap-pending-finalizers h))
            :generational (gc-heap-generational h)
            :cards (%copy-hash (gc-heap-cards h) #'copy-seq)
            :forwards (%copy-hash (gc-heap-forwards h))
            :collection-set (copy-list (gc-heap-collection-set h))
            :evac-worklist (copy-list (gc-heap-evac-worklist h))
            :evac-copies (copy-list (gc-heap-evac-copies h))
            :pre-evac-live (copy-list (gc-heap-pre-evac-live h))
            :remap-record (copy-tree (gc-heap-remap-record h))
            :sweep-queue (copy-list (gc-heap-sweep-queue h))
            :remap-worklist (copy-list (gc-heap-remap-worklist h))
            :snapshot (copy-list (gc-heap-snapshot h)))))
    c))

(defun fresh-state (s)
  "A branchable copy of state S (deep heap copy).  The copy's EVENTS list
is empty: each step reports only the events it produces itself.
Effects: ALLOC (verification tooling)."
  (%make-model-state
   :heap (deep-copy-heap (model-state-heap s))
   :phase (model-state-phase s)
   :program (model-state-program s)
   :pc (model-state-pc s)
   :pending-roots (copy-list (model-state-pending-roots s))
   :events '()
   :cycles (model-state-cycles s)
   :barrier (model-state-barrier s)))

;;;; ---------------------------------------------------------------
;;;; The collector as a pure step function.
;;;; ---------------------------------------------------------------

(defun %gc-step! (s)
  "Destructive single collector micro-step on state S.  One step is one
bounded slice of collector work (one mark-stack entry, one copied object,
one remapped object, one swept block) or one phase transition.
Effects: those of the phase it steps — see mark/sweep/evac."
  (let ((heap (model-state-heap s)))
    (ecase (model-state-phase s)
      (:idle
       ;; A new cycle: snapshot for the harness, then begin.  The remap
       ;; record from the previous cycle is stale the moment a new
       ;; snapshot exists.
       (setf (gc-heap-remap-record heap) '())
       (record-snapshot heap)
       (mark-begin heap)
       (setf (model-state-phase s) :marking))
      (:marking
       (when (mark-step heap :work-budget 1)
         (final-mark-pause heap)
         (if (select-collection-set heap :block-budget 2)
             (setf (model-state-phase s) :evac)
             (progn (sweep-start heap)
                    (setf (model-state-phase s) :sweep)))))
      (:evac
       (when (evac-step heap :budget 1)
         (remap-start heap)
         (setf (model-state-phase s) :remap)))
      (:remap
       (when (remap-step heap :budget 1)
         (push :remap-complete (model-state-events s))
         (sweep-start heap)
         (setf (model-state-phase s) :sweep)))
      (:sweep
       (when (sweep-step heap :block-budget 1)
         (push :cycle-complete (model-state-events s))
         (incf (model-state-cycles s))
         (setf (model-state-phase s) :idle))))
    s))

(defun gc-step (s)
  "The collector as a pure step function: S -> S', running one bounded
slice of collector work.  S itself is unchanged (the copy does the work),
which is what makes branching exploration possible.
Effects: ALLOC (verification tooling; the checked steps inside are the
alloc-free ones)."
  (%gc-step! (fresh-state s)))

;;;; ---------------------------------------------------------------
;;;; The mutator.
;;;; ---------------------------------------------------------------

(defun raw-store (heap obj slot new-value)
  "Store NEW-VALUE into OBJ's SLOT with NO write barrier.  This is the
*broken* mutator, present only so the negative test can demonstrate that
the harness catches the lost-object bug the SATB barrier prevents.  Never
used on a real mutator path.
Effects: none (alloc-free)."
  (setf (aref (aref (gc-heap-slots heap) obj) slot) new-value)
  heap)

(defun %live-ids (heap)
  "Sorted ids of all allocated, non-forwarding-stub objects.
Effects: ALLOC (verification tooling)."
  (loop for id below (gc-heap-capacity heap)
        when (and (object-allocated-p heap id)
                  (not (obj-forwarded-p heap id)))
        collect id))

(defun %apply-store (s a b c)
  "Interpret (:STORE A B C) against state S: object = A-th live object,
slot = B mod its slot count, value = C-th object reachable now (or the NIL
immediate).  With the barrier flag on, the store goes through MUTATE;
otherwise through the deliberately broken RAW-STORE.
Effects: those of MUTATE (barrier on) or none (barrier off)."
  (let* ((heap (model-state-heap s))
         (live (%live-ids heap)))
    (when live
      (let* ((obj (nth (mod a (length live)) live))
             (n (aref (gc-heap-nslots heap) obj))
             (slot (mod b (max 1 n)))
             (reach (reachable-set heap))
             (val (if (or (null reach) (zerop (mod c 4)))
                      nil
                      (nth (mod c (length reach)) reach))))
        (if (model-state-barrier s)
            (mutate heap obj slot val)
            (raw-store heap obj slot val)))))
  s)

(defun %apply-alloc (s size)
  "Interpret (:ALLOC SIZE): allocate, then stage the new object as a root
through PENDING-ROOTS and fold the root file down to its 3-slot window.
Effects: ALLOC (mutator-side)."
  (let* ((heap (model-state-heap s))
         (nslots (max 1 (min 4 (truncate size 32))))
         (id (alloc-object heap size nslots)))
    (when id
      (push id (model-state-pending-roots s))
      (setf (gc-heap-roots heap)
            (subseq (append (reverse (model-state-pending-roots s))
                            (gc-heap-roots heap))
                    0 (min 3 (+ (length (model-state-pending-roots s))
                                (length (gc-heap-roots heap))))))
      (setf (model-state-pending-roots s) '())))
  s)

(defun step-mutator (s op)
  "Pure mutator step: apply one mutator op to a copy of S.
Effects: ALLOC (verification tooling around mutator-row operations)."
  (let ((s2 (fresh-state s)))
    (ecase (car op)
      (:alloc (%apply-alloc s2 (cadr op)))
      (:store (%apply-store s2 (cadr op) (caddr op) (cadddr op)))
      (:gc))                            ; handled by the explorer itself
    s2))

;;;; ---------------------------------------------------------------
;;;; Invariants.
;;;; ---------------------------------------------------------------

(defun %raw-reachable-ids (heap)
  "All ids reachable from roots, following forwarding pointers but *not*
filtering out freed ids — so a dangling reference shows up here as an
unallocated id, which reachable-set would silently drop.
Effects: ALLOC (verification tooling)."
  (let ((seen (make-hash-table))
        (stack (copy-list (gc-heap-roots heap)))
        (out '()))
    (loop while stack
          do (let ((id (pop stack)))
               (when (and (integerp id) (<= 0 id)
                          (< id (gc-heap-capacity heap)))
                 (when (and (object-allocated-p heap id)
                            (obj-forwarded-p heap id))
                   (setf id (%resolve-forward heap id)))
                 (unless (gethash id seen)
                   (setf (gethash id seen) t)
                   (push id out)
                   (let ((slots (aref (gc-heap-slots heap) id)))
                     (when (vectorp slots)
                       (dotimes (i (length slots))
                         (push (aref slots i) stack))))))))
    out))

(defun %card-source-ids (heap)
  "Old objects homed in blocks with any dirty card — the remembered-set
grey sources.
Effects: ALLOC (verification tooling)."
  (let (out)
    (dolist (b (sort (loop for k being the hash-keys of (gc-heap-cards heap)
                           collect k)
                     #'<))
      (let ((cards (gethash b (gc-heap-cards heap))))
        (when (and cards (find 1 cards))
          (dolist (id (%block-object-ids heap b))
            (when (and (not (obj-forwarded-p heap id))
                       (eql (aref (gc-heap-gens heap) id) +gen-old+))
              (push id out))))))
    out))

(defun check-satb-invariant (s)
  "Invariant (a), checked while a mark cycle is active: every white object
that is root-reachable now or was live at the snapshot must be reachable
from the grey frontier (the mark stack plus remembered-set sources)
through white/grey objects only — i.e. it is on the mark stack or covered
via a dirty card, never stranded behind a black object.  Returns a list of
violation descriptions (empty when the invariant holds).
Effects: ALLOC (verification tooling)."
  (unless (eq (model-state-phase s) :marking)
    (return-from check-satb-invariant '()))
  (let* ((heap (model-state-heap s))
         (sources (append (copy-list (gc-heap-mark-stack heap))
                          (%card-source-ids heap)))
         (seen (make-hash-table))
         (stack (copy-list sources)))
    ;; Close over the grey frontier without crossing black objects.
    (loop while stack
          do (let ((x (pop stack)))
               (when (and (integerp x) (<= 0 x)
                          (< x (gc-heap-capacity heap))
                          (object-allocated-p heap x)
                          (not (obj-forwarded-p heap x))
                          (not (gethash x seen)))
                 (setf (gethash x seen) t)
                 (when (or (zerop (aref (gc-heap-marks heap) x))
                           (member x sources))
                   (let ((slots (aref (gc-heap-slots heap) x)))
                     (dotimes (i (length slots))
                       (push (aref slots i) stack)))))))
    ;; Every required white object must be covered.
    (let ((reachable (reachable-set heap))
          (violations '()))
      (dotimes (id (gc-heap-capacity heap))
        (when (and (object-allocated-p heap id)
                   (not (obj-forwarded-p heap id))
                   (zerop (aref (gc-heap-marks heap) id))
                   (or (member id reachable)
                       (member id (gc-heap-snapshot heap)))
                   (not (gethash id seen)))
          (push (format nil "SATB: white object ~a stranded (reachable-now ~
~a, snapshot ~a, not on mark stack or under a dirty card)"
                        id (and (member id reachable) t)
                        (and (member id (gc-heap-snapshot heap)) t))
                violations)))
      violations)))

(defun check-cycle-complete (s)
  "Invariant (b), checked when a mark+sweep cycle completes: the whole SATB
snapshot survived (modulo evacuation forwarding) and nothing reachable
from roots was swept or left as a dangling stub.
Effects: ALLOC (verification tooling)."
  (unless (member :cycle-complete (model-state-events s))
    (return-from check-cycle-complete '()))
  (let ((heap (model-state-heap s))
        (violations '()))
    (dolist (id (gc-heap-snapshot heap))
      (let ((rid (or (cdr (assoc id (gc-heap-remap-record heap))) id)))
        (unless (and (object-allocated-p heap rid)
                     (not (obj-forwarded-p heap rid)))
          (push (format nil "CYCLE: snapshot object ~a (resolved ~a) did ~
not survive the cycle" id rid)
                violations))))
    (dolist (id (%raw-reachable-ids heap))
      (unless (and (object-allocated-p heap id)
                   (not (obj-forwarded-p heap id)))
        (push (format nil "CYCLE: root-reachable object ~a was swept or ~
left dangling" id)
              violations)))
    violations))

(defun check-remap-complete (s)
  "Invariants (c) and (d), checked when the remap pause completes:
(c) the post-remap reachability closure from the roots is exactly the
    pre-evacuation live set (mapped through forwarding), modulo objects
    allocated during the pause;
(d) no slot or root names a freed or forwarding-stub object, and the
    forwarding table is empty.
Effects: ALLOC (verification tooling)."
  (unless (member :remap-complete (model-state-events s))
    (return-from check-remap-complete '()))
  (let* ((heap (model-state-heap s))
         (violations '())
         (map (gc-heap-remap-record heap))
         (expected (sort (mapcar (lambda (id) (or (cdr (assoc id map)) id))
                                 (gc-heap-pre-evac-live heap))
                         #'<))
         (actual (reachable-set heap))
         ;; The pre-evac universe = mapped live set; anything else in the
         ;; closure must have been allocated during the pause.
         (pre-evac-universe (append expected (gc-heap-pre-evac-live heap)))
         (filtered (sort (remove-if-not
                          (lambda (id) (member id pre-evac-universe))
                          actual)
                         #'<)))
    (unless (equal expected filtered)
      (push (format nil "EVAC: graph not preserved; expected live set ~a, ~
post-remap closure ~a" expected filtered)
            violations))
    (unless (zerop (hash-table-count (gc-heap-forwards heap)))
      (push "EVAC: forwarding table not empty after remap" violations))
    (dolist (id (gc-heap-roots heap))
      (unless (and (object-allocated-p heap id)
                   (not (obj-forwarded-p heap id)))
        (push (format nil "EVAC: root ~a dangling after remap" id)
              violations)))
    (dotimes (id (gc-heap-capacity heap))
      (when (and (object-allocated-p heap id)
                 (not (obj-forwarded-p heap id)))
        (let ((slots (aref (gc-heap-slots heap) id)))
          (dotimes (i (length slots))
            (let ((v (aref slots i)))
              (when (and (integerp v)
                         (or (not (<= 0 v (1- (gc-heap-capacity heap))))
                             (not (object-allocated-p heap v))
                             (obj-forwarded-p heap v)))
                (push (format nil "EVAC: slot ~a[~a] = ~a dangling after ~
remap" id i v)
                      violations)))))))
    violations))

(defun check-state (s)
  "All invariant violations that apply to state S, as a list of strings.
Effects: ALLOC (verification tooling)."
  (append (check-satb-invariant s)
          (check-cycle-complete s)
          (check-remap-complete s)))

;;;; ---------------------------------------------------------------
;;;; Canonical state keys for the visited table.
;;;; ---------------------------------------------------------------

(defun %bits->int (seq &optional (base 2))
  "Pack a byte/bit sequence into an integer, MSB first.
Effects: ALLOC (verification tooling)."
  (reduce (lambda (acc b) (+ (* base acc) b)) seq :initial-value 0))

(defun state-key (s)
  "A canonical sexp capturing everything that can influence the future of
state S: phase, pc, roots (ordered), every object's liveness/generation/
mark/slots/home, all collector side tables (order-sensitive lists kept in
order, tables sorted), and cursors.  Used with an EQUAL hash table — exact
state identity, no hash-collision pruning.
Effects: ALLOC (verification tooling)."
  (let* ((heap (model-state-heap s))
         (cap (gc-heap-capacity heap)))
    (list (model-state-phase s)
          (model-state-pc s)
          (copy-list (gc-heap-roots heap))
          (loop for id below cap
                for entry = (aref (gc-heap-objs heap) id)
                when entry
                collect (list id
                              (if (consp entry) (list :fwd (cdr entry)) entry)
                              (aref (gc-heap-gens heap) id)
                              (aref (gc-heap-marks heap) id)
                              (aref (gc-heap-sizes heap) id)
                              (aref (gc-heap-obj-block heap) id)
                              (aref (gc-heap-obj-line heap) id)
                              (coerce (aref (gc-heap-slots heap) id) 'list)))
          (copy-list (gc-heap-mark-stack heap))
          (sort (loop for b being the hash-keys of (gc-heap-cards heap)
                      using (hash-value cv)
                      collect (cons b (%bits->int cv)))
                #'< :key #'car)
          (sort (loop for o being the hash-keys of (gc-heap-forwards heap)
                      using (hash-value n)
                      collect (cons o n))
                #'< :key #'car)
          (sort (loop for id being the hash-keys of (gc-heap-finalizers heap)
                      collect id)
                #'<)
          (copy-list (gc-heap-pending-finalizers heap))
          (copy-list (gc-heap-free-ids heap))
          (copy-list (gc-heap-free-blocks heap))
          (copy-list (gc-heap-reusable-blocks heap))
          (sort (loop for b being the hash-keys of (gc-heap-block-table heap)
                      using (hash-value bm)
                      collect (list b (bm-size-class bm)
                                    (bm-next-free-line bm)
                                    (%bits->int (lt-marks (bm-lines bm)) 4)))
                #'< :key #'car)
          (copy-list (gc-heap-collection-set heap))
          (copy-list (gc-heap-evac-worklist heap))
          (copy-list (gc-heap-evac-copies heap))
          (copy-list (gc-heap-sweep-queue heap))
          (copy-list (gc-heap-remap-worklist heap))
          (gc-heap-live-count heap)
          (gc-heap-next-id heap))))

;;;; ---------------------------------------------------------------
;;;; The explorer.
;;;; ---------------------------------------------------------------

(defun generate-program (n seed)
  "A deterministic pseudo-random mutator program of N ops (LCG; the same
seed always yields the same program).  Ops: (:ALLOC size) with size in
16..128, (:STORE a b c) with small operands, (:GC k) with k in 1..3.
Effects: ALLOC (verification tooling)."
  (let ((x (logior 1 (logand seed #x7fffffff))))
    (flet ((rnd (m)
             (setf x (logand #x7fffffff (+ (* x 1103515245) 12345)))
             (mod (ash x -10) m)))
      (loop repeat n
            for r = (rnd 10)
            collect (cond ((< r 4) (list :alloc (* 16 (1+ (rnd 8)))) )
                          ((< r 8) (list :store (rnd 8) (rnd 4) (rnd 8)))
                          (t (list :gc (1+ (rnd 3)))))))))

(defun run-model (&key (max-objects 8) (max-ops 4) (barrier t) programs)
  "Systematically explore every interleaving of the mutator program(s)
with the incremental collector in which each ALLOC/STORE op is preceded
by 0, 1, or 2 collector steps, on a heap of capacity MAX-OBJECTS, for
programs of MAX-OPS ops (three deterministic programs, seeds 3/5/9 —
chosen because they reach every collector phase, dirty cards, and
evacuate — unless PROGRAMS is given).  Every state produced is checked
graph-preservation, and no-dangling-forwarding invariants.  Terminal
states (program exhausted) are driven collector-only back to :IDLE.
Returns (values states-explored violation-count violations).
Effects: ALLOC (verification tooling)."
  (let* ((programs (or programs
                       (list (generate-program max-ops 3)
                             (generate-program max-ops 5)
                             (generate-program max-ops 9))))
         (visited (make-hash-table :test 'equal))
         (explored 0)
         (violations '()))
    (labels ((note (s)
               (incf explored)
               (dolist (v (check-state s))
                 (push v violations)))
             (gc (s)
               (let ((s2 (gc-step s)))
                 (note s2)
                 s2))
             (mutate-op (s op)
               (let ((s2 (step-mutator s op)))
                 (note s2)
                 s2))
             (advance (s) (incf (model-state-pc s)) s)
             (drive-to-idle (s)
               (loop for guard below (* 16 (+ 2 max-objects))
                     until (eq (model-state-phase s) :idle)
                     do (setf s (gc s))))
             (rec (s)
               (let ((key (state-key s)))
                 (when (gethash key visited)
                   (return-from rec nil))
                 (setf (gethash key visited) t))
               (let ((program (model-state-program s))
                     (pc (model-state-pc s)))
                 (if (>= pc (length program))
                     (drive-to-idle s)
                     (let ((op (nth pc program)))
                       (if (eq (car op) :gc)
                           (let ((s2 (if (plusp (cadr op))
                                         s
                                         (fresh-state s))))
                             (dotimes (i (cadr op))
                               (setf s2 (gc s2)))
                             (rec (advance s2)))
                           (dolist (k '(0 1 2))
                             (let ((s2 s))
                               (dotimes (i k)
                                 (setf s2 (gc s2)))
                               (setf s2 (mutate-op s2 op))
                               (rec (advance s2))))))))))
      (dolist (program programs)
        (let ((s0 (%make-model-state
                   :heap (make-heap :capacity max-objects :generational t)
                   :phase :idle
                   :program program
                   :pc 0
                   :barrier barrier)))
          (note s0)
          (rec s0))))
    (values explored (length violations) (nreverse violations))))
