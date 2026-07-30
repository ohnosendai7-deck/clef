;;;; mark.lisp — host-modeled object space, SATB marking, ephemerons,
;;;; finalizers.  Layered on the Immix layout in heap.lisp.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; Implemented clean-room from: Yuasa's snapshot-at-the-beginning deletion
;;;; barrier (Yuasa 1990), Immix mark-region collection (Blackburn & McKinley
;;;; 2008), and the ephemeron semantics in Hayes 1997 ("Ephemerons: a new
;;;; finalization mechanism").  No collector source was consulted.
;;;;
;;;; ------------------------------------------------------------------
;;;; EFFECT-DISCIPLINE NOTE (for the eventual solver, DESIGN §6.5)
;;;; ------------------------------------------------------------------
;;;; This is host-model code: the type/effect solver cannot check it yet,
;;;; so the intended effect rows are recorded as DOCUMENTED DECLARATIONS in
;;;; each docstring ("Effects: ...").  The alloc-freedom argument the solver
;;;; will eventually prove for the target translation of this file:
;;;;
;;;;   * All collector metadata (object table, mark bits, cards, mark stack,
;;;;     forwarding table) lives in side tables fixed at heap creation and
;;;;     filled monotonically.  The mark/sweep phases never request fresh
;;;;     metadata storage: mutating a fixed side table requires no
;;;;     allocation, and the mark stack can only grow to the object
;;;;     capacity, which is reserved up front.
;;;;   * Mutator entry points (ALLOC-OBJECT, MUTATE) are outside the
;;;;     collector's effect row; on target they may allocate, which is why
;;;;     the collector itself is written against side tables only.
;;;;   * In this host model the side tables are Lisp vectors/hash tables
;;;;     with reserved capacity; the step functions below mutate them in
;;;;     place.  The no-fresh-metadata invariant still holds: marking and
;;;;     sweeping never create a table, they only flip bits and slot values.

(in-package #:clef/gc)

;;;; ---------------------------------------------------------------
;;;; Generation and object liveness encoding (side-table values).
;;;; ---------------------------------------------------------------

(defconstant +gen-young+ 0)
(defconstant +gen-old+ 1)

;;; The object table maps object id -> liveness entry:
;;;   NIL            slot is free (id may be recycled)
;;;   :dead          occupied but not yet proven live this cycle (white)
;;;   :live          proven live / allocated-black (black or grey)
;;;   (:forward . n) evacuated; n is the new object id
;;; Liveness is thus never stored in objects (DESIGN §6.3: metadata is
;;; out-of-band so conses stay headerless).

;;;; ---------------------------------------------------------------
;;;; The modeled heap.
;;;; ---------------------------------------------------------------

(defstruct (gc-heap (:constructor %make-gc-heap))
  "Host-modeled object space over the Immix region/block/line metadata.
Effects: constructor allocates the fixed side tables; every other operation
on a GC-HEAP mutates those tables in place."
  ;; --- object space ---
  (capacity 0 :type fixnum)         ; max live objects; fixed at creation
  (sizes (make-array 0 :element-type 'fixnum)
         :type (simple-array fixnum (*)))
  (slots (make-array 0) :type simple-vector) ; per-id vector of slot values
  (nslots (make-array 0 :element-type 'fixnum)
          :type (simple-array fixnum (*)))
  (objs (make-array 0 :initial-element :dead)) ; id -> liveness (see above)
  (gens (make-array 0 :element-type 'fixnum))  ; id -> +GEN-YOUNG+/+GEN-OLD+
  (free-ids '() :type list)         ; recycled ids (pre-sized at creation)
  (next-id 0 :type fixnum)          ; fresh ids, monotonic; total <= capacity
  (id-gen 0 :type fixnum)           ; generation counter mixed into hashes
  (live-count 0 :type fixnum)
  (roots '() :type list)            ; root set: list of object ids
  ;; --- Immix layout metadata (heap.lisp structs) ---
  (region (make-region-meta) :type region-meta) ; one region is enough here
  (block-table (make-hash-table) :type hash-table) ; block idx -> block-meta
  (free-blocks '() :type list)      ; region free list (block indices)
  (reusable-blocks '() :type list)  ; occupied blocks with free line runs
  ;; --- mark side tables ---
  (obj-block (make-array 0 :element-type 'fixnum)) ; id -> block index
  (obj-line (make-array 0 :element-type 'fixnum))  ; id -> first line in block
  (marks (make-array 0 :element-type 'bit) :type (simple-array bit (*)))
  (mark-stack '() :type list)       ; grey ids; bounded by capacity
  (ephemerons '() :type list)       ; list of GC-EPHEMERON
  (finalizers (make-hash-table) :type hash-table) ; id -> finalizer tag
  (pending-finalizers '() :type list) ; ids whose finalizer is due
  ;; --- generational remembered set ---
  (generational nil :type t)        ; mode flag
  (cards (make-hash-table) :type hash-table) ; block idx -> bit vector
  ;; --- evacuation side tables ---
  (forwards (make-hash-table) :type hash-table) ; old id -> new id
  (collection-set '() :type list)   ; block indices chosen for evacuation
  (evac-worklist '() :type list)    ; ids still to copy this evac pause
  (evac-copies '() :type list)      ; new ids produced by this evac pause
  (pre-evac-live '() :type list)    ; live set snapshot taken at evac start
  (remap-record '() :type list)     ; (old . new) alist from the last remap,
                                    ; kept until the next MARK-BEGIN purely so
                                    ; the model harness can check graph
                                    ; preservation; the collector never reads it
  ;; --- sweep / remap cursors (incremental phases) ---
  (sweep-queue '() :type list)      ; block indices still to sweep
  (remap-worklist '() :type list)   ; ids whose slots still need remapping
  ;; --- model-harness snapshots (SATB snapshot set) ---
  (snapshot '() :type list))        ; root-reachable ids at MARK-BEGIN time

(defstruct (gc-ephemeron (:constructor %make-gc-ephemeron))
  "An ephemeron (Hayes 1997): VALUE is retained iff KEY is live at the end
of marking.  KEY and VALUE are object ids or immediates (non-ids)."
  key
  value)

(defun card-vector (heap block)
  "The dirty-card bit vector for BLOCK, creating the fixed-size vector on
first use.  One bit per line (card = line in this model).
Effects: none (alloc-free) once every block's vector exists; on target these
live in the block's side table, allocated with the block."
  (or (gethash block (gc-heap-cards heap))
      (setf (gethash block (gc-heap-cards heap))
            (make-array +lines-per-block+ :element-type 'bit
                                          :initial-element 0))))

(defun make-heap (&key capacity (generational t))
  "Create a modeled heap holding at most CAPACITY objects, laid out over one
Immix region.  All side tables are sized here; collection phases never grow
them.
Effects: allocates (heap setup is outside the collector's effect row)."
  (check-type capacity (integer 1))
  (let* ((nblocks (max 2 (ceiling capacity 4)))
         (heap (%make-gc-heap
                :capacity capacity
                :sizes (make-array capacity :element-type 'fixnum
                                            :initial-element 0)
                :slots (make-array capacity :initial-element nil)
                :nslots (make-array capacity :element-type 'fixnum
                                             :initial-element 0)
                :objs (make-array capacity :initial-element nil)
                :gens (make-array capacity :element-type 'fixnum
                                           :initial-element +gen-young+)
                :obj-block (make-array capacity :element-type 'fixnum
                                                :initial-element -1)
                :obj-line (make-array capacity :element-type 'fixnum
                                               :initial-element -1)
                :marks (make-array capacity :element-type 'bit
                                            :initial-element 0)
                :generational generational
                :free-blocks (loop for i from 1 below nblocks collect i))))
    (setf (gc-heap-free-ids heap)
          (loop for i downfrom (1- capacity) to 0 collect i))
    ;; Every block's metadata side table exists from the start (on target
    ;; they live in the region's fixed side-table area, so the collector
    ;; never allocates metadata mid-cycle).  Block 0 is the active
    ;; allocation block; the rest are on the region free list.
    (dotimes (b nblocks)
      (setf (gethash b (gc-heap-block-table heap))
            (make-block-meta :size-class 0)))
    heap))

;;;; ---------------------------------------------------------------
;;;; Object space helpers.
;;;; ---------------------------------------------------------------

(declaim (inline object-live-p obj-forwarded-p))

(defun object-live-p (heap id)
  "True iff ID is allocated and proven live (or allocated-black).
Effects: none (alloc-free)."
  (eq (aref (gc-heap-objs heap) id) :live))

(defun object-allocated-p (heap id)
  "True iff ID names an allocated object (live, white, or forwarded).
Effects: none (alloc-free)."
  (not (null (aref (gc-heap-objs heap) id))))

(defun obj-forwarded-p (heap id)
  "True iff ID has been evacuated; the forwarding side table holds the copy.
Effects: none (alloc-free)."
  (consp (aref (gc-heap-objs heap) id)))

(defun heap-full-p (heap)
  (declare (gc-heap heap))
  (>= (gc-heap-live-count heap) (gc-heap-capacity heap)))

(defun lines-needed (size)
  "Whole lines occupied by an object of SIZE bytes.
Effects: none (alloc-free)."
  (max 1 (ceiling size +line-size+)))

(defun %free-block-line-runs (bmeta)
  "List of (start . length) maximal runs of free lines in BMETA.
Effects: none (alloc-free) on target — computed by scanning the line table;
the host model may cons the transient list outside collection pauses."
  (let ((marks (lt-marks (bm-lines bmeta)))
        (runs '())
        (start nil))
    (dotimes (i +lines-per-block+)
      (if (zerop (aref marks i))
          (unless start (setf start i))
          (when start (push (cons start (- i start)) runs)
                (setf start nil))))
    (when start (push (cons start (- +lines-per-block+ start)) runs))
    (nreverse runs)))

(defun %block-find-run (heap size)
  "Find (values block-index line-index) with a free run of lines for SIZE,
trying the active block, then partially-free reusable blocks, then blocks on
the region free list.  Blocks in the collection set are never targets (an
evacuation destination must outlive the pause).  NIL if none.
Effects: none (alloc-free)."
  (labels ((candidate-p (b)
             (and (not (member b (gc-heap-collection-set heap)))
                  (gethash b (gc-heap-block-table heap))))
           (fits (bmeta)
             (find-if (lambda (run) (>= (cdr run) (lines-needed size)))
                      (%free-block-line-runs bmeta))))
    (dolist (b (cons 0 (append (gc-heap-reusable-blocks heap)
                               (gc-heap-free-blocks heap))))
      (when (candidate-p b)
        (let ((run (fits (gethash b (gc-heap-block-table heap)))))
          (when run (return (values b (car run)))))))))

(defun %occupy-lines (bmeta line n)
  "Mark N lines starting at LINE occupied in BMETA's line table.
Effects: none (alloc-free) — side-table bit flips only."
  (dotimes (k n)
    (setf (aref (lt-marks (bm-lines bmeta)) (+ line k)) 1))
  (setf (bm-next-free-line bmeta) (max (bm-next-free-line bmeta) (+ line n))))

;;;; ---------------------------------------------------------------
;;;; Allocation and the mutator API.
;;;; ---------------------------------------------------------------

(defun alloc-object (heap size nslots &key (generation +gen-young+))
  "Allocate an object of SIZE bytes with NSLOTS reference slots, in
GENERATION.  Returns the new object id, or NIL when the heap is full.
Allocated-black (DESIGN §6.4): the new object is marked live for the current
cycle; floating garbage is reclaimed next cycle.
Effects: ALLOC (mutator-side).  Consumes pre-reserved ids and lines only."
  (declare (gc-heap heap))
  (when (heap-full-p heap)
    (return-from alloc-object nil))
  (multiple-value-bind (block line) (%block-find-run heap size)
    (unless block
      (return-from alloc-object nil))
    (let ((id (pop (gc-heap-free-ids heap))))
      (unless id
        (return-from alloc-object nil))
      (setf (aref (gc-heap-sizes heap) id) size
            (aref (gc-heap-nslots heap) id) nslots
            (aref (gc-heap-slots heap) id) (make-array nslots
                                                       :initial-element nil)
            (aref (gc-heap-gens heap) id) generation
            (aref (gc-heap-objs heap) id) :live   ; allocated-black
            (aref (gc-heap-marks heap) id) 1
            (aref (gc-heap-obj-block heap) id) block
            (aref (gc-heap-obj-line heap) id) line)
      (%occupy-lines (gethash block (gc-heap-block-table heap))
                     line (lines-needed size))
      ;; A block receiving its first object leaves the region free list;
      ;; any block with an object is a reusable allocation source while it
      ;; still has free runs (the sweep refreshes this bookkeeping too).
      (setf (gc-heap-free-blocks heap)
            (remove block (gc-heap-free-blocks heap)))
      (pushnew block (gc-heap-reusable-blocks heap))
      (incf (gc-heap-live-count heap))
      id)))

(defun set-roots (heap ids)
  "Replace the root set with IDS.  Roots must name allocated objects.
Effects: ALLOC (mutator-side, off the collector path)."
  (dolist (id ids)
    (unless (object-allocated-p heap id)
      (error "set-roots: ~s is not an allocated object" id)))
  (setf (gc-heap-roots heap) (copy-list ids))
  heap)

(defun add-root (heap id)
  "Add ID to the root set.
Effects: ALLOC (mutator-side)."
  (unless (object-allocated-p heap id)
    (error "add-root: ~s is not an allocated object" id))
  (pushnew id (gc-heap-roots heap))
  heap)

(defun remove-root (heap id)
  "Drop ID from the root set.
Effects: none (alloc-free)."
  (setf (gc-heap-roots heap) (remove id (gc-heap-roots heap)))
  heap)

;;;; ---------------------------------------------------------------
;;;; The SATB write barrier (Yuasa 1990) and the card barrier.
;;;; ---------------------------------------------------------------

(defun write-barrier (heap obj slot old-value new-value)
  "Pre-write SATB deletion barrier: log OLD-VALUE (the reference about to be
overwritten in OBJ's SLOT) by pushing it on the mark stack, preserving the
snapshot-at-the-beginning.  Also the generational card barrier: when
generational mode is on, OBJ is old, and NEW-VALUE is a young object, set
the dirty card for OBJ's block.
Effects: none (alloc-free) — the mark stack is bounded by heap capacity and
reserved at heap creation; the card table is a fixed side table."
  ;; SLOT is not needed on host: the card index comes from the object's
  ;; recorded home line.  On target it selects the card from the store
  ;; address.
  (declare (ignore slot))
  (when (and (integerp old-value)
             (object-allocated-p heap old-value))
    (push old-value (gc-heap-mark-stack heap)))
  (when (and (gc-heap-generational heap)
             (eql (aref (gc-heap-gens heap) obj) +gen-old+)
             (integerp new-value)
             (object-allocated-p heap new-value)
             (eql (aref (gc-heap-gens heap) new-value) +gen-young+))
    (let* ((block (aref (gc-heap-obj-block heap) obj))
           (cards (card-vector heap block)))
      ;; Dirty the card covering the object's first line (one card per line
      ;; in this model; on target the card comes from the store address).
      (setf (aref cards (%obj-line heap obj)) 1)))
  heap)

(defun %obj-line (heap id)
  "First line of ID's home within its block, recorded at allocation time.
On target this is computed from the object address ((addr - block-base) /
+line-size+, an alloc-free shift); the host model keeps it in a side table.
Effects: none (alloc-free)."
  (aref (gc-heap-obj-line heap) id))

(defun mutate (heap obj slot new-value)
  "Mutator store: run the SATB/card write barrier, then store NEW-VALUE into
OBJ's SLOT.  NEW-VALUE is an object id or an immediate (NIL, fixnum-as-data
is modeled by non-id values; ids are the only references).
Effects: ALLOC (mutator-side barrier log is pre-reserved; on target the
barrier itself is alloc-free)."
  (let ((old (aref (aref (gc-heap-slots heap) obj) slot)))
    (write-barrier heap obj slot old new-value)
    (setf (aref (aref (gc-heap-slots heap) obj) slot) new-value))
  heap)

;;; NOTE on %OBJ-LINE: on target the card index is computed from the
;;; object's address ((addr - block-base) / +line-size+, an alloc-free
;;; shift).  The host model records each object's first line in the
;;; OBJ-LINE side table at allocation time, which plays the same role:
;;; presence/absence of the dirty card is what the remembered set needs.

;;;; ---------------------------------------------------------------
;;;; Marking.
;;;; ---------------------------------------------------------------

(defun mark-begin (heap)
  "Start a mark cycle: whiten everything (clear mark bits), reset the mark
stack, and grey the snapshot-at-the-beginning sources: the roots and (in
generational mode) the old objects sitting under dirty cards.  Objects
allocated after this point are allocated-black (ALLOC-OBJECT marks them);
objects allocated before it are white unless the snapshot reaches them.
Effects: none (alloc-free)."
  (let ((marks (gc-heap-marks heap)))
    (dotimes (id (gc-heap-capacity heap))
      (setf (aref marks id) 0))
    (setf (gc-heap-mark-stack heap) '())
    ;; Roots seed the snapshot.
    (dolist (id (gc-heap-roots heap))
      (when (object-allocated-p heap id)
        (push id (gc-heap-mark-stack heap))))
    ;; Generational: dirty cards rescan old->young edges.
    (when (gc-heap-generational heap)
      (%push-card-objects heap))
    heap))

(defun %push-card-objects (heap)
  "Push every old object sitting in a block with a dirty card.
Effects: none (alloc-free)."
  (maphash
   (lambda (block cards)
     (unless (zerop (reduce #'+ cards))
       (dotimes (id (gc-heap-capacity heap))
         (when (and (object-allocated-p heap id)
                    (eql (aref (gc-heap-obj-block heap) id) block)
                    (eql (aref (gc-heap-gens heap) id) +gen-old+))
           (push id (gc-heap-mark-stack heap))))))
   (gc-heap-cards heap))
  heap)

(defun %mark-trace-id (heap id)
  "Grey ID if it is a white allocated object.
Effects: none (alloc-free)."
  (when (and (integerp id)
             (object-allocated-p heap id)
             (not (obj-forwarded-p heap id))
             (zerop (aref (gc-heap-marks heap) id)))
    (setf (aref (gc-heap-marks heap) id) 1)
    (push id (gc-heap-mark-stack heap)))
  heap)

(defun mark-step (heap &key (work-budget 1))
  "Process up to WORK-BUDGET mark-stack entries: pop a grey object, mark it
black, grey every white object it references.  Returns (values done-p
work-done); done-p is true when the stack drained (possibly before the
budget ran out).
Effects: none (alloc-free) — fixed side tables mutated in place."
  (declare (gc-heap heap) ((integer 0) work-budget))
  (let ((done 0))
    (loop while (and (< done work-budget)
                     (gc-heap-mark-stack heap))
          do (let ((id (pop (gc-heap-mark-stack heap))))
               (when (object-allocated-p heap id)
                 (setf (aref (gc-heap-objs heap) id) :live
                       (aref (gc-heap-marks heap) id) 1)
                 (let ((slots (aref (gc-heap-slots heap) id)))
                   (dotimes (s (length slots))
                     (%mark-trace-id heap (aref slots s)))))
               (incf done)))
    (values (null (gc-heap-mark-stack heap)) done)))

(defun %ephemeron-step (heap)
  "One pass over the ephemeron list: for each ephemeron whose key is marked
(grey or black — a grey key will end the cycle live) and whose value is a
white object, grey the value.  Returns true iff some value was greyed
(another pass is needed).  The key's *mark bit* is consulted, never a stale
allocated-black flag: liveness this cycle is exactly what the bits say.
Effects: none (alloc-free)."
  (let ((progress nil))
    (dolist (eph (gc-heap-ephemerons heap))
      (let ((k (gc-ephemeron-key eph))
            (v (gc-ephemeron-value eph)))
        (when (and (integerp k) (object-allocated-p heap k)
                   (= 1 (aref (gc-heap-marks heap) k))
                   (integerp v) (object-allocated-p heap v)
                   (not (obj-forwarded-p heap v))
                   (zerop (aref (gc-heap-marks heap) v)))
          (setf (aref (gc-heap-marks heap) v) 1)
          (push v (gc-heap-mark-stack heap))
          (setf progress t))))
    progress))

(defun final-mark-pause (heap)
  "The STW epilogue of a mark cycle:
  1. Fold the remembered set into the mark: cards dirtied *during* the
     cycle (mutator stores into old objects while the marker ran) name
     grey sources, exactly like cards dirty at MARK-BEGIN.
  2. Process ephemerons to fixpoint (Hayes 1997): a value is live iff its
     key is live at the end of marking; greying a value can expose more
     keys, so iterate, draining the stack between passes.
  3. Finalizer deferral: every white object with a registered finalizer is
     enqueued on the heap's pending-finalizer queue exactly once (the
     registration is consumed), marked, and *traced* — the finalizer may
     touch anything the object reaches, so its whole subgraph survives
     this cycle.  Next cycle it is an ordinary object.
  4. Clear dirty cards (the remembered set was folded in by step 1).
Effects: none (alloc-free)."
  ;; 1. Mid-cycle dirty cards are grey sources too.
  (when (gc-heap-generational heap)
    (%push-card-objects heap))
  (loop until (mark-step heap :work-budget (gc-heap-capacity heap)))
  ;; 2. Ephemeron fixpoint interleaved with stack drains.
  (loop while (%ephemeron-step heap)
        do (loop until (mark-step heap :work-budget (gc-heap-capacity heap))))
  (loop until (mark-step heap :work-budget (gc-heap-capacity heap)))
  ;; 3. Finalizer deferral: enqueue once, keep the subgraph one cycle.
  (let ((resurrect '()))
    (maphash (lambda (id tag)
               (declare (ignore tag))
               (when (and (object-allocated-p heap id)
                          (not (obj-forwarded-p heap id))
                          (zerop (aref (gc-heap-marks heap) id)))
                 (push id resurrect)))
             (gc-heap-finalizers heap))
    (dolist (id resurrect)
      (remhash id (gc-heap-finalizers heap))
      (pushnew id (gc-heap-pending-finalizers heap))
      (setf (aref (gc-heap-marks heap) id) 1
            (aref (gc-heap-objs heap) id) :live)
      (push id (gc-heap-mark-stack heap)))
    (loop until (mark-step heap :work-budget (gc-heap-capacity heap))))
  ;; 4. Cards consumed for this cycle.
  (maphash (lambda (block cards)
             (declare (ignore block))
             (fill cards 0))
           (gc-heap-cards heap))
  heap)

(defun mark-cycle (heap)
  "Convenience: run a whole mark cycle to completion (MARK-BEGIN, drain,
FINAL-MARK-PAUSE).  Used by unit tests; the model harness uses the
incremental steps.
Effects: none (alloc-free)."
  (mark-begin heap)
  (loop until (mark-step heap :work-budget (gc-heap-capacity heap)))
  (final-mark-pause heap)
  heap)

;;;; ---------------------------------------------------------------
;;;; Ephemeron / finalizer registration.
;;;; ---------------------------------------------------------------

(defun make-ephemeron (heap key value)
  "Register an ephemeron with KEY and VALUE (object ids or immediates).
Returns the ephemeron.
Effects: ALLOC (mutator-side; on target ephemeron tables are weak-side
tables grown outside GC pauses)."
  (let ((eph (%make-gc-ephemeron :key key :value value)))
    (push eph (gc-heap-ephemerons heap))
    eph))

(defun register-finalizer (heap id tag)
  "Register finalizer TAG for object ID.  When ID becomes unreachable it is
kept alive one extra cycle and enqueued on the heap's pending-finalizers.
Effects: ALLOC (mutator-side)."
  (unless (object-allocated-p heap id)
    (error "register-finalizer: ~s is not allocated" id))
  (setf (gethash id (gc-heap-finalizers heap)) tag)
  heap)

(defun pop-pending-finalizer (heap)
  "Pop the next id whose finalizer is due, or NIL.
Effects: none (alloc-free)."
  (pop (gc-heap-pending-finalizers heap)))

;;;; ---------------------------------------------------------------
;;;; Graph queries used by sweep/evac/model and tests.
;;;; ---------------------------------------------------------------

(defun reachable-set (heap &key (roots (gc-heap-roots heap)))
  "Set of object ids reachable from ROOTS, as a list.  Follows forwarding
pointers so the query is meaningful mid-evacuation.  Pure query.
Effects: ALLOC (transient host list; a query, not collector code)."
  (let ((seen (make-hash-table))
        (stack (copy-list roots))
        (out '()))
    (loop while stack
          do (let ((id (pop stack)))
               (when (integerp id)
                 (let ((entry (aref (gc-heap-objs heap) id)))
                   (cond ((consp entry)        ; forwarded: jump to the copy
                          (setf id (cdr entry)
                                entry (aref (gc-heap-objs heap) id)))
                         ((null entry) (setf id nil))))
                 (when (and id (not (gethash id seen)))
                   (setf (gethash id seen) t)
                   (push id out)
                   (let ((slots (aref (gc-heap-slots heap) id)))
                     (dotimes (s (length slots))
                       (push (aref slots s) stack)))))))
    out))

(defun %resolve-forward (heap id)
  "Follow ID's forwarding pointer, if any.  Returns the current id.
Effects: none (alloc-free)."
  (let ((entry (and (integerp id) (aref (gc-heap-objs heap) id))))
    (if (consp entry) (cdr entry) id)))

(defun marked-live-ids (heap)
  "All allocated ids with the mark bit set.
Effects: ALLOC (transient host list; a query)."
  (loop for id below (gc-heap-capacity heap)
        when (and (object-allocated-p heap id)
                  (not (obj-forwarded-p heap id))
                  (= 1 (aref (gc-heap-marks heap) id)))
        collect id))

(defun record-snapshot (heap)
  "Record the SATB snapshot set (root-reachable ids) in the heap's SNAPSHOT
slot.  The collector never consults this — it is the verification aid the
model harness checks the SATB invariant against.  Call immediately before
MARK-BEGIN.
Effects: ALLOC (host-only verification aid; not collector code)."
  (setf (gc-heap-snapshot heap) (reachable-set heap))
  heap)
