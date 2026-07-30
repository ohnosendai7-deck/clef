;;;; evac.lisp — bounded-STW evacuation for the host-modeled heap.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; Implemented clean-room from Immix (Blackburn & McKinley 2008, §4:
;;;; opportunistic evacuation of fragmented blocks) with side-table
;;;; forwarding pointers.  No collector source was consulted.
;;;;
;;;; ------------------------------------------------------------------
;;;; EFFECT-DISCIPLINE NOTE (for the eventual solver, DESIGN §6.5)
;;;; ------------------------------------------------------------------
;;;; Host-model code; the effect rows below are DOCUMENTED DECLARATIONS,
;;;; not solver-checked judgments.  The alloc-freedom argument the solver
;;;; will eventually prove for the target translation:
;;;;
;;;;   * Evacuation allocates *object* space for copies via the ordinary
;;;;     allocator (ALLOC-OBJECT's row), but no collector metadata: the
;;;;     forwarding table, collection set, and worklists are fixed side
;;;;     tables mutated in place, bounded by the heap capacity reserved
;;;;     at creation.
;;;;   * Selection scores are computed by scanning the line tables and
;;;;     mark bits — reads only.
;;;;   * Remapping rewrites slots in place; it neither allocates metadata
;;;;     nor moves objects.
;;;;
;;;; Ordering contract (driven by the model harness / future mutator
;;;; protocol): SELECT-COLLECTION-SET runs right after the final mark
;;;; pause; EVAC-STEP copies until the worklist drains; REMAP-START +
;;;; REMAP-STEP fix references; only then does the sweep run.  Forwarding
;;;; stubs keep REACHABLE-SET meaningful throughout.

(in-package #:clef/gc)

;;;; ---------------------------------------------------------------
;;;; Collection-set selection (fragmentation score).
;;;; ---------------------------------------------------------------

(defun %block-live-lines (heap block)
  "Lines in BLOCK covered by marked (surviving) objects.  Computed from the
mark bits, not the line table: the line table is pre-sweep occupancy and
still counts the dead.  This is Immix's post-mark line availability.
Effects: none (alloc-free)."
  (let ((live 0))
    (dolist (id (%block-object-ids heap block))
      (when (and (not (obj-forwarded-p heap id))
                 (= 1 (aref (gc-heap-marks heap) id)))
        (incf live (lines-needed (aref (gc-heap-sizes heap) id)))))
    live))

(defun select-collection-set (heap &key (block-budget 2))
  "Choose up to BLOCK-BUDGET blocks for evacuation: score every occupied
block by fragmentation = free-lines/total-lines (descending), where
free-lines counts lines not covered by *marked* objects; skip blocks with
no survivors (the sweep reclaims those wholesale) and blocks with no free
lines (nothing to gain).  Record the pre-evacuation live set (for the
harness's graph-preservation check) and build the copy worklist.
Returns the chosen block indices, worst first.
Effects: ALLOC (selection runs once per cycle, outside mutator pauses; the
lists are bounded by the block count and heap capacity)."
  (let* ((scored
          (loop for b in (occupied-blocks heap)
                for live = (%block-live-lines heap b)
                for free = (- +lines-per-block+ live)
                when (and (> live 0) (> free 0))
                collect (cons b free)))
         (worst (sort scored (lambda (x y)
                               (or (> (cdr x) (cdr y))
                                   (and (= (cdr x) (cdr y))
                                        (< (car x) (car y)))))))
         (chosen (subseq worst 0 (min block-budget (length worst))))
         (blocks (mapcar #'car chosen)))
    (setf (gc-heap-collection-set heap) blocks
          (gc-heap-pre-evac-live heap) (sort (copy-list (marked-live-ids heap)) #'<)
          (gc-heap-evac-copies heap) '()
          (gc-heap-evac-worklist heap)
          (loop for b in blocks append (%block-object-ids heap b)))
    ;; Only marked, non-stub objects get copied; the worklist may name
    ;; white objects (already dead) which EVAC-STEP skips.
    blocks))

;;;; ---------------------------------------------------------------
;;;; Evacuation (copying) — bounded steps.
;;;; ---------------------------------------------------------------

(defun evac-step (heap &key (budget 1))
  "Copy up to BUDGET live objects out of collection-set blocks.  Each copy
is allocated by the ordinary allocator (which never targets the collection
set), receives the same size, generation, and slot values, and is recorded
in the forwarding side table (old id -> new id); the old id's liveness
entry becomes a forwarding stub.  Roots are re-pointed immediately (this
is the bounded STW pause).  Objects that cannot be copied (destination
space exhausted) are simply left behind — the sweep keeps their block.
Returns (values done-p copies-made).
Effects: ALLOC (object space for copies only; metadata is side tables)."
  (declare (gc-heap heap) ((integer 0) budget))
  (let ((done 0))
    (loop while (and (< done budget) (gc-heap-evac-worklist heap))
          do (let ((old (pop (gc-heap-evac-worklist heap))))
               (when (and (object-allocated-p heap old)
                          (not (obj-forwarded-p heap old))
                          (= 1 (aref (gc-heap-marks heap) old)))
                 (let ((new (alloc-object heap
                                          (aref (gc-heap-sizes heap) old)
                                          (aref (gc-heap-nslots heap) old)
                                          :generation
                                          (aref (gc-heap-gens heap) old))))
                   (when new
                     ;; Copy slots verbatim; REMAP fixes interior refs.
                     (let ((from (aref (gc-heap-slots heap) old))
                           (to (aref (gc-heap-slots heap) new)))
                       (dotimes (s (length from))
                         (setf (aref to s) (aref from s))))
                     (setf (aref (gc-heap-objs heap) old)
                           (cons :forward new)
                           (gethash old (gc-heap-forwards heap)) new)
                     (push new (gc-heap-evac-copies heap))
                     ;; Root update is part of the pause.
                     (setf (gc-heap-roots heap)
                           (subst new old (gc-heap-roots heap)))
                     (incf done))))))
    (values (null (gc-heap-evac-worklist heap)) done)))

;;;; ---------------------------------------------------------------
;;;; Remapping — fix interior references, then retire the stubs.
;;;; ---------------------------------------------------------------

(defun remap-start (heap)
  "Begin the remap phase: the worklist is every object whose slots may
still name pre-evacuation ids — all allocated non-stub objects, copies
included.
Effects: ALLOC (the per-cycle remap worklist; bounded by capacity)."
  (setf (gc-heap-remap-worklist heap)
        (loop for id below (gc-heap-capacity heap)
              when (and (object-allocated-p heap id)
                        (not (obj-forwarded-p heap id)))
              collect id))
  heap)

(defun %remap-object (heap id)
  "Rewrite every slot of ID that names a forwarded object to name the copy.
Effects: none (alloc-free)."
  (let ((slots (aref (gc-heap-slots heap) id)))
    (dotimes (s (length slots))
      (let ((v (aref slots s)))
        (when (and (integerp v) (obj-forwarded-p heap v))
          (setf (aref slots s) (%resolve-forward heap v))))))
  heap)

(defun remap-step (heap &key (budget 1))
  "Remap up to BUDGET objects' slots.  When the worklist drains, finish the
pause: record the forwarding map for the harness's graph-preservation
check, retire every forwarding stub (recycle the old id — its space was
already dead), and clear the forwarding table and collection set.
Returns (values done-p objects-remapped).
Effects: none (alloc-free)."
  (declare (gc-heap heap) ((integer 0) budget))
  (let ((done 0))
    (loop while (and (< done budget) (gc-heap-remap-worklist heap))
          do (%remap-object heap (pop (gc-heap-remap-worklist heap)))
             (incf done))
    (when (null (gc-heap-remap-worklist heap))
      ;; Verification aid first: the harness checks graph preservation
      ;; against this map after the stubs are gone.
      (setf (gc-heap-remap-record heap)
            (sort (loop for old being the hash-keys of (gc-heap-forwards heap)
                        using (hash-value new)
                        collect (cons old new))
                  #'< :key #'car))
      ;; Retire stubs: recycle the old ids.
      (loop for id below (gc-heap-capacity heap)
            when (obj-forwarded-p heap id)
            do (%free-id heap id))
      (clrhash (gc-heap-forwards heap))
      (setf (gc-heap-collection-set heap) '()))
    (values (null (gc-heap-remap-worklist heap)) done)))
