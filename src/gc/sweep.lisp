;;;; sweep.lisp — incremental line/block sweep for the host-modeled heap.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; Implemented clean-room from Immix (Blackburn & McKinley 2008): the
;;;; sweeper scans line tables, frees lines whose objects are unmarked, and
;;;; returns blocks that became entirely free to the region's free list.
;;;; No collector source was consulted.
;;;;
;;;; ------------------------------------------------------------------
;;;; EFFECT-DISCIPLINE NOTE (for the eventual solver, DESIGN §6.5)
;;;; ------------------------------------------------------------------
;;;; Host-model code; the effect rows below are DOCUMENTED DECLARATIONS,
;;;; not solver-checked judgments.  The alloc-freedom argument the solver
;;;; will eventually prove for the target translation:
;;;;
;;;;   * Sweeping touches only fixed side tables: the per-object liveness
;;;;     array, the per-block line tables, and the block lists.  All were
;;;;     sized at heap creation; the sweep mutates bits and list cells in
;;;;     place and never requests fresh metadata storage.
;;;;   * The sweep queue is built once per cycle (SWEEP-START, outside the
;;;;     mutator's effect row) and drained by SWEEP-STEP without growth.
;;;;   * Freeing an object is an id recycle onto the pre-sized FREE-IDS
;;;;     list and a line-table bit clear — no allocation on target.
;;;;
;;;; Line tables are the truth (Immix §3): after a block is swept its line
;;;; marks are rebuilt from exactly the objects that survived in it.

(in-package #:clef/gc)

;;;; ---------------------------------------------------------------
;;;; Block enumeration (deterministic: sorted, never maphash order).
;;;; ---------------------------------------------------------------

(defun occupied-blocks (heap)
  "Sorted list of block indices that are not on the region free list.
Block metadata side tables exist for every block from heap creation, so
membership in the block table is not occupancy; the free list is.
Effects: ALLOC (transient host list; a query)."
  (let (out)
    (maphash (lambda (b meta) (declare (ignore meta)) (push b out))
             (gc-heap-block-table heap))
    (sort (set-difference out (gc-heap-free-blocks heap)) #'<)))

(defun %block-object-ids (heap block)
  "Sorted ids of all allocated objects (live, white, or forwarding stubs)
homed in BLOCK.
Effects: ALLOC (transient host list; a query)."
  (loop for id below (gc-heap-capacity heap)
        when (and (object-allocated-p heap id)
                  (eql (aref (gc-heap-obj-block heap) id) block))
        collect id into out
        finally (return (sort out #'<))))

(defun %free-id (heap id)
  "Recycle object ID: drop its liveness entry, return the id to the
pre-sized free list, and detach it from its block.  Also drops the id from
the pending-finalizer queue if it sat there undelivered (a finalizer that
was never run is moot once the object is gone).
Effects: none (alloc-free)."
  (setf (aref (gc-heap-objs heap) id) nil
        (aref (gc-heap-marks heap) id) 0
        (aref (gc-heap-obj-block heap) id) -1
        (aref (gc-heap-obj-line heap) id) -1)
  (push id (gc-heap-free-ids heap))
  (decf (gc-heap-live-count heap))
  (setf (gc-heap-pending-finalizers heap)
        (remove id (gc-heap-pending-finalizers heap)))
  heap)

;;;; ---------------------------------------------------------------
;;;; The sweep proper.
;;;; ---------------------------------------------------------------

(defun sweep-start (heap)
  "Begin the sweep phase: queue every occupied block for incremental
sweeping.
Effects: ALLOC (the per-cycle sweep queue; built outside mutator pauses on
target and pre-sized by the block count)."
  (setf (gc-heap-sweep-queue heap) (occupied-blocks heap))
  heap)

(defun %sweep-block (heap block)
  "Sweep one block:
  1. Free every unmarked object homed in it (forwarding stubs own no space
     — their lines died at evacuation — but a stub's id is REMAP's to
     recycle, not ours).
  2. Rebuild the line table from exactly the surviving objects.
  3. If nothing survives, return the block to the region free list and
     clear its card table; otherwise refresh NEXT-FREE-LINE and the
     reusable-block (size-class availability) bookkeeping.
  4. Generational mode: survivors are promoted to old — they lived through
     a cycle.
Effects: none (alloc-free)."
  (let ((bmeta (gethash block (gc-heap-block-table heap))))
    (unless bmeta (return-from %sweep-block heap))
    (let ((ids (%block-object-ids heap block))
          (survivors '()))
      ;; 1. Reclaim the white set.
      (dolist (id ids)
        (cond ((obj-forwarded-p heap id))       ; stub: remap owns the id
              ((zerop (aref (gc-heap-marks heap) id))
               (%free-id heap id))
              (t (push id survivors))))
      (setf survivors (sort survivors #'<))
      ;; 4. Promotion: a marked survivor lived through the cycle.
      (when (gc-heap-generational heap)
        (dolist (id survivors)
          (setf (aref (gc-heap-gens heap) id) +gen-old+)))
      ;; 2. Line table = exactly the survivors' runs.
      (let ((marks (lt-marks (bm-lines bmeta))))
        (fill marks 0)
        (dolist (id survivors)
          (let ((line (aref (gc-heap-obj-line heap) id))
                (n (lines-needed (aref (gc-heap-sizes heap) id))))
            (dotimes (k n)
              (setf (aref marks (+ line k)) 1))))
        ;; 3. Whole-block reclamation or availability refresh.  (IDS is
        ;; the pre-sweep census; after step 1 the only objects left homed
        ;; here are SURVIVORS plus any forwarding stubs, and stubs own no
        ;; space — their lines died at evacuation.)
        (if (null survivors)
            (progn
              ;; Return the block to the region free list.  Its metadata
              ;; side table stays (fixed at heap creation) and is reset
              ;; to virgin state; its cards die with it.
              (fill marks 0)
              (setf (bm-next-free-line bmeta) 0)
              (remhash block (gc-heap-cards heap))
              (setf (gc-heap-reusable-blocks heap)
                    (remove block (gc-heap-reusable-blocks heap)))
              (pushnew block (gc-heap-free-blocks heap)))
            (progn
              (setf (bm-next-free-line bmeta)
                    (or (position 0 marks) +lines-per-block+))
              ;; Size-class availability: a block with free runs is a
              ;; reusable allocation source for its size class.
              (if (position 0 marks)
                  (pushnew block (gc-heap-reusable-blocks heap))
                  (setf (gc-heap-reusable-blocks heap)
                        (remove block (gc-heap-reusable-blocks heap))))))))
    heap))

(defun sweep-step (heap &key (block-budget 1))
  "Sweep up to BLOCK-BUDGET blocks from the sweep queue.  Returns
(values done-p blocks-swept); done-p is true when the queue drained.
Effects: none (alloc-free) — the queue is pre-built, blocks are swept in
place."
  (declare (gc-heap heap) ((integer 0) block-budget))
  (let ((done 0))
    (loop while (and (< done block-budget)
                     (gc-heap-sweep-queue heap))
          do (%sweep-block heap (pop (gc-heap-sweep-queue heap)))
             (incf done))
    (values (null (gc-heap-sweep-queue heap)) done)))

(defun occupied-block-count (heap)
  "Number of blocks with block-meta.
Effects: none (alloc-free)."
  (hash-table-count (gc-heap-block-table heap)))

(defun sweep-all (heap)
  "Convenience: SWEEP-START then drain.  Used by unit tests; the model
harness steps the sweep incrementally.
Effects: ALLOC (SWEEP-START's queue), then none."
  (sweep-start heap)
  (loop until (sweep-step heap :block-budget (max 1 (occupied-block-count heap))))
  heap)
