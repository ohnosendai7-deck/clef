;;;; heap.lisp — Immix-style heap layout for the CLEF GC.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; Size-class-segregated heap: regions containing blocks divided into lines.
;;;; Small objects bump-allocate into lines; large objects get whole blocks;
;;;; humongous objects get whole regions. This is the host-modelable skeleton:
;;;; layout constants, line/block metadata, and the allocation fast path.
;;;; Concurrent marking (SATB) and evacuation are layered on top (DESIGN §6).

(in-package #:clef/gc)

;;;; Layout constants (bytes). Powers of two throughout.

(defconstant +line-size+ 128
  "Immix line granule. Objects smaller than a line share lines by size class.")
(defconstant +block-size+ (* 32 1024)
  "Immix block: 32 KiB.")
(defconstant +lines-per-block+ (/ +block-size+ +line-size+))   ; 256
(defconstant +region-size+ (* 4 1024 1024)
  "Region: 4 MiB.")
(defconstant +blocks-per-region+ (/ +region-size+ +block-size+)) ; 128
(defconstant +max-small-size+ (/ +line-size+ 2)
  "Objects up to this size allocate into shared lines.")
(defconstant +max-block-object-size+ (/ +block-size+ 2)
  "Objects up to this size allocate in whole blocks; larger are humongous.")

;;;; Size classes for small objects. We use a simple 16-byte spacing for the
;;;; first several classes, matching common Lisp object sizes (a headerless
;;;; 2-word cons is 16 bytes — class 0).

(defparameter *size-classes*
  #(16 32 48 64 80 96 112 128)
  "Small-object size classes (bytes).")

(defun size->class (size)
  "Size class index for a small object, or NIL if not small."
  (when (<= size +max-small-size+)
    (position-if (lambda (c) (<= size c)) *size-classes*)))

;;; The largest small-object class is capped at +MAX-SMALL-SIZE+ (64), so
;;; sizes 65..+MAX-SMALL-SIZE+ fall in the last class. *SIZE-CLASSES* only
;;; lists classes up to the cap.

(defun class->size (class) (aref *size-classes* class))

;;;; Line/block metadata live in side tables (out-of-band, per DESIGN §6.3),
;;;; not in object headers. For the host model we keep them in Lisp structs;
;;;; on target they live in raw memregion side tables.

(defstruct (line-table (:conc-name lt-))
  ;; For each line in a block: 0=free, 1=occupied, 2=marked.
  (marks (make-array +lines-per-block+ :element-type '(unsigned-byte 2)
                                       :initial-element 0)))

(defstruct (block-meta (:conc-name bm-))
  (size-class -1 :type fixnum)   ; which class this block serves (-1 = unset)
  (lines (make-line-table))
  (next-free-line 0 :type fixnum))

(defstruct (region-meta (:conc-name rm-))
  (base 0 :type (unsigned-byte 64))
  (blocks (make-array +blocks-per-region+ :initial-element nil)))

;;;; Allocation fast path (single-threaded for the skeleton; the real
;;;; allocator uses TLABs with allocated-black semantics, DESIGN §6.4).

(defun block-alloc-line (bmeta class)
  "Allocate one line's worth of space for CLASS in block BMETA.
Returns (values line-index ok)."
  (declare (ignore class))
  (let ((line (bm-next-free-line bmeta)))
    (if (>= line +lines-per-block+)
        (values nil nil)
        (progn
          (setf (aref (lt-marks (bm-lines bmeta)) line) 1)
          (setf (bm-next-free-line bmeta) (1+ line))
          (values line t)))))

;;; The mark phase sets line marks; the sweep phase scans line tables and
;;; returns blocks with all-free lines to the region's free list without
;;; moving anything. Evacuation (bounded-STW) is layered on only for blocks
;;; past a fragmentation threshold. See DESIGN §6.3.
