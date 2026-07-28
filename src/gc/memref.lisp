;;;; memref.lisp — raw memory access for the GC and systems subset.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; On the host (SBCL, during model-testing) a memref region is backed by a
;;;; byte vector. On target, the same API compiles down to raw loads/stores
;;;; into memory the GC manages directly (never Lisp objects, never collected).
;;;; This seam is what makes the GC testable before any target code exists.

(in-package #:clef/gc)

(defstruct (memregion (:constructor %make-memregion))
  (vector nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (base 0 :type (unsigned-byte 64))  ; target base address (host: 0)
  (size 0 :type (unsigned-byte 64)))

(defun make-memregion (size &key (base 0))
  "Allocate a raw region of SIZE bytes. On host, a byte vector."
  (%make-memregion :vector (make-array size :element-type '(unsigned-byte 8)
                                       :initial-element 0)
                   :base base
                   :size size))

(declaim (inline mem-u8 mem-u64 (setf mem-u8) (setf mem-u64)))

(defun addr->index (region addr)
  "Translate an address into a host vector index."
  (- addr (memregion-base region)))

(defun mem-u8 (region addr)
  (aref (memregion-vector region) (addr->index region addr)))

(defun (setf mem-u8) (v region addr)
  (setf (aref (memregion-vector region) (addr->index region addr))
        (logand v #xff)))

(defun mem-u64 (region addr)
  "Little-endian 64-bit load."
  (let ((i (addr->index region addr))
        (v (memregion-vector region)))
    (logior (aref v i)
            (ash (aref v (+ i 1)) 8)
            (ash (aref v (+ i 2)) 16)
            (ash (aref v (+ i 3)) 24)
            (ash (aref v (+ i 4)) 32)
            (ash (aref v (+ i 5)) 40)
            (ash (aref v (+ i 6)) 48)
            (ash (aref v (+ i 7)) 56))))

(defun (setf mem-u64) (val region addr)
  "Little-endian 64-bit store."
  (let ((i (addr->index region addr))
        (v (memregion-vector region)))
    (dotimes (k 8 val)
      (setf (aref v (+ i k)) (logand (ash val (* -8 k)) #xff)))))

;;; On target, MEM-U64/(SETF MEM-U64) and friends become primitive unboxed
;;; memory operations; the region/vector indirection is compiled away. The
;;; GC's mark/sweep/evacuate loops are written against this API and carry
;;; effect declarations excluding ALLOC, which the solver checks (DESIGN §6.5).
