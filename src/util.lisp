;;;; util.lisp — byte vectors and bit utilities.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.

(in-package #:clef/util)

(deftype bytevec () '(simple-array (unsigned-byte 8) (*)))

(defun make-bytevec (&optional (size 0))
  "Make a fresh zeroed byte vector of SIZE."
  (make-array size :element-type '(unsigned-byte 8) :initial-element 0))

(defun bytevec-length (bv) (length bv))

(defun bv-u8 (n)
  "One byte."
  (make-array 1 :element-type '(unsigned-byte 8) :initial-element (logand n #xff)))

(defun bv-u16le (n)
  "16-bit little-endian."
  (let ((bv (make-bytevec 2)))
    (setf (aref bv 0) (logand n #xff)
          (aref bv 1) (logand (ash n -8) #xff))
    bv))

(defun bv-u32le (n)
  "32-bit little-endian (N taken mod 2^32)."
  (let ((bv (make-bytevec 4)))
    (dotimes (i 4)
      (setf (aref bv i) (logand (ash n (* -8 i)) #xff)))
    bv))

(defun bv-u64le (n)
  "64-bit little-endian (N taken mod 2^64)."
  (let ((bv (make-bytevec 8)))
    (dotimes (i 8)
      (setf (aref bv i) (logand (ash n (* -8 i)) #xff)))
    bv))

(defun bv-s32le (n)
  "Signed 32-bit value encoded little-endian (two's complement)."
  (bv-u32le (logand n #xffffffff)))

(defun bv-concat (&rest bvs)
  "Concatenate byte vectors."
  (let* ((total (reduce #'+ bvs :key #'length :initial-value 0))
         (out (make-bytevec total))
         (pos 0))
    (dolist (bv bvs out)
      (replace out bv :start1 pos)
      (incf pos (length bv)))))

(defun bv-set (bv offset bytes)
  "Write BYTES (a bytevec or list of octets) into BV at OFFSET. Returns BV."
  (etypecase bytes
    (bytevec (replace bv bytes :start1 offset))
    (list (loop for b in bytes for i from offset do (setf (aref bv i) b))))
  bv)

(defun fit-s8 (n)
  "True if N fits in a signed byte."
  (and (<= -128 n) (<= n 127)))

(defun fit-s32 (n)
  "True if N fits in a signed 32-bit word."
  (and (<= (- (expt 2 31)) n) (<= n (1- (expt 2 31)))))

(defun logand-mask (n bits)
  "Low BITS of N."
  (logand n (1- (ash 1 bits))))
