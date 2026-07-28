;;;; test-gc.lisp — GC memref and heap-layout tests.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.

(in-package #:clef-test)

(deftest gc-mem-u64-roundtrip
  (let ((r (clef/gc::make-memregion 64)))
    (setf (clef/gc::mem-u64 r 0) #xdeadbeefcafe)
    (is (= (clef/gc::mem-u64 r 0) #xdeadbeefcafe))
    (setf (clef/gc::mem-u64 r 8) (1- (expt 2 64)))
    (is (= (clef/gc::mem-u64 r 8) (1- (expt 2 64))))))

(deftest gc-mem-u8-roundtrip
  (let ((r (clef/gc::make-memregion 16)))
    (setf (clef/gc::mem-u8 r 3) 200)
    (is (= (clef/gc::mem-u8 r 3) 200))))

(deftest gc-mem-base-offset
  ;; A region with a nonzero base translates addresses correctly.
  (let ((r (clef/gc::make-memregion 32 :base #x100000)))
    (setf (clef/gc::mem-u64 r (+ #x100000 8)) 12345)
    (is (= (clef/gc::mem-u64 r (+ #x100000 8)) 12345))))

(deftest gc-size-classes
  ;; Cons (16 bytes) is class 0. 64 (== +MAX-SMALL-SIZE+) is class 3.
  ;; Anything above +MAX-SMALL-SIZE+ isn't small.
  (is (= (clef/gc::size->class 16) 0))
  (is (= (clef/gc::size->class 1) 0))
  (is (= (clef/gc::class->size 0) 16))
  (is (= (clef/gc::size->class clef/gc::+max-small-size+) 3))
  (is (null (clef/gc::size->class (1+ clef/gc::+max-small-size+)))))

(deftest gc-line-allocation
  (let ((b (clef/gc::make-block-meta)))
    (multiple-value-bind (line ok) (clef/gc::block-alloc-line b 0)
      (is ok)
      (is (= line 0)))
    (multiple-value-bind (line ok) (clef/gc::block-alloc-line b 0)
      (is ok)
      (is (= line 1)))
    ;; line 0 and 1 are now marked occupied
    (is (= (aref (clef/gc::lt-marks (clef/gc::bm-lines b)) 0) 1))))

(deftest gc-layout-constants
  (is (= clef/gc::+lines-per-block+ 256))
  (is (= clef/gc::+blocks-per-region+ 128))
  (is (= clef/gc::+line-size+ 128))
  (is (= clef/gc::+block-size+ (* 32 1024))))
