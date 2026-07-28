;;;; test-proto.lisp — CLEF-H hosted prototype tests.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; Loads the boot library and exercises the self-hosting macro layer, the
;;;; evaluator, and builtins through the CLEF-H environment.

(in-package #:clef-test)

(defvar *proto-env* nil)

(defun proto-env ()
  "A fresh CLEF-H environment with builtins + boot library loaded (cached)."
  (or *proto-env*
      (setf *proto-env*
            (let ((e (clef/proto/env:make-toplevel-env)))
              (clef/proto/eval::install-builtins e)
              (clef/proto/eval::install-boot-glue e)
              (clef/proto/eval::load-lisp-file
               e (merge-pathnames "src/proto/boot/macros.lisp"
                                  (asdf:system-source-directory :clef)))
              e))))

(defun pev (form)
  "Evaluate FORM in the proto env, returning the primary value."
  (car (clef/proto/eval:clef-eval form (proto-env))))

(deftest proto-boot-loads
  ;; If we got here without error, the whole boot file loaded.
  (is (not (null (proto-env)))))

(deftest proto-arithmetic
  (is (= (pev '(+ 1 2)) 3))
  (is (= (pev '(* 6 7)) 42))
  (is (= (pev '(floor 7 2)) 3)))

(deftest proto-defun
  (pev '(defun square (x) (* x x)))
  (is (= (pev '(square 9)) 81)))

(deftest proto-let-lambda
  (is (= (pev '(let ((x 5) (y 6)) (+ x y))) 11))
  (is (= (pev '(funcall (lambda (x) (* x 2)) 21)) 42)))

(deftest proto-macros
  (is (= (pev '(when t 5)) 5))
  (is (= (pev '(unless nil 42)) 42))
  (is (= (pev '(cond (nil 1) (t 2))) 2))
  (is (= (pev '(and 1 2 3)) 3))
  (is (= (pev '(or nil nil 5)) 5))
  (is (= (pev '(case :b (:a 1) (:b 2) (t 3))) 2)))

(deftest proto-setf
  (is (= (pev '(let ((x 0)) (incf x 5) x)) 5))
  (is (equal (pev '(let ((l (list 1 2))) (push 0 l) l)) '(0 1 2)))
  (is (= (pev '(let ((l (list 1 2 3))) (pop l))) 1)))

(deftest proto-control
  (is (eql (pev '(block foo (return-from foo 42) 1)) 42))
  (is (eql (pev '(catch :k (throw :k 7))) 7))
  (is (eq (pev '(dolist (x (list 1 2 3) :done) x)) :done)))

(deftest proto-multiple-values
  (is (= (pev '(multiple-value-bind (a b) (values 1 2) (+ a b))) 3)))

(deftest proto-destructuring
  (is (= (pev '(destructuring-bind (x y) (list 10 20) (+ x y))) 30)))

(deftest proto-quasiquote
  (is (equal (pev '(let ((a 2)) `(1 ,a ,@(list 3 4)))) '(1 2 3 4))))

(deftest proto-loop
  (is (equal (pev '(loop for x in (list 1 2 3) collect (* x x))) '(1 4 9)))
  (is (= (pev '(loop for x in (list 1 2 3 4) sum x)) 10)))

(deftest proto-do
  (is (eq (pev '(do ((i 0 (1+ i))) ((= i 3) :done))) :done)))

(deftest proto-defstruct
  (pev '(defstruct point x y))
  (is (= (pev '(point-x (make-point :x 3 :y 4))) 3))
  (is (= (pev '(point-y (make-point :x 3 :y 4))) 4)))

(deftest proto-places
  (is (= (pev '(let ((v (make-array 3 :initial-element 0)))
                (setf (aref v 1) 9) (aref v 1))) 9))
  (is (= (pev '(let ((h (make-hash-table)))
                (setf (gethash :k h) 7) (gethash :k h))) 7)))
