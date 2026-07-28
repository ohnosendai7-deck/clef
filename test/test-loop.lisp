;;;; test-loop.lisp — CLEF-H extended loop tests.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.

(in-package #:clef-test)

(defvar *loop-env* nil)

(defun loop-env ()
  "A fresh CLEF-H env with builtins + boot loaded (cached)."
  (or *loop-env*
      (setf *loop-env*
            (let ((e (clef/proto/env:make-toplevel-env)))
              (clef/proto/eval::install-builtins e)
              (clef/proto/eval::install-boot-glue e)
              (clef/proto/eval::load-lisp-file
               e (merge-pathnames "src/proto/boot/macros.lisp"
                                  (asdf:system-source-directory :clef)))
              e))))

(defun lev (form)
  (car (clef/proto/eval:clef-eval form (loop-env))))

(deftest loop-for-in-collect
  (is (equal (lev '(loop for x in (list 1 2 3) collect (* x x))) '(1 4 9))))

(deftest loop-sum
  (is (eql (lev '(loop for x in (list 1 2 3 4) sum x)) 10)))

(deftest loop-for-from-to
  (is (equal (lev '(loop for i from 0 to 4 collect i)) '(0 1 2 3 4)))
  (is (equal (lev '(loop for i from 1 to 10 by 2 collect i)) '(1 3 5 7 9)))
  (is (equal (lev '(loop for i from 5 downto 1 collect i)) '(5 4 3 2 1))))

(deftest loop-across
  (is (equal (lev '(loop for x across (vector :a :b :c) collect x)) '(:a :b :c))))

(deftest loop-for-on
  (is (equal (lev '(loop for x on (list 1 2 3) collect x)) '((1 2 3) (2 3) (3)))))

(deftest loop-when
  (is (equal (lev '(loop for x in (list 1 2 3 4) when (evenp x) collect x)) '(2 4)))
  (is (equal (lev '(loop for x in (list 1 2 3 4) unless (evenp x) collect x)) '(1 3))))

(deftest loop-append
  (is (equal (lev '(loop for x in (list 1 2 3) append (list x x))) '(1 1 2 2 3 3))))

(deftest loop-max-min
  (is (eql (lev '(loop for x in (list 5 2 8 1) maximize x)) 8))
  (is (eql (lev '(loop for x in (list 5 2 8 1) minimize x)) 1)))

(deftest loop-count
  (is (eql (lev '(loop for x in (list 1 nil 3 nil) count x)) 2)))

(deftest loop-until
  (is (equal (lev '(loop for i from 0 until (= i 3) collect i)) '(0 1 2))))

(deftest loop-repeat
  (is (equal (lev '(loop repeat 3 for x in (list :a :b :c :d) collect x)) '(:a :b :c))))

;;; format (host-delegated, full directives)

(deftest format-basic
  (is (equal (lev '(format nil "~a" 42)) "42"))
  (is (equal (lev '(format nil "~d" 255)) "255"))
  (is (equal (lev '(format nil "~x" 255)) "FF")))

(deftest format-iteration
  (is (equal (lev '(format nil "~{~a~^, ~}" (list 1 2 3))) "1, 2, 3")))

(deftest format-float-and-case
  (is (equal (lev '(format nil "~5,2f" 3.14159)) " 3.14"))
  (is (equal (lev '(format nil "~@(~a~)" "hello")) "Hello")))

(deftest format-conditional
  (is (equal (lev '(format nil "~[one~;two~]" 1)) "two")))
