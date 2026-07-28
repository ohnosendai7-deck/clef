;;;; conformance.lisp — ANSI CL conformance suite for CLEF-H.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; A broad-coverage conformance suite, organized by ANSI CL chapter area.
;;;; Each deftest exercises one feature cluster end-to-end through the CLEF-H
;;;; evaluator (make-standard-env). This is the ratchet for "most of the spec":
;;;; add a test here when a feature lands.

(in-package #:clef-test)

(defvar *conf-env* nil)

(defun conf-env ()
  (or *conf-env* (setf *conf-env* (clef/proto:make-standard-env))))

(defun x (form)
  "Evaluate FORM in the conformance env; return the values list."
  (clef/proto/eval:clef-eval form (conf-env)))

(defun x1 (form) (car (x form)))

;;; --- 5. Forms and evaluation ---

(deftest conf-self-eval
  (is (eql (x1 42) 42))
  (is (equal (x1 "str") "str"))
  (is (eq (x1 :kw) :kw))
  (is (eq (x1 t) t))
  (is (eq (x1 nil) nil)))

(deftest conf-quote
  (is (equal (x1 '(quote (1 2 3))) '(1 2 3)))
  (is (eq (x1 ''sym) 'sym)))

(deftest conf-if-progn
  (is (eql (x1 '(if t 1 2)) 1))
  (is (eql (x1 '(if nil 1 2)) 2))
  (is (eql (x1 '(progn 1 2 3)) 3)))

;;; --- 6. Iteration ---

(deftest conf-dolist-dotimes
  (is (eql (x1 '(let ((s 0)) (dolist (x (list 1 2 3) s) (incf s x)))) 6))
  (is (eql (x1 '(let ((c 0)) (dotimes (i 5 c) (incf c)))) 5)))

(deftest conf-loop
  (is (equal (x1 '(loop for x in (list 1 2 3) collect (* x x))) '(1 4 9)))
  (is (eql (x1 '(loop for i from 1 to 5 sum i)) 15)))

;;; --- 7. Objects (CLOS) ---

(deftest conf-clos
  (x '(defclass conf-pt () ((x :initarg :x :initform 0 :accessor pt-x))))
  (is (eql (x1 '(pt-x (make-instance 'conf-pt :x 5))) 5))
  (is (eql (x1 '(pt-x (make-instance 'conf-pt))) 0)))

(deftest conf-generic
  (x '(defgeneric conf-area (o)))
  (x '(defclass conf-sq () ((s :initarg :s))))
  (x '(defmethod conf-area ((o conf-sq)) (let ((s (slot-value o 's))) (* s s))))
  (is (eql (x1 '(conf-area (make-instance 'conf-sq :s 4))) 16)))

;;; --- 9. Conditions ---

(deftest conf-handler-case
  (is (eq (x1 '(handler-case (error "e") (error (c) :caught))) :caught)))

(deftest conf-restarts
  (is (eql (x1 '(restart-case (invoke-restart 'r 10) (r (v) (* v 2)))) 20)))

;;; --- 10. Symbols ---

(deftest conf-symbols
  (is (equal (x1 '(symbol-name 'abc)) "ABC"))
  (is (eq (x1 '(keywordp :k)) t))
  (is (eq (x1 '(symbolp 's)) t)))

;;; --- 11. Packages ---

(deftest conf-packages
  (is (eq (x1 '(packagep (find-package :cl))) t))
  (is (equal (x1 '(package-name (find-package :keyword))) "KEYWORD")))

;;; --- 12. Numbers ---

(deftest conf-arithmetic
  (is (eql (x1 '(+ 1 2 3)) 6))
  (is (eql (x1 '(- 10 3)) 7))
  (is (eql (x1 '(* 6 7)) 42))
  (is (eql (x1 '(/ 12 4)) 3))
  (is (eql (x1 '(expt 2 10)) 1024))
  (is (eql (x1 '(mod 7 3)) 1))
  (is (eql (x1 '(gcd 12 18)) 6)))

(deftest conf-comparison
  (is (eq (x1 '(< 1 2 3)) t))
  (is (eq (x1 '(> 3 2 1)) t))
  (is (eq (x1 '(= 5 5)) t))
  (is (eq (x1 '(<= 1 1)) t)))

(deftest conf-predicates
  (is (eq (x1 '(zerop 0)) t))
  (is (eq (x1 '(plusp 1)) t))
  (is (eq (x1 '(minusp -1)) t))
  (is (eq (x1 '(evenp 4)) t))
  (is (eq (x1 '(oddp 3)) t)))

;;; --- 13. Conses ---

(deftest conf-conses
  (is (equal (x1 '(cons 1 2)) '(1 . 2)))
  (is (eql (x1 '(car (list 1 2 3))) 1))
  (is (equal (x1 '(cdr (list 1 2 3))) '(2 3)))
  (is (equal (x1 '(append (list 1) (list 2 3))) '(1 2 3)))
  (is (eql (x1 '(length (list 1 2 3 4))) 4))
  (is (equal (x1 '(reverse (list 1 2 3))) '(3 2 1)))
  (is (equal (x1 '(mapcar (lambda (x) (* x x)) (list 1 2 3))) '(1 4 9))))

;;; --- 14. Arrays ---

(deftest conf-arrays
  (is (eql (x1 '(let ((v (vector 1 2 3))) (aref v 1))) 2))
  (is (eql (x1 '(length (make-array 5))) 5))
  (is (eql (x1 '(let ((v (make-array 3 :initial-element 7))) (aref v 0))) 7)))

;;; --- 15. Strings ---

(deftest conf-strings
  (is (equal (x1 '(string-upcase "abc")) "ABC"))
  (is (equal (x1 '(string-downcase "ABC")) "abc"))
  (is (equal (x1 '(string-capitalize "hello world")) "Hello World"))
  (is (eql (x1 '(char "hello" 1)) #\e))
  (is (equal (x1 '(concatenate 'string "a" "b" "c")) "abc")))

;;; --- 16. Characters ---

(deftest conf-chars
  (is (eql (x1 '(char-code #\A)) 65))
  (is (eq (x1 '(char= #\a #\a)) t))
  (is (eq (x1 '(alpha-char-p #\a)) t))
  (is (eq (x1 '(digit-char-p #\5)) 5)))

;;; --- 17. Sequences ---

(deftest conf-sequences
  (is (eql (x1 '(count 2 (list 1 2 2 3))) 2))
  (is (eql (x1 '(find 3 (list 1 2 3))) 3))
  (is (eql (x1 '(position :b (list :a :b :c))) 1))
  (is (equal (x1 '(remove 2 (list 1 2 3 2))) '(1 3)))
  (is (equal (x1 '(subseq (list 1 2 3 4) 1 3)) '(2 3)))
  (is (equal (x1 '(sort (list 3 1 2) #'<)) '(1 2 3)))
  (is (eql (x1 '(reduce #'+ (list 1 2 3 4))) 10)))

;;; --- 18. Hash tables ---

(deftest conf-hash
  (is (eql (x1 '(let ((h (make-hash-table)))
                 (setf (gethash :a h) 1)
                 (setf (gethash :b h) 2)
                 (+ (gethash :a h) (gethash :b h)))) 3))
  (is (eql (x1 '(let ((h (make-hash-table))) (setf (gethash :x h) 9) (hash-table-count h))) 1)))

;;; --- 22. Printer / format ---

(deftest conf-format
  (is (equal (x1 '(format nil "~a" 1)) "1"))
  (is (equal (x1 '(write-to-string (list 1 2))) "(1 2)"))
  (is (equal (x1 '(princ-to-string :sym)) "SYM")))

;;; --- control: multiple values, block, catch ---

(deftest conf-multiple-values
  (is (equal (x '(values 1 2 3)) '(1 2 3)))
  (is (eql (x1 '(multiple-value-bind (a b) (floor 7 2) (+ a b))) 4))
  (is (eql (x1 '(nth-value 1 (floor 7 2))) 1)))

(deftest conf-control
  (is (eql (x1 '(block b (return-from b 5) 1)) 5))
  (is (eql (x1 '(catch :k (throw :k 9))) 9))
  (is (eql (x1 '(unwind-protect 1 2)) 1)))

;;; --- closures / functional programming ---

(deftest conf-closures
  (is (eql (x1 '(let ((counter (let ((n 0)) (lambda () (incf n)))))
                 (funcall counter) (funcall counter) (funcall counter))) 3))
  (is (eql (x1 '(funcall (lambda (x) (* x 2)) 21)) 42))
  (is (eql (x1 '(apply #'+ (list 1 2 3))) 6)))

(deftest conf-recursion
  (x '(defun conf-fact (n) (if (<= n 1) 1 (* n (conf-fact (- n 1))))))
  (is (eql (x1 '(conf-fact 5)) 120)))
