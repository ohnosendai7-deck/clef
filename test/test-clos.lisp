;;;; test-clos.lisp — CLEF-H CLOS subset tests.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.

(in-package #:clef-test)

(defvar *clos-env* nil)

(defun clos-env ()
  "A fresh CLEF-H env with builtins + boot + CLOS loaded (cached)."
  (or *clos-env*
      (setf *clos-env*
            (let ((e (clef/proto/env:make-toplevel-env)))
              (clef/proto/eval::install-builtins e)
              (clef/proto/eval::install-boot-glue e)
              (clef/proto/eval::load-lisp-file
               e (merge-pathnames "src/proto/boot/macros.lisp"
                                  (asdf:system-source-directory :clef)))
              (clef/proto/eval::install-clos e)
              ;; define the test class hierarchy
              (car (clef/proto/eval:clef-eval
                    '(defclass shape () ((name :initarg :name :initform "anon"
                                      :accessor shape-name))) e))
              (car (clef/proto/eval:clef-eval
                    '(defclass circle (shape) ((radius :initarg :radius
                                                :initform 1 :accessor circle-radius))) e))
              (car (clef/proto/eval:clef-eval
                    '(defparameter *c* (make-instance 'circle :radius 5 :name "c1")) e))
              e))))

(defun cev (form)
  (car (clef/proto/eval:clef-eval form (clos-env))))

(deftest clos-defclass-and-instance
  (is (eql (cev '(slot-value *c* 'radius)) 5))
  (is (equal (cev '(slot-value *c* 'name)) "c1")))

(deftest clos-accessors
  (is (eql (cev '(circle-radius *c*)) 5))
  (is (equal (cev '(shape-name *c*)) "c1")))

(deftest clos-setf-slot-value
  (is (eql (cev '(setf (slot-value *c* 'radius) 9)) 9))
  (is (eql (cev '(slot-value *c* 'radius)) 9))
  ;; restore
  (cev '(setf (slot-value *c* 'radius) 5)))

(deftest clos-inheritance
  (is (eql (cev '(progn (defclass rect (shape) ((w :initarg :w :initform 0)))
                        (slot-value (make-instance 'rect :name "r1" :w 3) 'w)))
           3))
  (is (equal (cev '(shape-name (make-instance 'rect :name "r2"))) "r2")))

(deftest clos-default-initform
  (is (equal (cev '(shape-name (make-instance 'circle))) "anon"))
  (is (eql (cev '(circle-radius (make-instance 'circle))) 1)))

(deftest clos-dispatch
  (cev '(defgeneric area (obj)))
  (cev '(defmethod area ((obj shape)) 0))
  (cev '(defmethod area ((obj circle)) 314))
  (is (eql (cev '(area *c*)) 314))
  (is (eql (cev '(area (make-instance 'shape))) 0)))

(deftest clos-call-next-method
  (cev '(defgeneric describe (obj)))
  (cev '(defmethod describe ((obj shape)) (list 'shape (shape-name obj))))
  (cev '(defmethod describe ((obj circle)) (cons 'circle (call-next-method))))
  (is (equal (cev '(describe *c*)) '(circle shape "c1"))))

(deftest clos-typep
  (is (eq (cev '(typep *c* 'circle)) t))
  (is (eq (cev '(typep *c* 'shape)) t))
  (is (eq (cev '(class-name (class-of *c*))) 'circle)))
