;;;; test-conditions.lisp — CLEF-H condition system tests.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.

(in-package #:clef-test)

(defvar *cond-env* nil)

(defun cond-env ()
  "A fresh CLEF-H env with builtins + boot + conditions loaded (cached)."
  (or *cond-env*
      (setf *cond-env*
            (let ((e (clef/proto/env:make-toplevel-env)))
              (clef/proto/eval::install-builtins e)
              (clef/proto/eval::install-boot-glue e)
              (clef/proto/eval::load-lisp-file
               e (merge-pathnames "src/proto/boot/macros.lisp"
                                  (asdf:system-source-directory :clef)))
              (clef/proto/eval::install-conditions e)
              e))))

(defun cdev (form)
  (car (clef/proto/eval:clef-eval form (cond-env))))

(defun cdev-str (string)
  "Evaluate a form read from STRING in the CL-USER package, so condition and
user-defined function names resolve the same way the boot glue registers them."
  (let ((*package* (find-package :cl-user)))
    (car (clef/proto/eval:clef-eval (read-from-string string) (cond-env)))))

(deftest cond-handler-case
  (is (eq (cdev '(handler-case (error "boom") (error (c) :caught))) :caught)))

(deftest cond-handler-case-typed
  (is (eq (cdev '(handler-case (error 'arithmetic-error)
                  (arithmetic-error (c) :arith)
                  (error (c) :other)))
          :arith)))

(deftest cond-handler-in-function
  (is (eq (cdev '(progn (defun div0 (x) (error "cannot divide ~s" x))
                        (handler-case (div0 5) (error (c) :handled))))
          :handled)))

(deftest cond-conditionp
  (is (eq (cdev-str "(handler-case (error \"x\") (error (c) (conditionp c)))") t)))

(deftest cond-handler-bind-continue
  (is (eq (cdev '(handler-bind ((warning (lambda (c) (declare (ignore c)) (muffle-warning))))
                  (warn "w") :after))
          :after)))

(deftest cond-restart-case-invoke
  (is (eql (cdev '(restart-case (invoke-restart 'my-restart 42)
                   (my-restart (x) (+ x 1))))
           43)))

(deftest cond-find-restart
  (is (eq (cdev '(restart-case (if (find-restart 'foo) :found :none)
                  (foo () :foo)))
          :found)))

(deftest cond-define-condition
  (is (eql (cdev-str "(progn (define-condition my-error (error) ((info :initarg :info :reader my-info)))
                             (handler-case (error 'my-error :info 5)
                               (my-error (c) (my-info c))))")
           5)))

(deftest cond-ignore-errors
  (is (eq (cdev '(ignore-errors (error "x") :not-reached)) nil)))

(deftest cond-cerror-continue
  (is (eq (cdev '(handler-bind ((error (lambda (c) (declare (ignore c)) (continue))))
                  (cerror "keep going" "something failed")
                  :survived))
          :survived)))
