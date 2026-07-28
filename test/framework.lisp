;;;; framework.lisp — a tiny test framework (no external deps).
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.

(in-package #:clef-test)

(defvar *tests* '())
(defvar *pass* 0)
(defvar *fail* 0)
(defvar *failures* '())

(defmacro deftest (name &body body)
  `(progn
     (setf *tests* (remove ',name *tests* :key #'car))
     (push (cons ',name (lambda () ,@body)) *tests*)
     ',name))

(defmacro is (form &optional (msg nil msgp))
  `(unless ,form
     (error "Check failed: ~s~@[ — ~a~]" ',form ,(and msgp msg))))

(defmacro signals-error (form)
  `(handler-case (progn ,form (error "Expected error from: ~s" ',form))
     (error () t)))

(defun %run-all ()
  (setf *pass* 0 *fail* 0 *failures* '())
  (dolist (entry (reverse *tests*))
    (destructuring-bind (name . fn) entry
      (handler-case (progn (funcall fn) (incf *pass*)
                           (format t "  ok  ~a~%" name))
        (error (e)
          (incf *fail*)
          (push (cons name e) *failures*)
          (format t "FAIL  ~a~%      ~a~%" name e)))))
  (format t "~%clef-test: ~d passed, ~d failed, ~d total~%"
          *pass* *fail* (+ *pass* *fail*))
  (when *failures*
    (format t "Failures:~%")
    (dolist (f (reverse *failures*))
      (format t "  ~a: ~a~%" (car f) (cdr f))))
  *fail*)
