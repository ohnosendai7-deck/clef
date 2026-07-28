;;;; image.lisp — a fully-assembled CLEF-H environment + REPL.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; make-standard-env builds a toplevel environment with builtins, the boot
;;;; library, CLOS, and the condition system installed — a complete CLEF-H
;;;; image. repl runs a read-eval-print loop over it. run-file loads a file of
;;;; CLEF-H forms.

(in-package #:clef/proto)

(defvar *boot-root* nil
  "Directory containing src/proto/boot/. Defaults to the clef system source.")

(defun boot-root ()
  (or *boot-root*
      (setf *boot-root* (asdf:system-source-directory :clef))))

(defun make-standard-env ()
  "Build a complete CLEF-H environment: builtins, boot library, CLOS,
conditions, extended loop."
  (let ((e (clef/proto/env:make-toplevel-env)))
    (clef/proto/eval::install-builtins e)
    (clef/proto/eval::install-boot-glue e)
    (clef/proto/eval::load-lisp-file
     e (merge-pathnames "src/proto/boot/macros.lisp" (boot-root)))
    (clef/proto/eval::install-clos e)
    (clef/proto/eval::install-conditions e)
    ;; A load function that works inside CLEF-H.
    (clef/proto/env:set-function-value
     e 'cl:load
     (lambda (path)
       (clef/proto/eval::load-lisp-file e (namestring path))
       t))
    e))

(defun repl (&key (env (make-standard-env)) (in *standard-input*) (out *standard-output*))
  "A read-eval-print loop over a CLEF-H environment. Reads host-side (the
CLEF-H reader is a later milestone), evaluates in CLEF-H, prints results.
Type :quit to exit."
  (format out "~&CLEF-H (hosted prototype). :quit to exit.~%~%")
  (loop
    (format out "clef> ")
    (finish-output out)
    (let ((form (read in nil :eof)))
      (cond
        ((eq form :eof) (format out "~%") (return))
        ((member form '(:quit :q) :test #'equal) (return))
        (t
         (handler-case
             (let ((vals (clef/proto/eval:clef-eval form env)))
               (format out "~{~s~^, ~}~%~%" vals))
           (error (c)
             (format out "ERROR: ~a~%~%" c))))))))

(defun run-file (path &key (env (make-standard-env)))
  "Load and evaluate each top-level form in PATH in a CLEF-H environment."
  (clef/proto/eval::load-lisp-file env (namestring path))
  env)
