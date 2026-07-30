;;;; test-contexts.lisp — package context tests (DESIGN.md §7, issue #6).
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; Exercises the CLEF-H package-context prototype: independent instances of
;;;; a stateful library, dynamic-binding tooling inside contexts, in-context
;;;; initforms, lazy fallback + sync-context, nesting, shared function and
;;;; constant cells, and the thread-inheritance model. The ANSI-untouched
;;;; property is the rest of the suite passing unmodified.

(in-package #:clef-test)

;;; A stand-in "library package" for the stateful library the contexts wrap.
(defpackage #:ctx-lib (:use #:cl))

(defun fresh-ctx-env ()
  "A fresh CLEF-H environment (builtins + boot library), isolated from the
shared *PROTO-ENV* so context tests cannot perturb other tests."
  (let ((e (clef/proto/env:make-toplevel-env)))
    (clef/proto/eval::install-builtins e)
    (clef/proto/eval::install-boot-glue e)
    (clef/proto/eval::load-lisp-file
     e (merge-pathnames "src/proto/boot/macros.lisp"
                        (asdf:system-source-directory :clef)))
    e))

(defun xev (env form)
  "Evaluate FORM in ENV, returning the primary value."
  (car (clef/proto/eval:clef-eval form env)))

(defun make-ctx (env package)
  (clef/proto/contexts:make-package-context env package))

(defun sync-ctx (env ctx)
  (clef/proto/contexts:sync-context env ctx))

(defmacro with-ctx ((ctx) &body body)
  `(clef/proto/contexts:with-package-env ,ctx ,@body))

(deftest contexts-independent-library-instances
  ;; The acceptance case: two contexts over one stateful library (a defvar +
  ;; a counter function defined once at toplevel) increment independently;
  ;; the global cell is untouched.
  (let ((env (fresh-ctx-env)))
    (xev env '(defvar ctx-lib::*counter* 0))
    (xev env '(defun ctx-lib::bump-counter () (incf ctx-lib::*counter*)))
    (let ((a (make-ctx env :ctx-lib))
          (b (make-ctx env :ctx-lib)))
      (with-ctx (a)
        (xev env '(ctx-lib::bump-counter))
        (xev env '(ctx-lib::bump-counter)))
      (with-ctx (b)
        (xev env '(ctx-lib::bump-counter)))
      (is (= 2 (with-ctx (a) (xev env 'ctx-lib::*counter*))))
      (is (= 1 (with-ctx (b) (xev env 'ctx-lib::*counter*))))
      (is (= 0 (xev env 'ctx-lib::*counter*))
          "the global cell is untouched"))))

(deftest contexts-dynamic-binding-tools
  ;; symbol-value / boundp / (setf symbol-value) / set / makunbound all
  ;; operate on the context's cells inside with-package-env.
  (let ((env (fresh-ctx-env)))
    (xev env '(defvar ctx-lib::*sv* 10))
    (let ((ctx (make-ctx env :ctx-lib)))
      (with-ctx (ctx)
        (is (xev env '(boundp 'ctx-lib::*sv*)))
        (is (= 10 (xev env '(symbol-value 'ctx-lib::*sv*))))
        (xev env '(setf (symbol-value 'ctx-lib::*sv*) 42))
        (is (= 42 (xev env '(symbol-value 'ctx-lib::*sv*))))
        (is (= 42 (xev env 'ctx-lib::*sv*))
            "(setf symbol-value) writes the cell direct references read")
        (xev env '(set 'ctx-lib::*sv* 43))
        (is (= 43 (xev env 'ctx-lib::*sv*)))
        (xev env '(makunbound 'ctx-lib::*sv*))
        (is (not (xev env '(boundp 'ctx-lib::*sv*))))
        (signals-error (xev env '(symbol-value 'ctx-lib::*sv*)))
        (signals-error (xev env 'ctx-lib::*sv*)))
      ;; The global cell was untouched the whole time.
      (is (xev env '(boundp 'ctx-lib::*sv*)))
      (is (= 10 (xev env 'ctx-lib::*sv*))))))

(deftest contexts-initform-evaluated-in-context
  ;; A defparameter executed in-context evaluates its initform IN the
  ;; context: it reads another special that differs per context.
  (let ((env (fresh-ctx-env)))
    (xev env '(defvar ctx-lib::*base* 1))
    (xev env '(defparameter ctx-lib::*derived* -1))
    (let ((a (make-ctx env :ctx-lib))
          (b (make-ctx env :ctx-lib)))
      (with-ctx (a) (xev env '(setq ctx-lib::*base* 10)))
      (with-ctx (b) (xev env '(setq ctx-lib::*base* 20)))
      (with-ctx (a)
        (xev env '(defparameter ctx-lib::*derived* (* ctx-lib::*base* 2))))
      (with-ctx (b)
        (xev env '(defparameter ctx-lib::*derived* (* ctx-lib::*base* 2))))
      (is (= 20 (with-ctx (a) (xev env 'ctx-lib::*derived*))))
      (is (= 40 (with-ctx (b) (xev env 'ctx-lib::*derived*))))
      (is (= -1 (xev env 'ctx-lib::*derived*))
          "the global cell is untouched")
      (is (= 1 (xev env 'ctx-lib::*base*))))))

(deftest contexts-lazy-fallback-and-sync
  ;; A defvar added to the library AFTER a context was created lazily falls
  ;; back to the global cell; sync-context materializes it (strict case).
  (let ((env (fresh-ctx-env)))
    (xev env '(defvar ctx-lib::*early* 5))
    (let ((ctx (make-ctx env :ctx-lib)))
      (xev env '(defvar ctx-lib::*late* 99))
      (with-ctx (ctx)
        (is (= 99 (xev env 'ctx-lib::*late*))
            "lazy fallback reads the global value")
        (xev env '(setq ctx-lib::*late* 100)))
      (is (= 100 (xev env 'ctx-lib::*late*))
          "lazy fallback writes through to the global cell")
      (sync-ctx env ctx)
      (with-ctx (ctx)
        (is (= 100 (xev env 'ctx-lib::*late*))
            "sync snapshots the current global value")
        (xev env '(setq ctx-lib::*late* 7))
        (is (= 7 (xev env 'ctx-lib::*late*))))
      (is (= 100 (xev env 'ctx-lib::*late*))
          "after sync the global cell is independent again"))))

(deftest contexts-defvar-in-context-skips-global
  ;; defvar executed in-context with no per-context cell creates one WITHOUT
  ;; touching the global cell.
  (let ((env (fresh-ctx-env)))
    (let ((ctx (make-ctx env :ctx-lib)))
      (with-ctx (ctx)
        (xev env '(defvar ctx-lib::*local-only* 77))
        (is (= 77 (xev env 'ctx-lib::*local-only*)))
        ;; defvar does not overwrite an existing per-context cell.
        (xev env '(defvar ctx-lib::*local-only* 99))
        (is (= 77 (xev env 'ctx-lib::*local-only*))))
      (is (not (xev env '(boundp 'ctx-lib::*local-only*)))
          "no global cell was created")
      (signals-error (xev env 'ctx-lib::*local-only*)))))

(deftest contexts-nesting-shadows-and-restores
  (let ((env (fresh-ctx-env)))
    (xev env '(defvar ctx-lib::*nest* 0))
    (let ((a (make-ctx env :ctx-lib))
          (b (make-ctx env :ctx-lib)))
      (with-ctx (a) (xev env '(setq ctx-lib::*nest* 1)))
      (with-ctx (b) (xev env '(setq ctx-lib::*nest* 2)))
      (with-ctx (a)
        (with-ctx (b)
          (is (= 2 (xev env 'ctx-lib::*nest*))
              "the inner context shadows the outer")
          (xev env '(setq ctx-lib::*nest* 22)))
        (is (= 1 (xev env 'ctx-lib::*nest*))
            "dynamic exit restores the outer context")
        (ignore-errors (with-ctx (b) (error "boom")))
        (is (= 1 (xev env 'ctx-lib::*nest*))
            "non-local exit also restores the outer context"))
      (is (= 22 (with-ctx (b) (xev env 'ctx-lib::*nest*))))
      (is (= 0 (xev env 'ctx-lib::*nest*))))))

(deftest contexts-functions-and-constants-shared
  ;; v1: function cells and defconstant cells are shared with the global
  ;; environment — a defun in one context is callable in the other (and at
  ;; toplevel), and a defconstant reads the same everywhere.
  (let ((env (fresh-ctx-env)))
    (xev env '(defconstant ctx-lib::+k+ 3))
    (let ((a (make-ctx env :ctx-lib))
          (b (make-ctx env :ctx-lib)))
      (with-ctx (a)
        (xev env '(defun ctx-lib::triple (x) (* x ctx-lib::+k+))))
      (is (= 12 (with-ctx (b) (xev env '(ctx-lib::triple 4)))))
      (is (= 12 (xev env '(ctx-lib::triple 4)))
          "a defun made in-context is visible globally")
      (is (= 3 (with-ctx (a) (xev env 'ctx-lib::+k+))))
      (is (= 3 (with-ctx (b) (xev env 'ctx-lib::+k+))))
      (is (= 3 (xev env 'ctx-lib::+k+))))))

(deftest contexts-thread-inherits-installing-context
  ;; Threads inherit the installing context at spawn, modeled by
  ;; call-in-fresh-thread-context capturing *current-context* at call time.
  (let ((env (fresh-ctx-env)))
    (xev env '(defvar ctx-lib::*tid* 0))
    (let ((ctx (make-ctx env :ctx-lib)))
      (with-ctx (ctx) (xev env '(setq ctx-lib::*tid* 111)))
      (is (= 111
             (with-ctx (ctx)
               (clef/proto/contexts:call-in-fresh-thread-context
                (lambda () (xev env 'ctx-lib::*tid*)))))
          "spawn captures the installing context")
      (with-ctx (ctx)
        (clef/proto/contexts:call-in-fresh-thread-context
         (lambda () (xev env '(setq ctx-lib::*tid* 222))))
        (is (= 222 (xev env 'ctx-lib::*tid*))
            "the inherited context is the same shared bundle of cells"))
      (is (= 0 (clef/proto/contexts:call-in-fresh-thread-context
                (lambda () (xev env 'ctx-lib::*tid*))))
          "spawn at toplevel captures the global context")
      (is (= 0 (xev env 'ctx-lib::*tid*))))))
