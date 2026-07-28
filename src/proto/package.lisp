;;;; package.lisp — CLEF-H (hosted prototype) package definitions.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; CLEF-H is the hosted prototype: a Common Lisp evaluator, CLOS subset,
;;;; condition system, and core library written in portable CL and run on the
;;;; SBCL bootstrap host in CLEF's own first-class environments (DESIGN.md §8).
;;;; It establishes conformance ahead of the native cold core; this code is
;;;; written to become target code when the cold core can host it.

(defpackage #:clef/proto/env
  (:use #:cl)
  (:shadow #:environment #:special-operator-p)
  (:export
   #:environment #:make-toplevel-env #:make-lexical-env
   #:env-parent
   ;; variables
   #:lookup-variable #:bind-variable #:set-variable-value #:variable-bound-p
   #:defconstant* #:defparameter* #:defvar*
   ;; functions
   #:lookup-function #:bind-function #:set-function-value #:function-bound-p
   ;; macros
   #:lookup-macro #:bind-macro #:make-macro #:macro-fn #:macro-function-p
   ;; special operators
   #:special-operator-p
   ;; symbol macros
   #:lookup-symbol-macro #:bind-symbol-macro
   ;; blocks / tags (bindings only; control via host throw/catch)
   #:bind-block #:lookup-block
   #:bind-tag #:lookup-tag
   ;; declarations
   #:proclaim-special #:special-variable-p))

(defpackage #:clef/proto/ll
  (:use #:cl)
  (:export #:parse-lambda-list #:bind-lambda-list #:lambda-list-keywords-p))

(defpackage #:clef/proto/eval
  (:use #:cl)
  (:export #:clef-eval #:clef-macroexpand #:clef-macroexpand-1
           #:clef-eval-seq #:make-clef-function
           #:clef-error #:clef-error-datum))

(defpackage #:clef/proto
  (:use #:cl)
  (:export #:repl #:run-file))
