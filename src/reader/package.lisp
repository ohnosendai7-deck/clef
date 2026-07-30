;;;; package.lisp — CLEF clean-room reader (ANSI §2, §23.2).
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.

(defpackage #:clef/reader
  (:use #:cl)
  (:shadow #:read #:read-preserving-whitespace #:read-from-string
           #:read-delimited-list #:*readtable* #:*read-base*
           #:*read-default-float-format* #:*read-suppress* #:*read-eval*
           #:copy-readtable #:set-macro-character #:get-macro-character
           #:make-dispatch-macro-character #:set-dispatch-macro-character
           #:get-dispatch-macro-character #:set-syntax-from-char
           #:readtable-case)
  (:export
   ;; conditions
   #:clef-reader-error
   ;; readtables
   #:clef-readtable #:*readtable* #:copy-readtable #:readtable-case
   #:set-macro-character #:get-macro-character
   #:make-dispatch-macro-character #:set-dispatch-macro-character
   #:get-dispatch-macro-character #:set-syntax-from-char
   ;; reader special vars
   #:*read-base* #:*read-default-float-format* #:*read-suppress* #:*read-eval*
   ;; entry points
   #:read #:read-preserving-whitespace #:read-from-string #:read-delimited-list
   ;; quasiquote representation (plain lists, interned here)
   #:quasiquote #:unquote #:unquote-splicing))
