;;;; clef.asd — CLEF system definitions.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;; To the extent possible under law, the author(s) have dedicated all
;;;; copyright and related and neighboring rights to this software to the
;;;; public domain worldwide. https://creativecommons.org/publicdomain/zero/1.0/

(asdf:defsystem "clef"
  :description "CLEF: an ANSI Common Lisp with a typed-effect IR, JIT-first, zero C."
  :author "CLEF contributors"
  :license "CC0-1.0 (public domain)"
  :version "0.0.0"
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:module "lap"
                              :serial t
                              :components ((:file "lap")))
                             (:module "elf"
                              :serial t
                              :components ((:file "elf")))
                             (:module "gc"
                              :serial t
                              :components ((:file "memref")
                                           (:file "heap")))
                             (:module "solver"
                              :serial t
                              :components ((:file "ukanren")
                                           (:file "bdd")))
                             (:module "proto"
                              :serial t
                              :components ((:file "package")
                                           (:file "env")
                                           (:file "ll")
                                           (:file "eval")
                                           (:file "builtins")
                                           (:file "clos")
                                           (:file "conditions")
                                           (:file "loop")
                                           (:file "image"))))))
  :in-order-to ((asdf:test-op (asdf:test-op "clef/test"))))

(asdf:defsystem "clef/test"
  :description "CLEF test suite."
  :license "CC0-1.0 (public domain)"
  :serial t
  :depends-on ("clef")
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "framework")
                             (:file "test-lap")
                             (:file "test-elf")
                             (:file "test-gc")
                             (:file "test-solver")
                             (:file "test-proto")
                             (:file "test-clos")
                             (:file "test-conditions")
                             (:file "test-loop")
                             (:file "conformance")
                             (:file "run"))))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :clef-test :run-all)))
