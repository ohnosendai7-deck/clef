;;;; run.lisp — test entry point.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.

(in-package #:clef-test)

(defun run-all ()
  "Run the full CLEF test suite. Returns the number of failures."
  (format t "~%Running CLEF test suite~%------------------------~%")
  (%run-all))
