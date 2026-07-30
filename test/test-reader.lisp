;;;; test-reader.lisp — tests for the CLEF clean-room reader (part 1).
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; NOTE ON PACKAGES: the reader currently interns symbols against the HOST
;;;; package system (see the SEAM comment in reader.lisp). A token like
;;;; "clef-test::a" below therefore resolves through the host's find-package —
;;;; correct per ANSI, and the seam gets swapped for first-class environments
;;;; in the cross-compilation reader.

(in-package #:clef-test)

(defun r (string)
  "Read one object from STRING with the standard readtable."
  (clef/reader:read-from-string string))

(defun rr (string rt)
  "Read one object from STRING with readtable RT."
  (let ((clef/reader:*readtable* rt))
    (clef/reader:read-from-string string)))

(deftest reader-lists
  (is (equal (r "(clef-test::a clef-test::b clef-test::c)") '(a b c)))
  (is (equal (r "(clef-test::a . clef-test::b)") '(a . b)))
  (is (equal (r "(clef-test::a clef-test::b . clef-test::c)") '(a b . c)))
  (is (equal (r "()") '()))
  (is (equal (r "(())") '(())))
  (is (equal (r "((1) (2 . 3) 4)") '((1) (2 . 3) 4)))
  (is (equal (r "( 1
2 ; comment
 3 )") '(1 2 3))))

(deftest reader-list-errors
  (signals-error (r "(a . b c)"))
  (signals-error (r "(a"))
  (signals-error (r ")")))

(deftest reader-quote
  (is (equal (r "'clef-test::a") '(quote a)))
  (is (equal (r "'(1 2)") '(quote (1 2))))
  (is (equal (r "''clef-test::a") '(quote (quote a)))))

(deftest reader-backquote
  (is (equal (r "`1") '(clef/reader:quasiquote 1)))
  (is (equal (r "`,1") '(clef/reader:quasiquote (clef/reader:unquote 1))))
  (is (equal (r "`(1 ,2 ,@3)")
             '(clef/reader:quasiquote (1 (clef/reader:unquote 2)
                                         (clef/reader:unquote-splicing 3)))))
  (is (equal (r "``(1 ,,2)")
             '(clef/reader:quasiquote
               (clef/reader:quasiquote (1 (clef/reader:unquote (clef/reader:unquote 2))))))))

(deftest reader-strings
  (is (string= (r "\"hello\"") "hello"))
  (is (string= (r "\"a\\\"b\\\\c\"") "a\"b\\c"))
  (is (string= (r "\"multi
line\"") (format nil "multi~%line")))
  (is (string= (r "\"\"") "")))

(deftest reader-integers
  (is (eql (r "42") 42))
  (is (eql (r "-17") -17))
  (is (eql (r "+99") 99))
  (is (eql (r "0") 0))
  (let ((clef/reader:*read-base* 16))
    (is (eql (r "ff") 255)))
  (let ((clef/reader:*read-base* 2))
    (is (eql (r "101") 5))))

(deftest reader-ratios
  (is (eql (r "3/4") 3/4))
  (is (eql (r "-1/2") -1/2)))

(deftest reader-floats
  (is (= (r "1.5") 1.5))
  (is (= (r "-2.5e3") -2500.0))
  (is (= (r "1.0d0") 1.0d0))
  (is (typep (r "1.5d2") (quote double-float)))
  (is (= (r "3f1") 30.0)))

(deftest reader-symbols
  (is (string= (symbol-name (r "foo")) "FOO"))   ; default upcase
  (is (string= (symbol-name (r "FOO")) "FOO"))
  (is (eq (r ":kw") :kw))
  (is (eq (r "cl:car") 'cl:car))
  (is (eq (r "clef-test::a") 'a))
  (is (string= (symbol-name (r "f|oo|bar")) "FooBAR"))
  (is (string= (symbol-name (r "f\\oobar")) "FoOBAR")))

(deftest reader-case-modes
  (let ((rt (clef/reader:copy-readtable)))
    (setf (clef/reader:readtable-case rt) :downcase)
    (is (string= (symbol-name (rr "FOO" rt)) "foo")))
  (let ((rt (clef/reader:copy-readtable)))
    (setf (clef/reader:readtable-case rt) :preserve)
    (is (string= (symbol-name (rr "FoO" rt)) "FoO")))
  (let ((rt (clef/reader:copy-readtable)))
    (setf (clef/reader:readtable-case rt) :invert)
    (is (string= (symbol-name (rr "FOO" rt)) "foo"))
    (is (string= (symbol-name (rr "foo" rt)) "FOO"))
    (is (string= (symbol-name (rr "Foo" rt)) "Foo"))))

(deftest reader-comments
  (is (eql (r "; just a comment
42") 42))
  (is (eql (r "1 ; trailing") 1)))

(deftest reader-read-suppress
  (let ((clef/reader:*read-suppress* t))
    (is (null (r "(a b)")))
    (is (null (r "\"str\"")))
    (is (null (r "`(a ,b ,@c)")))))

(deftest reader-from-string
  (multiple-value-bind (obj idx) (clef/reader:read-from-string "42 abc")
    (is (eql obj 42))
    (is (eql idx 3)))          ; consumed "42 " — index of 'a'
  (multiple-value-bind (obj idx) (clef/reader:read-from-string "  7" :start 2)
    (is (eql obj 7))
    (is (eql idx 3))))

(deftest reader-delimited-list
  (is (equal (with-input-from-string (s "1 2 3 ]")
               (clef/reader:read-delimited-list #\] s))
             '(1 2 3))))

(deftest reader-eof
  (is (eq (clef/reader:read-from-string "" nil :eof) :eof))
  (signals-error (r ""))
  (signals-error (r "\"unterminated")))
