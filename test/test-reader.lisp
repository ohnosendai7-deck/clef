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

;;; --- dispatch macros (ANSI §2.4.8) ---

(deftest reader-sharp-quote
  (is (equal (r "#'car") '(function car)))
  ;; structural check (host and clef readers intern into different packages
  ;; here; the cross-check is on shape + symbol names)
  (let ((form (r "#'(lambda (x) x)")))
    (is (eq (car form) 'function))
    (is (string= (symbol-name (car (second form))) "LAMBDA"))))

(deftest reader-sharp-vector
  (is (equalp (r "#(1 2 3)") #(1 2 3)))
  (is (equalp (r "#()") #()))
  ;; dotted list inside our #(...) is read as a list then coerced to vector
  (let ((v (r "#(a . (b))")))
    (is (and (vectorp v) (= (length v) 2)))))

(deftest reader-sharp-bitvector
  (is (equalp (r "#*1010") #*1010))
  (is (equalp (r "#*") #*))
  (is (equalp (r "#3*10") #*100))        ; last bit (0) repeated to length
  (is (equalp (r "#4*1") #*1111))        ; last bit (1) repeated
  (signals-error (r "#*12")))

(deftest reader-sharp-uninterned
  (let ((s (r "#:foo")))
    (is (symbolp s))
    (is (null (symbol-package s)))
    (is (string= (symbol-name s) "FOO"))))

(deftest reader-sharp-eval
  (is (eql (r "#.(+ 1 2)") 3))
  (let ((clef/reader:*read-eval* nil))
    (signals-error (r "#.(+ 1 2)"))))

(deftest reader-sharp-block-comment
  (is (eql (r "#| comment |# 2") 2))
  (is (eql (r "#| outer #| inner |# still comment |# 42") 42))
  (signals-error (r "#| unterminated")))

(deftest reader-sharp-features
  (let ((clef/reader:*features* '(:clef-test-feat)))
    (is (eql (r "#+:clef-test-feat 42") 42))
    (is (eql (r "#-:clef-test-feat 7 42") 42))     ; 7 skipped, 42 read
    (is (eql (r "#+(and :clef-test-feat) 42") 42))
    (is (eql (r "#+(or :no-such-feat :clef-test-feat) 42") 42))
    (is (eql (r "#+(not :clef-test-feat) 7 42") 42))
    (is (eql (r "#+:no-such-feat 7 42") 42))))

(deftest reader-sharp-complex
  (is (eql (r "#C(1 2)") #C(1 2)))
  (is (eql (r "#C(0 1)") #C(0 1)))
  (signals-error (r "#C(1 2 3)")))

(deftest reader-sharp-labels
  (let ((x (r "#1=(a b . #1#)")))
    (is (eq x (cddr x))))
  (is (equal (r "(#2=(1) #2#)") '((1) (1))))
  (signals-error (r "#9#")))

(deftest reader-sharp-radix
  (is (eql (r "#xFF") 255))
  (is (eql (r "#o17") 15))
  (is (eql (r "#b101") 5))
  (is (eql (r "#x-10") -16))
  (is (eql (r "#16rFF") 255))
  (is (eql (r "#2r101") 5))
  (is (eql (r "#36rZ") 35))
  (signals-error (r "#2r12"))
  (signals-error (r "#37r10")))

(deftest reader-sharp-char
  (is (char= (r "#\\a") #\a))
  (is (char= (r "#\\A") #\A))
  (is (char= (r "#\\Newline") #\Newline))
  (is (char= (r "#\\newline") #\Newline))
  (is (char= (r "#\\Space") #\Space))
  (is (char= (r "#\\(") #\())
  (signals-error (r "#\\Bogus")))

(deftest reader-sharp-unknown
  (signals-error (r "#q12")))

;;; --- source locations ---

(deftest reader-locations-conses
  (let* ((form (r "(1 (2 3)
4)"))
         (loc (clef/reader:form-source-location form)))
    (is (clef/reader:source-location-p loc))
    (is (eql (clef/reader:source-location-line loc) 1))
    (is (eql (clef/reader:source-location-column loc) 1))
    (is (eql (clef/reader:source-location-end-line loc) 2))
    ;; inner form (2 3) starts at line 1 col 4
    (let ((inner-loc (clef/reader:form-source-location (second form))))
      (is (clef/reader:source-location-p inner-loc))
      (is (eql (clef/reader:source-location-line inner-loc) 1))
      (is (eql (clef/reader:source-location-column inner-loc) 4)))))

(deftest reader-locations-multiline
  (let* ((form (r "; header comment
(alpha
  (beta 2)
  gamma)"))
         (inner (second form))
         (inner-loc (clef/reader:form-source-location inner)))
    (is (clef/reader:source-location-p inner-loc))
    (is (eql (clef/reader:source-location-line inner-loc) 3))
    (is (eql (clef/reader:source-location-column inner-loc) 3))))

(deftest reader-locations-atoms
  ;; strings get locations
  (is (clef/reader:source-location-p
       (clef/reader:form-source-location (r "\"hello\""))))
  ;; uninterned symbols get locations
  (is (clef/reader:source-location-p
       (clef/reader:form-source-location (r "#:sym"))))
  ;; fixnums do NOT (immediates; EQ identity useless)
  (is (null (clef/reader:form-source-location (r "42"))))
  ;; unattached objects return nil
  (is (null (clef/reader:form-source-location (list 1 2 3)))))
