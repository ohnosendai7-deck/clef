;;;; gen-test-key.lisp — generate the throwaway RSA test key + self-signed
;;;; X.509 certificate embedded as constants in test/test-android.lisp.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; This is a DEVELOPER tool, not part of the build or the test suite. It
;;;; uses only clef/android's own DER writer and RSA signer, so the test
;;;; fixtures exercise no foreign code. The key is 1024-bit RSA — fine for
;;;; tests, not for production. Run on SBCL:
;;;;
;;;;   sbcl --non-interactive --load tools/dev/gen-test-key.lisp
;;;;
;;;; The random state is fixed, so the output is reproducible.

(require :asdf)
(asdf:load-asd (truename "clef.asd"))
(asdf:load-system :clef)

(in-package #:cl-user)

(defparameter *rs* (make-random-state nil)) ; deterministic default state

(defun random-bits (bits rs)
  (random (ash 1 bits) rs))

(defun miller-rabin-pass (n a)
  "One Miller-Rabin round: true if N is a probable prime to base A."
  (let* ((n-1 (1- n))
         (d n-1)
         (r 0))
    (loop while (evenp d) do (setf d (ash d -1)) (incf r))
    (let ((x (clef/android::mod-expt a d n)))
      (or (= x 1) (= x n-1)
          (loop repeat (1- r)
                do (setf x (mod (* x x) n))
                   (when (= x n-1) (return t))
                finally (return nil))))))

(defun probable-prime-p (n &optional (rounds 40))
  (and (> n 3) (oddp n)
       (loop for a = (+ 2 (random (- n 3) *rs*))
             repeat rounds
             always (miller-rabin-pass n a))))

(defun generate-prime (bits)
  (loop for candidate = (logior (random-bits (1- bits) *rs*)
                                (ash 1 (1- bits)) 1)
        when (and (loop for p in '(3 5 7 11 13 17 19 23 29 31 37 41 43 47)
                        never (zerop (mod candidate p)))
                  (probable-prime-p candidate))
          do (return candidate)))

(defun extended-gcd (a b)
  "Return (values g x y) with g = ax + by."
  (if (zerop b)
      (values a 1 0)
      (multiple-value-bind (g x1 y1) (extended-gcd b (mod a b))
        (values g y1 (- x1 (* (floor a b) y1))))))

(defun mod-inverse (a m)
  (multiple-value-bind (g x y) (extended-gcd a m)
    (declare (ignore y))
    (assert (= g 1))
    (mod x m)))

;;; --- generate a 1024-bit RSA key ---

(format t "Generating 1024-bit RSA test key...~%")
(let* ((p (generate-prime 512))
       (q (loop for q = (generate-prime 512) unless (= q p) do (return q)))
       (n (* p q))
       (e 65537)
       (lambda-n (lcm (1- p) (1- q)))
       (d (mod-inverse e lambda-n))
       (dp (mod d (1- p)))
       (dq (mod d (1- q)))
       (qinv (mod-inverse q p))
       (pkcs1 (clef/android::der-sequence
               (clef/android::der-integer 0)
               (clef/android::der-integer n)
               (clef/android::der-integer e)
               (clef/android::der-integer d)
               (clef/android::der-integer p)
               (clef/android::der-integer q)
               (clef/android::der-integer dp)
               (clef/android::der-integer dq)
               (clef/android::der-integer qinv))))
  ;; --- mint a self-signed X.509 certificate with our own signer ---
  (flet ((name (cn)
           (clef/android::der-sequence
            (clef/android::der-set
             (clef/android::der-sequence
              (clef/android::der-oid '(2 5 4 3))      ; commonName
              (clef/android::der-tlv #x0C (map '(vector (unsigned-byte 8))
                                               #'char-code cn))))))
         (utc (s) (clef/android::der-tlv #x17 (map '(vector (unsigned-byte 8))
                                                   #'char-code s))))
    (let* ((spki (clef/android::der-sequence
                  (clef/android::der-sequence
                   (clef/android::der-oid '(1 2 840 113549 1 1 1))
                   (clef/android::der-null))
                  (clef/android::der-bit-string
                   (clef/android::der-sequence
                    (clef/android::der-integer n)
                    (clef/android::der-integer e)))))
           (tbs (clef/android::der-sequence
                 (clef/android::der-context 0 (clef/android::der-integer 2)) ; v3
                 (clef/android::der-integer 1)      ; serialNumber
                 (clef/android::der-sequence        ; signature algid
                  (clef/android::der-oid '(1 2 840 113549 1 1 11))
                  (clef/android::der-null))
                 (name "CLEF Test")                 ; issuer
                 (clef/android::der-sequence        ; validity
                  (utc "250101000000Z") (utc "450101000000Z"))
                 (name "CLEF Test")                 ; subject
                 spki))
           (key (clef/android:make-rsa-private-key :n n :e e :d d))
           (tbs-raw (let ((node (clef/android:der-parse tbs)))
                      (clef/android:der-node-raw node)))
           (sig (clef/android:rsa-sign-sha256 key tbs-raw))
           (cert (clef/android::der-sequence
                  tbs-raw
                  (clef/android::der-sequence
                   (clef/android::der-oid '(1 2 840 113549 1 1 11))
                   (clef/android::der-null))
                  (clef/android::der-bit-string sig))))
      (format t "~%;;; --- paste into test/test-android.lisp ---~%")
      (format t "~%(defparameter *test-key-der*~%  (coerce '~s '(simple-array (unsigned-byte 8) (*))))~%"
              (coerce pkcs1 'list))
      (format t "~%(defparameter *test-cert-der*~%  (coerce '~s '(simple-array (unsigned-byte 8) (*))))~%"
              (coerce cert 'list))
      (format t "~%(defparameter *test-key-n* ~d)~%" n)
      (format t "(defparameter *test-key-e* ~d)~%(defparameter *test-key-d* ~d)~%" e d))))
