;;;; sign.lisp — JAR v1 (APK v1) signing in Common Lisp.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; Implements the host-verifiable JAR v1 signature scheme used for APKs:
;;;;
;;;;   META-INF/MANIFEST.MF  — SHA-256 digest (base64) of each entry's bytes
;;;;   META-INF/CLEF.SF      — SHA-256 digest of MANIFEST.MF and of each of
;;;;                           its per-entry sections
;;;;   META-INF/CLEF.RSA     — a PKCS#7 (RFC 2315) SignedData blob over the
;;;;                           CLEF.SF bytes, signed RSA-PKCS#1-v1.5 with
;;;;                           SHA-256, embedding the signer's X.509 cert
;;;;
;;;; Clean-room: SHA-256 from FIPS 180-4, base64 from RFC 4648, RSA/EMSA
;;;; from PKCS#1 (RFC 8017), PKCS#7 from RFC 2315, DER from ITU-T X.690,
;;;; manifest format from the public JAR manifest specification. No C, no
;;;; openssl, no ironclad — pure Common Lisp (native bignums).
;;;;
;;;; The SignerInfo carries no authenticated attributes, so the signature is
;;;; computed directly over the CLEF.SF content bytes (RFC 2315 §9.2-9.3).

(in-package #:clef/android)

;;; ======================================================================
;;; SHA-256 (FIPS 180-4)
;;; ======================================================================

(defparameter +sha256-k+
  (make-array
   64 :element-type '(unsigned-byte 32)
   :initial-contents
   '(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1
     #x923f82a4 #xab1c5ed5 #xd807aa98 #x12835b01 #x243185be #x550c7dc3
     #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174 #xe49b69c1 #xefbe4786
     #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
     #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147
     #x06ca6351 #x14292967 #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
     #x650a7354 #x766a0abb #x81c2c92e #x92722c85 #xa2bfe8a1 #xa81a664b
     #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
     #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a
     #x5b9cca4f #x682e6ff3 #x748f82ee #x78a5636f #x84c87814 #x8cc70208
     #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))
  "The sixty-four round constants K (FIPS 180-4 §4.2.2).")

(defparameter +sha256-h0+
  (make-array 8 :element-type '(unsigned-byte 32)
              :initial-contents
              '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))
  "The initial hash value H(0) (FIPS 180-4 §5.3.3).")

(declaim (inline u32 rotr32))
(defun u32 (n) (logand n #xFFFFFFFF))
(defun rotr32 (x n)
  (logior (ash x (- n)) (u32 (ash x (- 32 n)))))

(defun sha256-pad (message)
  "Append the FIPS 180-4 padding to MESSAGE; return a new byte vector whose
length is a multiple of 64."
  (let* ((len (length message))
         (bits (* 8 len))
         (padded-len (* 64 (ceiling (+ len 1 8) 64)))
         (out (make-bytevec padded-len)))
    (replace out message)
    (setf (aref out len) #x80)
    (dotimes (i 8)
      (setf (aref out (+ padded-len -8 i))
            (logand (ash bits (* -8 (- 7 i))) #xFF)))
    out))

(defun sha256 (message)
  "SHA-256 of MESSAGE (a byte vector); returns the 32-byte digest."
  (let ((h (make-array 8 :element-type '(unsigned-byte 32)
                       :initial-contents +sha256-h0+))
        (w (make-array 64 :element-type '(unsigned-byte 32)))
        (padded (sha256-pad message)))
    (macrolet ((bsig0 (x) `(logxor (rotr32 ,x 2) (rotr32 ,x 13) (rotr32 ,x 22)))
               (bsig1 (x) `(logxor (rotr32 ,x 6) (rotr32 ,x 11) (rotr32 ,x 25)))
               (ssig0 (x) `(logxor (rotr32 ,x 7) (rotr32 ,x 18) (ash ,x -3)))
               (ssig1 (x) `(logxor (rotr32 ,x 17) (rotr32 ,x 19) (ash ,x -10))))
      (loop for block from 0 below (length padded) by 64
            do (dotimes (tt 16)
                 (let ((i (+ block (* 4 tt))))
                   (setf (aref w tt)
                         (logior (ash (aref padded i) 24)
                                 (ash (aref padded (+ i 1)) 16)
                                 (ash (aref padded (+ i 2)) 8)
                                 (aref padded (+ i 3))))))
               (loop for tt from 16 below 64
                     do (setf (aref w tt)
                              (u32 (+ (ssig1 (aref w (- tt 2)))
                                      (aref w (- tt 7))
                                      (ssig0 (aref w (- tt 15)))
                                      (aref w (- tt 16))))))
               (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
                     (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
                 (dotimes (tt 64)
                   (let ((t1 (u32 (+ hh (bsig1 e)
                                     (logxor (logand e f) (logand (lognot e) g))
                                     (aref +sha256-k+ tt) (aref w tt))))
                         (t2 (u32 (+ (bsig0 a)
                                     (logxor (logand a b) (logand a c)
                                             (logand b c))))))
                     (setf hh g g f f e e (u32 (+ d t1))
                           d c c b b a a (u32 (+ t1 t2)))))
                 (setf (aref h 0) (u32 (+ (aref h 0) a))
                       (aref h 1) (u32 (+ (aref h 1) b))
                       (aref h 2) (u32 (+ (aref h 2) c))
                       (aref h 3) (u32 (+ (aref h 3) d))
                       (aref h 4) (u32 (+ (aref h 4) e))
                       (aref h 5) (u32 (+ (aref h 5) f))
                       (aref h 6) (u32 (+ (aref h 6) g))
                       (aref h 7) (u32 (+ (aref h 7) hh))))))
    (let ((out (make-bytevec 32)))
      (dotimes (i 8 out)
        (dotimes (j 4)
          (setf (aref out (+ (* 4 i) j))
                (logand (ash (aref h i) (* -8 (- 3 j))) #xFF)))))))

;;; ======================================================================
;;; base64 (RFC 4648, standard alphabet, with padding)
;;; ======================================================================

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun base64-encode (bytes)
  "Base64-encode BYTES (RFC 4648 §4) and return a string."
  (let* ((len (length bytes))
         (out (make-string (* 4 (ceiling len 3))))
         (o 0))
    (loop for i from 0 below len by 3
          for b0 = (aref bytes i)
          for b1 = (if (< (+ i 1) len) (aref bytes (+ i 1)) 0)
          for b2 = (if (< (+ i 2) len) (aref bytes (+ i 2)) 0)
          for n = (logior (ash b0 16) (ash b1 8) b2)
          do (setf (char out o) (char +base64-alphabet+ (logand (ash n -18) #x3F))
                   (char out (+ o 1)) (char +base64-alphabet+ (logand (ash n -12) #x3F))
                   (char out (+ o 2)) (if (< (+ i 1) len)
                                          (char +base64-alphabet+ (logand (ash n -6) #x3F))
                                          #\=)
                   (char out (+ o 3)) (if (< (+ i 2) len)
                                          (char +base64-alphabet+ (logand n #x3F))
                                          #\=))
             (incf o 4))
    out))

;;; ======================================================================
;;; Minimal DER (ITU-T X.690): writer + reader
;;; ======================================================================

;;; --- writer ---

(defun der-length (n)
  "DER-encode a content length N."
  (cond ((< n #x80)
         (make-array 1 :element-type '(unsigned-byte 8) :initial-element n))
        (t (let ((bytes '()))
             (loop while (plusp n) do (push (logand n #xFF) bytes) (setf n (ash n -8)))
             (bv-concat
              (make-array 1 :element-type '(unsigned-byte 8)
                          :initial-element (logior #x80 (length bytes)))
              (map '(simple-array (unsigned-byte 8) (*)) #'identity bytes))))))

(defun der-tlv (tag content)
  "A DER TLV with the given single-octet TAG and CONTENT bytes."
  (bv-concat (make-array 1 :element-type '(unsigned-byte 8) :initial-element tag)
             (der-length (length content))
             content))

(defun der-integer (n)
  "DER-encode the non-negative integer N (two's complement, minimal)."
  (let ((bytes (if (zerop n) '(0) '())))
    (loop while (plusp n) do (push (logand n #xFF) bytes) (setf n (ash n -8)))
    (when (>= (first bytes) #x80) (push 0 bytes))
    (der-tlv #x02 (map '(simple-array (unsigned-byte 8) (*)) #'identity bytes))))

(defun der-oid (arcs)
  "DER-encode the object identifier given as a list of integer ARCS."
  (let ((out (list (+ (* 40 (first arcs)) (second arcs)))))
    (dolist (arc (cddr arcs))
      (cond ((zerop arc) (push 0 out))
            (t (let ((chunks '()))
                 (loop while (plusp arc)
                       do (push (logand arc #x7F) chunks) (setf arc (ash arc -7)))
                 (loop for c in chunks for first = t then nil
                       do (push (if first c (logior c #x80)) out))))))
    (der-tlv #x06 (map '(simple-array (unsigned-byte 8) (*)) #'identity (nreverse out)))))

(defun der-null () (der-tlv #x05 (make-bytevec 0)))
(defun der-sequence (&rest parts) (der-tlv #x30 (apply #'bv-concat parts)))
(defun der-set (&rest parts) (der-tlv #x31 (apply #'bv-concat parts)))
(defun der-octet-string (bytes) (der-tlv #x04 bytes))
(defun der-bit-string (bytes)
  (der-tlv #x03 (bv-concat (make-array 1 :element-type '(unsigned-byte 8)
                                       :initial-element 0)
                           bytes)))
(defun der-context (n content)
  "A context-specific constructed tag [N] wrapping CONTENT (implicit)."
  (der-tlv (logior #xA0 n) content))

;;; --- reader ---

(defstruct (der-node (:constructor %make-der-node))
  tag                                   ; integer tag octet
  content                               ; content octets (primitive nodes)
  children                              ; list of DER-NODE (constructed)
  raw)                                  ; the full TLV octets

(defun der-read-length (bytes pos)
  "Read a DER length at POS; return (values length content-start)."
  (let ((first (aref bytes pos)))
    (if (< first #x80)
        (values first (1+ pos))
        (let ((nbytes (logand first #x7F))
              (len 0))
          (dotimes (i nbytes)
            (setf len (logior (ash len 8) (aref bytes (+ pos 1 i)))))
          (values len (+ pos 1 nbytes))))))

(defun der-parse (bytes &optional (start 0))
  "Parse one DER TLV from BYTES at START; return (values node next-pos)."
  (let* ((tag (aref bytes start)))
    (multiple-value-bind (len cstart) (der-read-length bytes (1+ start))
      (let* ((cend (+ cstart len))
             (content (zslice bytes cstart len))
             (children (when (logbitp 5 tag)
                         (loop with pos = cstart
                               while (< pos cend)
                               collect (multiple-value-bind (node next)
                                           (der-parse bytes pos)
                                         (setf pos next)
                                         node)))))
        (values (%make-der-node :tag tag :content content :children children
                                :raw (zslice bytes start (- cend start)))
                cend)))))

(defun der-integer-value (bytes)
  "Interpret BYTES (big-endian magnitude, as in a DER INTEGER's content)
as a non-negative integer."
  (let ((n 0))
    (loop for b across bytes do (setf n (logior (ash n 8) b)))
    n))

;;; ======================================================================
;;; RSA (PKCS#1 / RFC 8017): RSASP1/RSAVP1 with EMSA-PKCS1-v1_5, SHA-256
;;; ======================================================================

(defstruct (rsa-private-key (:constructor make-rsa-private-key (&key n e d)))
  n e d)
(defstruct (rsa-public-key (:constructor make-rsa-public-key (&key n e)))
  n e)

(defun mod-expt (base exponent modulus)
  "(BASE ^ EXPONENT) mod MODULUS, by square-and-multiply."
  (when (or (minusp base) (minusp exponent))
    (error "MOD-EXPT requires non-negative BASE and EXPONENT"))
  (if (= modulus 1)
      0
      (let ((result 1)
            (b (mod base modulus)))
        (loop while (plusp exponent)
              do (when (oddp exponent)
                   (setf result (mod (* result b) modulus)))
                 (setf b (mod (* b b) modulus)
                       exponent (ash exponent -1)))
        result)))

(defparameter +sha256-digestinfo-prefix+
  (map '(simple-array (unsigned-byte 8) (*)) #'identity
       '(#x30 #x31 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03
         #x04 #x02 #x01 #x05 #x00 #x04 #x20))
  "DER prefix of the SHA-256 DigestInfo (RFC 8017 §9.2 note 1).")

(defun emsa-pkcs1-v1.5-encode (digest em-len)
  "EMSA-PKCS1-v1_5-ENCODE (RFC 8017 §9.2): EM = 00 01 PS 00 T, where T is
DigestInfo(SHA-256) || DIGEST and PS is #xFF octets."
  (let* ((t-len (+ (length +sha256-digestinfo-prefix+) (length digest)))
         (ps-len (- em-len t-len 3)))
    (when (< ps-len 8)
      (error "Intended encoded message length too short"))
    (let ((em (make-bytevec em-len)))
      (setf (aref em 0) #x00 (aref em 1) #x01)
      (loop for i from 2 below (+ 2 ps-len) do (setf (aref em i) #xFF))
      (setf (aref em (+ 2 ps-len)) #x00)
      (replace em +sha256-digestinfo-prefix+ :start1 (+ 3 ps-len))
      (replace em digest :start1 (+ 3 ps-len (length +sha256-digestinfo-prefix+)))
      em)))

(defun rsa-sign-sha256 (key message)
  "Sign MESSAGE with RSA private KEY using PKCS#1 v1.5 over SHA-256.
Returns the signature octet string (same length as the modulus)."
  (let* ((k (ceiling (integer-length (rsa-private-key-n key)) 8))
         (em (emsa-pkcs1-v1.5-encode (sha256 message) k))
         (m (der-integer-value em))
         (s (mod-expt m (rsa-private-key-d key) (rsa-private-key-n key)))
         (out (make-bytevec k)))
    (dotimes (i k out)
      (setf (aref out (- k 1 i)) (logand s #xFF)
            s (ash s -8)))))

(defun rsa-verify-sha256 (key message signature)
  "Verify SIGNATURE on MESSAGE with RSA public KEY (PKCS#1 v1.5, SHA-256)."
  (let ((k (ceiling (integer-length (rsa-public-key-n key)) 8)))
    (and (= (length signature) k)
         (let* ((s (der-integer-value signature))
                (m (mod-expt s (rsa-public-key-e key) (rsa-public-key-n key)))
                (em (emsa-pkcs1-v1.5-encode (sha256 message) k)))
           (loop for i below k
                 always (= (aref em i)
                           (logand (ash m (* -8 (- k 1 i))) #xFF)))))))

;;; --- key / certificate loading ---

(defun read-octet-source (source what)
  "Resolve SOURCE (a byte vector, pathname, or namestring) to a byte vector."
  (etypecase source
    ((vector (unsigned-byte 8))
     (map '(simple-array (unsigned-byte 8) (*)) #'identity source))
    ((or pathname string)
     (with-open-file (in source :element-type '(unsigned-byte 8))
       (let ((out (make-bytevec (file-length in))))
         (read-sequence out in)
         out)))))

(defun parse-rsa-private-key (bytes)
  "Parse an RSA private key from DER BYTES: either a PKCS#1 RSAPrivateKey
or a PKCS#8 PrivateKeyInfo wrapping one. Returns an RSA-PRIVATE-KEY."
  (let ((top (der-parse bytes)))
    (unless (= (der-node-tag top) #x30)
      (error "Not a DER SEQUENCE — unrecognised key format"))
    (let ((children (der-node-children top)))
      (cond
        ;; PKCS#8 PrivateKeyInfo: SEQUENCE { version, algorithm, OCTET STRING }
        ((and (= (length children) 3)
              (= (der-node-tag (second children)) #x30)
              (= (der-node-tag (third children)) #x04))
         (parse-rsa-private-key (der-node-content (third children))))
        ;; PKCS#1 RSAPrivateKey: SEQUENCE { version, n, e, d, p, q, ... }
        ((and (>= (length children) 4)
              (every (lambda (c) (= (der-node-tag c) #x02))
                     (subseq children 0 4)))
         (make-rsa-private-key
          :n (der-integer-value (der-node-content (second children)))
          :e (der-integer-value (der-node-content (third children)))
          :d (der-integer-value (der-node-content (fourth children)))))
        (t (error "Unrecognised RSA private key format"))))))

(defun load-rsa-private-key (source)
  "Load an RSA private key from an RSA-PRIVATE-KEY (returned as-is), DER
byte vector, or file path."
  (if (rsa-private-key-p source)
      source
      (parse-rsa-private-key (read-octet-source source "private key"))))

(defun cert-issuer-and-serial (cert-der)
  "Extract the issuer Name TLV and serialNumber INTEGER TLV (both raw DER)
from an X.509 certificate. Only this much of X.509 is needed for the
PKCS#7 IssuerAndSerialNumber."
  (let ((cert (der-parse cert-der)))
    (unless (and (= (der-node-tag cert) #x30) (der-node-children cert))
      (error "Not a DER SEQUENCE — unrecognised certificate"))
    (let* ((tbs (first (der-node-children cert)))
           (fields (der-node-children tbs)))
      ;; Skip the optional [0] EXPLICIT version; serial is the first
      ;; INTEGER, issuer the first SEQUENCE after it.
      (let ((serial (find-if (lambda (c) (= (der-node-tag c) #x02)) fields)))
        (unless serial (error "Certificate has no serialNumber"))
        (let ((issuer (find-if (lambda (c) (= (der-node-tag c) #x30))
                               (cdr (member serial fields)))))
          (unless issuer (error "Certificate has no issuer Name"))
          (values (der-node-raw issuer) (der-node-raw serial)))))))

;;; ======================================================================
;;; PKCS#7 SignedData (RFC 2315) — the CLEF.RSA signature block
;;; ======================================================================

(defparameter +oid-sha256+ '(2 16 840 1 101 3 4 2 1))
(defparameter +oid-rsa-encryption+ '(1 2 840 113549 1 1 1))
(defparameter +oid-data+ '(1 2 840 113549 1 7 1))
(defparameter +oid-signed-data+ '(1 2 840 113549 1 7 2))

(defun algid (oid)
  "An AlgorithmIdentifier: SEQUENCE { OID, NULL }."
  (der-sequence (der-oid oid) (der-null)))

(defun pkcs7-signed-data (content key cert-der)
  "Build a PKCS#7 SignedData (RFC 2315 §9) over CONTENT, signed with RSA
private KEY (PKCS#1 v1.5 + SHA-256), embedding the X.509 certificate
CERT-DER. Detached content, no authenticated attributes."
  (let ((signature (rsa-sign-sha256 key content)))
    (multiple-value-bind (issuer serial) (cert-issuer-and-serial cert-der)
      (let* ((signer-info
              (der-sequence
               (der-integer 1)                       ; version
               (der-sequence issuer serial)          ; issuerAndSerialNumber
               (algid +oid-sha256+)                  ; digestAlgorithm
               (algid +oid-rsa-encryption+)          ; digestEncryptionAlgorithm
               (der-octet-string signature)))        ; encryptedDigest
            (signed-data-body
              (bv-concat
               (der-integer 1)                        ; version
               (der-set (algid +oid-sha256+))         ; digestAlgorithms
               (der-sequence (der-oid +oid-data+))    ; contentInfo (detached)
               (der-context 0 cert-der)               ; certificates [0]
               (der-set signer-info))))               ; signerInfos
        (der-sequence
         (der-oid +oid-signed-data+)
         (der-context 0 (der-tlv #x30 signed-data-body)))))))

;;; ======================================================================
;;; JAR manifests (MANIFEST.MF / CLEF.SF)
;;; ======================================================================

(defun crlf ()
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code '(#\Return #\Newline)))

(defun ascii-bytes (string)
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code string))

(defun wrap-manifest-line (line)
  "Wrap a logical manifest LINE at 72 octets per the JAR manifest spec:
continuation lines begin with a single space. Returns a list of strings."
  (if (<= (length line) 72)
      (list line)
      (cons (subseq line 0 72)
            (wrap-manifest-line (concatenate 'string " " (subseq line 72))))))

(defun manifest-section-bytes (attributes)
  "Serialise one manifest section: ATTRIBUTES is an alist of (name . value)
strings. Each logical line is CRLF-terminated and 72-octet wrapped."
  (let ((parts '()))
    (dolist (attr attributes)
      (dolist (physical (wrap-manifest-line
                         (concatenate 'string (car attr) ": " (cdr attr))))
        (push (ascii-bytes physical) parts)
        (push (crlf) parts)))
    (push (crlf) parts)                     ; blank line ends the section
    (apply #'bv-concat (nreverse parts))))

(defun make-manifest-mf (entries)
  "Build MANIFEST.MF for ENTRIES (a list of ZENTRY). Returns (values
manifest-bytes sections) where SECTIONS is an alist (name . section-bytes)
of each per-entry section exactly as it appears in the manifest."
  (let ((main (manifest-section-bytes
               `(("Manifest-Version" . "1.0")
                 ("Created-By" . "1.0 (CLEF)"))))
        (sections '()))
    (dolist (entry entries)
      (push (cons (zentry-name entry)
                  (manifest-section-bytes
                   `(("Name" . ,(zentry-name entry))
                     ("SHA-256-Digest" . ,(base64-encode
                                           (sha256 (zentry-data entry)))))))
            sections))
    (setf sections (nreverse sections))
    (values (apply #'bv-concat main (mapcar #'cdr sections))
            sections)))

(defun make-signature-file (manifest-bytes sections)
  "Build CLEF.SF: the whole-manifest digest plus a digest of each of
MANIFEST.MF's per-entry sections (SECTIONS as returned by
MAKE-MANIFEST-MF)."
  (let ((main (manifest-section-bytes
               `(("Signature-Version" . "1.0")
                 ("Created-By" . "1.0 (CLEF)")
                 ("SHA-256-Digest-Manifest" . ,(base64-encode
                                                (sha256 manifest-bytes)))))))
    (apply #'bv-concat
           main
           (mapcar (lambda (section)
                     (manifest-section-bytes
                      `(("Name" . ,(car section))
                        ("SHA-256-Digest" . ,(base64-encode
                                              (sha256 (cdr section)))))))
                   sections))))

;;; ======================================================================
;;; sign-apk
;;; ======================================================================

(defun signing-file-p (name)
  "True if NAME is a META-INF signature-related file to strip on re-signing."
  (and (>= (length name) 9)
       (string-equal name "META-INF/" :end1 9)
       (let ((rest (subseq name 9)))
         (or (string-equal rest "MANIFEST.MF")
             (let ((dot (position #\. rest :from-end t)))
               (and dot
                    (member (string-upcase (subseq rest dot))
                            '(".SF" ".RSA" ".DSA" ".EC") :test #'string=)))))))

(defun sign-apk (apk-bytes &key private-key certificate)
  "Sign the unsigned APK byte vector APK-BYTES with RSA PRIVATE-KEY and
X.509 CERTIFICATE (each a byte vector of DER, or a path to a DER file;
PRIVATE-KEY may also be an RSA-PRIVATE-KEY). Returns a new, signed APK
byte vector with META-INF/MANIFEST.MF, META-INF/CLEF.SF and
META-INF/CLEF.RSA prepended. Signals an error if the key or certificate
is missing or malformed."
  (unless private-key
    (error "RSA private key required to sign an APK"))
  (unless certificate
    (error "X.509 certificate required to sign an APK"))
  (let* ((key (load-rsa-private-key private-key))
         (cert (read-octet-source certificate "certificate"))
         (entries (remove-if (lambda (e) (signing-file-p (zentry-name e)))
                             (read-zip apk-bytes))))
    (multiple-value-bind (manifest sections) (make-manifest-mf entries)
      (let* ((sf (make-signature-file manifest sections))
             (rsa (pkcs7-signed-data sf key cert))
             (meta-inf (list (make-entry "META-INF/MANIFEST.MF" manifest)
                             (make-entry "META-INF/CLEF.SF" sf)
                             (make-entry "META-INF/CLEF.RSA" rsa))))
        (write-zip (append meta-inf entries))))))
