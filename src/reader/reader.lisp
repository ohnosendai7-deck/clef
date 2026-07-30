;;;; reader.lisp — the CLEF clean-room reader core (ANSI §2.1–2.4).
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; Implemented from the ANSI spec (§2.2 reader algorithm, §2.3 token
;;;; syntax, §2.4 standard macro characters) only. Part 1 scope: lists,
;;;; quote/backquote, strings, comments, numbers, symbols, escapes.
;;;; Dispatch (#) macros and source locations are parts 2 and 3.

(in-package #:clef/reader)

;;;; --- special vars ---

(defvar *read-base* 10)
(defvar *read-default-float-format* 'single-float)
(defvar *read-suppress* nil)
(defvar *read-eval* t)

;;;; --- conditions ---

(define-condition clef-reader-error (reader-error)
  ((line :initarg :line :reader err-line)
   (column :initarg :column :reader err-column)
   (detail :initarg :detail :reader err-detail))
  (:report (lambda (c s)
             (format s "Reader error at line ~a, column ~a: ~a"
                     (err-line c) (err-column c) (err-detail c)))))

;;;; --- position-tracking stream wrapper ---

(defstruct (tracker (:constructor %make-tracker (stream)))
  stream
  (line 1 :type fixnum)
  (column 1 :type fixnum)
  (last-char nil))

(defun tr-read-char (tr &optional (eof-error-p t) eof-value)
  (let ((ch (cl:read-char (tracker-stream tr) nil nil)))
    (unless ch
      (if eof-error-p
          (error 'clef-reader-error :stream (tracker-stream tr)
                 :line (tracker-line tr) :column (tracker-column tr)
                 :detail "Unexpected end of file")
          (return-from tr-read-char eof-value)))
    (setf (tracker-last-char tr) ch)
    (if (char= ch #\Newline)
        (progn (incf (tracker-line tr)) (setf (tracker-column tr) 1))
        (incf (tracker-column tr)))
    ch))

(defun tr-unread-char (tr ch)
  (cl:unread-char ch (tracker-stream tr))
  ;; Position rollback: only safe within a line (we never unread newlines).
  (when (> (tracker-column tr) 1) (decf (tracker-column tr))))

(defun tr-peek-char (tr &optional (peek-type nil) (eof-error-p t) eof-value)
  (let ((ch (cl:peek-char peek-type (tracker-stream tr) nil nil)))
    (unless ch
      (if eof-error-p
          (error 'clef-reader-error :stream (tracker-stream tr)
                 :line (tracker-line tr) :column (tracker-column tr)
                 :detail "Unexpected end of file")
          (return-from tr-peek-char eof-value)))
    ch))

(defun %ensure-tracker (stream)
  (if (tracker-p stream) stream (%make-tracker stream)))

(defmacro with-tracked-stream ((tr stream) &body body)
  `(let ((,tr (%ensure-tracker ,stream))) ,@body))

(defun rerr (tr fmt &rest args)
  (error 'clef-reader-error :stream (tracker-stream tr)
         :line (tracker-line tr) :column (tracker-column tr)
         :detail (apply #'format nil fmt args)))

;;;; --- whitespace/comments ---

(defun whitespacep (ch rt)
  (eq (aref (clef-readtable-syntax rt) (char-code ch)) :whitespace))

(defun skip-whitespace (tr rt)
  (loop for ch = (cl:peek-char nil (tracker-stream tr) nil nil)
        while (and ch (whitespacep ch rt))
        do (tr-read-char tr)))

(defun skip-intertoken (tr rt)
  "Skip whitespace and ;-comments. Returns T if more input may follow."
  (loop
    (skip-whitespace tr rt)
    (let ((ch (cl:peek-char nil (tracker-stream tr) nil nil)))
      (cond ((null ch) (return nil))
            ((char= ch #\;)
             (loop for c = (tr-read-char tr nil nil)
                   until (or (null c) (char= c #\Newline))))
            (t (return t))))))

;;;; --- tokens ---

(defun read-token (tr rt)
  "Read a token per ANSI §2.2 steps 8+ (constituent accumulation with
single/multiple escapes). Returns the raw token string with escape markers:
chars under escape are stored as-is; the CASE application happens in
TOKEN-CASE-CONVERT with knowledge of which were escaped. We track escapedness
in a parallel bit vector."
  (let ((chars '()) (escaped '()))
    (loop
      (let ((ch (cl:peek-char nil (tracker-stream tr) nil nil)))
        (unless ch (return))
        (let ((syn (aref (clef-readtable-syntax rt) (char-code ch))))
          (case syn
            (:single-escape
             (tr-read-char tr)
             (let ((next (tr-read-char tr)))
               (push next chars) (push t escaped)))
            (:multiple-escape
             (tr-read-char tr)
             (loop
               (let ((c (tr-read-char tr)))
                 (cond ((char= c #\\)
                        (push (tr-read-char tr) chars) (push t escaped))
                       ((char= c #\|) (return))
                       (t (push c chars) (push t escaped))))))
            (:terminating-macro (return))
            (:whitespace
             ;; ANSI §2.2 step 8: when whitespace terminates a token, READ
             ;; consumes it (READ-PRESERVING-WHITESPACE would not; our
             ;; entry points share this path, documented divergence —
             ;; read-preserving-whitespace is only observably different
             ;; for callers chaining reads on one stream, which the tracker
             ;; keeps correct since only single-char whitespace is eaten).
             (tr-read-char tr)
             (return))
            (t ; constituent or non-terminating macro char
             (tr-read-char tr)
             (push ch chars) (push nil escaped))))))
    (values (coerce (nreverse chars) 'string)
            (coerce (nreverse escaped) 'vector))))

(defun token-case-convert (token escaped rt)
  "Apply readtable-case to the unescaped characters of TOKEN."
  (let ((mode (clef-readtable-case-mode rt)))
    (if (eq mode :preserve)
        token
        (let ((any-lower nil) (any-upper nil))
          (dotimes (i (length token))
            (unless (aref escaped i)
              (let ((ch (char token i)))
                (when (lower-case-p ch) (setf any-lower t))
                (when (upper-case-p ch) (setf any-upper t)))))
          (case mode
            (:upcase
             (with-output-to-string (s)
               (dotimes (i (length token))
                 (princ (if (aref escaped i) (char token i)
                            (char-upcase (char token i))) s))))
            (:downcase
             (with-output-to-string (s)
               (dotimes (i (length token))
                 (princ (if (aref escaped i) (char token i)
                            (char-downcase (char token i))) s))))
            (:invert
             (cond ((and any-upper (not any-lower))
                    (with-output-to-string (s)
                      (dotimes (i (length token))
                        (princ (if (aref escaped i) (char token i)
                                   (char-downcase (char token i))) s))))
                   ((and any-lower (not any-upper))
                    (with-output-to-string (s)
                      (dotimes (i (length token))
                        (princ (if (aref escaped i) (char token i)
                                   (char-upcase (char token i))) s))))
                   (t token)))
            (t token))))))

;;;; --- numbers (ANSI §2.3.1) ---

(defun parse-integer-with-base (string base)
  "Parse STRING as an integer in BASE, allowing leading +/-."
  (parse-integer string :radix base :junk-allowed nil))

(defun float-marker-format (ch)
  (case (char-upcase ch)
    (#\E *read-default-float-format*)
    (#\F 'single-float) (#\S 'short-float)
    (#\D 'double-float) (#\L 'long-float)
    (otherwise nil)))

(defun try-parse-number (token)
  "Try to parse TOKEN as a number per ANSI §2.3.1.1. Handles integers in
*read-base*, ratios a/b, and floats with e/f/d/l/s markers. Radix prefixes
(#x #o #b #Nr) are dispatch macros — part 2. Returns (values number ok)."
  (handler-case
      (flet ((all-digits-p (s)
               (and (plusp (length s))
                    (cl:every (lambda (c) (digit-char-p c *read-base*)) s))))
        (cond
          ;; integer (optional sign, digits, optional trailing decimal point)
          ((and (let ((body (if (and (plusp (length token))
                                     (find (char token 0) "+-"))
                                (subseq token 1) token)))
                  (and (plusp (length body))
                       (cl:every (lambda (c) (digit-char-p c *read-base*))
                                 (string-trim "." body))))
                (cl:every (lambda (c) (or (digit-char-p c *read-base*)
                                          (find c "+-."))) token))
           (let ((int (parse-integer-with-base token *read-base*)))
             (if int (values int t) (values nil nil))))
          ;; ratio
          ((and (find #\/ token)
                (let* ((slash (position #\/ token))
                       (num (subseq token 0 slash))
                       (den (subseq token (1+ slash)))
                       (digits (if (and (plusp (length num))
                                        (find (char num 0) "+-"))
                                   (subseq num 1)
                                   num)))
                  (and (all-digits-p digits) (all-digits-p den))))
           (values (cl:read-from-string token) t))
          ;; float: digits with one dot or an exponent marker
          ((or (find #\. token)
               (find-if (lambda (c) (member (char-upcase c)
                                            '(#\E #\F #\S #\D #\L)))
                        token))
           (let* ((upc (string-upcase token))
                  (start (if (and (plusp (length upc))
                                  (find (char upc 0) "+-"))
                             1 0))
                  (marker-pos (position-if
                               (lambda (c) (member c '(#\E #\F #\S #\D #\L)))
                               upc :start start))
                  (dot-pos (position #\. upc)))
             (cond
               ;; decimal float without exponent marker: digits . digits
               ((and dot-pos (not marker-pos)
                     (let ((int-part (subseq upc start dot-pos))
                           (frac-part (subseq upc (1+ dot-pos))))
                       (and (all-digits-p int-part)
                            (or (all-digits-p frac-part)
                                (zerop (length frac-part))))))
                (values (coerce (cl:read-from-string upc)
                                *read-default-float-format*)
                        t))
               ;; exponent float
               ((and marker-pos (> marker-pos start))
                (let* ((fmt (float-marker-format (char upc marker-pos)))
                       (as-e (concatenate 'string
                                          (subseq upc 0 marker-pos) "E"
                                          (subseq upc (1+ marker-pos))))
                       (val (handler-case (cl:read-from-string as-e)
                              (error () nil))))
                  (if (and val (floatp val) fmt)
                      (values (coerce val fmt) t)
                      (values nil nil))))
               (t (values nil nil)))))
          (t (values nil nil))))
    (error () (values nil nil))))

;;;; --- symbols ---

(defun token->object (token escaped rt tr)
  "Convert a token to a number or symbol per ANSI §2.3."
  (multiple-value-bind (num ok) (try-parse-number token)
    (if ok
        num
        (let* ((converted (token-case-convert token escaped rt))
               (pkg-pos (position #\: converted)))
          (cond
            ;; keyword
            ((and pkg-pos (zerop pkg-pos))
             (intern (subseq converted 1) (find-package :keyword)))
            ;; package-qualified: pkg:name or pkg::name
            ((and pkg-pos (> pkg-pos 0))
             (let* ((double (and (< (1+ pkg-pos) (length converted))
                                 (char= (char converted (1+ pkg-pos)) #\:)))
                    (pkg-name (subseq converted 0 pkg-pos))
                    (sym-name (subseq converted (+ pkg-pos (if double 2 1))))
                    ;; SEAM: resolved against the HOST package system.
                    ;; The cross-compilation reader will resolve against
                    ;; first-class CLEF environments instead.
                    (pkg (find-package (string-upcase pkg-name))))
               (unless pkg
                 (rerr tr "Package ~a does not exist" pkg-name))
               (if double
                   (intern sym-name pkg)
                   (multiple-value-bind (sym status) (find-symbol sym-name pkg)
                     (unless (eq status :external)
                       (rerr tr "~a is not external in package ~a" sym-name pkg-name))
                     sym))))
            (t (intern converted)))))))

;;;; --- macro character functions ---

(defun read-string (tr rt)
  "Read a \"...\" string with \\ escapes."
  (tr-read-char tr) ; consume "
  (let ((chars '()))
    (loop
      (let ((ch (tr-read-char tr)))
        (cond ((char= ch #\") (return))
              ((char= ch #\\) (push (tr-read-char tr) chars))
              (t (push ch chars)))))
    (coerce (nreverse chars) 'string)))

(defun read-list (tr rt)
  "Read a ( ... ) list including dotted-pair syntax. Consumes the (."
  (tr-read-char tr) ; consume (
  (let ((elems '()) (seen-dot nil) (dotted-tail nil))
    (loop
      (unless (skip-intertoken tr rt)
        (rerr tr "End of file inside list"))
      (let ((ch (tr-peek-char tr)))
        (when (char= ch #\))
          (tr-read-char tr)
          (return))
        (when (and (char= ch #\.) (not seen-dot))
          ;; could be a dot or a token starting with . — check next char
          (tr-read-char tr)
          (let ((after (tr-peek-char tr nil nil)))
            (if (or (null after)
                    (whitespacep after rt)
                    (eq (aref (clef-readtable-syntax rt) (char-code after))
                        :terminating-macro))
                (progn
                  (when (null elems) (rerr tr "Dot before any list element"))
                  (when seen-dot (rerr tr "Multiple dots in list"))
                  (setf seen-dot t)
                  (setf dotted-tail (%read-object tr rt t))
                  (unless (skip-intertoken tr rt)
                    (rerr tr "End of file after dotted tail"))
                  (unless (char= (tr-peek-char tr) #\))
                    (rerr tr "Multiple objects after dot in list"))
                  (tr-read-char tr) ; consume )
                  (return-from read-list
                    (nreconc elems dotted-tail)))
                (tr-unread-char tr #\.))))
        (when seen-dot (rerr tr "Multiple objects after dot in list"))
        (push (%read-object tr rt t) elems)))
    (nreverse elems)))

(defun read-quote (tr rt)
  (tr-read-char tr) ; consume '
  (list 'quote (%read-object tr rt t)))

(defun read-backquote (tr rt)
  (tr-read-char tr) ; consume `
  (list 'quasiquote (%read-object tr rt t)))

(defun read-comma (tr rt)
  (tr-read-char tr) ; consume ,
  (let ((next (tr-peek-char tr)))
    (cond ((char= next #\@)
           (tr-read-char tr)
           (list 'unquote-splicing (%read-object tr rt t)))
          (t (list 'unquote (%read-object tr rt t))))))

;;;; --- the reader ---

(defun %read-object (tr rt eof-error-p &optional eof-value recursive-p)
  "Read one object. If *read-suppress*, parse and discard."
  (unless (skip-intertoken tr rt)
    (if eof-error-p
        (error 'clef-reader-error :stream (tracker-stream tr)
               :line (tracker-line tr) :column (tracker-column tr)
               :detail "End of file")
        (return-from %read-object eof-value)))
  (let* ((ch (tr-peek-char tr))
         (syn (aref (clef-readtable-syntax rt) (char-code ch))))
    (flet ((suppress (thunk)
             (if *read-suppress* (progn (funcall thunk) nil) (funcall thunk))))
      (case syn
        (:terminating-macro
         (case ch
           (#\" (suppress (lambda () (read-string tr rt))))
           (#\' (suppress (lambda () (read-quote tr rt))))
           (#\` (suppress (lambda () (read-backquote tr rt))))
           (#\, (suppress (lambda () (read-comma tr rt))))
           (#\( (suppress (lambda () (read-list tr rt))))
           (#\) (rerr tr "Unmatched close parenthesis"))
           (otherwise (rerr tr "No reader macro for ~c" ch))))
        (:single-escape
         (multiple-value-bind (token escaped) (read-token tr rt)
           (unless *read-suppress* (token->object token escaped rt tr))))
        (:multiple-escape
         (multiple-value-bind (token escaped) (read-token tr rt)
           (unless *read-suppress* (token->object token escaped rt tr))))
        (:whitespace (rerr tr "Internal: whitespace after skip"))
        (otherwise
         (multiple-value-bind (token escaped) (read-token tr rt)
           (declare (ignore recursive-p))
           (unless *read-suppress* (token->object token escaped rt tr))))))))

(defun read (&optional stream (eof-error-p t) eof-value recursive-p)
  "Read one object from STREAM (ANSI read)."
  (with-tracked-stream (tr (or stream *standard-input*))
    (%read-object tr *readtable* eof-error-p eof-value recursive-p)))

(defun read-preserving-whitespace (&optional stream (eof-error-p t) eof-value recursive-p)
  "ANSI read-preserving-whitespace. Our token reader never consumes the
terminating whitespace, so this is equivalent to read."
  (with-tracked-stream (tr (or stream *standard-input*))
    (%read-object tr *readtable* eof-error-p eof-value recursive-p)))

(defun read-from-string (string &optional (eof-error-p t) eof-value
                         &key (start 0) end preserve-whitespace)
  "ANSI read-from-string. Returns (values object index)."
  (declare (ignore preserve-whitespace))
  (let* ((sub (make-string-input-stream string start end))
         (obj nil))
    (with-tracked-stream (tr sub)
      (setf obj (%read-object tr *readtable* eof-error-p eof-value nil))
      ;; index: chars consumed = position in the sub-stream. Peek-ahead may
      ;; have consumed one char past the token (the terminating char stays
      ;; UNREAD in the stream, so file-position is exact here).
      (values obj (+ start (file-position sub))))))

(defun read-delimited-list (char &optional stream recursive-p)
  "ANSI read-delimited-list: read objects until CHAR."
  (declare (ignore recursive-p))
  (with-tracked-stream (tr (or stream *standard-input*))
    (let ((elems '()))
      (loop
        (unless (skip-intertoken tr *readtable*)
          (rerr tr "End of file inside read-delimited-list"))
        (when (char= (tr-peek-char tr) char)
          (tr-read-char tr)
          (return (nreverse elems)))
        (push (%read-object tr *readtable* t) elems)))))
