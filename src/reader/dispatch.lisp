;;;; dispatch.lisp — dispatch (#) macro characters (ANSI §2.4.8).
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;; Clean-room from the ANSI spec.

(in-package #:clef/reader)

(defvar *features* (copy-list cl:*features*)
  "Feature list for #+/#- conditionalization (defaults to a copy of the
host's at load; the cross-compiler rebinds it for target conditionalization).")

;;;; --- feature expressions (§2.4.8.18) ---

(defun feature-present-p (feature-expr)
  "Evaluate a #+/#- feature expression: atom or (and/or/not ...)."
  (cond ((symbolp feature-expr)
         (member feature-expr *features*))
        ((consp feature-expr)
         (case (car feature-expr)
           (and (every #'feature-present-p (cdr feature-expr)))
           (or (some #'feature-present-p (cdr feature-expr)))
           (not (and (= (length (cdr feature-expr)) 1)
                     (not (feature-present-p (cadr feature-expr)))))
           (otherwise nil)))
        (t nil)))

;;;; --- sub-dispatch helpers ---

(defun read-dispatch-arg (tr)
  "Read an optional numeric infix argument between # and the subchar.
Returns (values arg subchar). E.g. #2r → (values 2 #\\r), #x → (values nil #\\x)."
  (let ((digits '()))
    (loop
      (let ((ch (tr-peek-char tr)))
        (if (digit-char-p ch)
            (progn (tr-read-char tr) (push ch digits))
            (return))))
    (values (when digits (parse-integer (coerce (nreverse digits) 'string)))
            (tr-read-char tr))))

(defun %intern-dispatch (rt disp sub fn)
  (set-dispatch-macro-character disp sub fn rt))

;;;; --- the sub-dispatch handlers ---
;;;; Each takes (tr subchar numeric-arg).

(defun sharp-quote (tr sub arg rt)
  (declare (ignore sub arg))
  (let ((form (%read-object tr rt t)))
    (unless *read-suppress* (list 'function form))))

(defun sharp-paren (tr sub arg rt)
  (declare (ignore sub arg))
  ;; #( already consumed; read the list body directly.
  (let ((list (%read-list-body tr rt)))
    (unless *read-suppress* (coerce list 'vector))))

(defun sharp-star (tr sub arg rt)
  (declare (ignore sub))
  (let ((token (read-token-string tr rt)))
    (unless *read-suppress*
      (when (and arg (> (length token) arg))
        (rerr tr "Bit vector longer than declared length ~a" arg))
      (let* ((len (or arg (length token)))
             (bv (make-array len :element-type 'bit :initial-element 0)))
        (dotimes (i (min (length token) len))
          (let ((c (char token i)))
            (unless (member c '(#\0 #\1))
              (rerr tr "Bad bit ~c in #* syntax" c))
            (setf (aref bv i) (digit-char-p c))))
        ;; pad with the LAST bit per ANSI
        (when (and (> (length token) 0) (< (length token) len))
          (let ((last (digit-char-p (char token (1- (length token))))))
            (loop for i from (length token) below len
                  do (setf (aref bv i) last))))
        bv))))

(defun sharp-colon (tr sub arg rt)
  (declare (ignore sub arg))
  (multiple-value-bind (token escaped) (read-token tr rt)
    (unless *read-suppress*
      (make-symbol (token-case-convert token escaped rt)))))

(defun sharp-dot (tr sub arg rt)
  (declare (ignore sub arg))
  (unless *read-eval*
    (rerr tr "#. read-time evaluation disabled by *read-eval*"))
  (let ((form (%read-object tr rt t)))
    (unless *read-suppress* (eval form))))

(defun sharp-bar (tr sub arg rt)
  "Nested block comment: consume through the matching |#."
  (declare (ignore sub arg))
  (let ((depth 1))
    (loop
      (let ((ch (tr-read-char tr)))
        (cond ((and (char= ch #\#)
                    (char= (tr-peek-char tr nil nil) #\|))
               ;; nested open #|
               (tr-read-char tr)
               (incf depth))
              ((and (char= ch #\|)
                    (char= (tr-peek-char tr nil nil) #\#))
               ;; close |#
               (tr-read-char tr)
               (when (zerop (decf depth)) (return)))))))
  +skip-marker+)

(defun sharp-plus (tr sub arg rt)
  (declare (ignore arg))
  (%read-feature-conditional tr rt sub))

(defun sharp-minus (tr sub arg rt)
  (declare (ignore arg))
  (%read-feature-conditional tr rt sub))

(defun %read-feature-conditional (tr rt sub)
  (let ((feature-expr (let ((*read-suppress* nil))
                        (%read-object tr rt t))))
    (let ((take (if (char= sub #\+)
                    (feature-present-p feature-expr)
                    (not (feature-present-p feature-expr)))))
      (if take
          (%read-object tr rt t)
          (let ((*read-suppress* t))
            (%read-object tr rt t)
            +skip-marker+)))))

(defun sharp-c (tr sub arg rt)
  (declare (ignore sub arg))
  (let ((pair (%read-object tr rt t)))
    (unless *read-suppress*
      (unless (and (consp pair) (consp (cdr pair)) (null (cddr pair)))
        (rerr tr "#C requires a two-element list"))
      (complex (car pair) (cadr pair)))))

;;;; labeled references #n= / #n#
(defvar *label-table* nil
  "Dynamically-bound alist of label → object for #n=/#n# within one top-level read.")

(defun sharp-equals (tr sub arg rt)
  (declare (ignore sub))
  (unless arg (rerr tr "#= requires a numeric label"))
  (when (assoc arg *label-table*)
    (rerr tr "Duplicate label #~a=" arg))
  ;; Forward-reference support for the cons case: #1=( ... #1# ... ).
  ;; Pre-allocate the cons, register it, then fill it in place.
  (unless (skip-intertoken tr rt)
    (rerr tr "End of file after #~a=" arg))
  (if (char= (tr-peek-char tr) #\()
      (let ((cell (cons nil nil)))
        (push (cons arg cell) *label-table*)
        (tr-read-char tr) ; consume (
        (let ((contents (%read-list-body tr rt)))
          (if (consp contents)
              (setf (car cell) (car contents)
                    (cdr cell) (cdr contents))
              (setf (car cell) contents)) ; (#1=()) — nil contents: (nil) cell is wrong; see below
          (when (null contents)
            ;; #1=() — the label IS nil; re-register to nil (placeholder
            ;; cons was premature; #1# refs already handed the cell out —
            ;; documented limitation: circular refs to nil are meaningless)
            (setf (cdr (assoc arg *label-table*)) nil))
          (unless *read-suppress* cell)))
      (let ((obj (%read-object tr rt t)))
        (push (cons arg obj) *label-table*)
        (unless *read-suppress* obj))))

(defun sharp-sharp (tr sub arg rt)
  (declare (ignore sub))
  (unless arg (rerr tr "## requires a numeric label"))
  (unless *read-suppress*
    (let ((entry (assoc arg *label-table*)))
      (unless entry (rerr tr "Undefined label #~a#" arg))
      (cdr entry))))

;;;; radix integers #x #o #b #Nr

(defun sharp-radix (tr sub arg rt)
  "Read an integer in radix: #x=16 #o=8 #b=2, or #Nr when arg present."
  (declare (ignore arg))
  (multiple-value-bind (token escaped) (read-token tr rt)
    (declare (ignore escaped))
    (unless *read-suppress*
      (let* ((radix (case (char-upcase sub)
                      (#\X 16) (#\O 8) (#\B 2)
                      (#\R (or (and (boundp '*radix-arg*) (symbol-value '*radix-arg*))
                               (rerr tr "#R requires a radix")))))
             (int (parse-integer token :radix radix :junk-allowed nil)))
        int))))

(defun sharp-r (tr sub arg rt)
  "#Nr: read integer in radix ARG (2..36)."
  (unless (and arg (<= 2 arg 36))
    (rerr tr "#R radix must be 2..36, got ~a" arg))
  (let ((*radix-arg* arg))
    (declare (special *radix-arg*))
    (sharp-radix tr sub arg rt)))

;;;; character literals #\

(defparameter +char-names+
  (let ((h (make-hash-table :test #'equal)))
    (setf (gethash "NEWLINE" h) #\Newline
          (gethash "SPACE" h) #\Space
          (gethash "TAB" h) #\Tab
          (gethash "RETURN" h) #\Return
          (gethash "PAGE" h) #\Page
          (gethash "LINEFEED" h) #\Linefeed
          (gethash "RUBOUT" h) #\Rubout)
    h)
  "Named characters for #\\ syntax (case-insensitive lookup).")

(defun sharp-backslash (tr sub arg rt)
  (declare (ignore sub arg))
  ;; ANSI §2.4.8.1: after #\, read a single char; if it is alphabetic,
  ;; keep reading a character name. Punctuation is taken literally
  ;; (e.g. #\( #\# #\\), so token-based reading is wrong for it.
  (let ((first (tr-read-char tr)))
    (unless *read-suppress*
      (if (not (alpha-char-p first))
          first
          ;; accumulate remaining constituent/name chars
          (let ((chars (list first)))
            (loop for ch = (cl:peek-char nil (tracker-stream tr) nil nil)
                  while (and ch (or (alphanumericp ch)
                                    (char= ch #\-) (char= ch #\+)))
                  do (tr-read-char tr) (push ch chars))
            (let ((name (string-upcase (coerce (nreverse chars) 'string))))
              (if (= (length name) 1)
                  first           ; single char: literal case, not upcased
                  (or (gethash name +char-names+)
                      (rerr tr "Unknown character name ~a" name)))))))))

;;;; helper: read a raw token without conversion (for #*)

(defun read-token-string (tr rt)
  "Read a token and return the raw string (no case conversion)."
  (multiple-value-bind (token escaped) (read-token tr rt)
    (declare (ignore escaped))
    token))

;;;; --- the dispatch entry ---

(defun read-sharp-dispatch (tr rt)
  "Handle # after it has been peeked. Consumes the # and dispatches."
  (tr-read-char tr) ; consume #
  (multiple-value-bind (arg sub) (read-dispatch-arg tr)
    (case (char-upcase sub)
      (#\' (sharp-quote tr sub arg rt))
      (#\( (sharp-paren tr sub arg rt))
      (#\* (sharp-star tr sub arg rt))
      (#\: (sharp-colon tr sub arg rt))
      (#\. (sharp-dot tr sub arg rt))
      (#\| (sharp-bar tr sub arg rt))
      (#\+ (sharp-plus tr sub arg rt))
      (#\- (sharp-minus tr sub arg rt))
      (#\C (sharp-c tr sub arg rt))
      (#\= (sharp-equals tr sub arg rt))
      (#\# (sharp-sharp tr sub arg rt))
      (#\X (sharp-radix tr sub arg rt))
      (#\O (sharp-radix tr sub arg rt))
      (#\B (sharp-radix tr sub arg rt))
      (#\R (sharp-r tr sub arg rt))
      (#\\ (sharp-backslash tr sub arg rt))
      (otherwise (rerr tr "Unknown dispatch character #~c" sub)))))
