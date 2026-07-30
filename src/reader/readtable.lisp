;;;; readtable.lisp — readtables for the CLEF clean-room reader (ANSI §23.1.3).
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.

(in-package #:clef/reader)

;;; Syntax types per char: :constituent, :whitespace, :terminating-macro,
;;; :non-terminating-macro, :single-escape, :multiple-escape.

(defstruct (clef-readtable (:constructor %make-readtable))
  (syntax (make-array 256 :initial-element :constituent))
  (macros (make-array 256 :initial-element nil))
  (dispatches (make-array 256 :initial-element nil)) ; subchar -> fn, for # dispatch (part 2)
  (case-mode :upcase))

(defvar *readtable* nil
  "The current readtable. Bound to a fresh standard readtable at load.")

(defun %standard-readtable ()
  "Build the ANSI standard readtable."
  (let ((rt (%make-readtable)))
    (flet ((syn (ch type &optional fn)
             (setf (aref (clef-readtable-syntax rt) (char-code ch)) type)
             (when fn (setf (aref (clef-readtable-macros rt) (char-code ch)) fn))))
      ;; whitespace
      (dolist (ch '(#\Space #\Tab #\Newline #\Return #\Linefeed #\Page))
        (syn ch :whitespace))
      ;; escapes
      (syn #\\ :single-escape)
      (syn #\| :multiple-escape)
      ;; terminating macro chars (functions wired in reader.lisp to avoid
      ;; forward references; entries here mark the syntax type)
      (dolist (ch '(#\" #\' #\( #\) #\, #\; #\`))
        (syn ch :terminating-macro))
      ;; # is a non-terminating macro char (dispatch handled in part 2)
      (syn #\# :non-terminating-macro))
    rt))

(defun copy-readtable (&optional (from *readtable*) to)
  "Copy readtable FROM (default *readtable*) into TO or a fresh one."
  (let* ((src (or from (%standard-readtable)))
         (dst (or to (%make-readtable))))
    (replace (clef-readtable-syntax dst) (clef-readtable-syntax src))
    (replace (clef-readtable-macros dst) (clef-readtable-macros src))
    (dotimes (i 256)
      (let ((d (aref (clef-readtable-dispatches src) i)))
        (setf (aref (clef-readtable-dispatches dst) i)
              (when d (copy-seq d)))))
    (setf (clef-readtable-case-mode dst) (clef-readtable-case-mode src))
    dst))

(defun readtable-case (rt) (clef-readtable-case-mode rt))

(defun (setf readtable-case) (mode rt)
  (unless (member mode '(:upcase :downcase :preserve :invert))
    (error "Invalid readtable-case: ~s" mode))
  (setf (clef-readtable-case-mode rt) mode))

(defun set-macro-character (char fn &optional non-terminating-p (rt *readtable*))
  "Set CHAR's macro function in RT (ANSI set-macro-character)."
  (setf (aref (clef-readtable-syntax rt) (char-code char))
        (if non-terminating-p :non-terminating-macro :terminating-macro))
  (setf (aref (clef-readtable-macros rt) (char-code char)) fn)
  t)

(defun get-macro-character (char &optional (rt *readtable*))
  "Return (values fn non-terminating-p) for CHAR in RT."
  (values (aref (clef-readtable-macros rt) (char-code char))
          (eq (aref (clef-readtable-syntax rt) (char-code char))
              :non-terminating-macro)))

(defun make-dispatch-macro-character (char &optional non-terminating-p (rt *readtable*))
  "Make CHAR a dispatch macro character (ANSI)."
  (setf (aref (clef-readtable-syntax rt) (char-code char))
        (if non-terminating-p :non-terminating-macro :terminating-macro))
  (setf (aref (clef-readtable-macros rt) (char-code char)) :dispatch)
  (setf (aref (clef-readtable-dispatches rt) (char-code char))
        (make-array 256 :initial-element nil))
  t)

(defun set-dispatch-macro-character (disp sub fn &optional (rt *readtable*))
  "Set FN as the dispatch function for DISP SUB in RT."
  (let ((table (aref (clef-readtable-dispatches rt) (char-code disp))))
    (unless table (error "~c is not a dispatch macro character" disp))
    (setf (aref table (char-code (char-upcase sub))) fn))
  t)

(defun get-dispatch-macro-character (disp sub &optional (rt *readtable*))
  "Return the dispatch function for DISP SUB in RT."
  (let ((table (aref (clef-readtable-dispatches rt) (char-code disp))))
    (when table (aref table (char-code (char-upcase sub))))))

(defun set-syntax-from-char (to-char from-char &optional (to-rt *readtable*) (from-rt *readtable*))
  "Give TO-CHAR the syntax of FROM-CHAR (ANSI; macro functions not copied
across readtables per spec — we copy the syntax type and any macro fn)."
  (setf (aref (clef-readtable-syntax to-rt) (char-code to-char))
        (aref (clef-readtable-syntax from-rt) (char-code from-char)))
  (setf (aref (clef-readtable-macros to-rt) (char-code to-char))
        (aref (clef-readtable-macros from-rt) (char-code from-char)))
  t)

(setf *readtable* (%standard-readtable))
