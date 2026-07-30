;;;; locations.lisp — source locations for the CLEF clean-room reader.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; Day-one design goal (DESIGN.md §11): every form the reader produces
;;;; carries its source location for macroexpansion debugging. Locations
;;;; are captured at read time by the tracker in reader.lisp and attached
;;;; here via a weak-key EQ table.
;;;;
;;;; Attach policy: conses, strings, numbers (where identity is stable),
;;;; and uninterned symbols get locations by default. INTERNED symbols are
;;;; shared across reads — attaching would overwrite earlier locations —
;;;; so they are only tracked when *record-symbol-locations* is true.

(in-package #:clef/reader)

(defstruct (source-location (:constructor %make-source-location))
  (file nil)
  (line 1 :type fixnum)
  (column 1 :type fixnum)
  (end-line 1 :type fixnum)
  (end-column 1 :type fixnum))

(defvar *source-locations* (make-hash-table :test #'eq :weakness :key)
  "Weak EQ table mapping reader-produced objects to their source locations.")

(defvar *record-symbol-locations* nil
  "When true, attach locations to interned symbols as well (overwrites a
shared symbol's earlier location across reads — documented behavior).")

(defvar *source-file* nil
  "The file string stored in locations (nil for string/stream reads).")

(defun form-source-location (object)
  "Return the source-location attached to OBJECT, or NIL."
  (gethash object *source-locations*))

(defun (setf form-source-location) (loc object)
  (setf (gethash object *source-locations*) loc))

(defun %attach-location (object start-line start-col end-line end-col)
  "Attach a location to OBJECT per the attach policy. Returns OBJECT."
  (when (or (consp object)
            (stringp object)
            (and (symbolp object)
                 (or (null (symbol-package object)) ; uninterned
                     *record-symbol-locations*))
            ;; numbers: attach where identity is meaningful (not fixnums —
            ;; they're immediates shared everywhere; EQ on them is useless)
            (and (numberp object) (not (typep object 'fixnum))
                 (not (characterp object))))
    (setf (gethash object *source-locations*)
          (%make-source-location :file *source-file*
                                 :line start-line :column start-col
                                 :end-line end-line :end-column end-col)))
  object)
