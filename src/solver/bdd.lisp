;;;; bdd.lisp — BDD set-constraint store for the CLEF type solver.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; The set-theoretic core of the hybrid solver (DESIGN §5.2). CL's type
;;;; algebra is set-based (or/and/not/member), so subtyping is semantic
;;;; subtyping (Frisch–Castagna): a type denotes a set of values, and
;;;; T1 <= T2 iff T1 ∩ ¬T2 is uninhabited. We decide inhabitation with
;;;; reduced ordered BDDs over atom variables.

(in-package #:clef/solver)

;;;; Reduced ordered BDD with unique-table memoization.
;;;; A node is (var low high); leaves are the constants 0 and 1.

(defstruct (bdd (:constructor %bdd (var low high)) (:conc-name bdd-))
  (var 0 :type fixnum)  ; atom variable index
  low high)             ; child BDDs (node, 0, or 1)

(defvar *bdd-unique* (make-hash-table :test 'equal))
(defvar *bdd-cache* (make-hash-table :test 'equal))

(defun bdd-node (var low high)
  "Return the (reduced) BDD node for VAR with LOW/HIGH children."
  (if (eq low high)
      low  ; reduction rule
      (let ((key (list var low high)))
        (or (gethash key *bdd-unique*)
            (setf (gethash key *bdd-unique*) (%bdd var low high))))))

(defparameter *bdd-false* 0)
(defparameter *bdd-true* 1)

(defun bdd-atom (var-index)
  "BDD for the single atom variable VAR-INDEX."
  (bdd-node var-index *bdd-false* *bdd-true*))

(defun bdd-apply (op f g)
  "Shannon expansion: apply boolean OP (:and :or :not-binary) to BDDs F and G."
  (labels ((rec (f g)
             (cond
               ;; constant folding
               ((and (integerp f) (integerp g))
                (ecase op
                  (:and (if (and (= f 1) (= g 1)) 1 0))
                  (:or  (if (or (= f 1) (= g 1)) 1 0))))
               ((integerp f)
                (ecase op
                  (:and (if (= f 0) 0 g))
                  (:or  (if (= f 1) 1 g))))
               ((integerp g)
                (ecase op
                  (:and (if (= g 0) 0 f))
                  (:or  (if (= g 1) 1 f))))
               (t
                (let* ((key (list op f g))
                       (hit (gethash key *bdd-cache*)))
                  (or hit
                      (let* ((vf (bdd-var f))
                             (vg (bdd-var g))
                             (v (min vf vg))
                             (fl (if (= vf v) (bdd-low f) f))
                             (fh (if (= vf v) (bdd-high f) f))
                             (gl (if (= vg v) (bdd-low g) g))
                             (gh (if (= vg v) (bdd-high g) g)))
                        (setf (gethash key *bdd-cache*)
                              (bdd-node v (rec fl gl) (rec fh gh))))))))))
    (rec f g)))

(defun bdd-and (f g) (bdd-apply :and f g))
(defun bdd-or (f g) (bdd-apply :or f g))

(defun bdd-not (f)
  "Complement a BDD."
  (if (integerp f)
      (if (= f 0) 1 0)
      (bdd-node (bdd-var f) (bdd-not (bdd-low f)) (bdd-not (bdd-high f)))))

(defun bdd-empty-p (f)
  "True if the BDD denotes the empty set (unsatisfiable)."
  (and (integerp f) (= f 0)))

;;;; Type-level interface: represent CL types as BDDs over "atom variables,"
;;;; where each atom is a primitive/base type or a type parameter. The
;;;; subtype test is semantic: T1 <= T2  iff  (T1 and (not T2)) is empty.

(defstruct (stype (:conc-name st-))
  (bdd 0))             ; BDD over atom variables
  ;; A real implementation also tracks product/cons structure; this skeleton
  ;; handles the boolean (union/intersection/negation) fragment.

(defun st-atom (var-index) (make-stype :bdd (bdd-atom var-index)))
(defun st-or (a b) (make-stype :bdd (bdd-or (st-bdd a) (st-bdd b))))
(defun st-and (a b) (make-stype :bdd (bdd-and (st-bdd a) (st-bdd b))))
(defun st-not (a) (make-stype :bdd (bdd-not (st-bdd a))))
(defparameter *st-top* (make-stype :bdd *bdd-true*))
(defparameter *st-bottom* (make-stype :bdd *bdd-false*))

(defun subtypep-bdd (t1 t2)
  "True if T1 is a (semantic) subtype of T2 in the boolean fragment."
  (bdd-empty-p (bdd-and (st-bdd t1) (bdd-not (st-bdd t2)))))

;;; This is the decidable core the Prolog front-end suspends subtype goals
;;; into (DESIGN §5.2). Cons/product types and parametric type constructors
;;; extend the atom set; see DESIGN §5 and the issue tracker.
