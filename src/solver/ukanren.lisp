;;;; ukanren.lisp — a µKanren core for the CLEF type solver.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; This is the logic front-end of the hybrid solver (DESIGN §5.2): logic
;;;; variables, unification (with rational-tree/occurs handling deferred to
;;;; the set-constraint layer), and the core goal constructors. The
;;;; set-theoretic subtype relation lives in the BDD store (bdd.lisp), which
;;;; attaches to variables as suspended constraints.

(in-package #:clef/solver)

;;;; Logic variables

(defstruct (lvar (:constructor make-lvar (id)) (:print-function print-lvar))
  (id 0))

(defun print-lvar (v stream depth)
  (declare (ignore depth))
  (format stream "#?~d" (lvar-id v)))

(defun fresh-var ()
  "A fresh logic variable with a unique identity."
  (make-lvar (gensym "V")))

;;;; Substitutions: an association list mapping lvar -> value.

(defparameter +empty-subst+ '()
  "The empty substitution. A valid substitution can be the empty list, so
UNIFY distinguishes success/failure with VALUES, not NIL alone.")

(defun walk (u subst)
  "Follow the substitution chain for U."
  (let ((v (and (lvar-p u) (assoc u subst :test #'eq))))
    (if v (walk (cdr v) subst) u)))

(defun extend-subst (var val subst)
  (acons var val subst))

(defun occurs-p (var val subst)
  "Does VAR occur in VAL (after walking)? Guards against circular bindings."
  (let ((val (walk val subst)))
    (cond ((eq var val) t)
          ((consp val) (or (occurs-p var (car val) subst)
                           (occurs-p var (cdr val) subst)))
          (t nil))))

(defun unify (u v subst)
  "Unify U and V under SUBST.
Returns (values new-subst success-p). SUCCESS-P distinguishes a legitimately
empty substitution from failure."
  (let ((u (walk u subst))
        (v (walk v subst)))
    (cond
      ((eq u v) (values subst t))
      ((lvar-p u)
       (if (occurs-p u v subst)
           (values nil nil)
           (values (extend-subst u v subst) t)))
      ((lvar-p v)
       (if (occurs-p v u subst)
           (values nil nil)
           (values (extend-subst v u subst) t)))
      ((and (consp u) (consp v))
       (multiple-value-bind (s1 ok) (unify (car u) (car v) subst)
         (if ok (unify (cdr u) (cdr v) s1) (values nil nil))))
      ((equal u v) (values subst t))
      (t (values nil nil)))))

;;;; Goals: functions from a state to a stream of states.
;;;; A state is just a substitution for now; the BDD constraint store will be
;;;; threaded through alongside it. Streams are lazy: NIL, a state, or a
;;;; thunk returning a stream.

(defun mzero () '())
(defun unit (state) (cons state '()))
(defun pull (stream) (if (functionp stream) (funcall stream) stream))

(defun mplus (s1 s2)
  (let ((s1 (pull s1)))
    (cond ((null s1) s2)
          ((functionp s1) (lambda () (mplus s2 (pull s1))))
          (t (cons (car s1) (mplus s2 (cdr s1)))))))

(defun bind (stream goal)
  (let ((stream (pull stream)))
    (cond ((null stream) (mzero))
          ((functionp stream) (lambda () (bind (pull stream) goal)))
          (t (mplus (funcall goal (car stream))
                    (bind (cdr stream) goal))))))

(defmacro disj2 (g1 g2)
  `(lambda (state) (mplus (funcall ,g1 state) (funcall ,g2 state))))

(defmacro disj (&rest goals)
  "Disjunction over GOALS (right-nested, lazy)."
  (cond ((null goals) `(lambda (state) (declare (ignore state)) (mzero)))
        ((null (cdr goals)) (car goals))
        (t `(disj2 ,(car goals) (disj ,@(cdr goals))))))

(defmacro conj2 (g1 g2)
  `(lambda (state) (bind (funcall ,g1 state) ,g2)))

(defmacro conj (&rest goals)
  "Conjunction over GOALS (right-nested, lazy)."
  (cond ((null goals) `(lambda (state) (unit state)))
        ((null (cdr goals)) (car goals))
        (t `(conj2 ,(car goals) (conj ,@(cdr goals))))))

(defun == (u v)
  "Unification goal."
  (lambda (state)
    (multiple-value-bind (s ok) (unify u v state)
      (if ok (unit s) (mzero)))))

(defmacro fresh ((&rest vars) &body goals)
  "Introduce fresh logic variables."
  (if (null vars)
      `(conj ,@goals)
      `(let ((,(car vars) (fresh-var)))
         (fresh ,(cdr vars) ,@goals))))

(defmacro rune ((var) &body goals)
  "Run GOALS, returning a list of reified values of VAR."
  `(let ((,var (fresh-var)))
     (mapcar (lambda (s) (reify ,var s))
             (take-all (funcall (conj ,@goals) +empty-subst+)))))

(defun take-all (stream)
  (let ((stream (pull stream)))
    (if (null stream) '()
        (cons (car stream) (take-all (cdr stream))))))

(defun reify (var subst)
  "Substitute VAR's value, recursively."
  (walk* var subst))

(defun walk* (u subst)
  (let ((u (walk u subst)))
    (if (consp u)
        (cons (walk* (car u) subst) (walk* (cdr u) subst))
        u)))

;;; Example relation, to prove the core works:
(defun conso (a d p)
  (== (cons a d) p))
