;;;; eval.lisp — the CLEF-H evaluator.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; A tree-walking evaluator over first-class environments, handling all 25
;;;; ANSI special operators plus macroexpansion. Multiple values and dynamic
;;;; non-local exits (block/return-from, tagbody/go, catch/throw,
;;;; unwind-protect) are delegated to the host, which has the same semantics.

(in-package #:clef/proto/eval)

;;; An evaluation error signalled by CLEF-H itself (not a Lisp `error`).
(define-condition clef-error (error)
  ((datum :initarg :datum :reader clef-error-datum))
  (:report (lambda (c s) (format s "CLEF error: ~a" (clef-error-datum c)))))

(defun clef-error* (fmt &rest args)
  (error 'clef-error :datum (apply #'format nil fmt args)))

;;; --- self-evaluation ---

(defun self-evaluating-p (form)
  (or (numberp form) (characterp form) (stringp form) (arrayp form)
      (keywordp form) (eq form t) (eq form nil)
      (pathnamep form) (hash-table-p form) (functionp form)))

;;; --- multiple value plumbing ---

(defun primary (values) (if (null values) nil (car values)))

;;; --- closures ---

(defstruct (clef-function (:constructor %make-clef-function))
  ll env body name)

(defun make-clef-function (lambda-list body env &optional name)
  "Create a CLEF function value. Calling it runs the tree-walking evaluator."
  (let ((ll (clef/proto/ll:parse-lambda-list lambda-list :destructuring t)))
    (%make-clef-function :ll ll :env env :body body :name name)))

(defun call-clef-function (fn args)
  (when *trace-calls*
    (format *trace-output* "~&call nargs=~d first-arg=~s~%"
            (length args)
            (and args (if (consp (car args)) (caar args) (car args)))))
  (let ((new-env (clef/proto/env:make-lexical-env (clef-function-env fn))))
    (clef/proto/ll:bind-lambda-list
     (clef-function-ll fn) args
     (lambda (var value) (clef/proto/env:bind-variable new-env var value))
     (lambda (init) (primary (clef-eval init new-env))))
    (clef-eval-seq (clef-function-body fn) new-env)))

(defvar *trace-calls* nil)

;;; --- the evaluator ---

(defun clef-eval-seq (body env)
  "Evaluate BODY forms in order, returning the last form's values as host
multiple values. Empty body returns NIL."
  (if (null body)
      (values nil)
      (loop for forms on body
            for result = (multiple-value-list (%eval (car forms) env))
            when (null (cdr forms))
              do (return (values-list result)))))

(defun clef-eval (form env)
  "Evaluate FORM in ENV, returning multiple values as a list."
  (multiple-value-list (%eval form env)))

(defun %eval (form env)
  "Evaluate FORM, returning host multiple values."
  (cond
    ;; self-evaluating
    ((self-evaluating-p form) form)
    ;; symbol: variable reference
    ((symbolp form)
     (let ((sm (clef/proto/env:lookup-symbol-macro env form)))
       (if sm
           (%eval (funcall sm form env) env)
           (clef/proto/env:lookup-variable env form))))
    ;; compound form
    ((consp form)
     (let ((op (car form)))
       (cond
         ((clef/proto/env:special-operator-p op)
          (eval-special op (cdr form) env))
         ((eq op 'cl:lambda)             ; (lambda ...) evaluates to a function
          (make-clef-function (cadr form) (cddr form) env))
         ((and (symbolp op)
               (clef/proto/env:lookup-symbol-macro env op))
          (%eval (funcall (clef/proto/env:lookup-symbol-macro env op) form env) env))
         ((symbolp op)
          (let ((macro (clef/proto/env:lookup-macro env op)))
            (if macro
                (%eval (funcall (clef/proto/env:macro-fn macro) form env) env)
                (eval-call op (cdr form) env))))
         ((and (consp op) (eq (car op) 'cl:lambda))
          (eval-lambda-call op (cdr form) env))
         (t (clef-error* "Invalid function position: ~s" op)))))
    (t (clef-error* "Cannot evaluate: ~s" form))))

(defun eval-call (name arg-forms env)
  "Call the function named NAME with evaluated arguments."
  (let ((fn (clef/proto/env:lookup-function env name))
        (args (mapcar (lambda (a) (primary (clef-eval a env))) arg-forms)))
    (clef-apply fn args)))

(defun eval-lambda-call (lambda-form arg-forms env)
  "((lambda (args) body) vals...) — a direct lambda call."
  (let ((fn (make-clef-function (cadr lambda-form) (cddr lambda-form) env))
        (args (mapcar (lambda (a) (primary (clef-eval a env))) arg-forms)))
    (clef-apply fn args)))

(defun clef-apply (fn args)
  "Apply FN (a clef-function or a host function) to ARGS."
  (cond
    ((clef-function-p fn) (call-clef-function fn args))
    ((functionp fn) (apply fn args))
    ((and (consp fn) (eq (car fn) :host-fn)) (apply (cdr fn) args))
    (t (clef-error* "Not a function: ~s" fn))))

;;; --- macroexpansion ---

(defun clef-macroexpand-1 (form env)
  "One step of macroexpansion. Returns (values expansion expanded-p)."
  (if (and (consp form) (symbolp (car form)))
      (let ((macro (clef/proto/env:lookup-macro env (car form))))
        (if macro
            (values (funcall (clef/proto/env:macro-fn macro) form env) t)
            (values form nil)))
      (values form nil)))

(defun clef-macroexpand (form env)
  "Fully macroexpand FORM at the top level."
  (loop
    (multiple-value-bind (exp expanded) (clef-macroexpand-1 form env)
      (if expanded (setf form exp) (return form)))))

;;; --- special operators ---

(defun eval-special (op args env)
  ;; Boot-time internal special operators are matched by name (any package).
  (let ((internal (and (symbolp op)
                       (member (symbol-name op)
                               clef/proto/env::*internal-special-operators*
                               :test #'string-equal))))
    (when internal
      (return-from eval-special
        (cond ((string-equal (car internal) "%DESTRUCTURING-BIND")
               (eval-p-destructuring-bind args env))
              ((string-equal (car internal) "%MULTIPLE-VALUE-BIND")
               (eval-p-multiple-value-bind args env))
              ((string-equal (car internal) "%DO")
               (eval-p-do args env nil))
              ((string-equal (car internal) "%DO*")
               (eval-p-do args env t))
              ((string-equal (car internal) "%UNWIND-PROTECT")
               (eval-p-unwind-protect args env))
              ((string-equal (car internal) "%LOOP")
               (eval-p-loop args env))
              ((string-equal (car internal) "%SETF")
               (eval-p-setf args env))))))
  (case op
    ((quote) (car args))
    ((if)
     (destructuring-bind (test then &optional else) args
       (if (primary (clef-eval test env))
           (%eval then env)
           (%eval else env))))
    ((progn) (clef-eval-seq args env))
    ((setq) (eval-setq args env))
    ((let) (eval-let (car args) (cdr args) env nil))
    ((let*) (eval-let (car args) (cdr args) env t))
    ((function) (eval-function (car args) env))
    ((block) (eval-block (car args) (cdr args) env))
    ((return-from) (eval-return-from (car args) (cadr args) env))
    ((tagbody) (eval-tagbody args env))
    ((go) (eval-go (car args) env))
    ((catch) (eval-catch args env))
    ((throw) (eval-throw args env))
    ((unwind-protect) (eval-unwind-protect args env))
    ((multiple-value-call) (eval-mvc args env))
    ((multiple-value-prog1) (eval-mvp1 args env))
    ((flet) (eval-flet (car args) (cdr args) env nil))
    ((labels) (eval-flet (car args) (cdr args) env t))
    ((macrolet) (eval-macrolet (car args) (cdr args) env))
    ((symbol-macrolet) (eval-symbol-macrolet (car args) (cdr args) env))
    ((the) (%eval (cadr args) env))       ; ignore type for now
    ((declare) (values nil))              ; declarations are no-ops in the prototype
    ((locally) (clef-eval-seq args env))
    ((eval-when) (eval-eval-when args env))
    ((load-time-value) (%eval (car args) env))
    ((progv) (eval-progv args env))
    (t (clef-error* "Unimplemented special operator: ~s" op))))

(defun eval-setq (pairs env)
  (let ((val nil))
    (loop for (var form) on pairs by #'cddr
          do (let ((sm (clef/proto/env:lookup-symbol-macro env var)))
               (setf val (primary (clef-eval form env)))
               (if sm
                   ;; symbol macro: setq becomes setf of the expansion
                   (let ((expansion (funcall sm var env)))
                     ;; expansion is a place; only handle (car cell)-like cases
                     (clef-error* "setq of symbol-macro ~s not yet supported" expansion))
                   (clef/proto/env:set-variable-value env var val))))
    val))

(defun eval-let (bindings body env sequential)
  (let ((new (clef/proto/env:make-lexical-env env)))
    (if sequential
        (dolist (b bindings)
          (destructuring-bind (var &optional init) (if (symbolp b) (list b) b)
            (clef/proto/env:bind-variable new var (primary (clef-eval init new)))))
        ;; parallel: evaluate all inits in the outer env first
        (let ((vals (mapcar (lambda (b)
                              (destructuring-bind (var &optional init) (if (symbolp b) (list b) b)
                                (declare (ignore var))
                                (primary (clef-eval init env))))
                            bindings)))
          (loop for b in bindings for v in vals
                do (clef/proto/env:bind-variable new (if (symbolp b) b (car b)) v))))
    (clef-eval-seq body new)))

(defun eval-function (arg env)
  (cond
    ((symbolp arg) (clef/proto/env:lookup-function env arg))
    ((and (consp arg) (eq (car arg) 'cl:lambda))
     (make-clef-function (cadr arg) (cddr arg) env))
    ((and (consp arg) (eq (car arg) 'cl:setf))
     (clef/proto/env:lookup-function env arg))
    (t (clef-error* "Bad function designator: ~s" arg))))

(defun eval-block (name body env)
  "Establish a block. (return-from name v...) throws the values LIST to the
block's tag; we catch it and return those values. Normal completion returns
the body's values directly."
  (let ((tag (gensym (format nil "BLOCK-~a" name)))
        (new (clef/proto/env:make-lexical-env env)))
    (clef/proto/env:bind-block new name tag)
    (let ((result (catch tag
                    (multiple-value-list (clef-eval-seq body new)))))
      ;; If a return-from threw, RESULT is the values list it threw (which is
      ;; indistinguishable from a normal single value that happens to be a
      ;; list). We disambiguate by making return-from throw a tagged marker.
      (if (and (consp result) (eq (car result) :clef-return-values))
          (values-list (cdr result))
          (values-list result)))))

(defun eval-return-from (name value-form env)
  (let ((tag (clef/proto/env:lookup-block env name)))
    (unless tag (clef-error* "No block named ~s is lexically visible." name))
    (throw tag (cons :clef-return-values
                     (multiple-value-list (%eval value-form env))))))

;;; tagbody: evaluate forms in order; a `go` throws the target tag symbol via
;;; the unique 'TAGBODY-GO catch. We catch, jump to that tag, and continue.
(defun eval-tagbody (body env)
  (let ((new (clef/proto/env:make-lexical-env env)))
    (dolist (form body)
      (when (symbolp form)
        (clef/proto/env:bind-tag new form (gensym (format nil "TAG-~a" form)))))
    (let ((pc body))
      (loop while pc
            for form = (car pc)
            do (if (symbolp form)
                   (setf pc (cdr pc))
                   (let ((jump (catch 'tagbody-go
                                 (%eval form new)
                                 :no-go)))
                     (if (eq jump :no-go)
                         (setf pc (cdr pc))
                         (setf pc (cdr (member jump body))))))))
    (values nil)))

(defun eval-go (tag env)
  (unless (clef/proto/env:lookup-tag env tag)
    (clef-error* "No go-tag ~s is lexically visible." tag))
  (throw 'tagbody-go tag))

(defun eval-catch (args env)
  (let ((tag (primary (clef-eval (car args) env))))
    (let ((result (catch tag
                    (multiple-value-list (clef-eval-seq (cdr args) env)))))
      (if (and (consp result) (eq (car result) :clef-return-values))
          (values-list (cdr result))
          (values-list result)))))

(defun eval-throw (args env)
  (let ((tag (primary (clef-eval (car args) env)))
        (vals (multiple-value-list (%eval (cadr args) env))))
    (throw tag (cons :clef-return-values vals))))

(defun eval-unwind-protect (args env)
  (unwind-protect (values-list (clef-eval (car args) env))
    (clef-eval-seq (cdr args) env)))

(defun eval-mvc (args env)
  (let ((fn (clef/proto/env:lookup-function env (car args)))
        (arg-values (mapcan (lambda (a) (copy-list (multiple-value-list (clef-eval a env))))
                            (cdr args))))
    (multiple-value-list (clef-apply fn arg-values))))

(defun eval-mvp1 (args env)
  (let ((vals (multiple-value-list (clef-eval (car args) env))))
    (clef-eval-seq (cdr args) env)
    (values-list vals)))

(defun eval-flet (defs body env recursive)
  (let ((new (clef/proto/env:make-lexical-env env)))
    ;; For labels, functions see the new env (recursion). For flet, the outer.
    (let ((fn-env (if recursive new env)))
      (dolist (def defs)
        (destructuring-bind (name lambda-list &rest fbody) def
          (clef/proto/env:bind-function new name
                                        (make-clef-function lambda-list fbody fn-env name)))))
    (clef-eval-seq body new)))

(defun eval-macrolet (defs body env)
  (let ((new (clef/proto/env:make-lexical-env env)))
    (dolist (def defs)
      (destructuring-bind (name lambda-list &rest mbody) def
        (clef/proto/env:bind-macro new name (make-macro-expander lambda-list mbody env))))
    (clef-eval-seq body new)))

(defun eval-symbol-macrolet (defs body env)
  (let ((new (clef/proto/env:make-lexical-env env)))
    (dolist (def defs)
      (destructuring-bind (name expansion) def
        (clef/proto/env:bind-symbol-macro new name
                                          (lambda (form e) (declare (ignore form e)) expansion))))
    (clef-eval-seq body new)))

(defun eval-eval-when (args env)
  (destructuring-bind (situations &rest body) args
    ;; In an evaluator, treat :execute and eval situations as active.
    (if (or (member :execute situations) (member 'cl:eval situations)
            (member :compile-toplevel situations) (member :load-toplevel situations))
        (clef-eval-seq body env)
        (values nil))))

(defun eval-progv (args env)
  (destructuring-bind (vars-form vals-form &rest body) args
    (let ((vars (primary (clef-eval vars-form env)))
          (vals (primary (clef-eval vals-form env)))
          (new (clef/proto/env:make-lexical-env env)))
      (loop for v in vars for val in vals
            do (clef/proto/env:bind-variable new v val)
            do (clef/proto/env:proclaim-special new v))
      (clef-eval-seq body new))))

;;; --- macro expander construction ---
;;; Builds a macro function (form env -> expansion) from a defmacro-style
;;; lambda list and body. The lambda list destructures (cdr form).

(defun make-macro-expander (lambda-list body def-env)
  "Return a macro function of (form env) that destructures (cdr form) via
LAMBDA-LIST and evaluates BODY to produce the expansion."
  (let ((ll (clef/proto/ll:parse-lambda-list lambda-list :destructuring t)))
    (lambda (form call-env)
      (declare (ignore call-env))
      (let ((new (clef/proto/env:make-lexical-env def-env)))
        (clef/proto/ll:bind-lambda-list
         ll (cdr form)
         (lambda (var value) (clef/proto/env:bind-variable new var value))
         (lambda (init) (primary (clef-eval init new))))
        (primary (clef-eval-seq body new))))))

;;; --- boot-time special operators ---
;;; These compute values directly (unlike macros, whose expansions %eval would
;;; re-evaluate). They are the value-computing glue the boot library builds on.
;;; The heavy lifting (do/loop/defstruct/setf engines) lives in builtins.lisp.

(defun eval-p-destructuring-bind (args env)
  "(%destructuring-bind ll list-form fn-form) — destructure the value of
LIST-FORM against LL by applying the function from FN-FORM to it."
  (destructuring-bind (ll list-form fn-form) args
    (declare (ignore ll))
    (let ((list (primary (clef-eval list-form env)))
          (fn (primary (clef-eval fn-form env))))
      (clef-apply fn list))))

(defun eval-p-multiple-value-bind (args env)
  "(%multiple-value-bind vars mv-form fn-form) — bind VARS to the values of
MV-FORM by applying the function from FN-FORM to them."
  (destructuring-bind (vars mv-form fn-form) args
    (declare (ignore vars))
    (let ((vals (clef-eval mv-form env))
          (fn (primary (clef-eval fn-form env))))
      (clef-apply fn vals))))

(defun eval-p-do (args env sequential)
  "(%do vars end fn-form) / (%do* ...) — the do/do* engine."
  (destructuring-bind (vars end fn-form) args
    (eval-do env vars end fn-form sequential)))

(defun eval-p-unwind-protect (args env)
  "(%unwind-protect protected cleanup-fn-form)."
  (destructuring-bind (protected cleanup-fn-form) args
    (let ((cleanup (primary (clef-eval cleanup-fn-form env))))
      (unwind-protect (values-list (clef-eval protected env))
        (clef-apply cleanup '())))))

(defun eval-p-loop (args env)
  "(%loop 'body) — the loop engine (subset). The body arrives quoted from the
loop macro, so unwrap the quote."
  (let ((body (car args)))
    (when (and (consp body) (eq (car body) 'cl:quote))
      (setf body (cadr body)))
    (eval-loop env body)))

(defun %eval-primary (form env)
  "Evaluate FORM in ENV and return its primary value (host multiple values)."
  (%eval form env))

(defun eval-p-setf (args env)
  "(%setf 'place1 val1 'place2 val2 ...) — set places, return the last value.
Place subforms arrive quoted from the setf macro."
  (let ((last nil))
    (loop for (place-form val-form) on args by #'cddr
          for place = (primary (clef-eval place-form env))
          for val = (primary (clef-eval val-form env))
          do (set-place env place val)
             (setf last val))
    last))
