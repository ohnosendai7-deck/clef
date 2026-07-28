;;;; conditions.lisp — the condition system for CLEF-H.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; The condition system maps directly onto the host's (CL conditions are the
;;;; model for CLEF's; DESIGN.md §2). Conditions are host condition objects;
;;;; signalling, handlers, and restarts delegate to the host. The macros turn
;;;; CLEF-H bodies into host thunks; clause/restart bodies run in the CLEF-H
;;;; lexical environment.

(in-package #:clef/proto/eval)

(defun install-conditions (env)
  "Install the condition system into ENV."
  (let* ((cl-user (find-package :cl-user))
         (sym (lambda (n) (intern (string-upcase n) cl-user)))
         (regm (lambda (n fn) (clef/proto/env:bind-macro env (funcall sym n) fn)))
         (regf (lambda (n fn) (clef/proto/env:set-function-value env (funcall sym n) fn))))

    ;;; --- signalling ---
    (funcall regf "error"
             (lambda (datum &rest args) (error (condition-designator datum args 'simple-error))))
    (funcall regf "cerror"
             (lambda (continue datum &rest args)
               (cerror continue (condition-designator datum args 'simple-error))))
    (funcall regf "warn"
             (lambda (datum &rest args) (warn (condition-designator datum args 'simple-warning))))
    (funcall regf "signal"
             (lambda (datum &rest args) (signal (condition-designator datum args 'simple-condition))))
    (funcall regf "invoke-debugger" (lambda (c) (invoke-debugger c)))
    (funcall regf "make-condition"
             (lambda (type &rest initargs) (apply #'make-condition type initargs)))
    (funcall regf "conditionp" (lambda (x) (typep x 'condition)))

    ;;; --- condition accessors (standard CL only) ---
    (funcall regf "cell-error-name" #'cell-error-name)
    (funcall regf "type-error-datum" #'type-error-datum)
    (funcall regf "type-error-expected-type" #'type-error-expected-type)
    (funcall regf "package-error-package" #'package-error-package)
    (funcall regf "stream-error-stream" #'stream-error-stream)
    (funcall regf "file-error-pathname" #'file-error-pathname)
    (funcall regf "arithmetic-error-operation" #'arithmetic-error-operation)
    (funcall regf "arithmetic-error-operands" #'arithmetic-error-operands)
    (funcall regf "simple-condition-format-control" #'simple-condition-format-control)
    (funcall regf "simple-condition-format-arguments" #'simple-condition-format-arguments)

    ;;; --- restarts (functions) ---
    (funcall regf "find-restart"
             (lambda (name &optional condition)
               (declare (ignore condition))
               (or (emulated-find-restart name) (find-restart name))))
    (funcall regf "compute-restarts"
             (lambda (&optional condition) (compute-restarts condition)))
    (funcall regf "restart-name" #'restart-name)
    (funcall regf "invoke-restart"
             (lambda (restart &rest args)
               (if (functionp restart)
                   (apply restart args)          ; emulated restart (a function)
                   (if (and (symbolp restart) (emulated-find-restart restart))
                       (apply (emulated-find-restart restart) args)
                       (apply #'invoke-restart restart args)))))
    (funcall regf "invoke-restart-interactively"
             (lambda (restart) (invoke-restart-interactively restart)))
    (funcall regf "abort" (lambda () (invoke-restart (find-restart 'abort))))
    (funcall regf "continue"
             (lambda () (let ((r (find-restart 'continue))) (and r (invoke-restart r)))))
    (funcall regf "muffle-warning"
             (lambda () (invoke-restart (find-restart 'muffle-warning))))
    (funcall regf "store-value"
             (lambda (v) (let ((r (find-restart 'store-value))) (and r (invoke-restart r v)))))
    (funcall regf "use-value"
             (lambda (v) (let ((r (find-restart 'use-value))) (and r (invoke-restart r v)))))

    ;;; --- handler macros: bodies become host thunks; clause/restart bodies
    ;;; run in the CLEF-H env. The % control operators they expand to are
    ;;; SPECIAL OPERATORS (see eval.lisp), not functions. ---

    (funcall regm "handler-case"
             (lambda (form call-env)
               (declare (ignore call-env))
               `(%handler-case (lambda () ,(cadr form)) ',(cddr form))))

    (funcall regm "handler-bind"
             (lambda (form call-env)
               (declare (ignore call-env))
               `(%handler-bind ',(cadr form) (lambda () ,@(cddr form)))))

    (funcall regm "restart-case"
             (lambda (form call-env)
               (declare (ignore call-env))
               `(%restart-case (lambda () ,(cadr form)) ',(cddr form))))

    (funcall regm "ignore-errors"
             (lambda (form call-env)
               (declare (ignore call-env))
               `(%ignore-errors (lambda () ,@(cdr form)))))

    (funcall regm "with-simple-restart"
             (lambda (form call-env)
               (declare (ignore call-env))
               (destructuring-bind ((name format-control &rest format-args) &body body) (cdr form)
                 `(%with-simple-restart ',name ,format-control (list ,@format-args)
                    (lambda () ,@body)))))

    ;;; --- define-condition ---
    (funcall regm "define-condition"
             (lambda (form call-env)
               (declare (ignore call-env))
               (destructuring-bind (name supers slots &rest options) (cdr form)
                 (eval-define-condition env name supers slots options)
                 `',name)))

    env))

;;; --- condition designator ---

(defun condition-designator (datum args default-type)
  "Turn an error/warn DATUM + ARGS into a condition object. Strings become
DEFAULT-TYPE with the string as format-control."
  (cond
    ((typep datum 'condition) datum)
    ((symbolp datum) (apply #'make-condition datum args))
    ((stringp datum) (make-condition default-type
                                     :format-control datum
                                     :format-arguments args))
    (t (make-condition default-type
                       :format-control "~s"
                       :format-arguments (list datum)))))

;;; --- handler-case ---
;;; clauses: ((type (var) body...) ...)  (:no-error is not yet supported)
;;;
;;; handler-bind is a macro that needs literal (type fn) bindings, so we can't
;;; call it with a computed list. Instead we register each handler function in
;;; a global table under a gensym and eval a handler-bind form that references
;;; the gensyms.

(defvar *handler-fns* (make-hash-table :test 'eq)
  "Maps gensym tags to condition handler functions for runtime handler-bind.")

(defun register-handler (fn)
  (let ((tag (gensym "HANDLER")))
    (setf (gethash tag *handler-fns*) fn)
    tag))

(defun call-handler-by-tag (tag condition)
  (funcall (gethash tag *handler-fns*) condition))

(defun call-with-runtime-handlers (bindings thunk)
  "Run THUNK under a host handler-bind whose (type handler) pairs come from
BINDINGS, a runtime list of (type function). handler-bind is a macro needing
literal bindings, so we register functions under gensyms and eval a literal
form; the thunk itself is invoked via a registered reference."
  (let* ((tagged (mapcar (lambda (b) (list (car b) (register-handler (cadr b))))
                         bindings))
         (literal-binds
           (mapcar (lambda (tb)
                     (list (car tb)
                           `(lambda (c) (call-handler-by-tag ',(cadr tb) c))))
                   tagged))
         (thunk-tag (register-handler (lambda (c) (declare (ignore c)) (funcall thunk)))))
    (eval `(handler-bind ,literal-binds
              (call-handler-by-tag ',thunk-tag nil)))))

(defun eval-handler-case (env body-thunk clauses)
  "Run BODY-THUNK; on a signalled condition, invoke the matching clause's
handler and return its body's values (aborting the protected body)."
  (let ((tag (gensym "HANDLER-CASE")))
    (catch tag
      (call-with-runtime-handlers
          (mapcar (lambda (clause)
                    (let ((type (car clause))
                          (var (car (cadr clause)))
                          (cbody (cddr clause)))
                      (list type
                            (lambda (condition)
                              (throw tag
                                (let ((new (clef/proto/env:make-lexical-env env)))
                                  (when var
                                    (clef/proto/env:bind-variable new var condition))
                                  (clef-eval-seq cbody new)))))))
                  clauses)
        body-thunk))))

;;; --- handler-bind ---

(defun eval-handler-bind (env bindings body-thunk)
  "Run BODY-THUNK with handlers from ((type handler-form)...). A handler that
returns normally lets signalling continue."
  (call-with-runtime-handlers
      (mapcar (lambda (b)
                (let ((type (car b))
                      (handler-form (cadr b)))
                  (list type
                        (lambda (condition)
                          (let ((fn (primary (clef-eval handler-form env))))
                            (clef-apply fn (list condition)))))))
              bindings)
    body-thunk))

;;; --- restart-case ---
;;; clauses: ((name arglist body...) ...) — we install host restarts whose
;;; functions run the clause bodies in the CLEF-H env.

(defun eval-restart-case (env body-thunk clauses)
  "Run BODY-THUNK with restarts installed. Each clause becomes a host restart
whose function runs the clause body in the CLEF-H env."
  (call-with-host-restart-bind
   (mapcar (lambda (clause)
             (let ((name (car clause))
                   (arglist (cadr clause))
                   (body (cddr clause)))
               (list name
                     (lambda (&rest rargs)
                       (run-restart-clause env arglist body rargs)))))
           clauses)
   body-thunk))

(defun call-with-host-restart-bind (restart-bindings thunk)
  "Bind restarts ((name fn)...) around THUNK using the host's restart
machinery, recursing one restart at a time. Names and functions are taken
from the bindings (evaluated), not read as literals."
  (if (null restart-bindings)
      (funcall thunk)
      (destructuring-bind (name fn) (car restart-bindings)
        (with-computed-restart name fn
          (lambda () (call-with-host-restart-bind (cdr restart-bindings) thunk))))))

;;; Install a single restart with a computed name and function around THUNK.
;;; We go through the host restart machinery via a helper that SBCL exposes;
;;; if unavailable, emulate by recording the restart in a dynamic cluster the
;;; invoke-restart wrappers consult.
(defvar *emulated-restarts* nil
  "Dynamically bound alist of (name . function) for emulated restarts.")

(defun with-computed-restart (name fn thunk)
  "Run THUNK with restart NAME (a symbol) bound to FN. Uses the host
restart-bind via a macro-free path: we push onto a dynamic cluster and rely
on find-restart/invoke-restart wrappers that consult it."
  (let ((*emulated-restarts* (acons name fn *emulated-restarts*)))
    (funcall thunk)))

(defun emulated-find-restart (name)
  (cdr (assoc name *emulated-restarts*)))

(defun emulated-invoke-restart (name args)
  (let ((fn (emulated-find-restart name)))
    (unless fn (error "No restart named ~s is active." name))
    (apply fn args)))

(defun run-restart-clause (env arglist body rargs)
  "The function a host restart calls: bind ARGLIST to RARGS in a fresh
CLEF-H env and run BODY."
  (let ((new (clef/proto/env:make-lexical-env env)))
    (clef/proto/ll:bind-lambda-list
     (clef/proto/ll:parse-lambda-list arglist)
     rargs
     (lambda (v val) (clef/proto/env:bind-variable new v val))
     (lambda (init) (primary (clef-eval init new))))
    (clef-eval-seq body new)))

;;; --- define-condition ---

(defun eval-define-condition (env name supers slots options)
  "Define a condition type via the host's define-condition, and register its
slot accessors (:reader/:writer/:accessor) into the CLEF-H function
namespace so condition slots are reachable from CLEF-H code."
  (let ((report (second (member :report options)))
        (default-initargs (second (member :default-initargs options)))
        (documentation (second (member :documentation options))))
    (eval
     `(define-condition ,name ,(or supers '(condition))
        ,slots
        ,@(when report `((:report ,report)))
        ,@(when default-initargs `((:default-initargs ,@default-initargs)))
        ,@(when documentation `((:documentation ,documentation))))))
  ;; register accessors into the CLEF-H env
  (dolist (slot slots)
    (when (consp slot)
      (loop for (k v) on (cdr slot) by #'cddr
            do (case k
                 ((:reader :accessor)
                  (clef/proto/env:set-function-value env v (fdefinition v)))
                 (:writer
                  (clef/proto/env:set-function-value env v (fdefinition v)))))))
  name)
