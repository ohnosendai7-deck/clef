;;;; env.lisp — first-class environments for CLEF-H.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; Environments are first-class values (SICL-style, DESIGN.md §8), so the
;;;; target's global environment never collides with the host's. Lexical
;;;; environments chain to a toplevel.

(in-package #:clef/proto/env)

(defstruct (environment (:constructor %make-environment))
  (parent nil :type (or null environment))
  ;; Lexical bindings. Each is a hash table from symbol to a cons cell whose
  ;; car is the current value (so SETF is shared with captured closures).
  (vars (make-hash-table :test 'eq))
  (funs (make-hash-table :test 'eq))
  ;; Symbol macros: symbol -> expansion function (form env -> expansion).
  (symbol-macros (make-hash-table :test 'eq))
  ;; Block / tagbody names -> unique catch tags (gensyms).
  (blocks (make-hash-table :test 'eq))
  (tags (make-hash-table :test 'eq))
  ;; Toplevel-only slots.
  (toplevel-p nil)
  (specials (make-hash-table :test 'eq)) ; proclaimed/special variables
  (constants (make-hash-table :test 'eq))
  ;; Free-form per-image storage (CLOS registries, etc.).
  (misc (make-hash-table :test 'eq)))

(defun make-toplevel-env ()
  "A fresh global environment."
  (%make-environment :parent nil :toplevel-p t))

(defun make-lexical-env (parent)
  "A fresh lexical environment chaining to PARENT."
  (%make-environment :parent parent))

(defun toplevel (env)
  (loop for e = env then (environment-parent e)
        while (environment-parent e)
        finally (return e)))

;;; --- variables ---

(defun variable-cell (env name)
  "The value cell for NAME in the nearest env that has it, else NIL."
  (loop for e = env then (environment-parent e)
        while e
        for cell = (gethash name (environment-vars e))
        when cell do (return cell)
        finally (return nil)))

(defun variable-bound-p (env name)
  (not (null (variable-cell env name))))

(defun lookup-variable (env name)
  "The value of variable NAME. Unbound variables signal an error on the host."
  (let ((cell (variable-cell env name)))
    (if cell
        (car cell)
        (error "Unbound variable: ~s" name))))

(defun set-variable-value (env name value)
  "Set an existing variable, or create a toplevel (special) binding."
  (let ((cell (variable-cell env name)))
    (if cell
        (setf (car cell) value)
        ;; Implicit toplevel definition.
        (setf (gethash name (environment-vars (toplevel env)))
              (cons value nil))
        )))

(defun bind-variable (env name value)
  "Create a new lexical binding of NAME to VALUE in ENV."
  (setf (gethash name (environment-vars env)) (cons value nil)))

(defun special-variable-p (env name)
  (gethash name (environment-specials (toplevel env))))

(defun proclaim-special (env name)
  (setf (gethash name (environment-specials (toplevel env))) t))

(defun defvar* (env name &optional (value nil valuep))
  "Define a special variable; only set value if currently unbound."
  (proclaim-special env name)
  (let ((top (toplevel env)))
    (unless (gethash name (environment-vars top))
      (setf (gethash name (environment-vars top)) (cons (and valuep value) nil))))
  name)

(defun defparameter* (env name value)
  (proclaim-special env name)
  (setf (gethash name (environment-vars (toplevel env))) (cons value nil))
  name)

(defun defconstant* (env name value)
  (proclaim-special env name)
  (setf (gethash name (environment-vars (toplevel env))) (cons value nil))
  (setf (gethash name (environment-constants (toplevel env))) t)
  name)

;;; --- functions ---

(defun lookup-function (env name)
  (loop for e = env then (environment-parent e)
        while e
        for cell = (gethash name (environment-funs e))
        when cell do (return (car cell))
        finally (error "Undefined function: ~s" name)))

(defun function-bound-p (env name)
  (loop for e = env then (environment-parent e)
        while e
        for cell = (gethash name (environment-funs e))
        when cell do (return t)
        finally (return nil)))

(defun bind-function (env name fn)
  (setf (gethash name (environment-funs env)) (cons fn nil)))

(defun set-function-value (env name fn)
  (bind-function (toplevel env) name fn))

;;; --- macros (macro function: form env -> expansion) ---

(defun lookup-macro (env name)
  (loop for e = env then (environment-parent e)
        while e
        for cell = (gethash name (environment-funs e))
        when (and cell (macro-function-p (car cell))) do (return (car cell))
        finally (return nil)))

(defun macro-function-p (x)
  (and (consp x) (eq (car x) :macro)))

(defun make-macro (fn) (cons :macro fn))
(defun macro-fn (m) (cdr m))

(defun bind-macro (env name macro-fn)
  "Bind NAME as a macro whose expander is MACRO-FN (form env -> expansion)."
  (setf (gethash name (environment-funs env)) (cons (make-macro macro-fn) nil)))

;;; --- special operators ---

(defparameter *special-operators*
  '(block catch eval-when flet function go if labels let let*
    load-time-value locally macrolet multiple-value-call multiple-value-prog1
    progn progv quote return-from setq symbol-macrolet tagbody the throw
    unwind-protect declare)
  "The 25 ANSI special operators, plus DECLARE (a no-op declaration form).")

(defparameter *internal-special-operators*
  '("%DESTRUCTURING-BIND" "%MULTIPLE-VALUE-BIND" "%DO" "%DO*"
    "%UNWIND-PROTECT" "%LOOP" "%SETF")
  "Boot-time special operator names (matched by symbol-name, any package).")

(defun special-operator-p (name)
  (and (symbolp name)
       (or (member name *special-operators*)
           (member (symbol-name name) *internal-special-operators*
                   :test #'string-equal))
       t))

;;; --- symbol macros ---

(defun lookup-symbol-macro (env name)
  (loop for e = env then (environment-parent e)
        while e
        for cell = (gethash name (environment-symbol-macros e))
        when cell do (return (car cell))
        finally (return nil)))

(defun bind-symbol-macro (env name expander)
  (setf (gethash name (environment-symbol-macros env)) (cons expander nil)))

;;; --- blocks and tags (binding scope only; control uses host throw/catch) ---

(defun bind-block (env name catch-tag)
  (setf (gethash name (environment-blocks env)) (cons catch-tag nil)))

(defun lookup-block (env name)
  "The catch tag for the nearest enclosing block NAME, else NIL."
  (loop for e = env then (environment-parent e)
        while e
        for cell = (gethash name (environment-blocks e))
        when cell do (return (car cell))
        finally (return nil)))

(defun bind-tag (env name catch-tag)
  (setf (gethash name (environment-tags env)) (cons catch-tag nil)))

(defun lookup-tag (env name)
  "The catch tag for the nearest enclosing go-tag NAME, else NIL."
  (loop for e = env then (environment-parent e)
        while e
        for cell = (gethash name (environment-tags e))
        when cell do (return (car cell))
        finally (return nil)))
