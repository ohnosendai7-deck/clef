;;;; contexts.lisp — package contexts for CLEF-H (DESIGN.md §7).
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; A package context is a first-class, dynamically-installable bundle of
;;;; per-package VARIABLE value cells. Function cells, macro cells, and
;;;; defconstant cells stay shared with the global environment in v1.
;;;;
;;;; Model: each context owns a table mapping symbols of its package to fresh
;;;; value cells (the same (value . nil) conses the environment uses).
;;;; Variable resolution checks the dynamically-installed context between the
;;;; lexical chain and the global toplevel cells, so the toplevel context IS
;;;; the global cells and code that never touches contexts is 100% ANSI.

(in-package #:clef/proto/contexts)

(defvar *current-context* nil
  "The dynamically-installed package context. NIL denotes the toplevel
context: the global environment's own cells, so untouched code is 100% ANSI.
Threads inherit the installing context at spawn — a spawned thread's initial
binding of *CURRENT-CONTEXT* is captured from the spawner, exactly as
CALL-IN-FRESH-THREAD-CONTEXT models.")

(defstruct (package-context (:constructor %make-package-context))
  ;; Host packages whose special variables this context owns cells for.
  (packages '() :type list)
  ;; symbol -> value cell (a cons, as in environment-vars), or :UNBOUND for a
  ;; variable explicitly made unbound in this context (suppresses fallback).
  (cells (make-hash-table :test 'eq)))

;;; --- cell plumbing ---

(defun context-package-p (context symbol)
  "True when SYMBOL's home package is one of CONTEXT's packages."
  (and (symbolp symbol)
       (member (symbol-package symbol) (package-context-packages context))
       t))

(defun context-cell (context symbol)
  "CONTEXT's entry for SYMBOL: a value cell (cons), :UNBOUND, or NIL when the
context has no entry (the symbol lazily falls back to its global cell)."
  (gethash symbol (package-context-cells context)))

(defun (setf context-cell) (value context symbol)
  (setf (gethash symbol (package-context-cells context)) value))

(defun global-cell (env symbol)
  "The global (toplevel) value cell for SYMBOL in ENV, else NIL."
  (gethash symbol
           (clef/proto/env::environment-vars (clef/proto/env::toplevel env))))

(defun lexical-cell (env symbol)
  "The value cell for SYMBOL in the nearest lexical environment below the
toplevel of ENV's chain, else NIL."
  (loop for e = env then (clef/proto/env::environment-parent e)
        while (and e (clef/proto/env::environment-parent e))
        for cell = (gethash symbol (clef/proto/env::environment-vars e))
        when cell do (return cell)))

(defun dynamic-cell (env symbol)
  "The dynamic (special) value cell for SYMBOL: the current context's cell
when it covers SYMBOL, else the global cell. Lexical bindings are skipped.
NIL when the dynamic binding is unbound."
  (let ((ctx *current-context*))
    (if (and ctx (context-package-p ctx symbol))
        (let ((entry (context-cell ctx symbol)))
          (cond ((consp entry) entry)
                ((eq entry :unbound) nil)
                ;; No per-context cell: lazily fall back to the global cell.
                (t (global-cell env symbol))))
        (global-cell env symbol))))

(defun resolve-variable-cell (env symbol)
  "The value cell a reference to SYMBOL resolves to: the nearest lexical
binding, else the current context's per-package cell, else the global cell.
NIL when unbound everywhere."
  (or (lexical-cell env symbol)
      (dynamic-cell env symbol)))

;;; --- variable access (used by the evaluator and the setf engine) ---

(defun context-variable-value (env symbol)
  "The value of variable SYMBOL, resolving through *CURRENT-CONTEXT*."
  (let ((cell (resolve-variable-cell env symbol)))
    (if cell
        (car cell)
        (error "Unbound variable: ~s" symbol))))

(defun set-context-variable-value (env symbol value)
  "SETQ/SETF of variable SYMBOL: store into the cell a reference would
resolve to. When no cell exists anywhere, define globally (the historical
implicit-toplevel behavior), except that a symbol explicitly MAKUNBOUND in
the current context gains a fresh per-context cell. Returns VALUE."
  (let ((cell (resolve-variable-cell env symbol)))
    (if cell
        (setf (car cell) value)
        (let ((ctx *current-context*))
          (if (and ctx (context-package-p ctx symbol)
                   (eq (context-cell ctx symbol) :unbound))
              (setf (context-cell ctx symbol) (cons value nil))
              (setf (gethash symbol
                             (clef/proto/env::environment-vars
                              (clef/proto/env::toplevel env)))
                    (cons value nil))))))
  value)

;;; --- symbol-level dynamic access (symbol-value & friends) ---

(defun context-symbol-value (env symbol)
  "SYMBOL-VALUE: the current dynamic value of SYMBOL (context-aware)."
  (let ((cell (dynamic-cell env symbol)))
    (if cell
        (car cell)
        (error "Unbound variable: ~s" symbol))))

(defun set-context-symbol-value (env symbol value)
  "(SETF SYMBOL-VALUE) and SET: store into SYMBOL's current dynamic cell."
  (let ((cell (dynamic-cell env symbol)))
    (if cell
        (setf (car cell) value)
        (set-context-variable-value env symbol value)))
  value)

(defun context-bound-p (env symbol)
  "BOUNDP: true when SYMBOL's current dynamic binding is bound."
  (not (null (dynamic-cell env symbol))))

(defun context-makunbound (env symbol)
  "MAKUNBOUND: make SYMBOL's current dynamic binding unbound. Inside a
covering context this unbinds the per-context binding only (the global cell
is untouched); at toplevel it removes the global cell. Returns SYMBOL."
  (let ((ctx *current-context*))
    (if (and ctx (context-package-p ctx symbol))
        (setf (context-cell ctx symbol) :unbound)
        (remhash symbol
                 (clef/proto/env::environment-vars
                  (clef/proto/env::toplevel env)))))
  symbol)

;;; --- defining forms ---

(defun active-context-for (symbol)
  "The installed context when it covers SYMBOL, else NIL."
  (let ((ctx *current-context*))
    (and ctx (context-package-p ctx symbol) ctx)))

(defun defvar-in-context (env symbol &optional value valuep)
  "DEFVAR against the current context. With a covering context installed and
no per-context cell yet, create a FRESH cell (from VALUE when VALUEP, else
snapshotting any global value) without touching the global cell. With no
covering context, plain global DEFVAR. Returns SYMBOL."
  (clef/proto/env:proclaim-special env symbol)
  (let ((ctx (active-context-for symbol)))
    (if ctx
        (unless (consp (context-cell ctx symbol))
          (setf (context-cell ctx symbol)
                (cons (if valuep
                          value
                          (let ((g (global-cell env symbol)))
                            (and g (car g))))
                      nil)))
        ;; env:defvar* takes (env name &optional (value nil valuep)) — three
        ;; arguments maximum; (and valuep value) carries the same meaning.
        (clef/proto/env:defvar* env symbol (and valuep value))))
  symbol)

(defun defparameter-in-context (env symbol value)
  "DEFPARAMETER against the current context: assign SYMBOL's per-context
cell (creating it if needed) when a covering context is installed, else the
global cell. Returns SYMBOL."
  (clef/proto/env:proclaim-special env symbol)
  (let ((ctx (active-context-for symbol)))
    (if ctx
        (let ((entry (context-cell ctx symbol)))
          (if (consp entry)
              (setf (car entry) value)
              (setf (context-cell ctx symbol) (cons value nil))))
        (clef/proto/env:defparameter* env symbol value)))
  symbol)

;;; --- context creation and synchronization ---

(defun make-package-context (env package)
  "Create a package context over PACKAGE (a package designator): a bundle of
fresh per-package VARIABLE value cells (DESIGN.md §7). Every special
variable of PACKAGE that currently has a global cell gets a fresh cell
initialized to a snapshot of the global value; function, macro, and
defconstant cells stay shared with the global environment (v1). Specials
defvar'd after context creation lazily fall back to their global cells
until SYNC-CONTEXT materializes them."
  (let ((ctx (%make-package-context :packages (list (find-package package)))))
    (sync-context env ctx)
    ctx))

(defun sync-context (env context)
  "Materialize per-context cells for every special variable of CONTEXT's
packages that has been lazily falling back to its global cell (the strict
case for defvars added after context creation). Fresh cells snapshot the
current global values. Constants stay shared; variables explicitly
MAKUNBOUND in CONTEXT are left unbound. Returns CONTEXT."
  (let ((specials (clef/proto/env::environment-specials
                   (clef/proto/env::toplevel env)))
        (constants (clef/proto/env::environment-constants
                    (clef/proto/env::toplevel env))))
    (maphash
     (lambda (symbol specialp)
       (when (and specialp
                  (context-package-p context symbol)
                  (not (gethash symbol constants))
                  (not (context-cell context symbol)))
         (let ((g (global-cell env symbol)))
           (when g
             (setf (context-cell context symbol) (cons (car g) nil))))))
     specials))
  context)

;;; --- dynamic installation ---

(defmacro with-package-env (context &body body)
  "Dynamically install CONTEXT for the extent of BODY: special-variable
access by evaluated code — direct references, SETQ, SYMBOL-VALUE, SET,
BOUNDP, MAKUNBOUND, and DEFVAR/DEFPARAMETER executed in BODY — resolves
through CONTEXT's per-package cells. Nests (an inner context shadows the
outer); the enclosing context is restored on exit, local or non-local."
  `(let ((*current-context* ,context))
     ,@body))

(defun call-in-fresh-thread-context (thunk)
  "Call THUNK with *CURRENT-CONTEXT* freshly rebound to the value it holds
at call time. This models thread spawn inheriting the installing context:
when real threads arrive, a spawned thread's initial context binding will
be captured from the spawner in exactly this way. Sharing one context
across threads is explicit opt-in shared state (DESIGN.md §7.2)."
  (let ((*current-context* *current-context*))
    (funcall thunk)))
