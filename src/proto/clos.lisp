;;;; clos.lisp — a CLOS subset for CLEF-H.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; Implements defclass, defgeneric, defmethod, make-instance, slot-value,
;;;; and standard method combination (before/after/around/primary,
;;;; call-next-method) with class and eql specializers, single and multiple
;;;; inheritance. Classes and instances are host structures; the evaluator
;;;; dispatches generic functions through here.

(in-package #:clef/proto/eval)

;;; --- representation ---

(defstruct (cclass (:conc-name cclass-))
  name                    ; symbol
  direct-supers           ; list of superclass names (symbols)
  slots                   ; list of slot-defs
  (cpl nil))              ; class precedence list (list of cclass), cached

(defstruct (slot-def (:conc-name sd-))
  name initform initarg (initform-p nil) accessors)

(defstruct (cinstance (:conc-name ci-))
  class                   ; cclass
  (slots (make-hash-table :test 'eq)))  ; slot-name -> value

(defstruct (cgeneric (:conc-name cg-))
  name
  lambda-list
  (methods '()))          ; list of cmethod

(defstruct (cmethod (:conc-name cm-))
  specializers            ; list (aligned to required params): class-name | (:eql val)
  qualifiers              ; subset of (:before :after :around) or ()
  lambda-list
  function)               ; clef-function or host function of the method args

;;; Registries live in the toplevel environment so they are per-image.
(defun environment-misc-table (env)
  (clef/proto/env::environment-misc (clef/proto/env::toplevel env)))

(defun class-registry (env)
  (or (gethash :clos-classes (environment-misc-table env))
      (setf (gethash :clos-classes (environment-misc-table env))
            (make-hash-table :test 'eq))))

(defun generic-registry (env)
  (or (gethash :clos-generics (environment-misc-table env))
      (setf (gethash :clos-generics (environment-misc-table env))
            (make-hash-table :test 'eq))))

;;; --- deftype: user-defined derived type specifiers ---
;;; Expanders live in a per-image registry (like the class registry); each
;;; expander is a function of the specifier's arguments returning a new type
;;; specifier. typep/subtypep expand deftype'd specifiers recursively.

(defun type-expander-registry (env)
  (or (gethash :deftype-expanders (environment-misc-table env))
      (setf (gethash :deftype-expanders (environment-misc-table env))
            (make-hash-table :test 'eq))))

(defun register-type-expander (env name expander)
  "Register EXPANDER (a function of the type specifier's args) for deftype
NAME. Returns NAME."
  (setf (gethash name (type-expander-registry env)) expander)
  name)

(defun find-type-expander (env type-spec)
  "The deftype expander applicable to TYPE-SPEC (a symbol naming a deftype,
or a compound specifier whose car does), else NIL."
  (let ((name (cond ((symbolp type-spec) type-spec)
                    ((and (consp type-spec) (symbolp (car type-spec)))
                     (car type-spec))
                    (t nil))))
    (and name (gethash name (type-expander-registry env)))))

(defun expand-deftype (env type-spec)
  "One deftype expansion step. Returns (values expansion expanded-p)."
  (let ((expander (find-type-expander env type-spec)))
    (if expander
        (values (clef-apply expander (if (consp type-spec) (cdr type-spec) '()))
                t)
        (values type-spec nil))))

;;; --- find-class / class-of ---

(defun clos-find-class (env name &optional errorp)
  (let ((c (gethash name (class-registry env))))
    (or c (and errorp (error "No class named ~s" name)))))

(defun (setf clos-find-class) (class env name)
  (setf (gethash name (class-registry env)) class))

(defun clos-class-of (env obj)
  (if (cinstance-p obj)
      (ci-class obj)
      ;; built-in classes for host types
      (or (gethash (type-of obj) (class-registry env))
          (gethash 'cl:t (class-registry env)))))

;;; --- CPL: depth-first left-to-right linearization with duplicates removed ---

(defun compute-cpl (env class)
  "Standard depth-first linearization of CLASS's superclass graph."
  (let ((result '()))
    (labels ((visit (c)
               (unless (member c result)
                 (push c result)
                 (dolist (sup-name (cclass-direct-supers c))
                   (let ((sup (clos-find-class env sup-name)))
                     (when sup (visit sup)))))))
      (visit class))
    ;; depth-first gives most-specific-last here; reverse for most-specific-first
    (nreverse result)))

(defun ensure-cpl (env class)
  (or (cclass-cpl class)
      (setf (cclass-cpl class) (compute-cpl env class))))

(defun subclass-of-p (env sub super)
  "Is SUB (cclass) a subclass of or equal to SUPER (cclass)? Returns a boolean."
  (and (member super (ensure-cpl env sub)) t))

;;; --- slot collection (all slots in the CPL, subclass slots win) ---

(defun all-slots (env class)
  "Effective slots of CLASS: walk CPL most-specific-first, first occurrence wins."
  (let ((seen '()) (out '()))
    (dolist (c (ensure-cpl env class) (nreverse out))
      (dolist (sd (cclass-slots c))
        (unless (member (sd-name sd) seen)
          (push (sd-name sd) seen)
          (push sd out))))))

;;; --- instance allocation/init ---

(defun clos-make-instance (env class-name initargs)
  (let* ((class (clos-find-class env class-name t))
         (inst (make-cinstance :class class)))
    (dolist (sd (all-slots env class))
      (let* ((name (sd-name sd))
             (kw (intern (symbol-name name) :keyword))
             (initarg (or (sd-initarg sd) kw)))
        (setf (gethash name (ci-slots inst))
              (cond ((key-present initarg initargs) (getf initargs initarg))
                    ((sd-initform-p sd)
                     (primary (clef-eval (sd-initform sd) env)))
                    (t nil)))))
    inst))

(defun key-present (kw plist)
  (loop for (k v) on plist by #'cddr thereis (eq k kw)))

;;; --- slot-value ---

(defun clos-slot-value (obj slot-name)
  (unless (cinstance-p obj)
    (error "slot-value of non-instance ~s" obj))
  (multiple-value-bind (v present) (gethash slot-name (ci-slots obj))
    (if present v (error "Slot ~s is unbound in ~s" slot-name obj))))

(defun (setf clos-slot-value) (value obj slot-name)
  (unless (cinstance-p obj)
    (error "(setf slot-value) of non-instance ~s" obj))
  (setf (gethash slot-name (ci-slots obj)) value))

;;; --- defclass ---

(defun parse-slot-spec (spec)
  "Parse a defclass slot specifier into a slot-def."
  (if (symbolp spec)
      (make-slot-def :name spec)
      (let ((name (car spec))
            (initform nil) (initform-p nil) (initarg nil) (accessors '()))
        (loop for (k v) on (cdr spec) by #'cddr
              do (case k
                   (:initform (setf initform v initform-p t))
                   (:initarg (setf initarg v))
                   ((:accessor :reader :writer) (push (list k v) accessors))))
        (make-slot-def :name name :initform initform :initform-p initform-p
                       :initarg initarg :accessors accessors))))

(defun clos-defclass (env name supers slot-specs options)
  "Define/redefine class NAME. Returns the class name."
  (declare (ignore options))
  (let* ((slots (mapcar #'parse-slot-spec slot-specs))
         (class (make-cclass :name name :direct-supers supers :slots slots)))
    (setf (clos-find-class env name) class)
    ;; invalidate CPL for this and any subclasses (lazy recompute)
    (setf (cclass-cpl class) nil)
    ;; define accessors as plain functions that read/write the slot directly.
    (dolist (sd slots)
      (dolist (acc (sd-accessors sd))
        (destructuring-bind (kind fname) acc
          (let ((slot-name (sd-name sd)))
            (ecase kind
              ((:accessor :reader)
               (clef/proto/env:set-function-value
                env fname (lambda (obj) (clos-slot-value obj slot-name))))
              (:writer
               (clef/proto/env:set-function-value
                env fname (lambda (newval obj)
                            (setf (clos-slot-value obj slot-name) newval)))))
            ;; An :accessor also gets a (setf fname) function so that
            ;; (setf (fname obj) v) works (e.g. through with-accessors).
            (when (eq kind :accessor)
              (clef/proto/env:set-function-value
               env (list 'cl:setf fname)
               (lambda (newval obj)
                 (setf (clos-slot-value obj slot-name) newval))))))))
    name))

;;; --- defgeneric / defmethod ---

(defun clos-defgeneric (env name)
  "Ensure a generic function NAME exists and is callable."
  (or (gethash name (generic-registry env))
      (let ((gen (make-cgeneric :name name)))
        (setf (gethash name (generic-registry env)) gen)
        ;; install a dispatcher as the function value
        (clef/proto/env:set-function-value
         env name (lambda (&rest args) (clos-dispatch env gen args)))
        gen)))

(defun clos-defmethod (env name lambda-list body &optional (qualifiers '()))
  "Add a method to generic NAME. LAMBDA-LIST has specialized required params."
  (let* ((gen (clos-defgeneric env name))
         ;; split required (specialized) params from the rest
         (reqs (loop for p in lambda-list
                     until (member p '(&optional &rest &key &aux &allow-other-keys))
                     collect p))
         (specializers (mapcar #'parse-specializer reqs))
         (plain-ll (mapcar (lambda (p) (if (consp p) (car p) p)) lambda-list))
         ;; build the method function: binds plain lambda list, runs body
         (fn (make-clef-function plain-ll body env)))
    (push (make-cmethod :specializers specializers
                        :qualifiers qualifiers
                        :lambda-list plain-ll
                        :function fn)
          (cg-methods gen))
    name))

(defun parse-specializer (param)
  "Parse a required param: var -> (var t); (var class) -> (var class);
(var (eql form)) -> (var (:eql form)). Returns the specializer descriptor."
  (cond ((symbolp param) 'cl:t)
        ((and (consp param) (consp (cadr param)) (eq (caadr param) 'cl:eql))
         (list :eql (cadr (cadr param))))
        ((consp param) (cadr param))
        (t 'cl:t)))

;;; --- dispatch ---

(defun method-applicable-p (env method args)
  "Does METHOD apply to ARGS (by required-arg specializers)?"
  (loop for spec in (cm-specializers method)
        for arg in args
        always (specializer-matches-p env spec arg)))

(defun specializer-matches-p (env spec arg)
  (cond
    ((eq spec 'cl:t) t)
    ((and (consp spec) (eq (car spec) :eql))
     (eql arg (primary (clef-eval (cadr spec) env))))
    (t
     ;; class specializer: arg's class CPL must include SPEC's class
     (let ((arg-class (clos-class-of env arg))
           (spec-class (clos-find-class env spec)))
       (and spec-class arg-class (subclass-of-p env arg-class spec-class))))))

(defun method-more-specific-p (env m1 m2 args)
  "Is M1 more specific than M2 for ARGS (first differing specializer)?"
  (loop for s1 in (cm-specializers m1)
        for s2 in (cm-specializers m2)
        for arg in args
        unless (equal s1 s2)
          do (return (specializer-more-specific-p env s1 s2 arg))
        finally (return nil)))

(defun specializer-more-specific-p (env s1 s2 arg)
  "Is specializer S1 more specific than S2 for ARG?"
  (cond
    ;; eql beats any class
    ((and (consp s1) (eq (car s1) :eql)) t)
    ((and (consp s2) (eq (car s2) :eql)) nil)
    ((eq s1 'cl:t) nil)
    ((eq s2 'cl:t) t)
    (t
     ;; earlier in arg's CPL = more specific
     (let* ((arg-class (clos-class-of env arg))
            (cpl (ensure-cpl env arg-class))
            (c1 (clos-find-class env s1))
            (c2 (clos-find-class env s2)))
       (< (position c1 cpl) (position c2 cpl))))))

(defun applicable-methods (env gen args)
  "Applicable primary-relevant methods, sorted most-specific-first."
  (let ((applicable (remove-if-not (lambda (m) (method-applicable-p env m args))
                                   (cg-methods gen))))
    (sort applicable (lambda (a b) (method-more-specific-p env a b args)))))

(defvar *next-methods* nil
  "Dynamically bound list of remaining methods for call-next-method.")
(defvar *method-args* nil
  "Dynamically bound original arguments, so a zero-arg (call-next-method)
re-passes them to the next method.")

(defun clos-dispatch (env gen args)
  "Standard method combination: around, before, primary (chain), after."
  (let* ((sorted (applicable-methods env gen args))
         (around (remove-if-not (lambda (m) (member :around (cm-qualifiers m))) sorted))
         (befores (remove-if-not (lambda (m) (member :before (cm-qualifiers m))) sorted))
         (afters (remove-if-not (lambda (m) (member :after (cm-qualifiers m))) sorted))
         (primaries (remove-if (lambda (m) (cm-qualifiers m)) sorted)))
    (flet ((call-primary-chain ()
             ;; run befores (most specific first)
             (dolist (m befores) (clef-apply (cm-function m) args))
             ;; run primaries as a chain with call-next-method
             (let ((result (call-method-chain env primaries args)))
               ;; run afters (least specific first)
               (dolist (m (reverse afters)) (clef-apply (cm-function m) args))
               result)))
      (if around
          (call-method-chain env (append around (list (make-wrapper (cg-name gen) #'call-primary-chain))) args)
          (call-primary-chain)))))

(defun make-wrapper (name thunk)
  "A pseudo-method wrapping a thunk (for around -> primary chaining)."
  (make-cmethod :specializers '() :qualifiers '() :lambda-list '()
                :function (lambda (&rest args) (declare (ignore args)) (funcall thunk))))

(defun call-method-chain (env methods args)
  "Call METHODS as a call-next-method chain. Returns the first method's values."
  (if (null methods)
      (error "No applicable method.")
      (let ((*next-methods* (cdr methods))
            (*method-args* args))
        (clef-apply (cm-function (car methods)) args))))

(defun call-next-method-fn (env args)
  (declare (ignore env))
  (if (null *next-methods*)
      (error "call-next-method with no next method")
      ;; A zero-arg (call-next-method) re-passes the original args; an explicit
      ;; arg list overrides.
      (call-method-chain env *next-methods* (if args args *method-args*))))

(defun next-method-p-fn () (not (null *next-methods*)))

;;; --- boot glue: macros and user-facing functions ---

(defun install-clos (env)
  "Install CLOS into ENV: defclass/defgeneric/defmethod macros and the
make-instance/slot-value/find-class/class-of/call-next-method functions."
  (let* ((cl-user (find-package :cl-user))
         (sym (lambda (n) (intern (string-upcase n) cl-user)))
         (regm (lambda (n fn) (clef/proto/env:bind-macro env (funcall sym n) fn)))
         (regf (lambda (n fn) (clef/proto/env:set-function-value env (funcall sym n) fn))))

    ;; defclass: (defclass name (supers...) (slot-specs...) options...)
    (funcall regm "defclass"
             (lambda (form call-env)
               (declare (ignore call-env))
               (destructuring-bind (name supers slots &rest options) (cdr form)
                 (clos-defclass env name supers slots options)
                 `',name)))

    ;; defgeneric: (defgeneric name lambda-list options...)
    (funcall regm "defgeneric"
             (lambda (form call-env)
               (declare (ignore call-env))
               (destructuring-bind (name lambda-list &rest options) (cdr form)
                 (declare (ignore options))
                 (clos-defgeneric env name)
                 `',name)))

    ;; defmethod: (defmethod name [qualifier] lambda-list body...)
    (funcall regm "defmethod"
             (lambda (form call-env)
               (declare (ignore call-env))
               (let* ((rest (cdr form))
                      (name (car rest))
                      (after-name (cdr rest))
                      ;; optional qualifier keyword (:before/:after/:around)
                      (qualifiers (if (keywordp (car after-name))
                                      (list (pop after-name))
                                      '()))
                      (lambda-list (car after-name))
                      (body (cdr after-name)))
                 (clos-defmethod env name lambda-list body qualifiers)
                 `',name)))

    ;; make-instance
    (funcall regf "make-instance"
             (lambda (class-name &rest initargs)
               (clos-make-instance env class-name initargs)))

    ;; slot-value and (setf slot-value)
    (funcall regf "slot-value"
             (lambda (obj slot-name) (clos-slot-value obj slot-name)))
    ;; (setf slot-value) is used as a function of 3 args internally
    (funcall regf "%set-slot-value"
             (lambda (value obj slot-name)
               (setf (clos-slot-value obj slot-name) value)))

    ;; find-class / class-of / class-name
    (funcall regf "find-class"
             (lambda (name &optional errorp) (clos-find-class env name errorp)))
    (funcall regf "class-of" (lambda (obj) (clos-class-of env obj)))
    (funcall regf "class-name" (lambda (class) (cclass-name class)))

    ;; call-next-method / next-method-p
    (funcall regf "call-next-method"
             (lambda (&rest args) (call-next-method-fn env args)))
    (funcall regf "next-method-p" (lambda () (next-method-p-fn)))

    ;; instance predicate
    (funcall regf "clos-instance-p" (lambda (x) (cinstance-p x)))

    ;; typep / subtypep that understand CLOS classes (override the host fns
    ;; installed by install-builtins). These return the primary value only.
    (funcall regf "typep" (lambda (obj type-spec)
                            (nth-value 0 (clos-typep env obj type-spec))))
    (funcall regf "subtypep" (lambda (t1 t2)
                               (nth-value 0 (clos-subtypep env t1 t2))))

    env))

;;; --- typep / subtypep integration ---

(defun clos-typep (env obj type-spec)
  "TYPEP understanding CLOS classes and deftype. TYPE-SPEC is a class name
symbol, a deftype specifier, or a host type specifier. Returns (values yes-p
certain-p)."
  (cond
    ;; a CLOS class name?
    ((and (symbolp type-spec) (clos-find-class env type-spec))
     (let ((spec-class (clos-find-class env type-spec))
           (obj-class (clos-class-of env obj)))
       (values (and obj-class spec-class (subclass-of-p env obj-class spec-class))
               t)))
    ;; a deftype? expand (recursively, via the recursive call) and retry
    ((find-type-expander env type-spec)
     (clos-typep env obj (expand-deftype env type-spec)))
    ;; fall back to host typep
    (t (values (typep obj type-spec) t))))

(defun clos-subtypep (env t1 t2)
  "SUBTYPEP for CLOS class names and deftype specifiers."
  (multiple-value-bind (e1 x1) (expand-deftype env t1)
    (multiple-value-bind (e2 x2) (expand-deftype env t2)
      (if (or x1 x2)
          (clos-subtypep env e1 e2)
          (let ((c1 (and (symbolp t1) (clos-find-class env t1)))
                (c2 (and (symbolp t2) (clos-find-class env t2))))
            (if (and c1 c2)
                (values (subclass-of-p env c1 c2) t)
                (values (subtypep t1 t2) t)))))))
