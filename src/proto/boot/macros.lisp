;;;; macros.lisp — the core macro layer, defined in CLEF-H Lisp.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; This file is evaluated by CLEF-H itself (via load-boot). It defines the
;;;; special-form-level macros: setf, defun, defmacro, defvar/defparameter/
;;;; defconstant, when/unless/cond/case/and/or, the iteration macros, incf/
;;;; decf/push/pop, multiple-value-bind, destructuring-bind, and quasiquote.
;;;; These are the definitions that make the system self-hosting.

;;; We need a small amount of host glue to define defmacro (it must register
;;; the macro expander into the environment). That glue lives in boot-host
;;; (builtins). Everything else is pure Lisp.
;;;
;;; Note: this file is read by the host reader; its symbols are in the host's
;;; CL-USER package. There is no in-package form because in-package is a
;;; load-time macro that the CLEF-H evaluator does not run.

;;; --- quasiquote (needed by almost every macro body) ---
;;; SBCL reads `x as (sb-int:quasiquote x), and ,y / ,@z as sb-impl::comma
;;; structures (kind 0 = unquote, 2 = splicing). We detect the comma structs
;;; via host glue (qq-comma-p/kind/expr) and expand to cons/list/append.
;;;
;;; Bootstrap note: this section predates defun/cond/and, so it is written
;;; against %defun, %defmacro, and the special forms IF/LET/QUOTE only.

(%defparameter '*qq-marker* (car (read-from-string "`x")))

(%defun 'qq-expand
  (lambda (form depth)
    (if (qq-comma-p form)
        ;; a ,x or ,@x standing alone
        (if (= (qq-comma-kind form) 0)
            (if (= depth 0) (qq-comma-expr form) (list 'quote form))
            (list 'quote form))
        (if (consp form)
            (if (eq (car form) *qq-marker*)
                ;; nested quasiquote
                (list 'list (list 'quote *qq-marker*) (qq-expand (cadr form) (1+ depth)))
                (qq-expand-cons form depth))
            (list 'quote form)))))

(%defun 'qq-expand-cons
  (lambda (form depth)
    (let ((head (car form)) (tail (cdr form)))
      (if (qq-comma-p head)
          ;; (,@x . tail) or (,x . tail)
          (if (= (qq-comma-kind head) 2)
              (if (= depth 0)
                  (list 'append (qq-comma-expr head) (qq-expand tail depth))
                  (list 'cons (list 'quote head) (qq-expand tail depth)))
              (if (= (qq-comma-kind head) 0)
                  (if (= depth 0)
                      (list 'cons (qq-comma-expr head) (qq-expand tail depth))
                      (list 'cons (list 'quote head) (qq-expand tail depth)))
                  (list 'cons (list 'quote head) (qq-expand tail depth))))
          (if (null tail)
              (list 'list (qq-expand head depth))
              (if (consp tail)
                  (list 'cons (qq-expand head depth) (qq-expand-cons tail depth))
                  ;; dotted tail: (x . y)
                  (list 'cons (qq-expand head depth) (qq-expand tail depth))))))))

;;; register the quasiquote macro on the reader marker
(%defmacro *qq-marker* (lambda (form env) (declare (ignore env)) (qq-expand (cadr form) 0)))

;;; --- core defining macros ---
;;;
;;; defmacro and defun cannot be defined by themselves, so they are defined
;;; here against %defmacro directly (their expanders use quasiquote, which is
;;; already live). Everything after this point uses ordinary defmacro/defun.

(%defmacro 'defmacro
  (lambda (form env)
    (declare (ignore env))
    (%destructuring-bind '(name lambda-list &body body) (cdr form)
      (lambda (name lambda-list &rest body)
        `(%defmacro ',name (lambda (form env)
                             (declare (ignore env))
                             (%destructuring-bind ',lambda-list (cdr form)
                               (lambda ,lambda-list ,@body))))))))

(%defmacro 'defun
  (lambda (form env)
    (declare (ignore env))
    (%destructuring-bind '(name lambda-list &body body) (cdr form)
      (lambda (name lambda-list &rest body)
        (let ((real-body (if (stringp (car body)) (cdr body) body)))
          `(%defun ',name (lambda ,lambda-list ,@real-body)))))))

(defmacro defvar (name &optional (init nil initp) doc)
  (declare (ignore doc))
  `(%defvar ',name ,init ,initp))

(defmacro defparameter (name &optional (init nil) doc)
  (declare (ignore doc))
  `(%defparameter ',name ,init))

(defmacro defconstant (name init &optional doc)
  (declare (ignore doc))
  `(%defconstant ',name ,init))

;;; --- conditional and boolean macros ---

(defmacro when (test &body body)
  `(if ,test (progn ,@body) nil))

(defmacro unless (test &body body)
  `(if ,test nil (progn ,@body)))

(defmacro cond (&rest clauses)
  (if (null clauses)
      nil
      (let ((clause (car clauses)))
        (if (cdr clause)
            `(if ,(car clause)
                 (progn ,@(cdr clause))
                 (cond ,@(cdr clauses)))
            ;; single-form clause: return the test value if true
            (let ((g (gensym "COND")))
              `(let ((,g ,(car clause)))
                 (if ,g ,g (cond ,@(cdr clauses)))))))))

(defmacro and (&rest forms)
  (cond ((null forms) t)
        ((null (cdr forms)) (car forms))
        (t `(if ,(car forms) (and ,@(cdr forms)) nil))))

(defmacro or (&rest forms)
  (if (null forms)
      nil
      (let ((g (gensym "OR")))
        `(let ((,g ,(car forms)))
           (if ,g ,g (or ,@(cdr forms)))))))

(defmacro case (keyform &rest clauses)
  (let ((g (gensym "CASE")))
    `(let ((,g ,keyform))
       (cond
         ,@(mapcar (lambda (clause)
                     (let ((keys (car clause)) (body (cdr clause)))
                       (cond ((or (eq keys t) (eq keys 'otherwise))
                              `(t ,@body))
                             ((listp keys)
                              `((member ,g ',keys) ,@body))
                             (t `((eql ,g ',keys) ,@body)))))
                   clauses)))))

(defmacro ecase (keyform &rest clauses)
  `(case ,keyform
     ,@clauses
     (t (error "ECASE: no matching clause for ~s" ,keyform))))

(defmacro typecase (keyform &rest clauses)
  (let ((g (gensym "TYPECASE")))
    `(let ((g ,g)) (declare (ignore ,g))
       (cond
         ,@(mapcar (lambda (clause)
                     (let ((type (car clause)) (body (cdr clause)))
                       (if (or (eq type t) (eq type 'otherwise))
                           `(t ,@body)
                           `((typep ,keyform ',type) ,@body))))
                   clauses)))))

;;; --- multiple values ---

(defmacro multiple-value-bind (vars form &body body)
  `(%multiple-value-bind ,vars ,form (lambda ,vars ,@body)))

(defmacro multiple-value-setq (vars form)
  (let ((temps (mapcar (lambda (v) (gensym (symbol-name v))) vars)))
    `(let ,(mapcar #'list temps vars)
       (multiple-value-bind ,temps ,form
         ,@(mapcar (lambda (v temp) `(setq ,v ,temp)) vars temps))
       ,(car vars))))

(defmacro multiple-value-list (form)
  `(%multiple-value-bind (&rest vals) ,form (lambda (&rest vals) vals)))

(defmacro nth-value (n form)
  `(nth ,n (multiple-value-list ,form)))

;;; --- binding / place macros ---

(defmacro destructuring-bind (lambda-list form &body body)
  `(%destructuring-bind ',lambda-list ,form (lambda ,lambda-list ,@body)))

(defmacro psetq (&rest pairs)
  (let ((vars '()) (vals '()) (temps '()))
    (loop for (v val) on pairs by #'cddr
          do (push v vars) (push val vals) (push (gensym (symbol-name v)) temps))
    `(let ,(mapcar #'list (reverse temps) (reverse vals))
       ,@(mapcar (lambda (v temp) `(setq ,v ,temp)) (reverse vars) (reverse temps))
       nil)))

(defmacro prog1 (first &rest body)
  (let ((g (gensym "PROG1")))
    `(let ((,g ,first)) ,@body ,g)))

(defmacro prog2 (first second &rest body)
  `(progn ,first (prog1 ,second ,@body)))

(defmacro prog (&rest body)
  `(let () ,@body))

(defmacro prog* (&rest body)
  `(let* () ,@body))

;;; --- setf and friends ---
;;; Simple setf: (setf place value). Handles variable places and accessor
;;; places via %set-place (host dispatch for the common accessors).

(defun setf-pairs->args (pairs)
  "((p1 v1 p2 v2 ...)) -> ('p1 v1 'p2 v2 ...) for %setf."
  (if (null pairs)
      '()
      (cons (list 'quote (car pairs))
            (cons (cadr pairs)
                  (setf-pairs->args (cddr pairs))))))

(defmacro setf (&rest pairs)
  `(%setf ,@(setf-pairs->args pairs)))

(defmacro incf (place &optional (delta 1))
  `(setf ,place (+ ,place ,delta)))

(defmacro decf (place &optional (delta 1))
  `(setf ,place (- ,place ,delta)))

(defmacro push (item place)
  `(setf ,place (cons ,item ,place)))

(defmacro pop (place)
  (let ((g (gensym "POP")))
    `(let ((,g ,place))
       (setf ,place (cdr ,place))
       (car ,g))))

(defmacro pushnew (item place &rest keys)
  `(setf ,place (adjoin ,item ,place ,@keys)))

(defmacro rotatef (&rest places)
  (let ((temps (mapcar (lambda (p) (gensym "ROT")) places)))
    `(let ,(mapcar #'list temps places)
       ,@(loop for (a b) on (append temps (list (car temps)))
               while b
               for (pa pb) on (append places (list (car places)))
               collect `(setf ,pb ,a))
       nil)))

(defmacro shiftf (&rest args)
  (let* ((places (butlast args))
         (newval (car (last args)))
         (temps (mapcar (lambda (p) (gensym "SHF")) places)))
    `(let ,(mapcar #'list temps places)
       ,@(loop for (a b) on (append (cdr temps) (list newval))
               while b
               for (pa pb) on (append (cdr places) (list nil))
               collect `(setf ,pa ,a))
       ,(car temps))))

;;; --- iteration ---

(defmacro dolist ((var list &optional result) &body body)
  (let ((g (gensym "DOLIST")))
    `(let ((,g ,list))
       (block nil
         (tagbody
          loop
          (when ,g
            (let ((,var (car ,g)))
              ,@body)
            (setq ,g (cdr ,g))
            (go loop)))
         ,result))))

(defmacro dotimes ((var count &optional result) &body body)
  (let ((g (gensym "DOTIMES")))
    `(let ((,g ,count) (,var 0))
       (block nil
         (tagbody
          loop
          (when (< ,var ,g)
            ,@body
            (setq ,var (1+ ,var))
            (go loop)))
         ,result))))

(defmacro do (vars end &body body)
  `(block nil
     (%do ,vars ,end (lambda ,(mapcar #'car vars) (tagbody ,@body)))))

(defmacro do* (vars end &body body)
  `(block nil
     (%do* ,vars ,end (lambda ,(mapcar #'car vars) (tagbody ,@body)))))

(defmacro loop (&rest body)
  `(%loop ',body))

;;; --- control ---

(defmacro return (&optional value)
  `(return-from nil ,value))

(defmacro unwind-protect (protected &rest cleanup)
  `(%unwind-protect ,protected (lambda () ,@cleanup)))

;;; --- defstruct (minimal) ---

(defmacro defstruct (name &rest slots)
  `(%defstruct ',name ',slots))
