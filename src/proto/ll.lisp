;;;; ll.lisp — lambda-list parsing and binding.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; Handles ordinary and destructuring lambda lists: required, &optional,
;;;; &rest/&body, &key, &allow-other-keys, &aux, and supplied-p parameters.
;;;; Nested patterns (destructuring) are represented as nested LL structures.

(in-package #:clef/proto/ll)

(defstruct (ll (:conc-name ll-) (:predicate ll-p))
  (required '())
  (optional '())          ; list of (var init supplied-p); var may be a nested ll
  (rest nil)              ; var (symbol or nested ll) or NIL
  (keys '())              ; list of (keyword var init supplied-p)
  (allow-other-keys nil)
  (aux '()))              ; list of (var init)

(defparameter +lambda-list-keywords+
  '(&optional &rest &key &allow-other-keys &aux &body &whole &environment))

(defun lambda-list-keyword-p (x) (member x +lambda-list-keywords+))

(defun parse-lambda-list (lambda-list &key (destructuring nil))
  "Parse LAMBDA-LIST into an LL structure. With DESTRUCTURING, required and
&optional/&rest parameters may themselves be lambda lists."
  (let ((ll (make-ll))
        (mode :required))
    (flet ((parse-var (x)
             (if (and destructuring (consp x))
                 (parse-lambda-list x :destructuring t)
                 x)))
      (dolist (item lambda-list)
        (cond
          ((eq item '&optional) (setf mode :optional))
          ((or (eq item '&rest) (eq item '&body)) (setf mode :rest))
          ((eq item '&key) (setf mode :key))
          ((eq item '&allow-other-keys) (setf (ll-allow-other-keys ll) t))
          ((eq item '&aux) (setf mode :aux))
          ((eq item '&whole) (setf mode :whole))
          ((eq item '&environment) (setf mode :skip-one))
          ((eq mode :skip-one) (setf mode :required))
          (t
           (case mode
             (:whole (setf (ll-rest ll) nil) ; whole captured separately; ignore here
                    (setf mode :required))
             (:required (push (parse-var item) (ll-required ll)))
             (:optional (push (parse-optional item destructuring) (ll-optional ll)))
             (:rest (setf (ll-rest ll) (parse-var item)) (setf mode :after-rest))
             (:after-rest
              ;; Only &key/&aux/&allow-other-keys may follow &rest; reprocess.
              (cond ((eq item '&key) (setf mode :key))
                    ((eq item '&aux) (setf mode :aux))
                    (t (error "Bad lambda list after &rest: ~s" item))))
             (:key (push (parse-key item destructuring) (ll-keys ll)))
             (:aux (push (parse-aux item) (ll-aux ll))))))))
    (setf (ll-required ll) (nreverse (ll-required ll))
          (ll-optional ll) (nreverse (ll-optional ll))
          (ll-keys ll) (nreverse (ll-keys ll))
          (ll-aux ll) (nreverse (ll-aux ll)))
    ll))

(defun parse-optional (item destructuring)
  "Return (var init supplied-p)."
  (cond ((symbolp item) (list item nil nil))
        ((consp item)
         (let ((var (if (and destructuring (consp (first item)))
                        (parse-lambda-list (first item) :destructuring t)
                        (first item))))
           (list var
                 (if (cdr item) (second item) nil)
                 (if (cddr item) (third item) nil))))
        (t (error "Bad &optional parameter: ~s" item))))

(defun parse-key (item destructuring)
  "Return (keyword var init supplied-p)."
  (cond ((symbolp item)
         (list (intern (symbol-name item) :keyword) item nil nil))
        ((consp item)
         (let* ((spec (first item))
                (init (if (cdr item) (second item) nil))
                (sp (if (cddr item) (third item) nil)))
           (cond
             ;; ((keyword var) ...) explicit keyword
             ((and (consp spec) (symbolp (first spec)))
              (let ((kw (first spec)) (v (second spec)))
                (list kw
                      (if (and destructuring (consp v))
                          (parse-lambda-list v :destructuring t)
                          v)
                      init sp)))
             ;; (var ...) — keyword derived from var name
             ((symbolp spec)
              (list (intern (symbol-name spec) :keyword) spec init sp))
             ;; destructuring pattern as the variable
             ((and destructuring (consp spec))
              (let ((v (parse-lambda-list spec :destructuring t)))
                (list (gensym "KEY") v init sp)))
             (t (error "Bad &key parameter: ~s" item)))))
        (t (error "Bad &key parameter: ~s" item))))

(defun parse-aux (item)
  (cond ((symbolp item) (list item nil))
        ((consp item) (list (first item) (if (cdr item) (second item) nil)))
        (t (error "Bad &aux parameter: ~s" item))))

;;; --- binding ---
;;; BINDER is (lambda (var value) ...) for each symbol binding.
;;; EVAL-INIT is (lambda (form) ...) evaluating init forms in the current env.

(defun bind-lambda-list (ll args binder eval-init)
  "Bind LL against ARGS, calling BINDER for each (var value)."
  (let ((rest args))
    (dolist (req (ll-required ll))
      (unless rest (error "Too few arguments."))
      (bind-one req (pop rest) binder eval-init))
    (dolist (opt (ll-optional ll))
      (destructuring-bind (var init sp) opt
        (if rest
            (progn (bind-one var (pop rest) binder eval-init)
                   (when sp (funcall binder sp t)))
            (progn (bind-one var (funcall eval-init init) binder eval-init)
                   (when sp (funcall binder sp nil))))))
    (when (ll-rest ll)
      (bind-one (ll-rest ll) rest binder eval-init))
    (when (ll-keys ll)
      (bind-keys ll rest binder eval-init))
    (when (and (not (ll-rest ll)) (not (ll-keys ll)) rest)
      (error "Too many arguments: ~s for lambda-list ~s" rest ll))
    (dolist (aux (ll-aux ll))
      (destructuring-bind (var init) aux
        (bind-one var (funcall eval-init init) binder eval-init))))
  (values))

(defun bind-keys (ll args binder eval-init)
  (let ((aok (or (ll-allow-other-keys ll) (getf args :allow-other-keys))))
    (unless aok
      (loop for (k v) on args by #'cddr
            do (unless (or (eq k :allow-other-keys)
                           (member k (ll-keys ll) :key #'first))
                 (error "Unknown keyword argument: ~s" k))))
    (dolist (key (ll-keys ll))
      (destructuring-bind (kw var init sp) key
        (if (key-present-p kw args)
            (progn (bind-one var (getf args kw) binder eval-init)
                   (when sp (funcall binder sp t)))
            (progn (bind-one var (funcall eval-init init) binder eval-init)
                   (when sp (funcall binder sp nil))))))))

(defun key-present-p (kw args)
  (loop for (k v) on args by #'cddr thereis (eq k kw)))

(defun bind-one (var value binder eval-init)
  (if (ll-p var)
      (bind-lambda-list var (if (listp value) value (list value)) binder eval-init)
      (funcall binder var value)))
