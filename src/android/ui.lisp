;;;; ui.lisp — native-UI JNI call plans (spec layer).
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; A thin, pure-data DSL describing Android native UIs. Constructors build
;;;; widget trees; JNI-PLAN lowers a tree to an ordered list of JNI call
;;;; descriptors against the outbound-JNI bridge contract of
;;;; docs/ANDROID-ABI.md section 5. No JNI happens here: the descriptors are
;;;; data, instantiated by a future executor once the aarch64 backend exists
;;;; (docs/REPL-APK.md section 6), so the whole layer is host-testable.
;;;;
;;;; Descriptor vocabulary (all plists):
;;;;
;;;;   (:op :new-object  :id ID :class JNI-CLASS
;;;;        :signature "(Landroid/content/Context;)V" :args (:context))
;;;;   (:op :call-method :id ID :class JNI-CLASS :name JNI-METHOD
;;;;        :signature JNI-SIGNATURE :args (ARG ...))
;;;;   (:op :add-view    :parent PARENT-ID :child CHILD-ID)
;;;;
;;;; JNI-CLASS is the slash-separated internal form ("android/widget/Button"),
;;;; JNI-SIGNATURE the real JNI method signature. Placeholders an executor
;;;; must substitute:
;;;;
;;;;   :context        — the Activity Context, supplied by the shim
;;;;   (:callback SYM) — a View.OnClickListener proxy that invokes the CLEF
;;;;                     function named by the symbol SYM (held as data,
;;;;                     never funcalled by this layer)

(in-package #:clef/android)

;;; --- widget tree ---

(defstruct widget
  "A native-UI widget specification (pure data).
KIND is :linear-layout, :text-view, :text-input or :button; ID is a keyword
naming the widget (referenced by :add-view descriptors); PROPERTIES is a
plist (keys :text, :hint, :enabled, :orientation, :on-tap); CHILDREN is a
list of child widgets (only layout kinds have any)."
  kind id properties children)

(defun widget-property (widget key &optional default)
  "The KEY entry of WIDGET's property plist, or DEFAULT."
  (getf (widget-properties widget) key default))

(defun widget-property-p (widget key)
  "True when WIDGET's property plist explicitly mentions KEY, even with a
nil value (so :enabled nil is distinguishable from \"not specified\")."
  (loop for (k v) on (widget-properties widget) by #'cddr
        thereis (eq k key)))

(defun text-view (id &key text)
  "A read-only label widget (android/widget/TextView)."
  (make-widget :kind :text-view :id id
               :properties (when text (list :text text))
               :children '()))

(defun text-input (id &key hint text)
  "An editable text field (android/widget/EditText)."
  (make-widget :kind :text-input :id id
               :properties (append (when hint (list :hint hint))
                                   (when text (list :text text)))
               :children '()))

(defun button (id &key text on-tap)
  "A push button (android/widget/Button). ON-TAP is a symbol naming the
CLEF function to invoke when the button is tapped; it is stored as data,
never funcalled by this layer."
  (make-widget :kind :button :id id
               :properties (append (when text (list :text text))
                                   (when on-tap (list :on-tap on-tap)))
               :children '()))

(defun layout (&rest widgets)
  "A root vertical LinearLayout (android/widget/LinearLayout) containing
WIDGETS, with the fixed id :root."
  (make-widget :kind :linear-layout :id :root
               :properties '(:orientation :vertical)
               :children widgets))

;;; --- lowering to JNI call descriptors ---

(defun widget-jni-class (kind)
  "The android.widget class name (JNI internal form) implementing KIND."
  (ecase kind
    (:linear-layout "android/widget/LinearLayout")
    (:text-view     "android/widget/TextView")
    (:text-input    "android/widget/EditText")
    (:button        "android/widget/Button")))

(defun jni-plan (widget)
  "Lower the WIDGET tree to an ordered list of JNI call descriptors (pure
data — see the file header for the vocabulary). Ordering is parent-first:
a widget's :new-object always precedes every descriptor referencing its
:id, and each :add-view follows the plans of both endpoints, so a future
executor can instantiate the plan sequentially."
  (let ((plan '()))
    (labels ((emit (op) (push op plan))
             (walk (w)
               (let ((id (widget-id w))
                     (class (widget-jni-class (widget-kind w))))
                 (emit (list :op :new-object :id id :class class
                             :signature "(Landroid/content/Context;)V"
                             :args (list :context)))
                 (when (widget-property-p w :orientation)
                   (emit (list :op :call-method :id id :class class
                               :name "setOrientation" :signature "(I)V"
                               :args (list (ecase (widget-property w :orientation)
                                             (:horizontal 0)
                                             (:vertical 1))))))
                 (when (widget-property-p w :text)
                   (emit (list :op :call-method :id id :class class
                               :name "setText"
                               :signature "(Ljava/lang/CharSequence;)V"
                               :args (list (widget-property w :text)))))
                 (when (widget-property-p w :hint)
                   (emit (list :op :call-method :id id :class class
                               :name "setHint"
                               :signature "(Ljava/lang/CharSequence;)V"
                               :args (list (widget-property w :hint)))))
                 (when (widget-property-p w :enabled)
                   (emit (list :op :call-method :id id :class class
                               :name "setEnabled" :signature "(Z)V"
                               :args (list (if (widget-property w :enabled) 1 0)))))
                 (when (widget-property-p w :on-tap)
                   (emit (list :op :call-method :id id :class class
                               :name "setOnClickListener"
                               :signature "(Landroid/view/View$OnClickListener;)V"
                               :args (list (list :callback
                                                 (widget-property w :on-tap))))))
                 (dolist (child (widget-children w))
                   (walk child)
                   (emit (list :op :add-view :parent id
                               :child (widget-id child)))))))
      (walk widget))
    (nreverse plan)))
