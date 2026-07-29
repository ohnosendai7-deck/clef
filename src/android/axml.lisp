;;;; axml.lisp — Android binary XML (AXML) writer in Common Lisp.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; Implements the Android binary-XML container format (the compiled form of
;;;; AndroidManifest.xml inside every APK): the RES_XML_TYPE file header, a
;;;; UTF-8 string-pool chunk, a resource-map chunk, and start/end namespace +
;;;; start/end element chunks. Clean-room: implemented from the publicly
;;;; documented on-disk format (the chunk layouts fixed by the Android
;;;; platform's binary-XML parser contract), never from aapt source.
;;;;
;;;; Scope: string/int/boolean typed attribute values, one android:
;;;; namespace, no styles, no CDATA — everything a manifest needs.

(in-package #:clef/android)

;;; --- chunk type constants (public binary-XML format) ---

(defconstant +res-string-pool-type+       #x0001)
(defconstant +res-xml-type+               #x0003)
(defconstant +res-xml-start-ns-type+      #x0100)
(defconstant +res-xml-end-ns-type+        #x0101)
(defconstant +res-xml-start-el-type+      #x0102)
(defconstant +res-xml-end-el-type+        #x0103)
(defconstant +res-xml-resource-map-type+  #x0180)

(defconstant +utf8-flag+ #x00000100 "String-pool flag: strings are UTF-8.")
(defconstant +no-index+  #xFFFFFFFF "Sentinel for an absent string-pool index.")

;;; Typed-value data types (Res_value).
(defconstant +type-string+      #x03)
(defconstant +type-int-dec+     #x10)
(defconstant +type-int-boolean+ #x12)

(defparameter +android-ns-uri+ "http://schemas.android.com/apk/res/android")
(defparameter +android-ns-prefix+ "android")

;;; Public android.R.attr resource IDs for the manifest attributes we emit
;;; (part of Android's stable public API surface), sorted by ID so the
;;; resource map is parallel to the first pool entries.
(defparameter +attr-resource-ids+
  '(("name"             . #x01010003)
    ("exported"         . #x01010010)
    ("minSdkVersion"    . #x0101020c)
    ("versionCode"      . #x0101021b)
    ("versionName"      . #x0101021c)
    ("targetSdkVersion" . #x01010270)))

(defun attr-resource-id (name)
  (cdr (assoc name +attr-resource-ids+ :test #'string=)))

(defun a-u16 (n) (bv-u16le n))
(defun a-u32 (n) (bv-u32le n))

;;; --- string pool ---

(defstruct (axml-pool (:constructor %make-axml-pool))
  (strings '())                            ; in reverse interning order
  (table (make-hash-table :test #'equal)))

(defun axml-pool-intern (pool string)
  "Add STRING to POOL (deduplicated) and return its index."
  (or (gethash string (axml-pool-table pool))
      (let ((index (length (axml-pool-strings pool))))
        (push string (axml-pool-strings pool))
        (setf (gethash string (axml-pool-table pool)) index)
        index)))

(defun utf8-encode (string)
  "Encode STRING as UTF-8 bytes (pure CL)."
  (let ((out '()))
    (loop for ch across string
          for code = (char-code ch)
          do (cond ((< code #x80) (push code out))
                   ((< code #x800)
                    (push (logior #xC0 (ash code -6)) out)
                    (push (logior #x80 (logand code #x3F)) out))
                   ((< code #x10000)
                    (push (logior #xE0 (ash code -12)) out)
                    (push (logior #x80 (logand (ash code -6) #x3F)) out)
                    (push (logior #x80 (logand code #x3F)) out))
                   (t
                    (push (logior #xF0 (ash code -18)) out)
                    (push (logior #x80 (logand (ash code -12) #x3F)) out)
                    (push (logior #x80 (logand (ash code -6) #x3F)) out)
                    (push (logior #x80 (logand code #x3F)) out))))
    (coerce (nreverse out) '(simple-array (unsigned-byte 8) (*)))))

(defun utf16-length (string)
  "Number of UTF-16 code units in STRING."
  (loop for ch across string sum (if (>= (char-code ch) #x10000) 2 1)))

(defun axml-length-field (n)
  "UTF-8 string-pool length field: one byte if N < #x80, else two bytes
with the high bit of the first set."
  (if (< n #x80)
      (make-array 1 :element-type '(unsigned-byte 8) :initial-element n)
      (make-array 2 :element-type '(unsigned-byte 8)
                  :initial-contents (list (logior #x80 (ash n -8))
                                          (logand n #xFF)))))

(defun axml-string-pool-chunk (pool)
  "Emit the RES_STRING_POOL_TYPE chunk for POOL (UTF-8, no styles)."
  (let* ((strings (nreverse (axml-pool-strings pool)))
         (count (length strings))
         (records '())
         (offsets '())
         (pos 0))
    (dolist (s strings)
      (let* ((bytes (utf8-encode s))
             (record (bv-concat (axml-length-field (utf16-length s))
                                (axml-length-field (length bytes))
                                bytes
                                (make-array 1 :element-type '(unsigned-byte 8)
                                              :initial-element 0))))
        (push pos offsets)
        (push record records)
        (incf pos (length record))))
    (let* ((pad (mod (- 4 (mod pos 4)) 4))
           (header-size 28)
           (strings-start (+ header-size (* 4 count)))
           (total (+ strings-start pos pad)))
      (apply #'bv-concat
             (a-u16 +res-string-pool-type+)
             (a-u16 header-size)
             (a-u32 total)
             (a-u32 count)
             (a-u32 0)                     ; styleCount
             (a-u32 +utf8-flag+)
             (a-u32 strings-start)
             (a-u32 0)                     ; stylesStart
             (append (mapcar #'a-u32 (nreverse offsets))
                     (nreverse records)
                     (list (make-array pad :element-type '(unsigned-byte 8)
                                           :initial-element 0)))))))

;;; --- XML event chunks ---

(defun axml-chunk (type header-size body)
  "Wrap BODY in a ResChunk_header of the given TYPE and HEADER-SIZE. BODY is
everything after the 8-octet ResChunk_header (for XML event chunks this
includes the lineNumber/comment fields, which are part of HEADER-SIZE)."
  (bv-concat (a-u16 type) (a-u16 header-size)
             (a-u32 (+ 8 (length body))) body))

(defun axml-namespace-chunk (type prefix-index uri-index)
  "Emit a start/end-namespace chunk (TYPE selects which)."
  (axml-chunk type 16
              (bv-concat (a-u32 0)              ; lineNumber
                         (a-u32 +no-index+)     ; comment
                         (a-u32 prefix-index)
                         (a-u32 uri-index))))

(defun axml-typed-value (pool kind value)
  "Return (values raw-value-index Res_value bytes) for an attribute value."
  (ecase kind
    (:string
     (let ((index (axml-pool-intern pool value)))
       (values index
               (bv-concat (a-u16 8)
                          (make-array 1 :element-type '(unsigned-byte 8)
                                        :initial-element 0) ; res0
                          (make-array 1 :element-type '(unsigned-byte 8)
                                        :initial-element +type-string+)
                          (a-u32 index)))))
    (:int
     (values +no-index+
             (bv-concat (a-u16 8)
                        (make-array 1 :element-type '(unsigned-byte 8)
                                      :initial-element 0)
                        (make-array 1 :element-type '(unsigned-byte 8)
                                      :initial-element +type-int-dec+)
                        (a-u32 value))))
    (:bool
     (values +no-index+
             (bv-concat (a-u16 8)
                        (make-array 1 :element-type '(unsigned-byte 8)
                                      :initial-element 0)
                        (make-array 1 :element-type '(unsigned-byte 8)
                                      :initial-element +type-int-boolean+)
                        (a-u32 (if value #xFFFFFFFF 0)))))))

(defun axml-sort-attributes (attrs)
  "Order ATTRS aapt-style: attributes without a resource ID first (source
order), then android: attributes sorted by resource ID."
  (stable-sort (copy-list attrs)
               (lambda (a b)
                 (< (or (attr-resource-id (first a)) most-positive-fixnum)
                    (or (attr-resource-id (first b)) most-positive-fixnum)))))

(defun axml-start-element (pool name attrs)
  "Emit a start-element chunk. ATTRS is a list of (name value kind ns)
where KIND is :string/:int/:bool and NS is :android or NIL."
  (let* ((name-index (axml-pool-intern pool name))
         (uri-index (axml-pool-intern pool +android-ns-uri+))
         (attr-ext-size 20)
         (attr-size 20)
         (sorted (axml-sort-attributes attrs))
         (attr-bytes '()))
    (dolist (attr sorted)
      (destructuring-bind (aname value kind ns) attr
        (multiple-value-bind (raw typed) (axml-typed-value pool kind value)
          (push (bv-concat (a-u32 (if (eq ns :android) uri-index +no-index+))
                           (a-u32 (axml-pool-intern pool aname))
                           (a-u32 raw)
                           typed)
                attr-bytes))))
    (axml-chunk +res-xml-start-el-type+ 16
                (apply #'bv-concat
                       (a-u32 0)                ; lineNumber
                       (a-u32 +no-index+)       ; comment
                       (a-u32 +no-index+)       ; element ns (none)
                       (a-u32 name-index)
                       (a-u16 attr-ext-size)    ; attributeStart
                       (a-u16 attr-size)
                       (a-u16 (length sorted))
                       (a-u16 0)                ; idIndex (none)
                       (a-u16 0)                ; classIndex
                       (a-u16 0)                ; styleIndex
                       (nreverse attr-bytes)))))

(defun axml-end-element (pool name)
  "Emit an end-element chunk for element NAME."
  (axml-chunk +res-xml-end-el-type+ 16
              (bv-concat (a-u32 0)
                         (a-u32 +no-index+)
                         (a-u32 +no-index+)   ; ns
                         (a-u32 (axml-pool-intern pool name)))))

;;; --- resource map ---

(defun axml-resource-map-chunk (names)
  "Emit the RES_XML_RESOURCE_MAP_TYPE chunk mapping the first (length NAMES)
string-pool indices to their public resource IDs."
  (axml-chunk +res-xml-resource-map-type+ 8
              (apply #'bv-concat
                     (mapcar (lambda (n) (a-u32 (attr-resource-id n))) names))))

;;; --- manifest tree ---

(defun manifest-tree (package version-name version-code min-sdk target-sdk
                      main-activity)
  "Build the manifest element tree: (name attrs child1 child2 ...)."
  `("manifest" (("package" ,package :string nil)
                ("versionCode" ,version-code :int :android)
                ("versionName" ,version-name :string :android))
    ("uses-sdk" (("minSdkVersion" ,min-sdk :int :android)
                 ("targetSdkVersion" ,target-sdk :int :android)))
    ("application" ()
      ("activity" (("name" ,main-activity :string :android)
                   ("exported" t :bool :android))
        ("intent-filter" ()
          ("action" (("name" "android.intent.action.MAIN" :string :android)))
          ("category" (("name" "android.intent.category.LAUNCHER" :string :android))))))))

(defun axml-intern-tree (pool tree)
  "First pass: intern every string the TREE will reference (element names,
attribute names, string values, namespace prefix/URI)."
  (axml-pool-intern pool +android-ns-prefix+)
  (axml-pool-intern pool +android-ns-uri+)
  (labels ((walk (node)
             (destructuring-bind (name attrs &rest children) node
               (axml-pool-intern pool name)
               (dolist (attr attrs)
                 (destructuring-bind (aname value kind ns) attr
                   (declare (ignore ns))
                   (axml-pool-intern pool aname)
                   (when (eq kind :string)
                     (axml-pool-intern pool value))))
               (mapc #'walk children))))
    (walk tree)))

(defun axml-emit-tree (pool tree)
  "Second pass: emit start-element, children, end-element chunks."
  (destructuring-bind (name attrs &rest children) tree
    (apply #'bv-concat
           (axml-start-element pool name attrs)
           (append (mapcar (lambda (c) (axml-emit-tree pool c)) children)
                   (list (axml-end-element pool name))))))

(defun write-android-manifest (&key package version-name version-code
                                 min-sdk target-sdk
                                 (main-activity ".MainActivity"))
  "Write a binary (AXML) AndroidManifest.xml and return it as a byte vector.

PACKAGE is the application ID string, VERSION-NAME a string, VERSION-CODE,
MIN-SDK and TARGET-SDK integers, MAIN-ACTIVITY the activity class name
string (default \".MainActivity\")."
  (unless (and (stringp package) (plusp (length package)))
    (error "PACKAGE must be a non-empty string"))
  (unless (stringp version-name)
    (error "VERSION-NAME must be a string"))
  (dolist (n (list version-code min-sdk target-sdk))
    (unless (and (integerp n) (not (minusp n)))
      (error "VERSION-CODE, MIN-SDK and TARGET-SDK must be non-negative integers")))
  (unless (stringp main-activity)
    (error "MAIN-ACTIVITY must be a string"))
  (let* ((pool (%make-axml-pool))
         (tree (manifest-tree package version-name version-code
                              min-sdk target-sdk main-activity))
         (attr-names (mapcar #'car +attr-resource-ids+)))
    ;; Intern the mappable attribute names first, in resource-ID order, so
    ;; the resource map is parallel to pool indices 0..k-1.
    (dolist (name attr-names) (axml-pool-intern pool name))
    ;; First pass collects every other string into the pool; the emission
    ;; pass re-interns (idempotent) so indices are stable.
    (axml-intern-tree pool tree)
    (let ((body (bv-concat
                 (axml-string-pool-chunk pool)
                 (axml-resource-map-chunk attr-names)
                 (axml-namespace-chunk +res-xml-start-ns-type+
                                       (axml-pool-intern pool +android-ns-prefix+)
                                       (axml-pool-intern pool +android-ns-uri+))
                 (axml-emit-tree pool tree)
                 (axml-namespace-chunk +res-xml-end-ns-type+
                                       (axml-pool-intern pool +android-ns-prefix+)
                                       (axml-pool-intern pool +android-ns-uri+)))))
      (bv-concat (a-u16 +res-xml-type+)
                 (a-u16 8)                 ; headerSize
                 (a-u32 (+ 8 (length body)))
                 body))))
