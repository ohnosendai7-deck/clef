;;;; apk.lisp — APK packager in Common Lisp.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; An APK is a ZIP archive containing a binary AndroidManifest.xml, native
;;;; libraries under lib/<abi>/, assets, and (after signing) META-INF
;;;; signature files. This module assembles the unsigned APK from a manifest
;;;; description plus caller-supplied file contents.

(in-package #:clef/android)

(defstruct (apk-file (:constructor make-apk-file (name data)))
  "A named blob of bytes to include in an APK."
  name                                ; string path inside the archive
  data)                               ; (unsigned-byte 8) vector

(defun apk-item-name (item)
  "Name of an APK input item: an APK-FILE or a (name . data) cons."
  (etypecase item
    (apk-file (apk-file-name item))
    (cons (car item))))

(defun apk-item-data (item)
  "Data of an APK input item, coerced to a byte vector."
  (let ((data (etypecase item
                (apk-file (apk-file-data item))
                (cons (cdr item)))))
    (etypecase data
      ((simple-array (unsigned-byte 8) (*)) data)
      (vector (map '(simple-array (unsigned-byte 8) (*)) #'identity data))
      (list (map '(simple-array (unsigned-byte 8) (*)) #'identity data)))))

(defun apk-item-entry (item)
  "Turn an APK input item into a ZIP entry."
  (make-entry (apk-item-name item) (apk-item-data item)))

(defun build-apk (&key package version-name version-code min-sdk target-sdk
                    (main-activity ".MainActivity") so-files assets)
  "Build an unsigned APK and return it as a byte vector.

SO-FILES is a list of (\"lib/<abi>/name.so\" . bytes) items and ASSETS a
list of (\"path\" . bytes) items; each item may also be an APK-FILE. The
manifest arguments are passed through to WRITE-ANDROID-MANIFEST. All
entries are stored uncompressed (the ZIP writer falls back to stored when
no compressor is available, which is also what mmap-able .so delivery
wants)."
  (let* ((manifest (write-android-manifest
                    :package package :version-name version-name
                    :version-code version-code :min-sdk min-sdk
                    :target-sdk target-sdk :main-activity main-activity))
         (entries (list* (make-entry "AndroidManifest.xml" manifest)
                         (append (mapcar #'apk-item-entry so-files)
                                 (mapcar #'apk-item-entry assets)))))
    (write-zip entries)))
