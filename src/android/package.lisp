;;;; package.lisp — CLEF Android tooling package.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.

(defpackage #:clef/android
  (:use #:cl)
  (:import-from #:clef/util #:bv-concat #:bv-u16le #:bv-u32le #:bv-u64le #:make-bytevec)
  (:export
   ;; zip
   #:make-entry #:write-zip #:read-zip #:crc32
   #:zentry-name #:zentry-data #:zentry-method
   ;; axml
   #:write-android-manifest
   ;; apk
   #:build-apk #:apk-file #:make-apk-file #:apk-file-name #:apk-file-data
   ;; hashing / encoding
   #:sha256 #:base64-encode
   ;; RSA (PKCS#1 v1.5 over SHA-256)
   #:make-rsa-private-key #:rsa-private-key-n #:rsa-private-key-e
   #:rsa-private-key-d
   #:make-rsa-public-key #:rsa-public-key-n #:rsa-public-key-e
   #:rsa-sign-sha256 #:rsa-verify-sha256
   ;; DER (minimal ASN.1)
   #:der-parse #:der-node-tag #:der-node-children #:der-node-content
   #:der-node-raw #:der-integer-value
   ;; signing
   #:sign-apk
   ;; ui (JNI call-plan DSL)
   #:widget #:widget-p #:make-widget
   #:widget-kind #:widget-id #:widget-properties #:widget-children
   #:widget-property #:widget-property-p
   #:text-view #:text-input #:button #:layout #:jni-plan))
