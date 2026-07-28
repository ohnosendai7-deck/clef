;;;; package.lisp — CLEF package definitions.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;; To the extent possible under law, the author(s) have dedicated all
;;;; copyright and related and neighboring rights to this software to the
;;;; public domain worldwide. https://creativecommons.org/publicdomain/zero/1.0/

(defpackage #:clef/util
  (:use #:cl)
  (:export
   ;; byte vectors / output
   #:make-bytevec #:bytevec #:bytevec-length #:bv-u8 #:bv-u16le #:bv-u32le
   #:bv-u64le #:bv-s32le #:bv-concat #:bv-set
   ;; bit utilities
   #:fit-s8 #:fit-s32 #:logand-mask))

(defpackage #:clef/lap
  (:use #:cl)
  (:import-from #:clef/util
                #:make-bytevec #:bytevec #:bytevec-length #:bv-u8 #:bv-u16le
                #:bv-u32le #:bv-u64le #:bv-s32le #:bv-concat #:bv-set
                #:fit-s8 #:fit-s32)
  (:export
   ;; assembler driver
   #:assemble #:assemble-into
   ;; operands & instruction constructors are used as s-expr forms:
   ;;   (mnemonic dst src) with registers as keywords and (disp base index scale)
   ;; We export the register keyword set and memory operand constructor.
   #:mem #:rip-mem
   ;; condition codes
   #:*cc-o* #:*cc-no* #:*cc-b* #:*cc-ae* #:*cc-e* #:*cc-ne* #:*cc-be* #:*cc-a*
   #:*cc-s* #:*cc-ns* #:*cc-p* #:*cc-np* #:*cc-l* #:*cc-ge* #:*cc-le* #:*cc-g*))

(defpackage #:clef/elf
  (:use #:cl)
  (:import-from #:clef/util
                #:make-bytevec #:bytevec #:bytevec-length #:bv-u8 #:bv-u16le
                #:bv-u32le #:bv-u64le #:bv-concat)
  (:export #:write-executable #:segment #:make-segment
           #:segment-data #:segment-vaddr #:segment-flags))

(defpackage #:clef/gc
  (:use #:cl)
  (:export))

(defpackage #:clef/solver
  (:use #:cl)
  (:export))
