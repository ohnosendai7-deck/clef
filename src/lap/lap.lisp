;;;; lap.lisp — the lap DSL: an x86-64 assembler in Common Lisp.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; This is the foundation of T0 copy-and-patch stencils and the cold core.
;;;; Programs are s-expressions:
;;;;
;;;;   (assemble
;;;;     (mov :rax 1)                  ; reg, imm
;;;;     (mov :rax :rbx)               ; reg, reg
;;;;     (mov (mem :rsp 8) :rax)       ; store
;;;;     (mov :rax (mem :rip 'lbl))    ; RIP-relative load (label-resolved)
;;;;     (:label here)
;;;;     (jmp :here))
;;;;
;;;; Only a subset of x86-64 is implemented — enough for the cold core and the
;;;; first T0 stencils. Extend as the compiler needs more.

(in-package #:clef/lap)

;;;; ---------------------------------------------------------------------
;;;; Registers

(defparameter *gprs*
  '(:rax :rcx :rdx :rbx :rsp :rbp :rsi :rdi
    :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15)
  "64-bit general-purpose registers, in encoding order (low 3 bits = index).")

(defun gpr-p (x) (and (keywordp x) (member x *gprs*) t))

(defun reg-index (reg)
  "Encoding index (0-15) of REG."
  (or (position reg *gprs*) (error "Unknown register: ~s" reg)))

(defun reg-low3 (reg) (logand (reg-index reg) 7))
(defun reg-ext-p (reg) (>= (reg-index reg) 8)) ; needs a REX extension bit

;;;; ---------------------------------------------------------------------
;;;; Memory operands

(defstruct (memop (:constructor %make-memop))
  base          ; register keyword, :rip, or NIL (absolute disp32)
  index         ; register keyword or NIL
  (scale 1)     ; 1,2,4,8
  (disp 0)      ; integer displacement (0 when disp-label is set)
  disp-label)   ; a label symbol for RIP-relative references

(defun mem (base &optional (disp 0) index (scale 1))
  "Memory operand [BASE + INDEX*SCALE + DISP].
When BASE is :rip, DISP may be a label symbol, producing a RIP-relative
reference that is resolved at assembly time."
  (if (and (eq base :rip) (symbolp disp) (not (null disp)))
      (%make-memop :base base :index index :scale scale :disp 0 :disp-label disp)
      (%make-memop :base base :index index :scale scale
                   :disp (if (symbolp disp) 0 disp))))

(defun rip-mem (label)
  "RIP-relative reference to LABEL."
  (mem :rip label))

(defun memop-rex-b (m)
  "REX.B contribution for a memop's base register."
  (if (gpr-p (memop-base m)) (reg-ext-p->bit (memop-base m)) 0))

(defun memop-rex-x (m)
  "REX.X contribution for a memop's index register."
  (if (gpr-p (memop-index m)) (reg-ext-p->bit (memop-index m)) 0))

(defun reg-ext-p->bit (reg) (if (reg-ext-p reg) 1 0))

;;;; ---------------------------------------------------------------------
;;;; REX prefix

(defun rex (&key (w 0) (r 0) (x 0) (b 0))
  "REX prefix byte. Each flag contributes its low bit."
  (logior #x40
          (ash (logand w 1) 3)
          (ash (logand r 1) 2)
          (ash (logand x 1) 1)
          (logand b 1)))

;;;; ---------------------------------------------------------------------
;;;; ModRM / SIB / displacement

(defun sib-scale (scale) (ecase scale (1 0) (2 1) (4 2) (8 3)))

(defun emit-modrm-sib (emit reg-field rm)
  "Emit ModRM, optional SIB, and displacement for an operand.
REG-FIELD is a register keyword or a raw 0-7 extension value.
RM is a register keyword or a memop.
Returns a :rip32 fixup if RM is a RIP-relative label reference, else NIL.
The fixup's offset is :patch-here (the last 4 bytes emitted); the driver
converts it to an absolute offset."
  (let ((rf (if (integerp reg-field) reg-field (reg-low3 reg-field))))
    (cond
      ((gpr-p rm)
       (funcall emit (bv-u8 (logior #b11000000 (ash rf 3) (reg-low3 rm))))
       nil)
      ((memop-p rm)
       (let ((base (memop-base rm))
             (index (memop-index rm))
             (scale (memop-scale rm))
             (disp (memop-disp rm))
             (label (memop-disp-label rm)))
         (cond
           ;; RIP-relative: mod=00 rm=101, disp32 (patched for labels)
           ((eq base :rip)
            (funcall emit (bv-u8 (logior (ash rf 3) #b101)))
            (funcall emit (bv-s32le disp))
            (when label
              (make-fixup :offset :patch-here :kind :rip32 :label label)))
           ;; Absolute disp32 (no base): mod=00 rm=100 + SIB base=101
           ((null base)
            (funcall emit (bv-u8 (logior (ash rf 3) #b100)))
            (funcall emit (bv-u8 (logior (ash (sib-scale scale) 6)
                                         (ash (if index (reg-low3 index) #b100) 3)
                                         #b101)))
            (funcall emit (bv-s32le disp))
            nil)
           (t
            (let* ((base-low3 (reg-low3 base))
                   (needs-sib (or index (= base-low3 #b100))) ; rsp/r12 need SIB
                   (rm-field (if needs-sib #b100 base-low3))
                   (mod (cond ((and (= disp 0) (/= base-low3 #b101)) #b00)
                              ((fit-s8 disp) #b01)
                              (t #b10))))
              (funcall emit (bv-u8 (logior (ash mod 6) (ash rf 3) rm-field)))
              (when needs-sib
                (funcall emit (bv-u8 (logior (ash (sib-scale scale) 6)
                                             (ash (if index (reg-low3 index) #b100) 3)
                                             base-low3))))
              (cond ((= mod #b01) (funcall emit (bv-u8 (logand disp #xff))))
                    ((or (= mod #b10) (and (= mod #b00) (= base-low3 #b101)))
                     (funcall emit (bv-s32le disp))))
              nil)))))
      (t (error "Invalid r/m operand: ~s" rm)))))

;;; Helper for encoders: call EMIT-MODRM-SIB and collect any returned fixup
;;; onto the FIXUPS list (the encoder's accumulator).
(defmacro with-modrm ((emit reg-field rm) fixups)
  `(let ((f (emit-modrm-sib ,emit ,reg-field ,rm)))
     (when f (push f ,fixups))))

;;;; ---------------------------------------------------------------------
;;;; Fixups

(defstruct fixup
  offset        ; byte offset in output of the 32-bit field to patch
  kind          ; :rel32 (branch) or :rip32 (RIP-relative data)
  label         ; target label symbol
  insn-end)     ; offset of the end of the instruction containing the field

;;;; ---------------------------------------------------------------------
;;;; Instruction set

(defparameter *mnemonics* (make-hash-table :test 'eq)
  "Maps mnemonic keyword -> (lambda (emit args) -> fixups).")

(defmacro definsn (name (emit args) &body body)
  "Define an instruction encoder. The body emits bytes via EMIT and returns
a list of fixups (usually via the FIXUPS local introduced here)."
  `(setf (gethash ,name *mnemonics*)
         (lambda (,emit ,args)
           (let ((fixups '()))
             ,@body
             fixups))))

(defun emit-byte (emit b) (funcall emit (bv-u8 b)))
(defun emit-bytes (emit &rest bytes)
  (dolist (b bytes) (emit-byte emit b)))

(defun memop-base-ext (m) (if (gpr-p (memop-base m)) (reg-ext-p->bit (memop-base m)) 0))
(defun memop-index-ext (m) (if (gpr-p (memop-index m)) (reg-ext-p->bit (memop-index m)) 0))

;;; --- data movement ---

(definsn :mov (emit args)
  (destructuring-bind (dst src) args
    (cond
      ;; mov reg, imm
      ((and (gpr-p dst) (integerp src))
       (if (fit-s32 src)
           (progn (emit-byte emit (rex :w 1 :b (reg-ext-p->bit dst)))
                  (emit-byte emit #xc7)
                  (with-modrm (emit 0 dst) fixups)
                  (funcall emit (bv-s32le src)))
           (progn (emit-byte emit (rex :w 1 :b (reg-ext-p->bit dst)))
                  (emit-byte emit (+ #xb8 (reg-low3 dst)))
                  (funcall emit (bv-u64le src)))))
      ;; mov reg, reg  and  mov reg, mem (load)
      ((gpr-p dst)
       (emit-byte emit (rex :w 1 :r (reg-ext-p->bit dst)
                            :b (if (gpr-p src)
                                   (reg-ext-p->bit src)
                                   (logior (memop-base-ext src) 0))
                            :x (if (memop-p src) (memop-index-ext src) 0)))
       (emit-byte emit #x8b)
       (with-modrm (emit dst src) fixups))
      ;; mov mem, reg (store)
      ((and (memop-p dst) (gpr-p src))
       (emit-byte emit (rex :w 1 :r (reg-ext-p->bit src)
                            :b (memop-base-ext dst) :x (memop-index-ext dst)))
       (emit-byte emit #x89)
       (with-modrm (emit src dst) fixups))
      ;; mov mem, imm32
      ((and (memop-p dst) (integerp src))
       (emit-byte emit (rex :w 1 :b (memop-base-ext dst) :x (memop-index-ext dst)))
       (emit-byte emit #xc7)
       (with-modrm (emit 0 dst) fixups)
       (funcall emit (bv-s32le src)))
      (t (error "Unsupported MOV: ~s ~s" dst src)))))

(definsn :lea (emit args)
  (destructuring-bind (dst src) args
    (assert (and (gpr-p dst) (memop-p src)))
    (emit-byte emit (rex :w 1 :r (reg-ext-p->bit dst)
                         :b (memop-base-ext src) :x (memop-index-ext src)))
    (emit-byte emit #x8d)
    (with-modrm (emit dst src) fixups)))

;;; --- arithmetic: add/or/and/sub/xor/cmp ---

(defmacro def-arith (name opcode-reg opcode-imm-ext)
  `(definsn ,name (emit args)
     (destructuring-bind (dst src) args
       (cond
         ((and (gpr-p dst) (integerp src))
          (emit-byte emit (rex :w 1 :b (reg-ext-p->bit dst)))
          (if (fit-s8 src)
              (progn (emit-byte emit #x83)
                     (with-modrm (emit ,opcode-imm-ext dst) fixups)
                     (emit-byte emit (logand src #xff)))
              (progn (emit-byte emit #x81)
                     (with-modrm (emit ,opcode-imm-ext dst) fixups)
                     (funcall emit (bv-s32le src)))))
         ((and (gpr-p dst) (gpr-p src))
          (emit-byte emit (rex :w 1 :r (reg-ext-p->bit src) :b (reg-ext-p->bit dst)))
          (emit-byte emit ,opcode-reg)
          (with-modrm (emit src dst) fixups))
         ((and (gpr-p dst) (memop-p src))
          (emit-byte emit (rex :w 1 :r (reg-ext-p->bit dst)
                               :b (memop-base-ext src) :x (memop-index-ext src)))
          (emit-byte emit (+ ,opcode-reg 2))
          (with-modrm (emit dst src) fixups))
         (t (error "Unsupported ~s: ~s ~s" ,name dst src))))))

(def-arith :add #x01 0)
(def-arith :or  #x09 1)
(def-arith :and #x21 4)
(def-arith :sub #x29 5)
(def-arith :xor #x31 6)
(def-arith :cmp #x39 7)

;;; --- inc/dec/neg/not ---

(defmacro def-grp1 (name ext)
  `(definsn ,name (emit args)
     (let ((r (first args)))
       (emit-byte emit (rex :w 1 :b (reg-ext-p->bit r)))
       (emit-byte emit ,(if (member ext '(0 1)) #xff #xf7))
       (with-modrm (emit ,ext r) fixups))))

(def-grp1 :inc 0)
(def-grp1 :dec 1)
(def-grp1 :not 2)
(def-grp1 :neg 3)

(definsn :imul (emit args)
  (destructuring-bind (dst src) args
    (emit-byte emit (rex :w 1 :r (reg-ext-p->bit dst)
                         :b (if (gpr-p src) (reg-ext-p->bit src) 0)))
    (emit-bytes emit #x0f #xaf)
    (with-modrm (emit dst src) fixups)))

;;; --- shifts ---

(defmacro def-shift (name ext)
  `(definsn ,name (emit args)
     (destructuring-bind (dst amt) args
       (emit-byte emit (rex :w 1 :b (reg-ext-p->bit dst)))
       (cond ((eq amt :cl)
              (emit-byte emit #xd3)
              (with-modrm (emit ,ext dst) fixups))
             ((= amt 1)
              (emit-byte emit #xd1)
              (with-modrm (emit ,ext dst) fixups))
             (t
              (emit-byte emit #xc1)
              (with-modrm (emit ,ext dst) fixups)
              (emit-byte emit (logand amt #xff)))))))

(def-shift :shl 4)
(def-shift :shr 5)
(def-shift :sar 7)

;;; --- push/pop ---

(definsn :push (emit args)
  (declare (ignore fixups))
  (let ((r (first args)))
    (when (reg-ext-p r) (emit-byte emit (rex :b 1)))
    (emit-byte emit (+ #x50 (reg-low3 r)))))

(definsn :pop (emit args)
  (declare (ignore fixups))
  (let ((r (first args)))
    (when (reg-ext-p r) (emit-byte emit (rex :b 1)))
    (emit-byte emit (+ #x58 (reg-low3 r)))))

;;; --- call / ret / jumps ---

(definsn :call (emit args)
  (let ((target (first args)))
    (cond ((gpr-p target)
           (when (reg-ext-p target) (emit-byte emit (rex :b 1)))
           (emit-byte emit #xff)
           (with-modrm (emit 2 target) fixups))
          (t
           (emit-byte emit #xe8)
           (push (make-fixup :offset :patch-here :kind :rel32 :label target)
                 fixups)
           (funcall emit (bv-s32le 0))))))

(definsn :jmp (emit args)
  (let ((target (first args)))
    (cond ((gpr-p target)
           (when (reg-ext-p target) (emit-byte emit (rex :b 1)))
           (emit-byte emit #xff)
           (with-modrm (emit 4 target) fixups))
          (t
           (emit-byte emit #xe9)
           (push (make-fixup :offset :patch-here :kind :rel32 :label target)
                 fixups)
           (funcall emit (bv-s32le 0))))))

(defparameter *jcc-opcodes*
  '((:jo . #x80) (:jno . #x81) (:jb . #x82) (:jae . #x83)
    (:je . #x84) (:jne . #x85) (:jbe . #x86) (:ja . #x87)
    (:js . #x88) (:jns . #x89) (:jp . #x8a) (:jnp . #x8b)
    (:jl . #x8c) (:jge . #x8d) (:jle . #x8e) (:jg . #x8f)))

(dolist (pair *jcc-opcodes*)
  (destructuring-bind (name . op2) pair
    (setf (gethash name *mnemonics*)
          (let ((op op2))
            (lambda (emit args)
              (emit-byte emit #x0f)
              (emit-byte emit op)
              (funcall emit (bv-s32le 0))
              (list (make-fixup :offset :patch-here :kind :rel32 :label (first args))))))))

(definsn :ret (emit args)
  (declare (ignore args fixups))
  (emit-byte emit #xc3))

(definsn :nop (emit args)
  (declare (ignore args fixups))
  (emit-byte emit #x90))

(definsn :int3 (emit args)
  (declare (ignore args fixups))
  (emit-byte emit #xcc))

(definsn :syscall (emit args)
  (declare (ignore args fixups))
  (emit-bytes emit #x0f #x05))

;;; --- test ---

(definsn :test (emit args)
  (destructuring-bind (dst src) args
    (cond ((and (gpr-p dst) (gpr-p src))
           (emit-byte emit (rex :w 1 :r (reg-ext-p->bit src) :b (reg-ext-p->bit dst)))
           (emit-byte emit #x85)
           (with-modrm (emit src dst) fixups))
          ((and (gpr-p dst) (integerp src))
           (emit-byte emit (rex :w 1 :b (reg-ext-p->bit dst)))
           (emit-byte emit #xf7)
           (with-modrm (emit 0 dst) fixups)
           (funcall emit (bv-s32le src)))
          (t (error "Unsupported TEST: ~s ~s" dst src)))))

;;; --- setcc ---

(dolist (pair *jcc-opcodes*)
  (destructuring-bind (jname . op2) pair
    (let ((sname (intern (concatenate 'string "SET" (subseq (symbol-name jname) 1))
                         :keyword))
          (op op2))
      (setf (gethash sname *mnemonics*)
            (lambda (emit args)
              (let ((fixups '())
                    (dst (first args)))
                ;; r/m8; a REX (any) is required to access sil/dil/bpl/spl and r8b+.
                (when (or (reg-ext-p dst) (member dst '(:rsp :rbp :rsi :rdi)))
                  (emit-byte emit (rex :b (reg-ext-p->bit dst))))
                (emit-byte emit #x0f)
                (emit-byte emit op)
                (with-modrm (emit 0 dst) fixups)
                fixups))))))

;;; --- cmov (cc op2 + #x40) ---

(dolist (pair *jcc-opcodes*)
  (destructuring-bind (jname . op2) pair
    (let ((cname (intern (concatenate 'string "CMOV" (subseq (symbol-name jname) 1))
                         :keyword))
          (op op2))
      (setf (gethash cname *mnemonics*)
            (lambda (emit args)
              (let ((fixups '()))
                (destructuring-bind (dst src) args
                  (emit-byte emit (rex :w 1 :r (reg-ext-p->bit dst)
                                       :b (reg-ext-p->bit src)))
                  (emit-byte emit #x0f)
                  (emit-byte emit (+ op #x40))
                  (with-modrm (emit dst src) fixups))
                fixups))))))

;;;; ---------------------------------------------------------------------
;;;; Driver

(defvar *trace-fixups* nil
  "When true, print fixup application to *TRACE-OUTPUT* (debugging).")

(defun assemble (&rest forms)
  "Assemble FORMS into a byte vector. See ASSEMBLE-INTO."
  (assemble-into forms))

(defun assemble-into (forms &key (base-vaddr 0))
  "Assemble FORMS, resolving labels. Returns (values code labels).
Directives: (:label name), (:bytes b...), (:d64 n), (:d32 n), (:align n).
BASE-VADDR is accepted for future absolute references; all references we
generate (branches, RIP-relative) are position-independent."
  (declare (ignore base-vaddr))
  (let ((out '())
        (labels (make-hash-table :test 'eq))
        (fixups '())
        (pos 0))
    (flet ((emit (bv)
             (push bv out)
             (incf pos (length bv))))
      (dolist (form forms)
        (cond
          ((and (consp form) (eq (car form) :label))
           (setf (gethash (cadr form) labels) pos))
          ((and (consp form) (eq (car form) :bytes))
           (dolist (b (cdr form)) (emit (bv-u8 b))))
          ((and (consp form) (eq (car form) :d64))
           (emit (bv-u64le (cadr form))))
          ((and (consp form) (eq (car form) :d32))
           (emit (bv-u32le (cadr form))))
          ((and (consp form) (eq (car form) :align))
           (let* ((n (cadr form))
                  (pad (mod (- n (mod pos n)) n)))
             (dotimes (i pad) (declare (ignore i)) (emit (bv-u8 #x90)))))
          ((consp form)
           (let ((encoder (gethash (car form) *mnemonics*)))
             (unless encoder (error "Unknown lap mnemonic/directive: ~s" (car form)))
             (dolist (f (funcall encoder #'emit (cdr form)))
               (when (eq (fixup-offset f) :patch-here)
                 (setf (fixup-offset f) (- pos 4)
                       (fixup-insn-end f) pos))
               (push f fixups))))
          (t (error "Bad lap form: ~s" form))))
      ;; materialize
      (let ((code (make-bytevec pos))
            (i 0))
        (dolist (bv (nreverse out))
          (replace code bv :start1 i)
          (incf i (length bv)))
        ;; apply fixups
        (dolist (f fixups)
          (let ((target (gethash (fixup-label f) labels)))
            (when *trace-fixups*
              (format *trace-output* "~&fixup: label=~s off=~s insn-end=~s target=~s~%"
                      (fixup-label f) (fixup-offset f) (fixup-insn-end f) target))
            (unless target (error "Undefined label: ~s" (fixup-label f)))
            (let ((rel (- target (fixup-insn-end f))))
              (unless (fit-s32 rel)
                (error "Label ~s out of rel32 range" (fixup-label f)))
              (bv-set code (fixup-offset f) (bv-s32le rel)))))
        (values code labels)))))
