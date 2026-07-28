;;;; test-lap.lisp — lap DSL tests.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.

(in-package #:clef-test)

(defun bytes (&rest bs)
  (make-array (length bs) :element-type '(unsigned-byte 8)
                          :initial-contents bs))

(deftest lap-mov-imm32
  ;; mov rax, 1  =>  48 c7 c0 01 00 00 00
  (is (equalp (clef/lap:assemble '(:mov :rax 1))
              (bytes #x48 #xc7 #xc0 1 0 0 0))))

(deftest lap-mov-imm64
  ;; mov rax, big  =>  48 b8 <imm64>
  (let ((code (clef/lap:assemble (list :mov :rax (expt 2 40)))))
    (is (= (length code) 10))
    (is (= (aref code 0) #x48))
    (is (= (aref code 1) #xb8))))

(deftest lap-mov-reg-reg
  ;; mov rax, rbx. Our :mov dst,src uses the 8B /r form (r/m64 -> r64):
  ;; modrm = 11 000 011 = C3. => 48 8B C3.
  (is (equalp (clef/lap:assemble '(:mov :rax :rbx))
              (bytes #x48 #x8b #xc3))))

(deftest lap-mov-load-mem
  ;; mov rax, [rsp+8]  =>  48 8b 44 24 08
  (is (equalp (clef/lap:assemble (list :mov :rax (clef/lap:mem :rsp 8)))
              (bytes #x48 #x8b #x44 #x24 #x08))))

(deftest lap-mov-store-mem
  ;; mov [rsp+8], rax  =>  48 89 44 24 08
  (is (equalp (clef/lap:assemble (list :mov (clef/lap:mem :rsp 8) :rax))
              (bytes #x48 #x89 #x44 #x24 #x08))))

(deftest lap-add-imm
  ;; add rax, 5  =>  48 83 c0 05
  (is (equalp (clef/lap:assemble '(:add :rax 5))
              (bytes #x48 #x83 #xc0 5))))

(deftest lap-sub-reg
  ;; sub rax, rcx  =>  48 29 c8
  (is (equalp (clef/lap:assemble '(:sub :rax :rcx))
              (bytes #x48 #x29 #xc8))))

(deftest lap-push-pop
  (is (equalp (clef/lap:assemble '(:push :rbp))
              (bytes #x55)))
  (is (equalp (clef/lap:assemble '(:pop :rbp))
              (bytes #x5d))))

(deftest lap-ret
  (is (equalp (clef/lap:assemble '(:ret)) (bytes #xc3))))

(deftest lap-syscall
  (is (equalp (clef/lap:assemble '(:syscall)) (bytes #x0f #x05))))

(deftest lap-jmp-forward-backward
  ;; A loop: label, dec, jne back. Branch displacement should be negative.
  (let ((code (clef/lap:assemble
                '(:mov :rax 3)
                '(:label :loop)
                '(:dec :rax)
                '(:jne :loop))))
    (is (> (length code) 10))
    ;; the rel32 of the jne (last 4 bytes) should be negative (high byte set)
    (is (> (aref code (1- (length code))) 0))))

(deftest lap-extended-reg
  ;; mov rax, r8. 8B /r form with r8 in the reg field => REX.R set (49), C0.
  (let ((code (clef/lap:assemble '(:mov :rax :r8))))
    (is (= (aref code 0) #x49))
    (is (= (aref code 1) #x8b))
    (is (= (aref code 2) #xc0))))

(deftest lap-labels-and-rip-relative
  ;; RIP-relative load resolves to a label. mov rax,[rip+disp32] is 7 bytes;
  ;; ret 1; data at offset 8. disp = 8 - 7 = 1 (little-endian at bytes 3..6).
  (let ((code (clef/lap:assemble
                (list :mov :rax (clef/lap:mem :rip :data))
                '(:ret)
                '(:label :data)
                '(:d64 42))))
    (is (= (aref code 0) #x48))
    (is (= (aref code 1) #x8b))
    (is (= (aref code 2) #x05))   ; modrm rip-relative
    (is (= (aref code 3) 1))     ; disp32 low byte = 1
    (is (= (aref code 4) 0))))
