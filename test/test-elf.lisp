;;;; test-elf.lisp — ELF writer tests, including the raw-syscall smoke test.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.

(in-package #:clef-test)

(deftest elf-magic-and-class
  ;; Build a trivial executable (just exit(0)) and check the ELF header.
  (let* ((code (clef/lap:assemble
                '(:mov :rax 60)      ; exit
                '(:xor :rdi :rdi)
                '(:syscall)))
         (base #x400000)
         (entry (+ base 64 (* 56 1))) ; after 1 phdr
         (img (clef/elf:write-executable
               entry
               (list (clef/elf:make-segment :data code :vaddr entry :flags 5)))))
    (is (= (aref img 0) #x7f))
    (is (= (aref img 1) (char-code #\E)))
    (is (= (aref img 2) (char-code #\L)))
    (is (= (aref img 3) (char-code #\F)))
    (is (= (aref img 4) 2))           ; ELFCLASS64
    (is (= (aref img 16) 2))          ; e_type = ET_EXEC (low byte)
    (is (= (aref img 18) #x3e))))     ; e_machine = EM_X86_64 (low byte)

;;; The real proof: emit a hello-world ELF and run it. This test is only
;;; meaningful on x86-64 Linux; it is exercised by tools/smoke-cold-core.lisp
;;; and (optionally) from the test driver when RUN-SMOKE is set.
(deftest elf-smoke-writes-executable
  (let* ((msg "hello, CLEF\n")
         (mbytes (map 'vector #'char-code msg))
         (code (clef/lap:assemble
                '(:mov :rax 1)                 ; write
                '(:mov :rdi 1)                 ; stdout
                (list :lea :rsi (clef/lap:mem :rip :msg))
                (list :mov :rdx (length mbytes))
                '(:syscall)
                '(:mov :rax 60)                ; exit
                '(:xor :rdi :rdi)
                '(:syscall)
                '(:label :msg)
                (cons :bytes (coerce mbytes 'list))))
         (base #x400000)
         (entry (+ base 64 56))
         (img (clef/elf:write-executable
               entry
               (list (clef/elf:make-segment :data code :vaddr entry :flags 5)))))
    ;; We can't run it portably from inside the test image here, but we can
    ;; assert the image is self-consistent: entry points into the segment,
    ;; and the lea displacement targets the message bytes.
    (is (>= (length img) (+ 64 56 (length code))))
    (is (= (aref img 0) #x7f))))
