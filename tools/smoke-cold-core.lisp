;;;; smoke-cold-core.lisp — end-to-end proof of the zero-C path.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; Assembles a raw-syscall hello-world with lap, emits a static ELF with no
;;;; libc and no `ld`, writes it to a file, runs it, and checks the output.
;;;; This is the coldest possible cold core: _start → write → exit.
;;;;
;;;; Usage: sbcl --script tools/smoke-cold-core.lisp

(require :asdf)
(require :sb-posix)
(asdf:load-asd (merge-pathnames "../clef.asd" *load-pathname*))
(asdf:load-system :clef)

(defparameter *msg* "hello, CLEF - zero C, from SBCL, through lap and ELF.\n")

(defun build-hello ()
  (let* ((mbytes (map 'list #'char-code *msg*))
         (code (clef/lap:assemble
                '(:mov :rax 1)                  ; sys_write
                '(:mov :rdi 1)                  ; fd = stdout
                (list :lea :rsi (clef/lap:mem :rip :msg))
                (list :mov :rdx (length mbytes))
                '(:syscall)
                '(:mov :rax 60)                 ; sys_exit
                '(:xor :rdi :rdi)               ; status 0
                '(:syscall)
                '(:label :msg)
                (cons :bytes mbytes)))
         (base #x400000)
         (entry (+ base 64 56))                ; after ehdr + 1 phdr
         (img (clef/elf:write-executable
               entry
               (list (clef/elf:make-segment :data code :vaddr entry :flags 5)))))
    img))

(defun main ()
  (let ((path "/tmp/clef-hello"))
    (with-open-file (s path :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (write-sequence (build-hello) s))
    (sb-posix:chmod path #o755)
    (format t "Wrote ~a (~d bytes). Running...~%" path
            (sb-posix:stat-size (sb-posix:stat path)))
    (let ((out (with-output-to-string (str)
                 (sb-ext:run-program path '() :output str :wait t))))
      (format t "Output:~%---~%~a---~%" out)
      (if (string= out *msg*)
          (progn (format t "SMOKE TEST PASSED: zero-C ELF ran and printed correctly.~%")
                 (uiop:quit 0))
          (progn (format t "SMOKE TEST FAILED: output mismatch.~%")
                 (uiop:quit 1))))))

(main)
