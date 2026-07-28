;;;; elf.lisp — a minimal static ELF64 writer for x86-64 Linux.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; Writes a static, position-dependent executable with program headers only
;;;; (no section headers), which is all the Linux loader needs. This is how the
;;;; cold core is emitted: raw syscalls, no libc, no `ld`.

(in-package #:clef/elf)

(defconstant +elf-header-size+ 64)
(defconstant +phdr-size+ 56)

(defstruct segment
  data          ; bytevec contents
  vaddr         ; virtual address to load at
  (flags 5))    ; PF_R=4 PF_W=2 PF_X=1

;;; Internal layout segment with an assigned file offset.
(defstruct (lseg (:conc-name seg-))
  data vaddr flags offset)

(defun compute-offsets (segments hdr-size page-size)
  "Assign file offsets to SEGMENTS so that offset ≡ vaddr (mod page-size),
which mmap requires. Returns a list of LSEG."
  (let ((offset hdr-size)
        (out '()))
    (dolist (s segments (nreverse out))
      (let* ((vaddr (segment-vaddr s))
             ;; smallest offset >= current that is page-congruent with vaddr
             (aligned (+ offset (mod (- vaddr offset) page-size))))
        (push (make-lseg :data (segment-data s)
                         :vaddr vaddr
                         :flags (segment-flags s)
                         :offset aligned)
              out)
        (setf offset (+ aligned (max 1 (length (segment-data s)))))))))

(defun elf-header (entry phoff phnum)
  "The 64-byte ELF header."
  (bv-concat
   (make-array 16 :element-type '(unsigned-byte 8)
                  :initial-contents
                  '(#x7f #x45 #x4c #x46  ; \x7f E L F
                    2                    ; ELFCLASS64
                    1                    ; ELFDATA2LSB
                    1                    ; EV_CURRENT
                    0                    ; ELFOSABI_SYSV
                    0 0 0 0 0 0 0 0))    ; ABI version + padding
   (bv-u16le 2)                          ; e_type = ET_EXEC
   (bv-u16le #x3e)                       ; e_machine = EM_X86_64
   (bv-u32le 1)                          ; e_version
   (bv-u64le entry)                      ; e_entry
   (bv-u64le phoff)                      ; e_phoff
   (bv-u64le 0)                          ; e_shoff (no sections)
   (bv-u32le 0)                          ; e_flags
   (bv-u16le +elf-header-size+)          ; e_ehsize
   (bv-u16le +phdr-size+)                ; e_phentsize
   (bv-u16le phnum)                      ; e_phnum
   (bv-u16le 0)                          ; e_shentsize
   (bv-u16le 0)                          ; e_shnum
   (bv-u16le 0)))                        ; e_shstrndx

(defun program-header (seg)
  "A 56-byte PT_LOAD program header for SEG (an LSEG)."
  (bv-concat
   (bv-u32le 1)                          ; p_type = PT_LOAD
   (bv-u32le (seg-flags seg))            ; p_flags
   (bv-u64le (seg-offset seg))           ; p_offset
   (bv-u64le (seg-vaddr seg))            ; p_vaddr
   (bv-u64le (seg-vaddr seg))            ; p_paddr
   (bv-u64le (length (seg-data seg)))    ; p_filesz
   (bv-u64le (length (seg-data seg)))    ; p_memsz
   (bv-u64le #x1000)))                   ; p_align

(defun write-executable (entry-vaddr segments &key (page-size #x1000))
  "Build a complete static ELF64 executable file image.
ENTRY-VADDR is the entry point virtual address; SEGMENTS is a list of SEGMENT.
Returns a byte vector suitable for writing to a file and chmod +x."
  (let* ((nseg (length segments))
         (hdr-size (+ +elf-header-size+ (* nseg +phdr-size+)))
         (lsegs (compute-offsets segments hdr-size page-size))
         (file-size (reduce #'max lsegs
                            :key (lambda (s) (+ (seg-offset s)
                                                (length (seg-data s))))
                            :initial-value hdr-size))
         (image (make-bytevec file-size)))
    ;; ELF header + program headers
    (replace image (elf-header entry-vaddr +elf-header-size+ nseg) :start1 0)
    (loop for s in lsegs
          for i from 0
          do (replace image (program-header s)
                      :start1 (+ +elf-header-size+ (* i +phdr-size+))))
    ;; segment bytes at their offsets
    (dolist (s lsegs image)
      (replace image (seg-data s) :start1 (seg-offset s)))))
