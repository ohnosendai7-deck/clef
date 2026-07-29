;;;; zip.lisp — a minimal ZIP writer in Common Lisp.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; Writes ZIP archives (local file headers + central directory + EOCD) with
;;;; stored (uncompressed) and raw-deflate entries. APKs are ZIP files, so
;;;; this is the foundation of the APK packager. CRC32 implemented here
;;;; (ISO 3309 / the standard table). Raw DEFLATE is delegated to a provided
;;;; compressor (host zlib on SBCL, or stored when unavailable).

(in-package #:clef/android)

;;; --- CRC32 (polynomial 0xEDB88320, reflected) ---

(defvar *crc-table*
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((c n))
        (dotimes (k 8)
          (setf c (if (oddp c)
                      (logxor #xEDB88320 (ash c -1))
                      (ash c -1))))
        (setf (aref table n) c))))
  "The standard CRC32 lookup table.")

(defun crc32 (bytes &optional (crc #xFFFFFFFF))
  "CRC32 of BYTES (a vector of (unsigned-byte 8)-valued octets)."
  (let ((c crc))
    (loop for b across bytes
          do (setf c (logxor (ash c -8)
                             (aref *crc-table* (logxor (logand c #xFF) b)))))
    (logxor c #xFFFFFFFF)))

;;; --- little-endian writers ---

(defun z-u16 (n) (clef/util:bv-u16le n))
(defun z-u32 (n) (clef/util:bv-u32le n))

;;; --- entries ---

(defstruct (zentry (:constructor %make-zentry))
  name                      ; string, UTF-8/ASCII path (e.g. "classes.dex")
  data                      ; (unsigned-byte 8) vector, uncompressed contents
  (method :stored))         ; :stored or :deflate

(defun make-entry (name data &key (method :stored))
  (%make-zentry :name name :data data :method method))

(defun name->bytes (name)
  "Encode an entry name to bytes (ASCII)."
  (map 'vector #'char-code name))

;;; --- compression ---
;;; We only store or use raw DEFLATE. Raw deflate via host zlib when on SBCL;
;;; otherwise store. Compression level is irrelevant to validity.

(defun compress-entry-data (entry)
  "Return (values method compressed-bytes crc uncompressed-size)."
  (let* ((data (zentry-data entry))
         (crc (crc32 data))
         (usize (length data)))
    (ecase (zentry-method entry)
      (:stored (values 0 data crc usize))
      (:deflate
       (multiple-value-bind (cbytes ok) (deflate-raw data)
         (if ok
             (values 8 cbytes crc usize)
             (values 0 data crc usize)))))))

(defun deflate-raw (data)
  "Raw DEFLATE (no zlib header) of DATA. Returns (values bytes ok). On SBCL
uses sb-zlib if available; otherwise (values data nil) so caller stores."
  (declare (ignorable data))
  (values data nil))  ; default: no compressor; see APK for SBCL-specific

;;; --- the writer ---

(defun write-zip (entries)
  "Build a ZIP archive (byte vector) from ENTRIES (a list of ZENTRY)."
  (let ((locals '())
        (centrals '())
        (offset 0))
    (dolist (e entries)
      (multiple-value-bind (method cdata crc usize) (compress-entry-data e)
        (let* ((nb (name->bytes (zentry-name e)))
               (csize (length cdata))
               (local (apply #'bv-concat
                             (z-u32 #x04034b50)      ; local file header sig
                             (z-u16 20)             ; version needed
                             (z-u16 0)              ; flags
                             (z-u16 method)
                             (z-u16 0)              ; mod time
                             (z-u16 0)              ; mod date
                             (z-u32 crc)
                             (z-u32 csize)
                             (z-u32 usize)
                             (z-u16 (length nb))
                             (z-u16 0)              ; extra len
                             nb
                             (list cdata))))
          (push local locals)
          (push (apply #'bv-concat
                       (z-u32 #x02014b50)          ; central header sig
                       (z-u16 20)                  ; version made by
                       (z-u16 20)                  ; version needed
                       (z-u16 0)                    ; flags
                       (z-u16 method)
                       (z-u16 0)                    ; time
                       (z-u16 0)                    ; date
                       (z-u32 crc)
                       (z-u32 csize)
                       (z-u32 usize)
                       (z-u16 (length nb))
                       (z-u16 0)                    ; extra
                       (z-u16 0)                    ; comment
                       (z-u16 0)                    ; disk number
                       (z-u16 0)                    ; internal attrs
                       (z-u32 0)                    ; external attrs
                       (z-u32 offset)               ; local header offset
                       (list nb))
                centrals)
          (incf offset (length local)))))
    (let* ((local-part (apply #'bv-concat (nreverse locals)))
           (central-part (apply #'bv-concat (nreverse centrals)))
           (cd-offset (length local-part))
           (cd-size (length central-part))
           (n (length entries))
           (eocd (bv-concat
                  (z-u32 #x06054b50)               ; EOCD sig
                  (z-u16 0) (z-u16 0)              ; disk numbers
                  (z-u16 n) (z-u16 n)              ; entry counts
                  (z-u32 cd-size)
                  (z-u32 cd-offset)
                  (z-u16 0))))                     ; comment len
      (bv-concat local-part central-part eocd))))

;;; --- the reader ---
;;; Minimal read support: enough to recover entry names and (stored)
;;; contents back from an archive, e.g. to re-sign an APK. Only the
;;; no-compression method is decoded.

(defun zref-u16 (bytes offset)
  "Read a little-endian u16 from BYTES at OFFSET."
  (logior (aref bytes offset)
          (ash (aref bytes (1+ offset)) 8)))

(defun zref-u32 (bytes offset)
  "Read a little-endian u32 from BYTES at OFFSET."
  (logior (aref bytes offset)
          (ash (aref bytes (1+ offset)) 8)
          (ash (aref bytes (+ offset 2)) 16)
          (ash (aref bytes (+ offset 3)) 24)))

(defun zslice (bytes start length)
  "Copy LENGTH octets of BYTES starting at START into a fresh byte vector."
  (let ((out (make-bytevec length)))
    (replace out bytes :start2 start)
    out))

(defun zfind-eocd (bytes)
  "Locate the end-of-central-directory record; return its offset."
  (let ((min (max 0 (- (length bytes) 22 65536))))
    (loop for pos downfrom (- (length bytes) 22) to min
          when (= (zref-u32 bytes pos) #x06054b50)
            do (return pos)
          finally (error "Not a ZIP archive: no end-of-central-directory"))))

(defun read-zip (bytes)
  "Parse the ZIP archive BYTES and return a list of ZENTRY (name + data).
Only stored (uncompressed) entries are supported."
  (let* ((eocd (zfind-eocd bytes))
         (count (zref-u16 bytes (+ eocd 10)))
         (cd-offset (zref-u32 bytes (+ eocd 16)))
         (entries '())
         (pos cd-offset))
    (dotimes (i count (nreverse entries))
      (unless (= (zref-u32 bytes pos) #x02014b50)
        (error "Bad central directory at offset ~d" pos))
      (let* ((method (zref-u16 bytes (+ pos 10)))
             (csize (zref-u32 bytes (+ pos 20)))
             (nlen (zref-u16 bytes (+ pos 28)))
             (elen (zref-u16 bytes (+ pos 30)))
             (clen (zref-u16 bytes (+ pos 32)))
             (lho (zref-u32 bytes (+ pos 42)))
             (name (map 'string #'code-char (zslice bytes (+ pos 46) nlen))))
        (unless (= (zref-u32 bytes lho) #x04034b50)
          (error "Bad local header for ~s" name))
        (unless (zerop method)
          (error "Unsupported compression method ~d for ~s (only stored)"
                 method name))
        (let* ((lnlen (zref-u16 bytes (+ lho 26)))
               (lelen (zref-u16 bytes (+ lho 28)))
               (data-start (+ lho 30 lnlen lelen)))
          (push (%make-zentry :name name
                              :data (zslice bytes data-start csize)
                              :method :stored)
                entries))
        (incf pos (+ 46 nlen elen clen))))))
