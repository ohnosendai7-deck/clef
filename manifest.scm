;;; manifest.scm — Guix manifest for the CLEF bootstrap host.
;;;
;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;
;;; Pins the bootstrap toolchain. SBCL is the only build-time dependency and
;;; is a tool, never a source of target code (see AGENTS.md, CLEANROOM.md).
;;; Use:
;;;   guix shell -m manifest.scm
;;; to get a reproducible bootstrap environment.

(specifications->manifest
 '("sbcl"))
