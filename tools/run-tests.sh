#!/bin/sh
# CC0 1.0 Universal — public domain. See LICENSE.
# Run the CLEF test suite on SBCL.
# Usage: tools/run-tests.sh [path-to-sbcl]
set -eu

if [ $# -ge 1 ]; then
  SBCL="$1"
elif command -v sbcl >/dev/null 2>&1; then
  SBCL="sbcl"
else
  # Guix store fallback: newest sbcl package.
  SBCL="$(ls -d /gnu/store/*-sbcl-2*/bin/sbcl 2>/dev/null | sort | tail -1)"
fi
[ -n "$SBCL" ] || { echo "no SBCL found" >&2; exit 1; }

ROOT="$(cd "$(dirname "$0")/.." && pwd)"

exec "$SBCL" --non-interactive \
  --eval "(require :asdf)" \
  --eval "(asdf:load-asd (truename \"$ROOT/clef.asd\"))" \
  --eval "(handler-case (asdf:load-system :clef/test)
            (error (e) (format *error-output* \"load error: ~a~%\" e)
                       (uiop:quit 2)))" \
  --eval "(let ((fails (funcall (intern \"RUN-ALL\" :clef-test))))
            (uiop:quit (if (plusp fails) 1 0)))"
