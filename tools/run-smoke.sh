#!/bin/sh
# CC0 1.0 Universal — public domain. See LICENSE.
# Run the cold-core smoke test: assemble + emit + execute a zero-C ELF.
set -eu

if [ $# -ge 1 ]; then
  SBCL="$1"
elif command -v sbcl >/dev/null 2>&1; then
  SBCL="sbcl"
else
  SBCL="$(ls -d /gnu/store/*-sbcl-2*/bin/sbcl 2>/dev/null | sort | tail -1)"
fi
[ -n "$SBCL" ] || { echo "no SBCL found" >&2; exit 1; }

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"
exec "$SBCL" --script tools/smoke-cold-core.lisp
