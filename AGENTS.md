# AGENTS.md — Instructions for AI Agents

This file tells AI coding agents how to work in the CLEF repository. Read it
fully before making changes.

## Project in one paragraph

CLEF is an ANSI Common Lisp implementation with a statically-typed,
effect-annotated IR, JIT-first compilation (copy-and-patch baseline), a
concurrent GC written in Lisp, and targets Linux (x86-64, raw syscalls) and
Android (aarch64, NativeActivity). **There is no C anywhere in the project.**
The toolchain is Common Lisp and CL-embedded DSLs, bootstrapped from SBCL.
The full design is in [docs/DESIGN.md](docs/DESIGN.md) — read it before
designing anything.

## Hard constraints (do not violate)

1. **No C.** No C source, no C compiler in the build, no FFI to C libraries
   from the toolchain, no GMP, no Boehm, no libffi. If you think you need C,
   you don't — find the Lisp solution or escalate to the maintainers.
2. **Public domain only.** Every file must be CC0. Never copy code from
   another CL implementation or any non-PD source. Read
   [CLEANROOM.md](CLEANROOM.md) before writing any component that has an
   existing implementation elsewhere. Implement from the ANSI spec, AMOP, and
   academic papers — never from other implementations' source.
3. **SBCL is a build-time tool only.** The cross-compiler runs on SBCL, but no
   SBCL code, SBCL internals, or SBCL-derived source enters the target image.
4. **The GC is written in CLEF's systems subset** and its allocation-freedom
   must be provable by the effect system. Do not introduce allocation into GC
   code paths.

## Repository layout

- `docs/` — design documents. `docs/DESIGN.md` is the canonical design.
- `src/` — implementation (empty until M0a).
- `test/` — test harness (empty until M0a).
- `.github/workflows/` — CI.

## Conventions (once code exists)

- **Lisp style:** SBCL-house style. Lowercase symbols, `defun` not `defun-inline`
  without reason, docstrings on all public functions. Package-per-module with
  explicit `:export` lists; no `:use` of implementation-internal packages.
- **Effect annotations:** systems-subset code (anything that might run during
  GC or bootstrap) must carry explicit effect declarations. The type solver
  checks them; CI fails if they don't hold.
- **Testing:** every component ships with tests. Run the full suite before
  submitting. Conformance is measured against `ansi-tests`.
- **Commit messages:** imperative mood, one line summary, body explaining *why*.
  Reference design doc sections when a change implements or alters the design.

## What agents should do

- Follow docs/DESIGN.md. When the design is silent or ambiguous, open an issue
  rather than inventing a direction.
- Write tests with (or before) implementation.
- When implementing a component that exists in other CLs, state in the PR
  description that the work is clean-room per CLEANROOM.md and list the
  documents you implemented from.

## What agents must not do

- Do not add any non-PD dependency, for any reason, without explicit maintainer
  approval in the issue tracker.
- Do not read or reproduce another CL implementation's source.
- Do not weaken the effect-system guarantees (e.g. "just allocate here, the GC
  will cope") to make something work — that is how the GC dies.
- Do not commit generated binary artifacts (images, fasls, ELFs) to the repo.

## Current status

M0a foundation landed and tested (see README.md): lap x86-64 assembler, ELF
writer, a running zero-C cold-core smoke test, GC and solver skeletons, and 30
passing host tests. The M0b+ workstreams are tracked in the issue tracker.

The first milestone work is: the cold core (raw-syscall runtime + tree-walking
evaluator), the concurrent GC (model-tested on SBCL), the reader (clean-room),
and growing the solver. See docs/DESIGN.md §Milestones.
