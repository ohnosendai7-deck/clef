# CLEF

**Common Lisp, Effects-First.**

CLEF is an ANSI Common Lisp implementation built around a statically-typed,
effect-annotated intermediate representation. It is JIT-first (copy-and-patch
baseline, specializing tiers above it), has a concurrent garbage collector
written in Lisp itself, and targets Linux (x86-64) and Android (aarch64) with
no libc dependency — raw syscalls on Linux, a native `NativeActivity` shared
library on Android.

The entire toolchain is Common Lisp or DSLs embedded in Common Lisp. There is
no C anywhere in the project: not in the runtime, not in the garbage collector,
not in the bootstrap. The system cross-compiles itself from SBCL and then
self-hosts.

Everything in this repository is released into the **public domain** under
[CC0 1.0 Universal](LICENSE). See [CLEANROOM.md](CLEANROOM.md) for the
contribution policy that keeps it that way.

## Status

Pre-M0. The design is settled (see [docs/DESIGN.md](docs/DESIGN.md)); the
repository is a seed. There is no runnable code yet.

## Design highlights

- **Typed-effect IR** — conditions are algebraic effects, special variables are
  dynamically-scoped state effects, and users can declare effect bounds that
  the compiler verifies.
- **Package contexts** — first-class, dynamically-installable bundles of
  per-package variable bindings (`with-package-env`), enabling multiple
  independent instances of stateful libraries in one image.
- **Logic-engine type system** — a hybrid of a tabled Prolog front-end and a
  BDD-based set-constraint solver for CL's set-theoretic type algebra.
- **Concurrent GC in CL** — Immix-style size-class segregation with humongous
  regions, SATB concurrent marking, and bounded-stop-the-world evacuation. The
  collector's allocation-freedom is proven by the effect system, not by
  convention.
- **Zero-C bootstrap** — cross-compiled from SBCL into a static ELF with raw
  syscalls; Android is the same compiler emitting a static `.so`.

## Documents

- [docs/DESIGN.md](docs/DESIGN.md) — the full design.
- [CLEANROOM.md](CLEANROOM.md) — clean-room policy (required reading for contributors).
- [CONTRIBUTING.md](CONTRIBUTING.md) — how to contribute.
- [AGENTS.md](AGENTS.md) — instructions for AI agents working in this repo.

## License

[CC0 1.0 Universal](LICENSE) — public domain dedication. No rights reserved.
