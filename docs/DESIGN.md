# CLEF — Design

**Common Lisp, Effects-First.**

This is the canonical design document for CLEF. It records settled decisions
and the reasoning behind them. Changes to this document are design changes and
go through the same review as code.

Status: **Settled design, pre-implementation.** Everything here is agreed
unless explicitly marked *open*.

---

## 1. Vision

CLEF is an ANSI Common Lisp implementation that treats Common Lisp as what it
secretly is: an effectful language without a type system. Its central move is
to compile through a **statically-typed, effect-annotated IR** in which CL's
most dynamic features — conditions, restarts, special variables, non-local
exits, CLOS dispatch, redefinition — are first-class, typed effects.

CLEF is a **prototyping vehicle**: the REPL is the product surface, startup and
dev-loop latency are primary features, and observability beats peak throughput.

Three commitments shape everything:

1. **JIT-first.** The compiler tiers are the product; AOT is just running the
   tiers offline and serializing.
2. **Zero C.** The entire toolchain — runtime, GC, assembler, linker — is
   Common Lisp or DSLs embedded in Common Lisp. Bootstrap is a cross-compile
   from SBCL, after which CLEF self-hosts.
3. **Public domain.** The tree is CC0, implemented clean-room from the ANSI
   spec, AMOP, and academic papers. See CLEANROOM.md.

Targets: **Linux x86-64** (static ELF, raw syscalls, no libc) and **Android
aarch64** (static `.so`, NativeActivity, no Java/dex in our tree).

---

## 2. Core thesis

The two things a typed-effect IR is best at describing are already central to
Common Lisp:

**Conditions and restarts are algebraic effects.** `signal` is performing an
operation; `handler-bind` is a *resuming* handler (the handler runs, returns,
and execution continues at the signal site); `handler-case` is an *abortive*
handler that unwinds; restarts are a dynamically-bound chain of operations
invoked from handlers. This is the algebraic-effects literature applied
directly.

**Special variables are dynamically-scoped state effects.** A `let` on a
special is a `with` on a named state handler; reading or writing a special is
a `Dyn(var)` effect. Dynamically-scoped ("named") handlers are exactly this.

Crucially, CL has **no call/cc**. All non-local exits (`throw`, `return-from`,
`go`) are dynamic-extent and single-shot. That is a massive simplification:
delimited control becomes handler frames and direct jumps, with no multi-shot
continuations and no stack copying.

---

## 3. Compilation pipeline

### 3.1 Tiers

JIT-first means the tiers *are* the product. Deoptimization always returns to
T0 frames, using LuaJIT-style snapshots (frame descriptors) at every guard.
The IR is serializable, so fasls, code caches, and Android AOT are one
mechanism.

| Tier | Technique | Role |
|------|-----------|------|
| **T0 baseline** | Copy-and-patch stencils | Compile on first eval/call; near-interpreter build cost; ~10× interpreter; seeds inline caches and counters. Stencils are **hand-authored in the lap DSL** (no Clang, unlike CPython). |
| **T1 specializing** | Optimize on the typed-effect IR: inlining with speculation, IC-feedback type specialization, effect-driven optimization | Hot functions; OSR at back-edges. |
| **T2 (optional)** | Sea-of-nodes or tracing for numeric loops | Peak throughput. |

### 3.2 Stages

Surface AST → **EIR** (ANF, statically typed, effect rows) → **LIR** (SSA) →
machine code. A **verifier pass** (like LLVM's) checks that every `perform` is
under a handler or declared in the row; this is essential for debugging a JIT
with a small team.

---

## 4. The typed-effect IR (EIR)

Every expression carries a **type** and an **effect row**.

```lisp
;; Source
(defun read-config (path)
  (with-open-file (s path) (read s)))

;; EIR sketch
λread-config(path: Any) → Any / {io, signal}
  %s = open(path)   : Stream / {io, signal}
  %v = read(%s)     : Any    / {io, signal, alloc}
  close(%s)         : ()     / {io}
  ret %v
```

### 4.1 Effect algebra

Effects tracked:

| Effect | Meaning |
|--------|---------|
| `alloc` | Heap allocation. |
| `io` | Input/output. |
| `signal` | May signal a condition. |
| `dyn(r, var)` / `dyn(w, var)` | Reads / writes special variable `var`. |
| `control` | Non-local exit (`throw`/`catch`; note `catch` tags are *evaluated*, so this is a dynamically-scoped control effect). |
| `dispatch` | CLOS generic call. |
| `foreign` | Calls foreign code. |
| `redef` | Global redefinition (gates inlining behind dependency hooks). |

### 4.2 Gradual types

CL's type algebra (`or`/`and`/`not`/`member`) is set-based, so subtyping is
**semantic subtyping** (Frisch–Castagna), not Hindley–Milner. Types are
gradual by construction: source is untyped; EIR types come from declarations,
flow analysis (`typep` branches refine), and **runtime feedback**. Unknown is
`Any` + top effect; guards deopt on violation, and IC feedback retunes guard
specificity over time.

### 4.3 Row polymorphism vs. specialization

Higher-order functions (`mapcar`) need effect polymorphism in principle, but
CLEF is JIT-first and specializes clones per calling context anyway. The
design uses **limited effect rows with simple unification, resolved by
specialization**, rather than general row inference.

Handlers lower by **evidence passing**: handlers become hidden arguments, and
evidence vectors map directly onto special-variable bindings, so codegen is
uniform.

### 4.4 What effects buy

- No `signal` in a region → elide handler-frame setup and unwind checks.
- Callee rows exclude `dyn(foo)` → prove a `let` of `*foo*` unobservable →
  skip save/restore, demote to a local.
- `alloc` + escape analysis → stack allocation. CL's `dynamic-extent`
  declarations become user-supplied effect contracts.
- `control`-free dynamic extent → `block`/`return-from`/`go` become local
  jumps with no landing pads.

### 4.5 User-facing effects

Effects are **both internal and user-facing**. Users may write
`(declare (effect (not io)))` and the compiler verifies it. This turns the
optimizer's contract into a language feature and — critically — is what makes
the garbage collector correct (§6).

---

## 5. The type system: a hybrid logic solver

### 5.1 Why hybrid

Prolog is a good *front-end* and a bad *solver core* for set-theoretic types.
Pure Prolog clauses for union/intersection/negation subtyping hit an
exponential wall. The design is a **hybrid**, in the HM(X)/CHR style: logic
variables and relations in Prolog, decidable fragments in custom constraint
stores.

### 5.2 Architecture

- **Core:** a tabled Prolog (or µKanren + tabling + attributed variables),
  written in CLEF itself. Tabling gives memoized recursive subtype queries;
  attributed variables give suspension-on-unbound (constraint stores);
  **rational-tree unification gives recursive types for free** (`list-of` is a
  cyclic term).
- **Set-theoretic constraint:** `sub(T1, T2)` over unions, intersections,
  negations, and products is a *suspended constraint* implemented with BDDs,
  per semantic subtyping.
- **Graduality:** `Any` + consistent-subtyping (Siek–Taha). Consistent
  subtyping is a relation, which Prolog expresses natively. Where consistency
  holds only dynamically, the solver emits a `guard` op with a deopt target.

### 5.3 JIT latency and redefinition

The solver is **modular**: per-function summaries (pre/post type + effect row,
with row variables) are cached and keyed by a *redefinition epoch*. T0 solves
nothing; T1 re-solves only hot functions and their invalidated dependencies.
Incremental invalidation matters more than raw solver speed, because
redefinition is constant in Lisp development.

### 5.4 Synergies

- **`subtypep`**: ANSI requires it and most implementations punt
  (`nil, nil`). CLEF answers real queries through the same engine — a genuine
  differentiator.
- **CLOS method applicability** is a logic query over specializers and class
  precedence lists — but it is solved at compile/redefinition time and
  compiled to decision trees, never at dispatch time.
- **Occurrence typing:** `typep` in a branch asserts facts into that branch's
  environment; `typecase` is disjunction elimination.

---

## 6. The garbage collector

### 6.1 Why concurrent from day one

The expensive part of a concurrent GC is not the collector — it is the
**write barriers**. Retrofitting barriers into a JIT built without them is a
multi-quarter disaster (every backend, IC, and deopt path must become
barrier-aware). Building T0/T1 with barriers as first-class LIR ops from the
start costs a few percent forever; adding them later costs a rewrite. So CLEF
is concurrent from the start, which in practice means **barrier-aware codegen
from the start**.

### 6.2 Size-class segregation

The heap is segregated by size class (Immix-style) so that a large object
never sits in the middle of small-object space and pins fragmentation.

| Space | Granularity | Serves | Behavior |
|-------|-------------|--------|----------|
| **Nursery** | TLABs, bump + semispace flip | All fresh allocation | Copying Cheney scan; stats feed the JIT. |
| **Small old-gen** | Immix blocks in ~1–4 MB regions; 128-byte lines with line-mark tables | Conses (dedicated class: 2 words, **no header**), closures, structs | Mark-region; reclaim free lines without moving; **opportunistic evacuation** of only the most-fragmented blocks → bounded worst-case pause. |
| **Medium** | Whole blocks | Mid-size vectors/strings | Block-per-object. |
| **Large / humongous** | Whole regions, multiples of region size | Big arrays | **Never moved, never mixed with small space.** |
| **Code** | Regions, W^X dual-mapped | JIT output | Reclaimed on redefinition/epoch rollover. |
| **Immortal RO image** | `mmap`'d read-only regions | AOT boot image, core fasls | No GC; shareable between processes (ART boot-image model → sub-100 ms on-device REPL start). |

Per-region **remembered sets** allow collecting subsets of the heap (G1-style
collection sets) — the incremental-pause story for Android — and compose with
`onTrimMemory`: empty regions are `madvise`d back to the OS.

### 6.3 The collector

| Phase | Mechanism | Where |
|-------|-----------|-------|
| **Marking** | SATB (snapshot-at-the-beginning, Yuasa deletion barrier) | Concurrent. |
| **Reclaim** | Immix line/block sweeping of free lines | Background threads. |
| **Defrag** | Bounded-STW evacuation of a collection set (most-fragmented blocks), G1-style | Short, size-bounded pause; tunable target ~1–5 ms. |
| **Nursery** | STW semispace scavenge | Sub-ms pause. |

**Not** full concurrent compaction on day one (no Shenandoah Brooks pointers,
no ZGC colored pointers): Brooks costs +1 header word on every object, which
would kill the headerless 2-word cons, and ZGC multi-mapping is unproven under
Android's address-space limits. G1-style keeps evacuation in a bounded pause,
lets forwarding overwrite the evacuated object, and keeps all mark metadata in
**out-of-band side tables** (line-mark bytes + per-block bitmaps), so conses
stay 16 bytes. The architecture leaves a seam for a Brooks/LVB upgrade later,
decided with real pause data in hand (M3 decision point).

### 6.4 Day-one invariants forced into the rest of the system

- **Two barrier families** on pointer stores: a pre-write SATB barrier (log
  the overwritten value) and a post-write card barrier (old→young remembered
  set). T0 emits both unconditionally; T1 elides via the typed IR —
  non-pointer stores, initializing stores, and escape-proven objects are
  provably barrier-free.
- **Polled safepoints + thread-local handshakes**, no POSIX signals (ART owns
  the signal space on Android). Code-space reclamation at redefinition epochs
  runs off handshakes — no thread may hold a PC in a dead epoch.
- **TLABs with allocated-black semantics**: objects allocated mid-cycle are
  implicitly live this cycle; floating garbage is collected next cycle.
- **Ephemeron/finalizer processing in the final mark pause.** The type
  solver's memo tables are ephemeron tables, so this matters early.

### 6.5 The GC is written in Lisp — and the effect system proves it correct

The classic objection to a GC in the language it collects is "the collector
must never allocate while collecting," historically enforced by code review
and prayer. CLEF does better: **alloc-freedom is an effect bound, and the
solver checks it.**

The GC lives in a systems subset of CLEF (the `Raw` subset: unboxed u64,
`memref`, no-boxing arithmetic — a first-class, user-visible restricted mode,
"CLEF/systems") whose declared effect rows exclude `alloc` and `safepoint`
except at explicit yield points. The solver *proves* the mark, sweep, and
evacuation paths are allocation-free at compile time; a violation is a compile
error, not latent heap corruption. GC metadata lives in raw side-table memory
through the `memref` layer — never Lisp objects, never collected.

### 6.6 Testability before the target exists

The `memref` seam inverts the scariest dependency in the project: **the GC is
testable on SBCL before any target code exists.** Back `memref` with a host
byte-vector, model mutators as op streams, and drive the collector as a step
function under a deterministic interleaving harness (systematic exploration +
randomized soak), asserting invariants at every step: every reachable object
marked, no dangling pointers after evacuation, SATB invariant holds. A
hand-rolled concurrent GC normally earns trust through years in production;
CLEF's earns it through exhaustive small-heap model checking before M0 boots.

---

## 7. Package contexts

### 7.1 The feature

```lisp
(defparameter *db-context* (initialize-package 'logic))
(with-package-env *db-context* (add-fact ...))
```

A **package context** is a first-class, dynamically-installable bundle of
per-package variable bindings. Because effects are user-facing, this is
**library code, not compiler magic**: a special-variable access is a `Dyn(var)`
effect performed against a dynamically-installed handler, and a context *is* a
bundle of named handlers (the Koka / Racket-parameterization / ContextL
family).

### 7.2 Semantics

- `initialize-package` (generalized to `make-package-context`) creates fresh
  **value cells** for every `defvar`/`defparameter` in the package, evaluates
  initforms in dependency order inside the new context, and returns the
  cell-table as a value. `defconstant` and function cells stay shared in v1.
- `with-package-env` dynamically installs the bundle as the handler for the
  package's `Dyn` operations. With evidence passing, a special read under a
  context is ~one indirect load; the effect rows let the optimizer prove when
  no intervening handler-install can occur and cache cell addresses across
  straight-line code.
- The default/toplevel context is the global cells → untouched code is 100%
  ANSI. `symbol-value` / `boundp` / `makunbound` operate on the current
  dynamic binding, so standard tooling works inside contexts.
- Threads inherit the installing context at spawn; sharing one context across
  threads is explicit opt-in shared state.
- A `defvar` added after context creation **lazily falls back to the global
  cell** (Racket-like, simplest); `sync-context` gives the strict case.

### 7.3 What it buys

- Multiple independent instances of a stateful library (two live DB contexts
  in the logic library).
- Test isolation without image forks.
- Per-embedding isolation on Android (two embedded scripts can't clobber each
  other's globals).
- The dev-loop primitive for a prototyping vehicle: one context per prototype
  run = a cheap image-fork substitute.
- The **dogfood loop**: the type solver keeps its constraint store in specials
  inside its own package, so parallel compilation is
  `(with-package-env *solver-ctx-for-unit-N* (infer-function f))` per worker
  thread. Contexts being fast forces special-variable access to be fast, which
  all ANSI code benefits from.

### 7.4 Scope

- **v1:** variable cells only.
- **v2 (open):** optional full namespaces (function/macro cells) for hostile
  embedded code.

---

## 8. Bootstrap: zero C

### 8.1 The constraint and its consequence

Nothing in the toolchain is C. Notably this closes two loopholes: copy-and-patch
stencils are normally generated with Clang (ours are hand-authored in the lap
DSL), and JNI's "C bridge" is just a calling convention, not a language. There
is strong precedent: Mezzano runs a full CL, GC included, on bare metal with no
C; SBCL's `make-host-1` / `genesis` / `make-target-2` is the proven
cross-compilation shape; SICL refined host/target environment separation.

### 8.2 Pipeline

```
SBCL host (pinned via Guix manifest)
  └─ xc cross-compiler + genesis image builder   (pure CL, ASDF-loadable)
       ├─ cold core: static ELF, x86-64, raw syscalls, no libc
       ├─ warm core: compiler loaded & run on target
       └─ self-host fixpoint: xc image ≡ self-compiled image
  └─ aarch64 backend → static .so, no DT_NEEDED, 16 KB PT_LOADs, W^X
       └─ NativeActivity APK, android:hasCode="false"
```

- **Stage 0 (host):** xc runs as an ordinary SBCL application — lap assembler
  DSL, EIR/LIR, object-layout constants, ELF writer (no `ld`; we emit bytes),
  and **genesis**, a host program that lays out the target boot image
  byte-for-byte. SICL-style first-class global environments keep target
  symbols from colliding with host CL.
- **Cold core:** memory regions + GC + threads (`clone`/`futex`; fs-base via
  `arch_prctl` on x86-64, `tpidr_el0` on aarch64) + a **small tree-walking
  evaluator** for the first REPL, all compiled by xc from restricted CL. Entry
  is `_start` doing raw `write`/`exit`. `strace` works regardless of libc —
  the early debugger.
- **Warm core:** on-target, load compiler sources, build T0/T1, dump a new
  image from within.
- **Fixpoint:** xc's image and the self-compiled image must agree — the
  reproducibility check that retires SBCL to historical-bootstrapper status.

### 8.3 Honest costs

- **Bignums are hand-rolled** (u64 vectors + carry intrinsics in lap; GMP is
  C, so out).
- **Float printing** needs a Grisu/Dragon port in CL (from the papers, per
  CLEANROOM).

### 8.4 The systems subset

The `Raw` subset — unboxed u64, `memref`, no-boxing arithmetic — is a
**first-class, user-visible backend target** ("CLEF/systems"), not an internal
hack, because the GC, lap-level primitives, and systems code in a prototyping
Lisp all depend on it. Given effects are user-facing, systems programming in
CLEF is a feature.

---

## 9. Platform support

### 9.1 Linux

Static ELF, x86-64, raw syscalls, no libc. The reference target.

### 9.2 Android

- **Shape:** the same compiler emitting a static `.so` with no `DT_NEEDED`,
  loaded by a `NativeActivity` APK with `android:hasCode="false"` — genuinely
  no Java/dex stub in our tree. The single entry point is an exported
  `ANativeActivity_onCreate` written in CL. JNI calls out are C-ABI
  trampolines generated by the JIT (no libffi).
- **API floor:** the next Android LTS-equivalent, taken as **API 30+**
  (Android 11). This gives `memfd_create` (API 30) for W^X dual mapping,
  mandatory 16 KB page support on new devices (API 31+), and stable SELinux
  `execmem` policy (API 29+). Targeting 30+ means testing against both 4 KB
  and 16 KB kernels early — desirable.
- **Reality checks (spike early, before the compiler depends on them):** W^X
  via dual mapping (`memfd_create` + separate RW/RX mappings), 16 KB `PT_LOAD`
  alignment (a Play requirement for native code), and `execmem` under the
  target SELinux policy. Fallbacks: ashmem dual-map or `mprotect` flipping.
- **Testing posture:** no real-device testing until later. The Android
  bring-up is a documented **ABI contract** (a header + README) satisfied by a
  thin platform shim; the CL side never calls NDK APIs directly. The shim is
  mocked on Linux for host-side testing. Guix's NDK packaging is incomplete —
  a known, tolerated gap with three fallback paths (Guix catches up; package
  the NDK as a Guix origin; use a foreign NDK for the APK step only). The
  compiler and GC never depend on the NDK — only final packaging does.
- **Delivery:** a `.so` + JNI bridge; ART never sees CLEF code. Tier-up
  threads expose a battery/thermal policy knob. Backends: x86-64 and aarch64
  only; keep the IR→machine layer thin.

---

## 10. Roadmap

CLEF is a prototyping vehicle, which reorders the roadmap: the REPL is the
product, FFI moves to the front, contexts are the dev-loop primitive, and
compliance is a ratchet, not a gate.

| Milestone | Content |
|-----------|---------|
| **M0a** | Host-side: lap assembler DSL, ELF writer, xc skeleton; **GC built & model-tested against a simulated heap on SBCL**; type-solver prototype answering `subtypep`; **reader written from ANSI §2** (clean-room, with source-location capture as a day-one goal). |
| **M0b** | Cold core on x86-64 Linux: raw-syscall hello → tree-walker REPL on the real concurrent GC. |
| **M1** | T0 copy-and-patch (lap-authored stencils), reader/eval, contexts, `ansi-tests` ratchet begins, debug tooling (symbol maps; own DWARF writer later). |
| **M2** | aarch64 backend, Android `.so` + NativeActivity REPL, 16 KB / W^X / `memfd_create` spikes closed (shim-mocked on Linux first), T1 + solver specialization. |
| **M3** | **Self-host fixpoint** (early, since the compiler has run on SBCL throughout); CLOS/MOP completeness; embedding SDK; the Brooks-pointer decision point with real pause data. |

### 10.1 FFI

Fully general: a signature-driven trampoline generator plus a JNI bridge
library. No app-specific drivers. FFI lands early because prototypes live and
die by calling platform APIs.

### 10.2 Compliance

Track `ansi-tests` pass rate as the public metric. Prioritize the prototyping
core (CLOS, conditions/restarts, reader/printer, `format` core); defer the
long tail (full pretty printer, exotic `loop` corners) without ever
architecting against it.

### 10.3 Observability

Allocation-profiling hooks in the TLAB path, trace hooks in T0, an
inspector-friendly object model. Prototypes need to be *seen* more than they
need to be fast.

---

## 11. Clean-room and licensing

CLEF is CC0 (public domain). The tree contains **zero non-PD source** and is
implemented clean-room from documents, never from other implementations'
source. The write-from-spec ledger:

| Component | Implemented from | Notes |
|-----------|------------------|-------|
| Reader | ANSI §2, §23.2 | Source-location capture built in. |
| CST / macro tracking | Folded into the reader | One component. |
| Printer | ANSI §22.3 | |
| `format` | ANSI §22.3 | Hardest single surface item; parallelizable. |
| Pretty printer | Waters' xp **paper** | Clean-room from the paper. |
| `loop` | ANSI §6 | Most fully specified macro in the standard. |
| CLOS/MOP | AMOP + ANSI §7 | PCL is reference only, never source. |
| Float printer | Ryū, Burger–Dybvig **papers** | |
| Bignums | Knuth vol. 2 | |
| GC | Immix, Yuasa SATB, G1 **papers** | |
| Type solver | Frisch–Castagna, HM(X) **papers** | |
| `ansi-tests` | — | **Public domain; vendored as the conformance baseline.** |

See CLEANROOM.md for the binding protocol.

### 11.1 Deferred risk: Unicode

ANSI requires only 96 characters, so ASCII-only case operations are conforming
and PD-trivial. Real Unicode case tables derive from the UCD, whose license is
attribution-required, not PD. **Deferred to post-ANSI**: `char-upcase` and
friends sit behind a case-table protocol so the swap is painless; the decision
(generated tables with a single documented non-PD input vs. a hand-rolled
limited mapping) is made later.

### 11.2 Deferred risk: timezones

tzdata is public domain, but CLEF's parser is written from the tzfile man
page, not copied.

---

## 12. Risks and open questions

### Settled but hard

1. **CLOS MOP performance** — method combination, slot access,
   redefinition/`change-class` layout migration. Mitigation: map deprecation +
   ICs (V8-style), decision-tree dispatch compiled by the solver.
2. **`eval`/`compile` at runtime** — fine because JIT-first, but
   macroexpansion environments must be correct.
3. **Bignums** — hand-rolled (§8.3).
4. **Full `format`/`loop`/reader/printer** — from spec; bounded but real work.
5. **Concurrent GC correctness** — mitigated by model-testing (§6.6) and
   effect-proven alloc-freedom (§6.5).

### Open

- **Contexts v2:** full namespaces (function/macro cells) for hostile embedded
  code. Scoped but not designed.
- **Unicode strategy:** deferred (§11.1).
- **Brooks-pointer upgrade:** decision point at M3 with pause data (§6.3).
- **Android NDK / Guix:** known gap, three fallback paths, not a blocker (§9.2).

---

## 13. Prior art

Clasp (CLOS/MOP on LLVM, MPS GC), SBCL (IR1/IR2 split, type derivation,
genesis bootstrap), Koka (evidence passing, named handlers), Effekt
(capability-passing CPS), LuaJIT (snapshots/deopt), V8 (ICs, map deprecation),
Flambda2, semantic subtyping (CDuce, and Elixir's gradual set-theoretic
types), Mezzano (all-CL bare metal), SICL (host/target environments), CPython
3.13 (copy-and-patch tiering), G1 / Shenandoah / ZGC (collectors), Immix
(size-class segregation), MMTk (GC toolkit — evaluated, not adopted:
concurrency is research-stage and it is Rust/C, violating zero-C).

---

*This document is the design. When the implementation and this document
disagree, one of them is wrong — fix it here first.*
