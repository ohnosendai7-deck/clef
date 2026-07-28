# CLEF-H conformance

Status of CLEF's hosted prototype (CLEF-H) against the ANSI Common Lisp
specification. This document tracks which areas are implemented and tested.

The conformance suite is `test/conformance.lisp` (run via `sh
tools/run-tests.sh`). Every feature listed as **supported** below has at least
one passing test there. The suite is the ratchet: a feature only counts as
supported once it has a green test.

## Legend

- **Supported** — implemented and tested in `test/conformance.lisp`.
- **Partial** — implemented for a useful subset; limitations noted.
- **Not yet** — on the roadmap (see the issue tracker).

## Coverage by area

| Area (chapter) | Status | Notes |
|---|---|---|
| Evaluation & compilation (3) | **Supported** | All 25 ANSI special operators, macroexpansion, closures, multiple values. Hosted evaluator (no compiler yet — that's the T0/T1 tiers on the roadmap). |
| Types & classes (4) | **Partial** | `typep`/`subtypep`/`type-of`/`coerce` for host types and CLOS classes; no full `deftype` yet. |
| Data & control flow (5) | **Supported** | `block`/`return-from`, `tagbody`/`go`, `catch`/`throw`, `unwind-protect`, `multiple-value-*`, `psetq`, `destructuring-bind`. |
| Iteration (6) | **Supported** | `dolist`, `dotimes`, `do`/`do*`, and an extended `loop` (for/in/on/across/from-to, collect/append/sum/count/max/min, when/unless/while/until/repeat/return). |
| Objects (7) | **Partial** | CLOS subset: `defclass` (slots, `:initform`/`:initarg`/`:accessor`/`:reader`/`:writer`), `defgeneric`/`defmethod` (class + eql specializers), standard method combination (before/after/around/primary, `call-next-method`, `next-method-p`), `make-instance`, `slot-value`, CPL linearization. No MOP, no `change-class`, no non-standard method combinations. |
| Structures (8) | **Partial** | `defstruct` minimal (constructor, predicate, accessors; `:initform` per slot). No `:type`, `:conc-name`, `:copier`, `:print-function`, or inheritance options. |
| Conditions (9) | **Supported** | `define-condition`, `error`/`cerror`/`warn`/`signal`, `handler-case`, `handler-bind`, `restart-case`, `ignore-errors`, `with-simple-restart`, `find-restart`/`invoke-restart`/`abort`/`continue`/`muffle-warning`, condition accessors. |
| Symbols (10) | **Supported** | `symbol-name`/`symbol-package`/`symbol-plist`/`boundp`/`makunbound`/`gensym`/`gentemp`/`intern`/`find-symbol`/`make-symbol`. |
| Packages (11) | **Partial** | `find-package`/`make-package`/`package-name`/`use-package`/`export`/`import`/`intern`/`find-symbol` etc. Package-local symbol interning is host-backed; the full ANSI package semantics are present but CLEF-H code runs mostly in CL-USER. |
| Numbers (12) | **Supported** | Full host numeric tower: arithmetic, comparison, `floor`/`ceiling`/`truncate`/`round`, `expt`/`sqrt`/`exp`/`log`, trig, `gcd`/`lcm`, bitwise ops, `random`, rationals/complex via host. |
| Characters (13) | **Supported** | `char`/`schar`, comparisons, `char-code`/`code-char`, case, `alpha-char-p`/`digit-char-p`/`alphanumericp`. ASCII case (see Unicode note below). |
| Conses (14) | **Supported** | Full cons API including all `c[ad]+r`, `rplaca`/`rplacd`, `mapcar`/`mapc`/`mapcan`/`mapl`/`maplist`, `assoc`/`member`/`subst`/`sublis`, `getf`, alists/plists. |
| Arrays (15) | **Supported** | `make-array`, `aref`/`svref`/`bit`/`sbit`, dimensions/rank/fill-pointer, `vector-push(-extend)`, `adjust-array`. |
| Strings (16) | **Supported** | `string`/comparison/case/trim, `make-string`, `parse-integer`. |
| Sequences (17) | **Supported** | `length`/`elt`/`subseq`/`copy-seq`/`fill`/`replace`/`count`/`find`/`position`/`remove`/`delete`/`substitute`/`remove-duplicates`/`mismatch`/`search`/`sort`/`stable-sort`/`merge`/`concatenate`/`map`/`map-into`/`reduce`/`every`/`some`/`notevery`/`notany`. |
| Hash tables (18) | **Supported** | `make-hash-table`/`gethash`/`remhash`/`clrhash`/`maphash`/count/test/sxhash. |
| Filenames & files (19, 20) | **Partial** | `open`/`close`/stream predicates, `probe-file`/`rename-file`/`delete-file`/`ensure-directories-exist`/`truename`, pathname accessors, `merge-pathnames`, `directory`. |
| Streams (21) | **Partial** | String streams, broadcast/echo/two-way/synonym/concatenated streams, file position/length. |
| Printer (22) | **Partial** | `print`/`prin1`/`princ`/`pprint`/`write`/`write-to-string`/`prin1-to-string`/`princ-to-string`, `format` (full host directives), printer control variables. Pretty-printing is host-delegated. |
| Reader (23) | **Not yet (host)** | CLEF-H reads via the host reader. A clean-room CLEF reader (with source locations) is milestone M0a/M1 on the roadmap. |
| System construction (24) | **Partial** | `load` (CLEF-H-aware), `compile-file` host-delegated. |
| Environment (25) | **Partial** | Time functions, `room`, `apropos`/`describe`/`documentation`/`disassemble`, `lisp-implementation-*`, `feature` introspection. |

## Notable gaps (roadmap)

- **A CLEF-native reader** with source-location tracking (currently host-read).
- **The native cold core** (raw-syscall runtime + GC + T0 JIT) — the lap/ELF
  foundation and zero-C smoke test already exist; see issues #1, #5.
- **Full CLOS/MOP** (issue #3), `deftype`, the pretty printer, the full `loop`
  clause vocabulary.
- **`format` is host-delegated**; a CLEF-native `format` (from the spec, per
  CLEANROOM.md) is a later milestone.
- **Unicode**: ASCII-only case operations for now (ANSI-conforming; see
  DESIGN.md §11.1). The Unicode case-table decision is deferred.

## The honest picture

CLEF-H implements the substantial majority of the ANSI **core language**:
evaluation, control, iteration, objects (a CLOS subset), conditions, and the
data-manipulation library (numbers, conses, sequences, strings, arrays, hash
tables) — all tested by 100+ conformance checks. What it is *not yet* is a
complete ANSI implementation: the reader, the native compiler/runtime, and the
MOP are the remaining major subsystems. Those are tracked on the issue board.
