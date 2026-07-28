# Clean-Room Policy

CLEF is dedicated to the public domain under CC0. Keeping that dedication
legally meaningful requires that **every line in this repository be original
work**, implementable from public specifications and published research without
copying expression from any existing implementation.

This file is the project’s clean-room protocol. It is binding on all
contributions, human or machine.

## The rule

**Never read another Common Lisp implementation's source code while
implementing the same component for CLEF.**

"Another implementation" includes, without limitation: SBCL, CMUCL, ECL, CCL,
CLISP, ABCL, Clasp, SICL, Mezzano, Genera, Allegro, LispWorks, and any
portable-CL library (Eclector, Concrete-Syntax-Tree, Incless, PCL, portable
FORMAT, portable LOOP, etc.) that implements functionality CLEF needs.

If you have recently read such source for a component, do not implement that
component for CLEF until sufficient time has passed that you are working from
the specification, not from memory of their code. When in doubt, say so on the
issue and let someone else take that component.

## Permitted inputs

You **may** read, cite, and implement from:

- The ANSI Common Lisp standard and the Common Lisp HyperSpec (specification,
  not code).
- The Art of the Metaobject Protocol (AMOP) — as a protocol description.
- Academic papers: Immix, Yuasa's SATB barrier, copy-and-patch compilation,
  Ryū, Burger–Dybvig, Waters' xp pretty printer, Knuth vol. 2 arithmetic,
  Frisch–Castagna semantic subtyping, HM(X), etc. Algorithms described in
  papers are free; verbatim code listings from papers should be treated as
  illustrative, not copied.
- Public-domain test suites (e.g. `ansi-tests`), which may be vendored.
- Man pages, ABI documents, and OS/kernel documentation.

## Prohibited inputs

- Source code of any other CL implementation for a component you are writing.
- Patched or "inspired-by" copies of code under non-PD licenses, even with
  attribution. Attribution does not make BSD/MIT code public domain.
- Output of an AI tool that reproduces another implementation's code. AI
  assistance is permitted (see AGENTS.md), but the contributor is responsible
  for ensuring the result is original expression.

## Test-driven verification

Conformance is demonstrated against `ansi-tests` and CLEF's own test suite —
both of which are public domain — never by comparing source against another
implementation.

## Why

Copyright protects expression, not ideas. The ANSI standard, AMOP, and the
research literature give us every idea we need. The only way to keep CLEF
maximally clean is to source every expression from those documents, not from
other codebases. This policy is the cost of a genuinely public-domain Common
Lisp, and it is non-negotiable.

## Questions

Open an issue before starting a component if you are unsure whether your prior
exposure to an implementation's source is a problem. It is always cheaper to
ask first.
