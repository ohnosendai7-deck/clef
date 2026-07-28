# Contributing to CLEF

Thank you for your interest. CLEF is pre-M0 and design-led; the most valuable
contributions right now are design review, specification analysis, and
correctness arguments.

## Before anything else

Read [CLEANROOM.md](CLEANROOM.md). It is binding. CLEF is public domain and
stays that way only if every contribution is original expression. If you have
recently read the source of another CL implementation for a component you want
to work on, tell us — we will route you to a different component.

## License

By contributing, you agree that your contributions are dedicated to the public
domain under [CC0 1.0 Universal](LICENSE). There is no contributor license
agreement and no copyright assignment; CC0 is the only term. If you cannot
dedicate your contribution to the public domain, do not contribute it.

## AI assistance

AI-assisted contributions are welcome. You are responsible for:

1. Ensuring the output is original expression (per CLEANROOM.md).
2. Understanding every line you submit — no "the model wrote it" defense.
3. Saying so in the commit message or PR if the contribution is substantially
   machine-generated.

## How to contribute

- **Design discussion** — open an issue. Reference docs/DESIGN.md sections.
- **Code** — fork, branch, PR. Keep commits focused. Follow the conventions in
  AGENTS.md.
- **Tests** — every component needs conformance tests against the ANSI spec.
  `ansi-tests` is the baseline; component-specific tests live alongside the
  component.

## Code of conduct

Be direct, be technical, be kind. Disagreement about design is the point of a
design-led project; personal attacks are not tolerated.

## Questions

Open an issue.
