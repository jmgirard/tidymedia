# M10: CRAN readiness (prep, hold submission)

- **Status:** idea <!-- stub; full plan written when this milestone is reached -->
- **Created:** 2026-07-10

## Idea

Bring the package to submission-ready but **hold the actual CRAN submission**
until the API has soaked (user decision 2026-07-10). API surface cleanup
(exported tidy-eval reexports like `enquo`/`:=`, stray utils like
`pad_integers`/`convert_fractions`), win-builder + R-hub checks, policy pass
(examples/vignette runtimes without binaries), version bump toward 0.2.0.
Deliberately last so M07–M09 API additions land first.
