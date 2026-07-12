# Lessons

_Durable, append-only repo lessons (build quirks, testing tricks) — captured at
milestone end, surfaced at plan time. Capped at 50 lines (D-015); prune the
least-useful when full. Not status, not decisions (a choice is a D-entry)._

- 2026-07-12 (M09): `cairn_validate.py`'s ISO-date check false-positives on the
  `devtools::check()` result shorthand written as three slash-joined numbers
  (it reads them as `M/D/Y`). Several archive summaries (M02–M08) trip it; it's
  benign. In new tracking files spell it "zero errors/warnings/notes" instead.
- 2026-07-12 (M09): a thin Layer-2 wrapper can forward `...` to `ffm_batch()`
  safely — its `verify`/`manifest`/`checksums`/`progress` params sit after `...`
  in the signature, so they bind by name and never leak into the per-row `.f`.
- 2026-07-12 (M10): `is.logical(x)` accepts vectors containing `NA`, so a
  bare `is.logical()` type-guard admits `NA` where `rlang::check_bool()`
  (scalar path) rejects it — pair it with `!anyNA()` when validating a logical
  column, or the `NA` leaks downstream to an internal check with a worse error.
