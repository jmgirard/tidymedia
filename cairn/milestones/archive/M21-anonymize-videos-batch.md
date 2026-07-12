# M21: Batch fixed-region anonymization verb

- **Status:** done · **PR:** https://github.com/jmgirard/tidymedia/pull/23
- **Depends on:** M20 · **Principles:** IP1, IP2

## Goal
Add `anonymize_videos(jobs, …)`, the batch sibling of `anonymize_video()`,
box-filling many videos over `ffm_batch` — one reproducible command per input.

## Outcome
Shipped `anonymize_videos()`: per-input `regions` as a required list-column,
optional per-row `output`/`color`/`vcodec`/`pixel_format` override columns,
auto-named `_anonymized.<ext>` outputs with a duplicate-input collision guard.
Thin Layer-2 fan-out over `ffm_batch` (D007) reusing `anonymize_pipeline()`, so
per-region validation and encode guards (even-dim floor, audio copy) are
inherited (M13). `verify`/`manifest`/`checksums`/parallel flow through `...`.
26 tests; full suite 870 pass; `check()` 0/0/0. Three-lens independent review
clean; CI green on all 7 jobs.

## Key decisions
- T1 resolved: per-input regions is a **required list-column**; `ffm_batch`'s
  `purrr::pmap` unwraps each cell's data frame by name, and purrr's
  `In index: N` annotation gives per-row error reporting for free.
- Per-row `color`/`vcodec`/`pixel_format` override columns (user-confirmed at
  the implement gate) — parity with `standardize_videos()`'s `pick()` pattern.
