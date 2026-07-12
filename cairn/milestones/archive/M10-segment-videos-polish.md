# M10: segment_videos() parity polish — done 2026-07-12

**Goal:** bring `segment_videos()` to parity with `segment_video()`.

**Outcome:** three additive, backward-compatible refinements (PR #12):
- Optional `output` — when absent, names derived per input file via a new
  shared `derive_segment_names()` helper (`<basename>_<n>.<ext>`, numbering
  restarting per input), now backing `segment_video()`'s naming too (byte-
  identical for the single-input case, confirmed by two reviewers).
- Per-row `reencode` column overrides the scalar arg (resolved in the batch
  closure from `purrr::pmap`'s name-matched `...`).
- Up-front validation: non-numeric/character `start`/`end`, and a
  non-logical-or-`NA` `reencode` column, abort naming the column before FFmpeg.

Absorbed the three M09 deferral candidates. 9 new `run = FALSE` tests;
`devtools::check()` zero errors/warnings/notes; all CI green.

**Key decisions:** `reencode` column wins over the scalar arg per row (scalar =
fallback); not an irreversible-API change (D-local, not promoted).

**Review:** no blame-history findings; diff-bug F1 (score 85) fixed in review —
`reencode` check used bare `is.logical()` (admits `NA`); tightened to reject
`NA` and name the column, with a regression test. PR #12.
