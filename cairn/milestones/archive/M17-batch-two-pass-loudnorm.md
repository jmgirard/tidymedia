# M17: Batch two-pass (measured/linear) loudnorm

- **Status:** done · **Depends on:** M15, M16 · **PR:** https://github.com/jmgirard/tidymedia/pull/19

## Goal

Add accurate two-pass loudnorm to `normalize_audios()` via `two_pass = TRUE`,
fanning M16's analyze-then-build path across a jobs tibble.

## Outcome

Shipped `normalize_audios(two_pass = TRUE)`. New `R/loudnorm_two_pass.R`
internals: `run_loudnorm_analysis_batch()` (Phase 1 analysis per row, honoring
`parallel` + per-row targets), `assemble_measured()` (M16's parser → five
`measured_I/TP/LRA/thresh` + `offset` columns; fail-fast naming any row that
exits non-zero or yields no parseable block), `run_normalize_correction()`
(Phase 2 fan-out over `ffm_batch()`/`normalize_audio_pipeline()`, `linear=true`).
Single-pass path untouched (pinned byte-for-byte). `run = FALSE` runs Phase 1,
returns correction commands unexecuted (D013). Each output within 0.01–0.02 LU of
target; `check()` 0/0/0; 704 tests pass. Review (2 lenses + scorer): 3 findings
≥80, all fixed — up-front `channels`/`sample_rate` validation before Phase 1
(F1, +2 tests), `check_installed("furrr")` guard in Phase 1 (F2), AC5 skip on
ffmpeg (F3). Milestone-local: measured cols use FFmpeg-arg spellings; `two_pass`
is a whole-table switch; no new D-entry (D013 covers it).
