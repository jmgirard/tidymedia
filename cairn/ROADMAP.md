# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Migrated from `project/` on 2026-07-11 (adopt-in-place); pre-cairn history in git log._
_Last hygiene check: 2026-07-12 (M09 done)_

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M05 | Docs, vignettes, pkgdown, release prep | done | M03, M04 | — | milestones/archive/M05-docs-release.md |
| M06 | Engine hardening & safe execution | done | M02 | — | milestones/archive/M06-engine-hardening.md |
| M07 | Complete the blessed multi-input verbs | done | M06 | — | milestones/archive/M07-multi-input-verbs.md |
| M08 | Verification & provenance | done | M04, M06 | normal | milestones/archive/M08-verification-provenance.md |
| M09 | Dataframe-driven batch segmentation | done | — | normal | milestones/archive/M09-batch-segmentation.md |

## Candidates
<!-- unnumbered ideas; one line each: idea — added YYYY-MM-DD — links -->
- Research-workflow Layer-2 verbs: standardization presets, EBU R128 loudnorm/downmix/resample, tibble-driven batch frame extraction, fixed-region box/blur anonymization (no face tracking). Four families user-confirmed. — added 2026-07-10
- CRAN readiness (prep, hold submission until API soaks): API-surface cleanup (tidy-eval reexports, stray utils), win-builder + R-hub, examples/vignette policy pass, bump toward 0.2.0. Deliberately last. — added 2026-07-10
- `segment_videos()` auto-naming: derive `output` paths when the column is absent (per-input-file segment numbering across a multi-file table). Deferred from M09 (there, `output` is required). — added 2026-07-12 — links M09
- `segment_videos()` per-row `reencode`: accept a `reencode` column in the jobs table (per-segment fast-copy vs accurate cut) instead of the scalar arg. Deferred from M09. — added 2026-07-12 — links M09
- `segment_videos()` start/end type validation: reject non-numeric/character `start`/`end` columns up front (parity with `segment_video()`), avoiding opaque FFmpeg errors from `as.character()` coercion. M09 review finding F3. — added 2026-07-12 — links M09
