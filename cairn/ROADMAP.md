# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Migrated from `project/` on 2026-07-11 (adopt-in-place); pre-cairn history in git log._
_Last hygiene check: 2026-07-12_

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M01 | Infrastructure modernization (testthat, CI, cli) | done | — | — | milestones/archive/M01-infrastructure.md |
| M02 | ffm builder engine rework (real command model) | done | M01 | — | milestones/archive/M02-ffm-engine.md |
| M03 | Task verbs rebuilt on the builder + batch support | done | M02 | — | milestones/archive/M03-task-verbs.md |
| M04 | Metadata layer polish (mediainfo/ffprobe tibbles) | done | M02 | — | milestones/archive/M04-metadata-layer.md |
| M05 | Docs, vignettes, pkgdown, release prep | done | M03, M04 | — | milestones/archive/M05-docs-release.md |
| M06 | Engine hardening & safe execution | done | M02 | — | milestones/archive/M06-engine-hardening.md |
| M07 | Complete the blessed multi-input verbs | done | M06 | — | milestones/archive/M07-multi-input-verbs.md |
| M08 | Verification & provenance | done | M04, M06 | normal | milestones/archive/M08-verification-provenance.md |
| M09 | Dataframe-driven batch segmentation | in-progress | — | normal | milestones/M09-batch-segmentation.md |

## Candidates
<!-- unnumbered ideas; one line each: idea — added YYYY-MM-DD — links -->
- Research-workflow Layer-2 verbs: standardization presets, EBU R128 loudnorm/downmix/resample, tibble-driven batch frame extraction, fixed-region box/blur anonymization (no face tracking). Four families user-confirmed. — added 2026-07-10
- CRAN readiness (prep, hold submission until API soaks): API-surface cleanup (tidy-eval reexports, stray utils), win-builder + R-hub, examples/vignette policy pass, bump toward 0.2.0. Deliberately last. — added 2026-07-10
- `segment_videos()` auto-naming: derive `output` paths when the column is absent (per-input-file segment numbering across a multi-file table). Deferred from M09 (there, `output` is required). — added 2026-07-12 — links M09
- `segment_videos()` per-row `reencode`: accept a `reencode` column in the jobs table (per-segment fast-copy vs accurate cut) instead of the scalar arg. Deferred from M09. — added 2026-07-12 — links M09
