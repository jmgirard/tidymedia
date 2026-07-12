# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Migrated from `project/` on 2026-07-11 (adopt-in-place); pre-cairn history in git log._
_Last hygiene check: 2026-07-12 (M16 planned)_

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M16 | Two-pass (measured/linear) EBU R128 loudnorm | planned | M14 | normal | milestones/M16-two-pass-loudnorm.md |
| M15 | Batch audio normalization verb | planned | M14 | normal | milestones/M15-batch-audio-normalization.md |
| M14 | Audio loudness normalization verb | done | — | normal | milestones/archive/M14-audio-loudness-normalization.md |
| M13 | Batch video standardization verb | done | — | normal | milestones/archive/M13-batch-standardization.md |
| M12 | Video standardization verb | done | — | normal | milestones/archive/M12-standardization-presets.md |
| M11 | Tibble-driven batch frame extraction | done | — | normal | milestones/archive/M11-batch-frame-extraction.md |
| M10 | segment_videos() parity polish | done | — | normal | milestones/archive/M10-segment-videos-polish.md |

## Candidates
<!-- unnumbered ideas; one line each: idea — added YYYY-MM-DD — links -->
- Batch two-pass loudnorm (`normalize_audios(two_pass = TRUE)`): fan the M16 analyze-then-build path across a jobs tibble via `ffm_batch()`, mirroring the M14→M15 scalar→batch split. — added 2026-07-12, split from M16 — depends on M15, M16; research-verbs family 3
- Fixed-region anonymization verb (no face tracking): box fill via `ffm_drawbox`; region blur (split→crop→boxblur→overlay) needs an IP2 filtergraph design call. — added 2026-07-10, split 2026-07-12 — research-verbs family 4
- CRAN readiness (prep, hold submission until API soaks): API-surface cleanup (tidy-eval reexports, stray utils), win-builder + R-hub, examples/vignette policy pass, bump toward 0.2.0. Deliberately last. — added 2026-07-10
