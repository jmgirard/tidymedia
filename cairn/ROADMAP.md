# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Migrated from `project/` on 2026-07-11 (adopt-in-place); pre-cairn history in git log._
_Last hygiene check: 2026-07-12 (M19 done)_

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M20 | Fixed-region box-fill anonymization verb | planned | — | normal | milestones/M20-anonymize-video-box-fill.md |
| M21 | Batch fixed-region anonymization verb | planned | M20 | normal | milestones/M21-anonymize-videos-batch.md |
| M19 | Consistent schema for an all-silent two-pass batch | done | M18 | normal | milestones/archive/M19-all-silent-schema-consistency.md |
| M18 | Graceful silent-input handling for two-pass loudnorm | done | M16, M17 | normal | milestones/archive/M18-silent-input-loudnorm.md |
| M17 | Batch two-pass (measured/linear) loudnorm | done | M15, M16 | normal | milestones/archive/M17-batch-two-pass-loudnorm.md |
| M16 | Two-pass (measured/linear) EBU R128 loudnorm | done | M14 | normal | milestones/archive/M16-two-pass-loudnorm.md |
| M15 | Batch audio normalization verb | done | M14 | normal | milestones/archive/M15-batch-audio-normalization.md |

## Candidates
<!-- unnumbered ideas; one line each: idea — added YYYY-MM-DD — links -->
- Fixed-region *region blur* (no face tracking): split→crop→boxblur→overlay needs an IP2 filtergraph design call (new blessed composite verb vs Layer 0) plus a new `ffm_boxblur` filter; not plannable until that call. Box-fill half became M20/M21. — added 2026-07-10, split 2026-07-12 — research-verbs family 4
- CRAN readiness (prep, hold submission until API soaks): API-surface cleanup (tidy-eval reexports, stray utils), win-builder + R-hub, examples/vignette policy pass, bump toward 0.2.0. Deliberately last. — added 2026-07-10
