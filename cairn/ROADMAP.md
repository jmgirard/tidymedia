# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Migrated from `project/` on 2026-07-11 (adopt-in-place); pre-cairn history in git log._
_Last hygiene check: 2026-07-12 (M19 planned)_

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M19 | Consistent schema for an all-silent two-pass batch | in-progress | M18 | normal | milestones/M19-all-silent-schema-consistency.md |
| M18 | Graceful silent-input handling for two-pass loudnorm | done | M16, M17 | normal | milestones/archive/M18-silent-input-loudnorm.md |
| M17 | Batch two-pass (measured/linear) loudnorm | done | M15, M16 | normal | milestones/archive/M17-batch-two-pass-loudnorm.md |
| M16 | Two-pass (measured/linear) EBU R128 loudnorm | done | M14 | normal | milestones/archive/M16-two-pass-loudnorm.md |
| M15 | Batch audio normalization verb | done | M14 | normal | milestones/archive/M15-batch-audio-normalization.md |
| M14 | Audio loudness normalization verb | done | — | normal | milestones/archive/M14-audio-loudness-normalization.md |

## Candidates
<!-- unnumbered ideas; one line each: idea — added YYYY-MM-DD — links -->
- Fixed-region anonymization verb (no face tracking): box fill via `ffm_drawbox`; region blur (split→crop→boxblur→overlay) needs an IP2 filtergraph design call. — added 2026-07-10, split 2026-07-12 — research-verbs family 4
- CRAN readiness (prep, hold submission until API soaks): API-surface cleanup (tidy-eval reexports, stray utils), win-builder + R-hub, examples/vignette policy pass, bump toward 0.2.0. Deliberately last. — added 2026-07-10
