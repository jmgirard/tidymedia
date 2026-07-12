# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Migrated from `project/` on 2026-07-11 (adopt-in-place); pre-cairn history in git log._
_Last hygiene check: 2026-07-12 (M22 planned)_

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M22 | Naming & docs audit + target-scheme decisions | review | — | normal | milestones/M22-naming-docs-audit.md |
| M21 | Batch fixed-region anonymization verb | done | M20 | normal | milestones/archive/M21-anonymize-videos-batch.md |
| M20 | Fixed-region box-fill anonymization verb | done | — | normal | milestones/archive/M20-anonymize-video-box-fill.md |
| M19 | Consistent schema for an all-silent two-pass batch | done | M18 | normal | milestones/archive/M19-all-silent-schema-consistency.md |
| M18 | Graceful silent-input handling for two-pass loudnorm | done | M16, M17 | normal | milestones/archive/M18-silent-input-loudnorm.md |
| M17 | Batch two-pass (measured/linear) loudnorm | done | M15, M16 | normal | milestones/archive/M17-batch-two-pass-loudnorm.md |

## Candidates
<!-- unnumbered ideas; one line each: idea — added YYYY-MM-DD — links -->
- Fixed-region *region blur* (no face tracking): split→crop→boxblur→overlay needs an IP2 filtergraph design call (new blessed composite verb vs Layer 0) plus a new `ffm_boxblur` filter; not plannable until that call. Box-fill half became M20/M21. — added 2026-07-10, split 2026-07-12 — research-verbs family 4
- Apply M22 naming/docs recommendations: execute the approved clean-break renames + arg harmonization and the targeted docs gap-fill (examples/return/seealso). Not plannable until M22's audit is approved; carries RB tripwire irreversible-api. — added 2026-07-12 — depends on M22
- CRAN readiness (release mechanics only): win-builder + R-hub, cran-comments, bump toward 0.2.0. API-surface cleanup + examples/vignette pass moved under the M22 effort. Deliberately last. — added 2026-07-10, trimmed 2026-07-12 — see M22
