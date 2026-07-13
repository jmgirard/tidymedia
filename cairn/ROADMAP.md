# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Migrated from `project/` on 2026-07-11 (adopt-in-place); pre-cairn history in git log._
_Last hygiene check: 2026-07-13 (M23 done)_

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M23 | API surface cleanup (clean-break renames, arg harmonization, un-exports) | done | M22 | normal | milestones/archive/M23-api-surface-cleanup.md |
| M24 | Docs gap-fill (@seealso web, metadata-boundary prose, batch disambiguation) | planned | M23 | normal | milestones/M24-docs-gap-fill.md |
| M22 | Naming & docs audit + target-scheme decisions | done | — | normal | milestones/archive/M22-naming-docs-audit.md |
| M21 | Batch fixed-region anonymization verb | done | M20 | normal | milestones/archive/M21-anonymize-videos-batch.md |
| M20 | Fixed-region box-fill anonymization verb | done | — | normal | milestones/archive/M20-anonymize-video-box-fill.md |
| M19 | Consistent schema for an all-silent two-pass batch | done | M18 | normal | milestones/archive/M19-all-silent-schema-consistency.md |

## Candidates
<!-- unnumbered ideas; one line each: idea — added YYYY-MM-DD — links -->
- Fixed-region *region blur* (no face tracking): split→crop→boxblur→overlay needs an IP2 filtergraph design call (new blessed composite verb vs Layer 0) plus a new `ffm_boxblur` filter; not plannable until that call. Box-fill half became M20/M21. — added 2026-07-10, split 2026-07-12 — research-verbs family 4
- CRAN readiness (release mechanics only): win-builder + R-hub, cran-comments, bump toward 0.2.0. API-surface cleanup + examples/vignette pass moved under the M22 effort. Deliberately last. — added 2026-07-10, trimmed 2026-07-12 — see M22
