# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Migrated from `project/` on 2026-07-11 (adopt-in-place); pre-cairn history in git log._
_Last hygiene check: 2026-07-13 (M26 planned)_

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M26 | Fixed-rate frame sampling (`sample_frames` + `_batch`) | review | — | high | milestones/M26-sample-frames.md |
| M25 | Verb coverage survey (research-domain gap analysis) | done | — | normal | milestones/archive/M25-verb-coverage-survey.md |
| M23 | API surface cleanup (clean-break renames, arg harmonization, un-exports) | done | M22 | normal | milestones/archive/M23-api-surface-cleanup.md |
| M24 | Docs gap-fill (@seealso web, metadata-boundary prose, batch disambiguation) | done | M23 | normal | milestones/archive/M24-docs-gap-fill.md |
| M22 | Naming & docs audit + target-scheme decisions | done | — | normal | milestones/archive/M22-naming-docs-audit.md |
| M21 | Batch fixed-region anonymization verb | done | M20 | normal | milestones/archive/M21-anonymize-videos-batch.md |

## Candidates
<!-- unnumbered ideas; one line each: idea — added YYYY-MM-DD — links -->
- `strip_metadata` — de-identification: remove container/stream metadata tags (device/GPS/identity) via stream-copy `-map_metadata -1`; core IRB need. — added 2026-07-13 — M25 survey §3 K2 (high)
- Fixed-region *region blur* (no face tracking): split→crop→boxblur→overlay needs an IP2 filtergraph design call (new blessed composite verb vs Layer 0) plus a new `ffm_boxblur` filter; not plannable until that call. Box-fill half became M20/M21. Confirmed in-scope (defer) by M25. — added 2026-07-10, split 2026-07-12, reconciled 2026-07-13 — research-verbs family 4; M25 survey §3 D1
- `burn_timecode` / drawtext text-and-timecode burn-in for coders & reliability raters; in-scope but needs a new `ffm_drawtext` Layer-1 filter + a surface-scope call. — added 2026-07-13 — M25 survey §3 D2 (defer)
- Minor in-scope convenience verbs (grouped): split multi-view→per-person clips, orientation fix (rotate/flip), contact-sheet QC montage; each needs a small arg-shape design call. — added 2026-07-13 — M25 survey §3 D3 (defer-low)
- CRAN readiness (release mechanics only): win-builder + R-hub, cran-comments, bump toward 0.2.0. API-surface cleanup + examples/vignette pass moved under the M22 effort. Deliberately last. — added 2026-07-10, trimmed 2026-07-12 — see M22
