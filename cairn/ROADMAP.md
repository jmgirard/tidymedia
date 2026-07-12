# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Migrated from `project/` on 2026-07-11 (adopt-in-place); pre-cairn history in git log._
_Last hygiene check: 2026-07-12 (M18 review)_

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M18 | Graceful silent-input handling for two-pass loudnorm | review | M16, M17 | normal | milestones/M18-silent-input-loudnorm.md |
| M17 | Batch two-pass (measured/linear) loudnorm | done | M15, M16 | normal | milestones/archive/M17-batch-two-pass-loudnorm.md |
| M16 | Two-pass (measured/linear) EBU R128 loudnorm | done | M14 | normal | milestones/archive/M16-two-pass-loudnorm.md |
| M15 | Batch audio normalization verb | done | M14 | normal | milestones/archive/M15-batch-audio-normalization.md |
| M14 | Audio loudness normalization verb | done | — | normal | milestones/archive/M14-audio-loudness-normalization.md |
| M13 | Batch video standardization verb | done | — | normal | milestones/archive/M13-batch-standardization.md |

## Candidates
<!-- unnumbered ideas; one line each: idea — added YYYY-MM-DD — links -->
- All-silent two-pass batch omits opt-in columns/attrs: `normalize_audios(two_pass=TRUE)` where *every* row is silent returns no `verified` column under `verify=` (a mixed batch includes it) and no manifest attribute under `manifest=` — a schema inconsistency across calls. — added 2026-07-12, from M18 review (scored 78)
- Fixed-region anonymization verb (no face tracking): box fill via `ffm_drawbox`; region blur (split→crop→boxblur→overlay) needs an IP2 filtergraph design call. — added 2026-07-10, split 2026-07-12 — research-verbs family 4
- CRAN readiness (prep, hold submission until API soaks): API-surface cleanup (tidy-eval reexports, stray utils), win-builder + R-hub, examples/vignette policy pass, bump toward 0.2.0. Deliberately last. — added 2026-07-10
