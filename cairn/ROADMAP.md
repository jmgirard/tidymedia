# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Migrated from `project/` on 2026-07-11 (adopt-in-place); pre-cairn history in git log._
_Last hygiene check: 2026-07-12 (M10 done)_

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M06 | Engine hardening & safe execution | done | M02 | — | milestones/archive/M06-engine-hardening.md |
| M07 | Complete the blessed multi-input verbs | done | M06 | — | milestones/archive/M07-multi-input-verbs.md |
| M08 | Verification & provenance | done | M04, M06 | normal | milestones/archive/M08-verification-provenance.md |
| M09 | Dataframe-driven batch segmentation | done | — | normal | milestones/archive/M09-batch-segmentation.md |
| M10 | segment_videos() parity polish | done | — | normal | milestones/archive/M10-segment-videos-polish.md |

## Candidates
<!-- unnumbered ideas; one line each: idea — added YYYY-MM-DD — links -->
- Research-workflow Layer-2 verbs: standardization presets, EBU R128 loudnorm/downmix/resample, tibble-driven batch frame extraction, fixed-region box/blur anonymization (no face tracking). Four families user-confirmed. — added 2026-07-10
- CRAN readiness (prep, hold submission until API soaks): API-surface cleanup (tidy-eval reexports, stray utils), win-builder + R-hub, examples/vignette policy pass, bump toward 0.2.0. Deliberately last. — added 2026-07-10
<!-- The three segment_videos() deferrals (auto-naming, per-row reencode, start/end validation) were absorbed into M10 on 2026-07-12. -->
