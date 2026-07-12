# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Migrated from `project/` on 2026-07-11 (adopt-in-place); pre-cairn history in git log._
_Last hygiene check: 2026-07-12 (M11–M12 planned)_

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M11 | Tibble-driven batch frame extraction | review | — | normal | milestones/M11-batch-frame-extraction.md |
| M12 | Video standardization verb | planned | — | normal | milestones/M12-standardization-presets.md |
| M06 | Engine hardening & safe execution | done | M02 | — | milestones/archive/M06-engine-hardening.md |
| M07 | Complete the blessed multi-input verbs | done | M06 | — | milestones/archive/M07-multi-input-verbs.md |
| M08 | Verification & provenance | done | M04, M06 | normal | milestones/archive/M08-verification-provenance.md |
| M09 | Dataframe-driven batch segmentation | done | — | normal | milestones/archive/M09-batch-segmentation.md |
| M10 | segment_videos() parity polish | done | — | normal | milestones/archive/M10-segment-videos-polish.md |

## Candidates
<!-- unnumbered ideas; one line each: idea — added YYYY-MM-DD — links -->
- Audio normalization verb: EBU R128 loudnorm (two-pass), downmix + resample, via a new Layer-1 audio-filter primitive (fills the engine's existing `filter_audio`/`-af` slot; no verb writes it yet). Distinct from D009-deferred `amix`. Needs EBU R128 / ITU-R BS.1770 primary source (no-oracle RB tripwire). — added 2026-07-10, split 2026-07-12 — research-verbs family 3
- Fixed-region anonymization verb (no face tracking): box fill via `ffm_drawbox`; region blur (split→crop→boxblur→overlay) needs an IP2 filtergraph design call. — added 2026-07-10, split 2026-07-12 — research-verbs family 4
- CRAN readiness (prep, hold submission until API soaks): API-surface cleanup (tidy-eval reexports, stray utils), win-builder + R-hub, examples/vignette policy pass, bump toward 0.2.0. Deliberately last. — added 2026-07-10
<!-- The three segment_videos() deferrals (auto-naming, per-row reencode, start/end validation) were absorbed into M10 on 2026-07-12. -->
