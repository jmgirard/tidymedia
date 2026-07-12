# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Migrated from `project/` on 2026-07-11 (adopt-in-place); pre-cairn history in git log._
_Last hygiene check: 2026-07-12 (M13 done)_

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M13 | Batch video standardization verb | done | — | normal | milestones/archive/M13-batch-standardization.md |
| M12 | Video standardization verb | done | — | normal | milestones/archive/M12-standardization-presets.md |
| M11 | Tibble-driven batch frame extraction | done | — | normal | milestones/archive/M11-batch-frame-extraction.md |
| M09 | Dataframe-driven batch segmentation | done | — | normal | milestones/archive/M09-batch-segmentation.md |
| M10 | segment_videos() parity polish | done | — | normal | milestones/archive/M10-segment-videos-polish.md |

## Candidates
<!-- unnumbered ideas; one line each: idea — added YYYY-MM-DD — links -->
- Audio normalization verb: EBU R128 loudnorm (two-pass), downmix + resample, via a new Layer-1 audio-filter primitive (fills the engine's existing `filter_audio`/`-af` slot; no verb writes it yet). Distinct from D009-deferred `amix`. Needs EBU R128 / ITU-R BS.1770 primary source (no-oracle RB tripwire). — added 2026-07-10, split 2026-07-12 — research-verbs family 3
- Fixed-region anonymization verb (no face tracking): box fill via `ffm_drawbox`; region blur (split→crop→boxblur→overlay) needs an IP2 filtergraph design call. — added 2026-07-10, split 2026-07-12 — research-verbs family 4
- CRAN readiness (prep, hold submission until API soaks): API-surface cleanup (tidy-eval reexports, stray utils), win-builder + R-hub, examples/vignette policy pass, bump toward 0.2.0. Deliberately last. — added 2026-07-10
