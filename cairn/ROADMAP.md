# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Migrated from `project/` on 2026-07-11 (adopt-in-place); pre-cairn history in git log._
_Last hygiene check: 2026-07-13 (M27 done)_

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M28 | Batch siblings for single-in/single-out verbs (`extract_audio`/`convert_audio`/`crop_video`/`format_for_web` `_batch`) | in-progress | — | high | milestones/M28-batch-single-io-verbs.md |
| M29 | Batch sibling for `separate_audio_video` (fan-out) | planned | M28 | normal | milestones/M29-batch-separate-audio-video.md |
| M27 | Metadata scrubbing for de-identification (`strip_metadata` + `_batch`) | done | — | high | milestones/archive/M27-strip-metadata.md |
| M26 | Fixed-rate frame sampling (`sample_frames` + `_batch`) | done | — | high | milestones/archive/M26-sample-frames.md |
| M25 | Verb coverage survey (research-domain gap analysis) | done | — | normal | milestones/archive/M25-verb-coverage-survey.md |
| M23 | API surface cleanup (clean-break renames, arg harmonization, un-exports) | done | M22 | normal | milestones/archive/M23-api-surface-cleanup.md |
| M24 | Docs gap-fill (@seealso web, metadata-boundary prose, batch disambiguation) | done | M23 | normal | milestones/archive/M24-docs-gap-fill.md |

## Candidates
<!-- unnumbered ideas; one line each: idea — added YYYY-MM-DD — links -->
- Fixed-region *region blur* (no face tracking): split→crop→boxblur→overlay needs an IP2 filtergraph design call (new blessed composite verb vs Layer 0) plus a new `ffm_boxblur` filter; not plannable until that call. Box-fill half became M20/M21. Confirmed in-scope (defer) by M25. — added 2026-07-10, split 2026-07-12, reconciled 2026-07-13 — research-verbs family 4; M25 survey §3 D1
- `burn_timecode` / drawtext text-and-timecode burn-in for coders & reliability raters; in-scope but needs a new `ffm_drawtext` Layer-1 filter + a surface-scope call. — added 2026-07-13 — M25 survey §3 D2 (defer)
- Minor in-scope convenience verbs (grouped): split multi-view→per-person clips, orientation fix (rotate/flip), contact-sheet QC montage; each needs a small arg-shape design call. — added 2026-07-13 — M25 survey §3 D3 (defer-low)
- Batch siblings for the composite/fan-in verbs (`concatenate_videos`, `compare_videos`, `picture_in_picture`): each takes many/multiple inputs per output, which the D007 pmap-over-columns jobs-tibble doesn't cover — needs an input-shape design call (per-row list-column of inputs vs. fixed input columns) before planning. — added 2026-07-12 — batch-coverage gap analysis Tier 3; single-in/out half is M28, fan-out half is M29
- CRAN readiness (release mechanics only): win-builder + R-hub, cran-comments, bump toward 0.2.0. API-surface cleanup + examples/vignette pass moved under the M22 effort. Deliberately last. — added 2026-07-10, trimmed 2026-07-12 — see M22
