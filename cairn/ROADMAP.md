# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Migrated from `project/` on 2026-07-11 (adopt-in-place); pre-cairn history in git log._
_Last hygiene check: 2026-07-27 (M35 done — audio_codec on the four re-encode verbs merged, archived; M30 row aged out; D018 narrows D017 on the GP2 audio-cut trade)_

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M35 | `audio_codec` for the four re-encode verbs (crop/segment/compare/pip + batch) — stream-copy by default | done | — | normal | milestones/archive/M35-audio-codec-reencode-verbs.md |
| M33 | Wire `hardware=` nvenc into `anonymize_video` (+ batch) | done | — | normal | milestones/archive/M33-anonymize-hardware.md |
| M34 | `video_codec` + `hardware=` for the four codec-less re-encode verbs (crop/segment/compare/pip) | done | — | normal | milestones/archive/M34-codec-hardware-reencode-verbs.md |
| M32 | Batch siblings for the fan-in verbs (`concatenate_videos`/`compare_videos`/`picture_in_picture` `_batch`) | done | — | normal | milestones/archive/M32-batch-fan-in-verbs.md |
| M31 | NVIDIA nvenc hardware encoding (opt-in) | done | — | normal | milestones/archive/M31-nvenc-encoding.md |

## Candidates
<!-- unnumbered ideas; one line each: idea — added YYYY-MM-DD — links -->
- Fixed-region *region blur* (no face tracking): split→crop→boxblur→overlay needs an IP2 filtergraph design call (new blessed composite verb vs Layer 0) plus a new `ffm_boxblur` filter; not plannable until that call. Box-fill half became M20/M21. Confirmed in-scope (defer) by M25. — added 2026-07-10, split 2026-07-12, reconciled 2026-07-13 — research-verbs family 4; M25 survey §3 D1
- `burn_timecode` / drawtext text-and-timecode burn-in for coders & reliability raters; in-scope but needs a new `ffm_drawtext` Layer-1 filter + a surface-scope call. — added 2026-07-13 — M25 survey §3 D2 (defer)
- Minor in-scope convenience verbs (grouped): split multi-view→per-person clips, orientation fix (rotate/flip), contact-sheet QC montage; each needs a small arg-shape design call. — added 2026-07-13 — M25 survey §3 D3 (defer-low)
- Video quality / rate-control knob (CRF↔CQ, `-preset p1–p7`, bitrate) — the package has no quality abstraction today; a cross-encoder mapping is opinionated + an irreversible-API commitment. Deferred from M31. — added 2026-07-26 — M31 Q4
- GPU *decode* / `-hwaccel cuda` input acceleration + GPU filter pipelines — needs a new engine input-options slot (none exists; only `seek_pre` goes before `-i`) + an IP2 filtergraph design call. — added 2026-07-26 — M31 Out
- Other hardware encode backends (videotoolbox/qsv/vaapi/amf) generalizing the `hardware=` arg beyond nvenc — needs a backend-detection + arg-vocabulary design call. — added 2026-07-26 — M31 Out
- `separate_audio_video`'s `reencode = TRUE` path re-encodes audio to the container default (the `ffm_codec(audio = "copy")` sits inside `if (!reencode)`) — needs its own arg-shape call, since `reencode` already *is* that verb's copy-vs-encode switch. Split out of M35. — added 2026-07-26 — M35 Out; D017
- `normalize_audio` always re-encodes (it filters audio) but offers no way to name the output encoder — an `audio_codec` addition on a verb where copy is impossible, so D017's default does not transfer. Split out of M35. — added 2026-07-26 — M35 Out; D017
- CRAN readiness (release mechanics only): win-builder + R-hub, cran-comments, bump toward 0.2.0. Deliberately last. API-surface cleanup shipped as M23; the vignette pass is M30; a roxygen `@examples` pass remains the only open docs slice under this row. — added 2026-07-10, trimmed 2026-07-12, reconciled 2026-07-12 (M30) — see M22/M23/M30
