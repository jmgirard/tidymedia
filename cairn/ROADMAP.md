# Roadmap

_The only authority on milestone status. Grouped by status, not ID._
_Migrated from `project/` on 2026-07-11 (adopt-in-place); pre-cairn history in git log._
_Last hygiene check: 2026-07-30 (post-merge pass for M42: archived, ROADMAP row done, two lessons captured, no lesson retired — M42's precedence test does not make M41's judgment-lesson fail. One candidate added from a logged review finding. M43 is now the only workable planned milestone.)_

## Milestones

| ID | Title | Status | Depends on | Priority | File/Archive |
|---|---|---|---|---|---|
| M41 | Front-door validation parity for the codec arguments | done | — | normal | milestones/archive/M41-codec-arg-front-door-guards.md |
| M42 | What `NULL` and column `NA` mean, settled across the codec family | done | M41 | normal | milestones/archive/M42-codec-null-na-semantics.md |
| M43 | Pick which audio track the extraction verbs take (`audio_stream`) | in-progress | M41, M42 | normal | milestones/M43-audio-stream-selection.md |
| M44 | Say something when audio tracks are dropped | planned | M43 | normal | milestones/M44-implicit-track-drop-warning.md |
| M39 | `audio_codec` for `standardize_video` and `anonymize_video` (+ batch) | done | — | normal | milestones/archive/M39-standardize-anonymize-audio-codec.md |
| M40 | `audio_codec` subsumes `format` on `convert_audio` (+ batch), closing the codec sweep | done | M39 | normal | milestones/archive/M40-convert-audio-codec-arg.md |
| M38 | `hardware=` nvenc on `separate_audio_video` (+ batch) | done | — | normal | milestones/archive/M38-separate-av-hardware.md |

## Candidates
<!-- unnumbered ideas; one line each: idea — added YYYY-MM-DD — links -->
- Fixed-region *region blur* (no face tracking): split→crop→boxblur→overlay needs an IP2 filtergraph design call (new blessed composite verb vs Layer 0) plus a new `ffm_boxblur` filter; not plannable until that call. Box-fill half became M20/M21. Confirmed in-scope (defer) by M25. — added 2026-07-10, split 2026-07-12, reconciled 2026-07-13 — research-verbs family 4; M25 survey §3 D1
- `burn_timecode` / drawtext text-and-timecode burn-in for coders & reliability raters; in-scope but needs a new `ffm_drawtext` Layer-1 filter + a surface-scope call. — added 2026-07-13 — M25 survey §3 D2 (defer)
- Minor in-scope convenience verbs (grouped): split multi-view→per-person clips, orientation fix (rotate/flip), contact-sheet QC montage; each needs a small arg-shape design call. — added 2026-07-13 — M25 survey §3 D3 (defer-low)
- ~~A scalar `audio_codec = NA` on a `_batch` verb silently compiles the default~~ — promoted to M41 (2026-07-29, corrected there): probes found only `normalize_audio_batch` affected, not the three verbs claimed; `standardize_video_batch` and `anonymize_video_batch` already abort via M39's `check_string()`. M41 also carries seven verb/argument pairs whose abort leaks Layer-1 names or fires mid-fan-out. — added 2026-07-26, promoted 2026-07-29 — M41
- Carry `audio_stream` to the verbs that pass audio *through* (`separate_audio_video`, `standardize_video`, `crop_video`, `segment_video`, `anonymize_video`), and reconcile it with the 0-based `audio =` *input* index on `compare_videos`/`picture_in_picture` (D009) — one argument or two. Plannable once M43 sets the argument's shape; promote if a caller needs to choose a track on any verb other than the two extraction verbs. — added 2026-07-29 — M43 Out; D009
- `separate_audio_video()` aborts on a multi-track input when the audio output is a single-stream container: it maps `0:a` (every audio stream), so a 3-track file to `.aac` exits 65514 with a zero-byte file, while `.mka` succeeds carrying all 3 (measured 2026-07-29). Unlike the `convert_audio` hotfix this is NOT "take the first track" — keeping every track is plausibly this verb's intent, so the call is between aborting at the front door with a container/stream-count message, selecting a track, and documenting the limit. Needs that design call; promote when a caller hits it or when M43 sets `audio_stream`'s shape, whichever comes first. — added 2026-07-29 — found by the convert_audio `-map a` hotfix (PR #44); see the `audio_stream`-carry row (selection, not this crash) and M44 (drop warning)
- A bad codec *token* still leaks Layer-1's argument name on the verbs whose pipelines call `ffm_codec()` directly instead of the `apply_video_codec()` / `apply_audio_codec()` seams: `extract_audio(audio_codec = "a c")` reports "`audio` must be a single clean token" blaming `ffm_codec()`, where `anonymize_video` names `video_codec` and blames the verb (measured 2026-07-30). M41 fixed this class for non-string *values* only; tokens were never probed. Routing `extract_audio_pipeline()`, `standardize_pipeline()` and `anonymize_pipeline()` through the two seams closes it and would also make D022's bullet 1 true, which names those seams as carrying the family rule though none of the three uses them. — added 2026-07-30 — M42 review F5 (scored 76, logged); M41 archive; D016/D022
- Video quality / rate-control knob (CRF↔CQ, `-preset p1–p7`, bitrate) — the package has no quality abstraction today; a cross-encoder mapping is opinionated + an irreversible-API commitment. Deferred from M31. — added 2026-07-26 — M31 Q4
- GPU *decode* / `-hwaccel cuda` input acceleration + GPU filter pipelines — needs a new engine input-options slot (none exists; only `seek_pre` goes before `-i`) + an IP2 filtergraph design call. — added 2026-07-26 — M31 Out
- Other hardware encode backends (videotoolbox/qsv/vaapi/amf) generalizing the `hardware=` arg beyond nvenc — needs a backend-detection + arg-vocabulary design call. — added 2026-07-26 — M31 Out
- CRAN readiness (release mechanics only): win-builder + R-hub, cran-comments, bump toward 0.2.0. Deliberately last. API-surface cleanup shipped as M23; the vignette pass is M30; a roxygen `@examples` pass remains the only open docs slice under this row. — added 2026-07-10, trimmed 2026-07-12, reconciled 2026-07-12 (M30) — see M22/M23/M30
