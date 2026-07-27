# M38: `hardware=` nvenc on `separate_audio_video` (+ batch)

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m38-separate-av-hardware`

## Goal

Give `separate_audio_video()` and `separate_audio_video_batch()` the opt-in
`hardware = "nvenc"` GPU toggle the other re-encode verbs carry, on the video
output alone.

## Scope

**In:** `hardware = c("none", "nvenc")` + `fallback = FALSE` on both verbs,
threaded through `separate_stream_pipeline()` (R/ffmpeg.R:302) into
`apply_video_codec()` (R/ffmpeg.R:1628) on the **video branch only** — nvenc
encodes video, so the audio branch never receives either argument. Because
`video_codec` defaults to `"copy"` here (D020), a guard aborts when a video
stream's resolved codec is `"copy"` and `hardware != "none"`, mirroring
`segment_pipeline()` (R/ffmpeg.R:1836) and enforcing D016's stream-copy rule.
Roxygen for both verbs, `has_nvenc()`/`nvenc_encoder()` back-pointers, NEWS.

**Out:** `hardware` as a per-row `jobs` column — it stays batch-wide per D016
(a machine property, not a file property), and a stray column is ignored like
any other unrecognized column. GPU *decode* / `-hwaccel` input acceleration,
other hardware backends (videotoolbox/qsv/vaapi/amf), and the quality /
rate-control knob each stay ROADMAP candidate rows.

## Acceptance criteria

- [ ] AC1 Both verbs accept `hardware = c("none", "nvenc")` and
      `fallback = FALSE`, documented in roxygen with `has_nvenc()` cross-references.
- [ ] AC2 With `hardware = "none"`, both verbs compile byte-identical commands
      to the pre-milestone default branch for the same call — verified by
      compiling both revisions side by side.
- [ ] AC3 `hardware = "nvenc"` where the video codec resolves to `"copy"`
      aborts naming both the cause and the fix — in the scalar, and per-row in
      the batch, including a jobs table mixing a copy row with a re-encode row.
      (RB tripwire: irreversible-api)
- [ ] AC4 `hardware = "nvenc"` with `video_codec = NULL` or an encoder name
      emits the nvenc encoder on the **video** command only; the audio command
      is byte-identical in every `hardware`/`fallback` combination.
- [ ] AC5 A `hardware` column in `jobs` is ignored (the batch-wide argument
      wins), documented in `@param jobs`.
- [ ] AC6 `devtools::test()` and `devtools::check()` clean (0 errors,
      0 warnings); `devtools::document()` produces no diff; NEWS entry present.

## Coverage

- AC1 → T3, T6
- AC2 → T1, T2, T3
- AC3 → T4
- AC4 → T1, T2, T3, T5
- AC5 → T1, T3
- AC6 → T6

## Tasks

- [x] T1 Tests first: extend `test-separate-av-codec.R` with the
      `hardware = "none"` byte-identical parity cases and the
      nvenc-on-video-only compile cases; extend
      `test-separate-audio-video-batch.R` with the inert-`hardware`-column
      case. Red until T2–T3.
- [x] T2 Thread `hardware`/`fallback` through `separate_stream_pipeline()`
      (R/ffmpeg.R:302) into `apply_video_codec()` on the video branch only.
      Edit `R/ffmpeg.R` as bytes to preserve its CRLF endings (M35 lesson).
- [x] T3 Add both arguments to `separate_audio_video()` (R/ffmpeg.R:352) and
      `separate_audio_video_batch()` (R/ffmpeg.R:3523); the batch captures them
      as scalars in its `ffm_batch` closure (R/ffmpeg.R:3643). T1 green.
- [x] T4 Tests then guard: the copy+nvenc abort in the scalar, per-row in the
      batch, and on a mixed copy/re-encode jobs table; then the guard itself in
      the video branch, mirroring `segment_pipeline()` (R/ffmpeg.R:1836).
- [ ] T5 Add the nvenc execution test gated on run-time usability — skip
      unless a 1-frame lavfi nvenc encode exits 0, never on the encoder merely
      being listed (M31 lesson).
- [ ] T6 Roxygen on both verbs + `has_nvenc()`/`nvenc_encoder()` back-pointers
      (M33 precedent); `devtools::document()`; NEWS entry; `devtools::check()`.

## Work log

- 2026-07-26: created by /milestone-plan.
- 2026-07-26: set in-progress; AC3's `irreversible-api` tripwire was offered and declined at the plan gate, so no implement gate — `ffm_batch` builds all pipelines before running any (R/ffm_batch.R:101), so the per-stream guard already fails before any encode.
- 2026-07-26: T1 tests written and committed red (8 new failures, 50 pre-existing green) — box stays unticked until T3 lands the arguments.
- 2026-07-26: T1-T3 done — `hardware`/`fallback` threaded through `separate_stream_pipeline()` and both verbs; `arg_match` at each front door (the unresolved default vector would otherwise fire T4's guard on every call). `devtools::test()` 1573 pass / 0 fail / 4 skip; CRLF preserved (diff 29/6, not whole-file).
- 2026-07-26: T4 done — copy+nvenc guard in `separate_stream_pipeline()`'s video branch; 4 guard tests red then green. `devtools::test()` 1583 pass / 0 fail / 4 skip.

## Decisions

## Review
