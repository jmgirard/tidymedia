# M38: `hardware=` nvenc on `separate_audio_video` (+ batch)

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** `m38-separate-av-hardware` / https://github.com/jmgirard/tidymedia/pull/40

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

- [x] AC1 Both verbs accept `hardware = c("none", "nvenc")` and
      `fallback = FALSE`, documented in roxygen with `has_nvenc()` cross-references.
- [x] AC2 With `hardware = "none"`, both verbs compile byte-identical commands
      to the pre-milestone default branch for the same call — verified by
      compiling both revisions side by side.
- [x] AC3 `hardware = "nvenc"` where the video codec resolves to `"copy"`
      aborts naming both the cause and the fix — in the scalar, and per-row in
      the batch, including a jobs table mixing a copy row with a re-encode row.
      (RB tripwire: irreversible-api)
- [x] AC4 `hardware = "nvenc"` with `video_codec = NULL` or an encoder name
      emits the nvenc encoder on the **video** command only; the audio command
      is byte-identical in every `hardware`/`fallback` combination.
- [x] AC5 A `hardware` column in `jobs` is ignored (the batch-wide argument
      wins), documented in `@param jobs`.
- [x] AC6 `devtools::test()` and `devtools::check()` clean (0 errors,
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
- [x] T5 Add the nvenc execution test gated on run-time usability — skip
      unless a 1-frame lavfi nvenc encode exits 0, never on the encoder merely
      being listed (M31 lesson).
- [x] T6 Roxygen on both verbs + `has_nvenc()`/`nvenc_encoder()` back-pointers
      (M33 precedent); `devtools::document()`; NEWS entry; `devtools::check()`.

## Work log

- 2026-07-26: created by /milestone-plan.
- 2026-07-26: set in-progress; AC3's `irreversible-api` tripwire was offered and declined at the plan gate, so no implement gate — `ffm_batch` builds all pipelines before running any (R/ffm_batch.R:101), so the per-stream guard already fails before any encode.
- 2026-07-26: T1 tests written and committed red (8 new failures, 50 pre-existing green) — box stays unticked until T3 lands the arguments.
- 2026-07-26: T1-T3 done — `hardware`/`fallback` threaded through `separate_stream_pipeline()` and both verbs; `arg_match` at each front door (the unresolved default vector would otherwise fire T4's guard on every call). `devtools::test()` 1573 pass / 0 fail / 4 skip; CRLF preserved (diff 29/6, not whole-file).
- 2026-07-26: T4 done — copy+nvenc guard in `separate_stream_pipeline()`'s video branch; 4 guard tests red then green. `devtools::test()` 1583 pass / 0 fail / 4 skip.
- 2026-07-26: T5-T6 done — GPU execution test (skips here, no NVIDIA hardware); roxygen on both verbs + `has_nvenc()` back-pointers; NEWS entry. `devtools::check()` Status: OK (0/0/0), `pkgdown::check_pkgdown()` clean, `devtools::test()` 1583 pass / 0 fail / 5 skip.
- 2026-07-26: AC2 evidence — compiled 6 call shapes (scalar defaults/both-NULL/named/mixed, 2 batch) on master and HEAD; `identical()` TRUE. R/ffmpeg.R diff 95 lines, CRLF 4342->4413 (no whole-file rewrite).
- 2026-07-26: all tasks done, `devtools::check()` OK (0/0/0) — status set to review.
- 2026-07-26: review — 3-lens fan-out, 3 findings all scored >=80 and all fixed on the branch (guard hint + NEWS accuracy under nvenc, GPU test fixture discrimination). Re-verified: check OK (0/0/0), 1585 tests pass.

## Decisions

## Review

**Date:** 2026-07-26 · **PR:** https://github.com/jmgirard/tidymedia/pull/40

### Acceptance criteria — fresh evidence

- **AC1** `formals()` on both verbs shows `hardware`, `fallback` in the
  documented slot order; `man/separate_audio_video.Rd` and
  `man/separate_audio_video_batch.Rd` each carry the `hardware` param and a
  `has_nvenc()` cross-reference.
- **AC2** Compiled 6 call shapes (scalar defaults / both-NULL / named / mixed,
  plus 2 batch shapes) on `master` and on HEAD and compared the saved objects:
  `identical()` TRUE. No existing caller's command changed.
- **AC3** Scalar with the copy default + `hardware = "nvenc"` aborts with
  "`hardware` needs a re-encoding `video_codec`" / "stream-copies the video, so
  no encoder runs"; a batch table mixing a `libx264` row with a `copy` row
  aborts the same way, before any encode (`ffm_batch` builds all pipelines
  first). Message accuracy fixed at review — see F1.
- **AC4** Audio command byte-identical across all four `hardware`/`fallback`
  combinations and carries no `-codec:v`; video resolves NULL->`h264_nvenc` and
  `libx265`->`hevc_nvenc`.
- **AC5** A `hardware` jobs column is dropped by the 2N reshape: no command
  carries nvenc, 4 rows returned over 2 inputs with both stream markers.
- **AC6** `devtools::test()` 1585 pass / 0 fail / 5 skip; `devtools::check()`
  Status: OK (0 errors, 0 warnings, 0 notes); `devtools::document()` no diff.

### Consistency gate

`cairn_validate` exit 0, all checks passed. No DESIGN.md principle changed, so
`cairn_impact` did not apply. Profile (`r-package`) slot: `document()` no diff,
generated files untouched by hand, README in sync, `pkgdown::check_pkgdown()`
clean, NEWS entry present, no new top-level files, full check clean.

### Independent review — 3 lenses + scorer

Blame-history (Sonnet): 0 findings. Diff-bug (Opus): 2. Prior-PR-comments
(Sonnet): 1 (GitHub inline-comment probe empty, so archived `## Review`
sections were the evidence base). All 3 scored >=80 by a fresh scorer; none
logged below threshold.

- **F1 (88, actioned — fixed)** The copy guard's hint said to pass
  `video_codec = NULL` "to let the output container choose one", but the guard
  only fires under `hardware != "none"`, where `resolve_hw_encoder()` assumes
  the H.264 family instead. Confirmed by compiling a `.webm` output: the hint's
  own advice yields `-codec:v h264_nvenc` in a WebM container, which FFmpeg
  rejects. Hint rewritten to state the H.264 assumption and to offer dropping
  `hardware`; a regression test now asserts the message says "H.264" and never
  "container choose".
- **F2 (85, actioned — fixed)** The same wrong claim in the user-facing NEWS
  bullet; rewritten to match.
- **F3 (88, actioned — fixed)** The new GPU execution test asserted the audio
  stayed a copy using `make_test_video()`, whose AAC-in-MP4 audio matches the
  container's own default encoder — so copy and re-encode were
  indistinguishable, the exact discrimination gap M35's lesson records. Swapped
  to `make_mp3_audio_video()` with an explicit `"mp3"` assertion.
