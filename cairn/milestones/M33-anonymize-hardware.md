# M33: Wire `hardware=` nvenc into `anonymize_video`

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** m33-anonymize-hardware

## Goal

Give `anonymize_video()` and `anonymize_video_batch()` the same opt-in
`hardware = "nvenc"` GPU-encoding toggle M31 shipped on `standardize_video`.

## Scope

**In:** Add `hardware = c("none", "nvenc")` + `fallback = FALSE` to
`anonymize_video()` (R/ffmpeg.R:759) and, batch-wide, to
`anonymize_video_batch()` (R/ffmpeg.R:959), reusing the M31 machinery
(`resolve_hw_encoder`/`codec_family`/`has_nvenc`). The verb already exposes a
user-controlled `video_codec` feeding `ffm_codec`, so this is a direct mirror
of the `standardize_video` change — one `resolve_hw_encoder()` line in
`anonymize_pipeline()` before `ffm_codec()`, Layer 1 untouched (IP1).

**Out:** The four codec-less re-encode verbs (`crop_video`, `segment_video`,
`compare_videos`, `picture_in_picture`) → M34; they need a new `video_codec`
arg first and its API shape is under a Fable review brief. New hardware
backends beyond nvenc, GPU decode, and the video-quality/rate-control knob stay
ROADMAP candidates (M31 Out).

## Acceptance criteria

- [ ] AC1 — `anonymize_video()` and `anonymize_video_batch()` accept
      `hardware = c("none","nvenc")` and `fallback = FALSE`; with the default
      `hardware="none"` each compiles a command byte-identical to the pre-M33
      verb (no behavior change), asserted by a compile-level test.
- [ ] AC2 — `hardware="nvenc"` rewrites the video encoder to the nvenc form of
      the `video_codec` family via `resolve_hw_encoder()`; unavailable nvenc
      aborts by default and re-encodes software with a message under
      `fallback=TRUE` (mirrors M31).
- [ ] AC3 — a `video_codec` outside the h264/hevc/av1 families combined with
      `hardware="nvenc"` aborts via `codec_family()` (reused, not reimplemented).
- [ ] AC4 — `anonymize_video_batch()` threads `hardware`/`fallback` batch-wide
      (captured scalars, **not** per-row job columns), documented as such,
      matching `standardize_video_batch`.
- [ ] AC5 — tests cover AC1–AC4 binary-free via the `tidymedia.nvenc_encoders`
      option seam; any execution test guards with a runtime `skip_if_no_nvenc()`
      probe (M31 lesson: CI lists `h264_nvenc` without a GPU). `devtools::test()`
      clean.
- [ ] AC6 — profile `verify` clean: `devtools::document()` produces no diff, new
      args documented in roxygen on both verbs (`@seealso has_nvenc()`), no new
      exports so `_pkgdown.yml` is unaffected.

## Coverage

- AC1 → T1, T2, T4
- AC2 → T1, T4
- AC3 → T1, T4
- AC4 → T2, T4
- AC5 → T4
- AC6 → T3, T5

## Tasks

- [x] T1 — Add `hardware`/`fallback` to `anonymize_video()` (R/ffmpeg.R:759):
      signature after `pixel_format`, `hardware <- rlang::arg_match(hardware)`,
      pass through. In `anonymize_pipeline()` (R/ffmpeg.R:783) add
      `hardware="none", fallback=FALSE` params and insert
      `video_codec <- resolve_hw_encoder(video_codec, hardware, fallback)`
      before `ffm_codec()`; keep the existing `check_token(video_codec)`.
- [x] T2 — Add batch-wide `hardware`/`fallback` to `anonymize_video_batch()`
      (R/ffmpeg.R:959), threaded to `anonymize_pipeline()` per row as captured
      scalars (no `pick()` over `...`), mirroring `standardize_video_batch`.
- [x] T3 — Roxygen: document `hardware`/`fallback` on both verbs (reuse M31
      wording from `standardize_video`, R/ffmpeg.R:626-639); `devtools::document()`.
- [x] T4 — Tests (tests/testthat): default no-op byte-identity (AC1), nvenc
      resolution via option seam (AC2), family rejection (AC3), batch-wide
      threading (AC4); execution tests behind `skip_if_no_nvenc()`.
- [ ] T5 — Run profile `verify`: `devtools::test()` clean, `devtools::document()`
      no diff, `devtools::check()` if anything structural was touched.

## Work log

- 2026-07-26: created by /milestone-plan; split from the M31-follow-on candidate (sibling M34 covers the four codec-less verbs, API shape under a Fable RB).
- 2026-07-26: T1–T4 — added `hardware`/`fallback` to `anonymize_video` (+pipeline, +batch, batch-wide) mirroring M31; roxygen + `document()`; nvenc tests in test-nvenc.R. `devtools::test()` clean (0 fail, 2 GPU skips, 1233 pass).

## Decisions

## Review
