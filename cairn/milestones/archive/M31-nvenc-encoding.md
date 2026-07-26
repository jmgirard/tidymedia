# M31: NVIDIA nvenc hardware encoding (opt-in)

**Status:** done (2026-07-26, PR #33 https://github.com/jmgirard/tidymedia/pull/33)

**Goal:** Let users with an nvenc-capable FFmpeg easily switch the standard
re-encode verbs to NVIDIA GPU encoding, with detection and a reproducible fallback.

**Outcome:** Exported `has_nvenc(codec)` (cheap `ffmpeg_encoders()` listing
check) and `nvenc_encoder(codec)` (pure `h264/hevc/av1 → *_nvenc` map). Internal
`codec_family()` + `resolve_hw_encoder()` back a `hardware = c("none","nvenc")` +
`fallback` argument on `standardize_video()`, `format_for_web()`, and their
`_batch` siblings — Layer 2 computes the name, Layer 1 unchanged (IP1/D009).
Unavailable nvenc aborts by default; `fallback = TRUE` re-encodes software with a
message. Hardware decode + GPU filters deferred (ROADMAP candidates).

**Decisions:** D-M31-1 — nvenc detection via a cheap `ffmpeg_encoders()` check
with a `tidymedia.nvenc_encoders` option seam, so compile tests stay binary-free
(withr::local_options) without a `local_mocked_bindings` testthat re-pin.

**Review:** 3-lens fan-out — diff-bug (Opus), blame + prior-review (Sonnet).
Blame + prior-review clean; F1 (error call-attribution to internal pipeline)
scored 74 → logged, not actioned. CI caught the M27 trap (Ubuntu lists
h264_nvenc without a GPU); fixed `skip_if_no_nvenc()` to probe with a trial
encode. AC3 wording amended (gate-approved) to the option seam. CI green.
