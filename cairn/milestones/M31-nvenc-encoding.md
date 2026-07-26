# M31: NVIDIA nvenc hardware encoding (opt-in)

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP1
- **Branch/PR:** —

## Goal

Let users with an nvenc-capable FFmpeg easily switch the standard re-encode
verbs to NVIDIA GPU encoding, with detection and a reproducible fallback.

## Scope

**In:** an exported detector `has_nvenc(codec)` (cheap check: is `<codec>_nvenc`
listed by `ffmpeg_encoders()`); an encoder resolver `nvenc_encoder(codec)`
(pure `h264/hevc/av1 → *_nvenc` map); a `hardware = c("none","nvenc")` +
`fallback` argument on `standardize_video`/`format_for_web` and their `_batch`
siblings, resolving the nvenc encoder from the codec family and passing it to
`ffm_codec()` (Layer 2 computes the argument; Layer 1 unchanged — D009, IP1);
docs (roxygen, a vignette note, NEWS) covering the cheap-check limitation and
the out-of-scope lines below.

**Out:** a video quality / rate-control knob (CRF↔CQ, `-preset p1–p7`) →
candidate row. GPU *decode* / `-hwaccel cuda` input acceleration + GPU filter
pipelines (needs an engine input-options slot + an IP2 filtergraph call) →
candidate row. Other hardware backends (videotoolbox/qsv/vaapi/amf) — the
`hardware=` arg is designed to extend, but only `nvenc` ships here → candidate
row. Wiring `hardware` into the remaining re-encode verbs
(`anonymize_video`, `crop_video`, `segment_video`, `compare_videos`,
`picture_in_picture`) → candidate row.

## Acceptance criteria

- [ ] `nvenc_encoder(codec)` (exported, pure, no binary) maps `"h264"`/`"hevc"`/
      `"av1"` → `"h264_nvenc"`/`"hevc_nvenc"`/`"av1_nvenc"` and `cli_abort`s on
      any other value.
- [ ] `has_nvenc(codec)` (exported) returns a length-1 logical, `TRUE` iff
      `nvenc_encoder(codec)` is in `ffmpeg_encoders()$name`; roxygen documents
      that this reflects FFmpeg build support, not a guaranteed runtime GPU.
- [ ] With nvenc available, `standardize_video(..., hardware = "nvenc")` and
      `format_for_web(..., hardware = "nvenc")` (and their `_batch` siblings)
      compile to `-codec:v <family>_nvenc`; availability is forced in the test
      via `local_mocked_bindings(has_nvenc = …)` so the check is binary-free.
- [ ] With nvenc unavailable, `hardware = "nvenc"` aborts (`cli_abort`, message
      names the fix); `fallback = TRUE` instead compiles the software
      `video_codec` and emits a `cli_inform`/`cli_warn`.
- [ ] `hardware` defaults to `"none"`: every existing default call compiles a
      byte-for-byte identical command to before this milestone (regression).
- [ ] A real GPU encode via `standardize_video(hardware = "nvenc")` writes a
      playable file (execution test, `skip_if_no_nvenc()`); `_pkgdown.yml` lists
      the two new exports and `devtools::check()` + `pkgdown::check_pkgdown()`
      are clean (0 errors/warnings; spelling wordlist updated for new terms).

## Coverage

- AC1 → T1
- AC2 → T1
- AC3 → T2, T3, T4, T5
- AC4 → T2, T3, T4, T5
- AC5 → T3, T4, T5
- AC6 → T5, T6

## Tasks

- [ ] T1: Add `nvenc_encoder()` (pure family→name map, `cli_abort` on unknown)
      and `has_nvenc()` (built on `ffmpeg_encoders()`, `R/ffmpeg.R:1279`),
      exported with `@family capability functions`, roxygen noting the
      build-support-vs-runtime-GPU caveat; add both to `_pkgdown.yml`.
- [ ] T2: Add internal resolver `resolve_hw_encoder(video_codec, hardware,
      fallback)` — infers the codec family from `video_codec` (libx264→h264,
      libx265/hevc→hevc, av1 variants→av1), returns the nvenc name when
      available, else aborts or (fallback) returns `video_codec`; `cli_abort` on
      an unmappable family.
- [ ] T3: Wire `hardware`/`fallback` through `standardize_video` +
      `standardize_pipeline` + `standardize_video_batch` (`R/ffmpeg.R:627/651/
      1985`); `hardware` is batch-wide scalar, default `"none"` preserves output.
- [ ] T4: Wire `hardware`/`fallback` through `format_for_web` +
      `format_for_web_pipeline` + `format_for_web_batch` (`R/ffmpeg.R:468/495/
      2817`); family fixed to h264; default `"none"` preserves output.
- [ ] T5: Tests — pure `nvenc_encoder` map; compile tests via
      `local_mocked_bindings(has_nvenc=…)` for available/unavailable/`fallback`;
      default-unchanged regression; structural `has_nvenc()` test
      (`skip_if_no_ffmpeg`); add `skip_if_no_nvenc()` to `helper-skip.R` + one
      execution smoke test.
- [ ] T6: Docs — a GPU-encoding note in `vignettes/workflow.Rmd` (state decode/
      GPU-filter out-of-scope), NEWS entry, `@seealso` cross-links; run
      `spelling::update_wordlist()` and confirm `pkgdown::check_pkgdown()`.

## Work log

- 2026-07-26: created by /milestone-plan.

## Decisions

## Review
