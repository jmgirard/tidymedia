# M31: NVIDIA nvenc hardware encoding (opt-in)

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP1
- **Branch/PR:** m31-nvenc-encoding · https://github.com/jmgirard/tidymedia/pull/33

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

- [x] `nvenc_encoder(codec)` (exported, pure, no binary) maps `"h264"`/`"hevc"`/
      `"av1"` → `"h264_nvenc"`/`"hevc_nvenc"`/`"av1_nvenc"` and `cli_abort`s on
      any other value.
- [x] `has_nvenc(codec)` (exported) returns a length-1 logical, `TRUE` iff
      `nvenc_encoder(codec)` is in `ffmpeg_encoders()$name`; roxygen documents
      that this reflects FFmpeg build support, not a guaranteed runtime GPU.
- [x] With nvenc available, `standardize_video(..., hardware = "nvenc")` and
      `format_for_web(..., hardware = "nvenc")` (and their `_batch` siblings)
      compile to `-codec:v <family>_nvenc`; availability is forced in the test
      via the `tidymedia.nvenc_encoders` option seam (`withr::local_options`)
      so the check is binary-free (D-M31-1).
- [x] With nvenc unavailable, `hardware = "nvenc"` aborts (`cli_abort`, message
      names the fix); `fallback = TRUE` instead compiles the software
      `video_codec` and emits a `cli_inform`/`cli_warn`.
- [x] `hardware` defaults to `"none"`: every existing default call compiles a
      byte-for-byte identical command to before this milestone (regression).
- [x] A real GPU encode via `standardize_video(hardware = "nvenc")` writes a
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

- [x] T1: Add `nvenc_encoder()` (pure family→name map, `cli_abort` on unknown)
      and `has_nvenc()` (built on `ffmpeg_encoders()`, `R/ffmpeg.R:1279`),
      exported with `@family capability functions`, roxygen noting the
      build-support-vs-runtime-GPU caveat; add both to `_pkgdown.yml`.
- [x] T2: Add internal resolver `resolve_hw_encoder(video_codec, hardware,
      fallback)` — infers the codec family from `video_codec` (libx264→h264,
      libx265/hevc→hevc, av1 variants→av1), returns the nvenc name when
      available, else aborts or (fallback) returns `video_codec`; `cli_abort` on
      an unmappable family.
- [x] T3: Wire `hardware`/`fallback` through `standardize_video` +
      `standardize_pipeline` + `standardize_video_batch` (`R/ffmpeg.R:627/651/
      1985`); `hardware` is batch-wide scalar, default `"none"` preserves output.
- [x] T4: Wire `hardware`/`fallback` through `format_for_web` +
      `format_for_web_pipeline` + `format_for_web_batch` (`R/ffmpeg.R:468/495/
      2817`); family fixed to h264; default `"none"` preserves output.
- [x] T5: Tests — pure `nvenc_encoder` map; compile tests via the option seam
      (`withr::local_options`) for available/unavailable/`fallback`;
      default-unchanged regression; structural `has_nvenc()` test
      (`skip_if_no_ffmpeg`); add `skip_if_no_nvenc()` to `helper-skip.R` + one
      execution smoke test.
- [x] T6: Docs — a GPU-encoding note in `vignettes/workflow.Rmd` (state decode/
      GPU-filter out-of-scope), NEWS entry, `@seealso` cross-links; run
      `spelling::update_wordlist()` and confirm `pkgdown::check_pkgdown()`.

## Work log

- 2026-07-26: created by /milestone-plan.
- 2026-07-26: implemented T1–T6 — nvenc exports + resolver, `hardware`/`fallback` on standardize/format_for_web + batch siblings, tests, docs; check 0/0/0, test PASS 1143 (1 SKIP nvenc).
- 2026-07-26: review — CI (#33) red on Ubuntu (listed-but-unusable nvenc, M27 trap); fixed `skip_if_no_nvenc()` to probe with a trial encode. Fan-out: F1 scored 74 (logged); blame + prior-review clean.
- 2026-07-26: AC3 wording amended (gate-approved) to name the option seam instead of `local_mocked_bindings`, matching D-M31-1; AC3 ticked. Merge approved for PR #33.

## Decisions

- D-M31-1 — nvenc detection via a cheap check with an option seam.
  `has_nvenc()` consults `getOption("tidymedia.nvenc_encoders")` when set (a
  character vector of encoder names to treat as available), else
  `ffmpeg_encoders()$name`. Keeps every compile test binary-free and GPU-free
  (tests inject availability with `withr::local_options()`) and doubles as a
  user override for known environments. Chosen over
  `testthat::local_mocked_bindings` (would tighten the `testthat` Suggests
  floor — a dependency re-pin needing a gate + D-entry) and over a GPU-only
  test (unrunnable on CI).

## Review

Reviewed 2026-07-26. PR #33. Branch `m31-nvenc-encoding`, master unmoved since cut.

### Acceptance-criteria evidence

- AC1 ✓ — `nvenc_encoder("h264"/"hevc"/"av1")` → `h264_nvenc`/`hevc_nvenc`/
  `av1_nvenc`; `nvenc_encoder("vp9")` aborts. Direct eval + `test-nvenc.R`.
- AC2 ✓ — `has_nvenc("h264")` is `logical`, length 1, non-NA against real
  ffmpeg; option seam yields TRUE/FALSE per family. roxygen states the
  build-support-not-runtime-GPU caveat (`nvenc_encoder.Rd`).
- AC3 ⧗ — substance verified (std → `-codec:v h264_nvenc -codec:a copy`; web →
  `-codec:v h264_nvenc -codec:a aac`; both `_batch` all rows carry
  `h264_nvenc`), but the criterion names `local_mocked_bindings`; shipped code
  uses the option seam (D-M31-1). Tick pending a gated wording amendment (see
  below).
- AC4 ✓ — unavailable (`options(tidymedia.nvenc_encoders=character(0))`) aborts
  with a fix-naming message; `fallback=TRUE` → `-codec:v libx264` + a "falling
  back" message.
- AC5 ✓ — default `hardware="none"` compiles `-codec:v libx264` with no
  `nvenc`; the pre-existing full-string standardize/format_for_web tests still
  pass unchanged (regression intact).
- AC6 ✓ — GPU execution test skips cleanly (`skip_if_no_nvenc`, no GPU here);
  `_pkgdown.yml` lists both exports; `devtools::check()` 0/0/0;
  `pkgdown::check_pkgdown()` and `spelling::spell_check_package()` clean.

`test-nvenc.R`: 38 pass, 0 fail, 1 skip (GPU).

### Consistency gate (r-package profile, inferred)

- `devtools::document()` — no diff. `devtools::check()` — 0 errors / 0 warnings
  / 0 notes. `pkgdown::check_pkgdown()` — no problems. NEWS.md has the
  user-visible entry. README unchanged (no rebuild needed). Spelling clean.
- `cairn_validate` — 2 FAILs, both PRE-EXISTING and untouched by this branch:
  `references index<->disk` (M22/M25 pages lack provenance blocks) and
  `scaffold present` (`.gitignore` missing `cairn/.merge-approved.pending`).
  Neither file is in the M31 diff; routed to a follow-up `/cairn-init` repair +
  provenance backfill, not folded into M31 (ROADMAP candidate; chip task_71add82f).

### CI-caught defect (fixed on branch)

Draft-PR CI (#33) went red on all three Ubuntu jobs — the exact M27 trap:
Ubuntu's FFmpeg *lists* `h264_nvenc` (build support) so `has_nvenc("h264")`
returned TRUE and `skip_if_no_nvenc()` did not skip, but there is no GPU
(`Cannot load libcuda.so.1`), so the real-encode test errored. Fix:
`skip_if_no_nvenc()` now probes with a tiny lavfi nvenc encode and skips unless
it exits 0 — a listed-but-unusable encoder no longer runs the test. Feature
detection is unchanged by design (cheap check; documented). Re-verified.

### Independent fresh-context review (3 lenses + scorer)

- [O] diff-bug (Opus): F1 (below) + F2 = the AC3 wording staleness (handled by
  the gated amendment above). No correctness/contract defects.
- [S] blame-history (Sonnet): no findings — every pipeline guard (audio-copy,
  even-dim crop, `+faststart`) preserved; no D-entry contradicted; M28 roxygen
  trap avoided.
- [S] prior-review (Sonnet): no findings — GitHub probe empty; M23/M27/M28/
  M30/M12/M13 archived findings all honored.
- Scorer (Sonnet): F1 = 74 (< 80 → logged, not actioned).

**Logged sub-threshold (not actioned):**
- F1 (74) — resolver aborts (`resolve_hw_encoder`/`codec_family`) attribute
  `conditionCall` to the internal `*_pipeline` helper, not the user-facing verb.
  Message and fix-hint are correct; only call attribution is off. Cosmetic, and
  the pipeline helpers are shared with the batch closures (no single "correct"
  frame), so left as-is.
