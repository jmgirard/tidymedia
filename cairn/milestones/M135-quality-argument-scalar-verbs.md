# M135: The re-encoding task functions take a `quality` argument that passes the encoder's own rate-control value through

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Resolves:** —
- **Surface tier:** user-facing — a new argument on eight exported functions and their help pages
- **Branch/PR:** m135-quality-argument-scalar-verbs

## Goal

A caller sets the encoder's own quality number on a re-encoding task function without building a pipeline by hand.

## Scope

**In:** a `quality = NULL` argument on the eight scalar task functions that take `hardware`: `anonymize_video()`, `compare_videos()`, `crop_video()`, `format_for_web()`, `picture_in_picture()`, `segment_video()`, `separate_audio_video()` and `standardize_video()`. One internal table, `quality_flags()`, keyed on the exact resolved encoder name: `libx264` and `libx265` take `-crf` (0 to 51), `h264_nvenc`, `hevc_nvenc` and `av1_nvenc` take `-cq` (0 to 51), and `h264_videotoolbox` and `hevc_videotoolbox` take `-q:v` (1 to 100). The number is passed through unchanged. No cross-encoder scale exists. `NULL` emits nothing, so the encoder default applies. One shared Layer 2 helper beside `emit_video_codec()` emits the flag through `ffm_output_options()`, so no task function glues a command string (IP1). An encoder the table lacks, including an alias such as `"h264"` or a software encoder such as `libvpx-vp9`, is refused with `quality` set. Under `fallback = TRUE` a fallback to software drops `quality` and the message says so. Front-door refusals before any probe (D036). Help pages, the workflow vignette section "Using video hardware", and a `NEWS.md` entry.

**Out:** the `quality` column on the eight `_batch` functions → M136. `-preset` and bitrate → candidate row "Eight things left out of the hardware surface" (g). A cross-encoder quality scale → never planned; the number means what the encoder says it means. GPU decoding → the same row's (h).

## Acceptance criteria

- [ ] AC1: Every exported function whose formals include `hardware` and `run` and exclude `jobs` — the set `Filter(function(f) all(c("hardware", "run") %in% names(formals(f))) && !"jobs" %in% names(formals(f)), mget(getNamespaceExports("tidymedia"), asNamespace("tidymedia")))` returns, 8 functions at the base commit — has a `quality` formal whose default is `NULL`.
- [ ] AC2: For every function in the AC1 set and every row of `quality_flags()` whose encoder that function's `video_codec` and `hardware` formals can resolve to (52 pairs at the base commit: 7 rows for each function with a `video_codec` formal, and the three H.264 rows for `format_for_web()`), a test calls the function with `run = FALSE`, the backend encoders reported present through the `tidymedia.hardware_encoders` option, and an in-range `quality`, and asserts that the compiled command contains `-codec:v <encoder>` and after it the row's flag and the value. The same call with `quality = NULL` compiles a command containing none of `-crf`, `-cq` and `-q:v`.
- [ ] AC3: For every function in the AC1 set, a test asserts that each of these calls is refused before any FFmpeg process starts, with an error whose call names that function: `quality` that is not one finite number; `quality` outside the range `quality_flags()` gives for the resolved encoder; and `quality` with a resolved encoder the table lacks, the error message naming that encoder. For every function in the AC1 set with a `video_codec` formal, the same for `quality` with `video_codec = "copy"` and for `quality` with `video_codec = NULL` under `hardware = "none"`. For `segment_video()`, the same for `quality` with `reencode = FALSE`.
- [ ] AC4: For every function in the AC1 set, a test asserts that under `fallback = TRUE`, with the backend encoder absent from the `tidymedia.hardware_encoders` option and `quality` set, the fallback message says `quality` was dropped, and the compiled command contains none of `-crf`, `-cq` and `-q:v`.
- [ ] AC5: Every `man/<name>.Rd` page of a function in the AC1 set has an `\item{quality}` entry, and `LC_ALL=en_US.UTF-8 Rscript tools/doc_prose_report.R` over those 8 pages and `vignettes/workflow.Rmd` prints no finding and exits 0. The vignette section "Using video hardware" names `quality` and the three flags.
- [ ] AC6: `NEWS.md` has an entry for the `quality` argument. `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T3, T4
- AC2 → T1, T2, T3, T4
- AC3 → T1, T2, T3, T4
- AC4 → T2
- AC5 → T5
- AC6 → T5, T6

## Tasks

- [x] T1: Tests first, then `quality_flags()` (the seven-row table) and `check_quality()` in `R/ffmpeg.R` beside `codec_family()` (`R/ffmpeg.R:3113`): one finite number, in the row's range, refused with the encoder named when the table lacks it. The `call` is threaded as an internal formal with no default (D087).
- [x] T2: Tests first, then `emit_quality()` beside `emit_video_codec()` (`R/ffmpeg.R:3340`): resolves the encoder the same way, checks the argument before the probe (D036, M095 ordering), and adds `ffm_output_options(p, "<flag> <value>")`. `resolve_hw_encoder()` (`R/ffmpeg.R:3147`) learns whether it fell back, so the fallback message can add that `quality` was dropped. Mock the option both ways (LESSONS M094).
- [x] T3: `quality` on `standardize_video()`, `format_for_web()`, `anonymize_video()` and `crop_video()`, through their pipelines (`R/ffmpeg.R:1499`, `:1850`, `:2027`, `:1387`), with the AC2 and AC3 grid tests for these four. Note `format_for_web()` fixes the family to H.264.
- [x] T4: `quality` on `segment_video()`, `separate_audio_video()`, `compare_videos()` and `picture_in_picture()` (`R/ffmpeg.R:623`, `:3849`, `:6719`, `:6859`), the `reencode = FALSE` refusal on `segment_video()`, and the grid tests for these four. Pass `NULL` through any `do.call()` grid as `args["quality"] <- list(NULL)` (LESSONS M106).
- [ ] T5: A `quality_param()` helper in `R/task-doc.R` beside `hardware_param()` (`R/task-doc.R:56`), used by the 8 pages; the vignette sentences; the `NEWS.md` entry; `devtools::document()`; the prose sweep.
- [ ] T6: `devtools::check()`, `devtools::test()` with no other R session working (LESSONS M124), and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-17: created by /milestone-plan, promoting candidate row "Eight things left out of the hardware surface" (g); lineage M31 Q4 and M100 Out.
- 2026-09-17: criteria audit ran in full mode. It returned 6 findings: AC1's filter enumerated 10 functions (fixed: `run` formal, count stated); AC2 quantified over pairs `format_for_web()` cannot reach (fixed: reachable pairs, census 52); AC3 blamed `ffm_output_options()` (fixed: the verb and the encoder); AC3's `copy` clauses were vacuous for `format_for_web()` (fixed: scoped); AC4's "absent" was ambiguous between table and build (fixed: the option); the Layer 1 route was unbound (placed in T2 as a mechanism, not a criterion).
- 2026-09-17: plan gate chose two milestones (scalar, then batch) over one because 16 verbs, a column form and two 52-pair grids exceed the 7-criteria and 150-line tripwires; falsified by M136 landing in under one session.
- 2026-09-17: plan gate chose dropping `quality` on fallback, with the message saying so, over refusing `quality` with `fallback = TRUE`, because a shared script must still complete on a machine without the hardware and no cross-encoder scale can carry the number; falsified by a user report of a silently lower-quality file that the message did not explain.
- 2026-09-17: plan gate chose refusing an encoder the table lacks over passing `-crf` to any software name, because `-crf` means something else or nothing on encoders outside the table; falsified by the first request naming a software encoder that reads `-crf` as x264 does.
- 2026-09-17: plan chose one Layer 2 helper calling `ffm_output_options()` over a new `ffm_quality()` engine verb, because the engine already carries raw output options and the table is a Layer 2 mapping like `resolve_hw_encoder()` (IP1, M31 precedent); falsified by a second Layer 2 site needing the flag positioned before the codec.
- 2026-09-17: gate chose `quality` after `fallback` in every signature (hardware, fallback, quality read as one encoder block; nothing positional before it moves). T1: `quality_flags()` and `check_quality()` in `R/ffmpeg.R` with `tests/testthat/test-quality.R` (169 assertions); full suite 0 failures.
- 2026-09-17: T2: `emit_video_codec()` takes `quality`, checks it against `intended_encoder()` (pure) before the probe, and emits through `emit_quality()` -> `ffm_output_options()`; the resolver body moved to `resolve_hw_encoder_info()` (encoder + `fell_back`), `resolve_hw_encoder()` stays the one-string wrapper. The fallback message gains an `i` bullet when `quality` is set. `test-quality-seam.R` (90 assertions). Two guard tests that read the resolver by name (`helper-hw-encoder-ledger.R`, `test-nvenc-front-door.R:556`) now read both names. Full suite: 0 failures outside those two files before the fix, 0 after.
- 2026-09-17: T3: `quality` after `fallback` on `standardize_video()`, `format_for_web()`, `anonymize_video()` and `crop_video()`; each pipeline takes `quality = NULL` as its last formal before `call` (no positional caller shifts) and hands it to the emit half. `format_for_web_pipeline()` and `anonymize_pipeline()` now reach the resolver through `emit_video_codec()` rather than directly, so the resolver ledger (`helper-hw-encoder-ledger.R`) shrinks to its one seam site and the discrimination cases use bodies that fail each reading. `quality_param()` added to `R/task-doc.R` (T5's helper, needed for the pages to document). Grid file `test-quality-grid.R` + `helper-quality-grid.R` over the four landed verbs (AC1–AC4 shape; T4 widens to AC1's filter). Two pinned sweeps grew by the 20 new cells (`test-nvenc-probe-blame.R` 499→519 kept; `test-unguarded-argument-front-doors.R` 1515→1535 rows, 1084→1104 kept), and the probe-blame message-equality comparison exempts encoder-keyed arguments (`tm_nvenc_encoder_keyed_args()`: `quality`'s refusal names the encoder `hardware` selects; the cells stay counted and kept). Full suite: 0 failures outside those files before the pin update, 0 after.
- 2026-09-17: found while writing the AC1 test: the criterion's literal filter expression errors with `value for '.data' not found`, because `mget()` over the exports hits rlang's reexported active binding; `ifnotfound = list(NULL)` fixes the instrument. Held for the amendment gate (criterion wording is substantive).
- 2026-09-17: T4: `quality` after `fallback` on `segment_video()`, `separate_audio_video()`, `compare_videos()` and `picture_in_picture()`. `segment_video()` checks it at its front door above the probe (the seam's check would run under `purrr::pmap()`), and a new `check_quality_needs_reencode()` (condition 2b, beside condition 2) refuses `quality` with `reencode = FALSE` at the front door and in `segment_pipeline()`. `separate_stream_pipeline()` passes it on the video branch only. The grid now enumerates AC1's filter: 8 verbs, 52 pairs (census test). Pins: probe-blame 519→539 kept; unguarded 1535→1555 rows, 1104→1124 kept. Full suite: 0 failures, 15441 passes.

## Decisions

## Review
