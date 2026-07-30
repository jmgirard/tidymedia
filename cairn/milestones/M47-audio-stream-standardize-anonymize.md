# M47: Stop `standardize_video()` and `anonymize_video()` picking an audio track by disposition

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** m47-audio-stream-standardize-anonymize

## Goal

Give `standardize_video()` and `anonymize_video()` (+ `_batch`) an explicit audio
map on every call and an `audio_stream` selector, so which tracks survive stops
being a property of the input's flags.

## Scope

**In:** `audio_stream` on `standardize_video()`, `anonymize_video()`,
`standardize_video_batch()`, `anonymize_video_batch()` — 0-based among the
input's audio streams (D023), scalar argument plus a per-row `audio_stream`
jobs column. Both pipelines emit an explicit map on every call:
`-map 0:v -map 0:a` when `audio_stream` is `NULL`, `-map 0:v -map 0:a:<n>` when
a track is named. A `cairn/DECISIONS.md` entry fixing the pass-through family's
rule and answering the question D025's fifth bullet left open. NEWS.

**Out:**
- `crop_video` / `segment_video` → M48 (planned now, depends on this).
- `ffm_copy()`/`ffm_concat()` idempotence → M48, the first milestone that
  narrows an `ffm_copy()` map.
- Carrying subtitle or data streams through these verbs, and a
  `subtitle_stream`/`video_stream` selector → the existing M45-Out candidate row.
- Carrying M44's dropped-audio-track warning here — not needed: the default now
  carries every track, and on the named path the caller chose it (the gate
  `extract_audio()` uses, `R/ffmpeg.R:476`).
- `run = FALSE` running `ffmpeg -encoders` under `hardware = "nvenc"` → new
  candidate row; it also falsifies D024's "sole exception" sentence.
- The `audio =` (D009) documentation reconciliation → new candidate row.

## Acceptance criteria

- [ ] AC1 With `audio_stream` unset, each verb compiles exactly two `-map`
      arguments, `-map 0:v` then `-map 0:a`, asserted as a committed literal
      command string; the invariant test at `tests/testthat/test-ffm.R:438`
      is restated to the rule these verbs follow and gains both of them.
- [ ] AC2 With `audio_stream = 2`, each verb compiles exactly two `-map`
      arguments, `-map 0:v` then `-map 0:a:2`.
- [ ] AC3 On both verbs a value that is non-numeric, non-whole, negative, `NA`,
      or longer than one aborts naming `audio_stream`, and `conditionCall()`
      resolves to the verb the caller wrote, not to a Layer-1 helper.
- [ ] AC4 Both `_batch` siblings take an `audio_stream` argument and an
      `audio_stream` jobs column that overrides it per row, where a cell of `NA`
      is the column form of `NULL`; a one-row batch call compiles a command
      byte-identical to the scalar call with the same arguments.
- [ ] AC5 A wrongly typed `audio_stream` column aborts before any row runs,
      naming the column and saying `NA` keeps every audio track; the message
      does not carry the extraction family's "keep the first audio track".
- [ ] AC6 With ffmpeg present, on a 3-audio-track `.mkv` whose DEFAULT
      disposition sits on track 1, `standardize_video(audio_stream = 2)` writes
      exactly one audio stream and it is `fra`; the same call with `audio_stream`
      unset writes all three (master writes one, `spa`).
- [ ] AC7 At the default `hardware`, no entry point runs a binary when
      `run = FALSE`: a counting mock over `run_program()`, `find_ffmpeg()` and
      `find_ffprobe()` records zero invocations across all four.
- [ ] AC8 `cairn/DECISIONS.md` gains an entry recording the pass-through rule and
      why it diverges from D023's first-track `NULL`; each `@param audio_stream`
      names the other two families' `NULL` (D025's stated cost); `NEWS.md`
      records the argument and both breaking changes; `devtools::document()`
      produces no diff, `devtools::test()` is clean, and `devtools::check()`
      reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T3, T4, T7
- AC2 → T2, T3, T4
- AC3 → T3, T4
- AC4 → T5, T6
- AC5 → T5, T6
- AC6 → T1, T8
- AC7 → T3, T4, T5, T6
- AC8 → T7, T8

## Tasks

- [x] T1 Record both verbs' current compiled commands as committed literals and
      add the failing-first compile tests. Extend `make_multitrack_video()`
      (`tests/testthat/helper-media.R:158`) to put the DEFAULT disposition on
      track 1 — it sets none today — and assert the fixture's own disposition
      flags before trusting any result, skipping if they did not take (M43).
- [ ] T2 Add a pass-through map resolver beside `audio_stream_map()`
      (`R/ffmpeg.R:273`) returning `c("0:v", "0:a")` for `NULL` and
      `c("0:v", "0:a:<n>")` for a named track; unit-test it.
- [ ] T3 `standardize_video()` / `standardize_pipeline()` (`R/ffmpeg.R:1265`,
      `:1298`): argument before `run` (M45's precedent), guard at the END of the
      front-door block so precedence does not move (M41), map in the pipeline.
- [ ] T4 Same for `anonymize_video()` / `anonymize_pipeline()`
      (`R/ffmpeg.R:1410`, `:1437`) — its front door is thin (`:1417-1419`) and
      most validation lives in the pipeline with `call =` threaded.
- [ ] T5 `standardize_video_batch()` (`R/ffmpeg.R:3115`): argument,
      `check_batch_audio_col(jobs, "audio_stream", na_means = …)`,
      `batch_stream_cell()` in the closure. No reshape, so
      `check_batch_stream_values()` is not needed (`R/ffmpeg.R:3793-3801`).
- [ ] T6 Same for `anonymize_video_batch()` (`R/ffmpeg.R:1661`); its closure
      names `regions` explicitly (`:1774`), so the new column arrives via `dots`.
- [ ] T7 Roxygen on all four, the `@param jobs` column enumerations (M39), and
      the D025 cross-references; `devtools::document()`.
- [ ] T8 Execution tests on the multi-track fixture; the D-entry; NEWS.

## Work log

- 2026-07-30: created by /milestone-plan.
- 2026-07-30: plan gate chose an always-emitted explicit map (`NULL` → `0:v` + `0:a`) over leaving `NULL` as today's command, because the latter keeps FFmpeg's DEFAULT-disposition heuristic as the resolved default — measured 3 audio tracks in, the second one out — which D023's second bullet rules out in terms that are not verb-scoped; falsified by a report of a caller relying on these verbs carrying subtitle or data streams.
- 2026-07-30: plan gate chose `0:v` + `0:a` over `-map 0` for the `NULL` case because `-map 0` into `.mp4` on a subtitle-bearing input fails outright (measured exit 8, ffmpeg 8.1.2), which would newly break both verbs on the package's flagship container; falsified by a default output container that accepts subtitles, or an FFmpeg build that stream-copies unencodable streams.
- 2026-07-30: plan gate chose two milestones by verb pair over one eight-entry-point milestone, because eight entry points is roughly twice M43's proven size and trips the >~7-criteria and >~10-task tripwires; falsified by M47 landing in well under one working session.
- 2026-07-30: criteria audit ([O], fresh context) returned 13 findings; 10 with one clear answer were fixed before the gate (non-discriminating execution criterion, an evaporating `master` baseline, unnamed output containers, four bundled criteria, omitted NEWS and `@param` obligations, the falsified `ffm_copy()` prose, undetermined scalar `NA`, the unmentioned map invariant, and a false `run = FALSE` purity claim under `hardware = "nvenc"` that I reproduced); the remaining 3 collapsed into the gate's first question.

- 2026-07-30: T1 — `make_multitrack_video()` gained `default_track =` rather than moving the disposition in place: 22 existing call sites use the fixture, and a defaulted parameter leaves every one of them compiling the identical command. `NULL` emits no `-disposition` flags at all.
- 2026-07-30: T1 — the fixture clears track 0's DEFAULT before setting the requested one; `-disposition:a:1 default` alone ADDS the flag, leaving two default tracks and FFmpeg back on its own preference. Verified `1 0 0` unchanged vs `0 1 0` with `default_track = 1`.
- 2026-07-30: T1 — 7 tests red for the right reason (`unused argument (audio_stream = 2)`), 36 green.

## Decisions

## Review
