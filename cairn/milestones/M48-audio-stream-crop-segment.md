# M48: Carry the track selector to `crop_video()` and `segment_video()`, and make `ffm_copy()` idempotent again

- **Status:** planned
- **Priority:** normal
- **Depends on:** M47
- **Driving RR:** —
- **Principles touched:** IP1, IP2, GP2
- **Branch/PR:** —

## Goal

Put `crop_video()` and `segment_video()` (+ `_batch`) on M47's map rule, and stop
a repeated `ffm_copy()` duplicating every output stream.

## Scope

**In:** `audio_stream` on `crop_video()`, `segment_video()`,
`crop_video_batch()`, `segment_video_batch()`, under M47's rule —
`-map 0:v -map 0:a` when `NULL`, `-map 0:v -map 0:a:<n>` when a track is named,
on both `segment_video()` branches. On the `reencode = FALSE` branch the map
replaces `ffm_copy()`'s `-map 0` rather than appending beside it, which gives
`ffm_map(replace = TRUE)` its first in-package caller (M43 shipped it with none).
`ffm_copy()` and `ffm_concat()` become idempotent again. NEWS.

**Out:**
- D018's GP2 trade on `segment_video()`'s audio stream: `audio_stream` selects
  which track carries the packet-boundary cut, never how it is cut.
- Subtitle and data carriage, and a `subtitle_stream`/`video_stream` selector →
  the existing M45-Out candidate row. `crop_video()` carries a subtitle today
  into `.mkv` and stops; the same change makes it stop failing into `.mp4`.
- The `audio =` (D009) documentation reconciliation → new candidate row.
- `hardware = "nvenc"` probing under `run = FALSE` → new candidate row (M47 Out).

## Acceptance criteria

- [ ] AC1 With `audio_stream` unset, `crop_video()`, `segment_video(reencode =
      TRUE)` and `segment_video(reencode = FALSE)` each compile exactly two
      `-map` arguments, `-map 0:v` then `-map 0:a`, asserted as committed
      literal command strings.
- [ ] AC2 With `audio_stream = 2`, each of those three compiles exactly two
      `-map` arguments, `-map 0:v` then `-map 0:a:2`; on the `reencode = FALSE`
      branch no `-map 0` survives, so the selector narrows `ffm_copy()`'s map
      rather than appending beside it.
- [ ] AC3 With ffmpeg present, on a 3-audio-track, 1-subtitle `.mkv`:
      `crop_video(audio_stream = 2)` into `.mkv` writes exactly one audio stream
      and it is `fra`; and `crop_video()` into `.mp4` exits 0, where on master
      the same call fails (measured exit 8, no default mp4 subtitle encoder).
- [ ] AC4 `ffm_copy()` applied twice compiles exactly one `-map 0`, and
      `ffm_concat() |> ffm_copy()` likewise; with ffmpeg present a doubled
      `ffm_copy()` over a 5-stream `.mkv` writes 5 streams, not the 10 master
      writes. `ffm_copy()`'s `@param streams` prose (`R/ffm.R:610-613`), which
      today documents the appending behavior as the contract, is rewritten.
- [ ] AC5 Both `_batch` siblings take an `audio_stream` argument and an
      `audio_stream` jobs column overriding it per row, `NA` being the column
      form of `NULL`; a one-row batch call compiles byte-identically to the
      scalar call; and `segment_video()`'s own fan-out (`R/ffmpeg.R:2440`)
      carries the argument to every segment it produces.
- [ ] AC6 A wrongly typed `audio_stream` column aborts before any row runs,
      naming the column and saying `NA` keeps every audio track.
- [ ] AC7 At the default `hardware`, no entry point runs a binary when
      `run = FALSE` (counting mock over `run_program()`, `find_ffmpeg()`,
      `find_ffprobe()`).
- [ ] AC8 The map invariant test at `tests/testthat/test-ffm.R:438` states the
      rule every verb now follows and covers each one M47 and M48 touched;
      `NEWS.md` records the argument, the `ffm_copy()` fix, and the
      subtitle-carriage change; `devtools::document()` produces no diff,
      `devtools::test()` is clean, and `devtools::check()` reports 0 errors and
      0 warnings.

## Coverage

- AC1 → T1, T3, T4
- AC2 → T3, T4
- AC3 → T7
- AC4 → T2
- AC5 → T4, T5
- AC6 → T5
- AC7 → T3, T4, T5
- AC8 → T6, T7

## Tasks

- [ ] T1 Record the three current commands as committed literals (the
      `baseline_pair()` pattern, `test-separate-av-multitrack.R:32-37`) and add
      the failing-first compile tests.
- [ ] T2 Restore `ffm_copy()`/`ffm_concat()` idempotence: either de-duplicate
      appended specifiers in `ffm_map()` (`R/ffm.R:590`) or have `ffm_copy()`
      set its map with `replace` (`R/ffm.R:639`). Record which in the decision
      log — de-duplicating changes a documented Layer-1 contract that D023's
      fourth bullet rests on. (RB tripwire: irreversible-api) Add the doubled-copy
      compile and execution tests, and rewrite the `@param streams` prose.
- [ ] T3 `crop_video()` / `crop_video_pipeline()` (`R/ffmpeg.R:1045`, `:982`):
      argument before `run`, guard last in the front-door block (M41), and
      replace `ffm_map(p, "0")` (`:989`) with M47's resolver.
- [ ] T4 `segment_pipeline()` (`R/ffmpeg.R:2499`) on both branches, and
      `segment_video()` (`:2402`) carrying the argument into the internal jobs
      tibble it builds at `:2440`.
- [ ] T5 `crop_video_batch()` (`R/ffmpeg.R:4246`) and `segment_video_batch()`
      (`:2622`): argument, `check_batch_audio_col(jobs, "audio_stream",
      na_means = …)`, `batch_stream_cell()` in each closure.
- [ ] T6 Rewrite the invariant test at `tests/testthat/test-ffm.R:438` to the
      new rule and extend it to every verb M47 and M48 touched.
- [ ] T7 Roxygen on all four plus the `@param jobs` enumerations (M39);
      `devtools::document()`; execution tests on the multi-track and
      subtitle fixtures; NEWS.

## Work log

- 2026-07-30: created by /milestone-plan.
- 2026-07-30: plan gate chose to fold the `ffm_copy()`/`ffm_concat()` idempotence fix in here over leaving it a candidate row, because this milestone narrows `ffm_copy()`'s map on `segment_video(reencode = FALSE)` and so re-enters and re-reads that contract anyway — the promotion condition the candidate row itself named; falsified by the fix needing tests or a design call that outgrow this milestone's budget.
- 2026-07-30: plan gate chose to keep `crop_video` and `segment_video` in one milestone over isolating `segment_video`, because crop is a single pipeline line and the shared `check_batch_jobs()` while segment carries the branch split and the fan-out, giving one milestone of roughly M43's size rather than a third planning cycle for a trivial verb; falsified by segment's two branches costing more than a working session on their own.

## Decisions

## Review
