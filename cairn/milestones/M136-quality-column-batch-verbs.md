# M136: The batch re-encoding functions take `quality` as an argument and as a jobs column

- **Status:** review
- **Priority:** normal
- **Depends on:** M135
- **Driving RR:** —
- **Principles touched:** IP1
- **Resolves:** —
- **Surface tier:** user-facing — a new argument and jobs column on eight exported batch functions
- **Branch/PR:** m136-quality-column-batch-verbs

## Goal

A batch caller sets the encoder's own quality number for the whole batch or per row.

## Scope

**In:** a `quality = NULL` argument on the eight `_batch` task functions that take `hardware`, passed to the scalar form, and a numeric `quality` column of `jobs` in which a column `NA` is the column form of the scalar default (D022). Column checks at the batch function's front door, before any row runs, the error naming the function and the row (D076, M56). The batch help pages and a `NEWS.md` line for the column. M135's table, checks and helper are reused unchanged.

**Out:** the scalar argument, the table and the fallback rule → M135. `-preset` and bitrate → candidate row "Eight things left out of the hardware surface" (g).

## Acceptance criteria

- [ ] AC1: Every exported function whose formals include both `hardware` and `jobs` — the set `Filter(function(f) all(c("hardware", "jobs") %in% names(formals(f))), mget(getNamespaceExports("tidymedia"), asNamespace("tidymedia"), ifnotfound = list(NULL)))` returns (`ifnotfound` because `mget()` over the exports hits rlang's reexported `.data` active binding, LESSONS M135), 8 functions at the base commit — has a `quality` formal whose default is `NULL`.
- [ ] AC2: For every function in the AC1 set and every row of `quality_flags()` whose encoder that function's `video_codec` surface and `hardware` can resolve to (52 pairs at the base commit: 7 rows for each function with a `video_codec` formal, and the three H.264 rows for `format_for_web_batch()`), a test calls the function with `run = FALSE`, the backend encoders reported present through the `tidymedia.hardware_encoders` option, and a two-job `jobs` (two output rows for the fan-in functions `compare_videos_batch()` and `picture_in_picture_batch()`) whose `quality` column is `c(NA, <in-range>)`, and asserts that every compiled command of the first job contains none of `-crf`, `-cq` and `-q:v`, and that the second job's compiled video command (`separate_audio_video_batch()` returns an audio and a video command per job; every other function returns one command per job) contains `-codec:v <encoder>` and after it the row's flag and the value.
- [ ] AC3: For every function in the AC1 set, a test asserts that each of these is refused before any FFmpeg process starts and before any row runs, with an error whose call names that function and whose message names the row: a `quality` column that is not numeric or logical all-`NA`; a cell outside the range for that row's resolved encoder; and, for every function with a `video_codec` formal, a cell on a row whose `video_codec` is `"copy"`, or is `NA` with the scalar `video_codec = NULL` under `hardware = "none"`.
- [ ] AC4: Every `man/<name>.Rd` page of a function in the AC1 set has an `\item{quality}` entry, and `LC_ALL=en_US.UTF-8 Rscript tools/doc_prose_report.R` over those 8 pages prints no finding and exits 0. `NEWS.md` names the column. `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T2, T3
- AC2 → T2, T3
- AC3 → T1, T2, T3
- AC4 → T4, T5

## Tasks

- [x] T1: Tests first, then `check_batch_quality()` beside `batch_codec_cell()` (`R/ffmpeg.R`): numeric or all-`NA` logical, each non-`NA` cell checked with M135's `check_quality()` against that row's resolved encoder, wrapped in `check_batch_cell()` so the error names the row and never purrr's index. It also takes the whole-batch argument, repeated per row with no locator.
- [x] T2: `quality` on `standardize_video_batch()`, `format_for_web_batch()`, `anonymize_video_batch()` and `crop_video_batch()`, picked per row like `video_codec` (`R/ffmpeg.R:2366`), with the AC2 and AC3 tests. Give each row its own output path in the column-form cells (LESSONS M109).
- [x] T3: `quality` on `segment_video_batch()`, `separate_audio_video_batch()`, `compare_videos_batch()` and `picture_in_picture_batch()` (`R/ffmpeg.R:4059`, `:4714`, `:5258` and the fan-in tables per D015), with their tests.
- [x] T4: `batch_quality_param()` beside `batch_hardware_param()` (`R/task-doc.R`); the `NEWS.md` entry; `devtools::document()`; the prose sweep.
- [x] T5: `devtools::check()`, `devtools::test()` alone (LESSONS M124), and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-17: created by /milestone-plan with M135, from candidate row "Eight things left out of the hardware surface" (g).
- 2026-09-17: criteria audit ran in full mode with M135's. It returned 3 findings on this file: AC1's `NA` clause restated D022 loosely (fixed: D022's wording); AC2's grid was unreachable for `format_for_web_batch()` and ambiguous for the fan-in tables (fixed: census 52, jobs shape named); AC3's `In index:` string clause bound purrr's wording (moved to T1).
- 2026-09-17: /milestone-implement started on branch m136-quality-column-batch-verbs. No question gate: D022 fixes what an NA cell means and the M135 scalar sites fix the check order.
- 2026-09-17: amendment (mini gate, accepted): AC1's filter expression errored as written (`value for '.data' not found`, rlang's reexported active binding); `ifnotfound = list(NULL)` added with a parenthetical naming why, the M135 repair. The set it names is unchanged (8).
- 2026-09-17: re-audit: AC1 (full) — nothing.
- 2026-09-17: amendment (mini gate, accepted): AC2 named "the first compiled command" and "the second", but `separate_audio_video_batch()` returns an audio and a video command per job, so its second command is the first job's video command and never carries the flag. Reworded to count jobs: every command of the first job carries no flag; the second job's video command carries it. Grid and values unchanged.
- 2026-09-17: re-audit: AC2 (full) — nothing (note: the fixture shape is instrument detail, load-bearing for the column form and the shape M135's AC2 carries).
- 2026-09-17: T1 done. `check_batch_quality(jobs, quality, codec_rows, hardware)` takes the argument and the column in one call, so the eight verbs need one line each; a "copy" cell bypasses `intended_encoder()` so the refusal names the copy and not a family. Direct tests in `test-quality-batch-col.R`. `devtools::test()` 0 failures.
- 2026-09-17: T2 and T3 done in one commit (all eight sites are in `R/ffmpeg.R`, so the code does not split by task). The grid in `test-quality-batch.R` over AC1's filter: 52 pairs, both range ends, the argument form, and NA over the argument (D022). Two planted defects went red where expected: a no-op `emit_quality()` failed AC2 alone (208 failures) and a no-op `check_batch_quality()` failed AC3 alone (288). `segment_video_batch()` also refuses a cell on a `reencode = FALSE` row per row (M135 condition 2b). The two census tests (`test-nvenc-probe-blame.R`, `test-unguarded-argument-front-doors.R`) grew by the 40 cells the eight formals add, all kept, pins updated.
- 2026-09-17: T4 done. `batch_quality_param(scalar, output)` writes the 8 `\item{quality}` entries; each `jobs` entry names the column. NEWS: the M135 entry's "do not take it yet" sentence removed and a new entry added. `document()` ran under roxygen2 8.0.0 against the 8.1.0 pin (LESSONS M128); the man diff carries only the quality text. Prose sweep over the 8 pages: no finding, exit 0. `pkgdown::check_pkgdown()`: no problems.
- 2026-09-17: claim audit: 21 claims read, 0 corrected — R/ffmpeg.R, R/task-doc.R, NEWS.md, man/*_batch.Rd, tests/testthat/test-quality-batch.R, test-quality-batch-col.R, test-nvenc-probe-blame.R, test-unguarded-argument-front-doors.R. One note: on `segment_video_batch()` a non-numeric `quality` column on a `reencode = FALSE` row reports the cut contradiction first; no added line claims otherwise.
- 2026-09-17: T5 done. `devtools::check()` 0 errors, 0 warnings, 0 notes (7m 19s). `devtools::test()` alone: 0 failures (the two census files re-run clean after their pins moved). `pkgdown::check_pkgdown()`: no problems. Status set to review.

## Decisions

- M136-1 (2026-09-17): one checker for both forms. `check_batch_quality(jobs, quality, codec_rows, hardware)` takes the argument and the column in one call and resolves per row, so each of the eight verbs adds one call above its availability probe (D036) and the row-locator rule (a column names the row, an argument names none) is written once. A `"copy"` cell bypasses `intended_encoder()` so the refusal names the copy and never a family lookup. On `separate_audio_video_batch()` the reshaped `quality` column is built after the check, so a wrong value never reaches `as.numeric()` and `rbind()`. Over: a per-verb sweep beside each codec sweep (eight copies of the locator rule).

## Review
