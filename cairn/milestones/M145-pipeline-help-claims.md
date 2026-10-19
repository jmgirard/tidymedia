# M145: The `ffm_*()` pipeline help pages state what the code does

- **Status:** review
- **Priority:** normal
- **Depends on:** M144
- **Driving RR:** —
- **Principles touched:** GP2
- **Resolves:** —
- **Surface tier:** user-facing — help pages that package users read
- **Branch/PR:** m145-pipeline-help-claims

## Goal

Thirteen `ffm_*()` help-page claims that the M128 and M131 claim audits found false or could not settle now say what the code does.

## Scope

**In:** correct the text of these claims. Line numbers are as read on 2026-10-18.

- (a) `?ffm_map` says it is the only pipeline function that adds to earlier calls, but
  `ffm_output_options()` and the filter functions append too (`R/ffm.R:570-574`, `:1161`).
- (b) `?ffm_run` Value says the output has a `status` attribute on a non-zero exit, but
  `ffm_run()` stops before it returns (`R/ffm.R:1553-1554`, `:1733-1739`).
- (c) `?ffm_run` Value says the pipeline never runs through a shell, but `system2()` uses `sh`
  on Unix (`R/ffm.R:1555-1556`, the comment at `:1655`, `R/program_management.R:1607-1617`).
- (d) `?ffm_map` says a map is added beside `-map "[vout]"` for a function with several inputs,
  but `ffm_concat()` sets no `[vout]` map (`R/ffm.R:575-579`, `:1309-1327`).
- (e) `?ffm_seek` says `reencode = FALSE` is a fast, lossless copy, but without `ffm_copy()`
  FFmpeg re-encodes (`R/ffm.R:160`).
- (f) `?ffm_drop`'s description says it removes streams from the media file, but it removes
  them from the output only (`R/ffm.R:213-214`).
- (g) `?ffm_seek` says a copy cut moves to the nearest keyframe. Unsettled: FFmpeg may seek to
  the keyframe at or before the time (`R/ffm.R:161`).
- (h) `?ffm_run` says `tm_status` is 128 plus the signal number for a killed program.
  Unsettled: the code passes `system2()`'s status unchanged (`R/ffm.R:1569-1572`, `:1724`).
- (i) `?ffm_crop` says `x` and `y` take a positive number, but zero is accepted
  (`R/ffm.R:258-262`, `R/utils.R:307`).
- (j) `?ffm_hstack` and `?ffm_vstack` say `shortest` trims all videos to the shortest, but with
  `resize = TRUE` the graph has no `shortest` term (`R/ffm.R:778-779`, `:818-823`, `:848-849`).
- (k) `?ffm_loudnorm` says the filter compiles to `-af`, but after `ffm_hstack()`,
  `ffm_vstack()` or `ffm_overlay()` the compiled command has no audio filter and no error
  (`R/ffm.R:370`, `:1309-1340`).
- (l) `?ffm_loudnorm` says `linear = FALSE` leaves out the option so the dynamic behavior does
  not change, but FFmpeg's `linear` option defaults to true (`R/ffm.R:411-414`, `:486`).
- (m) `?ffm_crop`, `?ffm_scale`, `?ffm_fps`, `?ffm_drawbox` and `?ffm_overlay` describe their
  size and position arguments as numbers or FFmpeg expressions, but `check_dim()` refuses an
  integer such as `5L` and accepts any single string (`R/utils.R:295-318`).

Also in: a code comment that repeats one of these claims, and an edit to the series'
`NEWS.md` entry.

**Out:** the setup and audio page claims → M144. The task-verb page claims → M146. The code
bugs behind (j), (k), (l) and (m) → their own candidate rows in `cairn/ROADMAP.md`.

## Acceptance criteria

- [ ] AC1: For each claim (a) to (f) and (i) to (m) under Scope In, the source lines that
      claim names and every `man/*.Rd` page generated from those lines no longer make the
      claim, and the replacement text states what the code at head does, read against the
      code site that claim names.
- [ ] AC2: For claims (g) and (h), in place of AC1: the text of `?ffm_seek` about where a copy
      cut starts states the start position a run on this machine shows, and says how the end
      cut behaves only if a run shows it. The text of `?ffm_run` about `tm_status` for a killed
      program states the values that runs with a signal FFmpeg catches and with `SIGKILL` show,
      and the platform, or says the status is what `system2()` reports, which depends on the
      signal and the platform.
- [ ] AC3: Every hunk of `git diff <merge-base>..HEAD -- R/ man/` changes, or re-wraps within
      an edited paragraph, a roxygen line, a code comment, a string literal in a documentation
      helper whose output feeds only roxygen, or a generated Rd line, each carrying one of
      claims (a) to (m) or its replacement. No other R line changes.
- [ ] AC4: The series' `NEWS.md` bullet under `## Documentation` also names the functions or
      groups of functions this milestone corrects, no second bullet is added, and the bullet
      names no milestone.
- [ ] AC5: `devtools::document()` leaves no diff, `devtools::test()` passes, and
      `devtools::check()` gives 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T2
- AC3 → T1, T2, T3, T5
- AC4 → T4
- AC5 → T5

## Tasks

- [x] T1: Correct claims (a) to (f) in `R/ffm.R`, with the comment at `R/ffm.R:1655`.
- [x] T2: Run a copy cut with `ffm_seek()` and `ffm_copy()` on a fixture whose keyframes are
      known, with the start time nearer the following keyframe than the one before, and read
      the first packet's timestamp. Run `ffm_run()` on a program killed once with `SIGTERM` and
      once with `SIGKILL`. Record each result, the platform and the FFmpeg version in the work
      log, then correct claims (g) and (h).
- [x] T3: Correct claims (i) to (m) in `R/ffm.R`. The text for (j), (k), (l) and (m) says what
      the code does now and does not promise the fix.
- [x] T4: Edit the series' `NEWS.md` bullet to name these pages. Avoid the phrases that
      `test-timeout-silence.R` fences out of `NEWS.md`.
- [x] T5: Run `devtools::document()`, then the full `devtools::test()` and `devtools::check()`.
      Read each changed sentence against its code site and record one line per claim in the
      work log.

## Work log

- 2026-10-18: created by /milestone-plan.
- 2026-10-18: tier check: not run — work named at the question set.
- 2026-10-18: feature series: M144 -> M145 -> M146
- 2026-10-18: plan gate chose text-only correction over changing the code to match the text, at the user's choice; falsified by a corrected page that must describe a behavior no caller can rely on.
- 2026-10-18: criteria audit (full mode, fresh Opus reader; logged in full in M144) findings for M145, fixed: AC2 replaces AC1 for (g) and (h); (h) no longer says "FFmpeg decides", since the status comes from the OS, `sh` and `system2()`, and T2 runs a caught signal and `SIGKILL`; (g)'s run puts the start nearer the next keyframe so it can tell "nearest" from "at or before"; AC3 allows doc-helper literals and re-wrapping; AC4 names functions or groups and no milestone.
- 2026-10-18: absorbs the M128 and M131 parts of candidate row "Six help-text claims on the `ffm_*()` input, output and run pages that were false before M128, kept as they were.".
- 2026-10-18: implement started; branch m145-pipeline-help-claims cut from master at 9c857bd2; tree clean.
- 2026-10-18: T1 done: (a) `?ffm_map` names the filter functions and `ffm_output_options()` as appending and `ffm_seek()`, `ffm_drop()`, `ffm_codec()`, `ffm_pixel_format()`, `ffm_copy()` as replacing; (b) `?ffm_run` Value says a non-zero exit gives an error; (c) it says each argument is `shQuote()`d and `system2()` uses a shell on Unix, with the comment above the `run_program()` call fixed; (d) the `[vout]` sentence names `ffm_hstack()`, `ffm_vstack()`, `ffm_overlay()`, the three functions that set `complex`; (e) `?ffm_seek` says `reencode = FALSE` is a copy only with `ffm_copy()`; (f) `?ffm_drop` says the streams leave the output file. Full `devtools::test()` passed.
- 2026-10-18: T2 runs, macOS 27.0 arm64, FFmpeg 9.0.2. Copy cut: fixture 10 s testsrc, libx264, keyframes at 0/2/4/6/8 s; `ffm_seek(start = 3.8, end = 7, reencode = FALSE) |> ffm_copy()` gave 125 packets, 5.000 s, first frame's MD5 equal to the source frame at 2 s (not 4 s or 3.8 s), last packet 4.96 s after the shift; so the start goes to the keyframe at or before `start` and the end stays at `end`. Signals: `ffm_run()` on a 120 s veryslow encode, `sh` parent of `ffmpeg`; SIGTERM to `ffmpeg` gave `tm_status` 255 ("Exiting normally, received signal 15"); SIGKILL gave 137. (g) and (h) corrected with those values, the platform and the date.
- 2026-10-18: T3 done: (i) `?ffm_crop` `x`/`y` say zero or more; (j) `?ffm_hstack`/`?ffm_vstack` `shortest` say it does nothing with `resize = TRUE`; (k) `?ffm_loudnorm` says the audio filter is absent after a stack or overlay; (l) `linear = FALSE` text says FFmpeg's default is true; (m) crop, scale, fps, drawbox, overlay say a double or a string, an integer refused, a string unchecked. Each read by a run of the code at head (integer `5L` and `30L` refused, a free string compiled, resize graph has no `shortest`, no `-af` after `ffm_hstack()`/`ffm_overlay()`, no `[vout]` after `ffm_concat()`).
- 2026-10-18: implementation choice: (m) is stated once per page in a Details paragraph for crop, scale and drawbox, in the parameter text for fps, in the existing Details paragraph for overlay; no doc helper added, so AC3's no-other-R-line rule holds.
- 2026-10-18: T4 done: the series `NEWS.md` Documentation bullet adds one sentence naming the twelve `?ffm_*` pages; no fenced phrase used.
- 2026-10-18: T1 to T4 share one checkpoint commit, because the edits in `R/ffm.R` interleave across tasks.
- 2026-10-18: checkpoint: the full `devtools::test()` rerun over T1 to T4 was still running at this commit; T5 records its result.
- 2026-10-18: the `devtools::test()` rerun over T1 to T4 passed with no failures.
- claim audit: 35 claims read, 2 corrected — R/ffm.R, man/ffm_*.Rd, NEWS.md (fresh Opus reader; `?ffm_map` now says `ffm_copy()` with `streams = TRUE` refuses a mapping other than "0" instead of replacing it; `?ffm_loudnorm` now says the audio filter is absent with a stack or overlay in either call order; the reader re-read both once: true).
- 2026-10-18: claim audit side finding (a SIGTERM that also reaches the `sh` from `system2()` lets `ffm_run()` return with no error) recorded as candidate row "`ffm_run()` signal to the shell"; outside this milestone's scope.
- 2026-10-18: T5 done at 45494c83: `devtools::document()` left no diff; full `devtools::test()` had no failures; `devtools::check()` gave 0 errors, 0 warnings, 0 notes (the test-output NOTE inside it is the spelling list's pre-existing "encodable" and "filesystem").
- 2026-10-18: T5 per-claim read against the code at head: (a) appending and replacing verbs match each function's assignment; (b) `ffm_run()` aborts on a status; (c) `run_program()` `shQuote()`s and `system2()` runs `sh` on Unix; (d) only hstack/vstack/overlay set `complex`; (e) `-ss`/`-to` go before `-i`, copy only with `ffm_copy()`; (f) `-vn`/`-an`/`-sn`/`-dn` are output options; (g) and (h) pinned to the T2 runs; (i) `check_dim(inclusive = TRUE)` takes 0; (j) resize graph has no `shortest`; (k) the `complex` branch never reads `filter_audio`; (l) `linear` default true in FFmpeg 9.0.2; (m) `is_double(n = 1)` or `is_character(n = 1)`.
- 2026-10-18: status review; handing to /milestone-review.

## Decisions

## Review
