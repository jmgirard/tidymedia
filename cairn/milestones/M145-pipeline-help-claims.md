# M145: The `ffm_*()` pipeline help pages state what the code does

- **Status:** planned
- **Priority:** normal
- **Depends on:** M144
- **Driving RR:** —
- **Principles touched:** GP2
- **Resolves:** —
- **Surface tier:** user-facing — help pages that package users read
- **Branch/PR:** —

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

- [ ] T1: Correct claims (a) to (f) in `R/ffm.R`, with the comment at `R/ffm.R:1655`.
- [ ] T2: Run a copy cut with `ffm_seek()` and `ffm_copy()` on a fixture whose keyframes are
      known, with the start time nearer the following keyframe than the one before, and read
      the first packet's timestamp. Run `ffm_run()` on a program killed once with `SIGTERM` and
      once with `SIGKILL`. Record each result, the platform and the FFmpeg version in the work
      log, then correct claims (g) and (h).
- [ ] T3: Correct claims (i) to (m) in `R/ffm.R`. The text for (j), (k), (l) and (m) says what
      the code does now and does not promise the fix.
- [ ] T4: Edit the series' `NEWS.md` bullet to name these pages. Avoid the phrases that
      `test-timeout-silence.R` fences out of `NEWS.md`.
- [ ] T5: Run `devtools::document()`, then the full `devtools::test()` and `devtools::check()`.
      Read each changed sentence against its code site and record one line per claim in the
      work log.

## Work log

- 2026-10-18: created by /milestone-plan.
- 2026-10-18: tier check: not run — work named at the question set.
- 2026-10-18: feature series: M144 -> M145 -> M146
- 2026-10-18: plan gate chose text-only correction over changing the code to match the text, at the user's choice; falsified by a corrected page that must describe a behavior no caller can rely on.
- 2026-10-18: criteria audit (full mode, fresh Opus reader; logged in full in M144) findings for M145, fixed: AC2 replaces AC1 for (g) and (h); (h) no longer says "FFmpeg decides", since the status comes from the OS, `sh` and `system2()`, and T2 runs a caught signal and `SIGKILL`; (g)'s run puts the start nearer the next keyframe so it can tell "nearest" from "at or before"; AC3 allows doc-helper literals and re-wrapping; AC4 names functions or groups and no milestone.
- 2026-10-18: absorbs the M128 and M131 parts of candidate row "Six help-text claims on the `ffm_*()` input, output and run pages that were false before M128, kept as they were.".

## Decisions

## Review
