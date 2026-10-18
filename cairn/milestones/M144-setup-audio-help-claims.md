# M144: The setup, capability and audio help pages state what the code does

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — help pages that package users read
- **Branch/PR:** m144-setup-audio-help-claims

## Goal

Eleven help-page claims that the M127 to M133 claim audits found false now say what the code does.

## Scope

**In:** correct the text of these claims. Line numbers are as read on 2026-10-18.

- (a) `?refresh_ffmpeg_capabilities` "Parallel workers" says a worker never gets the session's
  record, but a forked worker (`future::plan(multicore)`) starts with it (`R/cache.R:92-96`, and
  the comment at `R/cache.R:18-21`).
- (b) `?refresh_ffmpeg_capabilities` Details says the first `hardware` call always asks FFmpeg,
  but the `tidymedia.hardware_encoders` option, a missing FFmpeg, or an earlier read can skip
  the ask (`R/cache.R:58-62`; code at `R/ffmpeg.R:3165-3176`, `:3254`, `:3311-3313`).
- (c) `?audio_stream` names only `separate_audio_video()` and `normalize_audio()` as failing on
  an input with no audio, but `extract_audio()` and `convert_audio()` fail too
  (`R/audio-stream-doc.R:216-219`).
- (d) `?normalize_audio` and `?normalize_audio_batch` state the `NULL` reading of
  `audio_stream` a second time (`R/audio-stream-doc.R:167-168`).
- (e) `?mediainfo_parameter` does not say what a known section the file lacks returns
  (`R/mediainfo.R:65-70`, code at `:120-124`).
- (f) `?install_on_win` Value reads the Errors list as complete, but the `set_program()` call
  inside it can stop with three classes the list does not name (`R/program_management.R:400-437`).
- (g) `?local_timeout` says "the first two cases", which implies the third does not apply to
  `withr::local_options()`, and it does (`R/timeout.R:344-346`, measured at `:424-428`).
- (h) The dropped-track paragraph on the six `extract_audio()`, `convert_audio()` and
  `normalize_audio()` pages says the check is otherwise skipped silently, but a probe that
  reaches its time limit gives a `tidymedia_probe_timeout` warning (`R/task-doc.R:315`,
  `R/ffprobe.R:318-331`).
- (i) The `jobs_return()` text on seven batch pages lists a provenance manifest beside the
  `success` column, but the manifest is an attribute (`R/task-doc.R:364-366`,
  `R/ffm_batch.R:343-346`).
- (j) `?hardware_encoder`, the `hardware_param()` text that five task pages share, and the
  refusal message built at `R/ffmpeg.R:3810-3811` say a container that does not take H.264,
  such as `.webm`, needs an HEVC-family or AV1-family `video_codec`, but WebM does not take
  HEVC (`R/ffmpeg.R:3040-3042`, `R/task-doc.R:66-68`, `R/ffmpeg.R:1722-1724`).
- (k) `?normalize_audio_batch` lists `loudness_range = 7` with the EBU R 128 defaults, which
  `?normalize_audio` already corrected (`R/ffmpeg.R:5384-5388`, `:2462-2464`).

Also in: a code comment or message string that repeats one of these claims, and one
`NEWS.md` entry for the series.

**Out:** the `ffm_*()` pipeline page claims → M145. The task-verb page claims → M146.
Code changes for behavior the text describes better than the code has → one candidate row
each, added to `cairn/ROADMAP.md`. The series' changelog entry starts here and M145 and M146
edit it.

## Acceptance criteria

- [ ] AC1: For each claim (a) to (c) and (f) to (k) under Scope In, the source lines that
      claim names and every `man/*.Rd` page generated from those lines no longer make the
      claim, and the replacement text states what the code at head does, read against the
      code site that claim names. For (d), those pages state the `NULL` reading once.
- [ ] AC2: Claim (e)'s replacement states the value that `mediainfo_parameter()` returns
      for a section the file lacks under `typed = TRUE` and under `typed = FALSE`, each as
      the code at `R/mediainfo.R:120-124` gives it and as a run on this machine shows it.
- [ ] AC3: Every hunk of `git diff <merge-base>..HEAD -- R/ man/` changes, or re-wraps within
      an edited paragraph, a roxygen line, a code comment, a message string, a string literal
      in a documentation helper whose output feeds only roxygen, or a generated Rd line, each
      carrying one of claims (a) to (k) or its replacement. No other R line changes.
- [ ] AC4: `NEWS.md` has one bullet under a `## Documentation` heading in the development
      version section that names the corrected functions or groups of functions, and it
      names no milestone.
- [ ] AC5: `devtools::document()` leaves no diff, `devtools::test()` passes, and
      `devtools::check()` gives 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T2
- AC3 → T1, T2, T3, T6
- AC4 → T5
- AC5 → T6

## Tasks

- [x] T1: Correct claims (a), (b), (f) and (g) in `R/cache.R`, `R/program_management.R` and
      `R/timeout.R`, with the cache comment at `R/cache.R:18-21`.
- [ ] T2: Run `mediainfo_parameter()` on a file without the section, under `typed = TRUE` and
      `typed = FALSE`. Record the results and the MediaInfo version in the work log, then
      correct claims (c), (d) and (e). If a result differs from what the code path promises,
      add a candidate row for that code bug.
- [ ] T3: Correct claims (h) to (k) in `R/task-doc.R` and `R/ffmpeg.R`, with the refusal message
      and the `hardware_param()` copy of (j). Grep `tests/` for each changed phrase and keep
      any test that pins it passing.
- [ ] T4: Grep `R/` for a distinctive phrase of each claim. Add one candidate row listing any
      copy outside the source lines Scope In names, such as the "first such call" sentence in
      `hardware_probe_sentences()` that repeats claim (b) on about 16 pages.
- [ ] T5: Add the series' `## Documentation` bullet to `NEWS.md`. Avoid the phrases that
      `test-timeout-silence.R` fences out of `NEWS.md`.
- [ ] T6: Run `devtools::document()`, then the full `devtools::test()` and `devtools::check()`.
      Read each changed sentence against its code site and record one line per claim in the
      work log.

## Work log

- 2026-10-18: created by /milestone-plan.
- 2026-10-18: tier check: not run — work named at the question set.
- 2026-10-18: question set: what to plan — the help-page corrections in the two help-text candidate rows.
- 2026-10-18: question set: text only or code too — text only, each behavior bug becomes its own candidate row.
- 2026-10-18: question set: plan all parts now or the first only — all now, as M144, M145 and M146.
- 2026-10-18: feature series: M144 -> M145 -> M146
- 2026-10-18: plan gate chose text-only correction over changing the code to match the text, at the user's choice; falsified by a corrected page that must describe a behavior no caller can rely on.
- 2026-10-18: plan gate chose a split by page domain (setup and audio, `ffm_*()` pipeline, task verbs) over one milestone per candidate row, because D093 sizes a help milestone at what one reviewer can read against the code; falsified by a review that cannot read one part's sentences in one pass.
- 2026-10-18: three Haiku Explore readers checked all 34 listed claims at head: 33 still false, and the `?anonymize_video` copy of the FLAC sentence is gone. Four code bugs found; the plan commit adds one candidate row for each.
- 2026-10-18: criteria audit (full mode, fresh Opus reader) returned 10 findings across M144-M146, all fixed: AC3 now allows doc-helper string literals and re-wrapping; AC1 is bounded to the named source lines, and T4 rows any copy elsewhere (the `hardware_probe_sentences()` copy of (b), about 16 pages, which would break D093's size); AC1 states (d) as "once" and leaves (e) to AC2; AC2 runs both `typed` values; AC4 names functions or groups; M145 and M146 fixes are logged there.
- 2026-10-18: decided no new tests: the deliverable is help text, as in M127 to M134, and AC5 runs the existing documentation guards.
- 2026-10-18: absorbs candidate rows "Nine help-text claims M127's round-4 review left open, most on text M129 and M130 rewrite." in full and the M132 and M133 parts of "Six help-text claims on the `ffm_*()` input, output and run pages that were false before M128, kept as they were.".
- 2026-10-18: implement started on branch m144-setup-audio-help-claims, cut from master at 95495a4a.
- 2026-10-18: T1 done. (a) a run showed a `plan(multicore)` worker sees a filled memo and a `plan(multisession)` worker does not (future 1.75.0, R 4.6.1); the page and the comment at `R/cache.R:18-21` now say so. (b) Details names the three skips: a filled record, the option for a `hardware` call, no FFmpeg for `has_encoder()` (`R/ffmpeg.R:3163-3176`, `:3254`). (f) Value says the closing `set_program()` step can give its own errors, naming `tidymedia_location_not_written` (`R/program_management.R:867`). (g) a run showed `withr::local_options()` inside `withr::with_options()` leaves the outer value (withr 3.0.3), so the page says all three cases apply.

## Decisions

## Review
