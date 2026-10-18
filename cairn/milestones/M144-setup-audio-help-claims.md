# M144: The setup, capability and audio help pages state what the code does

- **Status:** review
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

- [x] AC1: For each claim (a) to (c) and (f) to (k) under Scope In, the source lines that
      claim names and every `man/*.Rd` page generated from those lines no longer make the
      claim, and the replacement text states what the code at head does, read against the
      code site that claim names. For (d), within the `audio_stream` item of
      `man/normalize_audio.Rd` and of `man/normalize_audio_batch.Rd`, of the sentences that a
      search for `NULL` in that item finds, the only ones that say which tracks `NULL`
      selects are the `\code{NULL} (default)` sentence and the shared family sentence.
- [x] AC2: Claim (e)'s replacement states the value that `mediainfo_parameter()` returns
      for a section the file lacks under `typed = TRUE` and under `typed = FALSE`, each as
      the code at `R/mediainfo.R:120-124` gives it and as a run on this machine shows it.
- [x] AC3: Every hunk of `git diff <merge-base>..HEAD -- R/ man/` changes, or re-wraps within
      an edited paragraph, a roxygen line, a code comment, a message string, a string literal
      in a documentation helper whose output feeds only roxygen, or a generated Rd line, each
      carrying one of claims (a) to (k) or its replacement. No other R line changes.
- [x] AC4: `NEWS.md` has one bullet under a `## Documentation` heading in the development
      version section that names the corrected functions or groups of functions, and it
      names no milestone.
- [x] AC5: `devtools::document()` leaves no diff, `devtools::test()` passes, and
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
- [x] T2: Run `mediainfo_parameter()` on a file without the section, under `typed = TRUE` and
      `typed = FALSE`. Record the results and the MediaInfo version in the work log, then
      correct claims (c), (d) and (e). If a result differs from what the code path promises,
      add a candidate row for that code bug.
- [x] T3: Correct claims (h) to (k) in `R/task-doc.R` and `R/ffmpeg.R`, with the refusal message
      and the `hardware_param()` copy of (j). Grep `tests/` for each changed phrase and keep
      any test that pins it passing.
- [x] T4: Grep `R/` for a distinctive phrase of each claim. Add one candidate row listing any
      copy outside the source lines Scope In names, such as the "first such call" sentence in
      `hardware_probe_sentences()` that repeats claim (b) on about 16 pages.
- [x] T5: Add the series' `## Documentation` bullet to `NEWS.md`. Avoid the phrases that
      `test-timeout-silence.R` fences out of `NEWS.md`.
- [x] T6: Run `devtools::document()`, then the full `devtools::test()` and `devtools::check()`.
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
- 2026-10-18: T2 done. (e) MediaInfoLib v26.10 prints one empty line for `--Inform=Audio;%Format%` on a video with no audio; `mediainfo_parameter()` returned `NA` (logical) under `typed = TRUE` and `""` under `typed = FALSE`, the same as a missing parameter, so no code bug and no row. (c) `extract_audio()` and `convert_audio()` on that file each stopped with `tidymedia_ffmpeg_exit`, so `?audio_stream` now names all four audio-output functions. (d) the `normalize_one_track` extra no longer restates the `NULL` reading. The audio tests' 9 warnings come from multi-track fixtures in code this branch does not touch.
- 2026-10-18: checkpoint, T3 to T5 edits made and not yet ticked: the related test run is still going. (j) FFmpeg 9.0.2 refused `libx265` into `.webm` (exit 234, "Only VP8 or VP9 or AV1 video") and wrote `libsvtav1` there. T4's grep found one copy elsewhere, `hardware_probe_sentences()` repeating claim (b), now the "Hardware probe sentence" candidate row.
- 2026-10-18: full `devtools::test()` before the audit fixes: FAIL 0, WARN 12, SKIP 7, PASS 18801.
- claim audit: 44 claims read, 9 corrected — R/cache.R, R/timeout.R, R/program_management.R, R/audio-stream-doc.R, R/ffmpeg.R, R/mediainfo.R, NEWS.md. None false; fixed: the NEWS `.webm` line (now scoped to `hardware`), "batch task functions" (now seven, as `jobs_return()` serves), "audio track check" (now the dropped-track warning), install saves only usable programs, the option skip scoped to picking the encoder, the refusal message's NULL advice, "for this reason" order, the `with_options()` sentence, and the stale MediaInfo comment at `R/mediainfo.R:121`. Not changed: the glossary pointer for "family", which the pages already carry; 6 external-behavior claims rest on the recorded runs above.
- 2026-10-18: T6 done. `devtools::check()` after the audit fixes: 0 errors, 0 warnings; R CMD check's one NOTE is the `spelling.Rout` mismatch. It flagged `install's` (this branch, reworded, and the install tests then passed 931/931), and `encodable` (from 59375e76) and `filesystem` in `NEWS.md`, both on master before this branch. `devtools::document()` leaves no diff. Status set to review.
- 2026-10-18: amendment routed: AC1 — "For (d), those pages state the `NULL` reading once" cannot hold: every `audio_stream` page states the reading in its opening sentence and in the shared family sentence by design, and claim (d) named only the third, restating sentence.
- re-audit: AC1 (full) — 3 findings, all fixed in the wording: the draft named the item's opening sentence, which does not state the reading (the `\code{NULL} (default)` sentence does); "no third statement" named no procedure, now a search for `NULL` in each item; "no longer states the reading" was reader-dependent.
- re-audit: AC1 (full) — no blocking finding; HEAD meets it (`normalize_audio.Rd:82` and `normalize_audio_batch.Rd:106`: the default sentence and the first-track family sentence; the other `NULL` hits name no tracks). Two minor points answered without change, since a further rewording would need a third reader: "the shared family sentence" is singular where the helper gives two sentences (the first-track one is the one that names `normalize_audio()`), and "the only ones" would still hold with the default sentence deleted (a narrower promise, kept).
- 2026-10-18: amendment return: AC1 — "For (d), within the `audio_stream` item of `man/normalize_audio.Rd` and of `man/normalize_audio_batch.Rd`, of the sentences that a search for `NULL` in that item finds, the only ones that say which tracks `NULL` selects are the `\code{NULL} (default)` sentence and the shared family sentence."

## Decisions

## Review

Pass 1, 2026-10-18, on branch head 53e8baf4 (master 95495a4a, not moved since the cut).

- AC1: fails as written for (d), the other ten claims pass. `git diff master...HEAD -- R/` read claim by claim: (a), (b), (c), (f), (g), (h), (i), (j) (all three copies), (k) no longer make their claims and the replacements match the code sites the plan names; a grep of `man/*.Rd` for the eight old phrases finds none. (d): the `normalize_one_track` text no longer restates the `NULL` reading, but `?normalize_audio` still states it in the item's opening sentence and in the shared family sentence, so "state the `NULL` reading once" is not met. Those two statements are on every `audio_stream` page by design, so the criterion promises more than the finding asked: amendment routed.
- AC2: met. `mediainfo_parameter()` on a video with no audio, section `"Audio"`: `NA` under `typed = TRUE`, `""` under `typed = FALSE` (rerun at review, MediaInfoLib v26.10); the page states both, matching `R/mediainfo.R:120-126`.
- AC3: met. Every hunk of `git diff master...HEAD -- R/` is a roxygen line, a comment, the one `cli_abort()` message string at `R/ffmpeg.R:3810-3813`, or a string literal in `R/task-doc.R` or `R/audio-stream-doc.R`, each carrying a listed claim or its replacement; the 21 changed `man/*.Rd` files are the pages generated from those sources.
- AC4: met. `NEWS.md` line 71 opens `## Documentation` in the development version section with one bullet naming the corrected pages and function groups; no milestone id.
- AC5: pending, a fresh `devtools::check()` is running.

Pass 2, 2026-10-18, on head 5b643faa after the AC1 amendment; `git diff 53e8baf4 5b643faa -- R man NEWS.md` is empty, so pass 1's AC2-AC4 evidence and the running check cover this head.

- AC1: met. Pass 1's reading of (a)-(c) and (f)-(k) stands. (d): splitting the `audio_stream` item into sentences and keeping those with `NULL` gives, in `man/normalize_audio.Rd`, the `\code{NULL} (default)` sentence, the first-track family sentence and "(default = \code{NULL})"; in `man/normalize_audio_batch.Rd` the same plus "An \code{NA} cell ... means \code{NULL} for that row". Only the first two name tracks.
- AC2: met (pass 1 evidence).
- AC3: met (pass 1 evidence).
- AC4: met (pass 1 evidence).
- spawned: diff-bug, blame-history, prior-review
- diff-bug #1: "The two-pass analysis measures each audio track" is false, since the analysis maps one track (`R/loudnorm_two_pass.R:45`), and the branch made the clause its own sentence — fix now: sentence deleted.
- diff-bug #2: the NEWS bullet reads as changed behavior, omits the refusal message and AV1's `nvenc`-only limit — fix now: says "help text", names the message and the limit.
- diff-bug #3: "a family it takes" omits the backend's own families, so `videotoolbox` has no `.webm` route — reject, false: both texts say AV1 works only under `"nvenc"`; prores is outside the claim.
- diff-bug #4: the refusal message's "(AV1, under nvenc)" reads as applying to every non-H.264 container — fix now: the AV1 limit is its own sentence about `.webm`.
- diff-bug #5: Details omits `has_hardware_encoder()` from the first askers — follow-up, predates the branch: added to the "Hardware probe sentence" row.
- diff-bug #6: the `@param` head "The EBU R128 loudness targets" on `?normalize_audio_batch` still calls `loudness_range` an R 128 target — fix now: "The loudness targets".
- diff-bug #7: three added roxygen lines over 80 columns — fix now: re-wrapped (`R/mediainfo.R`, `R/program_management.R`, `R/task-doc.R`).
- diff-bug #8: the `?install_on_win` Value sentence is a long comma chain and does not say earlier programs stay saved when a later save fails — fix now for the wording (moved to an Errors bullet, prior-review #2); the partial-save half is follow-up, already item (c) of "Location folder edges after M141".
- diff-bug #9: the page says `NA` for "more than one line" where the code gives `NA` for anything but one line — fix now: "does not print exactly one line".
- diff-bug #10: Scope (j) says five task pages render `hardware_param()`; four do (`compare_videos`, `crop_video`, `picture_in_picture`, `segment_video`) plus `?hardware_encoder` — noted here as the correction; `?standardize_video`'s `.webm` advice with `hardware` is follow-up, added to the "Hardware probe sentence" row.
- blame-history #1: the fork text contradicts D044 and D050, which say every worker loads fresh — fix now: D106 corrects both claims and leaves their rules.
- blame-history #2: the `""` promise may depend on the MediaInfo version — reject, false as stated: no build was observed printing nothing; the page now says any output but one line is `NA` (diff-bug #9).
- blame-history #3: "read `with_options()` in place of `with_timeout()`" is unclear — fix now: says when the third case happens for `local_options()`.
- blame-history #4: the `NULL`-first-track sentence was removed — reject, planned change (claim (d)).
- blame-history #5: dated measurement stamps in code comments — reject, planned change: the derived-figures rule asks a figure to be pinned to its date.
- blame-history #6: two lines over 80 columns — fix now with diff-bug #7.
- prior-review #1: the manifest sentence lost the `run = TRUE` condition that M133's review O1 restored, and the manifest is attached only inside `if (run)` (`R/ffm_batch.R:190`, `:343`) — fix now: "A run also attaches a provenance manifest".
- prior-review #2: the `?install_on_win` Value sentence ran to 31 words and put an error class in Value, against the plain-docs rules — fix now: Value is back to its base sentence and the `set_program()` errors are a bullet in Errors.
- The stale `devtools::check()` on 53e8baf4 was stopped before it finished; AC5 runs on the fixed head.
- Fix-now commits: diff-bug #1, #2, #4, #6, #7, #8 (wording), #9; blame-history #1, #3, #6; prior-review #1, #2, fixed aca7db40. After them AC1 (d)'s `NULL` sentences and AC3's hunk kinds were re-read: unchanged and met.
- AC5: met on aca7db40. `devtools::document()` leaves no diff; `devtools::test()`: FAIL 0, WARN 12, SKIP 7, PASS 18801; `devtools::check()`: 0 errors, 0 warnings, 0 notes in devtools' summary. R CMD check's own "1 NOTE" is the `spelling.Rout` mismatch over `encodable` and `filesystem`, both on master before the branch.
- Gate: `cairn_validate.py` all checks passed; no principle changed, so `cairn_impact` was skipped; no new top-level file; README and `_pkgdown.yml` untouched; NEWS has the entry (AC4).
