# M127: ?tidymedia and the setup, metadata, timeout and batch help pages read as plain English

- **Status:** review
- **Priority:** high
- **Depends on:** M126
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — shipped help pages, including the package's landing page
- **Branch/PR:** `m127-plain-setup-help-pages`

## Goal

The M127 help-page domain, including `?tidymedia`, uses plain English for an R user who does not know FFmpeg.

## Scope

**In:** the roxygen text behind the M127 domain in `cairn/references/plain-docs.md` (28 pages on 2026-09-13). That includes `?tidymedia`, whose timeout detail moves to `?with_timeout` and `?local_timeout`. It also includes the generated audio-stream sentences in `R/audio-stream-doc.R` that other pages reuse. The tests that pin wording on these pages are in scope too.

**Out:** the `ffm_*()` pages go to M128. The task-function pages go to M129 and M130. Code comments in `R/` stay as they are. `NEWS.md` gets no entry (D091).

## Acceptance criteria

- [x] AC1: The prose sweep over the M127 domain prints no sentence that matches a maintainer term.
- [x] AC2: The prose sweep over the M127 domain prints no sentence over 25 words.
- [ ] AC3: For each page in the domain, take each glossary stem found in its `--prose` output. The page defines the term at its first use or names the glossary in `vignette("tidymedia")`. One ledger row per page records the stems and how each is met.
- [x] AC4: `tools::Rd2txt()` output for `man/tidymedia-package.Rd` is at most 80 lines. Its first paragraph says what the package does and names `vignette("tidymedia")` as the place to start.
- [x] AC5: Every match of `\btidymedia[._][a-z_.]+` or `\btm_[a-z_]+` in the 28 domain pages at the base commit (`264afff4`) is found as a whole word in some `man/*.Rd` file at head.
- [x] AC6: `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T2, T3, T4, T5, T6, T7
- AC2 → T2, T3, T4, T5, T6, T7
- AC3 → T2, T3, T4, T5, T6, T7
- AC4 → T2
- AC5 → T1, T7
- AC6 → T7

## Tasks

- [x] T1: Run the sweep over the domain at the base commit. Record the pages and the AC5 identifiers in a new M127 ledger section.
- [x] T2: Rewrite `R/tidymedia-package.R`. Keep a short overview, where to start, and the session options. Move the timeout detail to `R/timeout.R` and the error-class lists to short end sections.
- [x] T3: Rewrite `R/timeout.R`. Keep what a user sets and what a user sees. Move measured timings to code comments.
- [x] T4: Rewrite `R/program_management.R`, with `install_on_win()` and `program_status()` first.
- [x] T5: Rewrite `R/mediainfo.R`, `R/ffprobe.R` and `R/audio-stream-doc.R`. For the generated sentences, check the task-function pages that reuse them after `devtools::document()`.
- [x] T6: Rewrite `R/ffm_batch.R`, `R/ffm_jobs.R`, `R/cache.R`, `R/verify.R`, `R/ffm_manifest.R`, `R/utils-tidy-eval.R` and `R/ffm_oop.R`. In every `R/` file, rename the `@family` labels "escape hatch functions", "builder functions" and "task verb functions" to the names in rule 5.
- [x] T7: A test that pins changed wording now pins the new wording of the same property. A test whose claim left the user docs is removed, with a ledger row. A test named in a `cairn/DECISIONS.md` entry is rewritten, never removed. Fill the ledger. Run the sweep, `devtools::document()`, `devtools::check()`, `devtools::test()` with no other R session working, and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-13: created by /milestone-plan (series M126-M130). The criteria audit and gate choices are logged in M126's work log.
- 2026-09-13: implement gate: class lists split, old settings location on one page, sweep gaps not fixed here (see Decisions).
- 2026-09-13: minor amendment: each of T2-T6 updates the tests that pin its own pages, because the verify slot needs `devtools::test()` clean per task. T7 keeps the final sweep and the checks.
- 2026-09-13: T1: base sweep over 28 pages printed 185 findings. Ledger section `### M127` added to `cairn/references/plain-docs.md` with the pages and 27 identifiers.
- 2026-09-13: T2 and T3 in one commit, because the timeout text and its tests moved between the two files. `?tidymedia` renders 72 lines. The sweep is clean on `tidymedia-package`, `with_timeout` and `local_timeout`. Four test files re-pinned to the new page or wording (package-topic, check-tracks-docs, runtime-timeout, timeout-silence) and `helper-rd.R` now reads `?with_timeout`. None removed. Full suite: 1738 tests, 0 failed, 5 skipped.
- 2026-09-13: T4, T5 and T6 delegated to three [O] agents in separate worktrees and merged. T4 moved blocks in `R/program_management.R`, and parsing both versions shows the function bodies unchanged. T5 rewrote the generated audio argument text, which lands on 25 task function pages, and re-pinned two phrases in `test-audio-index-docs.R`. The `@family` labels are renamed in 7 `R/` files. The sweep over the 28 pages exits 0. Checkpoint: T4-T6 stay unticked until the full suite passes on this commit.
- 2026-09-13: T4-T6: full suite on the merged code, 1738 tests, 0 failed, 5 skipped. T4-T6 ticked.
- 2026-09-13: T7: sweep over 28 pages exits 0; `?tidymedia` renders 72 lines; all 27 base identifiers found in `man/`; 8 pages with glossary stems all name the glossary. Ledger filled. Check and pkgdown check running.
- 2026-09-13: T7: on `19b59ac5`, `devtools::check()` 0 errors, 0 warnings, 0 notes; `pkgdown::check_pkgdown()` no problems.
- 2026-09-13: claim audit: 240 claims read, 8 corrected — R/ffprobe.R, R/mediainfo.R, R/timeout.R, R/ffm_batch.R, R/tidymedia-package.R, R/program_management.R
- 2026-09-13: the 8 corrections: direct commands return standard output only; `mediainfo_parameter()` warns only for missing or timed-out files; the time limit applies per program plus the stop delay (3 sites); the track check skips one call per distinct input with no `audio_stream`; `program_status()` warnings name a location or a file; `install_on_win()` names only the first case of leftovers, created folders or your files. Sweep still exits 0; `?tidymedia` renders 73 lines. Final check re-running.
- 2026-09-13: claim audit re-read: the same reader found all 8 corrections correct.
- 2026-09-13: T7 done. On `2e04a4be`: `devtools::check()` 0 errors, 0 warnings, 0 notes (tests included); `devtools::document()` leaves `man/` unchanged. Agent worktrees removed. Status set to review.
- 2026-09-13: review checkpoint: AC1-AC5 evidence recorded and ticked. AC6 check and the three reviewers still running.
- 2026-09-13: review return 1 (defect): AC6 fails, because `devtools::check()` on `ca35647a` ends "Status: 1 NOTE". The spelling test flags `dplyr` at `probe_all.Rd:45`. AC5 fails, because one fact left `?tidymedia` with no ledger row (reviewer O4). Status back to in-progress. See the Review section for the fixes to make.
- 2026-09-13: return gate (user's choice): fix both defects and findings O1-O15 in this pass. O16 stays, because decision records are history.
- 2026-09-13: S1 fixed: `dplyr` is a code span again in `R/ffprobe.R`. O4 fixed: `?tidymedia` Session options says when the track check runs, and that a bad value gives an error naming the option. Measured with `"yes"`: an error on a run with no `audio_stream`, none with `run = FALSE`. Ledger rows for `tidymedia-package` and `with_timeout` corrected in place.
- 2026-09-13: O1, O2, O5, O9 on `?with_timeout`. `segment_video()` moved to the warning list, because it runs through `ffm_batch()`. The check-order paragraph is back, with `anonymize_video_batch()`'s `pixel_format` as its example (measured to lose to a 0.5 limit). The `output` column is not named, because it is checked first at head (measured). The `NULL` exception is stated. Four functions from `tm_timeout_reached_master()` were added to the lists. The comment above `resolve_timeout()` now names `?with_timeout`.
- 2026-09-13: O6-O8, O12-O15. The `ffm_batch` text says how long R waits. The exit-class item names task functions. `?local_timeout` points its restore claim at the exceptions, and the `.local_envir` text is scoped. Measured: a returned function's environment and `new.env()` both leave the limit set with no condition. Two grammar slips and three roxygen lines over 80 characters fixed.
- 2026-09-13: O3, O10, O11: three doc guards changed so they can fail. Each went red on a planted Rd defect, and the Rd files were then restored. The claim audit was not re-run. Each new claim was measured or read against the test census.
- 2026-09-13: on the committed tree: the sweep over 28 pages exits 0, and `?tidymedia` renders 78 lines. `devtools::check()` gives 0 errors, 0 warnings, 0 notes, with the spelling comparison OK and tests included. `devtools::document()` leaves `man/` unchanged. `pkgdown::check_pkgdown()` finds no problems. Status set to review.
- 2026-09-13: review round 2 on `629d68a4`: AC1-AC4 and AC6 pass with fresh evidence. AC5 fails a second time, on reviewer finding R2-7. The widening test applies, because adding rows widens a list that the author wrote from memory, so this is an amendment return and not a defect return.
- 2026-09-13: amendment return: AC5 — "Each page in the domain has a ledger row. Every match of `\btidymedia[._][a-z_.]+` or `\btm_[a-z_]+` in the domain at the base commit is still found in some `man/*.Rd` file at head, or has a ledger row."
- 2026-09-13: gate (user's choice): narrow AC5 as above, through the amendment protocol with a fresh re-audit reader. Rows for the three items in R2-7 are still to be added. Fix now R2-1 to R2-6 and R2-8 to R2-12 on the branch. Status back to in-progress.
- 2026-09-13: re-audit: AC5 (full) — 12 findings on the gate wording. Clear ones applied at the mini gate: whole-word matching, base commit named, ledger-row escape removed, row-exists sentence dropped. Judgment ones went to the mini gate. It found no IP or D-entry conflict, and the wording holds at head.
- 2026-09-13: mini gate (user's choice): AC5 is the identifier check only. The Scope Out sentence on the 0.1.0 settings location is deleted. The three R2-7 items and a note that AC5 does not check the ledger's Result column are added to the ledger.
- 2026-09-13: re-audit: AC5 (full) — 8 findings, none blocking. Wording holds at HEAD and in the working tree, all 27 names found with `git grep -wF`. Not applied, because a further wording change goes to the user: use fixed-string matching (`git grep -wF`) instead of "whole word", and point "the 28 domain pages" at the domain grep in `cairn/references/plain-docs.md`. Applied: ledger heading no longer says AC5. Accepted as a gap (see Decisions): no criterion checks that facts survive the rewrite.
- 2026-09-13: amendment executed: AC5 and Scope written as chosen at the mini gate.
- 2026-09-13: R2-1 to R2-6 and R2-8 to R2-12 fixed. R2-1 was measured with `tm_force_timeout()`: the two-pass analysis and a `hardware` backend's encoder check both give errors on a reached limit. The `ffprobe()` redirect claim was measured with `2>&1`. The `mediainfo()` redirect claim was read from its `system()` call. R2-11: a missing landing topic now drops only its own assertions. Sweep over 28 pages exits 0. `devtools::test()`: 1738 tests, 0 failed, 5 skipped.
- 2026-09-13: on `98e2ea71`: `devtools::check()` gives 0 errors, 0 warnings, 0 notes, with spelling OK and tests included. `devtools::document()` leaves `man/` unchanged. `pkgdown::check_pkgdown()` finds no problems. Status set to review.
- 2026-09-13: review round 3 checkpoint on `de5b70ba`: AC1, AC2, AC4 and AC5 pass. AC3 fails on `?with_timeout` ("encoder check", no glossary pointer). Gate (user's choice): run the three reviewers before the return. Check and reviewers still running.
- 2026-09-13: review return 2 (defect): AC3 fails on `de5b70ba`, because `?with_timeout` says "encoder check" with no glossary pointer and no ledger row. AC1, AC2, AC4, AC5 and AC6 pass. Status back to in-progress. Defect returns: 2. Amendment returns: 1.
- 2026-09-13: return gate (user's choice): fix AC3 and all of R3-1 to R3-16 in this pass. First check R3-2, R3-10, R3-13 and R3-16, and reject any that prove false, with the reason. A third defect return makes descope or park the recommended choice.
- 2026-09-13: AC3 fixed: `?with_timeout` names the glossary, and the ledger has a `with_timeout` row. R3-4 is in the same sentence: the check asks FFmpeg only with no option set and no stored answer (read from `hardware_encoder_available()`).
- 2026-09-13: the four unconfirmed findings were checked, and all four hold. R3-2 was read from R's `?system`, and a mid-command `2>&1` was measured on macOS. R3-10 was read from `check_hardware_available()`. R3-13 was read from the test. R3-16 was measured: a missing parameter prints one empty line, which gives `""` with `typed = FALSE`.
- 2026-09-13: R3-1 measured with withr 3.0.3: `with_options()` restores under `on.exit(NULL)`, and `local_options()` does not. R3-1 to R3-16 fixed in R/, tests and the `audio_stream` generator. The claim audit was not re-run, because each new claim was measured or read against the code. Sweep over 28 pages exits 0. `?tidymedia` renders 78 lines. Full suite running.
- 2026-09-13: on `82b7c72d`: `devtools::test()` gives 1738 tests, 0 failed, 5 skipped. `devtools::check()` gives 0 errors, 0 warnings, 0 notes, with spelling OK. `devtools::document()` leaves `man/` unchanged. `pkgdown::check_pkgdown()` finds no problems. Status set to review.

## Decisions

- Implement gate (user's choice): the timeout condition classes go to `?with_timeout` with the other timeout detail. `?tidymedia` keeps one short end section with one line for each FFmpeg-exit class and its fields.
- Implement gate (user's choice): only `?find_ffmpeg` explains the settings file that version 0.1.0 used, in a short end section. `?program_status` and `?unset_program` link to it.
- Review return 1: D074 names `?tidymedia` as where the per-row check order is disclosed. M127 moved that text to `?with_timeout` with the other timeout detail. The code comment now names the new page. D074 stays as written, because it is history.
- Amendment return (user's choice): AC5 checks only that the base identifiers are still in `man/`. No criterion checks that other facts survive the rewrite. Two lists of removed text written from memory each missed facts, and no procedure can list reworded facts. Keeping facts is left to the claim audit and the review's diff reviewer. The ledger's Result column stays as the author's account.
- Implement gate (user's choice): M127 does not fix the six prose-sweep parse gaps (shipped-docs candidate row, item (o)). The rewrite avoids the text shapes that the gaps misread.

## Review

Evidence is from 2026-09-13 on `a7f5d749`. That commit contains `origin/master` (`264afff4`), so no sync merge was needed. The domain grep returned 28 pages.

- AC1: `Rscript tools/doc_prose_report.R` over the 28 pages prints no finding and exits 0. At `264afff4`, the same sweep prints 185 findings. Of these, 73 are maintainer terms. So the sweep can fail on this domain.
- AC2: the same run prints no sentence over 25 words. At `264afff4` it printed 112.
- AC3: a new stem search over the `--prose` output finds stems on 8 pages. The pages and stems are the same as in the ledger table. Each of the 8 rendered pages names the glossary in `vignette("tidymedia")`. The other 20 pages have no stem. The ledger says so in one sentence, as the M126 ledger did for `batch.Rmd`.
- AC4: `tools::Rd2txt()` on `man/tidymedia-package.Rd` gives 73 lines. The Description paragraph says what the package does. It ends "Start with `vignette("tidymedia")`".
- AC5: all 28 pages have a row in the ledger's AC5 table. At `264afff4` the pattern finds 27 identifiers in the domain. At head, a `\bname\b` search finds all 27 in `man/*.Rd`. The search finds no made-up name, and `tidymedia_ffm` does not match inside `tidymedia_ffmpeg_exit`. Spot reads found four moved items at their ledger targets. These are the timings in `R/timeout.R`, the two decision-id comments, and the `?find_ffmpeg` section on earlier versions.
- AC5 correction: the box is unticked. Reviewer O4 found a fact that left `?tidymedia` with no ledger row. The base text said a bad `tidymedia.check_tracks` value gives an error that names the option. No `man/*.Rd` page says this at head.
- AC6: fails. `devtools::document()` leaves `man/` unchanged, and a planted roxygen edit shows that it does write. `pkgdown::check_pkgdown()` finds no problems. `devtools::check()` on `ca35647a` gives 0 errors and 0 warnings, but ends "Status: 1 NOTE". The note is at "checking tests": `spelling.Rout` differs from `spelling.Rout.save`, because `dplyr` at `probe_all.Rd:45` is flagged. Commit `1efbcc43` changed `\code{dplyr}` to plain "dplyr" in `R/ffprobe.R:46`. `devtools::test()` did not run on its own, because AC6 already failed.
- Consistency gate: `cairn_validate` passes. The diff adds no top-level file and changes no `README` or `NEWS.md`.

### Review findings

Three new-context reviewers ran on `ca35647a`. The blame-history reviewer and the prior-review reviewer found no defects. The blame-history reviewer made one note: the new `@family` names differ from the layer names in `CLAUDE.md`. The diff-bug reviewer ranked 16 findings. Items O1, O2 and O4 are confirmed against the code. The rest are as reported. Dispositions are open until the next review gate.

- O1: `?with_timeout` says every task function whose name does not end in `_batch` gives a `tidymedia_timeout` error. `segment_video()` runs through `ffm_batch()` and warns instead (`R/ffmpeg.R`, `helper-timeout-sweep.R` `ffm_batch_class`).
- O2: the branch deleted text that D074 places in `?tidymedia`. The text said that a set limit wins over two per-row checks. These are the `output` column of a `_batch` job table and the checks of `anonymize_video_batch()`. No page has it now, and the comment above `resolve_timeout()` in `R/timeout.R` still says `?tidymedia` has it.
- O3: `test-timeout-silence.R:667` pins only "error" on `?with_timeout`, and other text on that page matches it.
- O4: the `tidymedia.check_tracks` refusal left the docs with no ledger row (see the AC5 correction).
- O5: `?with_timeout` says a limit set with `options()` "follows the same rule", but `options(tidymedia.timeout = NULL)` is accepted.
- O6: `R/ffm_batch.R:314` says the limit is how long a program can run. `?with_timeout` says it is how long R waits.
- O7: the `check_tracks` bullet in `?tidymedia` says that an input names no `audio_stream`. A call or a row names it. The bullet also omits that `run = FALSE` skips the probe.
- O8: the `tidymedia_ffmpeg_exit` item in `?tidymedia` does not say that task functions raise it too (same at base).
- O9: the error and warning lists on `?with_timeout` omit `ffmpeg_codecs()`, `ffmpeg_encoders()`, `has_hardware_encoder()` and the version probe of `program_status()` (same at base).
- O10: the M69 guard in `test-timeout-silence.R:680-696` no longer reads `?tidymedia`.
- O11: the new order check in `test-package-topic.R:84-86` cannot fail, because "exited non-zero" opens the item.
- O12: the `?local_timeout` Description gives no limit to its restore claim. The two exceptions are only in Details.
- O13: the `.local_envir` text can be read to include top-level use, which withr handles.
- O14: two grammar slips, in the `separation_container` text of `R/audio-stream-doc.R` ("Count among ... not by") and on `?audio_stream` ("functions differ in how much audio it selects").
- O15: one roxygen line in `R/ffm_batch.R:370` is over 80 characters.
- O16: D053 and D066 quote help text that changed. The quotes are old, not false.
- Session finding S1: the spelling note behind the AC6 failure (`R/ffprobe.R:46`).

### Review round 2

Evidence is from 2026-09-13 on `629d68a4`. That commit contains `origin/master` (`264afff4`), so no sync merge was needed. No PR exists for the branch.

- AC1: `Rscript tools/doc_prose_report.R` over the 28 ledger pages prints no finding and exits 0. The same sweep over the 28 pages as they were at `264afff4` prints 185 findings and exits 1. Of these, 73 are maintainer terms.
- AC2: the same head run prints no sentence over 25 words. At `264afff4` it printed 112.
- AC3: a search for the ten glossary stems over each page's `--prose` output finds stems on 8 pages, with the same pages and stems as the ledger table. Each of the 8 rendered pages names the glossary in `vignette("tidymedia")`.
- AC4: `tools::Rd2txt()` on `man/tidymedia-package.Rd` gives 78 lines. The Description paragraph says what the package does and ends "Start with `vignette("tidymedia")`".
- AC5: the ledger's AC5 table has one row for each of the 28 pages. At `264afff4` the pattern finds 27 identifiers in the 28 pages. At head, a whole-word search finds all 27 in `man/*.Rd`. The fact that reviewer O4 found missing is back on `?tidymedia`. `man/tidymedia-package.Rd` says that a bad `tidymedia.check_tracks` value gives an error that names the option. The `tidymedia-package` and `with_timeout` rows record the check-order text that moved to `?with_timeout`.
- AC6: passes on `32894fbf`, whose code is the same as `629d68a4`. `devtools::check()` on `629d68a4` gives 0 errors, 0 warnings and 0 notes, and the spelling comparison is OK. `devtools::document()` leaves `man/` and `NAMESPACE` unchanged. In round 1, a planted roxygen edit showed that it does write. `devtools::test()` gives 1738 tests, 0 failed, 0 errors, 5 skipped. `pkgdown::check_pkgdown()` finds no problems.
- AC5 correction: the box is unticked again. Reviewer finding R2-7 shows three facts that left `?tidymedia` with no ledger row. These are what the FFprobe and MediaInfo readers return, and that the full function list is on the reference index. The third is the layer description. The reworded overview may cover it, and the row must say so.
- Consistency gate: `cairn_validate` passes. The diff adds no top-level file and changes no `README`, `NEWS.md`, `DESCRIPTION` or `DESIGN.md`. No principle changed.

### Review round 2 findings

Three new-context reviewers ran on `629d68a4`. The prior-review reviewer found no PR comments, and its one finding is R2-2. The blame-history reviewer's one finding is also R2-2. It made one note: the `@family` renames are consistent and contradict no decision. The diff-bug reviewer ranked 12 findings, listed here in its order. It found the code the same as at base, apart from reordered functions and generated help strings. R2-2, R2-6 and R2-7 are confirmed by this session. The rest are as reported.

- R2-1: `?with_timeout` lists every `_batch` function and `segment_video()` as warning on a reached limit. Two paths give a `tidymedia_timeout` error instead. The first is the analysis pass of `normalize_audio_batch(two_pass = TRUE)`, which calls `run_program()` inside `purrr::pmap()` with no catch. The second is the encoder check under a named `hardware` backend. The same gap is at base.
- R2-2: "which counts every stream.An input" is missing a space in `R/audio-stream-doc.R:151`. The O14 fix added the error, and the text lands on `?separate_audio_video` and `?separate_audio_video_batch`.
- R2-3: `?local_timeout` narrowed the second case to a function that has returned. A `.local_envir` such as `new.env()` also leaves the limit set, and the page no longer says so.
- R2-4: the `?local_timeout` Description says "except in the two cases in Details". Details also describes a third case, a call written directly inside `with_timeout()`.
- R2-5: `R/verify.R:26` is a new roxygen line of 83 characters, so O15 is not fully fixed.
- R2-6: the comment above `resolve_timeout()` still names `segment_video()`'s `outfiles` as losing to the limit. `outfiles` is checked first (`R/ffmpeg.R:3842`, before 3906).
- R2-7: the `tidymedia-package` ledger row does not record three removed items. See the AC5 correction.
- R2-8: in `?refresh_ffmpeg_capabilities` Parallel workers, "This is not the case when you have set `tidymedia.hardware_encoders`" attaches to the wrong sentence.
- R2-9: in the `?install_on_win` Value section, "Other failures give an error, listed in the section Errors" also covers argument errors that the section does not list.
- R2-10: the `ffprobe()` and `mediainfo()` Value sections say that standard error is not returned. A command ending in `2>&1` does return it.
- R2-11: in `test-timeout-silence.R:692`, the new `skip_if()` for the landing topic also skips the `rd` and `news` assertions.
- R2-12: the `?with_timeout` Description says "the session's own limit is back". Inside a function that called `local_timeout()`, it is the limit that was in force.
- Dispositions (gate, 2026-09-13): R2-7 is an amendment return on AC5. The narrowed wording is in the work log, and the three rows are added as well. R2-1 to R2-6 and R2-8 to R2-12 are fix now. None is rejected or deferred.

### Review round 3

Evidence is from 2026-09-13 on `de5b70ba`. That commit contains `origin/master` (`264afff4`), so no sync merge was needed. No PR exists for the branch.

- AC1: `Rscript tools/doc_prose_report.R` over the 28 domain pages prints no finding and exits 0. The same sweep over the 28 pages as they were at `264afff4` prints 185 findings and exits 1. Of these, 73 are maintainer terms.
- AC2: the same head run prints no sentence over 25 words. At `264afff4` it printed 112.
- AC3: fails. A search for the ten glossary stems over each page's `--prose` output finds stems on 9 pages. Eight are the ledger's pages, and each names the glossary in `vignette("tidymedia")`. The ninth is `with_timeout`. Its prose line 99 says "The other is the encoder check of a call that names a backend", which matches `encod`. The page does not define the term, does not name the glossary, and has no ledger row. Commit `98e2ea71` added the sentence as the R2-1 fix. The box is unticked.
- AC4: `tools::Rd2txt()` on `man/tidymedia-package.Rd` gives 78 lines. The Description paragraph says what the package does and ends "Start with `vignette("tidymedia")`".
- AC5: at `264afff4` the pattern finds 27 identifiers in the 28 domain pages. At head, `git grep -wF` finds each of the 27 in `man/*.Rd`. The box is ticked.
- AC6: `devtools::check()` on `de5b70ba` gives 0 errors, 0 warnings and 0 notes, with the spelling comparison OK and tests included. After it, `devtools::document()` leaves `man/` and `NAMESPACE` unchanged. `pkgdown::check_pkgdown()` finds no problems. `devtools::test()` gives 13883 expectations, 0 failed, 0 errors, 5 skipped.
- Consistency gate: `cairn_validate` passes. The diff adds no top-level file and changes no `README`, `NEWS.md`, `DESCRIPTION` or `DESIGN.md`. No principle changed.

### Review round 3 findings

Three new-context reviewers ran on `de5b70ba`. The prior-review reviewer found no PR comments. Its one finding is the AC3 failure above, and it found the O-series and R2-series fixes still in place. The blame-history reviewer found no defects. It compared the statements of every changed `R/` file at base and head, and found only reordering and help-text strings. The diff-bug reviewer ranked 16 findings, listed here in its order. It found no other page that fails AC3. R3-1 and R3-3 are confirmed by this session against the source text.

- R3-1: `R/timeout.R:327-328` says `withr::with_options()` has "the same limits" as all three `?local_timeout` cases. `with_options()` has no `.local_envir` and restores through its own exit handler. Measured by the reviewer: with `on.exit(NULL)` in the body, `with_options()` restores the option and `local_options()` does not. New in `98e2ea71`.
- R3-2: the `ffprobe()` and `mediainfo()` Value sections say a command that ends with `2>&1` returns standard error. R's documentation says that `system()` runs no shell on Windows. So on Windows the redirect is not a redirect. On Unix, a redirect works anywhere in the command. Not confirmed on Windows.
- R3-3: the comment above `resolve_timeout()` says a wrong form of `segment_video()`'s `outfiles` passes the `check_string()` loop and loses to the limit, and that `?with_timeout` says so. M096 closed that path (`test-unguarded-argument-front-doors.R:105-110`), and `?with_timeout` does not mention it. New in `98e2ea71`.
- R3-4: `?with_timeout` uses "backend" with no definition, and does not say that the `tidymedia.hardware_encoders` option or a stored answer skips the encoder check (`R/ffmpeg.R:3204-3219`).
- R3-5: `?with_timeout` Details says "The function you called gives the error, even when `run = FALSE`" after a sentence that names no error, and it reads as covering every function.
- R3-6: `?refresh_ffmpeg_capabilities` See also says `hardware_encoder()` uses the stored answer. It only reads a lookup table (`R/ffmpeg.R:3119-3129`). Same at base.
- R3-7: `R/audio-stream-doc.R:152` is a 104-character source line, new in `98e2ea71`.
- R3-8: `R/ffm_batch.R:11` and `:76` are roxygen lines of 81 characters. Both are older than the branch.
- R3-9: `?install_on_win` Value says the Errors section lists "the error classes of the install steps". `tidymedia_wrong_platform` and `tidymedia_confirmation_unavailable` fire before any install step, and argument errors are in neither sentence.
- R3-10: in `?refresh_ffmpeg_capabilities` Parallel workers, "That option works in a different way" now points two sentences back. The reviewer thinks "Asks FFmpeg `W` times" is W+1 with the main-session check. The count is not confirmed.
- R3-11: `R/timeout.R:190` uses a serial comma, and the rest of the page does not.
- R3-12: stale test comments: `test-runtime-timeout.R:532-536` counts an `abort` assertion that is gone, and the `test-timeout-silence.R:681` title says "both docs" but reads three sources.
- R3-13: in `test-timeout-silence.R:692-696`, a landing topic that is not found drops its assertions with no skip message. Not confirmed in practice.
- R3-14: the generated `audio_stream` text on `?separate_audio_video` says functions that pass video through do not fail, on a function that passes video through, and it repeats the `NULL` default sentence.
- R3-15: the `tidymedia.check_tracks` item in `?tidymedia` no longer says that the default is `TRUE`.
- R3-16: `?mediainfo_parameter` Value no longer says an empty value gives `NA`. The reviewer reads `R/mediainfo.R:113` as giving `""` for one empty line with `typed = FALSE`. Not confirmed.
