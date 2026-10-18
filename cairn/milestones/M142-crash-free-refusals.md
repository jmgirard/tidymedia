<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M142: Two `ffm_batch()` refusals and the direct-command functions stop with their own message instead of an R error

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate; RR<NN> whose Binding criteria bind this milestone's ACs (binding-criteria check), or — -->
- **Principles touched:** IP1   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Resolves:** —   <!-- owner: plan · create/amend-via-gate; comma-separated GitHub issues the scope absorbs, each `#N closes` (the PR closes it at merge) or `#N partial` (the remainder gets a candidate row), or — ; skill conduct only — no validate check parses it -->
- **Surface tier:** user-facing — changes what exported functions report when they refuse   <!-- owner: plan · create/amend-via-gate; user-facing | internal — <one-clause reason>; skill conduct only — no validate check parses it -->
- **Branch/PR:** m142-crash-free-refusals   <!-- owner: implement (branch) / review (PR URL) · create; a companion checkout the milestone also works in is one further entry per checkout, `companion: <abs-path> <branch>` (implement), its PR URL appended by review — /milestone-review merges companions first, in listed order -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Callers who trip two `ffm_batch()` refusals, or who run a direct-command or capability-listing function with no program found, get tidymedia's own message in place of an R error that names nothing.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:** the not-a-pipeline and invalid-`verify`-spec refusals in
`R/ffm_batch.R` (`Job{?s} {.val {which(...)}}`, lines 159 and 455), which
fail inside cli with `length(object) == 1 is not TRUE` when two or more jobs
are bad and print "Jobs 2" for one bad job at a row other than 1 (both
measured 2026-10-18); `ffmpeg()`, `ffprobe()` and `mediainfo()`, which with no
program found return `character(0)` (`ffmpeg()`) or stop with `non-empty
character argument expected`; and `ffmpeg_codecs()` and `ffmpeg_encoders()`,
which then stop with `argument of length 0` (measured 2026-10-18). The help
pages of those five functions, and `NEWS.md`.

**Out:**
- A condition class for the missing-program refusal → the "Unclassed
  aborts" candidate row (its naming pass); this milestone reuses
  `run_program()`'s unclassed `Could not locate {program}.` message.
- The verify-spec refusal's blame frame (`resolve_batch_verify()`, not
  `ffm_batch()`), and the hardware builders that reach `ffmpeg_encoders()`
  through `cached_encoder_names()` and will see the refusal blamed on
  `ffmpeg_encoders()` → the "Argument classes refused below the verb"
  candidate row, items (c) and (d).
- `run_program()`'s own message → unchanged; the timeout sweeps read it.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets. -->

- [x] AC1: With two or more jobs whose `.f` returns something other than an
      ffm pipeline, `ffm_batch()` stops with its own refusal, "`.f` must
      return an ffm pipeline for every job.", and, where 20 or fewer jobs are
      bad, the message names the row number of each such job. A test runs a
      three-job batch whose jobs 1 and 3 are bad and asserts that the message
      names jobs 1 and 3 and not job 2. A second test with one bad job, at a
      row other than 1, asserts the singular wording "Job N".
- [x] AC2: With `run = TRUE` and a `verify` function whose result is not a
      non-empty named list for two or more jobs, `ffm_batch()` stops with its
      own refusal, "Each `verify` spec must be a non-empty named list of
      properties.", and, where 20 or fewer specs are invalid, the message
      names the row number of each such job. Tests as in AC1.
- [x] AC3: When the program lookup returns `NULL`, `ffmpeg()`, `ffprobe()`
      and `mediainfo()` stop with "Could not locate FFmpeg.", "Could not
      locate FFprobe." and "Could not locate MediaInfo." respectively, and
      the error's call is the function the caller typed. A test for each of
      the three mocks the lookup to return `NULL` and asserts the message and
      the call.
- [x] AC4: Under the same lookup, `ffmpeg_codecs()` and `ffmpeg_encoders()`
      stop with "Could not locate FFmpeg.", and the error's call is
      `ffmpeg_codecs()` or `ffmpeg_encoders()`. A test for each asserts the
      message and the call.
- [x] AC5: The help pages `?ffmpeg`, `?ffprobe`, `?mediainfo`,
      `?ffmpeg_codecs` and `?ffmpeg_encoders` each say that the call stops
      with an error when tidymedia cannot find the program.
- [x] AC6: `devtools::check()` reports 0 errors and 0 warnings.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1
- AC2 → T1
- AC3 → T2
- AC4 → T2
- AC5 → T3
- AC6 → T4

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] T1: Write the AC1 and AC2 tests first and see them red on master (the
      two-bad-job case with cli's error, the one-bad-job case with "Jobs 2").
      Fix both messages with `{cli::qty(n)}` right before `{?s}` (M141
      lesson). In `tests/testthat/test-batch-output-collision.R:235`, the
      comment that avoids two invalid specs no longer holds: update it.
- [x] T2: Write the AC3 and AC4 tests first, mocking `find_program()` to
      return `NULL` and leaving `tidymedia.timeout` unset (an invalid limit
      stops first). Move `run_program()`'s refusal into a shared
      `tm_require_location()` that `ffmpeg()`, `ffprobe()` and `mediainfo()`
      call on their lookup before they spawn anything. `ffmpeg_codecs()` and
      `ffmpeg_encoders()` keep calling `ffmpeg()`, the seam 33 test mocks
      replace, and raise its missing-program refusal again under their own
      frame through `tm_reblame_missing()` (D074). The command string stays
      as the caller wrote it (IP1).
- [x] T3: Add the AC5 sentence to the five roxygen blocks, run
      `devtools::document()`, and confirm the roxygen2 version line (M128
      lesson) is absent from its output.
- [x] T4: Add one `NEWS.md` bug-fix entry per crash under the development
      version, each backed by the T1 or T2 tests. Run `devtools::test()` with
      no other R session working (M124 lesson) and `devtools::check()`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates. -->

- 2026-10-18: created by /milestone-plan.
- 2026-10-18: tier check: not run — work named at the question set.
- 2026-10-18: question set: which work to plan — the two crashes (ffm_batch() plural refusals; direct-command and capability functions with no program).
- 2026-10-18: collision sweep: absorbs the `ffm_batch()` two-refusals candidate row in full and the `ffmpeg_encoders()` sentence of the "Unclassed aborts" row in part; 0 open issues and 0 open PRs on GitHub; no D-entry rejects this work.
- 2026-10-18: the candidate row's "one bad job reads correctly" is false: one bad job at row 2 prints "Jobs 2" (criteria audit, measured with cli 3.6.6), so AC1 and AC2 test a row other than 1.
- 2026-10-18: criteria audit (full mode, fresh Opus reader) returned 6 findings: cli shortens past 20 values (AC1 and AC2 narrowed to 20 or fewer); a singular test at row 1 passes on master (moved to a row other than 1); invalid specs reach the refusal only under `run = TRUE` with a `verify` function (AC2 narrowed); the verify refusal blames `resolve_batch_verify()` and the hardware path would blame `ffmpeg_encoders()` (both decided toward the narrower promise, sent to the "Argument classes refused below the verb" row); AC4 needs an internal form under D087 (T2 says so). AC3, AC5 and AC6 had no findings.
- 2026-10-18: plan gate chose reusing `run_program()`'s unclassed `Could not locate {program}.` message over a new condition class, because a public class name belongs to the "Unclassed aborts" row's naming pass; falsified by a caller who needs to catch the missing-program refusal by class.
- 2026-10-18: plan gate chose threading the blame through an internal `tm_ffmpeg()` over a `find_ffmpeg()` check at the top of each capability function, because a second lookup gives a second "Failed to find" warning; falsified by a measured run where the internal form still warns twice.
- 2026-10-18: implement started on branch m142-crash-free-refusals.
- 2026-10-18: T1 done: four tests in test-ffm-batch.R (two-bad-job and one-bad-job-at-row-2 cases for both refusals) red on the base code with cli's error and "Jobs 2", green after `{cli::qty()}`; test-batch-output-collision.R comment updated; full devtools::test() 0 failures.
- 2026-10-18: T2 approach changed from the plan's internal `tm_ffmpeg(command, call)`: 33 test mocks in 16 files replace `ffmpeg()` as the listings' seam, and an internal form would bypass them; so `ffmpeg()` refuses through a shared `tm_require_location()` (also `run_program()`'s, message unchanged) and the listings re-raise that refusal under their own frame with `tm_reblame_missing()`, which recognizes it by an internal `tm_missing_program` field, no class. Task text edited to match (minor amendment, criteria unchanged).
- 2026-10-18: implementation choice: the three direct-command functions now look up the program outside `guard_timeout()`, so a lookup warning reaches the caller with its own class instead of being held and re-raised as a plain warning; rejected keeping the lookup inside, because the refusal must come before the spawn.
- 2026-10-18: first full run after T2 failed 10 tests in test-normalize-audio*.R and test-nvenc.R with "Could not locate FFmpeg.": the new test file's helper mocked `find_program()` with `.env = topenv()`, and that mock was not undone when the helper returned (measured: `find_ffmpeg()` NULL after the file, `Sys.which()` still found FFmpeg). Switched to `.package = "tidymedia"`, the suite's own form; `find_ffmpeg()` resolves after the file. T3 roxygen text added and `devtools::document()` rewrote the five Rd pages with no roxygen2 version line.
- 2026-10-18: T2 and T3 done: test-missing-program-refusal.R (5 tests) red on the base code with the R errors the plan names, green after; full devtools::test() 0 failures with the scoped mock. NEWS.md has the two T4 entries.
- 2026-10-18: claim audit: 37 claims read, 6 corrected — R/ffm_batch.R, R/ffmpeg.R, R/ffprobe.R, R/mediainfo.R, tests/testthat/test-missing-program-refusal.R (the five help sentences now say the find_*() warning comes first and that argument and timeout checks stop the call before the search; two comments narrowed; the test comment now states the measurement, not a mechanism). The re-read found five corrections true; the measured test comment cannot be settled from the code.
- 2026-10-18: T4 done: devtools::check() 0 errors, 0 warnings, 0 notes (the spelling test's output lists "encodable" and "filesystem", both in text already on master). After the claim fixes, devtools::document() rewrote the five Rd pages, tools::checkRd() was clean, and the seven Rd-reading and new test files passed with NOT_CRAN=true. Status set to review.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->

Review pass 1, 2026-10-18, on branch head f1452a91 (master 2f023576 unmoved).

- AC5 evidence: `man/ffmpeg.Rd`, `man/ffmpeg_codecs.Rd`, `man/ffmpeg_encoders.Rd`, `man/ffprobe.Rd` and `man/mediainfo.Rd` each hold "If tidymedia cannot find <program>, the call stops with the error "Could not locate <program>."" (grep over each regenerated Rd, line breaks joined).
- AC1 evidence (head 4b4d2d7d, `NOT_CRAN=true`, test-ffm-batch.R): "ffm_batch() names every job whose .f returned no pipeline (M142)" passes on jobs 1 and 3 of three ("Jobs 1 and 3 returned something else."), "names one job …" passes on job 2 ("Job 2 returned something else."), and the 20-and-25 test passes; 5 M142 tests, 16 expectations, 0 failed, 0 skipped. On the base code the two-job case failed with cli's "length(object) == 1 is not TRUE" and the one-job case read "Jobs 2" (T1 work log).
- AC2 evidence (same run): the two verify-spec tests pass with `run = TRUE` and a `verify` function, naming jobs 1 and 3 ("Jobs 1 and 3 produced an invalid spec.") and job 2 ("Job 2 produced an invalid spec."); red on the base code the same way as AC1.
- AC3 evidence (same head, test-missing-program-refusal.R): with `find_program()` mocked to `NULL`, `ffmpeg()`, `ffprobe()` and `mediainfo()` give exactly "Could not locate FFmpeg.", "Could not locate FFprobe." and "Could not locate MediaInfo." and `cnd$call[[1]]` is `ffmpeg`, `ffprobe` and `mediainfo`; 6 M142 tests, 21 expectations, 0 failed. Red on the base code (implement work log).
- AC4 evidence (same run): `ffmpeg_codecs()` and `ffmpeg_encoders()` give exactly "Could not locate FFmpeg." with `cnd$call[[1]]` `ffmpeg_codecs` and `ffmpeg_encoders`; a real run with FFmpeg hidden printed "Error in `ffmpeg_encoders()`: ! Could not locate FFmpeg." (implement, 2026-10-18).
- AC6 evidence: `devtools::check()` on 4b4d2d7d: 0 errors, 0 warnings, 0 notes (7m 13s). The log's one NOTE is the spelling test's output against its saved copy, listing "encodable" (ffm_batch.Rd, ffm_run.Rd) and "filesystem" (NEWS.md), both in text already on master.
- Consistency gate: `cairn_validate.py` all checks passed; no principle changed, so `cairn_impact` skipped; `devtools::document()` left no diff; `pkgdown::check_pkgdown()` found no problems; README.Rmd untouched; no new top-level file (NEWS.md only); NEWS.md has the two bug-fix entries with no milestone numbers.
- No-binaries run on fe9014da: full `devtools::test()` with `PATH=/usr/local/bin:/usr/bin:/bin` and both config dirs redirected to an empty folder (FFmpeg, FFprobe and MediaInfo absent, checked by `Sys.which()` first): 0 failures. The pre-fix test-with-timeout.R run the same way errors in "each spawn site is handed the per-call limit" with `Error in ffmpeg("-version"): Could not locate FFmpeg.`; the fixed file passes.
- spawned: diff-bug, blame-history, prior-review
- diff-bug #1: `test-with-timeout.R` "each spawn site is handed the per-call limit" mocks only `guard_timeout()`, so with the lookup now ahead of it the test stops with "Could not locate" on a machine with no media binaries, CRAN included — fix now, fixed fe9014da (locators pinned as helper-timeout-sweep.R pins them; confirmed by the no-binaries suite run below).
- blame-history #1: the same test-with-timeout.R defect — fix now, fixed fe9014da (same fix as diff-bug #1).
- diff-bug #2: NEWS does not say that the lookup's warning now keeps its class in the three functions — fix now, fixed fe9014da (NEWS sentence plus a test that a classed lookup warning reaches each caller before the refusal).
- blame-history #2: the same warning-class change — fix now, fixed fe9014da (same fix as diff-bug #2).
- prior-review #1: NEWS says "names every bad job", but cli shortens a list past 20 values — fix now, fixed fe9014da (NEWS narrowed to "lists up to 20 job numbers in full and shortens a longer list", with a test at 20 and 25 bad jobs).
- diff-bug #3: `cnd$tm_missing_program` matches partially — fix now, fixed fe9014da (`[[`).
- blame-history #4: D087 says an internal function taking `call` has no default, and both new helpers had one — fix now, fixed fe9014da (defaults removed, each site passes `rlang::current_env()`).
- prior-review #3: the help paragraph was repeated in three blocks of R/ffmpeg.R, and `tools/roxygen_repeats.R R/ffmpeg.R` exits 1 on the branch where master's file exits 0 — fix now, fixed fe9014da (one `missing_program_sentences()` in R/task-doc.R, called from all five blocks; the tool exits 0 on R/ffmpeg.R).
- diff-bug #4: no test for "the warning from find_*() comes first" — fix now, fixed fe9014da (covered by the warning-class test of diff-bug #2).
- prior-review #2: "the warning from find_ffmpeg() comes first" is false when nothing is configured — reject, false: `find_program()`'s no-config branch warns "Failed to find {program}." (R/program_management.R, the final `else`), seen in this session's no-FFmpeg run, and every NULL path warns.
- prior-review #4: on the hardware path, a missing FFmpeg is blamed on `ffmpeg_encoders()`, not the typed verb — follow-up, already the "Argument classes refused below the verb" row's item (d), filed at plan time; master blamed `ffmpeg_encoders()` there too.
- blame-history #3: `ffmpeg()` now stops where it returned `character(0)` — reject, planned change (AC3).
- blame-history #5: the lookup warning now survives a timeout, which D047's held-warning drop covered for R's own timeout warning — reject, false as a defect: D047 drops the timeout warning because it carries the command line, and the lookup warning carries none.
- blame-history #6: confirms the M140 quoting, D074 order, `-hide_banner` and the M96 check are kept, and the encoder diagnosis fails open as before — reject, no defect reported.
- blame-history #7: the removed test-batch-output-collision.R comment described the bug this fixes — reject, planned change (T1).
- diff-bug #5: the timeout sweep's absorber census in helper-timeout-sweep.R does not list `tm_reblame_missing()`'s handler — reject, false as a defect: the handler re-raises only an error carrying `tm_missing_program` and passes every other condition on with `zap()`, so it cannot absorb a timeout; the reviewer says so too.
- diff-bug #6: the re-raised refusal drops the original as `parent` — reject, planned change: the refusal is re-raised under the typed verb with the same message, as T2 states.
- diff-bug #7: the new paragraph lands in `\details` on three pages and `\description` on two — reject, style.
- diff-bug #8: the two verify tests use `run = TRUE` with relative outputs and depend on the refusal coming before dispatch — reject, false as a defect: the refusal comes before dispatch by design (the test at test-ffm-batch.R:75 relies on the same order), and a regression there fails these tests.
