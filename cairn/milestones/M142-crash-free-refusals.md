<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M142: Two `ffm_batch()` refusals and the direct-command functions stop with their own message instead of an R error

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
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

- [ ] AC1: With two or more jobs whose `.f` returns something other than an
      ffm pipeline, `ffm_batch()` stops with its own refusal, "`.f` must
      return an ffm pipeline for every job.", and, where 20 or fewer jobs are
      bad, the message names the row number of each such job. A test runs a
      three-job batch whose jobs 1 and 3 are bad and asserts that the message
      names jobs 1 and 3 and not job 2. A second test with one bad job, at a
      row other than 1, asserts the singular wording "Job N".
- [ ] AC2: With `run = TRUE` and a `verify` function whose result is not a
      non-empty named list for two or more jobs, `ffm_batch()` stops with its
      own refusal, "Each `verify` spec must be a non-empty named list of
      properties.", and, where 20 or fewer specs are invalid, the message
      names the row number of each such job. Tests as in AC1.
- [ ] AC3: When the program lookup returns `NULL`, `ffmpeg()`, `ffprobe()`
      and `mediainfo()` stop with "Could not locate FFmpeg.", "Could not
      locate FFprobe." and "Could not locate MediaInfo." respectively, and
      the error's call is the function the caller typed. A test for each of
      the three mocks the lookup to return `NULL` and asserts the message and
      the call.
- [ ] AC4: Under the same lookup, `ffmpeg_codecs()` and `ffmpeg_encoders()`
      stop with "Could not locate FFmpeg.", and the error's call is
      `ffmpeg_codecs()` or `ffmpeg_encoders()`. A test for each asserts the
      message and the call.
- [ ] AC5: The help pages `?ffmpeg`, `?ffprobe`, `?mediainfo`,
      `?ffmpeg_codecs` and `?ffmpeg_encoders` each say that the call stops
      with an error when tidymedia cannot find the program.
- [ ] AC6: `devtools::check()` reports 0 errors and 0 warnings.

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
- [ ] T2: Write the AC3 and AC4 tests first, mocking `find_program()` to
      return `NULL` and leaving `tidymedia.timeout` unset (an invalid limit
      stops first). Move `run_program()`'s refusal into a shared
      `tm_require_location()` that `ffmpeg()`, `ffprobe()` and `mediainfo()`
      call on their lookup before they spawn anything. `ffmpeg_codecs()` and
      `ffmpeg_encoders()` keep calling `ffmpeg()`, the seam 33 test mocks
      replace, and raise its missing-program refusal again under their own
      frame through `tm_reblame_missing()` (D074). The command string stays
      as the caller wrote it (IP1).
- [ ] T3: Add the AC5 sentence to the five roxygen blocks, run
      `devtools::document()`, and confirm the roxygen2 version line (M128
      lesson) is absent from its output.
- [ ] T4: Add one `NEWS.md` bug-fix entry per crash under the development
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

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->
