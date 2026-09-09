# M118: The binary-executing tests skip on CRAN's own check

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — what runs on CRAN's machines is the shipped tarball's behaviour
- **Branch/PR:** `m118-cran-skips-binary-tests` / https://github.com/jmgirard/tidymedia/pull/122

## Goal

CRAN's own submission check runs none of the tests that spawn FFmpeg, FFprobe or
MediaInfo, while every other run of the suite still does.

## Scope

**In:** `skip_on_cran()` in the five `skip_if_no_*` helpers — the three
name-resolution helpers (`tests/testthat/helper-skip.R:4-23`) and the two
hardware-probe helpers `skip_if_no_nvenc()` and `skip_if_no_videotoolbox()`,
whose one-frame probe encode spawns FFmpeg before any of the three is consulted
— plus the three test sites the measurement found reaching a binary outside any
helper (`test-unguarded-argument-front-doors.R:289`, which hand-rolls the
helper's own guard, and `test-normalize-audio-batch.R:228` and `:239`, whose
incidental track-count check spawns FFprobe), and the measurement that shows no
spawn survives it.

**Out:** cutting the pure-R suite to reach a check-time target → declined at this
plan's gate; the maintainer chose the cheap fix on the measurement below.
A ROADMAP candidate row holds the profiling work.

## Acceptance criteria

- [x] AC1: In a non-interactive R session — the condition `R CMD check` runs the
      test suite under — on a machine with `ffmpeg`, `ffprobe` and `mediainfo` all
      on `PATH`, each of `skip_if_no_ffmpeg()`, `skip_if_no_ffprobe()` and
      `skip_if_no_mediainfo()` skips with the reason `"On CRAN"` when `NOT_CRAN` is
      unset, and does not skip when `NOT_CRAN` is set to `true`. Verified by
      `Rscript -e 'testthat::test_local(filter = "cran-skip-helpers")'`; the file
      sets `NOT_CRAN` itself per assertion, so the runner's own `NOT_CRAN="true"`
      does not reach it.
- [x] AC2: With `NOT_CRAN` unset and the three binaries on `PATH`, a full run of the
      suite makes no spawn that resolves one of the three names through `PATH`.
      Measured by shimming the three names onto `PATH` ahead of the real ones with a
      wrapper appending one line per call to a log; the same shim under
      `NOT_CRAN=true` writes a non-empty log, which is what shows the instrument can
      detect a spawn.
- [x] AC3: The config-directory route that AC2's `PATH` shim cannot see is a real
      route in the shipped package, not an assumption: with the three program names
      unresolvable on `PATH` and a logging stand-in written as FFmpeg's remembered
      location, `find_ffmpeg()` returns that location and `run_program()` spawns the
      stand-in, which logs the call. Procedure:
      `Rscript tools/cran_spawn_check.R --mode=config`, whose config-route probe
      reports at least one logged line. AC3 claims nothing about the suite's own
      spawn count under that mode, nor about the emptied-`PATH` route of
      `tests/testthat/helper-program-config.R:43`, which no stand-in can reach.
- [x] AC4: With `NOT_CRAN=true` and the three binaries on `PATH`, the set of skipped
      test names is the same at this milestone's base commit `ea433d5` and at the
      branch head. Each list is produced by
      `Rscript -e 'testthat::test_local(reporter = "summary")'` — non-interactive by
      construction, the session kind `R CMD check` runs the suite under — on one
      machine against the same resolved binaries, the base checked out in a detached
      worktree; the two lists of skipped test names are sorted and `diff`ed, `diff`
      empty and both lists non-empty, so an empty capture is not read as agreement.
- [x] AC5: `R CMD check --as-cran` with `NOT_CRAN` unset and the three binaries on
      `PATH` reports 0 errors, 0 warnings, and no note other than one naming the
      version number or a new submission.

## Coverage

- AC1 → T2, T6
- AC2 → T3, T7
- AC3 → T4, T7
- AC4 → T5
- AC5 → T5

## Tasks

- [x] T1: Measure, do not trust, the precedent the repo records at
      `tests/testthat/test-runtime-timeout.R:188-190` — that `devtools::check()` and
      the CI workflow both set `NOT_CRAN`, so CI keeps running these tests.
      `.github/workflows/R-CMD-check.yaml` sets no `NOT_CRAN` of its own, so the value
      comes from `r-lib/actions/check-r-package@v2`. If it does not, this milestone
      would silently gut CI coverage and stops here for a re-gate.
- [x] T2: Add `skip_on_cran()` to the five helpers in `helper-skip.R`.
- [x] T3: Build the PATH-shim spawn counter and run the suite in both modes.
- [x] T4: Run the two escape-route conditions of AC3.
- [x] T5: Record the base-commit skipped-test set, then run `devtools::test()` in
      developer mode and `R CMD check --as-cran` in CRAN mode; record each run's
      `Duration` and tests-step timing against the base commit's 7m47s and
      `[368s/436s]` (measured 2026-09-07).
- [x] T6: Guard `test-cran-skip-helpers.R` so it passes from an interactive console
      as well: assert the CRAN branch with `NOT_CRAN="false"`, which fires in either
      session kind, and ask the unset-variable question only where the session is
      the non-interactive one CRAN checks in.
- [x] T7: Repair `tools/cran_spawn_check.R` so it refuses to report a spawn count
      beside a suite run that did not finish, and so the shim source quotes the
      paths it interpolates; then re-run `--mode=path` (AC2) and `--mode=config`
      (amended AC3) against the fixed instrument.
- [x] T8: The three prose and assertion repairs the re-review coupled to this
      round: `cairn/DESIGN.md`'s execution-test convention gains the CRAN half;
      `test-normalize-audio-batch.R`'s new comment stops claiming the production
      path changed and its two `expect_error()` patterns name which refusal they
      expect; and a superseding entry corrects this file's two wrong decision-log
      statements about the script.

## Work log

- 2026-09-07: created by /milestone-plan.
- 2026-09-07: plan-gate criteria audit ran in FULL mode (declared tier user-facing), two rounds, fresh-context [O] reader. Findings against this milestone: AC1's "skips on that account alone for no other reason" was self-contradictory, since the helpers must keep skipping for an absent binary (repaired); AC2's "every spawn the run actually makes" was unbounded, the PATH shim seeing only bare-name resolution while a remembered absolute location and `helper-program-config.R:43`'s emptied `PATH` both escape it (repaired — AC2 narrowed to `PATH`-resolved spawns, AC3 added for the two escape routes, and a control run added so an empty log is not read as success); AC4's equal skip counts passed a swap (repaired — compares the set of skipped test names); AC3's timing sentence was an unfalsifiable recording act (repaired — moved to T5).
- 2026-09-07: plan gate chose `skip_on_cran()` in the three helpers over profiling and cutting the pure-R suite, because the measurement showed binary execution is about one minute of the six — `R CMD check` 7m47s with the binaries against a 5m06s binary-absent suite run (2026-09-07) — so the larger cut buys little against real risk to 1,568 test bodies; falsified by a CRAN check-time NOTE that survives this fix.
- 2026-09-08: T1 measured. The precedent holds but names the wrong source. `devtools::check()` sets it (installed `devtools::check` carries `env_vars = c(NOT_CRAN = "true")`); `r-lib/actions/check-r-package@v2` does not — it calls `rcmdcheck::rcmdcheck()` with no `env`, and `rcmdcheck` 1.4.0's `env` default is `character()`. `setup-r@v2` is what sets it: run 34275894026's predecessor 34261398144 dumps `NOT_CRAN: true` in the job env from the `setup-r-dependencies` step onward, and no test in that run skipped for an "On CRAN" reason though three `skip_on_cran()` sites were in the suite. CI coverage survives this milestone; no re-gate needed.
- 2026-09-08: amendment (substantive, Scope In) at the question gate: scope widened from three helpers to five. `skip_if_no_nvenc()` and `skip_if_no_videotoolbox()` each spawn a one-frame FFmpeg encode to decide, and check `Sys.which("ffmpeg")` inline rather than calling `skip_if_no_ffmpeg()`, so six tests (`test-nvenc.R:435,446,458`, `test-video-codec.R:480,489`, `test-hardware-backends.R:315`) would keep spawning under AC2. No acceptance criterion changed.
- 2026-09-08: question gate chose a committed `tools/cran_spawn_check.R` for the AC2/AC3 shim over a throwaway harness, matching the two measurement scripts already in `tools/`; costs one `.Rbuildignore` entry.
- 2026-09-08: amendment return: AC1 — "Each of `skip_if_no_ffmpeg()`, `skip_if_no_ffprobe()` and `skip_if_no_mediainfo()` skips when `NOT_CRAN` is unset". Falsified in an interactive session, where `testthat:::on_cran()` reads `!interactive()` on the unset branch and the helpers do not skip (measured at review under `R --interactive`: reason `NA`). AC1 names no procedure and does not bound the session's interactivity, so this is the never-reinterpret rule's unbounded-criterion case, not a defect in the work — `skip_on_cran()` is correct and CRAN's check is never interactive. Status set to in-progress for the amendment alone; the amendment round should also add the interactive guard `test-cran-skip-helpers.R` needs, since it goes red under `devtools::test()` from an interactive console today. First amendment return on this milestone; defect-return count unchanged at 0.
- 2026-09-08: T2 done. `skip_on_cran()` added first in all five helpers, ahead of the binary question, so the reason reported on CRAN is "On CRAN" whether or not the machine has the binary. `tests/testthat/test-cran-skip-helpers.R` asserts which skip fires, never a bare one; proven able to fail by two planted defects — dropping the call from `skip_if_no_ffprobe()` (red on the missing skip and on the wrong reason) and moving it below the binary check in `skip_if_no_mediainfo()` (red on the ordering test alone). `devtools::test()` clean: FAIL 0, WARN 10, SKIP 18, PASS 13188.
- 2026-09-08: re-audit: AC1 (full) — six findings on the drafted amendment. Three fixed before the gate: the named verification command did not run (`Rscript tests/testthat/test-cran-skip-helpers.R` has no `library()`/`source()`, so `testthat::test_local(filter=)` is named instead); the no-skip half could be vacuous on a machine missing a binary (the run condition "all three on `PATH`" added, which the sibling criteria already carry); "a reason naming CRAN" was looser than the file's own assertion (now the reason `"On CRAN"`). One declined: citing the planted-defect matrix inside AC1 would bind a property of the harness, which the instrument question forbids — the plants stay in T2's and T6's work-log lines. Two posed at the mini gate (below).
- 2026-09-08: amendment return: AC1 — "In a non-interactive R session — the condition `R CMD check` runs the test suite under — on a machine with `ffmpeg`, `ffprobe` and `mediainfo` all on `PATH`, each of `skip_if_no_ffmpeg()`, `skip_if_no_ffprobe()` and `skip_if_no_mediainfo()` skips with the reason `"On CRAN"` when `NOT_CRAN` is unset, and does not skip when `NOT_CRAN` is set to `true`. Verified by `Rscript -e 'testthat::test_local(filter = "cran-skip-helpers")'`; the file sets `NOT_CRAN` itself per assertion, so the runner's own `NOT_CRAN="true"` does not reach it." The interactive axis the review's F1 falsified is now excluded by the criterion's own domain rather than reinterpreted. Accepted unchanged at the mini gate, so the wording spends no re-entry.
- 2026-09-08: mini gate held AC1 at the three name-resolution helpers rather than widening it to all five, and closed the interactive failure as work (T6) rather than as a sixth criterion — the criteria set neither grows nor loosens on an amendment round. The two hardware-probe helpers keep their coverage through AC2's zero-spawn count and the test file, which asserts all five.
- 2026-09-08: T6 done, and AC1 re-verified. `test-cran-skip-helpers.R` now asks the CRAN branch twice: `NOT_CRAN="false"`, which reaches `testthat:::on_cran()`'s `as.logical()` branch and so fires in either session kind, asserted unconditionally for all five helpers; and `NOT_CRAN` unset, which means CRAN only where `interactive()` is `FALSE`, in its own block behind `skip_if(interactive())`. The ordering block moved to `"false"` for the same reason. `Rscript -e 'testthat::test_local(filter = "cran-skip-helpers")'`: FAIL 0, SKIP 0, PASS 18. The same file from `R --interactive`: FAIL 0, SKIP 1, PASS 13 — ten failures before this task, measured 2026-09-08. Proven able to fail against the guarded file by re-planting T2's two defects: dropping `skip_on_cran()` from `skip_if_no_ffprobe()` (3 failures non-interactive, 2 interactive) and moving it below the binary check in `skip_if_no_mediainfo()` (1 failure interactive, which is the block the old file could not have caught).
- 2026-09-08: `devtools::test()` clean after the amendment round — FAIL 0, WARN 10, SKIP 18, PASS 13193, five passes up on T5's 13188 and the same skip count. Status set back to review.
- 2026-09-08: amendment return: AC3 — "The two routes that escape AC2's shim are measured rather than assumed." Both clauses report zero spawns, which is what the criterion's second sentence demands, but neither instruments its route: `--mode=config`'s planted `R_USER_CONFIG_DIR` is overwritten by `tm_redirect_config()` in the very test files that take the config route (`tests/testthat/helper-program-config.R:41-43`, read at review), and with the three names off `PATH` every helper skips on `Sys.which()` before that route is consulted, so the mode's control cannot fail; `--mode=emptypath` the script itself labels UNINSTRUMENTED. The criterion, not the work, is what is wrong — the amendment should narrow AC3 to what the liveness probe shows, that the escape route is reachable. First amendment return on AC3; defect-return count unchanged at 0.
- 2026-09-08: amendment return: AC4 — "With `NOT_CRAN=true` and the three binaries on `PATH`, the set of skipped test names is the same as at this milestone's base commit." True in a non-interactive session, measured this pass at 18 identical names; from a console the branch skips one test the base does not, because T6's guard stands down there. AC4 names no procedure and does not bound the session's interactivity — the same unbounded-criterion shape AC1 carried, so the same amendment. First amendment return on AC4; defect-return count still 0.
- 2026-09-08: re-review gate chose the amendment round over merging as-is or amending AC4 alone. The round carries, beside the two criteria, the findings coupled to them: R3 (refuse to report a zero beside a failed run), R4 (`shQuote` the shim paths), R7 (the CRAN half of the `DESIGN.md:54` convention), R8 (the overstated comment and the loose `expect_error` regexp) and R12 (the stale decision-log prose). R5, R6, R9, R10, R11 and R13 stay logged for triage at the re-review's gate.

- 2026-09-08: re-audit: AC3 (full) — four findings on the drafted amendment, three fixed before the gate. The draft's "whose reported probe line count is at least 1" bound the measuring script's own liveness gate (`tools/cran_spawn_check.R:206-215`), which cannot report below 1 because a failure exits the script — the instrument question's case, so the subject was restated as package behaviour and the script demoted to procedure; "each program's remembered absolute location" described the three-program plant matrix where only FFmpeg is asserted (`:198`), narrowed to FFmpeg's; and Coverage still credited T3, corrected to T4, T7. The fourth, that AC3 could be retired outright since every config-route test file resolves only `tm_redirect_config()` stubs (`tests/testthat/helper-program-config.R:12-23`, `:41-43`) and never one of the three binaries, went to the mini gate.
- 2026-09-08: re-audit: AC4 (full) — three findings, all fixed before the gate. The draft named a comparison but no command producing each list, though the runner choice is load-bearing (`devtools::test()` and `test_local()` both force `NOT_CRAN="true"` inside the run); two empty lists would have diffed clean, the false-green shape this milestone already hit twice; and the two runs were not bound to one machine, where the hardware-probe helpers make skip sets machine-dependent (`tests/testthat/helper-skip.R:41-65`, `:135-157`). The reader found no unbounded quantifier and confirmed both amendments narrow rather than widen.
- 2026-09-08: amendment return: AC3 — "The config-directory route that AC2's `PATH` shim cannot see is a real route in the shipped package, not an assumption: with the three program names unresolvable on `PATH` and a logging stand-in written as FFmpeg's remembered location, `find_ffmpeg()` returns that location and `run_program()` spawns the stand-in, which logs the call. Procedure: `Rscript tools/cran_spawn_check.R --mode=config`, whose config-route probe reports at least one logged line. AC3 claims nothing about the suite's own spawn count under that mode, nor about the emptied-`PATH` route of `tests/testthat/helper-program-config.R:43`, which no stand-in can reach." Accepted unchanged at the mini gate, so the wording spends no re-entry.
- 2026-09-08: amendment return: AC4 — "With `NOT_CRAN=true` and the three binaries on `PATH`, the set of skipped test names is the same at this milestone's base commit `ea433d5` and at the branch head. Each list is produced by `Rscript -e 'testthat::test_local(reporter = \"summary\")'` — non-interactive by construction, the session kind `R CMD check` runs the suite under — on one machine against the same resolved binaries, the base checked out in a detached worktree; the two lists of skipped test names are sorted and `diff`ed, `diff` empty and both lists non-empty, so an empty capture is not read as agreement." Accepted unchanged at the mini gate, so the wording spends no re-entry.
- 2026-09-08: mini gate kept AC3 rather than retiring it, so the criteria set stays at five and neither grows nor loosens; the escape route it now certifies is package behaviour the shim is blind to, which is why the plan gate added the criterion. T7 and T8 added for the re-measurement and the four prose and assertion repairs the re-review coupled to this round.
- 2026-09-08: T8 edits in, task not yet ticked — `devtools::test()` has not been re-run, the machine being held by T7's three measurement runs. `cairn/DESIGN.md:54`'s execution-test convention now carries the CRAN half; `test-normalize-audio-batch.R`'s comment says the production path still probes and the mock is the test's own, and its two `expect_error()` patterns name which refusal they expect (`filter = "normalize-audio-batch"`: FAIL 0, PASS 104). The tightened patterns are proven able to fail: renaming the column in the refusal's own message keeps the old `"channels|whole"` pattern green and turns the new one red. The superseding entry for the two wrong decision-log statements is below.
- 2026-09-08: T7 done. `tools/cran_spawn_check.R` now refuses to report a spawn count beside a suite that exited non-zero, and `shQuote()`s the log path, the real binary path and the program name it bakes into each stand-in. Both proven able to fail: an instantly-dying suite planted into the runner prints `spawns logged (NOT A RESULT): 0` and `REFUSED: the suite exited 1` where the old script printed `SPAWNS LOGGED: 0`; and a `TMPDIR` holding a `$` and a single quote logged 0 lines for 3 direct calls under the pre-fix quoting against 3 after it. Re-measured against the fixed instrument: `--mode=path` control (`--not-cran`) 1226 spawns — ffmpeg 812, ffprobe 390, mediainfo 24 — exit 0 in 6 min; `--mode=path` CRAN condition 0 spawns, exit 0 in 3.8 min; `--mode=config` CRAN condition reports `config-route probe: live, 1 line(s) logged` and then 0 suite spawns, exit 0 in 3.3 min. AC2 and amended AC3 both hold on the fixed instrument, at the same figures as the pass before it.
- 2026-09-08: T8 done. `devtools::test()` clean after the T8 edits — FAIL 0, WARN 10, SKIP 18, PASS 13193, the same figures as after T6. `devtools::document()` leaves no diff. Status set back to review.

- 2026-09-09: step-7 approval: PR #122 approved for merge.

## Decisions

- 2026-09-08: the spawn measurement runs the suite through `R CMD INSTALL` plus
  `test_check()` from `tests/`, not through `devtools::test()` or
  `testthat::test_local()`. Both of those force `NOT_CRAN="true"` inside the run
  — `devtools:::r_env_vars()` carries it, and `test_local()` sets it too
  (measured 2026-09-08: a probe test printed `Sys.getenv("NOT_CRAN")` as `true`
  with the variable unset in the calling process). Under either runner
  `skip_on_cran()` can never fire, so an empty spawn log would have measured the
  runner rather than the change. `test_check()` sets only
  `TESTTHAT_IS_CHECKING`, which is what `tests/testthat.R` reaches under
  `R CMD check`.
- 2026-09-08: T3 first measurement, path mode. Control (`NOT_CRAN=true`): 1228 spawns — 812 ffmpeg, 392 ffprobe, 24 mediainfo — suite exit 0 in 5.7 min, so the stand-ins are visible. CRAN condition (`NOT_CRAN` unset): 4 spawns, not zero. Located by a per-file sweep over all 91 test files, then by tracing `base::system2()` to name the calling test.
- 2026-09-08: amendment (substantive, Scope In) at a mini gate: three test sites added, found by the measurement rather than by reading. `test-unguarded-argument-front-doors.R:289` hand-rolled `skip_if_not(nzchar(Sys.which("ffmpeg")))` instead of calling the helper, so it never picked up the CRAN skip (2 × `ffmpeg -codecs`); `test-normalize-audio-batch.R:228` and `:239` carry no binary guard by design, and were each spawning one `ffprobe -select_streams a` from the track-count check that runs ahead of the refusal they assert. No acceptance criterion changed.
- 2026-09-08: mini gate chose mocking `find_ffprobe()` to NULL in the normalize pair over skipping them on CRAN, so both keep running everywhere and their own "needs no ffmpeg binary" comment becomes true; an absent FFprobe is a documented state, `count_audio_streams()` answering NA and the check standing down, so the refusal under test fires from the same guard. Both files pass after the change.
- 2026-09-08: the log line the stand-ins write now folds newlines and carriage returns in the arguments to spaces — the suite passes metadata values containing both, and the first control run wrote 1230 lines for 1228 spawns. The count is lines, so the error only inflated; the per-program tally was what it broke.
- 2026-09-08: T3 done. Re-measured against the fixed instrument, same suite, same stand-ins: control (`NOT_CRAN=true`) 1226 spawns — 812 ffmpeg, 390 ffprobe, 24 mediainfo, suite exit 0 in 5.3 min; CRAN condition (`NOT_CRAN` unset) 0 spawns, exit 0 in 3.2 min.
- 2026-09-08: T4 done, and the first config-mode attempt was a false green worth recording. With `PATH=""` the suite exits 1 before reaching any program, so that mode's CONTROL logged 0 as well — an empty log from an instrument that cannot see. The mode now drops only the directories actually holding one of the three (`/opt/homebrew/bin` here), keeping every other tool, and refuses to report unless a liveness probe first resolves FFmpeg through the remembered location and logs the spawn. With the route proved live (1 line), the suite logged 0 spawns through it under both `NOT_CRAN` states, exit 0.
- 2026-09-08: AC3's emptied-`PATH` clause reports 0 spawns, and is recorded as UNINSTRUMENTED rather than as evidence: with `PATH` empty no stand-in is reachable either, and the run exits 1. What carries that route is the config run above, which realizes the same escape — a program resolved without consulting `PATH` — with a control that can fail.
- 2026-09-08: T5 done. AC4: the skipped-test sets are identical, base `ea433d5` and branch, 18 skipped tests each under `NOT_CRAN=true` with the binaries on `PATH`; the branch runs 1579 tests to the base's 1576, the three added by `test-cran-skip-helpers.R`. AC5: `R CMD check --as-cran` with `NOT_CRAN` unset reports 0 errors, 0 warnings, 1 NOTE, and the NOTE names only the new submission and the version's large components.
- 2026-09-08: timing. The check went 7m47s → 4m33.2s and its tests step `[368s/436s]` → `[223s/230s]`, 145 s off the tests step. The plan's gate expected about one minute, reasoning from a 5m06s binaries-hidden `devtools::test()`; skipping the execution tests also drops their fixture building and their waiting, not only FFmpeg's own time.
- 2026-09-08: no `NEWS.md` entry. The change is confined to the test suite and the measurement script; no exported behaviour, message or default moves, so there is nothing user-visible to record.
- 2026-09-08: all tasks done, `devtools::test()` clean — FAIL 0, WARN 10, SKIP 18, PASS 13188, the same figures as at T2. Status set to review.
- 2026-09-08: superseding the 2026-09-08 work-log line that recorded the
  question gate's choice of a committed `tools/cran_spawn_check.R`. Two of its
  statements are wrong, measured at the base commit `ea433d5`: the script costs
  no `.Rbuildignore` entry, because `^tools$` was already at `.Rbuildignore:21`
  before the branch was cut; and `tools/` held five scripts, not two —
  `build_vignettes_without_binaries.R`, `config_leak_check.R`,
  `pkgdown_duplicate_topics.R`, `vignette_chunk_guards.R` and
  `vignette_chunk_program_identity.R`. The choice itself stands unchanged: the
  script is committed rather than thrown away, and the precedent it matches is
  five siblings rather than two.

## Review

### Third pass — after the AC3/AC4 amendment (2026-09-09)

- Opened on branch head `be6b410`. `master` still at `ea433d5`, unmoved since
  the branch was cut, so nothing to merge in. PR #122 already open (draft).
- AC1 PASS, on the criterion's own procedure and under its own run conditions.
  `Rscript -e 'testthat::test_local(filter = "cran-skip-helpers")'`: FAIL 0,
  WARN 0, SKIP 0, PASS 18. The same `Rscript` reports `interactive()` as
  `FALSE`, and `ffmpeg`, `ffprobe` and `mediainfo` all resolve to
  `/opt/homebrew/bin` — the two conditions the criterion names. Nothing
  skipped, so no assertion was silently absent.
- AC2 PASS. `tools/cran_spawn_check.R --mode=path`, stand-ins prepended to
  `PATH` ahead of the real binaries, on branch head `be6b410`. Control
  (`--not-cran`): 1226 spawns — ffmpeg 812, ffprobe 390, mediainfo 24 — suite
  exit 0 in 7.4 min, so the instrument sees. CRAN condition (`NOT_CRAN` unset):
  0 spawns, suite exit 0 in 4.8 min. The non-empty control is what makes the
  zero evidence; both runs exited 0, so the script reported them as results
  rather than refusing them. **Re-measured after the gate-directed fixes**, on
  the shipped script rather than the one those figures came from: control 1226
  spawns — ffmpeg 812, ffprobe 390, mediainfo 24 — exit 0 in 5.3 min; CRAN
  condition 0 spawns, exit 0 in 3.4 min. Same figures either side of the fix.
- AC3 PASS, on the amended criterion's own procedure.
  `tools/cran_spawn_check.R --mode=config`: `/opt/homebrew/bin` dropped from
  `PATH` so none of the three names resolves there, a stand-in written as each
  program's remembered location, and the config-route probe reports
  `live, 1 line(s) logged` — `find_ffmpeg()` answered from the planted config
  file and `run_program()` spawned the stand-in, which logged the call. That
  one line is what the criterion asks for. The suite then logged 0 spawns and
  exited 0 in 4.2 min, recorded here as the criterion says it is: not evidence.
  **Re-measured after the gate-directed fixes** on the shipped script: the probe
  again reports `live, 1 line(s) logged`, the suite again logs 0, exit 0 in
  3.4 min.
- AC4 PASS, under the criterion's own procedure and session kind.
  `Rscript -e 'testthat::test_local(reporter = "summary")'` in each tree — base
  `ea433d5` in a detached worktree, branch head `be6b410` — on this one machine
  against the same `/opt/homebrew/bin` binaries. 18 skipped test names each,
  both lists non-empty, and `diff` of the two sorted lists is empty: identical,
  not merely equal in count.
- AC5 PASS. `R CMD check --as-cran --no-manual` on a freshly built tarball with
  `NOT_CRAN` unset (`env -u NOT_CRAN`) and the three binaries on `PATH`:
  **0 errors, 0 warnings, 1 NOTE**, `Status: 1 NOTE`, tests step `[184s/190s]`.
  The NOTE is `checking CRAN incoming feasibility`, and its whole body is
  `New submission` plus `Version contains large components (0.1.0.9000)` — the
  two the criterion allows, and nothing else. Against the base commit's 7m47s
  and `[368s/436s]`.

### Triage and dispositions (third pass, 2026-09-09)

**Fixed on the branch, before the merge marker.** V1 and V2 together:
`cairn/DESIGN.md`'s clause now says no *test* spawns a binary under CRAN's
check and states in the same sentence that examples and vignette chunks, gated
on `Sys.which()` alone, still do; the clause cites D090, appended this pass to
record the convention the clause states. V3: an empty control log is refused
rather than reported — `control_blind` now feeds the `reportable` flag, so such
a run prints `spawns logged (NOT A RESULT): 0` and `REFUSED`, and exits 1.
Proven able to fail by planting a control whose shim directory never reaches
the child's `PATH` while the run itself exits 0: the pre-fix script printed a
bare `SPAWNS LOGGED: 0` and exited 0. V4: the `helper-skip.R` comment now
states `on_cran()`'s unset branch — non-interactive only — and names the guard
that exists because of it. V5: the ROADMAP row's one-minute estimate corrected
in place against the measured figures, marked `corrected M118`. V6:
`shQuote(..., type = "sh")` on all three interpolations (a no-op on this
machine, where `"sh"` is already the default, and the point is Windows).

**Rejected, with reason.** V8 — pinning `rlang`'s own message text is what the
tightening is *for*: the criterion the repo holds a test to is that it names
which failure it expects, and an upstream wording change surfacing as one red
test is the cheap end of that trade. A `class = "rlang_error"` assertion would
not discriminate, since that class is shared by every rlang input check.

**Follow-up, one candidate row written at the post-merge hygiene pass** (held
until then so the ROADMAP's 60-line cap is never breached: this milestone's
row turning terminal prunes M115's under the three-terminal-row rule, and the
new row takes the freed line). It absorbs V7 (the emptied-`PATH` mode's exit
code cannot distinguish a new failure from the designed one), V9 with its
constraint (the two hardware-probe helpers have no control-half coverage, and
extending the existing control to them would itself spawn under
`NOT_CRAN=true`, so closing it needs a mock), and V10-V15 — the CI dependency
on `setup-r@v2`, `--lib=DIR`'s missing freshness guard, the two surviving
hand-rolled guards and the unshimmed `ffplay`, the script's three hygiene
defects, and the examples that still spawn on CRAN.

**PR conversation.** No reviews, no unresolved threads, one comment.
`conversation: codecov[bot] PR #122 — noted` (coverage report, requests
nothing).

**Return floor.** None of the sixteen demonstrates an acceptance criterion
failing, and none is a load-bearing defect in what the package does for its
users, so no finding returns the milestone. Defect-return count stays 0;
amendment returns stand at three (AC1, AC3, AC4), each once.

### Consistency gate — PASS (third pass)

`cairn_validate.py` exit 0, 23 checks green, no `release window` advisory.
`cairn_impact.py` not run — `Principles touched:` is `—` and no DESIGN.md
principle changed (the DESIGN.md edit is a Conventions bullet, not an IP/GP).
Toolchain half, from the `r-package` profile: `devtools::document()` leaves the
tree clean; `R CMD check --as-cran` 0 errors / 0 warnings / 1 allowed NOTE
above; `pkgdown::check_pkgdown()` reports no problems; README.Rmd and README.md
untouched on this branch (last changed at `0cf121d`, before the base) and in
sync; no new exported object, so no `_pkgdown.yml` row owed; no `NEWS.md` entry
owed, the change being confined to the test suite, an `.Rbuildignore`d script
and tracking files; `^tools$` present at `.Rbuildignore:21`.

### Independent review — three lenses, full fan-out (third pass)

Declared surface tier is user-facing and the diff touches executable surface,
so all three lenses ran fresh-context on distinct evidence bases. V-numbers are
this pass's; F- and R-numbers refer to the two passes recorded below.

**[S] blame-history — no findings.** Traced every modified block to the commit
that introduced it. `skip_on_cran()` sits ahead of the nvenc/videotoolbox probe
encodes because the probe is itself a spawn, so a later placement would be too
late; the `find_ffprobe` NULL mock puts the code into the NA stand-down
`R/ffprobe.R:210-238` already documents; the loose `"channels|whole"` regexp it
traced to `a33f2cb`, a mechanical rename-era commit, so it was never a
deliberate guard against message instability and tightening it undoes nothing;
the guard replaced at `test-unguarded-argument-front-doors.R:289` is a straight
bugfix losing no coverage. It re-verified the CI claim against
`.github/workflows/R-CMD-check.yaml:47` and `test-coverage.yaml:26`.

**[S] prior-review — no regressions.** `gh api .../pulls/comments` returned
`[]`, so the PR-thread walk was skipped by the probe gate and the doctrine
modules were the surface. It confirmed R1, R2, R3, R4, R7, R8 and R12 each
actually repaired rather than merely claimed, checking the base commit's
`tools/` listing and `.Rbuildignore` itself. It re-surfaced R9 and R11 as still
open in a file this pass edited — both already deferred, carried below as V15.

**[O] diff-bug — sixteen findings.** Consolidated below, most severe first.
Two of them (its findings 2 and 3, that AC4 and AC5 had no evidence at the
current head) are closed by this pass's own runs above and are not carried
forward as findings.

- **V1.** `cairn/DESIGN.md:54-56`, added this pass repairing R7, now states
  something the branch does not deliver: "`skip_on_cran()` ahead of that, so
  CRAN's own check of the tarball spawns no binary even where its machine has
  one". The check also runs 21 `@examplesIf nzchar(Sys.which(...))` example
  blocks and the vignette chunks gated the same way, every one of which spawns
  on a CRAN machine that has the binary. The Goal is correctly scoped to tests
  and R13 recorded the examples fact as out of scope; the repair carried that
  out-of-scope fact into a false statement in the architecture record. No
  acceptance criterion fails. New this pass.
- **V2.** `cairn/DESIGN.md:56` attributes the new CRAN clause to
  `(D004, D024, D034)`, none of which decides anything about CRAN skipping, and
  no D-entry was added — `DECISIONS.md` runs to D089 with no CRAN-skip entry.
  The citation was correct for the pre-existing sentence and the added clause
  inherits it without authority. New this pass.
- **V3.** `tools/cran_spawn_check.R:254-275` refuses one false green and still
  prints the other. T7's `reportable <- instrumented && status == 0L` catches a
  dead suite; it does not catch a *control* run whose log came back empty. A
  `--not-cran --mode=path` invocation whose shim directory failed to reach the
  child's `PATH` prints `SPAWNS LOGGED: 0` and exits 0 — the shape AC2's second
  sentence exists to rule out, left to the operator's eye across two
  invocations. `self_test()` does not close it: it calls the stand-ins by
  absolute path before any `PATH` surgery, so it proves the shims log, never
  that the run can see them. New this pass; same family as R3/F2.
- **V4.** `tests/testthat/helper-skip.R:5-11` states `skip_on_cran()`'s rule
  wrongly — "reads NOT_CRAN and skips unless it is `"true"`". With `NOT_CRAN`
  unset `testthat:::on_cran()` returns `!interactive()`, so an interactive
  console does not skip. That is the F1 falsification, and the reason AC1 had
  to be bounded and T6's guard added; the comment is where a maintainer looks
  for the rule. New this pass (it landed at T2 and survived both earlier
  passes).
- **V5.** `cairn/ROADMAP.md:29` still carries the estimate this milestone
  falsified — "ffmpeg execution is about one minute of six and M118's
  `skip_on_cran()` buys only that" — against the measured 7m47s → 4m33.2s and
  `[368s/436s]` → `[223s/230s]`. The diff touches the file and left the stale
  sentence, which is the row's promotion condition. New this pass.
- **V6.** `tools/cran_spawn_check.R:93,95` calls `shQuote()` without
  `type = "sh"`, whose default is `type = "cmd"` on Windows — so the fix that
  makes the shim source correct on POSIX would emit cmd-style quoting into a
  `#!/bin/sh` file there. New this pass, low.
- **V7.** `tools/cran_spawn_check.R:268` gates the non-zero exit on
  `instrumented`, so `--mode=emptypath` exits 0 however the suite died: a run
  that dies for a new reason is indistinguishable at the exit-code level from
  the designed one, separated only by the prose NOTE. F2 says a blanket
  `stop()` would be wrong, so this is narrow. New this pass, low.
- **V8.** `tests/testthat/test-normalize-audio-batch.R:236` pins an upstream
  `rlang` message string — `` "`channels` must be a whole number" `` is
  `check_number_whole()`'s own wording, not the package's, so it will break on
  an rlang wording change for a reason unrelated to tidymedia. Its sibling at
  `:258` targets the package's own `cli_abort()`. New this pass, minor.
- **V9.** R6 stands, and the obvious fix conflicts with AC2: extending
  `test-cran-skip-helpers.R`'s `NOT_CRAN=true` control to the two hardware-probe
  helpers would call each with `NOT_CRAN=true`, and each then runs a one-frame
  probe encode — on a CRAN machine with FFmpeg that is a spawn from a file with
  no CRAN skip of its own, falsifying AC2. Closing R6 needs a mock, not the
  naive extension. Repeats R6; the constraint is new.
- **V10-V15.** Repeats, all still open and all deferred at the re-review gate:
  R5 (nothing in the repo fails if `setup-r@v2` stops exporting `NOT_CRAN`);
  R9 (`--lib=DIR` skips `R CMD INSTALL` with no freshness guard); R10 (two
  hand-rolled binary guards survive, and `ffplay` is shimmed by nothing);
  R11 (`TIDYMEDIA_SPAWN_LOG` set and never read, `R_LIBS` gains a trailing
  empty entry, the required working directory undocumented); R13 (examples
  spawn on CRAN — now also the subject of V1).

Verified by the [O] lens and explicitly *not* problems: `skip_on_cran()` really
is first in all five helpers; `test-cran-skip-helpers.R` spawns nothing under
CRAN conditions, block by block; the file still passes under the DESCRIPTION
floor `testthat (>= 3.0.0)`, whose `on_cran()` is the simpler predicate; the
`find_ffprobe` NULL mock does reach `count_audio_streams()`'s documented
`NA_integer_` stand-down; both tightened patterns match without cli
line-wrapping risk; the config-mode probe genuinely proves AC3's route, because
the `leftover` check first establishes the three names are unresolvable on the
trimmed `PATH`, so `find_ffmpeg()`'s answer can only come from the planted
config file; T8's absence from the Coverage map is fine, since the rule maps
criteria to tasks and not the reverse; and `test-ffmpeg-exit-condition.R:141`
and `test-hardware-backends.R:319` spawn via bare `system2()` but sit under
helper skips in their own bodies.

### Re-review after the AC1 amendment (2026-09-08)

- Opened on branch head `dc69b79`. `master` still at `ea433d5`, unmoved since the
  branch was cut, so nothing to merge in. PR #122 already open (draft); all ten
  CI legs green on `dc69b79` — six `R-CMD-check` platform legs, `pkgdown`,
  `test-coverage`, and both codecov gates.
- AC1 PASS, on the criterion's own procedure and under its own run conditions.
  `Rscript -e 'testthat::test_local(filter = "cran-skip-helpers")'`: FAIL 0,
  WARN 0, SKIP 0, PASS 18. `Rscript` reports `interactive()` as `FALSE`, and
  `ffmpeg`, `ffprobe` and `mediainfo` all resolve to `/opt/homebrew/bin`, which
  are the two conditions the amended criterion names. The 18 passes are the
  file's four blocks: five helpers asserted to report `"On CRAN"` with
  `NOT_CRAN="false"`, the same five with `NOT_CRAN` unset, the three name
  helpers asserted NOT to skip with `NOT_CRAN=true` and the binary on `PATH`
  (the control half, which is what rules out an unconditionally-skipping
  helper), and five ordering assertions with `PATH = ""`. Nothing skipped, so
  no assertion was silently absent.

- AC4 PASS, in the non-interactive session the run was made in. Skipped-test
  sets compared by name, base `ea433d5` in a detached worktree against branch
  head `dc69b79`, both under `NOT_CRAN=true` with the three binaries on `PATH`:
  18 skipped test names each, and `diff` of the two sorted lists is empty —
  identical, not merely equal in count. Expectation counts 13221 on the branch
  against 13203 at base, the 18 added by `test-cran-skip-helpers.R`. The
  session-kind caveat is finding R2 below, which is why this line names the
  session it ran in.

- AC2 PASS. `tools/cran_spawn_check.R --mode=path`, stand-ins prepended to
  `PATH` ahead of the real binaries. Control (`--not-cran`): 1226 spawns —
  ffmpeg 812, ffprobe 390, mediainfo 24 — suite exit 0 in 5.7 min, so the
  instrument sees. CRAN condition (`NOT_CRAN` unset): 0 spawns, suite exit 0 in
  3.7 min. Same figures as the first pass, on the amended branch.
- AC3 PASS as written, and the writing is the problem — see finding R1.
  `--mode=config`: the liveness probe resolved FFmpeg through a remembered
  location and logged 1 line in both runs, and the suite then logged 0 spawns
  under both `NOT_CRAN` states, exit 0 in 3.7 min each. `--mode=emptypath`:
  0 spawns, suite exit 1, the script's own output labelling the mode
  UNINSTRUMENTED. Both clauses therefore "report zero spawns" literally, which
  is what AC3 demands; neither clause delivers AC3's opening sentence, that the
  two escape routes are "measured rather than assumed".
- AC5 PASS. `R CMD check --as-cran --no-manual` with `NOT_CRAN` unset and the
  three binaries on `PATH`: **0 errors, 0 warnings, 0 notes**, `Status: OK`,
  Duration 5m26.8s, tests step `[243s/308s]`. Against the base commit's 7m47s
  and `[368s/436s]`. Slower than the first pass's 3m42s because two other
  measurement runs shared the machine; the criterion is about the counts, and
  they are clean.

### Consistency gate — PASS (re-review)

`cairn_validate.py` exit 0, 23 checks green, no `release window` advisory.
`cairn_impact.py` not run — `Principles touched:` is `—` and no DESIGN.md
principle changed. Toolchain half, from the `r-package` profile:
`devtools::document()` leaves the tree clean; `devtools::check()` 0/0/0 above;
`pkgdown::check_pkgdown()` reports no problems; README.Rmd and README.md
untouched and in sync; no new exported object, so no `_pkgdown.yml` row owed;
no `NEWS.md` entry owed, the change being confined to the test suite and an
`.Rbuildignore`d script; `^tools$` present at `.Rbuildignore:21`, and
`check()` raised no missing-ignore NOTE. CI: all ten legs green on `dc69b79`.

### Independent review — three lenses, full fan-out (re-review)

Declared surface tier is user-facing and the diff touches executable surface,
so all three lenses ran fresh-context on distinct evidence bases. F-numbers
below refer to the first pass's findings, kept in the section that follows.

**[S] blame-history — no findings.** Traced every modified block to the commit
that introduced it: `skip_on_cran()` only prepends an early exit ahead of M31's
and M100's probe logic rather than disturbing it; the `find_ffprobe` NULL mock
exercises the documented NA-standdown path and the real spawn/warn path keeps
its coverage in `test-audio-track-drop.R` and `test-check-tracks-seam.R`; the
hand-rolled guard replaced at `test-unguarded-argument-front-doors.R:289`
traces to M096 with no recorded reason for bypassing the shared helper. No
D-entry addresses CRAN skip behaviour, so none is contradicted.

**[S] prior-review — two regressions of taught lessons.** No archived `## Review`
section names these files, and `gh api .../pulls/comments` returns `[]`, so the
prior-review surface is the doctrine modules. Against those: R3 below is the
shape M117's review found in the sibling `tools/config_leak_check.R` one
milestone ago ("a PASS reported for a command that never ran"), and R1 is
`false-greens.md`'s "control that stopped controlling". Both were found by this
milestone's own first pass and deferred rather than repaired. Its third
observation — that config mode redirects `R_USER_CONFIG_DIR` but not
`XDG_CONFIG_HOME` — it withdrew on reading `R/program_management.R:83-90`,
since the legacy path is consulted only when the new location holds no file.

**[O] diff-bug — fifteen findings.** Consolidated below, most severe first.

- **R1 (criterion-level; AC3).** Neither of AC3's two clauses instruments the
  route it names. `--mode=config` plants a remembered location and points
  `R_USER_CONFIG_DIR` at it, but the test files that actually take that route
  call `tm_redirect_config()`, which sets its own `R_USER_CONFIG_DIR` and
  `PATH = ""` (`tests/testthat/helper-program-config.R:41-43`) — so the plants
  are invisible to exactly those files. Verified by reading the helper at this
  review. The mode's suite-level control cannot fail either: with the three
  names off `PATH`, every helper skips on `Sys.which()` before the config route
  is consulted, so its `NOT_CRAN=true` control logs 0 for a reason unrelated to
  the property. Only the liveness probe can fail, and it tests the route, not
  the suite. `--mode=emptypath` is UNINSTRUMENTED by the script's own label.
  AC3's clauses are literally satisfied and its opening promise is not.
  Extends F3.
- **R2 (criterion-level; AC4).** AC4 quantifies over sessions without bounding
  interactivity, the shape AC1 was just amended for. The new guard means the
  branch skips one test an interactive session that the base commit does not,
  so the skip sets are identical non-interactively (measured above) and differ
  by one from a console. Not a defect in the work.
- **R3.** `tools/cran_spawn_check.R:221-248` captures the suite's exit status
  and only prints it; nothing branches on it, so a run that dies early prints
  `SPAWNS LOGGED: 0` in the same shape as a genuine clean zero. Repeats F2, and
  M117's review found the identical shape in the sibling script.
- **R4.** `tools/cran_spawn_check.R:87-93` interpolates the log path and the
  real binary path into `/bin/sh` shim source with `sprintf` and no `shQuote`,
  so a `TMPDIR` containing a space, quote or `$` yields a shim that either
  fails to log — a silent under-count, since the `exec` still runs — or fails
  to exec. New this pass.
- **R5.** Nothing in the repo fails if CI stops setting `NOT_CRAN`. T1
  established the value comes from `r-lib/actions/setup-r@v2`, which no file
  here controls; if that changes, both workflows go green with the execution
  coverage silently gone. New this pass.
- **R6.** `test-cran-skip-helpers.R`'s control half covers only the three name
  helpers, so `skip_if_no_nvenc()` and `skip_if_no_videotoolbox()` are pinned
  by their CRAN-reason assertions alone: either helper reduced to a bare
  `skip_on_cran()`, losing its `Sys.which`, encoder-list and probe-encode
  logic, passes every block green. New this pass.
- **R7.** `cairn/DESIGN.md:54` still states the convention as "Command
  execution tests `skip_if` the ffmpeg/mediainfo binaries are absent"; the new
  and load-bearing half, "and on CRAN", lives only as a comment in
  `helper-skip.R`. New this pass.
- **R8.** The comment added at `test-normalize-audio-batch.R` overstates what
  changed — the production path still probes; the suppression is the test's
  mock — and the pre-existing `expect_error(..., "channels|whole")` regexp is
  loose enough to match an error raised by the newly-NULL FFprobe location
  rather than by the refusal under test. Extends F6.
- **R9.** `--lib=DIR` skips `R CMD INSTALL` with no freshness guard, so a
  re-run after an edit can measure the previously installed package. Repeats F8.
- **R10.** Two hand-rolled binary guards of the class this milestone patched
  survive — `test-program-status-and-unset.R:33` and `test-nvenc-memo.R:92` —
  each spawning nothing today only because of a neighbouring guard or mock, and
  `ffplay` is shimmed by nothing at all. Repeats F4/F5.
- **R11.** Script hygiene: `env[["TIDYMEDIA_SPAWN_LOG"]]` is set and never read;
  the harness is POSIX-only and the header documents every other assumption;
  `R_LIBS` gains a trailing empty entry and the required working directory is
  undocumented. Repeats F7/F9 plus one new pair.
- **R12.** Stale prose in this file's decision log: the script "costs one
  `.Rbuildignore` entry" (`^tools$` was already there) and matches "the two
  measurement scripts already in `tools/`" (there are five). Repeats F10.
- **R13.** Outside the Goal's wording but bounding its win: `R CMD check` still
  runs `@examplesIf nzchar(Sys.which("ffmpeg"))` examples, which spawn binaries
  wherever CRAN's machine has them. The Goal says "tests", so nothing is
  unsatisfied. New this pass.

Dropped by the [O] lens after checking, and recorded so they are not re-found:
the shim's `tr` under an inherited `PATH=""` does not under-count (the line is
still written); `test-cran-skip-helpers.R` spawns nothing under CRAN;
`expect_match(NA_character_, ...)` fails cleanly rather than erroring; the
`names(present)` index is correct; the `find_ffprobe` mock does reach
`count_audio_streams()`; no `skip_if_no_*()` is called at any file's top level,
swept across all 91 test files, so the added skip cannot abort a whole file.

**PR conversation.** No reviews, no unresolved threads, one comment.
`conversation: codecov[bot] PR #122 — noted` (coverage unchanged at 98.43%,
requests nothing).

### Disposition — amendment return on AC3 and AC4

The milestone's Goal is met and the work is right: CRAN's check makes no spawn,
and every other run still makes all 1226. What R1 and R2 show is that two
criteria promise more than the evidence behind them delivers — AC3 claims a
measurement of two routes that neither of its clauses instruments, and AC4
quantifies over session kinds without naming one. Under the never-reinterpret
rule neither can be read charitably at review, so both are amendment returns,
not defect returns, and neither increments the defect-return count. Status is
`in-progress` for that amendment alone; review stops here and resumes after it.

### First pass (2026-09-08)

- 2026-09-08: review opened. Branch synced with `master` at `ea433d5`; `master`
  has not moved since the branch was cut, so nothing to merge in. Draft PR #122
  opened for CI.
- AC1 PASS. `devtools::test(filter = "cran-skip-helpers")` on the branch:
  FAIL 0, WARN 0, SKIP 0, PASS 13 — the three `test_that()` blocks of
  `tests/testthat/test-cran-skip-helpers.R` all green. They assert *which* skip
  fires, never a bare one: with `NOT_CRAN` unset each of the five helpers
  reports a reason matching "On CRAN"; with `NOT_CRAN=true` and the binary on
  `PATH` each of the three name helpers does not skip at all (the control half,
  which is what rules out an unconditionally-skipping helper); and with
  `PATH = ""` and `NOT_CRAN` unset — both conditions true at once — the reason
  is still "On CRAN", which is the ordering assertion. AC1 names the three name
  helpers; the file covers those plus the two hardware-probe helpers.
  **Superseded — see the amendment return below. AC1's box is unticked: the
  measurement above is sound but non-interactive, and the criterion as written
  quantifies over more than that.**
- AC2 PASS. `tools/cran_spawn_check.R --mode=path`, stand-ins prepended to
  `PATH` ahead of the real binaries. Control (`--not-cran`): 1226 spawns —
  ffmpeg 812, ffprobe 390, mediainfo 24 — suite exit 0 in 6.6 min, so the
  instrument can see. CRAN condition (`NOT_CRAN` unset): 0 spawns, suite exit 0
  in 4.4 min. The non-empty control is what makes the zero evidence.
- AC3 PASS as written, with the caveat the plan already recorded. `--mode=config`
  (the three names dropped from `PATH` by removing `/opt/homebrew/bin`, a
  remembered absolute location for each pointing at a stand-in): the liveness
  probe resolved FFmpeg through the remembered location and logged 1 line, so
  the route is proved reachable; the suite then logged 0 spawns under both
  `NOT_CRAN` states, exit 0. `--mode=emptypath`: 0 spawns, but suite exit 1 and
  the script itself reports `instrumented=FALSE` — recorded as UNINSTRUMENTED,
  not as evidence, exactly as the milestone's own decision log states. See
  finding F3 below on how much the config zero carries.
- AC4 PASS. Skipped-test sets compared by name, base `ea433d5` in a detached
  worktree against the branch, both under `NOT_CRAN=true` with the three
  binaries on `PATH`: 18 skipped test names each, and `diff` of the two sorted
  sets is empty — identical, not merely equal in count. Branch runs 1579 tests
  to the base's 1576, the three added by `test-cran-skip-helpers.R`.
- AC5 PASS. `R CMD check --as-cran` (`--no-manual --as-cran`) with
  `NOT_CRAN` unset and the three binaries on `PATH`: **0 errors, 0 warnings,
  0 notes**, `Status: OK`, Duration 3m42s, tests step `[196s/202s]`. The
  criterion allows one version/new-submission NOTE; there were none. Against
  the base commit's 7m47s and `[368s/436s]` recorded at T5.

### Consistency gate — PASS

Universal cairn-file checks: `cairn_validate.py` exit 0, all 16 PASS/OK checks
green, no `release window` advisory. `cairn_impact.py` skipped — the milestone's
`Principles touched:` slot is `—` and no DESIGN.md principle changed.

Toolchain checks, from the `r-package` profile's `consistency-gate` slot:
`devtools::document()` produces no diff (`git status --porcelain` empty after);
`devtools::check()` clean at 0/0/0 (above); `pkgdown::check_pkgdown()` passes;
README.Rmd and README.md are in sync (untouched, same commit); no new exported
object, so no `_pkgdown.yml` row is owed; no `NEWS.md` entry owed — the change
is confined to the test suite and an `.Rbuildignore`d measurement script, and
the gate asks only for user-visible changes; `tools/` is already covered by
`.Rbuildignore`'s `^tools$`, and `check()` raised no missing-ignore NOTE.

### Independent review — three lenses, full fan-out

Declared surface tier is user-facing, so the full three-lens fan-out ran, each
lens fresh-context on a distinct evidence base.

**[S] blame-history — no findings.** The probe-encode ordering in
`skip_if_no_nvenc()`/`skip_if_no_videotoolbox()` preserves M31's and M100's
deliberate design rather than undoing it; the `find_ffprobe` NULL mock exercises
D024's documented NA-standdown path and matches a pattern already used in four
test files; the hand-rolled guard replaced in
`test-unguarded-argument-front-doors.R` was never a deliberate variant.

**[S] prior-review — no regression.** The existence probe returned `[]`: this
repo has no inline PR review comments at all, so that surface was skipped after
one call. No archived `## Review` section names any touched file. One
related-file observation surfaced for completeness, which is F2 below.

**[O] diff-bug — ten findings, listed with disposition.**

- **F1 (floor-qualifying; routes to an amendment return — see below).** The new
  `test-cran-skip-helpers.R` goes red in any *interactive* session, and AC1 as
  written is falsified there. Verified independently at this review, not taken
  on the reviewer's account: `testthat:::on_cran()` (testthat 3.3.2) is
  `if (identical(Sys.getenv("NOT_CRAN"), "")) !interactive() else
  !isTRUE(as.logical(env))`. The test file unsets `NOT_CRAN` via
  `with_envvar(c(NOT_CRAN = NA))`, which lands on the `""` branch, so under
  `R --interactive` `on_cran()` is `FALSE` and `skip_if_no_ffmpeg()` returns
  without skipping (measured: reason `NA`). Test 1 then expects `"On CRAN"` and
  gets `NA`; test 3 gets the binary-absence reason. The maintainer's own
  `devtools::test()` from RStudio would report two failures. Every run recorded
  in this milestone was non-interactive, which masked it.
- **F2 (deferred to re-review triage).** `tools/cran_spawn_check.R` captures the
  suite's exit `status` and only `cat()`s it — nothing branches on it, so a run
  that dies early still prints `SPAWNS LOGGED: 0`, which reads as a clean zero.
  Confirmed by reading the script. This is the shape M117's review found in the
  sibling `tools/config_leak_check.R` ("a PASS reported for a command that never
  ran"), and the emptypath false green the work log describes was this same
  failure mode, fixed by labelling that one mode rather than by refusing to
  report a zero beside a non-zero exit. A blanket `stop()` is the wrong repair —
  `emptypath` exits 1 by design.
- **F3 (deferred to re-review triage).** `config` mode prints
  `instrumented=TRUE`, but its suite-level control cannot fail: with the three
  names off `PATH` every helper skips on `Sys.which()` alone, so the suite never
  reaches a remembered location whatever `NOT_CRAN` says, and the `NOT_CRAN=true`
  control logs 0 for that reason rather than a good one. Only the liveness probe
  can fail, and it tests the route, not the suite. AC3's second clause is
  therefore weaker than AC2's — it shows the config route is reachable, not that
  the suite declines to use it. The script warns about exactly this for
  `emptypath` and not for `config`.
- **F4, F5 (deferred; candidate-row material).** Two hand-rolled binary guards
  of the same shape as the one this milestone patched survive, both confirmed by
  reading: `test-program-status-and-unset.R:33`
  (`skip_if_not(nzchar(Sys.which("ffplay")), ...)`), saved today only because
  `skip_if_no_ffmpeg()` sits three lines above it; and `test-nvenc-memo.R:92`
  (`skip_if(!nzchar(ffmpeg_path), ...)`), which spawns nothing today only
  because `local_encoder_probe_counter()` mocks `ffmpeg_encoders()`. Related:
  the instrument shims three of the four programs `find_program()` knows —
  `ffplay` is unmeasured, though it is outside AC2's stated domain by
  construction.
- **F6 (deferred; comment accuracy).** The added comment in
  `test-normalize-audio-batch.R` says the "needs no ffmpeg binary" note is "now
  true rather than nearly so", but the production path still runs the
  track-count probe first — the suppression lives in the test's mock, not in the
  code. The mock itself is correct and the assertions still test what they claim.
- **F7, F8, F9 (deferred; script hygiene).** `env[["TIDYMEDIA_SPAWN_LOG"]]` is
  set and never read, implying a mechanism that does not exist (the shims bake
  the path in at generation time). `--lib=DIR` skips `R CMD INSTALL` with no
  freshness guard, so a re-run after an edit can measure the previously
  installed package. The script is POSIX-only (`#!/bin/sh` shims, `tr`) and the
  header documents every other assumption but not this one.
- **F10 (deferred; stale prose).** Two statements in this file's decision log
  are wrong: the script "costs one `.Rbuildignore` entry" (`^tools$` was already
  there, and the diff correctly adds nothing), and it matches "the two
  measurement scripts already in `tools/`" (there are five).

Verified by the [O] lens and explicitly *not* problems: the CI claim in the new
`helper-skip.R` comment is accurate — `r-lib/actions/setup-r`'s
`src/installer.ts:807` does
`if (!process.env["NOT_CRAN"]) core.exportVariable("NOT_CRAN", "true")`, and
both test-running workflows use `setup-r@v2`, so neither CI checks nor covr
coverage are gutted; `withr` is in Imports; no `skip_if_no_*()` is called at
test-file top level, so the added skip cannot abort a whole file.

### Disposition — amendment return on AC1

F1 does not show the work wrong. `skip_on_cran()` is the correct idiom and
CRAN's own check is never interactive, so the milestone's Goal is met. What F1
shows is that **AC1's text is wrong**: it names no procedure and no measurement
context, and quantifies over "when `NOT_CRAN` is unset" without bounding the
session's interactivity — a dimension across which the behaviour genuinely
differs. Under the never-reinterpret rule a charitable reading is not available
at review, so this is an amendment return (M130), not a defect return, and it
does not increment the defect-return count the thrash rule reads.

The amendment round should also carry the interactive guard the test file needs;
F2-F10 stay logged here and take their triage at the re-review's gate.

