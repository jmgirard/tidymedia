# M118: The binary-executing tests skip on CRAN's own check

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — what runs on CRAN's machines is the shipped tarball's behaviour
- **Branch/PR:** —

## Goal

CRAN's own submission check runs none of the tests that spawn FFmpeg, FFprobe or
MediaInfo, while every other run of the suite still does.

## Scope

**In:** `skip_on_cran()` in the three `skip_if_no_*` helpers
(`tests/testthat/helper-skip.R:4-23`), and the measurement that shows no spawn
survives it.

**Out:** cutting the pure-R suite to reach a check-time target → declined at this
plan's gate; the maintainer chose the cheap fix on the measurement below.
A ROADMAP candidate row holds the profiling work.

## Acceptance criteria

- [ ] AC1: Each of `skip_if_no_ffmpeg()`, `skip_if_no_ffprobe()` and
      `skip_if_no_mediainfo()` skips when `NOT_CRAN` is unset, and does not skip on
      the CRAN account when `NOT_CRAN` is set to `true`.
- [ ] AC2: With `NOT_CRAN` unset and the three binaries on `PATH`, a full run of the
      suite makes no spawn that resolves one of the three names through `PATH`.
      Measured by shimming the three names onto `PATH` ahead of the real ones with a
      wrapper appending one line per call to a log; the same shim under
      `NOT_CRAN=true` writes a non-empty log, which is what shows the instrument can
      detect a spawn.
- [ ] AC3: The two routes that escape AC2's shim are measured rather than assumed.
      Under `NOT_CRAN` unset, a run with `PATH` emptied — the condition
      `tests/testthat/helper-program-config.R:43` creates for whole test files — and a
      run with a shim installed at a remembered absolute location for each of the three
      programs, which `find_program()` resolves without consulting `PATH`, each report
      zero spawns.
- [ ] AC4: With `NOT_CRAN=true` and the three binaries on `PATH`, the set of skipped
      test names is the same as at this milestone's base commit.
- [ ] AC5: `R CMD check --as-cran` with `NOT_CRAN` unset and the three binaries on
      `PATH` reports 0 errors, 0 warnings, and no note other than one naming the
      version number or a new submission.

## Coverage

- AC1 → T2
- AC2 → T3
- AC3 → T3, T4
- AC4 → T5
- AC5 → T5

## Tasks

- [ ] T1: Measure, do not trust, the precedent the repo records at
      `tests/testthat/test-runtime-timeout.R:188-190` — that `devtools::check()` and
      the CI workflow both set `NOT_CRAN`, so CI keeps running these tests.
      `.github/workflows/R-CMD-check.yaml` sets no `NOT_CRAN` of its own, so the value
      comes from `r-lib/actions/check-r-package@v2`. If it does not, this milestone
      would silently gut CI coverage and stops here for a re-gate.
- [ ] T2: Add `skip_on_cran()` to the three helpers at `helper-skip.R:4-23`.
- [ ] T3: Build the PATH-shim spawn counter and run the suite in both modes.
- [ ] T4: Run the two escape-route conditions of AC3.
- [ ] T5: Record the base-commit skipped-test set, then run `devtools::test()` in
      developer mode and `R CMD check --as-cran` in CRAN mode; record each run's
      `Duration` and tests-step timing against the base commit's 7m47s and
      `[368s/436s]` (measured 2026-09-07).

## Work log

- 2026-09-07: created by /milestone-plan.
- 2026-09-07: plan-gate criteria audit ran in FULL mode (declared tier user-facing), two rounds, fresh-context [O] reader. Findings against this milestone: AC1's "skips on that account alone for no other reason" was self-contradictory, since the helpers must keep skipping for an absent binary (repaired); AC2's "every spawn the run actually makes" was unbounded, the PATH shim seeing only bare-name resolution while a remembered absolute location and `helper-program-config.R:43`'s emptied `PATH` both escape it (repaired — AC2 narrowed to `PATH`-resolved spawns, AC3 added for the two escape routes, and a control run added so an empty log is not read as success); AC4's equal skip counts passed a swap (repaired — compares the set of skipped test names); AC3's timing sentence was an unfalsifiable recording act (repaired — moved to T5).
- 2026-09-07: plan gate chose `skip_on_cran()` in the three helpers over profiling and cutting the pure-R suite, because the measurement showed binary execution is about one minute of the six — `R CMD check` 7m47s with the binaries against a 5m06s binary-absent suite run (2026-09-07) — so the larger cut buys little against real risk to 1,568 test bodies; falsified by a CRAN check-time NOTE that survives this fix.
