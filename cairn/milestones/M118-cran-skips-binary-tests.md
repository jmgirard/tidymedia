# M118: The binary-executing tests skip on CRAN's own check

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — what runs on CRAN's machines is the shipped tarball's behaviour
- **Branch/PR:** `m118-cran-skips-binary-tests`

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

- [x] T1: Measure, do not trust, the precedent the repo records at
      `tests/testthat/test-runtime-timeout.R:188-190` — that `devtools::check()` and
      the CI workflow both set `NOT_CRAN`, so CI keeps running these tests.
      `.github/workflows/R-CMD-check.yaml` sets no `NOT_CRAN` of its own, so the value
      comes from `r-lib/actions/check-r-package@v2`. If it does not, this milestone
      would silently gut CI coverage and stops here for a re-gate.
- [x] T2: Add `skip_on_cran()` to the five helpers in `helper-skip.R`.
- [x] T3: Build the PATH-shim spawn counter and run the suite in both modes.
- [x] T4: Run the two escape-route conditions of AC3.
- [ ] T5: Record the base-commit skipped-test set, then run `devtools::test()` in
      developer mode and `R CMD check --as-cran` in CRAN mode; record each run's
      `Duration` and tests-step timing against the base commit's 7m47s and
      `[368s/436s]` (measured 2026-09-07).

## Work log

- 2026-09-07: created by /milestone-plan.
- 2026-09-07: plan-gate criteria audit ran in FULL mode (declared tier user-facing), two rounds, fresh-context [O] reader. Findings against this milestone: AC1's "skips on that account alone for no other reason" was self-contradictory, since the helpers must keep skipping for an absent binary (repaired); AC2's "every spawn the run actually makes" was unbounded, the PATH shim seeing only bare-name resolution while a remembered absolute location and `helper-program-config.R:43`'s emptied `PATH` both escape it (repaired — AC2 narrowed to `PATH`-resolved spawns, AC3 added for the two escape routes, and a control run added so an empty log is not read as success); AC4's equal skip counts passed a swap (repaired — compares the set of skipped test names); AC3's timing sentence was an unfalsifiable recording act (repaired — moved to T5).
- 2026-09-07: plan gate chose `skip_on_cran()` in the three helpers over profiling and cutting the pure-R suite, because the measurement showed binary execution is about one minute of the six — `R CMD check` 7m47s with the binaries against a 5m06s binary-absent suite run (2026-09-07) — so the larger cut buys little against real risk to 1,568 test bodies; falsified by a CRAN check-time NOTE that survives this fix.
- 2026-09-08: T1 measured. The precedent holds but names the wrong source. `devtools::check()` sets it (installed `devtools::check` carries `env_vars = c(NOT_CRAN = "true")`); `r-lib/actions/check-r-package@v2` does not — it calls `rcmdcheck::rcmdcheck()` with no `env`, and `rcmdcheck` 1.4.0's `env` default is `character()`. `setup-r@v2` is what sets it: run 34275894026's predecessor 34261398144 dumps `NOT_CRAN: true` in the job env from the `setup-r-dependencies` step onward, and no test in that run skipped for an "On CRAN" reason though three `skip_on_cran()` sites were in the suite. CI coverage survives this milestone; no re-gate needed.
- 2026-09-08: amendment (substantive, Scope In) at the question gate: scope widened from three helpers to five. `skip_if_no_nvenc()` and `skip_if_no_videotoolbox()` each spawn a one-frame FFmpeg encode to decide, and check `Sys.which("ffmpeg")` inline rather than calling `skip_if_no_ffmpeg()`, so six tests (`test-nvenc.R:435,446,458`, `test-video-codec.R:480,489`, `test-hardware-backends.R:315`) would keep spawning under AC2. No acceptance criterion changed.
- 2026-09-08: question gate chose a committed `tools/cran_spawn_check.R` for the AC2/AC3 shim over a throwaway harness, matching the two measurement scripts already in `tools/`; costs one `.Rbuildignore` entry.
- 2026-09-08: T2 done. `skip_on_cran()` added first in all five helpers, ahead of the binary question, so the reason reported on CRAN is "On CRAN" whether or not the machine has the binary. `tests/testthat/test-cran-skip-helpers.R` asserts which skip fires, never a bare one; proven able to fail by two planted defects — dropping the call from `skip_if_no_ffprobe()` (red on the missing skip and on the wrong reason) and moving it below the binary check in `skip_if_no_mediainfo()` (red on the ordering test alone). `devtools::test()` clean: FAIL 0, WARN 10, SKIP 18, PASS 13188.

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
