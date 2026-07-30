# M46: Stop the subtitle fixture hanging, and bound every fixture command

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m46-fixture-hang-timeout`

## Goal

Every FFmpeg fixture command in the test suite terminates: the subtitle fixture
stops deadlocking, and any fixture command that does hang fails fast instead of
stalling the whole run.

## Scope

**In:** the subtitle-bearing fixture command (today inline at
`tests/testthat/test-audio-stream.R:308-314`, from M43 `ccfaf64`); a test-only
FFmpeg runner in `tests/testthat/helper-media.R` carrying a wall-clock timeout,
and the routing of every fixture-generation call site through it; committed
tests for the timeout mechanism and for the fixture; direct coverage for the
exported `ffmpeg()`, which those call sites are currently its only exercise of.

**Out:** a `timeout=` on the package's own runtime path (`ffmpeg()`,
`run_program()`, `ffm_run()`) — an exported-API change needing its own D-entry
and default-value decision → ROADMAP candidate row added by this plan.
Reporting the `-shortest`-plus-subtitle deadlock upstream to FFmpeg → not this
repo's work; recorded at the fixture comment only. Every other open candidate
row, `audio_stream`-carry included.

## Acceptance criteria

- [ ] AC1 `helper-media.R` carries a `make_subtitle_video()` generator holding
      the subtitle-bearing fixture command; that command passes no `-shortest`,
      and a comment there records both that the lavfi sources are already
      bounded by their `duration=` options and that FFmpeg deadlocks
      intermittently when `-shortest` accompanies a mapped subtitle stream. The
      subtitle test in `test-audio-stream.R` calls the generator rather than
      building the command inline.
- [ ] AC2 Review records two 25-run probes of the subtitle-fixture command,
      each run bounded by a 20-second wall-clock limit: post-fix, 25 of 25
      complete; pre-fix (command recovered from git), at least one run reaches
      the limit. Every post-fix output's `ffprobe -v error -show_entries
      stream=codec_type -of csv=p=0` reads exactly `video`, `audio`, `subtitle`,
      in that order.
- [ ] AC3 Each of the twelve FFmpeg fixture-generation call sites under
      `tests/testthat/` — `helper-media.R:27,46,63,82,99,127,158,164`,
      `test-audio-stream.R:275,308`, `test-ffmpeg.R:287,303` — runs its command
      through the new timeout-bearing helper, and after the change the only
      calls to `ffmpeg()` under `tests/testthat/` are AC4's.
- [ ] AC4 The exported `ffmpeg()` gains the direct coverage it has only
      incidentally today: a committed test calls `ffmpeg("-version")` and
      asserts FFmpeg's version banner in the returned vector, skipping when the
      binary is absent, and a second fires its `rlang::check_string()` branch on
      a non-string `command`.
- [ ] AC5 A committed test proves the timeout fires: a command that would
      otherwise run well past the limit runs through the helper with a 3-second
      limit, and the call fails within 8 seconds with a message naming the
      binary and the limit in seconds — and naming neither the command string
      nor any temp path. It skips when the ffmpeg binary is absent.
- [ ] AC6 A committed regression test generates the subtitle fixture 10
      consecutive times through the helper and requires every generation to
      complete within the limit; the subtitle-presence check stays a
      fixture-validity skip, never the discriminating assertion. It skips when
      ffmpeg or ffprobe is absent. Re-adding `-shortest` to the generator makes
      it fail within at most three suite runs, recorded in Review.
- [ ] AC7 `devtools::test()` passes with 0 failures and `devtools::check()`
      reports 0 errors and 0 warnings, any NOTE justified in Review.

## Coverage

- AC1 → T3
- AC2 → T3, T7
- AC3 → T1, T2
- AC4 → T4
- AC5 → T1, T5
- AC6 → T3, T6
- AC7 → T7

## Tasks

- [x] T1 Add the timeout-bearing runner to `tests/testthat/helper-media.R`:
      resolve with `find_ffmpeg()`, run `system(..., intern = TRUE, input = "",
      timeout = )` (default 120 s, overridable), muffle its warning, and on a
      `status` attribute of 124 raise an error naming the binary and the
      limit only. Confirm no orphan `ffmpeg` survives the kill.
- [x] T2 Route the twelve fixture call sites in AC3 through it; leave every
      other `ffmpeg`-stemmed call (`find_ffmpeg`, `skip_if_no_ffmpeg`,
      `ffmpeg_codecs`, `ffmpeg_encoders`) untouched.
- [x] T3 Move the subtitle fixture command into `make_subtitle_video()` in
      `helper-media.R`, drop `-shortest`, record why at the generator, and
      repoint `test-audio-stream.R:298-327` at it.
- [x] T4 Add the direct `ffmpeg()` tests to `test-ffmpeg.R` (AC4).
- [x] T5 Add the timeout-mechanism test in a new
      `tests/testthat/test-fixture-helpers.R` (AC5).
- [x] T6 Add the 10-run regression test beside the subtitle test, then probe it
      red by re-adding `-shortest` to the generator — commit the baseline first,
      since `git checkout` restores from the index and would otherwise revert
      the fix itself (M44) — and restore.
- [ ] T7 Run the 25-run before/after probe (AC2); `devtools::test()` and
      `devtools::check()` (AC7); confirm CI green on both platforms, ubuntu's
      ffmpeg 6.1.1 included (M45). No `NEWS.md` entry — test-only, nothing
      user-visible.

## Work log

- 2026-07-30: created by /milestone-plan; promotes the 2026-07-30 ROADMAP candidate (second observation of the hang, found during M45 review; fixture from M43 `ccfaf64`).
- 2026-07-30: measured on ffmpeg 8.1.2/macOS — the committed fixture command hung 10/25 under a 20 s limit; with `-shortest` removed, 0/15; with `-shortest` kept and the subtitle map dropped, 0/15. The deadlock needs `-shortest` AND a mapped subtitle stream. Post-fix output still probes video+audio+subtitle at 2.023 s.
- 2026-07-30: verified `system(cmd, intern = TRUE, input = "", timeout = N)` returns at N seconds with a warning and `status` 124 (R 4.6.1, macOS) — base R suffices, no dependency gate.
- 2026-07-30: plan gate chose dropping `-shortest` over a two-pass subtitle remux because the lavfi sources are already `duration=`-bounded so the flag is redundant and its removal is one line; falsified by a post-fix output whose duration or stream set differs from the pre-fix one, or by a hang recurring without `-shortest`.
- 2026-07-30: plan gate chose a test-only timeout helper over a `timeout=` argument on `ffmpeg()`/`run_program()` because the exported-API change needs its own D-entry and default-value decision; falsified by a user report of a package call hanging (a candidate row carries it).
- 2026-07-30: plan gate chose committing the 10-run regression test over review-only probe evidence, against the "never test dependency behavior" reading the [O] criteria audit surfaced, because the subject under test is this repo's fixture recipe rather than FFmpeg's behavior; falsified by the test going red for any cause other than a reintroduced `-shortest`.
- 2026-07-30: [O] criteria audit ran on the step-2 criteria and returned six findings — a false premise under AC3 (no test exercises `ffmpeg()` as its subject, so routing all twelve sites would strip the exported function of all coverage; AC4 added), the AC1/AC5 fixture-location split, strict-vs-diagnostic and ordered-vs-set ambiguities in AC2, an under-specified timeout message, a missing binary-absent skip, and a decorative assertion in the repeat test (the guard is completion-within-limit, not subtitle presence). Five fixed in the wording; the sixth went to the gate as Q2.

- 2026-07-30: T1 done — `run_ffmpeg_fixture(command, timeout = 120)` in `helper-media.R`; it errors rather than skipping (a skip would go green on CI, which is the failure this milestone closes) and names only the binary and the limit. Probed with a 3-second limit against an unbounded encode: returned at 3.0 s with "ffmpeg fixture generation timed out after 3 seconds.", and `pgrep ffmpeg` found no survivor, so R's kill reaps the child.
- 2026-07-30: T6 done — mutation probe against the committed baseline: `-shortest` re-added to `make_subtitle_video()`, `test-audio-stream.R` run three times, ALL THREE red (AC6 allows up to three, needing one). Runs 1 and 2 failed in the 10-run regression test at `:325`, run 3 in the original subtitle test at `:305`; every failure read `Error: ffmpeg fixture generation timed out after 120 seconds.` — an error, never a hang, which is the whole point. Restored with `git checkout` (clean vs HEAD) and the file re-ran green.
- 2026-07-30: T5 done — `test-fixture-helpers.R`: a 1080p60 unbounded encode under a 3-second limit errors in under 8 s naming `ffmpeg` and "timed out after 3 seconds" while naming neither the command nor `tempdir()`, plus a finishing command returning FFmpeg's output. Recorded in the file what the test cannot catch: a mutation that stops passing the limit through makes it HANG rather than go red, since non-termination is the failure under test. Full suite FAIL 0, PASS 2837.
- 2026-07-30: T4 done — two tests in `test-ffmpeg.R`: `ffmpeg("-version")` returns a character vector whose first line matches `^ffmpeg version`, and the `check_string()` branch fires on a length-2 vector, a number, and `NULL`. That branch had never been fired. Full suite FAIL 0, PASS 2829.
- 2026-07-30: minor amendment — T1's text said `testthat::fail()`; the helper raises an error instead. `fail()` records a failure and returns, so the generator would run on into its `skip_if_not(file.exists(path))` and report the test as failed AND skipped; an error stops at the hang and is what `expect_error()` can pin in T5. Behavior is the one the criteria name (loudly red, never a skip).
- 2026-07-30: minor amendment — T3 moved ahead of T2. Verification between tasks runs the full suite, and until `-shortest` is gone each run carries the measured ~40% hang; fixing the fixture first makes every later run deterministic. Task text unchanged.
- 2026-07-30: T3 done — `make_subtitle_video()` in `helper-media.R` holds the command, `-shortest` is gone, and the measured hang rates are recorded at the generator. Added `stream_types()` beside it since two tests now probe codec types (the inline `types()` closure in `test-audio-stream.R` is retired). T6's 10-run test is committed in the same change so its mutation probe runs against a committed baseline (M44). `test-audio-stream.R` 24 tests pass, the new one 10/10.
- 2026-07-30: T2 done — the eleven remaining fixture sites (T3 absorbed the twelfth into `make_subtitle_video()`) now call `run_ffmpeg_fixture()`: `helper-media.R` ×8, `test-audio-stream.R` ×1, `test-ffmpeg.R` ×2. No direct `ffmpeg()` call is left under `tests/testthat/` — which is exactly the gap AC4 exists to close. Full suite FAIL 0, PASS 2824, 60 s.
- 2026-07-30: baseline suite on the branch before any test change — FAIL 0, WARN 4, SKIP 5, PASS 2814, 57 s (this run did not hit the hang).

## Decisions

## Review
