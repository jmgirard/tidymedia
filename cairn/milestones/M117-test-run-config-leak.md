# M117: A test run leaves no location behind in the user's real config directories

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — the shipped test suite's filesystem behaviour is what CRAN's machines run
- **Branch/PR:** —

## Goal

A run of the test suite writes no remembered location into either of the two real
user config directories.

## Scope

**In:** find and fix the site(s) that escape the config redirect; add a
before/after harness with a positive control in both forms it must cover.

**Out:** removing the two files already on the maintainer's machine → the
maintainer's own call, reported not done. The `set_program()` consent gate's
design → unchanged (D080). A config path that is a directory, or a remembered
location that exists but is not executable → the two existing candidate rows.
`tm_install_dir()` (`R/program_management.R:33`) → outside the promise; this
milestone is scoped to the two config directories.

## Acceptance criteria

- [ ] AC1: On a machine where both `tools::R_user_dir("tidymedia", "config")` and
      `rappdirs::user_config_dir("tidymedia", "R")` are absent or empty before the
      run, `devtools::test()` leaves both absent or empty afterwards.
- [ ] AC2: The same holds for `R CMD check` on the same machine.
- [ ] AC3: The before/after comparison is shown able to fail in both forms it must
      cover: a planted write into each of the two directories from a test body makes
      AC1's comparison report a difference, and a planted write from outside any test
      body — during the check's build or install phase, which is where AC2's leaks
      would come from — makes AC2's comparison report a difference.
- [ ] AC4: `devtools::check()` reports 0 errors and 0 warnings, and the `verify` slot
      of `cairn/PROFILE.md` is clean.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T1, T2, T3
- AC3 → T3, T4
- AC4 → T5

## Tasks

- [ ] T1: Reproduce from a clean state. With both directories emptied (back them up
      first), run `devtools::test()` and then `R CMD check`, recording which run
      creates which file and each run's skip count. The evidence in hand:
      `mediainfo_location.txt` in the legacy `rappdirs` directory holding
      `/private/tmp/claude-501/.../25b4c341-.../scratchpad/blockbin.sh` (2026-08-26)
      and `ffmpeg_location.txt` under `R_user_dir` (2026-09-01).
- [ ] T2: Name the escaping site(s) by file:line and fix them. The redirect helper is
      `tests/testthat/helper-program-config.R:41-60`; `R_USER_CONFIG_DIR` redirects
      only the `tools::` half while `rappdirs::user_config_dir` is mocked (`:41-49`).
      The un-redirected `set_program()` calls found so far are at
      `tests/testthat/test-blame-frame-table.R:21-39` and `:115-119`.
- [ ] T3: Build the before/after harness over both directories.
- [ ] T4: Add both plant forms, confirm each comparison reports the difference, and
      revert every plant.
- [ ] T5: `devtools::test()` and `devtools::check()` clean.

## Work log

- 2026-09-07: created by /milestone-plan.
- 2026-09-07: plan-gate criteria audit ran in FULL mode (declared tier user-facing), over two rounds with a fresh-context [O] reader that authored none of the criteria. Round 1 returned 11 findings across four milestones, round 2 returned 12 across six. Findings against this milestone: AC3's single plant form stood in for a family free in form as well as location and reached only AC1's comparison (repaired — AC3 now names both forms and binds the second to AC2); AC1/AC2's "the run's skip count is recorded" second sentences were unfalsifiable recording acts (repaired — moved to T1). AC4 noted as a restatement of PROFILE's standing gate; kept.
- 2026-09-07: plan gate chose fixing the escaping test site over widening the redirect helper to cover every write, because the helper already redirects both directories and the defect is a site that bypasses it, not a gap in it; falsified by a second leak from a site that does use the helper.
