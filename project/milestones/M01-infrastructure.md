# M01: Infrastructure modernization

- **Status:** review <!-- mirror of ROADMAP.md; ROADMAP wins on conflict -->
- **Created:** 2026-07-10
- **Completed:** —

## Goal

Bring the package up to current R development standards so every later
milestone has automated gates: a real testthat suite, GitHub Actions CI, and
cli-based error reporting. No new features; behavior-preserving except where
noted.

## Scope

**In:** testthat 3e setup; unit tests for all pure command-building code;
GHA workflows (R-CMD-check, test-coverage); migrate assertthat → rlang/cli
across all R/ files; DESCRIPTION cleanup (add cli, drop assertthat);
NEWS.md entry.

**Out:** fixing builder bugs found while writing tests (e.g., `ffm_trim()`
ignores its `setpts` argument; `ffm_compile()` emits invalid
`-filter_complex:v`). Write characterization tests, log the bugs in M02's
idea notes, fix them in M02. Out: any API changes.

## Acceptance criteria

Each must be verified with evidence at review.

- [ ] `devtools::check()` passes locally with 0 errors, 0 warnings.
- [ ] R-CMD-check GHA green on ubuntu, macOS, and windows.
- [ ] testthat 3e active; every exported command-building function has
      tests; execution tests `skip_if` ffmpeg/mediainfo are absent.
- [ ] Coverage workflow runs; baseline percentage recorded in this file.
- [ ] Zero remaining `assertthat` calls; all user-facing errors use
      `cli::cli_abort()` with informative, classed messages.
- [ ] NEWS.md documents the changes.

## Plan

Tasks sized to one working session or less.

- [x] T1: `usethis::use_testthat(3)`; write pure tests for `ffm_*` builders
      and `ffm_compile()` output strings (no binaries needed).
- [x] T2: Tests for `get_codecs()`, `get_encoders()`, and task functions,
      gated on binary availability via a `skip_if_no_ffmpeg()` helper.
- [x] T3: `usethis::use_github_action("check-standard")` and
      `use_github_action("test-coverage")`; install ffmpeg in the workflow
      (ubuntu) so execution tests run on at least one platform.
- [x] T4: Migrate `R/ffm.R`, `R/ffm_oop.R`, `R/ffmpeg.R` off assertthat to
      rlang/cli checks.
- [x] T5: Migrate remaining files (`mediainfo.R`, `ffprobe.R`,
      `program_management.R`, `utils.R`); drop assertthat from Imports;
      update DESCRIPTION and NEWS.md.

## Work log

Append-only; newest last. One line per session: date, what happened, next.

- 2026-07-10: Milestone planned.
- 2026-07-10: Started implementation; branched milestone/M01-infrastructure.
- 2026-07-10: T1 done — testthat 3e set up; pure builder/compile tests
  (test-ffm.R, test-utils.R, helper-skip.R) incl. characterization of known
  bugs. 36 tests pass. Next: T2 binary-gated tests.
- 2026-07-10: T2 done — binary-gated tests (ffmpeg task verbs, get_codecs/
  get_encoders, mediainfo, ffprobe) + skip/media helpers. Namespace-qualified
  bare dplyr/tidyr calls in ffprobe.R (deps declared in T5). 59 pass, 5 skip
  (mediainfo absent locally). Next: T3 GHA workflows.
- 2026-07-10: T3 done — R-CMD-check.yaml (5 configs) + test-coverage.yaml;
  both install ffmpeg+mediainfo on Linux so execution tests run there. YAML
  valid. Green-CI verified at review (needs push). Next: T4 assertthat->cli.
- 2026-07-10: T4 done — ffm.R/ffm_oop.R/ffmpeg.R off assertthat. Added
  internal check_ffm()/check_dim()/check_file_exists() helpers; rlang
  check_string/check_bool/check_number_whole + cli_abort for custom cases;
  match.arg->arg_match. Removed dead commented ffm_trim. Note: rlang doesn't
  export check_character/check_logical (used is_character()+cli_abort).
  new_ffm() keeps stopifnot (internal invariants, not user-facing). 63 pass.
- 2026-07-10: T5 done — migrated mediainfo/ffprobe/program_management/utils
  off assertthat; warnings->cli_warn; dropped assertthat, added
  cli/dplyr/tidyr/purrr to Imports (+withr Suggests). Also cleaned every
  R CMD check warning/NOTE (documented 7 undocumented exports + ffm_copy
  args, removed dup ffm alias, utils::read.csv, .data/all_of in ffprobe,
  get_volume out->output typo). devtools::check() = 0 errors / 0 warnings /
  0 notes. 64 tests pass, 5 skip (mediainfo). README badges added.
- 2026-07-10: Coverage NOT measurable locally — covr 3.6.5 on R 4.6.1 is
  broken here (function_coverage yields NaN; env issue, not the package).
  Baseline % to be captured from the first CI test-coverage run at review.
- 2026-07-10: All tasks complete; check clean. Status -> review (recap given).

## Decisions

Milestone-local decisions; promote cross-cutting ones to ../DECISIONS.md.

- 2026-07-10: Error idiom = rlang standalone `check_*` helpers for standard
  type checks + `cli::cli_abort()` for custom conditions (mutually-exclusive
  args, file existence). Matches CLAUDE.md.
- 2026-07-10: Target a NOTE-free `devtools::check()`. Add dplyr/tidyr/purrr to
  Imports and namespace-qualify the bare calls in ffprobe.R/mediainfo.R.
  Deeper probe_* logic bugs stay deferred to M04.
- 2026-07-10: CI installs both ffmpeg and mediainfo on the ubuntu runner so
  binary-gated tests execute rather than skip.
- 2026-07-10: `new_ffm()` (internal constructor) keeps `stopifnot` for its
  internal-invariant checks; the "user-facing errors use cli" criterion
  targets exported/user-reachable validation, which is fully migrated.

## Review

Filled in by `/milestone review M01`.

- Criteria verification:
- check()/test()/coverage results:
- Follow-ups spawned:
