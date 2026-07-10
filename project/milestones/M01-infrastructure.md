# M01: Infrastructure modernization

- **Status:** planned <!-- mirror of ROADMAP.md; ROADMAP wins on conflict -->
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

- [ ] T1: `usethis::use_testthat(3)`; write pure tests for `ffm_*` builders
      and `ffm_compile()` output strings (no binaries needed).
- [ ] T2: Tests for `get_codecs()`, `get_encoders()`, and task functions,
      gated on binary availability via a `skip_if_no_ffmpeg()` helper.
- [ ] T3: `usethis::use_github_action("check-standard")` and
      `use_github_action("test-coverage")`; install ffmpeg in the workflow
      (ubuntu) so execution tests run on at least one platform.
- [ ] T4: Migrate `R/ffm.R`, `R/ffm_oop.R`, `R/ffmpeg.R` off assertthat to
      rlang/cli checks.
- [ ] T5: Migrate remaining files (`mediainfo.R`, `ffprobe.R`,
      `program_management.R`, `utils.R`); drop assertthat from Imports;
      update DESCRIPTION and NEWS.md.

## Work log

Append-only; newest last. One line per session: date, what happened, next.

- 2026-07-10: Milestone planned.

## Decisions

Milestone-local decisions; promote cross-cutting ones to ../DECISIONS.md.

- (none yet)

## Review

Filled in by `/milestone review M01`.

- Criteria verification:
- check()/test()/coverage results:
- Follow-ups spawned:
