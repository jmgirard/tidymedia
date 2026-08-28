# M076: The R version the package actually needs

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

DESCRIPTION declares an R version floor derived from a measurement of the
package's own code, its shipped help-page examples, and its declared dependency
floors, rather than from silence.

## Scope

Surface tier: **user-facing** — `Depends: R (>= )` is what a user's installer
resolves against, so its absence is a contract the package does not state.

**In:** `data-raw/r-floor.R`, a developer-only sweep that finds the
R-version-gated syntax forms in the shipped surface and reads the `Depends: R`
field of each declared `Imports` floor version; a `Depends: R (>= )` field in
DESCRIPTION set to the maximum; an `R-CMD-check.yaml` job pinned to that
version; a NEWS entry naming the floor.

**Out:** the nine other `Imports` package floors and whether each is exercised
against the version it names → M077. Hardening `data-raw/withr-floor.R`'s four
inherited rough edges → stays on the `Imports`-floors candidate row. Removing
`|>` from the examples to hold a lower floor → rejected, see the work log.

## Acceptance criteria

- [ ] AC1 `DESCRIPTION` carries a `Depends: R (>= <v>)` field, where `<v>` is
      the maximum `data-raw/r-floor.R` prints when rerun at review over two
      inputs: (a) `4.1.0` for each occurrence of the native pipe `|>` or the
      backslash lambda `\(` in parsed *code* — `getParseData()` over each file
      `list.files("R", "[.]R$")` enumerates, so roxygen comment lines do not
      count, and over each `\examples` section of each file
      `list.files("man", "[.]Rd$")` enumerates, excluding occurrences inside a
      `\dontrun{}` or `\donttest{}` block; and (b) the `Depends: R` field read
      from the DESCRIPTION of each package version named by
      `read.dcf(DESCRIPTION, "Imports")`. The script prints both inputs
      separately and their maximum.
- [ ] AC2 `R CMD check` on the package runs with 0 errors and 0 warnings at
      exactly the R version AC1 declares, evidenced by a green
      `R-CMD-check.yaml` job pinned to that version on the milestone's pull
      request (the workflow triggers on `pull_request`, not on a branch push).
- [ ] AC3 `NEWS.md` states the newly declared R floor as a user-visible fact.
      The statement of what was and was not run against it lives in the
      milestone file, bound by no criterion (T5).
- [ ] AC4 `devtools::test()` clean and `devtools::check()` clean (0 errors, 0
      warnings) on the current R.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4
- AC4 → T6

## Tasks

- [ ] T1 Write `data-raw/r-floor.R`: the two-input sweep AC1 names, printing
      each input and the maximum. Add `.Rbuildignore` coverage (`^data-raw$`
      already present — confirm, do not duplicate).
- [ ] T2 Run it; record both inputs and the maximum in the work log. Set
      DESCRIPTION's `Depends:` field to the result.
- [ ] T3 Add a matrix row to `.github/workflows/R-CMD-check.yaml` pinned to the
      declared floor (`r: '<v>'`). Confirm current dependencies resolve there
      before relying on it — measured at plan time: current `dplyr` 1.2.1,
      `glue` 1.8.1, `purrr` 1.2.2, `rappdirs` 0.3.4 and `testthat` 3.3.2 each
      already declare `R (>= 4.1.0)`, so 4.1.0 sits exactly at the boundary and
      a later dependency release could push it up.
- [ ] T4 NEWS entry: the declared floor, user-visible wording only.
- [ ] T5 Record in the milestone file what was run against the floor (T3's job,
      with its runner OS and the concrete R version) and what was not: no R
      version between the declared floor and the concrete oldest version the
      matrix ran at the time of writing (name it, never the moving `oldrel-1`
      label), and no dependency pinned to its own declared floor for that run.
- [ ] T6 `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Work log

- 2026-08-27: created by /milestone-plan.
- 2026-08-27: criteria audit (full mode, user-facing tier) returned 13 findings across both milestones' drafts; 9 fixed here, 3 posed at the gate, 1 (M077 AC5) clean. Fixed for M076: AC1 bound the milestone file's prose rather than the script's output; the `\dontrun` clause was inert and ambiguously scoped; the R/ leg counted roxygen comment lines, so it could not tell package code from examples; AC2 promised a NOTE justification in the record; AC2's reachability was unverified (checked — 4.1.0 resolves); AC3 named the moving `oldrel-1` label.
- 2026-08-27: plan gate chose declaring `R (>= 4.1.0)` over removing `|>` from 24 help-page examples to hold a lower floor, because current `dplyr`, `glue`, `purrr`, `rappdirs` and `testthat` already force 4.1.0 on any install, so the rewrite would buy no reachable user; falsified by a report of a user on R 4.0.x who can install every current dependency.

## Decisions

## Review
