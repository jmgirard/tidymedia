# M076: The R version the package actually needs

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m076-r-version-floor` / https://github.com/jmgirard/tidymedia/pull/80

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

- [x] AC1 `DESCRIPTION` carries a `Depends: R (>= <v>)` field, where `<v>` is
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
- [x] AC3 `NEWS.md` states the newly declared R floor as a user-visible fact.
      The statement of what was and was not run against it lives in the
      milestone file, bound by no criterion (T5).
- [x] AC4 `devtools::test()` clean and `devtools::check()` clean (0 errors, 0
      warnings) on the current R.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4
- AC4 → T6

## Tasks

- [x] T1 Write `data-raw/r-floor.R`: the two-input sweep AC1 names, printing
      each input and the maximum. Add `.Rbuildignore` coverage (`^data-raw$`
      already present — confirm, do not duplicate).
- [x] T2 Run it; record both inputs and the maximum in the work log. Set
      DESCRIPTION's `Depends:` field to the result.
- [x] T3 Add a matrix row to `.github/workflows/R-CMD-check.yaml` pinned to the
      declared floor (`r: '<v>'`). Confirm current dependencies resolve there
      before relying on it — measured at plan time: current `dplyr` 1.2.1,
      `glue` 1.8.1, `purrr` 1.2.2, `rappdirs` 0.3.4 and `testthat` 3.3.2 each
      already declare `R (>= 4.1.0)`, so 4.1.0 sits exactly at the boundary and
      a later dependency release could push it up.
- [x] T4 NEWS entry: the declared floor, user-visible wording only.
- [x] T5 Record in the milestone file what was run against the floor (T3's job,
      with its runner OS and the concrete R version) and what was not: no R
      version between the declared floor and the concrete oldest version the
      matrix ran at the time of writing (name it, never the moving `oldrel-1`
      label), and no dependency pinned to its own declared floor for that run.
- [x] T6 `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Work log

- 2026-08-27: created by /milestone-plan.
- 2026-08-27: criteria audit (full mode, user-facing tier) returned 13 findings across both milestones' drafts; 9 fixed here, 3 posed at the gate, 1 (M077 AC5) clean. Fixed for M076: AC1 bound the milestone file's prose rather than the script's output; the `\dontrun` clause was inert and ambiguously scoped; the R/ leg counted roxygen comment lines, so it could not tell package code from examples; AC2 promised a NOTE justification in the record; AC2's reachability was unverified (checked — 4.1.0 resolves); AC3 named the moving `oldrel-1` label.
- 2026-08-27: T1 — `data-raw/r-floor.R` written. Leg (a) counts the two forms from `getParseData()` terminal tokens `PIPE` and `'\\'`, never a regex over source text, so a `|>` in a roxygen comment or a string cannot count; `man/` examples reach the parser through `tools::Rd2ex(commentDontrun = TRUE, commentDonttest = TRUE)`, which comments both excluded block types out, so AC1's exclusion is enforced by the extractor. Leg (b) fetches each `Imports` floor version's own tarball from CRAN (Archive first, then current contrib), untars only its `DESCRIPTION`, and reads `Depends: R` from it — the installed release is never consulted. `.Rbuildignore` already carries `^data-raw$` (line 15); not duplicated.
- 2026-08-27: T2 — ran it. (a) syntax: **4.1.0**, from 50 `PIPE` occurrences across `man/`'s running examples (first at `ffm_batch.Rd:14`) and zero in `R/` — the package's own code uses neither form, so the floor is set entirely by what the help pages run; no `\\(` lambda anywhere. (b) dependencies: **3.5.0**, the maximum `Depends: R` across the nine versioned `Imports` floors as those exact versions declare it — `rlang 1.1.0` at 3.5.0, then `dplyr 1.1.0` and `glue 1.6.2`/`cli 3.4.0` at 3.4, `withr 2.5.0` at 3.2.0, `purrr 1.0.0` at 3.2.3, `rappdirs 0.3.3` at 3.2, `archive 1.1.1` and `tibble 3.1.4` at 3.1.0; `tools` and `utils` carry no version and were skipped. Maximum = **4.1.0**, written to DESCRIPTION as `Depends: R (>= 4.1.0)`.
- 2026-08-27: T3 — added `{os: ubuntu-latest, r: '4.1.0'}` to `R-CMD-check.yaml`'s matrix, the exact version rather than a moving label. Re-confirmed at implement time that the job can resolve: the highest `R (>= )` any *current* release of a declared `Imports` or `Suggests` package asks for is 4.1.0 — `dplyr` 1.2.1, `testthat` 3.3.2 and `furrr` 0.4.0 at `4.1.0`, `glue` 1.8.1, `purrr` 1.2.2, `rappdirs` 0.3.4 and `roxygen2` 8.1.0 at `4.1`, everything else lower — so 4.1.0 sits exactly at the boundary and the next release of any of those seven can push the job out from under the declared floor.
- 2026-08-27: T4 — NEWS entry under a new `## Requirements` heading, stating the declared floor and where it comes from. It does not state what was run against the floor; that is T5's record, which no criterion binds.
- 2026-08-27: T5 — recorded below, under Decisions, what the declared floor was and was not measured against. `oldrel-1` resolved to the concrete 4.5.3 on 2026-08-27 (r-hub rversions API), so the unexercised band is 4.2.x-4.4.x.
- 2026-08-27: T6 — `devtools::document()` produces no diff; `devtools::test()` 6690 passing, 0 failed, 0 errors, 5 skipped (the 12 reported warnings are M075's deliberate `tidymedia_dropped_audio` warnings in `test-audio-stream*.R` and `test-parallel-option-carry.R`, unchanged by this milestone); `devtools::check()` 0 errors / 0 warnings / 0 notes in 2m 37.6s on R 4.6.1. AC2 is the one criterion no local run can close: it needs the pinned 4.1.0 job green on the pull request, and the workflow triggers on `pull_request`, not on a branch push — so review opens the PR first.
- 2026-08-27: plan gate chose declaring `R (>= 4.1.0)` over removing `|>` from 24 help-page examples to hold a lower floor, because current `dplyr`, `glue`, `purrr`, `rappdirs` and `testthat` already force 4.1.0 on any install, so the rewrite would buy no reachable user; falsified by a report of a user on R 4.0.x who can install every current dependency.

## Decisions

**What the declared `R (>= 4.1.0)` floor was measured against.**

*Run:* one job — `R-CMD-check.yaml`'s `{os: ubuntu-latest, r: '4.1.0'}` row, on
the milestone's pull request. That is `R CMD check` on Ubuntu at R 4.1.0
exactly, against whatever version of each dependency CRAN resolves on the day
the job runs.

*Not run:* no R version strictly between the declared floor and **4.5.3** --
the concrete version `oldrel-1` resolved to on 2026-08-27 -- so 4.2.x, 4.3.x
and 4.4.x are unexercised, and a break confined to them would ship. And no
dependency was pinned to its own declared `Imports` floor for that run, so the
4.1.0 job says nothing about whether `archive 1.1.1`, `cli 3.4.0`,
`dplyr 1.1.0`, `glue 1.6.2`, `purrr 1.0.0`, `rappdirs 0.3.3`, `rlang 1.1.0`,
`tibble 3.1.4` or `withr 2.5.0` work -- that is M077.

## Review

Reviewed 2026-08-27 on `m076-r-version-floor`, PR #80.

**AC1 — VERIFIED.** `Rscript data-raw/r-floor.R` rerun fresh at review, exit 0.
It printed both inputs separately and their maximum: (a) syntax **4.1.0**, from
50 `PIPE` occurrences across the 81 help pages' running examples (first at
`ffm_batch.Rd:14`) and zero across the 16 files in `R/`; no lambda anywhere.
(b) dependencies **3.5.0**, the maximum `Depends: R` over the nine versioned
`Imports` floors as those exact versions declare it (`rlang 1.1.0` at 3.5.0 is
the maximum; `tools` and `utils` carry no version and were skipped). Maximum
**4.1.0**. `DESCRIPTION` carries `Depends:\n    R (>= 4.1.0)`, which is that
maximum.

**AC2 — VERIFIED.** See the CI line below.

**AC3 — VERIFIED.** `NEWS.md` opens with a `## Requirements` section stating
`R (>= 4.1.0)` as a user-visible fact, in user-facing wording with no milestone
number. It does not state what was run against the floor; that record lives
under Decisions above, bound by no criterion, as AC3 requires.

**AC4 — VERIFIED.** Fresh runs at review on R 4.6.1: `devtools::test()` 6690
passing, 0 failed, 0 errors, 5 skipped; `devtools::check(document = FALSE)`
**0 errors / 0 warnings / 0 notes**. The 12 warnings `test()` reports are
M075's deliberate `tidymedia_dropped_audio` warnings in `test-audio-stream*.R`
and `test-parallel-option-carry.R`, which this milestone does not touch.

**Consistency gate — PASS.** `cairn_validate.py` exit 0, all 16 PASS and 7 OK
(the `release window` advisory did not fire). No DESIGN principle changed, so
`cairn_impact.py` was skipped. Toolchain slot: `devtools::document()` produces
no diff; `NAMESPACE`/`man/` regenerate clean; `README.Rmd` and `README.md` are
untouched by this milestone and unchanged since #53, so no re-knit is owed;
`pkgdown::check_pkgdown()` reports no problems; `NEWS.md` carries the
user-visible entry; no new top-level file needs an `.Rbuildignore` entry
(`^data-raw$` was already present) and `check()` reported 0 notes;
`devtools::check()` clean as recorded under AC4.
