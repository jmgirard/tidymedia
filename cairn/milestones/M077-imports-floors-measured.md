# M077: The nine other Imports floors, measured

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M076
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m077-imports-floors-measured`

## Goal

Every version floor DESCRIPTION's `Imports` field declares is exercised against
the version it names, so each floor states what was measured rather than what
was assumed.

## Scope

Surface tier: **user-facing** — a declared floor is what a user's installer may
resolve to, so a floor nothing has run is a promise the package has not kept.

**In:** `data-raw/imports-floors.R`, a developer-only script that builds one
library pinning every declared `Imports` floor at once and runs the test suite
under it; per-package bisection only if that run fails; DESCRIPTION floors left
or moved on the result; a D-entry carrying the per-floor disclosure; a NEWS
entry carrying the user-visible fact.

**Out:** a permanent CI job installing the floors on every push → a candidate
row added by this plan, promoted on the first floor regression that reaches a
user. Nine one-floor-at-a-time legs and a three-OS run → not done; the gap each
leaves is disclosed in the D-entry, not closed. The `Depends: R (>= )` field →
M076. Hardening `data-raw/withr-floor.R`'s four inherited rough edges → stays
on the `Imports`-floors candidate row.

## Acceptance criteria

- [ ] AC1 Every non-base entry of DESCRIPTION's `Imports` field — the entries
      `read.dcf(DESCRIPTION, "Imports")` enumerates, less `tools` and `utils`,
      which carry no floor — declares a version that AC2's run loaded and
      passed on. A floor whose run failed has moved to one that passes.
- [ ] AC2 The package's `testthat` suite runs to completion in a fresh
      `Rscript` session whose first `.libPaths()` entry is a library holding
      exactly the version each of those entries declares, with 0 test failures
      and a skip count equal to the same suite's skip count on current
      dependencies; the session asserts `ffmpeg` and `mediainfo` are both on
      `PATH` before running, and asserts per pinned package both the version
      loaded and the directory it loaded FROM. A mismatch, a failing
      `test_that()` block, an absent binary, or a non-zero child exit stops the
      run.
- [ ] AC3 Where a declared floor cannot be installed or built on the R and
      system toolchain AC2's run uses, the error is recorded and the floor
      moves to the first version that installs and passes AC2, found by walking
      that package's CRAN Archive listing forward from the declared floor and
      attempting each in turn.
- [ ] AC4 A `cairn/DECISIONS.md` entry states, per floor this milestone leaves
      in place or moves, what was run against it — naming AC2's suite, its R
      version and its runner OS — and these three things that were not: the
      pinned set is the direct `Imports` only, so siblings and transitive
      dependencies were at their current CRAN versions, except the packages
      the run held back so the pinned floors could load at all — the entry
      names each one, the version it was held at, and the requirement that
      forced it; no floor was run alone against
      current siblings, so a joint pass does not attribute; and the run was on
      one operating system.
- [ ] AC5 `NEWS.md` states as a user-visible fact that the declared dependency
      floors are now measured. `devtools::test()` and `devtools::check()` clean
      (0 errors, 0 warnings) on current dependencies.

## Coverage

- AC1 → T2, T4
- AC2 → T1, T2
- AC3 → T3
- AC4 → T5
- AC5 → T6

## Tasks

- [x] T1 Write `data-raw/imports-floors.R`: read the `Imports` entries and their
      floors from DESCRIPTION by `read.dcf`, install each into one library
      under a temporary root, and drive a fresh `Rscript` child with that
      library first. Carry over M074's load-bearing control from
      `data-raw/withr-floor.R:1-40` — assert per package the *directory* it
      loaded from, not only the version string, since the user library holds
      current releases and a failed pin would otherwise pass silently.
- [ ] T2 Record the current-dependency baseline skip count first, then run the
      pinned suite. Record per-file pass / fail / skip counts in the milestone
      file. Note `archive` 1.1.1 needs `libarchive` and compiles against `cli`
      headers (`LinkingTo: cli, cpp11`), and `purrr` 1.0.0 also `LinkingTo:
      cli` — expect these two to be where an install fails, if any does.
- [ ] T3 On a failure: bisect per package (re-run with one floor pinned and the
      rest current) to attribute it, then apply AC3's Archive walk to the
      package it attributes to.
- [ ] T4 Apply the result to DESCRIPTION — floors left or moved.
- [ ] T5 Draft the D-entry per AC4, including its falsifier.
- [ ] T6 NEWS entry, `devtools::document()`, `devtools::test()`,
      `devtools::check()`.

## Work log

- 2026-08-27: created by /milestone-plan.
- 2026-08-27: criteria audit (full mode, user-facing tier) returned 13 findings across both milestones' drafts; AC5 was the one criterion here that passed all six questions clean. Fixed for M077: AC1 was a tautology satisfied by any DESCRIPTION; AC2's "0 test failures" was satisfiable by a run where every execution test skipped, since the suite `skip_if`s the media binaries; AC3's "the oldest version that does install" named no procedure enumerating the archived versions; AC4 claimed something of *every* transitive dependency with nothing enumerating the closure.
- 2026-08-27: plan gate chose one joint pinned-library run with bisection on failure over nine one-floor legs plus the joint one, because the extra nine builds only buy attribution in the case where something fails; falsified by a joint run that fails and whose bisection is itself expensive, or by a floor that passes jointly and fails against current siblings.
- 2026-08-27: plan gate chose a one-off `data-raw/` script over a permanent hand-rolled min-deps CI job, because the r-lib action has no oldest-version input so the job must be hand-rolled, and a permanent job commits the repo to keeping ten floors green on every push; falsified by a floor regression reaching a user between audits.
- 2026-08-27: plan gate chose keeping the per-floor what-ran / what-did-not statement in the D-entry with NEWS carrying only the user-visible fact, over binding that sentence in NEWS, because that exact slot failed three review rounds in M074 before being descoped to this row; the audit also read a NEWS-bound version as promising a property of the write-up rather than of the package. Disclosed: AC4 still binds a record rather than the package, which is the user's call at this gate rather than an oversight. Falsified by the D-entry proving as hard to state accurately as the NEWS sentence was.

- 2026-08-27: gate 1 — the declared floors do not build on the host's R 4.6.1: R 4.5 hid `Rf_findVar`/`ATTRIB` behind `ENABLE_LEGACY_NONAPI_FUNS` and dropped `SET_FORMALS`/`SET_CLOENV`/`PRVALUE` outright, so `cli` 3.4.0, `rlang` 1.1.0, `tibble` 3.1.4, `purrr` 1.0.0 and `dplyr` 1.1.0 fail to compile and `archive` 1.1.1 finds no `libarchive`; AC3's walk forward reached today's current release in every case. User chose measuring under an older R in a container (colima + `rocker/r-ver:4.4.3`, the newest release that still declares those entry points and inside the package's own `R (>= 4.1.0)`) over moving six floors to 2026 releases or amending AC1/AC3 to disclose instead. Criteria unchanged.
- 2026-08-27: amendment (substantive, gate 2) — AC4's "current CRAN versions" clause now carves out the test harness. With the nine floors pinned and everything else current, R's own namespace version checks reject three floors for reasons no user installs: current `testthat` requires `withr (>= 3.0.2)` and `cli (>= 3.6.5)`, current `furrr` requires `purrr (>= 1.2.1)`. Moving those three would have superseded D053's measured `withr` 2.5.0 floor for a test-side reason. User chose holding the harness at the newest versions the floors permit, so floors move only where tidymedia's own runtime closure forces it — current `vctrs` and `dplyr` requiring `rlang (>= 1.1.7)`, current `dplyr` requiring `cli (>= 3.6.2)` and `tibble (>= 3.2.0)`. AC1, AC2, AC3 and AC5 unchanged; AC4 amended as above.
- 2026-08-27: criteria audit of the amended AC4, full mode (user-facing tier), run in-session rather than by a fresh-context [O] reader because agent delegation is disabled in this session — one finding, fixed at the gate: the draft carved out "`testthat`, `pkgload`, and the `Suggests` the suite loads", a universal claim over a set no named procedure enumerates; the carve-out now names the packages the run itself held back, which the run records. The satisfiability, reachability, proportionality and probe questions returned nothing. The instrument question returns what the plan gate already disclosed and the user already took — AC4 binds a record rather than the package — unchanged by this amendment.

- 2026-08-28: T1 — `data-raw/imports-floors.R` written. It reads the `Imports` floors by `read.dcf`, orders the installs by the LinkingTo/Imports edges among the pinned set itself (so `archive` and `purrr` compile against the PINNED `cli` headers rather than the user library's), drives `R CMD INSTALL` directly so AC3 records the compiler's own error rather than `install.packages()`'s "non-zero exit status" warning, and runs the suite in a fresh `Rscript` whose first `.libPaths()` entry is the pinned library. It carries M074's load-bearing control over: per pinned package it asserts the version AND the directory resolved, before anything loads and again for every pinned namespace loaded after the suite. Added beyond the plan: `--baseline`, `--only`, `--repair` and `--walk` modes; `TM_LIBROOT` to persist the pinned libraries across runs (whose reuse is guarded by re-reading the installed DESCRIPTION's `Version`, not by `file.exists()` — the trap M074's review left on the `Imports`-floors row); and `TM_RUN_TIMEOUT`, a wall-clock bound on each child, after two runs wedged for over half an hour on a single spawn.

## Decisions

## Review
