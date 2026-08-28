# M079: The floor harness measures what it reports

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m079-floor-harness-hardening` / https://github.com/jmgirard/tidymedia/pull/83

## Goal

Make the four `data-raw` floor-measurement scripts fail loudly where they
today return a number nobody measured, and delete the two modes whose bugs
are cheaper to remove than to fix.

## Scope

**Surface tier: internal.** Nothing under `data-raw/` ships
(`.Rbuildignore:15` `^data-raw$`), so no external consumer of the repo
relies on any of it; what is at stake is whether the next floor
measurement can be trusted, not user-visible behavior.

**In:** the fourteen shapes the M074/M076/M077/M078 reviews left on the
carry row, across `data-raw/imports-floors.R`, `withr-floor.R`,
`r-floor.R` and `timeout-bound.R`. Three are retired by deleting
`--repair` and `--walk` (M077 F7, F22, F14) rather than repaired; the rest
are fixed. `--only` survives: D055 item 4 reserves it as the attribution
tool for the still-open one-floor-at-a-time candidate.

**Out:** committing the container (M078 already did — `Dockerfile.floors`).
A permanent CI job installing the floors on every push → the standing
ROADMAP candidate, declined at M077's gate. The nine one-floor-at-a-time
legs and the three-OS run → still-open ROADMAP candidates, whose gaps D055
discloses rather than closes. Any change to a declared floor VERSION →
none is proposed; D053 and D055 stand untouched.

## Acceptance criteria

- [x] AC1 — Planted at the cache path, each of three defect forms is
      refused and re-fetched by `imports-floors.R`'s `fetch_tarball()` and
      by `withr-floor.R`'s fetch: a truncated gzip, an HTTP error body
      above the 1000-byte size floor, and a well-formed tarball carrying no
      `DESCRIPTION`. Today `imports-floors.R:124` returns the first two and
      `withr-floor.R:61-72` returns all three, because both short-circuit
      before the validation the download branch runs at `:145`.
- [x] AC2 — `install_pin()` (`imports-floors.R:162`) and `install_withr()`
      (`withr-floor.R:57`) reuse an installed library entry only when its
      `DESCRIPTION` `Version` matches AND no pinned package it
      LinkingTo-depends on has been installed since; otherwise they
      reinstall. Neither passes `--no-test-load` (`:179`, `:76`), so an
      install that cannot be loaded is a reported failure rather than a
      pass. A library root whose path contains `~` and a space installs
      successfully.
- [x] AC3 — Each of the three DESCRIPTION readers aborts rather than
      returning a wrong value on input it cannot parse: `withr-floor.R:46`
      on an `Imports` whose `withr` entry lost its `(>= )`, where today
      `sub()` returns the whole `Imports` field; `r-floor.R`'s
      `r_floor_of()` on `Depends: R (> 4.0)` and on `R (== 4.1.0)`, both of
      which today read as "none declared"; and the unversioned-entry
      carve-out at `imports-floors.R:96` and its `r-floor.R` twin, narrowed
      from `priority = c("base", "recommended")` to the unversioned entries
      DESCRIPTION actually declares, so an unversioned `MASS` aborts
      instead of being silently skipped.
- [x] AC4 — Four sites that today report a value the run did not measure
      are corrected: a failed `available.packages()` fetch
      (`imports-floors.R:413`) is distinguished from "no later versions
      exist"; the reconciliation loop (`:628`) aborts on non-convergence
      instead of falling out of `1:5` silently; the per-file summary table
      prints the errors it aggregates rather than only counting them; and
      `timeout-bound.R:360`'s `elapsed(s)` column reports the per-case
      `observed elapsed` of `:267` that D056 quotes, not the driver wall
      clock of `:342`, ~2.2 s above it.
- [x] AC5 — `imports-floors.R`'s holdback set is the named test-harness
      packages the pinned floors cannot satisfy (`testthat`, `furrr` — D055
      item 2), replacing the "everything outside the runtime closure"
      definition at `:626-663`; the computation returns no package outside
      that named set.
- [x] AC6 — `--repair` and `--walk` are gone from `imports-floors.R` —
      flags, `opt_value()` reads, code paths, and the header's usage lines
      — such that `grep -n -e '--repair' -e '--walk' data-raw/` returns no
      match; `--only` survives with its name guard reachable; and the dead
      code M077 F18 names, with its comment, is deleted.
- [x] AC7 — `devtools::check()` clean (0 errors / 0 warnings) and
      `devtools::test()` green, unchanged from the pre-milestone baseline.

## Coverage

- AC1 → T3, T4
- AC2 → T4, T5
- AC3 → T4, T6
- AC4 → T2, T7, T8
- AC5 → T2
- AC6 → T1
- AC7 → T9

## Tasks

- [x] T1 — Delete `--repair` and `--walk` from `imports-floors.R`: the flag
      parsing at `:67-69`, their code paths, and the usage lines at `:9`
      and `:15`. Keep `--only` and make its name guard reachable (M077 F14
      was the `--only X --walk Y` bypass). Delete the M077 F18 dead code
      and its comment.
- [x] T2 — Redefine the holdback set (`:626-663`) as the named test-harness
      packages; make the `for (round in 1:5)` loop abort on non-convergence
      instead of exiting silently.
- [x] T3 — Make `fetch_tarball()`'s cache branch (`:124`) call the same
      validator as its download branch (`:145`); give `withr-floor.R`'s
      fetch (`:61-72`) that validator too.
- [x] T4 — Build the planted-defect harness under `data-raw/`. Per the M52
      lesson, plant one probe per INPUT CLASS the code distinguishes and
      vary FORM, not only location: three cache defect forms, a library
      entry with the right `Version` and stale headers, an `Imports` whose
      `withr` entry lost its `(>= )`, `Depends: R (> 4.0)` and `R (==
      4.1.0)`, an unversioned `MASS`, and a `~`-and-space library root.
- [x] T5 — Strengthen the install-reuse guard to `Version` plus LinkingTo
      provenance (`:162`, `withr-floor.R:57`); drop `--no-test-load` from
      both call sites; verify the `~`-and-space library root reaches
      `R CMD INSTALL -l` intact (M077 F17 left this unverified).
- [x] T6 — Fix the three DESCRIPTION readers: `withr-floor.R:46`'s `sub()`
      to abort on no match, `r_floor_of()` to handle or refuse `>`/`==`
      comparators, and the carve-out at `imports-floors.R:96` and its
      `r-floor.R` twin.
- [x] T7 — Distinguish an `available.packages()` failure from an empty
      result (`:413`); print the aggregated per-file errors in the summary
      table (M077 F15).
- [x] T8 — Fix `timeout-bound.R:360` to report `observed elapsed`; confirm
      the corrected column reproduces D056's quoted per-case numbers.
- [x] T9 — `devtools::check()` and `devtools::test()`. Then a smoke run of
      `imports-floors.R` end to end in `Dockerfile.floors` — NOT an
      acceptance criterion (it crosses an environment boundary, which the
      internal-tier criteria standard bars a promise from spanning) — and
      record its outcome in the work log either way.

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in REDUCED mode (surface tier internal, no RB-tripwire tag on any criterion or task); delegation is disabled for this session, so the reader was in-session rather than fresh-context — a real weakening of the instrument, recorded rather than papered over. Two findings, both fixed at the gate: AC1 as first drafted bound an instrument property (it required the demonstration to be "a script under `data-raw/`" and its transcript quoted in evidence), so the harness moved to T4 and the criterion narrowed to the two fetch functions' behavior; AC4 as first drafted read "the run reports no result it did not measure", a universal over a domain no named procedure enumerates, and was narrowed to the four enumerated sites.
- 2026-08-28: plan gate chose simplify-then-fix over hardening all fourteen shapes across two milestones (M079 + M080) because deleting `--repair` and `--walk` retires M077 F7, F22 and F14 outright and the checker-regress shape makes simplification the recommended disposition; falsified by the one-floor-at-a-time legs candidate being planned and needing either deleted mode.
- 2026-08-28: plan gate chose to keep `--only` over deleting it alongside `--repair`/`--walk` because D055 item 4 names it "the attribution tool" and reserves it for the still-open one-floor legs; falsified by those legs being dropped, which would leave `--only` with no caller.
- 2026-08-28: plan gate chose host-side planted defects over binding a full container run as a criterion because the internal-tier criteria standard bars a promise spanning an environment boundary and the container run costs an hour-plus; falsified by a host-side probe passing while the container run regresses, which is why T9 runs the container anyway and logs the outcome.
- 2026-08-28: plan gate chose fixing the scripts over deleting them because the nine one-floor-at-a-time legs and the three-OS run are open candidates that would have to hand-roll the harness again; falsified by both being dropped, after which D053/D055/D056's recorded prose is the whole value.
- 2026-08-28: T1 — `--repair` and `--walk` deleted from `imports-floors.R`: usage lines, flag parsing, the walk block and its `--only`-bypassable name guard (M077 F14), and the repair path with F18's dead `probe <- file.path(LIBROOT, "walk")` and its comment. A failed floor install now aborts naming the floors, instead of telling the reader to re-run with a mode that no longer exists. `grep -n -e '--repair' -e '--walk' data-raw/` returns no match; `--only nosuch` still stops with "nosuch is not a versioned Imports entry".
- 2026-08-28: T2 — the holdback set is `HOLDBACK_SET <- c("testthat", "furrr")` (D055 item 2), and a requirer outside both the runtime closure and that set now stops the run naming itself rather than being downgraded; the `1:5` loop aborts on non-convergence instead of falling out silently. Both decisions moved into `reconcile(pins, closure, gather, pick, version_of)`, whose `gather`/`pick` are arguments rather than globals, so T4's probes can reach either refusal without a network or an install.
- 2026-08-28: T3 — `is_package_tarball()` in both `imports-floors.R` and `withr-floor.R`; `fetch_tarball()`'s cache branch and `withr-floor.R`'s fetch (which had no validated cache branch at all) now both go through it, and a refused file is unlinked and refetched. Measured while writing it: the listing test the download branch already ran is NOT sufficient for a truncated gzip — a 190,000-of-202,719-byte truncation still lists `DESCRIPTION` and `tar` reports the truncation only by exiting 1, so the validator reads `attr(inside, "status")` as well as the listing.
- 2026-08-28: task order — T5/T6/T7/T8 taken before T4 so the probe harness exercises the final code rather than code it would then have to be rewritten against. Minor amendment; no criterion or scope text changed.
- 2026-08-28: T5 — `install_pin()` writes a `tidymedia-floor-pin.dcf` stamp beside each installed DESCRIPTION recording, per PINNED package the entry LinkingTo-depends on, the version in the library at compile time; reuse now needs the `Version` AND that stamp to match. `install_withr()`'s guard was `dir.exists()` alone and is now the installed `Version`, with withr's LinkingTo-vacuity checked rather than assumed. `--no-test-load` dropped from both call sites.
- 2026-08-28: T5 — M077 F17 measured rather than left unverified: `R CMD INSTALL -l '~/tm floor probe/lib'` (tilde AND space, shQuote'd exactly as `install_pin()` passes it) installed a probe package with status=0, as did the `path.expand()`ed form — R expands the tilde itself, so F17's concern does not materialize on macOS 26.5 / R 4.6.1. `path.expand()` on `TM_LIBROOT`/`TM_SCRATCH` is kept anyway: it makes the result independent of R continuing to do that.
- 2026-08-28: T6 — `withr-floor.R`'s `sub()` replaced by a `regexec()` that aborts when `Imports` declares no `withr (>= ...)`, instead of returning the whole field as the floor. `r_floor_of()` now captures the version spec whole and reads the comparator out of it, aborting on anything but `>=`, so `R (> 4.0)` and `R (== 4.1.0)` stop the run rather than reading as "none declared" and dropping that dependency from the maximum. The unversioned carve-out in both scripts is `UNVERSIONED_OK <- c("tools", "utils")` — the entries DESCRIPTION declares — not `priority = c("base", "recommended")`, which waved through ~30 packages including `MASS`. `BASE_PKGS` stays broad where it is still right for what it does (`ensure_deps`, `runtime_closure`).
- 2026-08-28: T6 — `r-floor.R`'s `fetch_description()` given the same `is_package_tarball()` validator, on its cache path and its download. Not named by AC1, which binds the two fetches in `imports-floors.R` and `withr-floor.R`; it is the third copy of the one shape Scope's In lists (M076 P1), and leaving it accepting on size alone would have left the shape half-fixed. `Rscript data-raw/r-floor.R` runs end to end and still reports (a) 4.1.0, (b) 4.0.0, maximum 4.1.0 — M076's result unchanged.
- 2026-08-28: T7 — `archive_versions()` aborts on both of its network reads rather than falling back to an empty list: a failed Archive listing and a failed `available.packages()` were indistinguishable from "no later versions exist", and that list is what `newest_compatible()` searches, so an empty one surfaced as "no version of X is compatible with the pinned floors". `available.packages()` reports a failed fetch as a warning and a zero-row matrix rather than an error, so the row count is checked too, not only the class.
- 2026-08-28: T7 — the per-file table and the TOTALS line now print the `error` column they were already aggregating (M077 F15); the driver's TOTALS parser and the baseline/pinned comparison lines carry `err` with them. This changes the shape of the TOTALS line D055 quotes — a re-run now prints `pass=... fail=... err=... skip=... files=...` where D055 recorded four fields. D055's numbers stand; only the line's field count moves.
- 2026-08-28: T8 — `timeout-bound.R`'s summary reports each case's own `observed elapsed` and carries the driver stopwatch in a second, separately labelled column. Re-run on the host: A1 42.02, A2 22.01, A3 42.02, A4 42.02, B1 2.02, B2 2.02, C1 2.50 against D056's host column of 42.03/22.02/42.03/42.01/2.01/2.01/2.37 — every case within 0.13 s, C1 the widest. The driver column read 44.35/24.39/44.37/44.36/4.37/4.36/4.83, i.e. 2.33 s above, which is the number the old single column was printing.
- 2026-08-28: T4 — `data-raw/floor-probes.R`: 45 probes, 0 failed (`--offline` runs 38 of them). Each of the four scripts is sourced under `TM_DEFS_ONLY`, a guard that stops it just above its driver; `defs_of()` refuses to return if a script runs past that guard, so the harness cannot silently start a measurement. Probes needing a DESCRIPTION the repo does not have run the real script from a staged package root of symlinks with one modified DESCRIPTION — nothing writes to the repo.
- 2026-08-28: T4 — the late-truncation probe (A3/A6) is the one that earned its place: a gzip truncated PAST the DESCRIPTION entry still lists it, so the listing check the download branch had always run accepts it (A6 asserts exactly that), and only `tar`'s exit status refuses it. The fixture asserts it still lists DESCRIPTION before the probes run, so it cannot degrade into passing for the wrong reason. Two probes failed while being written and were real: a non-settling `gather` that in fact settled, and an H1 whose own label was the only match its grep found.
- 2026-08-28: T9 — `devtools::check()` 0 errors / 0 warnings / 0 notes (3m 26.6s); `devtools::test()` FAIL 0 | WARN 12 | SKIP 5 | PASS 6692, identical to the pre-milestone baseline taken on this branch before any edit. Nothing under `R/`, `man/`, `tests/` or `NAMESPACE` was touched by this milestone, so the profile's per-task `devtools::test()` trigger ("after code changes") never fired between tasks; it was run at the start and at the end instead, and that is stated here rather than implied.
- 2026-08-28: T9 — container smoke run, `docker run --rm -v $PWD:/pkg -w /pkg tidymedia-floors:r443 Rscript data-raw/imports-floors.R`, exit 0. It reproduces D055's measurement on the hardened script: baseline and pinned both **pass=6120 fail=0 err=0 skip=22 over 66 files**, no floor moved, holdbacks exactly `furrr` 0.4.0 → 0.3.1 and `testthat` 3.3.2 → 3.1.10 — D055 item 2's pair, now reached by the named `HOLDBACK_SET` rather than by the runtime-closure complement. All nine floors installed and test-loaded with `--no-test-load` gone. A FIRST container run aborted at its closing report with a parse error: the file was edited on the mounted repo while `Rscript` was still reading it, which shifted the byte offsets mid-parse. That is an artifact of the edit, not of the script; the run above is on the committed file with no concurrent edit, and it is the one reported.

## Decisions

**M079-D1 — the floor harness refuses rather than reports, and two of its modes
are gone.** Milestone-local; D053, D055 and D056 stand untouched, and no
declared floor moved.

- **`--repair` and `--walk` are deleted, not fixed.** A floor that will not
  install here now prints every failure and stops. Walking a package's Archive
  forward to something that does build chooses a version for DESCRIPTION to
  declare, and that is a decision, not a measurement — the two modes carried
  three of the fourteen shapes (M077 F7, F22, F14) between them and had no
  other caller. `--only` survives: D055 item 4 reserves it for the
  one-floor-at-a-time legs, which stay plannable.
- **The holdback set is named, not derived.** `HOLDBACK_SET <- c("testthat",
  "furrr")` — D055 item 2's pair. The old definition, "everything outside the
  runtime closure", described the container it was measured in; on another host
  it would downgrade whatever unrelated package happened to declare a
  requirement the floors miss, and report the result as a floor measurement. A
  requirer outside the closure and outside the set now stops the run by name.
  **Consequence:** a future host that legitimately needs a third harness package
  held back will stop rather than proceed, and someone must decide whether to
  add it or move a floor.
- **"Installs" now means "loads".** `--no-test-load` is gone from both install
  sites, so a floor that compiles and cannot be loaded is a reported failure.
  All nine declared floors install and test-load in `Dockerfile.floors` under
  this change.
- **The unversioned carve-out is a literal list.** `UNVERSIONED_OK <-
  c("tools", "utils")`, not `priority = c("base", "recommended")` — the latter
  is a property of the R installation doing the measuring and waved through ~30
  packages. **Consequence:** adding an unversioned `Imports` entry to
  DESCRIPTION now requires editing both scripts, deliberately.
- **M077 F17 is measured, not carried.** `R CMD INSTALL -l` was given a
  `~`-and-space library root, shQuote'd exactly as `install_pin()` passes it,
  and installed at status 0 — R expands the tilde itself, so the concern does
  not materialize on macOS 26.5 / R 4.6.1. `path.expand()` on `TM_LIBROOT` and
  `TM_SCRATCH` is kept so the result stops depending on R continuing to do that.
- **Two recorded numbers now read differently.** D055's `TOTALS` line gains an
  `err` field, so a re-run prints five counts where D055 quotes four; its
  numbers are unchanged. D056's per-case figures were always the child's
  `observed elapsed`, but `timeout-bound.R`'s summary column printed the
  driver's stopwatch, ~2.3 s above them — the column now reports what D056
  quotes, and carries the driver clock beside it, labelled.

## Review

Reviewed 2026-08-28 on branch `m079-floor-harness-hardening`, PR
https://github.com/jmgirard/tidymedia/pull/83. `master` had not moved since the
branch was cut (0 behind, 9 ahead, clean tree), so no merge-and-retest was
needed. Diffstat: 7 files, +1036 / -247; the executable surface is the four
`data-raw` scripts plus the new `data-raw/floor-probes.R`, so the full
three-lens fan-out applied.

**The probe harness was proven able to fail before its green was trusted.**
`data-raw/floor-probes.R` is this milestone's own instrument, so review planted
defects in the scripts it measures and re-ran it three times. Round 1 —
`is_package_tarball()` stubbed to `return(TRUE)` in all three scripts, the
`can_reuse()` stamp requirement dropped, `r_floor_of()`'s comparator refusal
disabled, and the `stray` requirer check emptied — turned 21 of 45 probes red
(A2-A5 in all three scripts, B1-B4, C2, C3, E2, E3, G3). Round 2 — the two
`archive_versions()` network aborts reverted to empty fallbacks and
`observed_elapsed()`'s `(none)` reverted to a number — turned F1, F3 and F5 red.
Round 3 — `withr-floor.R`'s `regexec` abort reverted to `sub()`, and
`UNVERSIONED_OK` widened back to `BASE_PKGS` in both scripts — turned E5, E6-r-f
and E6-imp red. Every planted class went red in the probe that claims it, and
the scripts were restored to a clean `git diff` after each round.

### Acceptance criteria

- **AC1 — cache-path defects refused and refetched.** Fresh run of
  `Rscript data-raw/floor-probes.R`: 45 probes, 0 failed. A1-A5 exercise
  `is_package_tarball()` in `imports-floors.R`, `withr-floor.R` and `r-floor.R`
  over five inputs (a real tarball accepted; gzip truncated before DESCRIPTION;
  gzip truncated after it; an HTTP error body over the 1000-byte floor; a
  well-formed tarball with no DESCRIPTION). B1-B4 plant each defect form at the
  cache path and assert the fetch refuses it, unlinks it and refetches the real
  file. Discrimination: stubbing the validator turned all twelve A2-A5 probes
  and all four B probes red. Verified.
- **AC2 — install reuse, `--no-test-load`, tilde-and-space root.** C1-C4:
  matching `Version` with a matching linkage stamp reuses; a stamp naming an
  older `cli` reinstalls; no stamp at all (a pre-M079 library) reinstalls; a
  wrong `Version` reinstalls. Discrimination: dropping the stamp requirement
  from `can_reuse()` turned C2 and C3 red. D1-D3: an install into a library root
  containing both `~` and a space reports no error, lands the entry with its
  stamp, and is reused on a second call. `grep -n 'no-test-load' data-raw/`
  returns no match at either call site. Verified.
- **AC3 — the three DESCRIPTION readers abort on unparseable input.** E1-E4:
  `Depends: R (>= 4.0.0)` reads as 4.0.0; `R (> 4.0)` and `R (== 4.1.0)` are
  each refused rather than read as "none declared"; a package name ending in `R`
  does not become the R floor. E5: `withr-floor.R` refuses an `Imports` with no
  `withr (>= )` instead of handing back the whole field. E6-r-f / E6-imp: an
  unversioned `MASS` stops both scripts rather than being waved through.
  Discrimination: disabling the comparator refusal turned E2/E3 red; reverting
  the `sub()` and widening `UNVERSIONED_OK` back to `BASE_PKGS` turned E5, E6-r-f
  and E6-imp red. Verified.
- **AC4 — four sites that reported an unmeasured value.** F1/F2/F3: a failed
  Archive listing, an empty CRAN database and a failed `available.packages()`
  are each refused rather than read as "no later versions exist"; discrimination
  confirmed for F1 and F3 by restoring the empty fallbacks. G4: rounds that never
  settle stop the run instead of falling out of the loop. The per-file summary
  prints its `error` column (`imports-floors.R`, the child's TOTALS and per-file
  lines). Fourth site measured directly rather than through a probe — a fresh
  `Rscript data-raw/timeout-bound.R` reports `elapsed(s)` 42.02 / 22.01 / 42.02 /
  42.02 / 2.01 / 2.01 / 2.42 for A1-A4, B1, B2, C1 against D056's host column of
  42.03 / 22.02 / 42.03 / 42.01 / 2.01 / 2.01 / 2.37 — every case within 0.05 s,
  C1 the widest — while the separately labelled `driver(s)` column reads 44.28 /
  24.28 / 44.29 / 44.29 / 4.28 / 4.28 / 4.68, i.e. 2.26 s above, which is what the
  old single column printed. Verified.
- **AC5 — the holdback set is the named packages.** `HOLDBACK_SET <-
  c("testthat", "furrr")` at `data-raw/imports-floors.R:501`, read at `:539` and
  `:545`. G2: a named harness package is held back. G3: a requirer outside both
  the runtime closure and the named set stops the run by name; discrimination
  confirmed by emptying the `stray` computation, which turned G3 red. The second
  clause holds structurally — `stray` aborts before any `holdbacks[[r]]` is
  recorded, so the returned holdback set is a subset of `HOLDBACK_SET` by
  construction. Verified.
- **AC6 — `--repair` and `--walk` are gone.** `grep -rn -e '--repair' -e
  '--walk' data-raw/` exits 1 with no match. `--only` survives with its guard at
  `imports-floors.R:118-122`, reached unconditionally above the `TM_DEFS_ONLY`
  stop at `:799`: `Rscript data-raw/imports-floors.R --only nosuch` halts with
  "nosuch is not a versioned Imports entry". M077 F18's `probe <-
  file.path(LIBROOT, "walk")` and its comment are covered by the same empty
  grep. Verified.
- **AC7 — check and test unchanged from baseline.** `devtools::check()`:
  0 errors / 0 warnings / 0 notes, 2m 41.8s. `devtools::test()`:
  FAIL 0 | WARN 12 | SKIP 5 | PASS 6692 — identical to the pre-milestone
  baseline the T9 work-log line records. Verified.

### Consistency gate

`cairn_validate.py` exit 0, all checks passed, every advisory clean (the
`release window` advisory did not fire). No DESIGN principle changed
(`Principles touched: --`), so `cairn_impact.py` was skipped. Toolchain checks
from the `r-package` profile's `consistency-gate` slot: `devtools::document()`
produced no diff (`git status` clean apart from this milestone file);
`NAMESPACE`, `man/` and `data/` are untouched by the diff; `README.Rmd`,
`_pkgdown.yml` and the package surface are untouched, and
`pkgdown::check_pkgdown()` reports no problems; `NEWS.md` needs no entry because
nothing user-visible changed -- `.Rbuildignore:15` (`^data-raw$`) keeps every
file this milestone touched out of the built package; the diff adds no
top-level file; `devtools::check()` clean as recorded under AC7.

### Independent review

Three fresh-context lenses, none having seen the implementation, each on a
distinct evidence base.

**[S] prior-review-record lens — no findings.** The archive carries prose
`Review` paragraphs rather than `## Review` sections; four are relevant by
content (M074, M076, M077, M078). It checked the current code against each named
carried finding (M076 F8/P1/P2, M077 F7/F14/F15/F17/F18/F22) and found no
regression. The GitHub probe `gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1`
returned `[]`, so the per-PR walk was skipped. It noted `BASE_PKGS` surviving at
`imports-floors.R:95` and confirmed it is retained only for `ensure_deps` /
`runtime_closure`, as the milestone states, not for the unversioned carve-out.

**[S] blame-history lens — no findings.** It traced every deletion and
behavior change to a dated acceptance criterion and cross-checked D047, D053,
D055 and D056 plus the M077 archive and the ROADMAP carry row. It confirmed
`archive_versions()` survives the `--walk` deletion because `newest_compatible()`
still calls it, that `--no-test-load` is recorded nowhere as a deliberate fix,
and that every narrowing (the named holdback set, `UNVERSIONED_OK`, the
comparator refusal) is disclosed in M079-D1.

**[O] diff-bug lens — fifteen findings, ranked as reported.** Verbatim, with
disposition:

- **F1 — `--offline` still goes to CRAN, and offline it fails.**
  "`floor-probes.R:327-332` runs `r-floor.R` end to end at the staged root;
  `r-floor.R`'s driver calls `fetch_description()` for all nine versioned
  `Imports` entries *before* it reaches the appended unversioned `MASS`, so
  E6-r-f downloads nine tarballs. The header (`floor-probes.R:8`) and the T4
  work-log line both say `--offline` 'skip[s] the four that fetch'; with no
  network E6-r-f never sees the abort message and reports a probe FAILURE, not a
  skip." Confirmed by reading `floor-probes.R:36`, `:207`, `:270` and `:326-332`
  — the E6 loop sits outside both `if (!OFFLINE)` guards.
- **F2 — A6 is host-dependent, and the exit-status branch it justifies is dead
  under R's internal tar.** "`attr(untar(list=TRUE), 'status')` is only ever set
  when `untar` shells out; with `TAR` unset/empty R uses the internal reader,
  which (measured here, fractions 0.5->0.9999) returns *zero* entries for the
  late-truncated fixture. So `is_package_tarball` still refuses (good), but
  `listing_only()` returns FALSE and probe A6 at `floor-probes.R:203`, which
  asserts TRUE, fails — aborting the whole harness on a host where nothing is
  wrong. `imports-floors.R:142-143` (and its two copies) is a no-op on that same
  host." Reproduced at review: with `TAR=""` and `TAR="internal"`,
  `untar(late, list = TRUE)` errors and returns NULL, so `listing_only()` is
  FALSE and A6 would fail. `is_package_tarball()` still refuses in both modes.
- **F3 — AC1 names `withr-floor.R`'s fetch; no probe exercises it.** "B1-B4
  (`floor-probes.R:213-229`) plant only at `imports-floors.R`'s `fetch_tarball`
  cache path. `withr-floor.R:88-92` and `r-floor.R:168-176` are covered only at
  the validator (`is_package_tarball`), never at the branch that calls it,
  unlinks and re-fetches — which is the behaviour AC1 binds for both fetches."
  Confirmed as a coverage gap in the harness. **AC1 itself was not left
  unverified:** review measured `fetch_withr_tarball()` directly, planting each
  of the four defect forms at `withr-floor.R`'s own cache path, and all four were
  refused, unlinked and refetched.
- **F4 — `can_reuse()` can abort the run where the design says it collects a
  failure.** "`imports-floors.R:219` calls `can_reuse` -> `linkingto_state` ->
  `linkingto_of` -> `fetch_tarball` (`:341`, unconditional), which `stop()`s on a
  fetch failure. Only the *later* fetch at `:224` is wrapped in `tryCatch`, so a
  failure inside the reuse check propagates out of `install_pin` and kills the
  run instead of joining the per-floor `failures` list the milestone's 'prints
  every failure and stops' contract depends on. It also makes reuse require the
  tarball, so a persisted `TM_LIBROOT` with a fresh `TM_SCRATCH` re-downloads
  everything it was meant to skip." Confirmed by reading the call chain:
  `linkingto_of()` calls `fetch_tarball()` unconditionally, before its own
  `file.exists(desc)` check.
- **F5 — `withr-floor.R` still treats a benign download warning as a failed
  fetch.** "`withr-floor.R:98-104` keeps `warning = function(w) FALSE` and then
  `unlink(tgz)`, which is precisely the hazard `imports-floors.R:167-172` and
  `r-floor.R:183-187` document and muffle. A warned-but-complete Archive fetch is
  deleted and retried against the contrib URL, which 404s for an archived
  version, so the run reports 'could not fetch withr X' for a tarball it had."
  Confirmed: `imports-floors.R:164-172` muffles the warning and checks the
  status; `withr-floor.R:98-102` does not.
- **F6 — probe D3 cannot fail for the reason it claims.**
  "`floor-probes.R:287-288` asserts only that a second `install_pin()` returns
  `NULL`; a full reinstall returns `NULL` too. Nothing (mtime, stamp contents,
  timing, a log marker) distinguishes reuse from reinstall, so the 'reuses it
  rather than reinstalling' label is untested."
- **F7 — probe H1 has no positive control.** "`floor-probes.R:406-412` passes if
  grep finds nothing, if grep is missing (warning suppressed, `character(0)`), or
  if the path were wrong. The self-match trap was correctly avoided, but the
  check's domain can silently empty; a companion assertion that the same grep for
  `[-][-]only` *does* match would fix it."
- **F8 — the MOVE line reports a version the run is not using.**
  "`imports-floors.R:527-529` prints `version_of(requirer)`, the *installed*
  version, for a requirement read out of a *pinned* tarball (`tarball_reqs`), so
  e.g. a dplyr-1.1.0-sourced requirement is annotated with the host's dplyr
  1.1.4. Carried over from master, but it sits inside the function this milestone
  rewrote and is the exact 'reports what it did not measure' shape."
- **F9 — `is_package_tarball` accepts a `DESCRIPTION` anywhere in the archive.**
  "`imports-floors.R:144` (and twins) tests `any(basename(inside) ==
  'DESCRIPTION')`, so a non-package tarball carrying `foo/inst/DESCRIPTION`
  passes. Probe A5 uses a tarball with none at all, so this form of the
  'well-formed tarball that is not a package' class is unprobed."
- **F10 — F4/F5 are decoupled from the format they parse.**
  "`floor-probes.R:358-363` feeds `observed_elapsed()` hand-written lines rather
  than anything derived from `emit()` (`timeout-bound.R:49-51`), so a change to
  the emitted key would silently restore the `(none)` substitution the fix exists
  to prevent. Related nit: `observed_elapsed(out, name)`'s `name` argument is
  never used (`timeout-bound.R:301`)." Independently reproduced at review: with
  the summary's `elapsed(s)` column rewired to print `r$wall`, probe F4 stayed
  green, so no probe covers AC4's fourth clause. That clause was verified instead
  by the direct `timeout-bound.R` run recorded under AC4.
- **F11 — G-probes depend on unrelated packages being installed.**
  "`floor-probes.R:387-400` leaves `version_of` at its default, so G2 calls
  `packageVersion('testthat')` and G4 `packageVersion('dplyr')`; on a host missing
  either, the probe errors for a reason unrelated to what it tests. G4 also
  hardcodes '5 rounds' while `MAX_ROUNDS` is a named constant
  (`imports-floors.R:503`)."
- **F12 — `run_under()` drops the shQuote the sibling scripts call
  load-bearing.** "`withr-floor.R:165` builds `R_LIBS=%s` unquoted for
  `system2(env=)`. Harmless with a tempdir-derived `LIBROOT`, but the
  `path.expand()` added at `withr-floor.R:60-61` is decorative for the same reason
  (`tempdir()` never contains `~`), so that half of the T5 change buys nothing
  here."
- **F13 — no probe for 'installs means loads'.** "AC2's `--no-test-load` clause
  is verified only by the container smoke run recorded in the work log; T4's probe
  list never plants a package that compiles and fails to load, so the criterion
  has no host-side evidence." AC2's clause as written is that neither call site
  passes the flag, which the empty grep settles; the missing probe is about what
  the flag's removal buys, not about the criterion.
- **F14 — the Decisions bullet states something D055 does not say.** "The M079
  Decisions bullet ('D055's `TOTALS` line ... a re-run prints five counts where
  D055 quotes four', milestone file line ~194, work-log line 151) describes a
  quoted TOTALS line; D055 (`cairn/DECISIONS.md:2470-2474`) quotes prose ('6120
  passing, 0 failing, 22 skipped over 66 files'), not the line. The code change is
  right; the claim about the prior record is not." Confirmed: `grep -n TOTALS
  cairn/DECISIONS.md` returns no match.
- **F15 — two smaller ones.** "`stage_root()` ignores every `file.symlink()`
  return value (`floor-probes.R:92-94`), so a partially staged root would
  under-test silently; and `imports-floors.R:41` still names 'pkgload, testthat
  and devtools' as the harness while `HOLDBACK_SET` is `testthat`+`furrr`, so the
  header and the set disagree about what the harness is (a `pkgload`/`devtools`
  requirement now stops the run — the documented consequence, but the header does
  not say so)."

**Return floor.** No finding demonstrates an acceptance criterion failing inside
its named procedure's domain, and none is a load-bearing defect in what the
package does for its users — nothing this milestone touches ships
(`.Rbuildignore:15`). F3 and F10 name gaps in the *evidence instrument*, not in
the behavior the criteria bind; review measured both clauses directly instead.
No defect return, no amendment return.

### Triage
