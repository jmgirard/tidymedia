# M079: The floor harness measures what it reports

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m079-floor-harness-hardening`

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

- [ ] AC1 — Planted at the cache path, each of three defect forms is
      refused and re-fetched by `imports-floors.R`'s `fetch_tarball()` and
      by `withr-floor.R`'s fetch: a truncated gzip, an HTTP error body
      above the 1000-byte size floor, and a well-formed tarball carrying no
      `DESCRIPTION`. Today `imports-floors.R:124` returns the first two and
      `withr-floor.R:61-72` returns all three, because both short-circuit
      before the validation the download branch runs at `:145`.
- [ ] AC2 — `install_pin()` (`imports-floors.R:162`) and `install_withr()`
      (`withr-floor.R:57`) reuse an installed library entry only when its
      `DESCRIPTION` `Version` matches AND no pinned package it
      LinkingTo-depends on has been installed since; otherwise they
      reinstall. Neither passes `--no-test-load` (`:179`, `:76`), so an
      install that cannot be loaded is a reported failure rather than a
      pass. A library root whose path contains `~` and a space installs
      successfully.
- [ ] AC3 — Each of the three DESCRIPTION readers aborts rather than
      returning a wrong value on input it cannot parse: `withr-floor.R:46`
      on an `Imports` whose `withr` entry lost its `(>= )`, where today
      `sub()` returns the whole `Imports` field; `r-floor.R`'s
      `r_floor_of()` on `Depends: R (> 4.0)` and on `R (== 4.1.0)`, both of
      which today read as "none declared"; and the unversioned-entry
      carve-out at `imports-floors.R:96` and its `r-floor.R` twin, narrowed
      from `priority = c("base", "recommended")` to the unversioned entries
      DESCRIPTION actually declares, so an unversioned `MASS` aborts
      instead of being silently skipped.
- [ ] AC4 — Four sites that today report a value the run did not measure
      are corrected: a failed `available.packages()` fetch
      (`imports-floors.R:413`) is distinguished from "no later versions
      exist"; the reconciliation loop (`:628`) aborts on non-convergence
      instead of falling out of `1:5` silently; the per-file summary table
      prints the errors it aggregates rather than only counting them; and
      `timeout-bound.R:360`'s `elapsed(s)` column reports the per-case
      `observed elapsed` of `:267` that D056 quotes, not the driver wall
      clock of `:342`, ~2.2 s above it.
- [ ] AC5 — `imports-floors.R`'s holdback set is the named test-harness
      packages the pinned floors cannot satisfy (`testthat`, `furrr` — D055
      item 2), replacing the "everything outside the runtime closure"
      definition at `:626-663`; the computation returns no package outside
      that named set.
- [ ] AC6 — `--repair` and `--walk` are gone from `imports-floors.R` —
      flags, `opt_value()` reads, code paths, and the header's usage lines
      — such that `grep -n -e '--repair' -e '--walk' data-raw/` returns no
      match; `--only` survives with its name guard reachable; and the dead
      code M077 F18 names, with its comment, is deleted.
- [ ] AC7 — `devtools::check()` clean (0 errors / 0 warnings) and
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
- [ ] T4 — Build the planted-defect harness under `data-raw/`. Per the M52
      lesson, plant one probe per INPUT CLASS the code distinguishes and
      vary FORM, not only location: three cache defect forms, a library
      entry with the right `Version` and stale headers, an `Imports` whose
      `withr` entry lost its `(>= )`, `Depends: R (> 4.0)` and `R (==
      4.1.0)`, an unversioned `MASS`, and a `~`-and-space library root.
- [x] T5 — Strengthen the install-reuse guard to `Version` plus LinkingTo
      provenance (`:162`, `withr-floor.R:57`); drop `--no-test-load` from
      both call sites; verify the `~`-and-space library root reaches
      `R CMD INSTALL -l` intact (M077 F17 left this unverified).
- [ ] T6 — Fix the three DESCRIPTION readers: `withr-floor.R:46`'s `sub()`
      to abort on no match, `r_floor_of()` to handle or refuse `>`/`==`
      comparators, and the carve-out at `imports-floors.R:96` and its
      `r-floor.R` twin.
- [ ] T7 — Distinguish an `available.packages()` failure from an empty
      result (`:413`); print the aggregated per-file errors in the summary
      table (M077 F15).
- [ ] T8 — Fix `timeout-bound.R:360` to report `observed elapsed`; confirm
      the corrected column reproduces D056's quoted per-case numbers.
- [ ] T9 — `devtools::check()` and `devtools::test()`. Then a smoke run of
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

## Decisions

## Review
