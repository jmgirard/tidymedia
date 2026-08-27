<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M074: The floor says what was measured

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate -->
- **Principles touched:** —   <!-- owner: plan · create/amend-via-gate -->
- **Branch/PR:** `m074-withr-version-floor` · https://github.com/jmgirard/tidymedia/pull/78   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create -->

Establish, by measurement rather than assumption, whether `local_timeout()`
behaves as documented on the oldest `withr` DESCRIPTION permits, and make the
floor state what was measured.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**Surface tier: user-facing.** DESCRIPTION's `Imports` floor is the install-time
contract — it decides which `withr` a user's installer resolves — and
`local_timeout()`'s `@details` are read by callers.

**In:** `withr` alone. It has exactly one runtime call site, `withr::defer()` at
`R/timeout.R:253` inside `local_timeout()`; everything else in `R/` is a roxygen
cross-reference. M073 measured every claim on withr 3.0.3, while DESCRIPTION
declares `withr (>= 2.5.0)`, and withr's NEWS puts a mechanism change between
them: 3.0.0 made `defer()` a thin `base::on.exit(after=)` wrapper where earlier
versions ran their own handler stack, and that prepend ordering is the whole
reason D052 chose `defer()` over `on.exit()`. 3.0.0 also broke the other way —
`source()` into a local environment needs `options(withr.hook_source = TRUE)`
from 3.0.0, and worked by default at 2.5.0.

The measurement decides the floor. If 2.5.0 holds every documented claim, the
floor stays and the D-entry records what was tested; if it does not, the floor
rises to the lowest version that does. A null result is a result: the candidate
row asked whether the floor understates, and "it does not" answers it.

**Out:**
- The other nine `Imports` floors, and the fact that CI verifies none of them →
  ROADMAP candidate row.
- The absent `Depends: R (>= )` line — withr 3.0.3 needs R >= 3.6.0, so any
  raise to 3.x moves tidymedia's effective R floor silently → same candidate row.
- A minimum-dependency CI job → same candidate row; it would commit the repo to
  keeping all ten floors green, which is the wide audit this milestone declined.
- Test-side `withr` use (`local_tempfile`, `local_options` in `helper-media.R`,
  `test-timeout-silence.R`, `test-nvenc.R`, `test-standardize-video-batch.R`) →
  not measured here; those are Suggests-side and say nothing about what a user
  installing tidymedia gets.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets. -->

- [x] AC1 — Run in an isolated library against the lowest `withr` version
      DESCRIPTION permits, `testthat::test_file()` reports zero failures over
      every `test_that()` block of `tests/testthat/test-local-timeout.R` and
      `tests/testthat/test-with-timeout.R`. The same session prints
      `packageVersion("withr")` and it equals the floor DESCRIPTION declares —
      an isolated library that silently resolved 3.0.3 makes the run vacuous,
      so the control is part of the criterion, not of its evidence.
- [x] AC2 — For each of two named forms — `local_timeout()` at the top level of
      a file run by `Rscript`, and at the top level of a file passed to
      `source()` with its default `globalenv()` — the value of
      `getOption("tidymedia.timeout")` after the file ends is recorded on the
      declared floor version and on 3.0.3. Where the two versions differ for
      either form, `local_timeout()`'s `@details` names the versions on which
      each behavior holds.
- [x] AC3 — Each of the four behavioral claims `local_timeout()`'s documentation
      makes — two calls in one frame unwind to the caller's state; a frame's own
      `on.exit()` without `add = TRUE` discards the undo; a `.local_envir` that
      is not a live frame takes the undo with it; a `local_timeout()` written
      directly inside `with_timeout()`'s `expr` outlives the wrapper — reads
      true when re-measured on the declared floor version, or the documentation
      names the versions on which it holds.
- [x] AC4 — `devtools::check()` is clean (0 errors / 0 warnings) and
      `devtools::test()` passes on the developer's current `withr`.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1, T2, T6
- AC2 → T3, T6
- AC3 → T3, T5, T6, T8
- AC4 → T5, T9

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->
<!-- T1-T17 compressed in one pass at the weight cap (tracking-rules): the work
     log below carries each task's full statement, findings and result. -->

- [x] T1 — Build `data-raw/withr-floor.R`: install a given `withr` from CRAN
      into an isolated library, evaluate under it, and prove the pin.
- [x] T2 — Run both timeout-wrapper test files under 2.5.0, a verdict per
      `test_that()` block; walk upward only on a failure.
- [x] T3 — Measure AC2's two top-level forms and the four documented claims on
      2.5.0 and on 3.0.3; record the differences.
- [x] T4 — Settle the floor and write the D-entry: the version, what forced it,
      what was and was not tested, and its own falsifier.
- [x] T5 — Update `DESCRIPTION`, `NEWS.md` and `local_timeout()`'s roxygen;
      `document()`, then `check()` and `test()`.
- [x] T6 — (return 1) Extend the harness to measure what the shipped docs claim
      — the `withr::` comparison, `parent.frame()` at each top level, the
      `Rscript` form's undo, `source(local = TRUE)` — and fail on a wrong pin.
- [x] T7 — (return 1) Rewrite D053 from T6: the globalenv branch IS reached,
      `is_top_level_global_env()` is not 3.x-only, D052 stands unqualified.
- [x] T8 — (return 1) Fix the roxygen and the call-site comment to say what T6
      measured; `document()`.
- [x] T9 — (return 1) Rewrite NEWS to an anchored range stating what was
      measured; file the harness's remaining edges; `verify` slot + `check()`.
- [x] T10 — (return 2) Harness: assert library provenance, stop on a FAIL
      block, measure where `defer()` registered at each form; re-run both.
- [x] T11 — (return 2) Rewrite D053's mechanism paragraph from T10, and say
      where the harness fetches each version.
- [x] T12 — (return 2) Fix the call-site comment to T10's measurement;
      `document()`.
- [x] T13 — (return 2) Rewrite NEWS to the two files and 35 blocks actually
      run and the one version difference; correct the ROADMAP row.
- [x] T14 — (descope) Execute the gated AC3 amendment: drop AC3, renumber the
      four that remain, retire the NEWS-accuracy promise to a candidate row.
- [x] T15 — (return 3) Correct the per-spawned-program clause in NEWS and D053:
      those tests are inside `test-with-timeout.R` and passed on 2.5.0 (O1).
- [x] T16 — (return 3) O2-O8: source D053's internals reading (O2), fix the
      ROADMAP arithmetic (O3), name AC2's forms in NEWS (O4), stop
      extrapolating past the one observation point (O5), correct where 3.0.3 is
      fetched from (O6), reconcile the unmeasured sets (O7), drop the "no claim
      covers" judgment (O8).
- [x] T17 — Shed the weight-cap overage by compressing Tasks in one pass;
      re-run `cairn_validate.py`, the `verify` slot and `devtools::check()`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates. -->

- 2026-08-27: created by /milestone-plan, absorbing the `withr` floor candidate row (M073 review P2; D052).
- 2026-08-27: criteria audit ran in FULL mode (declared tier user-facing). No fresh-context [O] reader was available — the AgentTool is disabled for this session — so the audit's questions were run by the plan author over the author's own draft, which is weaker than the instrument specifies and is recorded as such. Four findings. Fixed at the gate: draft AC1 promised that a measurement was recorded and draft AC4 that a D-entry exists, both instrument properties under D-118/D-120, rewritten to bind `local_timeout()`'s behavior and moved to T4 respectively; draft AC1 was vacuously satisfiable by a run that silently resolved 3.0.3, so the in-session `packageVersion("withr")` control became part of the criterion; draft AC2 quantified over "any difference between the forms", an unenumerated domain, and was narrowed to the two named forms. Raised as a gate question: AC1's test domain.
- 2026-08-27: plan gate chose withr alone over a full ten-package Imports audit because only withr has a load-bearing call site and a known mechanism change, and no symptom drives the other nine; falsified by a second floor being reported wrong before this one is.
- 2026-08-27: plan gate chose the two timeout-wrapper test files as AC1's domain over the whole suite under old withr because the rest of the suite's withr use is Suggests-side and says nothing about what a user installing tidymedia gets; falsified by an old-withr failure in `helper-media.R` that reaches a user.
- 2026-08-27: plan gate chose keeping 2.5.0 on a null result over pinning to 3.0.0 (the `defer()` mechanism change) or 3.0.3 (what M073 measured on) because a floor should exclude users for a measured reason, not a plausible one; falsified by a caller on 2.5.x hitting a difference this milestone's forms do not reach.
- 2026-08-27: plan gate chose a one-time measurement over a minimum-dependency CI job because such a job verifies all ten floors and commits the repo to keeping them green, which is the wide audit this milestone declined; falsified by a floor regressing between this milestone and the next DESCRIPTION edit.
- 2026-08-27: T1 — harness committed as `data-raw/withr-floor.R`; each version installs from the CRAN archive into its own library and every child session prints the `withr` it loaded (2.5.0 and 3.0.3 each reported their own, and `defer()`'s body carries `global_defer` only at 3.0.3, so the pin is real and the two versions are the two mechanisms).
- 2026-08-27: T2 — all 35 `test_that()` blocks of `test-local-timeout.R` and `test-with-timeout.R` pass under withr 2.5.0 with `NOT_CRAN=true`; identical 35/35 under 3.0.3, 0 failures and 0 skips on both, so no upward walk was needed.
- 2026-08-27: T3 — AC2's two top-level forms and AC4's four documented claims measured on 2.5.0 and 3.0.3 with identical results on every one; the Rscript form leaves the limit set at `.Last` and at a later finalizer on both, and the `source()` form restores the caller's value when `source()` returns on both (`parent.frame()` at a sourced file's top level is `source()`'s own eval frame, not `globalenv()`, so `defer()`'s globalenv branch — the one 3.0.0 rewrote — is not reached from either form).
- 2026-08-27: T4 — null result, so the floor stays 2.5.0 per the plan gate; promoted as D053, extending D052's dependency bullet with what was measured, why there was nothing to find, and what was not measured.
- 2026-08-27: T5 — DESCRIPTION needed no edit (`withr (>= 2.5.0)` is the measured floor); NEWS.md's withr bullet now states the floor and what was measured against it; `local_timeout()`'s two `@details` measurement notes and the `defer()` comment at the call site now name 2.5.0 alongside 3.0.3 and point at `data-raw/withr-floor.R` and D053. `devtools::document()` regenerated `man/local_timeout.Rd`; `devtools::test()` 0 failures / 6635 passing; `devtools::check()` 0 errors / 0 warnings / 0 notes.
- 2026-08-27: review returned M074 to in-progress. AC3 fails on its NEWS clause: the bullet states more than was measured (F5 — "each behavior their documentation describes was re-measured on both" is false for the per-program and parallel claims; F6 — the unanchored "an older withr"). Three load-bearing defects in what shipped, each re-verified against the implementation: F2 — the roxygen and man page credit withr 2.5.0 with a `withr::defer()`/`local_options()`/`with_options()` measurement `data-raw/withr-floor.R` never performs (zero `withr::` calls in it); F1 — D053 and the new comment at R/timeout.R:255-260 say `local_timeout()` never reaches `defer()`'s globalenv branch, but at the top level of a `source()`d file `parent.frame()` IS `globalenv()`, so it does; F3 — D053 calls `is_top_level_global_env()` 3.x-only, and it is in 2.5.0 at compat-defer.R:174. F4, F7, F8, F9 also to fix; F10 to a candidate row; F11 rejected. First defect return on this milestone.

- 2026-08-27: T6 — harness extended and re-run on both versions. New measurements: `parent.frame()` at an `Rscript` top level AND at a `source()`d file's top level is `globalenv()` on both versions (F1 — the branch IS reached); `deferred_run(globalenv())` restores the caller's 99 on both, so the `Rscript` form does have an undo scheduled and `.Last`/a later finalizer merely observe before it (F4); the `withr::defer()`/`local_options()`/`with_options()` comparison the roxygen makes now runs against withr itself and gives 30/30/30/30 and 99-then-30 identically on both (F2); `source(file, local = TRUE)` from a function frame is the one measured DIFFERENCE — 2.5.0 holds the limit inside the sourced file, 3.0.3 has it back at the caller's value on the next line, and both are back to 99 once the frame returns (F7). A mismatched pin now aborts the child and stops the driver, verified by a negative control (F8, F10-exit).

- 2026-08-27: T7 — D053 rewritten from T6's measurements: the globalenv branch is reached rather than avoided (F1), `is_top_level_global_env()` is 2.5.0-and-later while only `global_defer()` is 3.x-only (F3), the Rscript form has an undo scheduled that its own exit hooks observe before (F4), `source(local = TRUE)` is recorded as the one measured difference (F7), D052's `defer()` justification is said to stand unqualified (F9), and the two non-frame `@details` claims are named under "What was not measured" (F5's other half).

- 2026-08-27: T8 — the call-site comment at `R/timeout.R` now says the globalenv branch IS reached and that the two forms still agree, and points at the harness's withr:: arm (F1, F2). The two `@details` sentences crediting withr 2.5.0 with a `defer()`/`local_options()`/`with_options()` comparison now stand on T6's committed measurement rather than on assertion, so their wording is unchanged. `devtools::document()` produced no `man/` diff — the edit is a source comment.

- 2026-08-27: T9 — NEWS's withr bullet rewritten: it now names the two versions measured, says what was re-measured on each (the test suite and the four frame claims), names the two `@details` claims that were NOT run on the floor (per spawned program, and the `parallel = TRUE` fan-out) and the unmeasured interior, and anchors its closing sentence to "resolves withr 2.5.0 rather than the current release" (F5, F6 — AC3). F10's three remaining harness rough edges appended to the `Imports`-floors candidate row. `devtools::test()` 0 failures / 6635 passing / 5 skips; `devtools::check()` 0 errors / 0 warnings / 0 notes (2m 54s).
- 2026-08-27: defect return closed. AC3 re-ticked on the rewritten NEWS bullet; the other four criteria were re-ticked at review on fresh evidence and nothing in this round's edits touches what they measured — the harness's added arms only extend it, and its previously-reported values are unchanged on the re-run.
- 2026-08-27: review returned M074 to in-progress a second time. AC3 fails again inside its own domain: NEWS still states other than what was measured — G2, "the whole `with_timeout()` and `local_timeout()` test suite passes" when `test-parallel-option-carry.R` (`with_timeout()` at :494, :517, :535) was not run under the floor; G3, the one measured version difference (`source(local = TRUE)`, 30 on 2.5.0 against 99 on 3.0.3) is recorded in D053 and omitted from NEWS. G1 is a second load-bearing defect: D053's mechanism paragraph and the comment at R/timeout.R:259-262 say `local_timeout()` reaches the `defer()` branch withr 3.0.0 rewrote from both top-level forms; verified against both versions' source and empirically, it reaches it from the Rscript form only — under `source()` both versions redirect to `source()`'s frame first (3.0.3 `source_exit_frame_option()`, 2.5.0 `exit_frame()`/`source_frame()`). G4-G8 also to fix; G9 to the candidate row; G10, G11 rejected. Second defect return; thrash rule trigger (b) fires — AC3 twice, same shape — and the recorded alternative to reconsider is the plan gate's choice of AC1's two-file domain.

- 2026-08-27: T10 — harness re-run on both versions, exit 0, 70 PASS / 0 FAIL / 0 SKIP (35 blocks per version). The pin control is now provenance, not a version string: each of the 16 child sessions asserts that `dirname(find.package("withr"))` is the library it was handed, and `install_withr()` refuses to return a library with no `withr` in it (G4). Negative control run: an empty pinned library with the user library still on `.libPaths()` loads withr 3.0.3, passes the version assertion, and is caught by the provenance one — the exact false green G4 named. A `FAIL` verdict now stops the child (G5); negative control on a deliberately failing block exits 1 with the block named. New arm `formB-where.R` measures WHERE `defer()` registered at the `source()` top level, using `deferred_run(globalenv())` rather than either version's internals: on 2.5.0 and on 3.0.3 alike it reports "No deferred expressions to run" and leaves the limit at 30, where the same probe at an `Rscript` top level restores the caller's 99 — so the branch is reached from the `Rscript` form and redirected away from it under `source()`, on both versions (G1). No package code changed in this task, so the `verify` slot's `devtools::test()` has nothing to bite on; it runs at T12 and T13.

- 2026-08-27: T11 — D053's mechanism paragraph rewritten from T10's measurement: the branch withr 3.0.0 rewrote is reached from the `Rscript` form and NOT from the `source()` form, where both versions redirect the handler to `source()`'s own frame first by different routes, so the source form's cross-version agreement comes from a redirect both have rather than from the rewritten branch (G1). Round 1 replaced "never reached" with "always reached"; both went past the harness, which had measured only `parent.frame() == globalenv()`, and the new arm measures the thing itself. D053's "What was measured" paragraph now says the harness fetches the Archive for a retired version and the current `src/contrib` for the release, which is where 3.0.3 comes from (G8), and describes the pin control as provenance rather than a version string plus the new failing-block stop.

- 2026-08-27: T12 — the comment at `R/timeout.R:257-269` now says what T10 measured: `local_timeout()` hands `defer()` `globalenv()` from both top-level forms, only the `Rscript` form's undo lands there, and both versions redirect the `source()` form to `source()`'s own frame first (G1). `devtools::document()` produced no `man/` diff — the edit is a source comment, and the `@details` sentences it sits under are unchanged. `devtools::test()` 0 failures / 6635 passing / 5 skips / 4 warnings.

- 2026-08-27: T13 — NEWS's withr bullet now names what was actually run: "all 35 `test_that()` blocks of `test-local-timeout.R` and `test-with-timeout.R`" in place of "the whole `with_timeout()` and `local_timeout()` test suite" (G2); it states the one measured version difference, `source(file, local = TRUE)` from a function frame holding the limit on 2.5.0 and not on 3.0.3, with the note that either way the caller's value is back once the frame returns (G3); and the four documented claims are now enumerated as four, the two ways the undo can be lost named as part of them rather than added to them (G6). The ROADMAP harness row no longer presents its inventory as complete: it records that two of the five rough edges the reviews raised were reachable in the form M074 ran and were fixed here, lists the three that remain plus `install_withr()`'s reuse short-circuit, and says why each is a trap only for a wider run (G7, G9). `devtools::test()` 0 failures / 6635 passing / 5 skips / 4 warnings; `devtools::check()` `Status: OK`, 0 errors / 0 warnings / 0 notes (9m 23s).
- 2026-08-27: second defect return closed. AC3 re-ticked on the rewritten NEWS bullet; AC1, AC2, AC4 and AC5 were re-ticked at review round 2 on fresh evidence and this round's harness edits change no measured value — the pin and failing-block controls only make a false green fail, and the added `formB-where.R` arm reports a new fact rather than revising one. Re-run confirms it: 70 PASS / 0 FAIL / 0 SKIP, and every previously-reported value unchanged on both versions.
- 2026-08-27: thrash-rule gate (trigger (b), AC3 twice) — the user chose to HOLD AC1's two-file domain over widening it to include `test-parallel-option-carry.R`. No criterion changed. The recommendation was to hold under D-118 (a widening amendment on a milestone with two defect returns), and because the honest NEWS sentence is available without it: naming the two files and 35 blocks says what was measured, where "the whole test suite" was the overclaim that failed. `test-parallel-option-carry.R`'s floor coverage keeps its follow-up home on the `Imports`-floors candidate row. Falsified by a caller hitting a floor-dependent difference in the parallel fan-out that the two measured files do not reach.
- 2026-08-27: review returned M074 to in-progress a THIRD time. Two independent gate failures. (1) Consistency gate: `cairn_validate.py` exits 1 on `weight caps` — the milestone file carries 157 plan-owned lines against a cap of 150 (Tasks 66), the accumulated cost of two defect returns; review cannot shed it, every named section is plan-owned. (2) AC3 fails inside its own domain for the third time: NEWS says the per-spawned-program claim was not run on 2.5.0 and that its tests live outside the two measured files, and both halves are false — `test-with-timeout.R:255`, `:279`, `:432`, `:487` test exactly that claim, are inside one of the two files, and all four PASSED under the pinned 2.5.0 library with 0 SKIP (O1). O2-O8 to fix; O9 rejected. AC1, AC2, AC4, AC5 all met on fresh evidence and the measurement is unchallenged. Thrash rule: trigger (a) fires (third return — descope-or-park recommended, no further retry queued under the current plan) and trigger (b) fires again (AC3 three times, same shape); (b)'s recorded alternative — widening AC1's domain — was spent at round 2's gate when the user held it, so what remains of (b) is the offered `/milestone-brief` escalation.
- 2026-08-27: thrash-rule gate (trigger (a), third defect return) — the user chose DESCOPE over parking, escalation, or a bare retry. M074 narrows to the four criteria verified green at review round 3 (AC1, AC2, AC4, AC5); AC3's second half — NEWS stating what was measured against the floor — exits the milestone. Descope runs as a gated criterion amendment at `/milestone-implement` step 6, then re-review of the narrowed set. Three things ride with it and are NOT discharged by the descope: O1's false NEWS clause must still be fixed, because narrowing a criterion does not unship a user-facing sentence that says a claim was unmeasured when it was measured and passing; O2-O8 stay logged fix-now; and the `weight caps` gate failure (157 plan-owned lines against a cap of 150) must be shed in the plan-owned sections before the next review, since review cannot touch them. The NEWS-accuracy requirement itself exits to a candidate row.

- 2026-08-27: T14 — descope executed as a gated criterion amendment. AC3 is dropped; its DESCRIPTION half is not left unpromised, because AC1's own text already binds it ("it equals the floor DESCRIPTION declares"), and its NEWS half is what the thrash gate retired. `cairn_validate`'s `coverage complete` check counts criteria and demands Coverage reference exactly AC1..ACn, so the drop forced a renumber: old AC4 (the four documented claims) is now AC3 and old AC5 (`check()`/`test()` clean) is now AC4, both verbatim as written — the three Review sections above predate the renumber and their AC4/AC5 refer to the old numbering. The narrowing needs no fresh-reader audit under step 6: nothing was reworded, one criterion was deleted.
- 2026-08-27: T14 — the retired promise absorbed into the existing `Imports`-floors candidate row rather than filed as a new one (search-first): that row already inherits `data-raw/withr-floor.R` and M074's leftovers. It now records that no criterion binds a floor's NEWS disclosure, names the three shapes AC3 failed in, and asks whoever takes it to hold the disclosure sentence to the same evidence bar as the measurement.

- 2026-08-27: T15 — O1 fixed in NEWS and in D053's "What was not measured". The harness was re-run first, in this session, so the corrected sentence rests on its own measurement rather than on the review's account of one: exit 0, 70 PASS / 0 FAIL / 0 SKIP, and the four per-spawned-program blocks (`test-with-timeout.R:255`, `:279`, `:432`, `:487`) PASS under the pinned 2.5.0 library at log lines 55-58. `grep -n parallel` over both measured files still returns nothing, so only the `parallel = TRUE` half of the retired sentence was true. Both texts now say two things were unrun, not three, and say that the per-spawned-program claim was run and where its blocks live.

- 2026-08-27: T16 — O2-O8. O2: D053's mechanism paragraph now says the redirect clause is read from withr's own sources rather than from the harness, which reads no version's internals, and names the three readings and where they are recorded (review round 2 of this file); the call-site comment at `R/timeout.R` carries the same split between the measured outcome and the sourced explanation. O3: the ROADMAP row's inventory now adds up — seven rough edges raised, the three reachable in M074's form fixed (the third being `run_under()`'s ignored exit status, now a `stop()` at `data-raw/withr-floor.R:103-105`), four remaining. O4: NEWS now names AC2's two top-level forms and what each leaves behind. O5: NEWS and D053 no longer say 2.5.0 holds the limit "for the rest of the sourced file" — the harness has one observation point inside that file, so both now state the direction of the split and say so. O6: D053 says the script tries the Archive URL first for every version and records nothing about which URL answered. O7: NEWS's unmeasured set now matches D053's, the `knitr` target environment included. O8: the "which no claim on that page covers" judgment is out of NEWS; D053 keeps it as the floor rationale and now flags the tension with `?local_timeout`'s own description as a reading rather than a measurement. `devtools::document()` produced no `man/` diff — the R edit is a source comment. `devtools::test()` 0 failures / 6635 passing / 5 skips / 4 warnings.
- 2026-08-27: T17 — Tasks compressed in one pass, the remedy tracking-rules names for the heaviest plan-owned section: 81 lines to 43, every T-id kept so the Coverage map still resolves, and the work log left carrying each task's detail. `cairn_validate.py` now passes every check including `weight caps` (was 168 plan-owned lines against a cap of 150). One advisory remains and is not a gate failure: `sizing (split tripwires)` reads 17 tasks against a >10 tripwire, which is what three defect returns and a descope cost a milestone that was planned at five.

- 2026-08-27: `neighbouring` in the new NEWS sentence tripped `spelling.R` (the package's wordlist is US English), which surfaced as R CMD check's one NOTE while `devtools::check()`'s own summary line still printed 0 notes; reworded to `sibling` and re-run. `devtools::check()` `Status: OK`, 0 errors / 0 warnings / 0 notes (3m 0.5s).
- 2026-08-27: third defect return closed, as a descope rather than a fourth attempt at AC3. The four criteria that remain (AC1, AC2 and the renumbered AC3, AC4) were all met on fresh evidence at review round 3 and nothing this session touched what they measured: no package behavior changed, the only `R/` edit is a source comment, and the harness was re-run unchanged in this session — exit 0, 70 PASS / 0 FAIL / 0 SKIP, every previously reported value identical. What changed is what the repo says: one false NEWS clause corrected and seven small texts fixed. Status to `review`.

## Decisions
<!-- owner: implement / review · append-only -->

## Review
<!-- owner: review · exclusive -->

### Fencing note

The five acceptance-criterion checkboxes arrived at review already ticked by
the implement phase (`git diff origin/master...HEAD` shows all five flipped in
the implement commits). Under AC fencing an already-ticked criterion with no
recorded evidence is unverified, so all five were unticked at the start of this
phase and re-ticked below only as each evidence line was recorded from a fresh
run.

### Acceptance criteria — fresh evidence (2026-08-27)

- **AC1 — met.** `Rscript data-raw/withr-floor.R` re-run from a clean session
  (exit 0). Under the pinned 2.5.0 library, `testthat::test_file()` reports
  35 PASS / 0 FAIL / 0 SKIP across every `test_that()` block of
  `test-local-timeout.R` and `test-with-timeout.R`. The control holds: all four
  child sessions under that library printed `withr actually loaded: 2.5.0`, and
  `DESCRIPTION:29` declares `withr (>= 2.5.0)` — the printed version equals the
  declared floor. The 3.0.3 arm printed 3.0.3 in all four of its sessions and
  also reported 35 PASS / 0 FAIL / 0 SKIP.
- **AC2 — met.** Both named forms measured on 2.5.0 and on 3.0.3 in the same
  run. Rscript top level: limit in force `30`, and after the top level ended
  `30` at `.Last` and `30` at a finalizer registered after withr's — identical
  on both versions. `source()` top level: `30` inside the sourced file, `99`
  (the caller's value) after `source()` returns — identical on both versions.
  Neither form differs between the versions, so the criterion's conditional
  documentation clause is not triggered; the `@details` nonetheless name both
  versions.
- **AC4 — met.** All four documented claims re-measured on the declared floor
  and read true as written: two calls in one frame → `45` inside the frame,
  `99` after it returns (the caller's state, per `@details` "both are undone
  together, back to what the caller had"); a clobbering `on.exit()` without
  `add = TRUE` → `30` after return (the documented hole); a dead
  `.local_envir` → `30` after return (the `@param` claim); a `local_timeout()`
  inside `with_timeout()`'s `expr` → `99` inside the frame once the wrapper
  returned and `30` left behind by the frame, matching "the wrapper's limit is
  what the frame leaves behind". Every value is identical on 3.0.3.
- **AC3 — NOT met.** First half holds: `DESCRIPTION:29` reads
  `withr (>= 2.5.0)`, the version AC1 and AC2 measured green above. The second
  half fails. `NEWS.md`'s withr bullet does not state what was measured against
  that floor; it states more than was measured. "each behavior their
  documentation describes was re-measured on both and agrees" is false —
  `local_timeout()`'s `@details` also claim the limit applies per spawned
  program rather than per frame and that it reaches a `parallel = TRUE`
  fan-out, and neither was run under the floor (AC1's domain was the two
  timeout-wrapper files; `test-parallel-option-carry.R` was not). Its closing
  sentence, "an installation that resolves an older withr is running the
  behavior these pages describe", also has no anchor: read plainly it covers
  versions below 2.5.0, which DESCRIPTION forbids and nothing measured, and
  every version between 2.5.0 and 3.0.3, which D053 itself lists as
  unmeasured. This is a failure inside the criterion's own domain, so it is a
  defect return, not an amendment.
- **AC5 — met.** Fresh runs on the developer's current `withr` (3.0.3):
  `devtools::test()` → 0 failures, 6635 passing, 5 skips, 4 warnings;
  `devtools::check()` → 0 errors, 0 warnings, 0 notes (2m 34s).

### Consistency gate (2026-08-27)

Universal cairn-file checks: `cairn_validate.py` exits 0 — all 16 PASS checks
pass and all 7 advisories read OK, including `coverage complete` and the
`release window` advisory. No `DESIGN.md` principle changed in this diff, so
`cairn_impact.py --changed` does not apply.

Toolchain checks, from the `r-package` profile's `consistency-gate` slot:
`devtools::document()` produces no diff (only the review-owned edits to this
milestone file are dirty afterward); `NAMESPACE`, `man/` and `data/*.rda` are
generated, and the only `man/` change in the diff is `local_timeout.Rd`
regenerated from the roxygen edit; `README.Rmd`/`README.md` are untouched by the
diff and in sync; `pkgdown::check_pkgdown()` reports "No problems found";
`NEWS.md` carries the user-visible entry, with no milestone number in it; the one
new top-level file, `data-raw/withr-floor.R`, sits under the existing
`.Rbuildignore` entry `^data-raw$` (line 15) and `check()` raises no NOTE about
it; `devtools::check()` is clean at 0/0/0.

`R CMD build` emits two pre-existing `Added dependency on R >= …` warnings
(check.txt:18, :24) at the build stage; `Status: OK` and 0 check warnings. They
predate this diff and belong to the absent `Depends: R (>= )` line, which this
milestone put Out of scope and which is already a ROADMAP candidate row.

### Independent fresh-context review (2026-08-27)

Declared tier is user-facing and the diff touches `R/` and a new script, so the
full three-lens fan-out ran, each lens with its own evidence base.

- **[S] blame-history (Sonnet): zero findings.** The roxygen and call-site edits
  are additive — the M073/D052 measurement note "on withr 3.0.3" gains a second
  version rather than being replaced or narrowed — the `defer()`-before-
  `options()` ordering M073's review fixed is untouched, and D053 extends D052
  without reversing it.
- **[S] prior-review record (Sonnet): zero findings.** The `gh` inline-comment
  probe returned nothing, so the PR-thread walk was not paid for. Against
  `M073-timeout-wrapper-tail.md`'s archived `## Review`, none of its six fixed
  findings is reintroduced; the five that are not the ordering fix live in files
  this diff does not touch. No `LESSONS.md` line covers withr or version floors.
- **[O] diff-bug (Opus): eleven findings, ranked.** Verbatim below with their
  dispositions. Findings 1, 2 and 3 were re-verified against the implementation
  rather than against the reviewer's account of it.

**F1 (fix now — verified).** *"D053's 'why there was nothing to find' rests on a
false mechanism claim, repeated in `R/timeout.R`. D053 says `local_timeout()`
'never reaches that branch: … at the top level of a `source()`d file
[`parent.frame()`] is `source()`'s own eval frame rather than `globalenv()`',
and the new comment at `R/timeout.R:255-260` says the same. Measured: at the top
level of a `source()`d file, `identical(parent.frame(), globalenv())` is `TRUE`,
so `withr::defer()` does enter the `identical(envir, globalenv())` branch — the
branch 3.0.0 rewrote. The two versions agree because withr redirects the handler
to `source()`'s frame in both (`source_frame()` via `exit_frame()` in 2.5.0;
`source_exit_frame_option()` in 3.0.3), not because the branch is unreached. The
observation is right; the explanation, the falsifier's scope, and the source
comment are wrong."* — Re-verified here: a function called at the top level of a
`source()`d file sees `parent.frame()` identical to `globalenv()`, so
`local_timeout()`'s default `.local_envir` IS `globalenv()` in that form.

**F2 (fix now — verified).** *"The roxygen edits attribute to withr 2.5.0 a
measurement the committed harness never performs. `R/timeout.R:170-171` and
`:183-184` (and the regenerated `man/local_timeout.Rd`) now say
`withr::defer()`/`withr::local_options()` lose the undo, and
`withr::with_options()`/`withr::local_options()` behave identically, 'on withr
2.5.0 and 3.0.3 alike' — but `data-raw/withr-floor.R` contains no call to any
`withr::` function at all (grep: zero hits); `ac4.R` exercises only
`local_timeout()`/`with_timeout()`. The 3.0.3 half was M073's; the 2.5.0 half is
asserted, not measured, in user-facing documentation — the exact failure mode
the milestone exists to end."* — Re-verified here: `grep -c 'withr::'
data-raw/withr-floor.R` returns 0.

**F3 (fix now — verified).** *"D053: '`is_top_level_global_env()` and
`global_defer()` are 3.x-only' is half false. `is_top_level_global_env()` is
defined in withr 2.5.0 at `withr/R/compat-defer.R:174`, with the same body. Only
`global_defer()` is 3.x-only."* — Re-verified here against the 2.5.0 tarball from
the CRAN archive: `is_top_level_global_env` is at `compat-defer.R:174` and is
called at `:65`.

**F4 (fix now).** *"D053's Rscript-form conclusion states a mechanism that
contradicts withr's code, and the measurement is inconclusive by construction.
D053 says 'there both versions leave the limit set, because there is no frame
left to unwind.' Both versions do schedule an unwind: 3.0.3's `global_defer()`
and 2.5.0's `setup_handlers()` each call `reg.finalizer(globalenv(),
function(env) deferred_run(env), onexit = TRUE)`. The harness's `.Last` and its
own later-registered finalizer simply observe before withr's finalizer runs —
and the harness comment itself concedes the ordering 'is not this package's to
promise'. So 'the limit stays set' is a statement about hook ordering, not about
the absence of an undo, and no observation point in the harness could have shown
the undo running."*

**F5 (fix now — this is the AC3 failure).** *"NEWS overclaims coverage. 'each
behavior their documentation describes was re-measured on both and agrees' —
`local_timeout()`'s `@details` also claim the limit applies per spawned program
rather than per frame, and that it reaches a `parallel = TRUE` fan-out. Neither
was re-measured on 2.5.0: AC1's domain was the two timeout-wrapper files, and
`tests/testthat/test-parallel-option-carry.R` (which exercises
`local_timeout()`) was not run under the floor."*

**F6 (fix now — part of the AC3 failure).** *"NEWS's closing sentence has an
unanchored 'older'. 'So an installation that resolves an older withr is running
the behavior these pages describe' — older than 3.0.3 is meant, but it reads as
older than 2.5.0, which DESCRIPTION forbids and which nothing measured. It also
implies coverage of every version between 2.5.0 and 3.0.3, which D053 correctly
lists as unmeasured."*

**F7 (fix now).** *"The one 3.0.0 break the milestone's own Scope named is never
measured, and D053's 'What was not measured' omits it. Scope flags that
`source()` into a local environment needs `withr.hook_source = TRUE` from 3.0.0
and worked by default at 2.5.0 — but AC2 measured only `source()` with its
default `globalenv()`, precisely the form 3.x auto-detects. Measured here on
3.0.3: a `defer()`-based `local_timeout()` equivalent inside `source(file, local
= TRUE)` called from a function frame loses the limit immediately (the option is
back to the caller's value on the next line of the sourced file), while 2.5.0's
`exit_frame()`/`source_frame()` path would redirect to `source()`'s frame. This
neighbourhood is not obviously benign and is unlisted."* — Fix is to list it in
D053's "What was not measured", or to measure it; AC2's two named forms are not
widened, since criteria are not reinterpreted at review.

**F8 (fix now).** *"The harness pins by precedence, not isolation, and the AC1
control is a human read of stdout. `run_under()` sets only `R_LIBS`, which
prepends; the user library holding 3.0.3 stays on the child's `.libPaths()`
(confirmed). That is necessary (`pkgload`, `testthat` live there), but it means
a pinned install that failed to yield a loadable withr would silently fall
through. The child prints `packageVersion('withr')` — as AC1 requires — but
nothing asserts it equals the requested version, so a future re-run can go wrong
without failing."* — The control held on this run (all eight child sessions
printed their own pinned version), so AC1 stands on this evidence; the fix is a
`stopifnot()` in the harness so a future re-run cannot pass vacuously.

**F9 (fix now, cheap).** *"D053 sits in mild tension with D052 without saying so.
D052 justifies choosing `withr::defer()` partly because 'it also handles a global
or knitr target environment'; D053 now argues that branch is effectively
unreachable from `local_timeout()` and names knitr as untested. Both entries
stand as written, but a reader gets opposite impressions of how load-bearing
that branch is; D053 claims to leave all of D052 standing."* — F1 dissolves most
of this: the branch IS reached, so D052's justification stands unqualified and
D053's rewrite should say so.

**F10 (follow-up, not this milestone).** *"Harness robustness (minor). The floor
regex `sub('.*withr \\(>= ([^)]+)\\).*', ...)` returns the whole `Imports` field
as the 'version' if the `withr` entry ever loses its `(>= )`; `run_under()`
ignores the child's exit status, so a child that failed to `load_all()` prints an
error and the driver continues to the next form; `download.file` failure is
inferred from a 1000-byte size heuristic with warnings swallowed;
`sprintf('source(\"%s\")', inner)` would break on a `tempdir()` containing quotes
or backslashes."* — The exit-status half is worth folding into F8's fix; the rest
is hardening of a `data-raw/` script that never ships.

**F11 (rejected).** *"`pkgload` is a harness dependency not declared anywhere
(minor). `data-raw/` is already in `.Rbuildignore` (so `R CMD check` is
unaffected), but the reproduce line in the harness header assumes `pkgload` is
installed and does not say so."* — Out-of-scope taxonomy: a `data-raw/` script
run by a developer who has devtools installed, with no user-facing surface.

### Gate outcome — returned to `in-progress`

AC3 fails inside its own domain (F5, F6): `NEWS.md` does not state what was
measured against the floor, it states more than was measured. F1, F2 and F3 are
each independently a load-bearing defect in what this milestone ships — the
milestone's whole promise is that the floor and its documentation say what was
measured, and instead `R/timeout.R`'s roxygen credits withr 2.5.0 with a
measurement no committed script performs (F2), while D053 and the call-site
comment explain the null result with a mechanism the code contradicts (F1, F3).
Under the return floor this is a defect return. It is the first return on this
milestone; the thrash rule is not engaged.

Not in question: the measurement itself. AC1, AC2, AC4 and AC5 stand on fresh
evidence, and the empirical result — 2.5.0 and 3.0.3 agree on every point
measured, so the floor stays 2.5.0 — survives every finding above. What fails is
what the repo says about it.

---

## Review — round 2 (after the defect return)
<!-- owner: review · exclusive -->

### Fencing note

All five criterion checkboxes again arrived at review ticked (the implement
phase re-ticked them closing the defect return). Under AC fencing they were
unticked at the start of this round and re-ticked below one at a time, each as
its own fresh evidence line was recorded from a run made in this phase. The
round-1 evidence above is not carried forward: T6 extended the harness, so
every measurement was taken again.

### Acceptance criteria — fresh evidence (2026-08-27, round 2)

- **AC1 — met.** `Rscript data-raw/withr-floor.R` re-run clean, exit 0. Under
  the pinned 2.5.0 library `testthat::test_file()` reports 35 PASS / 0 FAIL /
  0 SKIP across every `test_that()` block of `test-local-timeout.R` and
  `test-with-timeout.R`; the 3.0.3 arm reports the same 35 PASS / 0 FAIL /
  0 SKIP (70 PASS, 0 FAIL, 0 SKIP over the whole log). The control is now an
  assertion, not a printed line: all seven child sessions per version printed
  `withr actually loaded: <ver>` AND passed `stopifnot(loaded ==
  WITHR_EXPECT)`, and `DESCRIPTION:29` reads `withr (>= 2.5.0)` — the pinned
  and loaded version equals the declared floor. Negative control run here: a
  child given a mismatched `WITHR_EXPECT` halts with
  `Error: identical(loaded, Sys.getenv("WITHR_EXPECT")) is not TRUE`, and
  `run_under()` now stops the driver on a non-zero child status, so a future
  re-run cannot pass vacuously (F8, F10-exit).
- **AC2 — met.** Both named forms measured on 2.5.0 and on 3.0.3 in the same
  run. Rscript top level: limit in force `30`; `30` at `.Last` and `30` at a
  finalizer registered after withr's — identical on both. `source()` with its
  default `globalenv()`: `30` inside the sourced file, `99` (the caller's
  value) after `source()` returns — identical on both. Neither form differs
  between the versions, so the criterion's conditional documentation clause is
  not triggered; the `@details` name both versions anyway. The harness now also
  reports what `parent.frame()` is at each of those top levels — `TRUE` for
  `identical(., globalenv())` on both versions and both forms — and probes
  whether an undo was scheduled at the `Rscript` top level:
  `withr::deferred_run(globalenv())` restores the caller's `99` on both, so the
  form's outcome is hook ordering, not a missing undo (F1, F4).
- **AC4 — met.** All four documented claims re-measured on the declared floor
  and read true as written: two calls in one frame → `45` inside, `99` after
  (the caller's state); a clobbering `on.exit()` without `add = TRUE` → `30`
  after return (the documented hole); a dead `.local_envir` → `30` after
  return; a `local_timeout()` inside `with_timeout()`'s `expr` → `99` inside
  the frame once the wrapper returned, `30` left behind by the frame. Every
  value identical on 3.0.3. New this round, the harness also runs the `withr::`
  calls the `@details` compare `local_timeout()` to, so those two sentences now
  stand on measurement rather than assertion (F2): `withr::defer()` +
  clobbering `on.exit()` → `30`, `withr::local_options()` + clobbering
  `on.exit()` → `30`, `withr::defer()` into a dead envir → `30`,
  `withr::local_options()` into a dead envir → `30`,
  `withr::with_options()` + `withr::local_options()` → `99` inside the frame
  and `30` left behind — identical on 2.5.0 and 3.0.3, which is what the
  roxygen now claims.
- **AC3 — NOT met (revised after the fan-out).** First half holds:
  `DESCRIPTION:29` reads `withr (>= 2.5.0)`, the version AC1 and AC2 measured
  green above. The anchoring half of round 1's failure is fixed: the closing
  sentence now reads "an installation that resolves withr 2.5.0 rather than the
  current release", and the two `@details` claims not run on the floor are named.
  But the second half still fails, in two new places, both of the same shape as
  round 1's F5 — NEWS stating other than what was measured. (a) It states MORE:
  "the whole `with_timeout()` and `local_timeout()` test suite passes" is false
  — the domain was two files, 35 blocks, and
  `tests/testthat/test-parallel-option-carry.R` calls `with_timeout()` at
  `:494`, `:517` and `:535` and was not run under the floor. (b) It states LESS:
  the run found exactly one place where the two versions part —
  `source(file, local = TRUE)` from a function frame, `30` inside the sourced
  file on 2.5.0 against `99` on 3.0.3 — which D053 records and NEWS omits
  entirely, while its closing sentence implies the versions agreed on
  everything looked at. This is a failure inside the criterion's own domain, so
  it is a defect return, not an amendment.

- **AC5 — met.** Fresh runs on the developer's current `withr` (3.0.3):
  `devtools::test()` → 0 failures, 6635 passing, 5 skips, 4 warnings;
  `devtools::check()` → `Status: OK`, 0 errors / 0 warnings / 0 notes
  (2m 29.6s).

### Consistency gate (2026-08-27, round 2)

Universal cairn-file checks: `cairn_validate.py` exits 0 — every PASS check
passes and every advisory reads OK, `coverage complete` and `release window`
included. No `DESIGN.md` principle changed in this diff, so
`cairn_impact.py --changed` does not apply.

Toolchain checks, from the `r-package` profile's `consistency-gate` slot:
`devtools::document()` produces no diff (working tree clean afterward);
`NAMESPACE`, `man/` and `data/*.rda` are generated, and the only `man/` change
in the diff is `local_timeout.Rd` regenerated from the roxygen edit;
`README.Rmd`/`README.md` are untouched by the diff and in sync;
`pkgdown::check_pkgdown()` reports "No problems found"; `NEWS.md` carries the
user-visible entry with no milestone number in it; the one new top-level file,
`data-raw/withr-floor.R`, sits under the existing `.Rbuildignore` entry
`^data-raw$` (line 15) and `check()` raises no NOTE about it; `devtools::check()`
is clean at 0/0/0.

`R CMD build` still emits the two pre-existing `Added dependency on R >= …`
warnings (check.log:18, :24) at the build stage; the check itself is `Status: OK`
with 0 warnings. They predate this diff, belong to the absent
`Depends: R (>= )` line that this milestone put Out of scope, and are already on
a ROADMAP candidate row.

### Independent fresh-context review (2026-08-27, round 2)

Declared tier is user-facing and the diff touches `R/` and a script, so the full
three-lens fan-out ran again, each lens with its own evidence base and none
having seen the implementation.

- **[S] blame-history (Sonnet): zero findings.** The `withr::defer()`-before-
  `options()` ordering M073 fixed (`4bf0b7a`) is intact at `R/timeout.R:265-266`;
  the M073-authored measurement sentences gain a second version clause rather
  than being replaced; D052 is untouched and D053's rewrite resolves rather than
  deepens the tension with it; `man/local_timeout.Rd` is a mechanical
  regeneration.
- **[S] prior-review record (Sonnet): zero findings.** The `gh` inline-comment
  probe returned `[]`, so the PR-thread walk was not paid for. Against the
  archived `## Review` sections of M071/M072/M073 nothing is reintroduced, and
  no `LESSONS.md` line covers withr, version floors, or
  documentation-vs-measurement. It traced each of round 1's F1-F9 to a real
  change rather than a rewording.
- **[O] diff-bug (Opus): eleven findings, ranked.** Verbatim below with
  dispositions. G1, G2, G3 and G4 were each re-verified here against the
  implementation and against withr's own source, not against the reviewer's
  account of them.

**G1 (defect return — verified).** *"D053 and the new call-site comment replace
F1's wrong mechanism with a different wrong mechanism: the rewritten
`globalenv()` branch is NOT reached from the `source()` form. `parent.frame() ==
globalenv()` is not the same fact as 'defer() takes the branch 3.0.0 rewrote',
and only the first was measured. In 3.0.3's `defer()`, the
`identical(envir, globalenv())` block calls `source_exit_frame_option()` FIRST
and only falls through to `global_defer()` when that is `NULL`; inside a
`source()` the redirect wins. On 2.5.0 the source form does not touch a globalenv
branch at all: `add_handler()` runs `exit_frame()` → `source_frame()`
(`compat-defer.R:40, 104, 127`) and redirects to `source()`'s frame before
`setup_handlers()` is consulted, where `is_top_level_global_env()` is then
`FALSE`. So the source-form agreement between versions is caused by the
source-frame redirect present in both — exactly what the prior F1 said — not by
the rewritten branch being reached. This is the milestone's own failure mode — a
mechanism asserted past what the harness measured — in the very entry written to
end it."* — Re-verified here three ways. (i) `deparse(withr::defer)` on 3.0.3
shows `source_exit_frame_option()` consulted before `is_top_level_global_env()`
inside the `globalenv()` block, with `global_defer()` reached only when the
former is `NULL`. (ii) The 2.5.0 tarball from the CRAN archive shows
`add_handler()` calling `exit_frame()` before `set_handlers()`/`setup_handlers()`
(`compat-defer.R:35-49`), so `setup_handlers()` receives `source()`'s frame and
`is_top_level_global_env()` (`:172-180`) returns `FALSE`. (iii) Empirically on
3.0.3: `length(withr:::the$global_exits)` is `0` inside and after a `source()`d
file and `1` after the same call at an `Rscript` top level. D053's "The mechanism
changed; what these forms observe did not" paragraph and `R/timeout.R:259-262`
both say the branch is reached from both forms; it is reached from one.

**G2 (defect return — verified; part of the AC3 failure).** *"NEWS overstates the
suite that was run. `NEWS.md`: 'the whole `with_timeout()` and `local_timeout()`
test suite passes'. The domain was two files (`test-local-timeout.R`,
`test-with-timeout.R`, 16 + 19 = 35 blocks).
`tests/testthat/test-parallel-option-carry.R` (3 uses) also exercises these
functions and was not run under the floor — as the bullet's own next sentence
concedes when it names the parallel fan-out claim as unmeasured. This is F5's
overclaim re-entering the rewritten bullet in a new place."* — Re-verified:
`test-parallel-option-carry.R` calls `with_timeout()` at `:494`, `:517`, `:535`.

**G3 (defect return — verified; part of the AC3 failure).** *"NEWS never tells the
reader that a version difference WAS found. D053 records `source(file, local =
TRUE)` from a function frame as the one measured place where 2.5.0 and 3.0.3 part
(30 vs 99 inside the sourced file), and the user-facing note omits it entirely
while implying the versions agree on everything looked at. The milestone's
promise is 'no more and no less', and this is the 'less' half."* — Re-verified:
the fresh run reproduces the 30-vs-99 split, and the NEWS bullet contains no
mention of it.

**G4 (fix now — verified).** *"The AC1 pin control still cannot fail for the 3.0.3
arm on the developer's machine, so a future re-run can be a false green.
`install_withr()` never checks that `install.packages()` produced anything, and
the pin is by `R_LIBS` precedence with the user library still on `.libPaths()`.
The user library here holds withr 3.0.3, so if the 3.0.3 install silently fails
the child loads the user copy, `loaded` is `\"3.0.3\"`, the assertion passes, and
the arm reports green for an unpinned library. The assertion closes the hole only
for the floor arm. Correct fix: assert provenance, not version — e.g.
`stopifnot(identical(normalizePath(dirname(find.package(\"withr\"))),
normalizePath(Sys.getenv(\"WITHR_LIB\"))))` — and/or
`stopifnot(dir.exists(file.path(lib, \"withr\")))` inside `install_withr()`."* —
AC1 itself still stands: its control is over the FLOOR arm, and 2.5.0 is not the
user library's version, so that arm cannot fall through. The hole is real for the
3.0.3 comparison arm.

**G5 (fix now).** *"A failing `test_that()` block does not fail the harness run.
`suite.R` prints per-block verdicts and exits 0 regardless, and `run_under()`
only stops on a non-zero exit status, so AC1's 'zero failures' verdict rests
entirely on a human reading 35 lines of stdout. F8's fix hardened the pin but not
the result. Correct: accumulate the verdicts and `stop()` if any block is
`FAIL`."*

**G6 (fix now, cheap).** *"NEWS's enumeration reads as six measurements where four
were made. 'the four things `?local_timeout` says about when the undo runs and
the two ways it can be lost were re-measured on each and agree' — the 'two ways
it can be lost' ARE two of AC4's four claims; listing them additively implies a
sixth and fifth measurement."*

**G7 (fix now, cheap).** *"The ROADMAP row presents an inventory of the harness's
remaining rough edges as complete, and it is not. 'which M074's review left three
rough edges in after the exit-status half was fixed: … None is reachable in the
single-package form M074 ran'. Findings 2 and 3 above are both reachable in
exactly the form M074 ran, and neither is listed."*

**G8 (fix now, cheap).** *"D053 misdescribes where the harness fetches from.
'`data-raw/withr-floor.R` installs a given `withr` from the CRAN archive into its
own library' — the script tries the Archive URL first and then falls back to
`https://cloud.r-project.org/src/contrib/` (`:59-62`), which is where the current
release actually comes from; 3.0.3 is not in the Archive, so the 3.0.3 arm never
used the path D053 names."*

**G9 (follow-up).** *"`install_withr()`'s reuse short-circuit trusts a directory's
existence (`:57`). A half-written install directory is silently accepted as a
good one. Unreachable today because `LIBROOT` lives under `tempdir()` and is
fresh per run, so this is only a trap for whoever later persists the library
root."* — Folds into G4's fix or the candidate row.

**G10 (rejected).** *"The dead-`.local_envir` claim is verified only by its
observable, and the harness cannot tell 'undo lost' from 'undo ran immediately'
— the exact conflation F4 fixed for the `Rscript` form. `@param .local_envir`
states the mechanism ('takes the undo with it')."* — Out-of-scope taxonomy: the
`@param` wording at `R/timeout.R:143-145` is pre-existing and untouched by this
diff. Probed here anyway — the option reads `99` immediately after the
`defer()` into a dead envir and `30` after the write, so nothing observed
contradicts the documented text; the reviewer is right that the two mechanisms
are indistinguishable from outside, which is a doc-precision question about a
line this milestone did not write.

**G11 (rejected).** *"Convention (low confidence): D053 is ~60 lines against
`cairn/DECISIONS.md`'s 'One short entry each' rule, and the milestone file's
`## Decisions` section is empty although T4 promoted D053."* — The reviewer
withdrew the first half itself (D052 above sets the precedent), and the empty
`## Decisions` section is template scaffolding that `cairn_validate.py` passes
on; the archived M072/M073 files carry it empty too.

### Gate outcome — returned to `in-progress` (second defect return)

AC3 fails again inside its own domain: `NEWS.md` still does not state what was
measured against the floor — it states more (G2: "the whole test suite", when
one of the three files exercising these functions was not run) and it states
less (G3: the one place the two versions were measured to part is missing).
G1 is independently a load-bearing defect in what ships: `R/timeout.R:259-262`
and D053's central mechanism paragraph assert that `local_timeout()` reaches the
`defer()` branch withr 3.0.0 rewrote from both top-level forms, and it reaches it
from one. Round 1 fixed the claim that the branch is never reached by asserting
that it always is; both go past what the harness measured, which is
`parent.frame() == globalenv()`.

Not in question, again: the measurement. AC1, AC2, AC4 and AC5 all stand on
fresh evidence from a re-run of the extended harness, and the empirical
result — 2.5.0 and 3.0.3 agree on every documented claim, and part only at
`source(local = TRUE)` — survives every finding above. What fails is still what
the repo says about it.

**Thrash rule — trigger (b) fires.** This is the second defect return, so
trigger (a) (the third return) has not fired. But AC3 has now failed twice, each
time by a new mechanism of the same shape: round 1 by an overclaim
("each behavior their documentation describes was re-measured on both") plus an
unanchored range, round 2 by an overclaim ("the whole test suite") plus an
omission. Re-cutting around the same predicate buys the next mechanism, not a
fix. The remedy is the alternative the plan gate recorded against, and this
milestone's work log records one directly on point (2026-08-27): *"plan gate
chose the two timeout-wrapper test files as AC1's domain over the whole suite
under old withr because the rest of the suite's withr use is Suggests-side and
says nothing about what a user installing tidymedia gets."* G2 is exactly the
cost of that choice — `test-parallel-option-carry.R` is not Suggests-side withr
use, it is a third file exercising `with_timeout()`, and every honest NEWS
sentence about the floor has to carve it out. Widening AC1's domain to include
it would let NEWS say plainly what it keeps trying to say. That widening is a
criterion amendment and belongs at `/milestone-implement` step 6's gate, which
is the user's decision, not review's.

---

## Review — round 3 (after the second defect return)
<!-- owner: review · exclusive -->

### Fencing note

All five criterion checkboxes again arrived at review ticked (the implement
phase re-ticked them closing the second defect return). Under AC fencing they
were unticked at the start of this round and re-ticked below one at a time,
each as its own fresh evidence line was recorded from a run made in this phase.
Round 2's evidence is not carried forward: T10 added an arm to the harness, so
every measurement was taken again.

### Acceptance criteria — fresh evidence (2026-08-27, round 3)

- **AC1 — met.** `Rscript data-raw/withr-floor.R` re-run from a clean session,
  exit 0. Under the pinned 2.5.0 library `testthat::test_file()` reports 35
  PASS / 0 FAIL / 0 SKIP across every `test_that()` block of
  `test-local-timeout.R` (16 blocks) and `test-with-timeout.R` (19); the 3.0.3
  arm reports the same 35, for 70 PASS / 0 FAIL / 0 SKIP over the whole log.
  The control is an assertion on provenance, not a printed version: each of the
  eight child sessions per version printed both `withr actually loaded: <ver>`
  and `withr loaded from: <the pinned library path>`, and passed
  `stopifnot(identical(loaded, WITHR_EXPECT))` plus
  `stopifnot(identical(normalizePath(dirname(find.package("withr"))),
  normalizePath(WITHR_LIB)))`. `DESCRIPTION:29` reads `withr (>= 2.5.0)` — the
  pinned, loaded and located version equals the declared floor.
- **AC2 — met.** Both named forms measured on 2.5.0 and on 3.0.3 in the same
  run. `Rscript` top level: limit in force `30`; `30` at `.Last` and `30` at a
  finalizer registered after withr's — identical on both. `source()` with its
  default `globalenv()`: `30` inside the sourced file, `99` (the caller's value)
  after `source()` returns — identical on both. Neither form differs between the
  versions, so the criterion's conditional documentation clause is not
  triggered; the `@details` name both versions anyway. Supporting arms unchanged
  from round 2 and re-measured here: `parent.frame()` is `globalenv()` at both
  top levels on both versions (`TRUE`), `deferred_run(globalenv())` restores the
  caller's `99` at the `Rscript` top level on both, and the `formB-where` arm
  reports `30` — the redirect, not a globalenv registration — inside a
  `source()`d file on both.
- **AC4 — met.** All four documented claims re-measured on the declared floor
  and read true as written: two calls in one frame → `45` inside, `99` after
  (the caller's state); a clobbering `on.exit()` without `add = TRUE` → `30`
  after return (the documented hole); a dead `.local_envir` → `30` after return;
  a `local_timeout()` inside `with_timeout()`'s `expr` → `99` inside the frame
  once the wrapper returned, `30` left behind by the frame. Every value
  identical on 3.0.3. The harness also runs the `withr::` calls the `@details`
  compare `local_timeout()` to, so those two sentences stand on measurement:
  `withr::defer()` + clobbering `on.exit()` → `30`, `withr::local_options()` +
  clobbering `on.exit()` → `30`, `withr::defer()` into a dead envir → `30`,
  `withr::local_options()` into a dead envir → `30`, `withr::with_options()` +
  `withr::local_options()` → `99` inside the frame and `30` left behind —
  identical on 2.5.0 and 3.0.3, which is what the roxygen claims.
- **AC5 — met.** Fresh runs on the developer's current `withr` (3.0.3):
  `devtools::test()` → `[ FAIL 0 | WARN 4 | SKIP 5 | PASS 6635 ]`, exit 0;
  `devtools::check()` → `Status: OK`, 0 errors / 0 warnings / 0 notes
  (22m 12.9s).

### Consistency gate (2026-08-27, round 3) — FAILS

Universal cairn-file checks: **`cairn_validate.py` exits 1.** One check FAILs:

```
FAIL  weight caps (1)
        cairn/milestones/M074-withr-version-floor.md: 157 plan-owned lines
        (cap <150; shed >=8)
        heaviest first: Tasks 66 · Scope 34 · Acceptance criteria 30 ·
        Coverage 9 · Goal 7
```

Every other PASS check passes and every advisory reads OK except one:
`sizing (split tripwires)` warns `M074: 13 tasks (>10 tripwire)`, which is an
advisory rather than a gate failure. The `release window` advisory reads OK.
No `DESIGN.md` principle changed in this diff, so `cairn_impact.py --changed`
does not apply.

The failure is not caused by this review round's edits: the Review section is
review-owned and does not count toward the plan-owned total, and the run above
was made before any edit in this phase. It is the accumulated cost of two
defect returns — the Tasks section grew from T1-T5 to T1-T13 — and review
cannot shed it, because Tasks, Scope, Acceptance criteria, Coverage and Goal
are all plan-owned sections that a review phase never rewrites.

Toolchain checks, from the `r-package` profile's `consistency-gate` slot — all
pass: `devtools::document()` produces no diff (only this review-owned file is
dirty afterward); `NAMESPACE`, `man/` and `data/*.rda` are generated, and the
only `man/` change in the diff is `local_timeout.Rd` regenerated from the
roxygen edit; `README.Rmd`/`README.md` are untouched by the diff and in sync;
`pkgdown::check_pkgdown()` reports "No problems found"; `NEWS.md` carries the
user-visible entry with no milestone number in it; the one new top-level file,
`data-raw/withr-floor.R`, sits under the existing `.Rbuildignore` entry
`^data-raw$` (line 15) and `check()` raises no NOTE about it;
`devtools::check()` is `Status: OK` at 0/0/0.

### Acceptance criteria — AC3 (2026-08-27, round 3)

- **AC3 — NOT met.** First half holds: `DESCRIPTION:29` reads
  `withr (>= 2.5.0)`, the version AC1 and AC2 measured green above. Round 2's
  two failures are genuinely fixed — NEWS now names the two files and 35 blocks
  rather than "the whole test suite", and it states the one measured version
  difference. The second half fails a third time, in a new place. NEWS's
  disclosure sentence reads: *"Three things were not run on 2.5.0: the claim
  that the limit applies per spawned program, the claim that it reaches a
  `parallel = TRUE` fan-out — their tests live outside the two files above —
  and every withr between 2.5.0 and 3.0.3."* The per-spawned-program half is
  false on both counts. Its tests do not live outside the two measured files:
  `test-with-timeout.R` carries "each spawn site is handed the per-call limit"
  (`:255`), "`ffm_batch()`'s up-front limit check reads the per-call value"
  (`:279`), "no process `tm_release_fifo()` starts outlives the frame" (`:432`)
  and "a per-call limit kills a hung program with no session limit set"
  (`:487`), the last of which drives a real FFmpeg process to its limit. All
  four ran under the pinned 2.5.0 library and all four PASSED (harness log
  lines 55-58, with 0 SKIP over the 35 blocks). So NEWS says a claim was not
  measured on the floor when it was, and locates its tests in a file where they
  are not. The `parallel = TRUE` half of the same sentence is correct —
  `grep -n parallel` over both measured files returns nothing. This is a
  failure inside the criterion's own domain and its repair widens no
  enumeration, so it is a defect return, not an amendment.

### Independent fresh-context review (2026-08-27, round 3)

Declared tier is user-facing and the diff touches `R/` and a script, so the full
three-lens fan-out ran again, each lens with its own evidence base and none
having seen the implementation. The [O] lens had to be relaunched twice — the
first run stalled on a stream watchdog, the second died to a machine-sleep API
error — and the reported run is the third, given the harness's captured output
rather than being asked to re-run it.

- **[S] blame-history (Sonnet): zero defects.** M073's
  `withr::defer()`-before-`options()` write ordering is intact at the call site,
  untouched by this branch's comment-only edit above it; `git diff` shows D053
  purely appended with no line of D052 changed, and D053's "stands unqualified"
  reading of D052 is accurate now that the rewritten branch is reached from a
  real call path. Its one non-finding was an observation that this round's
  Review section was not yet written when it looked — it was reading the
  implement→review handoff, which is this phase.
- **[S] prior-review record (Sonnet): zero findings.** The
  `gh api .../pulls/comments` probe returned `[]`, so the PR-thread walk was not
  paid for. Against the archived `## Review` sections of M069-M073 and against
  this milestone's own two rounds, nothing is reintroduced: it traced each of
  G1-G5 and G7 to a real change rather than a rewording, and found no
  `LESSONS.md` line covering withr, version floors, or
  documentation-vs-measurement.
- **[O] diff-bug (Opus): nine findings, ranked.** Verbatim below with
  dispositions. O1, O3 and O6 were re-verified here against the implementation
  rather than against the reviewer's account of them.

**O1 (defect return — verified; the AC3 failure).** *"`NEWS.md:92-94` and
`cairn/DECISIONS.md:2362-2364` — 'their tests live outside the two files.'
NEWS: 'the claim that the limit applies per spawned program, the claim that it
reaches a `parallel = TRUE` fan-out — their tests live outside the two files
above'; D053 repeats it verbatim. `grep -rl local_timeout tests/testthat/`
returns only `test-local-timeout.R`, `test-with-timeout.R` and
`helper-timeout-probes.R` — no test outside the two measured files exercises
`local_timeout()` at all, so the asserted elsewhere-tests do not exist; worse,
the nearest tests of the per-spawned-program semantics (`each spawn site is
handed the per-call limit`, `ffm_batch()'s up-front limit check reads the
per-call value`, `a per-call limit kills a hung program…`) are inside
`test-with-timeout.R` and are shown passing on 2.5.0 at harness-r3.log:55-58,
i.e. a thing NEWS says was 'not run on 2.5.0' was run. This is a new
unsupported claim introduced by the round-2 fix for G2/G3, in the same AC3 slot
both returns fell on."* — Re-verified here: the four blocks are at
`test-with-timeout.R:255`, `:279`, `:432` and `:487`; the harness log shows all
four PASS under the pinned 2.5.0 library with 0 SKIP; and `grep -n parallel`
over both measured files returns nothing, so only the per-spawned-program half
of the sentence is wrong.

**O2 (fix now — verified true, wrongly sourced).** *"`cairn/DECISIONS.md` and
`R/timeout.R` — withr-internals mechanism stated as measurement. D053:
'withr redirects the handler to `source()`'s own frame first, and both versions
do it, by different routes (3.0.3 consults `source_exit_frame_option()` before
reaching `global_defer()`; 2.5.0 runs `exit_frame()`/`source_frame()` before
`setup_handlers()` is reached at all)' and 'Only `global_defer()` is new in
3.x; `is_top_level_global_env()` is already in 2.5.0 (`compat-defer.R:174`,
called at `:65`)'; the call-site comment carries the short form.
`data-raw/withr-floor.R` never reads withr's sources or internals — its own
comment says the probe 'discriminates the two without reaching into either
version's internals, which differ' — and the log measures only 30 vs 99 after
`deferred_run(globalenv())`, which cannot distinguish 'redirected to
`source()`'s frame' from any other reason nothing is on `globalenv()`. Under
the milestone's stated promise, this is the same class of defect as F1/G1, now
sourced from the reviewer's own deparse rather than the harness."* — The claims
are TRUE: round 2's Review section records them verified three ways against
`deparse(withr::defer)` on 3.0.3, against the 2.5.0 tarball from the CRAN
archive, and against `length(withr:::the$global_exits)`. What the finding gets
right is provenance: nothing committed on this branch reproduces that reading,
so a text whose whole point is "this was measured" cites an unrecorded source.
The fix is to say in D053 where the internals reading came from, not to retract
it.

**O3 (fix now, cheap — verified).** *"`cairn/ROADMAP.md` — the rough-edge
inventory does not add up. 'which M074 left four rough edges in after its two
reviews. Two of the five its reviews raised were reachable … and were fixed
there' — two fixed plus the four it then lists is six, not five; and three were
actually fixed, since F10's fourth item ('`run_under()` ignores the child's
exit status') was folded into F8's fix and is now `run_under()`'s `stop()` at
`data-raw/withr-floor.R:102-105`, unlisted in either half. G7 faulted this row
for presenting an incomplete inventory as complete; the replacement is
internally inconsistent."* — Re-verified: the arithmetic is off, and
`run_under()` does `stop()` on a non-zero child status.

**O4 (fix now, cheap).** *"`NEWS.md` — AC2's measurement is absent from NEWS.
AC3 as written requires NEWS to state the floor 'and what was measured against
it'; the bullet reports the 35 test blocks (AC1), the four documented claims
(AC4) and the `source(local = TRUE)` split, but never mentions the two
top-level forms that AC2 names, which are the criterion most specific to the
floor. Read uncharitably, that is the 'less' half of the promise again, in a
place the two prior returns did not cover."* — Not counted as the AC3 failure
on its own: AC3 asks NEWS to state what was measured, not to enumerate every
arm. But the two forms are the measurement AC2 exists for, and a sentence
naming them is cheap.

**O5 (fix now, cheap).** *"`NEWS.md` and `cairn/DECISIONS.md` — 'for the rest
of the sourced file.' 'withr 2.5.0 keeps the limit in force for the rest of the
sourced file' — `formC-inner.R` reads the option once, on the line immediately
after `local_timeout(30)`, and never again inside the file; the log shows a
single value. The direction of the split is right; 'for the rest of the file'
is an extrapolation from one observation point."*

**O6 (fix now, cheap — verified).** *"`cairn/DECISIONS.md` — where 3.0.3 came
from. 'the Archive for a retired version, the current `src/contrib` directory
for the release, which is where 3.0.3 comes from' — `install_withr()` tries the
Archive URL first for every version and records nothing about which URL
succeeded; the log prints no provenance for the download, only for the load.
G8 asked for this sentence to be corrected; the correction went one step
further than the script records."* — Re-verified: `install_withr()` loops over
Archive then `cloud.r-project.org/src/contrib` for every version and breaks on
the first success without recording which.

**O7 (fix now, cheap).** *"`NEWS.md` — 'Three things were not run on 2.5.0.' A
closed count, while D053's own 'What was not measured' paragraph lists the
`knitr` target environment as untested too and names it in the falsifier. The
two texts enumerate the unmeasured set differently."*

**O8 (fix now).** *"`NEWS.md` — 'which no claim on that page covers.' Not
something the harness measures, and arguably contradicted by the page itself:
`?local_timeout`'s description promises a limit 'for the remainder of the
function you call this from', and on 3.0.3 the limit is gone on the next line
inside a `source(local = TRUE)`d file while the enclosing frame is still
running. It is a judgment call presented in the same voice as the
measurements."*

**O9 (rejected).** *"AC3's box is still `- [ ]` while the ROADMAP row now reads
`review`; a bookkeeping mismatch, not a claim defect."* — That is AC fencing
working as designed: review unticks every criterion at phase start and re-ticks
only against a recorded evidence line. AC3 is unticked because it is unmet.

### Gate outcome — returned to `in-progress` (third defect return)

Two independent failures, either of which returns the milestone on its own.

**The consistency gate fails.** `cairn_validate.py` exits 1 on `weight caps`:
the milestone file carries 157 plan-owned lines against a cap of 150, Tasks
alone accounting for 66. This is not a claim defect — it is the accumulated
bookkeeping cost of two defect returns, which grew Tasks from T1-T5 to T1-T13 —
but it is a mechanical gate failure, and review cannot shed it, because every
section it names is plan-owned.

**AC3 fails a third time, inside its own domain.** NEWS states that the
per-spawned-program claim was not run on 2.5.0 and that its tests live outside
the two measured files. Both halves are false: four blocks testing exactly that
claim sit in `test-with-timeout.R`, one of the two files, and all four passed
under the pinned floor library. Rounds 1 and 2 failed on NEWS claiming more
coverage than it had; this round fails on NEWS claiming less, and misplacing
the tests while it does. The failure sits in the sentence the round-2 fix
wrote.

Not in question, a third time: the measurement. AC1, AC2, AC4 and AC5 all stand
on fresh evidence from a re-run of the harness — 70 PASS / 0 FAIL / 0 SKIP,
`devtools::test()` 0 failures / 6635 passing, `devtools::check()` `Status: OK`
at 0/0/0 — and the empirical result is unchanged and unchallenged: 2.5.0 and
3.0.3 agree on every documented claim and part only at `source(local = TRUE)`,
so the floor stays 2.5.0. What fails is still, and only, what the repo says
about it.

**Thrash rule — trigger (a) fires, and (b) fires again.**

(a) This is the third defect return. It is a threshold, not a moment: no
further retry is queued under the current plan. The recommended disposition is
descope-or-park.

(b) AC3 has now failed three times, each by a new mechanism of the same shape —
round 1 an overclaim plus an unanchored range, round 2 an overclaim plus an
omission, round 3 an understatement plus a misplaced test location. The
alternative the plan gate recorded against — widening AC1's domain beyond the
two timeout-wrapper files — was already put to the user at round 2's thrash
gate and deliberately held (work log, 2026-08-27). That remedy is therefore
spent, and what remains of (b) is the `/milestone-brief` escalation, offered
per instance rather than automatically.

Where they compose, (a) governs the disposition and (b)'s escalation offer
rides into it. The work log records no re-plan or split spent on this
milestone, so a same-objective `/milestone-plan` re-cut remains a present
option — never the recommended one, since both downstream lineages on record
show a re-cut buying further returns rather than a fix.

Worth noting for whichever disposition is chosen: the three texts are close.
O1 is one false clause in one NEWS sentence, and O2-O8 are seven small
corrections to sentences that are otherwise supported. Nothing found in three
rounds has touched the measurement, the floor, or the shipped runtime.
