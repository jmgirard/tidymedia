<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M074: The floor says what was measured

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate -->
- **Principles touched:** —   <!-- owner: plan · create/amend-via-gate -->
- **Branch/PR:** `m074-withr-version-floor`   <!-- owner: implement (branch) / review (PR URL) · create -->

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
- [x] AC3 — DESCRIPTION's `withr` floor names a version on which AC1 and AC2
      were measured green, and `NEWS.md` states that floor and what was measured
      against it.
- [x] AC4 — Each of the four behavioral claims `local_timeout()`'s documentation
      makes — two calls in one frame unwind to the caller's state; a frame's own
      `on.exit()` without `add = TRUE` discards the undo; a `.local_envir` that
      is not a live frame takes the undo with it; a `local_timeout()` written
      directly inside `with_timeout()`'s `expr` outlives the wrapper — reads
      true when re-measured on the declared floor version, or the documentation
      names the versions on which it holds.
- [x] AC5 — `devtools::check()` is clean (0 errors / 0 warnings) and
      `devtools::test()` passes on the developer's current `withr`.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1, T2
- AC2 → T3
- AC3 → T5
- AC4 → T3, T5
- AC5 → T5

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] T1 — Build the harness: a scratch script that installs a given `withr`
      version from the CRAN archive into an isolated `.libPaths()` entry and
      evaluates an expression under it. withr's only Imports are `graphics` and
      `grDevices`, so each install is self-contained. The harness prints
      `packageVersion("withr")` from inside the evaluating session; verify it
      reports the requested version and not the user library's 3.0.3 before
      trusting any result from it.
- [x] T2 — Run `test-local-timeout.R` and `test-with-timeout.R` under withr
      2.5.0, recording pass/fail per `test_that()` block by name. If any block
      fails, walk upward — 2.5.1, 2.5.2, 3.0.0, 3.0.1, 3.0.2, 3.0.3 — to the
      lowest version on which every block passes, recording the results at each
      step rather than only the endpoints.
- [x] T3 — Measure AC2's two top-level forms, and AC4's four documented claims,
      on 2.5.0 and on 3.0.3; record the differences. `local_timeout()` reaches
      `withr::defer()`'s `globalenv()` branch only from these forms, and withr's
      2.5.0 NEWS claims globalenv unwinding that 3.0.3 routes through
      `is_top_level_global_env()`/`global_defer()` instead — the point is which
      of the two the caller actually observes.
- [x] T4 — Settle the floor from T2/T3 and write the D-entry: the version, the
      behavior that forced it (or the measurement that permits keeping 2.5.0),
      what was and was not tested, and the entry's own falsifier.
- [x] T5 — Update `DESCRIPTION`, `NEWS.md`, and any version-dependent wording in
      `local_timeout()`'s roxygen at `R/timeout.R:120-200`; run
      `devtools::document()`, then `devtools::check()` and `devtools::test()`.

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

## Decisions
<!-- owner: implement / review · append-only -->

## Review
<!-- owner: review · exclusive -->
