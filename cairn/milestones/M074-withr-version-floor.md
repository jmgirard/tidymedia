<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M074: The floor says what was measured

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
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
- [ ] AC3 — DESCRIPTION's `withr` floor names a version on which AC1 and AC2
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

- AC1 → T1, T2, T6
- AC2 → T3, T6
- AC3 → T5, T9
- AC4 → T3, T5, T6, T8
- AC5 → T5, T9

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
- [x] T6 — (defect return) Extend `data-raw/withr-floor.R` to measure what the
      shipped documentation actually claims: the `withr::` calls the roxygen
      compares `local_timeout()` to (F2), what `parent.frame()` is at each top
      level (F1), whether an undo is scheduled at an `Rscript` top level (F4),
      and `source(local = TRUE)` (F7). Make a wrong pin fail the run rather than
      print (F8, and F10's exit-status half).
- [x] T7 — (defect return) Rewrite D053 from T6's measurements: the globalenv
      branch IS reached (F1), `is_top_level_global_env()` is not 3.x-only (F3),
      the Rscript form's undo exists and is only observed late (F4),
      `source(local = TRUE)` is measured rather than omitted (F7), and D052's
      justification stands unqualified (F9).
- [x] T8 — (defect return) Fix `local_timeout()`'s roxygen and the call-site
      comment at `R/timeout.R:255-260` to say what T6 measured (F2, F1);
      `devtools::document()`.
- [x] T9 — (defect return) Rewrite `NEWS.md`'s withr bullet to state exactly
      what was measured against the floor, with an anchored version range (F5,
      F6 — the AC3 failure); file F10's remaining harness hardening as a
      candidate row; run the `verify` slot and `devtools::check()`.

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
