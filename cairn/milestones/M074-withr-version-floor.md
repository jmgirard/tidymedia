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
- [ ] T8 — (defect return) Fix `local_timeout()`'s roxygen and the call-site
      comment at `R/timeout.R:255-260` to say what T6 measured (F2, F1);
      `devtools::document()`.
- [ ] T9 — (defect return) Rewrite `NEWS.md`'s withr bullet to state exactly
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
