# M081: The unchecked flag, and the second copy of the one predicate

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Close the three of M080's four residual shipped-predicate findings that are
real, and bound the first one's class by a walk rather than a hand-list.

## Scope

Surface tier: **user-facing** — the deliverable is shipped front-door guard
predicates whose abort wording callers read. IP1 is untouched: D037 scopes it
to command assembly, not validation.

**In:** (a) the flag guards that branch on an unchecked required formal, found
by a parsed-body walk rather than named in a list, and repaired so `NA` gets a
cli/rlang refusal instead of a bare `simpleError`; (b) the second `file.access`
call in `check_batch_inputs()`, folded into one non-aborting predicate both it
and `check_paths_readable()` reach, which is D041's rule applied to the filter
as well as the abort; (c) the comment over `reject_duplicate_inputs()`, which
promises a later multi-input verb inherits a wording that would be wrong for it.

**Out:** parameterizing `reject_duplicate_inputs()` by carrier column — the plan
gate rejected it as wrong, not merely unused (see the work log and D059); it
does not live anywhere else. Also out: M080's F10, which the criteria audit
found is not a defect — `rlang::caller_arg()` resolves at the guard's own frame,
so the abort already reads `` `reencode` ``. F10 is retired, not deferred, and
the candidate row's claim about it is corrected in the same commit.

## Acceptance criteria

- [ ] AC1: A walk over the installed namespace's parsed function bodies returns
      no `check_*` predicate that applies `!`, `&&`, or `||` to a required
      formal it has not first passed to `rlang::check_bool()`. Membership is
      decided by the walk over the namespace, never by a list edited to add or
      exempt a predicate. The walk carries positive controls: it flags a planted
      predicate that branches on a bare required formal, in each of the three
      operator forms, and does not flag one that checks the formal first.
- [ ] AC2: Every predicate AC1's walk flags on the merge-base ref —
      `check_audio_codec_needs_reencode()` (`R/ffmpeg.R:2849`) and
      `check_resize_needs_two_inputs()` (`R/ffmpeg.R:2898`) — signals an
      rlang/cli condition naming its own flag formal, for each of the four types
      `na_values()` declares (`NA`, `NA_integer_`, `NA_real_`, `NA_character_`),
      where the merge-base signals a bare `simpleError`.
- [ ] AC3: `file.access` is called from exactly one function of the installed
      namespace, decided by `tm_call_graph()`'s parsed-call-node walk and never
      by a substring search over deparsed bodies (helper-input-paths.R:1-9). That
      function is a non-aborting predicate; `check_paths_readable()`
      (`R/utils.R:83`) and `check_batch_inputs()` (`R/ffmpeg.R:4700`) both call
      it, and `check_batch_inputs()` keeps its per-carrier test, so the union
      sweep that names both bad carriers in one call (M62 F2/N3) is unchanged.
- [ ] AC4: The comment over `reject_duplicate_inputs()` (`R/ffmpeg.R:4718`)
      states that a later derived-output verb with more than one input column
      must compare each row's whole input tuple, not one column, and no longer
      promises such a verb inherits this wording. The function's formals and
      body are unchanged.
- [ ] AC5: No abort rendering changes except AC2's. Every wording expectation in
      `test-input-path-front-door.R`, `test-contradiction-front-door.R`,
      `test-na-value-guards.R` and `test-front-door-ordering.R` passes with its
      expected string unedited: `git diff` over those four files against the
      merge-base deletes no expectation line.
- [ ] AC6: `Rscript -e 'devtools::test()'` clean; `Rscript -e
      'devtools::check()'` 0 errors, 0 warnings; `devtools::document()` produces
      no diff; `NEWS.md` carries an entry for AC2's user-visible change.

## Coverage

- AC1 → T1, T2
- AC2 → T2, T3
- AC3 → T4
- AC4 → T5
- AC5 → T6
- AC6 → T6

## Tasks

- [ ] T1: Write `unchecked_flag_guards()` in
      `tests/testthat/helper-na-guards.R`, walking parsed bodies for `!`, `&&`,
      `||` applied to a required formal with no prior `rlang::check_bool()` on
      it. Reuse the recursive call-node walk shape of `tm_callees()`
      (`helper-input-paths.R:12-32`), which reads the namespace rather than
      `R/` so it survives `R CMD check`. Add the AC1 positive controls: a
      planted predicate per operator form that must be flagged, and a
      check-first predicate that must not be.
- [ ] T2: Test-first. Assert `unchecked_flag_guards()` returns empty; confirm it
      fails on the merge-base naming exactly
      `check_audio_codec_needs_reencode` and `check_resize_needs_two_inputs`,
      and record that list as AC2's domain.
- [ ] T3: Add `rlang::check_bool(<flag>, call = call)` to both predicates,
      matching the twin at `R/ffmpeg.R:2829` and its comment's reasoning
      (every caller checks its own flag first, so this refuses no call that was
      reaching here). Assert the four `na_values()` renderings for each.
- [ ] T4: Extract the non-aborting readability predicate into `R/utils.R` beside
      `check_paths_readable()`; rewire `R/utils.R:83` and `R/ffmpeg.R:4700` to
      it; add the `tm_call_graph()` one-site test to
      `test-input-path-front-door.R` beside the existing one-site abort test
      (`:410-414`).
- [ ] T5: Rewrite the comment at `R/ffmpeg.R:4718` per AC4, citing D057 for the
      order it does keep single and D059 for the generality it refuses.
- [ ] T6: Write D059 in `cairn/DECISIONS.md`; add the `NEWS.md` entry; run
      `devtools::document()`, `devtools::test()`, `devtools::check()`; verify
      AC5's diff.

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in FULL mode (surface tier user-facing); returned eight findings over five drafted criteria. Five fixed here: the F10 criterion dropped as a no-op (verified `check_bool`'s `arg` already resolves to `reencode`); AC1's NA probes widened from two types to the four `na_values()` declares, since `check_bool` renders a different message per type; AC3's cited snapshot evidence replaced with the `expect_match` at `test-input-path-front-door.R:401` (`_snaps/` holds only `ffm.md` and records no guard abort); AC4's substring search replaced with `tm_call_graph()`'s parsed walk; AC5 restated over the four test files' renderings after it was found to bind instrument state (snapshot mtimes, `git status`) and to be vacuous. Three went to the gate as questions. The audit also found a live third instance of F4's class the row's hand-list missed, `check_resize_needs_two_inputs(NA, 3)`.
- 2026-08-28: plan gate chose a parsed-body walk over hand-fixing the three known flag guards, and over widening `na_sweep_predicates()`' formals filter to two required arguments; the hand-list is the shape that shipped this gap (it missed `check_resize_needs_two_inputs`), and the formals widening pulls in `check_batch_cell`, whose `NA_integer_` row is deliberate (`R/ffmpeg.R:3395`), so it would need an exemption registry. Falsified by a flag guard the walk passes that still crashes on `NA`, or by the walk flagging a predicate whose bare branch is correct.
- 2026-08-28: plan gate chose correcting `reject_duplicate_inputs()`' comment over parameterizing it by carrier column. Duplication on a fan-in derived-output verb is a property of the row's whole input tuple, so a per-column check would refuse a legal table whose `main` repeats with distinct `overlay`; the sibling's `col` is a vector swept in one call, which a scalar `jobs[[col]]` is not; GP1 prefers refusing scope. Falsified by a derived-output verb arriving whose duplication really is per-column.
- 2026-08-28: plan gate chose one D-entry extending D041 over none and over two. The flag-guard rule and the column-parameterization refusal are both genuine rejections needing rationale on the record; splitting them would make a future supersession read past one to reach the other. Falsified by a supersession that needs to move only half the entry.

## Decisions

## Review
