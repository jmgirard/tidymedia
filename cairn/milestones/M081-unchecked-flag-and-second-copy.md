# M081: The unchecked flag, and the second copy of the one predicate

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m081-unchecked-flag-and-second-copy`

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
- [ ] AC6: Every exported call that can hand a non-flag to a flag guard
      refuses it on this ref, naming the flag. Membership is derived, never
      listed: the exports transitively reaching AC2's two predicates come
      from `getNamespaceExports()` and `tm_call_graph()`'s parsed-call-node
      walk, and each member's `jobs` column carriers from the flag names that
      member's own body quotes as column literals, so a spec cannot declare
      fewer carriers than the verb accepts. The sweep fails on a member no
      call-shape spec covers, on a spec no member matches, and on an empty
      member set. For every member, every derived delivery form, and each of
      six SCALAR value forms — the four types `na_values()` declares, `1L`,
      `"yes"` — the call signals an `rlang_error` naming the flag in that
      form's own spelling: `` `<flag>` `` as an argument, `<flag> column` as a
      column. Non-scalar values are out of the domain, since in a `jobs`
      column length is row count. Whether these renderings are unchanged from
      the merge-base is AC5's `git diff`, not this criterion's.
- [ ] AC7: `Rscript -e 'devtools::test()'` clean; `Rscript -e
      'devtools::check()'` 0 errors, 0 warnings; `devtools::document()`
      produces no diff.

## Coverage

- AC1 → T1, T2
- AC2 → T2, T3
- AC3 → T4
- AC4 → T5
- AC5 → T6
- AC6 → T7
- AC7 → T6

## Tasks

- [x] T1: Write `unchecked_flag_guards()` in
      `tests/testthat/helper-na-guards.R`, walking parsed bodies for `!`, `&&`,
      `||` applied to a required formal with no prior `rlang::check_bool()` on
      it. Reuse the recursive call-node walk shape of `tm_callees()`
      (`helper-input-paths.R:12-32`), which reads the namespace rather than
      `R/` so it survives `R CMD check`. Add the AC1 positive controls: a
      planted predicate per operator form that must be flagged, and a
      check-first predicate that must not be.
- [x] T2: Test-first. Assert `unchecked_flag_guards()` returns empty; confirm it
      fails on the merge-base naming exactly
      `check_audio_codec_needs_reencode` and `check_resize_needs_two_inputs`,
      and record that list as AC2's domain.
- [x] T3: Add `rlang::check_bool(<flag>, call = call)` to both predicates,
      matching the twin at `R/ffmpeg.R:2829` and its comment's reasoning
      (every caller checks its own flag first, so this refuses no call that was
      reaching here). Assert the four `na_values()` renderings for each.
- [x] T4: Extract the non-aborting readability predicate into `R/utils.R` beside
      `check_paths_readable()`; rewire `R/utils.R:83` and `R/ffmpeg.R:4700` to
      it; add the `tm_call_graph()` one-site test to
      `test-input-path-front-door.R` beside the existing one-site abort test
      (`:410-414`).
- [x] T5: Rewrite the comment at `R/ffmpeg.R:4718` per AC4, citing D057 for the
      order it does keep single and D059 for the generality it refuses.
- [ ] T6: Write D059 in `cairn/DECISIONS.md`; confirm `NEWS.md` is unchanged
      against the merge-base; run `devtools::document()`, `devtools::test()`,
      `devtools::check()`; verify AC5's diff.
- [x] T7: Write `flag_guard_verbs()` and `flag_guard_specs()` in
      `helper-na-guards.R` — the walk over exports reaching either flag guard,
      and one entry per verb per delivery form carrying `arg` and `via` as
      `check_dim_specs()` does — plus the AC6 sweep in
      `test-na-value-guards.R`: the two-way cover check, the non-vacuity
      floor, the column-carrier derivation from each verb's own body literals,
      and the six-scalar-form × delivery-form refusal loop asserting
      `rlang_error` and the flag name in the spelling `via` declares.

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in FULL mode (surface tier user-facing); returned eight findings over five drafted criteria. Five fixed here: the F10 criterion dropped as a no-op (verified `check_bool`'s `arg` already resolves to `reencode`); AC1's NA probes widened from two types to the four `na_values()` declares, since `check_bool` renders a different message per type; AC3's cited snapshot evidence replaced with the `expect_match` at `test-input-path-front-door.R:401` (`_snaps/` holds only `ffm.md` and records no guard abort); AC4's substring search replaced with `tm_call_graph()`'s parsed walk; AC5 restated over the four test files' renderings after it was found to bind instrument state (snapshot mtimes, `git status`) and to be vacuous. Three went to the gate as questions. The audit also found a live third instance of F4's class the row's hand-list missed, `check_resize_needs_two_inputs(NA, 3)`.
- 2026-08-28: T3. `rlang::check_bool(<flag>, call = call)` added to `check_audio_codec_needs_reencode()` and `check_resize_needs_two_inputs()`, each with the comment its twin at `R/ffmpeg.R:2829` carries. Both claims that this refuses no call that was reaching there are verified, not asserted: all three callers of the first run `check_codec_needs_reencode()` on the same value first (`R/ffmpeg.R:3095`, `:3191`, `:3397`), and both callers of the second check `resize` themselves (`:6114`, `:6439`, plus the column guard at `:6454`). Measured on the merge-base: every one of the five exported routes already signals an `rlang_error` naming the flag; only the direct internal calls signal the bare `simpleError` reading `missing value where TRUE/FALSE needed`. All four `na_values()` types now render `` `<flag>` must be `TRUE` or `FALSE` `` on both predicates; the contradiction aborts and the legal calls are unchanged. Suite: 0 failures, 7998 passing.
- 2026-08-28: T4. `unreadable_paths()` extracted into `R/utils.R` beside `check_paths_readable()` — non-aborting, returns the unique unreadable paths, and carries the `as.character()` coercion so the batch sweep gets it without passing through the abort. `check_paths_readable()` and `check_batch_inputs()` both reach it; `file.access` is now called from one function. Discrimination shown, not assumed: the same parsed-call-node walk run over the merge-base sources returns two callers (`check_batch_inputs`, `check_paths_readable`), so the one-site assertion is red there and green here. The per-carrier test and the union call that names both bad carriers are untouched.
- 2026-08-28: T5. The comment over `reject_duplicate_inputs()` (`R/ffmpeg.R`) now separates what a later verb inherits — the order, D057's — from what it does not: this wording reads `jobs$input` by name, and a derived-output verb with more than one input column must compare each row's whole input tuple. It no longer promises such a verb inherits the wording, and it records why the column parameterization was refused (D059), including that `reject_duplicate_outputs()`' `col` sweeps a vector in one call rather than indexing a scalar. Verified against the code, not composed: all three callers (`R/ffmpeg.R:1999`, `:4034`, `:4456`) carry a single `input` column, and `git diff` shows only comment lines changed — the function's formals and body are byte-identical.
- 2026-08-28: AMENDMENT (substantive, gated twice). AC6's `NEWS.md` clause is withdrawn and AC6 restated as an exported-surface promise; the three tool-gate clauses split out as AC7; Coverage becomes AC6 → T7, AC7 → T6; T6's "add the NEWS.md entry" becomes "confirm NEWS.md is unchanged against the merge-base"; T7 added. Motivating finding: AC2's change is unreachable from every exported call — all four reaching verbs already refuse a non-flag at their own `rlang::check_bool()` on the merge-base — so the planned entry would have asserted a behavior no test can fail without. Two fresh-context [O] criteria audits ran in FULL mode on the amended wording. The first killed a draft that justified the drop with a hand-list of three functions and six line numbers (one already wrong, all six stale after T3) and bound a changelog-recording act rather than the deliverable. The second, on the repaired wording, found four more: the column routes render `<flag> column` not `` `<flag>` ``, so one message assertion would have passed for the wrong reason; the spec cover clause failed on an uncovered member but not on a spec declaring fewer delivery forms than its verb accepts; there was no floor against the walk returning empty after a rename; and "no exported call's refusal CHANGES" is a cross-ref claim no single-ref test can make. All four are in the adopted text, which the user chose over the auditor's longer verbatim version at the second mini gate.
- 2026-08-28: T7. `flag_guard_verbs()` and `flag_guard_specs()` added to `helper-na-guards.R`; the AC6 sweep added to `test-na-value-guards.R`. The walk returns four members — `compare_videos`, `compare_videos_batch`, `segment_video`, `segment_video_batch` — over six delivery forms; 36 assertions pass. Each of the four guard clauses was planted and seen red: a member with no spec, a spec with no member, an empty member set, and a dropped column delivery form (the body-literal derivation reports `reencode`). The `via` distinction is load-bearing, not decorative: the column route renders `The reencode column of {.arg jobs} must be TRUE or FALSE (no NA).`, so an assertion on the backticked `` `reencode` `` would have failed there — the defect the second criteria audit caught.
- 2026-08-28: plan gate chose a parsed-body walk over hand-fixing the three known flag guards, and over widening `na_sweep_predicates()`' formals filter to two required arguments; the hand-list is the shape that shipped this gap (it missed `check_resize_needs_two_inputs`), and the formals widening pulls in `check_batch_cell`, whose `NA_integer_` row is deliberate (`R/ffmpeg.R:3395`), so it would need an exemption registry. Falsified by a flag guard the walk passes that still crashes on `NA`, or by the walk flagging a predicate whose bare branch is correct.
- 2026-08-28: plan gate chose correcting `reject_duplicate_inputs()`' comment over parameterizing it by carrier column. Duplication on a fan-in derived-output verb is a property of the row's whole input tuple, so a per-column check would refuse a legal table whose `main` repeats with distinct `overlay`; the sibling's `col` is a vector swept in one call, which a scalar `jobs[[col]]` is not; GP1 prefers refusing scope. Falsified by a derived-output verb arriving whose duplication really is per-column.
- 2026-08-28: T1/T2. `unchecked_flag_guards()` added to `helper-na-guards.R`, walking the namespace's `check_*` bodies in top-level-statement order for a required formal made the direct operand of `!`, `&&` or `||` with no earlier `rlang::check_bool()` on it. Positive controls pass: one planted predicate per operator form is flagged naming `flag`, and `!is.null(flag)`, a check-first predicate, and an unbranched second formal are all left alone; a check-AFTER-branch control is flagged, fixing "first" as positional. On the merge-base namespace the walk returns exactly `check_audio_codec_needs_reencode` (`reencode`) and `check_resize_needs_two_inputs` (`resize`) — AC2's domain, measured, not listed.
- 2026-08-28: plan gate chose one D-entry extending D041 over none and over two. The flag-guard rule and the column-parameterization refusal are both genuine rejections needing rationale on the record; splitting them would make a future supersession read past one to reach the other. Falsified by a supersession that needs to move only half the entry.

## Decisions

## Review
