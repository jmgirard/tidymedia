# M61: A value error and a contradiction resolve the same way in both forms

- **Status:** planned
- **Priority:** normal
- **Depends on:** M59
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

On `compare_videos_batch()` and `picture_in_picture_batch()`, make a value error
and an argument contradiction resolve identically whether the value arrived as an
argument or in a `jobs` column.

## Scope

**In:** four guards, and only four. M59's review measured that the argument and
column forms of one mistake can report different errors, and three successive
attempts to *describe* that irregularity were each wrong. This milestone removes
the irregularity instead. The non-uniform set is exactly `direction` (compare),
`position` and `margin` (pip), and the per-row `audio` upper bound (both): every
other front-door value guard already answers alike in both forms, because its
column counterpart also sits above the contradiction sweep. **That set is closed
by inspection at the commit named in the milestone-local decision entry — no
procedure enumerates it and no criterion here claims one.**

Each of the four moves *below* the M58 contradiction checkers, in both
`*_pipeline()` functions and both `_batch` front doors, so the contradiction
reports first in both forms. That is D036's rule restored unconditionally, and it
makes D038's recorded exception retirable. `picture_in_picture_batch()`'s per-row
`audio` check is folded in: it still lives in the fan-out closure and reports the
internal name `aud` against `purrr::pmap()` (M59 review F7), so without moving it
that verb has no front-door `audio` guard to order.

**Out — and these must NOT move:** `rlang::check_bool(resize)`, because
`check_resize_needs_two_inputs()` consumes `resize` and degrades to unattributed
base-R errors without it (measured: `"invalid 'x' type in 'x && y'"`,
`"missing value where TRUE/FALSE needed"`, `"'length = 2' in coercion to
logical(1)"`); the jobs-shape guards and every column *type* guard, because the
contradiction row-sweep reads `nrow(jobs)` and `jobs$inputs`; `check_token()` on
both codecs and `arg_match(hardware)`, whose column counterparts already sit
above the sweep, so they are already uniform. Also out: the scalar verbs'
`compare_videos()` / `picture_in_picture()` front doors beyond their shared
pipeline; any change to which calls are refused.

## Acceptance criteria

- [ ] AC1 — For each of the four values in Scope In, in each of its argument and
      `jobs`-column forms, a call also violating a contradiction that verb
      carries reports the contradiction. Cells where the two cannot co-occur are
      recorded nonexistent rather than asserted — pip's `audio` against its only
      contradiction in the argument form, since a non-NULL `audio` removes that
      contradiction.
- [ ] AC2 — A committed before/after grid crosses each of the four guards, in
      each form, with each front-door error named in the milestone-local
      decision entry, run against both refs. It compares abort kind, blame
      target **and message text**. Every cell whose reported error changes is
      listed in NEWS, and a reader asserts no cell's abort lost its `call` — the
      unattributed-base-error regression the Scope Out clause exists to prevent.
- [ ] AC3 — Over the AC2 grid: no cell that compiled before aborts after, none
      that aborted before compiles after, and every in-range baseline succeeds on
      both refs. Additionally, on all four sites a value check still reports
      before `check_nvenc_available()`, preserving M59's AC5(b).
- [ ] AC4 — `picture_in_picture_batch()`'s per-row `audio` index is checked at
      its front door: a violating call aborts naming the verb, with a message
      carrying none of `pmap`, `In index:` or `aud`. Run at both `parallel`
      settings for parity with M59's suite, which is not additional evidence.
- [ ] AC5 — The exception retires everywhere it is asserted as current: a new
      D-entry supersedes D038; both `@param hardware` blocks and NEWS's
      known-gap paragraph are rewritten; and M59's tests asserting the argument
      form reports the value check are inverted. A residue grep over `R/`, `man/`, `NEWS.md` and `tests/` for the
      exception's wording returns nothing. `cairn/` is excluded by design — its
      archive keeps the historical record (IP4).
- [ ] AC6 — Both `_batch` verbs' `@param hardware` blocks and the NEWS entry
      state exactly: "A value error and a contradiction resolve the same way
      whether the value arrived as an argument or in a `jobs` column; the
      contradiction reports first." Each of that sentence's two quantified terms
      — the four values, the two forms — has a cell in the AC2 grid, and the test
      file names the sentence it pins. Any later edit to the quoted sentence is a
      widening unless the enumeration travels with it.
- [ ] AC7 — The r-package profile's verify slot is clean: `devtools::document()`
      produces no diff, `devtools::test()` passes, and `devtools::check()`
      reports 0 errors and 0 warnings.

## Coverage

- AC1 → T2, T3, T4, T6
- AC2 → T5
- AC3 → T5, T6
- AC4 → T4, T6
- AC5 → T7
- AC6 → T6, T7
- AC7 → T7

## Tasks

- [ ] T1 — Milestone-local decision entry: the four-guard set with the commit it
      was closed at, the displaced-error list AC2 crosses, and why `resize`,
      the jobs-shape and column-type guards stay above.
- [ ] T2 — Move `check_vocab_arg()` below the contradiction checkers in
      `compare_videos_pipeline()` and `picture_in_picture_pipeline()`.
- [ ] T3 — `compare_videos_batch()`: move the scalar `direction` guard down;
      delete the scalar `audio` lower-bound check the per-row sweep now covers.
- [ ] T4 — `picture_in_picture_batch()`: move the scalar `position` and `margin`
      guards down; add the front-door per-row `audio` sweep and retire the
      fan-out closure's copy.
- [ ] T5 — Extend the grid: the four guards × both forms × the displaced errors,
      plus the message-text and `call`-presence readers.
- [ ] T6 — Tests: AC1's cells with their nonexistent pair recorded, AC3's nvenc
      invariant, AC4, AC6's sentence cells; invert M59's argument-form tests.
- [ ] T7 — D-entry superseding D038; roxygen and NEWS; the residue grep;
      run the verify slot clean.

## Work log

- 2026-08-07: created by /milestone-plan, from M59's third-pass thrash stop.
- 2026-08-07: absorbs the ROADMAP candidate row added at M59's first review return ("the same value mistake is answered differently depending on whether it arrives as an argument or as a `jobs` column"), whose promotion condition was "alongside the next milestone touching these verbs' front-door ordering". The row is removed in this same commit — a planned milestone supersedes the candidate it came from — so no criterion here claims to retire it.
- 2026-08-07: plan gate chose to move the four guards BELOW the contradiction checkers over moving the column sweeps above them, because D036's machine-independence reasoning says the contradiction should win and the alternative would make D036 false in both forms rather than true in both; falsified by a caller who needs the value error first on a call that also contradicts itself, which no report has yet asked for.
- 2026-08-07: plan gate chose to accept the user-visible change (a call wrong in a scalar value AND contradicting itself now reports the contradiction, where it reported the value) over preserving today's behavior, the package being pre-1.0 and the alternative leaving the disagreement that produced three failed descriptions; falsified by a downstream caller matching on the value message for such a call.
- 2026-08-07: criteria audit ([O] fresh-context reader) returned findings on all seven drafts. Acted on before the gate: the Approach was narrowed from "every per-value check" to the four genuinely non-uniform guards, after the reader MEASURED that moving `check_bool(resize)` degrades `check_resize_needs_two_inputs()` to three unattributed base-R errors; AC1 and AC2 were bounded to named values and to the grid, dropping "every"; AC2 gained message-text comparison (the M59 F4 hole) and the lost-`call` reader; AC3 gained the AC5(b) nvenc invariant, which moving checks downward could otherwise have silently inverted; AC5's `cairn/`-only grep was replaced with an enumerated retirement list plus a residue grep over `R/`, `man/`, `NEWS.md` and `tests/`, the reader having found the exception's wording lives outside `cairn/` and that a `cairn/` grep can never return the claimed result because IP4 forbids rewriting M59's record; AC6 now quotes the sentence it pins instead of promising a test that tracks prose. AC7 returned clean. Two vacuous cells the reader found are recorded in AC1 rather than asserted.

## Decisions

## Review
