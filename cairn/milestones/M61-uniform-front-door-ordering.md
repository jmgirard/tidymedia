# M61: A value error and a contradiction resolve the same way in both forms

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M59
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m61-uniform-front-door-ordering`

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

- [x] T1 — Milestone-local decision entry: the four-guard set with the commit it
      was closed at, the displaced-error list AC2 crosses, and why `resize`,
      the jobs-shape and column-type guards stay above.
- [x] T2 — Move `check_vocab_arg()` below the contradiction checkers in
      `compare_videos_pipeline()` and `picture_in_picture_pipeline()`.
- [x] T3 — `compare_videos_batch()`: move the scalar `direction` guard down;
      move the scalar `audio` lower-bound check down beside the per-row sweep.
- [x] T4 — `picture_in_picture_batch()`: move the scalar `position` and `margin`
      guards down; add the front-door per-row `audio` sweep and retire the
      fan-out closure's copy.
- [x] T5 — Extend the grid: the four guards × both forms × the displaced errors,
      plus the message-text and `call`-presence readers.
- [x] T6 — Tests: AC1's cells with their nonexistent pair recorded, AC3's nvenc
      invariant, AC4; invert M59's argument-form tests.
- [ ] T7 — D-entry superseding D038; roxygen and NEWS; AC6's sentence cells;
      the residue grep; run the verify slot clean.

## Work log

- 2026-08-07: created by /milestone-plan, from M59's third-pass thrash stop.
- 2026-08-07: absorbs the ROADMAP candidate row added at M59's first review return ("the same value mistake is answered differently depending on whether it arrives as an argument or as a `jobs` column"), whose promotion condition was "alongside the next milestone touching these verbs' front-door ordering". The row is removed in this same commit — a planned milestone supersedes the candidate it came from — so no criterion here claims to retire it.
- 2026-08-07: plan gate chose to move the four guards BELOW the contradiction checkers over moving the column sweeps above them, because D036's machine-independence reasoning says the contradiction should win and the alternative would make D036 false in both forms rather than true in both; falsified by a caller who needs the value error first on a call that also contradicts itself, which no report has yet asked for.
- 2026-08-07: plan gate chose to accept the user-visible change (a call wrong in a scalar value AND contradicting itself now reports the contradiction, where it reported the value) over preserving today's behavior, the package being pre-1.0 and the alternative leaving the disagreement that produced three failed descriptions; falsified by a downstream caller matching on the value message for such a call.
- 2026-08-07: criteria audit ([O] fresh-context reader) returned findings on all seven drafts. Acted on before the gate: the Approach was narrowed from "every per-value check" to the four genuinely non-uniform guards, after the reader MEASURED that moving `check_bool(resize)` degrades `check_resize_needs_two_inputs()` to three unattributed base-R errors; AC1 and AC2 were bounded to named values and to the grid, dropping "every"; AC2 gained message-text comparison (the M59 F4 hole) and the lost-`call` reader; AC3 gained the AC5(b) nvenc invariant, which moving checks downward could otherwise have silently inverted; AC5's `cairn/`-only grep was replaced with an enumerated retirement list plus a residue grep over `R/`, `man/`, `NEWS.md` and `tests/`, the reader having found the exception's wording lives outside `cairn/` and that a `cairn/` grep can never return the claimed result because IP4 forbids rewriting M59's record; AC6 now quotes the sentence it pins instead of promising a test that tracks prose. AC7 returned clean. Two vacuous cells the reader found are recorded in AC1 rather than asserted.
- 2026-08-08: question gate chose to extend `data-raw/value-guard-baseline.R` rather than add a sibling script (it already probes these four guards in both forms); to cross the grid against the contradiction, `check_nvenc_available()` and `ffm_batch()`'s `run` guard; and to rewrite NEWS's known-gap paragraph in place rather than append a retraction, the whole entry being unreleased.
- 2026-08-08: T1 — M61-D1 records the four-guard set closed by inspection at `1d54b20`, the three displaced errors AC2's grid crosses, and why `resize`, the jobs-shape and the column-type guards stay above.
- 2026-08-08: T2 — `check_vocab_arg()` now runs below the contradiction checkers in both `*_pipeline()` functions, so `direction` and `position` report after them in the argument form as they already did in the column form. `devtools::test()`: 0 failures, 4402 passing.
- 2026-08-08: T3 — `compare_videos_batch()` checks `direction` and the scalar `audio` bound below the contradiction sweep. Minor task refinement: T3 said DELETE the scalar `audio` check as covered by the per-row sweep; measured on `1d54b20` that all three scalar guards refuse today even when a `jobs` column overrides the argument (`compare_videos_batch(jobs_with_audio_column, audio = -1)` aborts), so deleting one would lose a refusal, which Scope Out and AC3 both forbid. It moves instead. `devtools::test()`: 0 failures, 4402 passing.
- 2026-08-08: T4 — `picture_in_picture_batch()` checks `position`, `margin` and `audio` below the contradiction sweep, and gains a front-door per-row `audio` sweep; the fan-out closure's copy retires. Measured: an out-of-range `audio` column now aborts naming the verb at both `parallel` settings, with no `pmap`, `In index:` or `aud` in the message. As in T3 the scalar guards move rather than being deleted. `devtools::test()`: 0 failures, 4402 passing.
- 2026-08-08: T5 — `data-raw/value-guard-baseline.R` gains the ordering dimension: each of the four guards plus pip's `audio` (new site 7), in scalar and column form, crossed with the contradiction, `check_nvenc_available()` and `ffm_batch()`'s `run` guard, each paired with a control proving the crossed error is live. 110 cells. Two readers added — `value_guard_missing_call()` (no abort lost its `call`) and `value_guard_ordering()` / `value_guard_dead_controls()`. Two existing readers narrowed with their reasons recorded in the file: `value_guard_message_regressions()` now covers only `crossed = "none"` cells (an ordering cell changes its message with its blame frame unmoved, which is the deliverable, not a regression), and `value_guard_blame_regressions()` exempts the `run_guard` controls (`ffm_batch()` names itself for its own guard, and did before this milestone).
- 2026-08-08: T5 — measured against `origin/master`: 0 changed refusals, 0 message regressions, 0 blame regressions, 0 aborts missing a `call`, 0 dead controls, 0 vacuous cells on either ref. Six cells change which error they report: the four scalar-argument cells crossed with a contradiction (`direction`, `position`, `margin`, and compare's `audio` at its LOWER bound) move value → contradiction; pip's `audio` column crossed with nvenc and with the `run` guard moves those → value, the front-door guard being new. Probing compare's `audio` only at its upper bound would have missed its moving cell — the upper bound already sat below the sweep, which is the asymmetry D038 recorded.
- 2026-08-08: T6 — new `tests/testthat/test-front-door-ordering.R`: AC1's eleven ordering cells (four guards x both forms, compare's `audio` at both bounds) each with its control, the nonexistent pip-`audio`-argument cell asserted rather than left silent, AC3's nvenc invariant over all five sites in both forms, and AC4 at both `parallel` settings. Suite: 0 failures, 4578 passing (was 4402).
- 2026-08-08: T6 — the "invert M59's argument-form tests" clause found NOTHING to invert: no M59 test asserts the argument form reports the value check ahead of a contradiction. Measured, not assumed — the full suite passed unchanged after T2-T4 moved every one of the four guards. M59's AC5(a)/AC5(b) tests order the value check against nvenc and against `ffm_batch()`'s `run` guard, both invariants this milestone preserves, and its column-form ordering test is unaffected. The clause is recorded as vacuous rather than quietly dropped.
- 2026-08-08: T6 — `blamed_verb()` / `catch_call()` lifted into `tests/testthat/helper-blame.R`; the identical copies in `test-contradiction-front-door.R` and `test-value-check-front-door.R` are deleted rather than a third being written (the M40 trap).
- 2026-08-08: T6 — the new tests verified red on the pre-milestone ref: the four scalar cells report the value on `origin/master` (grid), and pip's `audio` column there aborts with `purrr::pmap`, `In index: 1` and `aud` in the message, which is exactly what AC4 forbids.

## Decisions

### M61-D1 — The four non-uniform guards, the errors they now report after, and what stays above (2026-08-08)

**The set, closed by inspection at `1d54b20`.** Four front-door value guards on
`compare_videos_batch()` / `picture_in_picture_batch()` answer differently
depending on whether the value arrived as an argument or in a `jobs` column:

| Guard | Verb | Argument form, before | Column form, before |
|---|---|---|---|
| `direction` vocabulary | compare | `check_vocab_arg()` at the top of the verb AND of `compare_videos_pipeline()` | `check_batch_vocab_col()`, below the contradiction sweep |
| `position` vocabulary | pip | `check_vocab_arg()` at the top of the verb AND of `picture_in_picture_pipeline()` | `check_batch_vocab_col()`, below the contradiction sweep |
| `margin` lower bound | pip | `check_number_whole(margin, min = 0)` at the top of the verb | the per-row `margin` sweep, below the contradiction sweep |
| `audio` upper bound | both | compare: no upper bound above (only `min = 0`); pip: `max = 1` at the top of the verb | compare: the per-row sweep below the contradiction sweep; pip: **not at the front door at all** — inside the fan-out closure, reported against `purrr::pmap()` with the local name `aud` (M59 review F7) |

The set is closed by reading the two verbs and their two pipelines at that
commit; no procedure enumerates it. Every other front-door value guard on these
verbs already answers alike in both forms, because its column counterpart also
sits below the contradiction sweep.

**How each is made uniform.** The argument-form guard moves below the M58
contradiction sweep, joining its column counterpart, rather than the column
sweep moving up: D036's reasoning is that a contradiction is decided identically
on every machine and should therefore report first, and moving the column sweeps
up would make D036 false in both forms rather than true in both. For `margin`
and compare's `audio` the move is a deletion: the existing per-row sweep already
resolves the argument through `batch_arg_rows()`, so it covers the argument form
once the scalar guard above is gone. Pip's `audio` gains a front-door per-row
sweep it never had, and the fan-out closure's copy retires with it.

**The displaced errors AC2's grid crosses.** Three front-door errors can be
reordered against these four guards, and the grid measures each guard × each
form against each of them:

1. the verb's M58 contradiction — `audio_codec` naming an encoder with no audio
   carried (both verbs), and `resize` across other than two inputs (compare);
2. `check_nvenc_available()` — which must still report *after* every one of the
   four, preserving M59's AC5(b);
3. `ffm_batch()`'s own `run` / `parallel` guards — likewise still after.

Only (1) changes. (2) and (3) are measured because a downward move could
silently invert either, and an unmeasured invariant is an assumed one.

**What stays above, and why.**

- `rlang::check_bool(resize)` on `compare_videos_batch()`. Measured: without it
  `check_resize_needs_two_inputs()` degrades to unattributed base-R errors —
  `"invalid 'x' type in 'x && y'"`, `"missing value where TRUE/FALSE needed"`,
  `"'length = 2' in coercion to logical(1)"`. The contradiction checker consumes
  `resize`, so its type guard cannot move below it.
- The jobs-shape guards (`check_fanin_jobs()`, pip's inline `main`/`overlay`/
  `output` block, `reject_duplicate_outputs()`) and every column *type* guard.
  The contradiction row-sweep reads `nrow(jobs)` and `jobs$inputs`, so it cannot
  run before the table is known to have them.
- `check_token()` on both codecs and `arg_match(hardware)`. Their column
  counterparts already sit above the contradiction sweep, so they are uniform
  already; moving them would create the disagreement this milestone removes.

## Review
