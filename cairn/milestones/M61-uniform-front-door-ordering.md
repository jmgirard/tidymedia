# M61: A value error and a contradiction resolve the same way in both forms

- **Status:** review
- **Priority:** normal
- **Depends on:** M59
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m61-uniform-front-door-ordering` · https://github.com/jmgirard/tidymedia/pull/64

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

- [x] AC1 — For each of the four values in Scope In, in each of its argument and
      `jobs`-column forms, a call also violating a contradiction that verb
      carries reports the contradiction. No cell is recorded nonexistent. Where
      a pairing is reachable only at a particular value, the cell is asserted at
      that value and the value is named: pip's `audio` reaches its verb's only
      contradiction in the argument form at `audio = NA`, which
      `batch_stream_cell()` resolves to `NULL`, dropping the audio the encoder
      needs.
- [x] AC2 — A committed before/after grid crosses each of the four guards, in
      each form, with each front-door error named in the milestone-local
      decision entry, run against both refs. It compares abort kind, blame
      target **and message text**. Every cell whose reported error changes is
      listed in NEWS, and a reader asserts no cell's abort lost its `call` — the
      unattributed-base-error regression the Scope Out clause exists to prevent.
- [x] AC3 — Over the AC2 grid: no cell that compiled before aborts after, none
      that aborted before compiles after, and every in-range baseline succeeds on
      both refs. Additionally, on all four sites a value check still reports
      before `check_nvenc_available()`, preserving M59's AC5(b).
- [x] AC4 — `picture_in_picture_batch()`'s per-row `audio` index is checked at
      its front door: a violating call aborts naming the verb, with a message
      carrying none of `pmap`, `In index:` or `aud`. Run at both `parallel`
      settings for parity with M59's suite, which is not additional evidence.
- [x] AC5 — The exception retires everywhere it is asserted as current: a new
      D-entry supersedes D038; both `@param hardware` blocks and NEWS's
      known-gap paragraph are rewritten; and M59's tests asserting the argument
      form reports the value check are inverted. A residue grep over `R/`, `man/`, `NEWS.md` and `tests/` for the
      exception's wording returns nothing. `cairn/` is excluded by design — its
      archive keeps the historical record (IP4).
- [x] AC6 — Both `_batch` verbs' `@param hardware` blocks and the NEWS entry
      state exactly: "A value error and a contradiction resolve the same way
      whether the value arrived as an argument or in a `jobs` column; the
      contradiction reports first." Each of that sentence's two quantified terms
      — the four values, the two forms — has a cell in the AC2 grid, and the test
      file names the sentence it pins. Any later edit to the quoted sentence is a
      widening unless the enumeration travels with it.
- [x] AC7 — The r-package profile's verify slot is clean: `devtools::document()`
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
- [x] T7 — D-entry superseding D038; roxygen and NEWS; AC6's sentence cells;
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
- 2026-08-08: T7 — D039 appended, superseding D038 and restoring D036 unconditionally. Both `_batch` verbs' `@param hardware` blocks and the NEWS entry now carry the pinned sentence; NEWS's known-gap paragraph is rewritten in place (the whole entry is unreleased) and a second paragraph records pip's new front-door `audio` check and the two errors that now report after it. `test-front-door-ordering.R` gains the AC6 tests: the sentence in both Rd topics and in NEWS, matched on markup-normalized text, and an enumeration test keyed on (verb, value) asserting each pair's forms — including that pip's `audio` has the column form only, which is the cell measured not to exist.
- 2026-08-08: T7 — residue grep over `R/`, `man/`, `NEWS.md`, `tests/` and `data-raw/` for the exception's wording returns nothing.
- 2026-08-08: T7 — verify slot clean: `devtools::document()` writes only the two intended Rd files, `devtools::test()` 0 failures / 4586 passing (4 warnings, 5 skips, both unchanged from the branch base), `devtools::check()` 0 errors / 0 warnings / 0 notes.
- 2026-08-08: review — all seven criteria verified with fresh evidence; consistency gate clean (`cairn_validate` exit 0, `document()` no diff, `pkgdown::check_pkgdown()` clean, `check()` 0/0/0). One criterion needed a fix at review: AC5's residue grep returned a match in the new test file's own header, which carried D038's "disclosed gap" phrasing; the comment was reworded and the grep now returns nothing.
- 2026-08-08: review returned M61 to in-progress. AC1 failed as written: it records pip's `audio` against its only contradiction in the ARGUMENT form as a cell that cannot exist, "since a non-NULL `audio` removes that contradiction" — measured false, because `audio = NA` is non-NULL and `batch_stream_cell()` maps it to `NULL`, so the contradiction fires (`picture_in_picture_batch(jobs, audio = NA, audio_codec = "aac")` reports the value error on `origin/master` and the contradiction on the branch). The criterion embeds the false premise, so this is an amendment return, not a defect return. AC1 and AC6 unticked; AC2, AC3, AC4, AC5, AC7 keep their evidence. Three further findings scored >= 80 and are triaged fix-now in the same return: the guards were also reordered against `check_token()`/`arg_match(hardware)`/`scale`/the jobs-shape guards with no disclosure (F1, the disclosure D038 named as the work); the scalar `compare_videos()`/`picture_in_picture()` changed their error through the shared pipeline with no disclosure and no grid cell (F2); and M61-D1's table records a `check_vocab_arg()` at the top of the scalar verbs that is not there (F3), which is why F2 went unseen.
- 2026-08-08: amendment return: AC1 — "No cell is recorded nonexistent. Where a pairing is reachable only at a particular value, the cell is asserted at that value and the value is named: pip's `audio` reaches its verb's only contradiction in the argument form at `audio = NA`, which `batch_stream_cell()` resolves to `NULL`, dropping the audio the encoder needs."
- 2026-08-08: user override, logged per tracking-rules: M61-D1 and D039 are corrected IN PLACE rather than by superseding entries, against the never-edit rule for decision records. Both were authored on this branch and neither has reached `master`, so the alternative publishes a false entry together with its retraction for a mistake no reader ever saw; the branch history holds the original wording either way.

## Decisions

### M61-D1 — The four non-uniform guards, the errors they now report after, and what stays above (2026-08-08)

**The set, closed by inspection at `1d54b20`.** Four front-door value guards on
`compare_videos_batch()` / `picture_in_picture_batch()` answer differently
depending on whether the value arrived as an argument or in a `jobs` column:

| Guard | Verb | Argument form, before | Column form, before |
|---|---|---|---|
| `direction` vocabulary | compare | `check_vocab_arg()` at the top of `compare_videos_batch()` AND at the top of `compare_videos_pipeline()` — the scalar `compare_videos()` has none of its own and reaches the check only through the pipeline | `check_batch_vocab_col()`, below the contradiction sweep |
| `position` vocabulary | pip | `check_vocab_arg()` at the top of `picture_in_picture_batch()` AND at the top of `picture_in_picture_pipeline()` — the scalar `picture_in_picture()` has none of its own | `check_batch_vocab_col()`, below the contradiction sweep |
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

**A fourth class, added after M61's review measured it (F1).** The four guards
also now report after every argument check that stays above them —
`check_token()` on both codecs, `arg_match(hardware)`, `check_number_decimal(scale)`,
`rlang::check_bool(resize)` — and after the jobs-shape and column-type guards.
D038 named exactly this consequence and called the disclosure "the work"; the
list above initially carried only the three errors the grid crosses, which is
not the same thing. No refusal changes: a call wrong in both is refused either
way, and only which error it is told about moves. NEWS states it.

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
- 2026-08-08: M61-D1 corrected in place per the logged override — the `direction`/`position` rows said `check_vocab_arg()` sits "at the top of the verb", conflating the `_batch` verb with the scalar one, which is why the scalar verbs' change went unseen (review F3); and its displaced-error list gained the fourth class D038 named (review F1).
- 2026-08-08: grid widened for the amendment and for review F2/F4 — pip's `audio`/argument cell asserted at `NA` with an `audio = NULL` control (an in-range control would remove the contradiction it exists to prove), the same cell added for compare, and the two scalar verbs gained cells at `form = "argument"`. 118 cells; the only remaining nonexistent cell is M59's site-3 `regions` argument, which is not one of AC1's. Eleven cells now change which error they report, against the six recorded before.
- 2026-08-08: NEWS gained the two disclosures review asked for (F1: the four checks also report after the codec/hardware/scale/jobs-shape checks; F2: the same reordering reaches the single-call `compare_videos()` / `picture_in_picture()`), plus the `audio = NA` case F4 surfaced. D039 corrected in place per the logged override: the scalar verbs named, the changed-cell count six -> eleven, and the false "cannot exist" reasoning replaced by the reachability condition and its control. Verify slot clean: `document()` no diff, `devtools::test()` 0 failures / 4620 passing, `devtools::check()` 0/0/0.

## Review

_Reviewed 2026-08-08 on `m61-uniform-front-door-ordering` at PR #64, against `origin/master` @ `1d54b20`. Every line below is a command run in this session, never recall._

### Acceptance criteria

- **AC1** — `devtools::test(filter = "front-door-ordering")` passes: the eleven ordering cases (four guards x both forms, compare's `audio` at both bounds) each report the contradiction, each with a control first asserting the contradiction is live on that call. The one cell that cannot exist is asserted rather than omitted — pip's `audio` argument with `audio_codec = "aac"` reports the value, and the same call with the index in range COMPILES, which measures why no contradiction is there to cross. The grid agrees: every (guard, form, contradiction) cell reads `contradiction` after, and the single `exists = FALSE` cell is pip `audio`/scalar.
- **AC2** — `data-raw/value-guard-baseline.R` run against both refs: 110 cells, 72 crossed, 3 nonexistent. Coverage enumerated from the grid itself — 5 (verb, guard) pairs x 2 forms x 3 crossings (contradiction, nvenc, `run` guard), the crossings being exactly those M61-D1 names. It compares abort kind (`value_guard_refusals`), blame target (`value_guard_blame_regressions`) and message text (`value_guard_messages`). `value_guard_missing_call(after)` returns 0 rows: no abort lost its `call`. Six cells change which error they report, in two families, both stated in NEWS — the four scalar-argument cells crossed with a contradiction, and pip's `audio` column crossed with the availability and `run` guards.
- **AC3** — over the grid: `value_guard_refusals(before, after)` 0 rows (nothing that compiled aborts, nothing that aborted compiles); `value_guard_vacuous()` 0 rows on BOTH refs, so no in-range baseline failed on either side. The nvenc invariant is separately pinned by test: with the encoder seam held empty, all five sites in both forms report the value and never `nvenc`, each control asserting the availability abort is real.
- **AC4** — `picture_in_picture_batch()`'s `audio` index aborts at the front door in all three forms (argument, column, mixed column) at both `parallel` settings, blaming `picture_in_picture_batch`, with no `pmap`, no `In index:` and no `aud` in the message or the deparsed call. A clean `audio` column still compiles both rows, so the guard sweeps rather than gates. Measured on `origin/master` for contrast: the same column call there reports `purrr::pmap` / `In index: 1` / `` `aud` ``.
- **AC5** — D039 appended, superseding D038 and restoring D036 unconditionally; both `@param hardware` blocks and NEWS's known-gap paragraph rewritten. The residue grep over `R/`, `man/`, `NEWS.md` and `tests/` returned ONE match at review — the new test file's header carried D038's "disclosed gap" phrasing — which was reworded rather than argued away; the grep now returns nothing. The "invert M59's argument-form tests" clause is vacuous, and the emptiness is measured, not assumed: M59's two ordering cases (`test-value-check-front-door.R:452-471`) both carry the bad value in a `jobs` COLUMN and assert the contradiction wins, which this milestone preserves; and the full suite is green with all four guards moved, which a test asserting the old argument-form answer would have reddened.
- **AC6** — the sentence appears in both `_batch` Rd topics and in NEWS, matched on markup-normalized text so `\code{jobs}` and `` `jobs` `` compare equal while a changed word still fails. The Rd assertion runs both directions: exactly `compare_videos_batch` and `picture_in_picture_batch` carry it, so no verb whose front door was never reordered claims that it was. The enumeration test keys on (verb, value) and requires each pair's forms — four pairs at both forms, and pip's `audio` at `column` only, which is the cell measured not to exist.
- **AC7** — `devtools::document()` leaves `man/` and `NAMESPACE` with no diff; `devtools::test()` 0 failures, 4586 passing (4 warnings, 5 skips, both unchanged from the branch base); `devtools::check()` 0 errors, 0 warnings, 0 notes.

### Consistency gate

`cairn_validate` exit 0 — all PASS, all advisories OK. Toolchain slot: `document()` no diff, generated files unedited, README.Rmd untouched so README.md stays in sync, `pkgdown::check_pkgdown()` "No problems found", NEWS carries the user-visible change with no milestone numbers, no new top-level files, `check()` clean.

### Independent review — three lenses, then a scorer

Three fresh-context reviewers, distinct evidence bases. The **[S] blame-history** lens returned zero findings (it blamed every moved and deleted line back to M32/M59, and read D035–D039). The **[S] prior-review** lens returned zero findings; its GitHub inline-comment probe came back empty, so archived `## Review` sections were the evidence base. The **[O] diff-bug** lens returned twelve. A fourth **[S] scorer**, which did not generate them, scored each against the rubric.

**Actioned (>= 80).** All four re-verified in this session by running the named calls on both refs.

- **F1 (88) — the four guards were also reordered against `check_token()`, `arg_match(hardware)`, `scale` and the jobs-shape guards, and that is undisclosed.** D038 said of this move: "The work is that disclosure and its tests, not the move." M61 made the move and disclosed only the contradiction crossing. Measured: `compare_videos_batch(jobs, direction = "sideways", hardware = "bogus")` reports `direction` on master and `hardware` on HEAD; `picture_in_picture_batch(jobs, position = "middleish", scale = "x")` reports `position` then `scale`; `picture_in_picture_batch(jobs[0, ], margin = -3)` reports `margin` then the empty-table error. A 4488-shape sweep found 718 cells whose message changed, against the six the milestone enumerates. No refusal changed (0 differing verdicts across all 4488), so this is diagnostics ordering on multi-error calls, not a contract change. **Triage: fix now** — the disclosure D038 asked for, in NEWS and in M61-D1's displaced-error list.
- **F2 (82) — the scalar verbs `compare_videos()` and `picture_in_picture()` changed their user-visible error, and no document says so.** Neither has its own `check_vocab_arg()`; the shared pipeline is their only vocabulary guard, so moving it moved theirs. Measured: `compare_videos(c(s, s), "o.mp4", direction = "sideways", audio_codec = "aac")` reports `direction` on master and the contradiction on HEAD; same shape for `picture_in_picture()`. Scope Out excludes those front doors "beyond their shared pipeline", so the change is IN scope — what is missing is its disclosure, and the grid probes only the `_batch` verbs so no criterion ever saw these cells. **Triage: fix now** — NEWS names the scalar verbs, and the grid gains their cells.
- **F3 (80) — M61-D1's table states a false fact, and it is the one the closed-by-inspection set rests on.** It records `check_vocab_arg()` as sitting "at the top of the verb AND of `compare_videos_pipeline()`", but the scalar verbs have no top-level copy — only the `_batch` verb does. The table conflates the `_batch` verb with the scalar verb, which is why F2 went unnoticed. **Triage: fix now**, by a superseding milestone-local entry — M61-D1 is history and is never edited (D-074, IP4).
- **F4 (88) — AC1's "cannot exist" cell does exist, and it changed behavior.** AC1, D039, the grid's `exists = FALSE` cell and a test all rest on "a non-NULL `audio` removes that contradiction". `NA` is non-NULL, and `batch_stream_cell()` maps it to `NULL`, so the contradiction fires. Measured: `picture_in_picture_batch(jobs, audio = NA, audio_codec = "aac")` reports the value error on master and the contradiction on HEAD; likewise `compare_videos_batch(jobs, audio = NA, audio_codec = "aac")`. These are changed cells seven and eight beyond D039's "six cells change". **This is an amendment return** — see below.

**Logged, below threshold (8).** Surfaced, not actioned:

- F9 (78) — the AC6 enumeration test builds its expectation from the same list it asserts over, so reverting every guard move leaves it green.
- F11 (76) — NEWS and both `@param hardware` blocks understate the changed population; subsumed by F1/F2/F4's fixes.
- F5 (68) — `value_guard_blame_regressions()` exempts by message class rather than by `control == TRUE`, so it would also exempt the one non-control cell it exists to catch.
- F10 (65) — `value_guard_ordering()` returns a table for a human to read where the other readers assert emptiness.
- F6 (62) — `value_guard_message_regressions()`'s narrowing drops the nvenc- and `run_guard`-crossed cells too, whose messages are meant to be invariant; measured to hide nothing today.
- F7 (45) — `value_guard_error_class()` matches `nvenc` as a substring, latent until a cell probes an nvenc-family `video_codec`.
- F8 (45) — AC4's `parallel = TRUE` arm now executes identical code to the `FALSE` arm; AC4's own text already says it is not additional evidence.
- F12 (8) — claimed the AC checkboxes were unticked; they had already been ticked when scored.

### Outcome — amendment return on AC1

AC1 states as fact that pip's `audio` against its only contradiction "cannot co-occur" in the argument form, "since a non-NULL `audio` removes that contradiction". F4 measures that false. The criterion instructs recording a cell nonexistent that is now known to exist, so no amount of work satisfies it as written — the criterion is wrong, not the work. That routes to the gated criterion-amendment protocol (`/milestone-implement` step 6) and re-review, per the amendment-return rule.

AC1 and AC6 are unticked: AC1 because its own text is what is wrong, AC6 because its enumeration test encodes the same carve-out and its evidence is therefore contaminated. AC2, AC3, AC4, AC5 and AC7 keep their evidence and their ticks. This is M61's first return, and an amendment return, which counts on its own track and not toward the defect-return count.

### Second round (after the amendment return), 2026-08-08

Re-verified against `origin/master` @ `1d54b20`. `R/ffmpeg.R` is unchanged since the first round, so the runtime change keeps that round's clearance; the delta is the grid, the suite and the prose records.

- **AC1** (re-verified, was unticked) — the grid records **zero** crossed cells as nonexistent. The one nonexistent cell left is M59's site-3 `regions` argument on `anonymize_video_batch()`, which carries `crossed = "none"` and is not a cell of this criterion — AC1 quantifies over the four values in Scope In, and `regions` is not one of them. All 16 non-control contradiction cells report the contradiction, the pip `audio`/argument cell among them: it reads `value` on master and `contradiction` on the branch, asserted at `audio = NA` with `audio = NULL` as its control. The suite pins the reachability condition itself in its own test — `NA` reaches the contradiction, an out-of-range index does not (it carries audio), an in-range index compiles.
- **AC2** (re-verified after widening) — 118 cells, 80 crossed, run against both refs. `value_guard_missing_call()` and `value_guard_dead_controls()` both 0 rows. Eleven cells change which error they report, and NEWS states all eleven in three groups: the six `_batch` scalar-argument cells crossed with a contradiction, the two pip `audio` column cells crossed with the availability and `run` guards, and the three scalar-verb cells. The `audio = NA` cells are called out separately in NEWS because that value behaves opposite to the rest of its own guard.
- **AC3** (re-verified after widening) — `value_guard_refusals()` 0 rows, `value_guard_vacuous()` 0 rows on both refs, `value_guard_message_regressions()` 0, `value_guard_blame_regressions()` 0.
- **AC6** (re-verified, was unticked) — the sentence still appears exactly once in each of the two Rd topics and in NEWS. The enumeration test now requires both forms for every `_batch` (verb, value) pair including pip's `audio`, and the argument form alone for the two scalar verbs, which take no `jobs` table. The carve-out it previously asserted as a measured impossibility is gone.
- **AC4, AC5, AC7** — re-run, unchanged: `devtools::test()` 0 failures / 4620 passing (was 4586), `devtools::check()` 0 errors / 0 warnings / 0 notes, `document()` no diff, residue grep still returns nothing.

Consistency gate re-run: `cairn_validate` exit 0, `pkgdown::check_pkgdown()` "No problems found".
