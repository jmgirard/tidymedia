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
- [x] T8 — Generate the grid's ordering crossings from a declared cross-product
      instead of hand-written cells, with a reader that reports any combination
      with no cell. Discovered sub-task: the thrash-rule remedy for AC2 failing
      twice on a missing hand-written cell.
- [x] T9 — The round-3 record fixes: the reachability biconditional at
      `audio = NULL`, the retired "exactly one value" wording surviving in the
      test file, NEWS's list-item indent and its in-range claim, and the
      `resize`/`scale` scoping in D039.
- [x] T10 — Round-4 gate work at the user's direction: harden the completeness
      reader so a variant cannot stand in for its base guard (F3), and validate
      a control at crossing grain rather than error-class grain (F4). Both
      verified by mutation.

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

- 2026-08-08: question gate on the thrash trigger (b) remedy chose to generate the grid's ordering crossings from a declared cross-product over adding H1's one missing cell by hand or escalating via `/milestone-brief`. The two alternatives the plan gate recorded against are both about the runtime design, which every round has cleared; what thrashes is AC2's enumeration being satisfied by hand, so the remedy aims there. Falsified by a fourth review round finding a missing combination anyway, which would mean the cross-product itself is declared wrong rather than under-populated.
- 2026-08-08: T8 — the grid's ordering cells are now generated from `VALUE_GUARD_PAIRS` x `VALUE_GUARD_FORMS` x `VALUE_GUARD_CROSSINGS`, declared once at the top of `data-raw/value-guard-baseline.R`; each guard supplies only its verb's call shape and its violating value, and a new `crossing` column carries the full crossing id where `crossed` carries its error class. The two contradictions `compare_videos_batch()` carries are separate members rather than one standing for both. New reader `value_guard_uncovered()` re-derives the product and names any combination with no cell; it reads the pairs from a declaration the generator does not use, so a dropped guard reddens, while a crossing dropped from the shared list is the case it cannot catch, recorded in D039.
- 2026-08-08: T8 — H1's uncovered triple (`compare_videos_batch` / `audio` / column / `audio_codec`) exists now by construction, not by being noticed; measured to report the contradiction on both refs, so it is a coverage cell and not a changed one, as review measured. The suite gains its `audio-na/column` case for the same triple. Grid now 128 cells, 90 crossed; `value_guard_uncovered()` 0 rows, and vacuous / refusals / message-regression / blame-regression / missing-call / dead-control readers all 0 on both refs. Fourteen cells change which error they report, against 13 before — the new one is compare's `audio(NaN)` scalar cell, which the generated product reaches for both verbs where the hand-written grid probed only pip's.
- 2026-08-08: T9 — H3: D039's reachability biconditional gains its non-`NULL` half, which `audio = NULL` (its own control) falsified. H4/H5/H6: the retired "exactly one value" / "only at `NA`" wording is gone from the test file's three sites including the `test_that()` name, and that test now asserts both `NA` and `NaN` reach the pairing. H7: NEWS's dropped two-space continuation indent restored — verified discriminating, the last two paragraphs of the entry render OUTSIDE the list item on `HEAD` and inside it after the fix, through `commonmark::markdown_html()`. H8: NEWS no longer claims an in-range index reports a value; it compiles. H9/H10: D039's displaced-check list gains `resize` and scopes both `resize` (compare) and `scale` (pip), and NEWS scopes `resize` the same way.
- 2026-08-08: T9 — H11 (40, logged) earns a ROADMAP candidate row rather than silence: a wrongly-TYPED value still answers differently by form, which M61's Scope Out keeps that way on purpose.
- 2026-08-08: verify slot clean after T8/T9: `devtools::document()` no diff, `devtools::test()` 0 failures / 4647 passing (4 warnings, 5 skips, both unchanged from the branch base), `devtools::check()` 0 errors / 0 warnings / 0 notes. Residue grep over `R/`, `man/`, `NEWS.md`, `tests/` and `data-raw/` still returns nothing.

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
- 2026-08-08: review round 2 returned M61 to in-progress. AC2 failed as written: it requires the grid to cross each of the four guards, in each form, with each front-door error named in M61-D1, and M61-D1's crossing (1) names TWO contradictions for compare — `audio_codec` with no audio, and `resize` across other than two inputs. `direction` is crossed with the first and never the second, and that crossing is a real changed cell (`compare_videos_batch(jobs_3_inputs, direction = "sideways", resize = TRUE)` reports `direction` on `origin/master` and `resize` on the branch) — a twelfth changed cell outside the grid and outside D039's eleven. Two further findings scored >= 80 and are triaged fix-now in the same return: the grid's site-4 comment (G1) and the test file's case-list comment (G2) both still assert the "supplying `audio` at all removes the contradiction" reasoning the first round measured false, each now contradicted by a cell the amendment itself added a few lines below. First defect return; the first round's was an amendment return, counted separately.
- 2026-08-08: defect-return work. G4: the grid and the suite now cross `direction` with compare's SECOND contradiction (`resize` across other than two inputs), in both forms — the crossing M61-D1 names and AC2 requires. G1 and G2: the grid's site-4 comment and the test file's case-list comment no longer assert "supplying `audio` at all removes the contradiction"; each now says what is true of in-range values and points at the NA-ish cell that is the exception. G3 (logged at 62, fixed anyway at the user's direction): `NaN` reaches the same pairing because `batch_stream_cell()` tests `is.na()`, so the grid and the suite probe it, and D039 and NEWS now name the mechanism rather than enumerating values — the two over-generalizations this milestone made have one shape, and the mechanism is what survives both. AC1 is NOT amended: it rests on that mechanism, which was right all along.
- 2026-08-08: two logged findings fixed in the same NEWS paragraph rather than left standing, being factual errors in prose already being edited — G6 (75): `resize` added to the displaced-check list, which D039 had and NEWS did not; G7 (60): `scale` marked as `picture_in_picture_batch()`-only, `compare_videos_batch()` having no such argument.
- 2026-08-08: grid now 124 cells, 86 crossed; 13 cells change which error they report, against 11 before. All readers still empty on both refs. `document()` no diff, `devtools::test()` 0 failures / 4638 passing, `devtools::check()` 0/0/0.
- 2026-08-08: review round 3 returned M61 to in-progress. AC2 failed again (H1, 88): compare's `audio` is crossed with the `audio_codec` contradiction in the scalar form only, leaving `compare_videos_batch` / `audio` / column / `audio_codec` the one uncovered triple. Second defect return; trigger (b) of the thrash rule has fired — the same criterion, twice, each by a new missing hand-written cell. Eight further findings actioned fix-now: D039's NA-ish biconditional is false at `audio = NULL` (H3); the retired "exactly one value" wording survives in three places in the test file including a test name (H4/H5/H6); the round-3 NEWS edit detached two paragraphs from their list item (H7); NEWS gained a false claim that an in-range index reports a value (H8); and the G6/G7 corrections reached NEWS but not D039, with `resize`/`scale` scoping still wrong in both (H9/H10).
- 2026-08-08: review round 4 — all seven criteria re-verified with fresh evidence; consistency gate clean (`cairn_validate` exit 0, `document()` no diff, `pkgdown::check_pkgdown()` clean, `check()` 0/0/0) and CI green on nine checks. Seventeen findings across three lenses, NONE scoring >= 80: the highest is F3 at 76, on the new completeness reader collapsing a variant label to its base value name. No return; the defect-return count stands at two.
- 2026-08-08: approval withheld at the round-4 merge gate; the user directed that F3 (76) and F4 (68) be fixed before merge rather than left logged. Status back to in-progress for T10, then re-review of the affected evidence.
- 2026-08-08: T10 — F3: `value_guard_uncovered()` matched a variant label back to its base value, so `audio(low)` and `audio(NaN)` stood in for `audio`; it now matches the bare value name. Verified discriminating by mutation — deleting the `compare_videos_batch`/`audio` guard spec reports 8 uncovered combinations after the fix against 1 before. F4: `value_guard_dead_controls()` compared a control's raised error at CLASS grain, so a control for one of compare's two contradictions passed when the other fired; a new `value_guard_error_crossing()` classifies at crossing grain and the validator compares against `crossing`. Verified discriminating by mutation — making the `resize` crossing dead reports 7 dead controls after the fix against 4 before, the three newly caught being exactly the `direction` controls the class-grain check let through.
- 2026-08-08: T10 — grid re-run against both refs after the fixes: 128 cells, 90 crossed, every reader still 0 rows, 14 changed cells unchanged. Verify slot clean: `document()` no diff, `devtools::test()` 0 failures / 4647 passing, `devtools::check()` 0/0/0.

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

### Second round — independent review of the amendment delta

One **[O]** reviewer on the delta (`6a1b316..HEAD`); `R/ffmpeg.R` is unchanged in it, so the first round's blame-history and prior-review clearances still cover the runtime change. Thirteen findings, scored by a fresh **[S]** scorer.

**Actioned (>= 80).**

- **G4 (80) — the grid crosses `direction` with only one of compare's two contradictions, and the uncrossed one is a changed cell.** M61-D1's crossing (1) names both the `audio_codec` contradiction and `resize` across other than two inputs. The grid crosses `direction` with the first, `nvenc` and the `run` guard — never with the second. Measured: `compare_videos_batch(jobs_3_inputs, direction = "sideways", resize = TRUE)` reports `direction` on `origin/master` and "`resize` currently supports exactly two inputs" on the branch. A twelfth changed cell, outside the 118-cell grid and outside D039's eleven. The delta added a `direction(resize)` cell for the SCALAR `compare_videos()` while leaving the `_batch` verb uncrossed. **This fails AC2 as written** — see the disposition below.
- **G1 (85) — the grid's site-4 comment still asserts the over-generalization the amendment exists to retire.** `data-raw/value-guard-baseline.R:347-352` reads "a non-NULL `audio` is what MAKES the audio_codec contradiction go away, so crossing this guard with it is the cell that cannot exist" — verbatim the reasoning the first round measured false, now contradicted 85 lines below by the `audio(NA)` cell the delta added. **Triage: fix now.**
- **G2 (82) — the same stale comment inside the test file's case list.** `tests/testthat/test-front-door-ordering.R:52-55` claims the resize contradiction is "the only contradiction on `compare_videos_batch()` that an `audio` case can cross"; the `audio-na/argument` case the delta added a few lines below crosses the `audio_codec` one. **Triage: fix now.**

**Logged, below threshold (10).**

- G6 (75) — NEWS's displaced-check list omits `resize`, which D039's list includes; measured real (`resize = "yes"` displaces `direction` on the branch).
- G5 (70) — no assertion in the suite pins that a scalar `audio = NA` is a value error, so the NA cell and its `NULL` control produce identical messages and the pair could go vacuous if `NA` ever became a legal argument spelling.
- G10 (62) — deleting the compare-side NA case leaves the enumeration test green (its `audio-low` sibling supplies the same stripped key), where deleting its pip twin reddens it; unequal protection from inconsistent id spellings.
- G3 (62) — `NaN` is a second value reaching the same pairing (`is.na(NaN)` is TRUE), so D039's "exactly one value", NEWS's "the one `audio` argument" and AC1's named example are incomplete. The scorer's read, which this review adopts: AC1 rests on the MECHANISM it names — `batch_stream_cell()`'s `is.na()` resolution — which already covers `NaN`, so the criterion is true and its illustration incomplete. Not a second falsification of AC1, and so not a second amendment return.
- G7 (60) — NEWS attributes `scale` to both verbs; `compare_videos_batch()` has no `scale` argument.
- G8 (35) — the scalar verbs' changed error is in NEWS but on no help page; F2's triage scoped the fix to NEWS and the grid.
- G9 (30) — the enumeration test's `"scalar-verb"` form label is not one of the sentence's two forms; a naming choice, not a correctness claim.
- G11 (15) — AC1's "No cell is recorded nonexistent" reads unqualified while site 3's `regions` cell remains; the reviewer's own judgment, which this review shares, is that `regions` is not one of AC1's four values.
- G12 (10), G13 (10) — the first round's F9 and F5, explicitly conceded not materially worse.

### Outcome — defect return on AC2

G4 demonstrates AC2 failing inside the domain of the procedure AC2 names: the grid does not cross `direction`, in either form, with the second contradiction M61-D1 lists. AC2 is unticked; its round-2 evidence line above claimed completeness resting on M61-D1's crossing (1), and that claim is wrong. Every other criterion keeps its evidence.

This is M61's first DEFECT return. The first round's return was an amendment return, which the thrash rule counts on a separate track, so the defect-return count stands at one.

### Third round, 2026-08-08 — and a thrash trigger

One **[O]** reviewer on the round-3 delta (`cb19b85..HEAD`); `R/ffmpeg.R` still unchanged across the whole milestone. Thirteen findings, scored by a fresh **[S]** scorer. Nine actioned.

**The return: H1 (88) — AC2 fails a second time, by a second mechanism of the same shape.** The round-2 fix crossed `direction` with both of compare's contradictions in both forms, but crossed compare's `audio` with `resize` in both forms and with the `audio_codec` contradiction in the scalar form only. Enumerating verb × guard × form × crossing, every triple is covered except `compare_videos_batch` / `audio` / column / `audio_codec`. Measured reachable and live: `compare_videos_batch(jobs, audio = c(NA, 7), audio_codec = "aac")` reports the contradiction on both refs — a coverage gap, not a changed cell, but the identical failure shape round 2 returned on.

**Actioned, triaged fix-now (8).**

- **H3 (85)** — D039's third statement of the NA-ish claim is false at `audio = NULL`, which is its own control. `batch_stream_cell()` returns `NULL` for input `NULL` too (length 0 takes the `else` branch), so "reachable exactly where it resolves the argument to `NULL`" admits a call with no value error at all. The correct biconditional adds *non-`NULL`*.
- **H4 (82), H5 (85), H6 (82)** — the "exactly one value" / "only at `NA`" wording that rounds 2 and 3 removed from D039 and NEWS survives in three places in the test file, including a `test_that()` **name**, each sitting within a few lines of the `audio-nan` case that falsifies it.
- **H7 (87)** — the round-3 NEWS edit dropped a two-space continuation indent, so the last two paragraphs of the entry render *outside* their list item. Confirmed by rendering both refs through `commonmark::markdown_html()`. `check()` and `check_pkgdown()` do not catch it.
- **H8 (85)** — NEWS now says "An index still reports the value, in range or out"; an in-range index compiles silently, as the suite itself asserts. The pre-delta wording was correct and the edit widened it into a falsehood.
- **H9 (75→actioned with H10)**, **H10 (80)** — the G6/G7 corrections landed in NEWS but not in D039, which still omits `resize` and leaves `scale` unscoped; and NEWS's own new list scopes `scale` to pip while leaving `resize` unscoped, though `resize` is compare-only. A work-log line claiming "which D039 had" is itself inaccurate.

**Logged, below threshold (4).**

- H11 (40) — measured real: a wrongly-**typed** value still answers differently by form (`margin = "x"` as an argument reports the contradiction; as a column it reports the type error). The scorer's read, adopted here: Scope Out deliberately keeps every column *type* guard above the sweep, and AC6's sentence promises uniformity for a *value* error, not a type error. Disclosed consequence of an intentional boundary, not a falsification — but it is a real thing a user could hit, and it earns a candidate row rather than silence.
- H2 (22) — the orchestrator's own reading, that AC2's "each front-door error named in the milestone-local decision entry" reaches M61-D1's fourth class. M61-D1 self-scopes ("The displaced errors AC2's grid crosses… three"), D039 says the grid pins those three, and round 2's return used the same narrow reading. Not actioned.
- H12 (32) — AC1's "a particular value" singular; the round-2 disposition already settled that AC1 rests on the mechanism.
- H13 (35) — the enumeration test's key regex lists `na` before `nan`; correct under R's default matcher, latent only under `perl = TRUE`.

### Outcome — defect return on AC2, and thrash trigger (b)

H1 returns the milestone: AC2 fails inside the domain of the procedure it names. That is **defect return two**; the round-1 amendment return counts on its own track, so trigger (a) — the third defect return — has not fired.

**Trigger (b) HAS fired: the same criterion has now failed twice, each by a new mechanism of the same shape.** Round 2 was `direction` × the second contradiction; round 3 is `audio` × the first contradiction in the column form. Both are a hand-written cell that was never written. The rule's remedy is to reconsider the alternative the plan gate recorded against — but the two alternatives it recorded ("move the column sweeps above the contradiction checkers", "preserve today's behavior") are about the runtime design, which has been clean in every round and was cleared by the blame-history and prior-review lenses in round 1. Neither addresses what is actually thrashing, which is AC2's enumeration being satisfied by hand.

The structural read: AC2 demands an exhaustive cross-product (guards × forms × named errors) and the grid supplies it as hand-written `order_add()` calls. Three rounds, three missing combinations, each found by a reviewer rather than by the grid. Generating the crossings from the enumeration would make completeness hold by construction instead of by vigilance.

### Fourth round, 2026-08-08 — after the generated cross-product

Re-verified against `origin/master` @ `1d54b20`, which has not moved since the branch was cut. `R/ffmpeg.R` is unchanged since round 1, so the runtime change keeps that round's clearance; the delta is the grid's generation, the suite and the prose records. Every line below is a command run in this session.

- **AC2** (re-verified, was unticked) — `data-raw/value-guard-baseline.R` run against both refs: 128 cells, 90 crossed, 1 nonexistent. The crossings are no longer hand-written: the grid declares the (verb, value) pairs, the two forms and each verb's crossings and builds the product from them. Coverage enumerated from the grid's own output — 37 distinct (verb, value, form, crossing) triples, which is exactly the product M61-D1 names: five `_batch` (verb, value) pairs x 2 forms x their verb's crossings (compare 4, counting both of its contradictions; pip 3) = 34, plus the three scalar-verb cells. `value_guard_uncovered()`, which re-derives the same product and looks each triple up in the baseline, returns **0 rows** — the reader H1 and G4 would each have failed. Abort kind, blame target and message text all compared; `value_guard_missing_call()` 0 rows, so no abort lost its `call`. Fourteen cells change which error they report, and NEWS states all of them: the six `_batch` scalar-argument cells crossed with a contradiction, compare's `audio(low)` x `resize` and both verbs' `audio(NaN)` cells, the two pip `audio` column cells crossed with the availability and `run` guards, and the three scalar-verb cells.
- **AC1** (re-verified) — every crossed contradiction cell reads `contradiction` after: `sum(crossed == "contradiction" & after != "contradiction")` is 0 over the non-control cells. The only nonexistent cell in the grid is M59's site-3 `regions` argument on `anonymize_video_batch()`, which carries `crossing = "none"` and is not one of AC1's four values. `devtools::test(filter = "front-door")` passes 1934 assertions, including the reachability test, which now asserts both `NA` and `NaN` reach pip's contradiction, an out-of-range index does not, and an in-range index compiles.
- **AC3** (re-verified) — over the grid: `value_guard_refusals()` 0 rows, `value_guard_vacuous()` 0 rows on BOTH refs, `value_guard_message_regressions()` 0, `value_guard_blame_regressions()` 0, `value_guard_dead_controls()` 0. The nvenc invariant holds on every crossed cell: no `nvenc`- or `run_guard`-crossed cell reads anything but `value` after, and the two that read the crossed error *before* are pip's `audio` column, whose front-door guard is new — the change AC4 is about, not an inversion.
- **AC4** (re-verified) — covered by the passing front-door filter above: the index aborts at the front door in all three forms at both `parallel` settings, blaming `picture_in_picture_batch`, with no `pmap`, `In index:` or `aud` in the message or the deparsed call, and a clean column still compiles both rows.
- **AC5** (re-verified) — the residue grep over `R/`, `man/`, `NEWS.md`, `tests/` and `data-raw/` returns nothing, now also covering the "exactly one value" and "only at `NA`" wordings round 3 found surviving in the test file.
- **AC6** (re-verified) — scanning all 78 Rd files on markup-normalized text, exactly `compare_videos_batch.Rd` and `picture_in_picture_batch.Rd` carry the sentence, and it appears once in NEWS. The enumeration test requires both forms for every `_batch` (verb, value) pair and the argument form alone for the two scalar verbs.
- **AC7** (re-verified) — `devtools::document()` leaves the tree clean; `devtools::test()` 0 failures, 4647 passing (4 warnings, 5 skips, both unchanged from the branch base); `devtools::check()` 0 errors, 0 warnings, 0 notes.

### Fourth round — consistency gate

`cairn_validate` exit 0 — all 16 checks PASS, all 8 advisories OK. Toolchain slot: `document()` no diff, generated files unedited, README.Rmd untouched so README.md stays in sync, `pkgdown::check_pkgdown()` "No problems found", NEWS carries the user-visible change with no milestone numbers, no new top-level files, `check()` clean. CI green on all nine checks at PR #64.

### Fourth round — independent review, three lenses and a scorer

Three fresh-context reviewers, distinct evidence bases, aimed at the generated-crossing rework. The **[S] blame-history** lens returned zero defects: it diffed the ~280-line rewrite line-by-line against `HEAD~1` and confirmed M59's sites 1-6, the `mixed` form, the `informative = FALSE` markers and site 3's `exists = FALSE` gap are byte-identical, that every cell earlier rounds added survives under a generated name, and that both narrowed readers are unchanged. The **[S] prior-review** lens returned one finding; its GitHub inline-comment probe was run in this session rather than by the lens and came back EMPTY, so archived `## Review` sections plus this milestone's own three rounds were the whole evidence base. The **[O] diff-bug** lens returned sixteen. A fourth **[S] scorer**, which did not generate them, scored all seventeen against the rubric with the diff and the milestone file in hand.

**Actioned (>= 80): none.** No finding reached the threshold; the highest scored 76. Under the return floor no finding demonstrates an acceptance criterion failing inside the domain of the procedure it names, and none scores >= 90 on a user-facing defect, so the milestone does not return. The defect-return count stands at two and trigger (a) has not fired.

**Logged, below threshold (17).** Surfaced, not actioned:

- F3 (76) — `value_guard_uncovered()`'s label read-back collapses a variant to its base value name, so `audio(low)` and `audio(NaN)` count as covering `audio`. Measured: deleting the whole `compare_videos_batch`/`audio` guard spec (8 cells + 8 controls) leaves the reader reporting ONE row, with 7 of the 8 lost combinations absorbed by the variant. The reader's own comment claims a dropped pair is what it catches, and for a value carrying variants that is only partly true.
- F4 (68) — `value_guard_dead_controls()` validates a control against the coarse `crossed` class rather than the new `crossing`, so a dead `contradiction:resize` control would pass on the `audio_codec` contradiction. Harmless today (`resize` cells default `audio_codec = "copy"`, which raises no contradiction) but a latent re-run of G4's conflation.
- F6 (58) — M61-D1's "How each is made uniform" still says the move is a DELETION for `margin` and compare's `audio`, where D039 and the code both say the guards moved. Prose inconsistency between two records on this branch; the work log records the refinement.
- F7 (55) — D039's "three of the four" undercounts: `check_batch_vocab_col()` resolves column-over-argument the same way, so all five scalar guards refuse a call whose column overrides them (measured on both refs).
- F10 (55) — round 1's F9 restated: the AC6 enumeration test builds its expectation from the list it asserts over, so reverting every guard move leaves it green. Unfixed across four rounds, and unchanged by this delta.
- F8 (50) — NEWS's "where it used to be told about the value" is unqualified; compare's `audio` UPPER bound reports the `resize` contradiction on both refs, that bound having already sat below the sweep. D039 discloses the asymmetry under "Which bound, not only which value".
- F5 (48) — message-text comparison is scoped to the 38 uncrossed cells; the 90 crossed ones are compared on error class. Round 1's F6 at a larger population; the scorer's read, adopted here, is that a crossed cell's text is EXPECTED to change, so class is the meaningful comparison.
- P1 (46) — `value_guard_uncovered()` derives its expectation from the same crossing and form declarations the generator consumes, so a crossing dropped from that list shrinks both together. Disclosed in the file, in D039 and in the work log; it is the reader's stated boundary rather than an oversight.
- F13 (45) — `value_guard_error_class()` matches `nvenc` as a bare substring; latent, and more consequential now that 90 cells are read through it.
- F16 (42) — `value_guard_blame_regressions()` exempts by message class rather than by `control == TRUE`. Round 1's F5, unfixed, no live cell wrongly exempted.
- F2 (42) — the reader's blind spot covers a dropped FORM as well as a dropped crossing; the file's comment claims a form is caught, which holds when the builder skips one, not when the declaration loses it.
- F12 (42) — the `audio` x `audio_codec` pair probes `NA` in the scalar form and `c(NA, bad)` in the column form, so the two forms carry different violating values; no cell probes `c(NA, NA)`. Measured to report the contradiction on both refs, so coverage shape rather than a changed cell.
- F14 (40) — the wrongly-TYPED asymmetry, re-measured, with the observation that its direction is now inverted from the one this milestone removed. Already routed to a ROADMAP candidate row this round.
- F9 (38) — M61-D1's fourth-class list leaves `resize` and `scale` unscoped, where D039 and NEWS now scope both.
- F15 (30) — AC4's `parallel = TRUE` arm executes identical code to the `FALSE` arm now that the abort precedes `ffm_batch()`; AC4's own text says it is not additional evidence.
- F1 (22) — every "empty is the evidence" reader is also empty on the pre-milestone ref. The scorer's read, adopted here: AC2's mechanism is the before/after comparison across two refs, which does surface the fourteen changed cells; a single-ref self-comparison being vacuous is not that mechanism failing.
- F11 (22) — the enumeration key regex lists `na` before `nan`; correct under R's default matcher, latent only under `perl = TRUE`.

### Outcome — fourth round passes

All seven criteria carry fresh evidence, the consistency gate is clean, CI is green, and no finding reached the actioned threshold. AC2 — the criterion that failed in rounds 2 and 3, each time on a hand-written cell nobody wrote — now holds by construction rather than by vigilance: the grid generates the product and `value_guard_uncovered()` returns zero rows over 37 triples. The thrash remedy is judged to have worked on the axis it was aimed at; F3 and P1 record the two ways the new reader can still be fooled, neither of which is a present incompleteness.

### Fourth round — addendum after the gate, 2026-08-08

Approval was withheld at the merge gate and the two highest sub-threshold findings were fixed at the user's direction rather than left logged (T10). Both were verified discriminating by mutation — the fix is shown to catch what the old code let through, not merely to be present.

- **F3, fixed.** `value_guard_uncovered()` matched a variant label back to its base value name, so `audio(low)` and `audio(NaN)` counted as covering `audio`. It now matches the bare name. Mutation: deleting the whole `compare_videos_batch`/`audio` guard spec reports **8** uncovered combinations after the fix, against **1** before — the eight actually owed.
- **F4, fixed.** `value_guard_dead_controls()` compared a control's raised error at class grain, so a control naming one of compare's two contradictions passed when the other fired. A new `value_guard_error_crossing()` classifies at crossing grain and the validator compares against `crossing`. Mutation: making the `resize` crossing dead reports **7** dead controls after the fix, against **4** before; the three newly caught are exactly the `direction` controls, which had fallen through because the substituted contradiction shares their class.

Re-verified after the fixes: grid 128 cells / 90 crossed against both refs, `value_guard_uncovered()` 0 rows, every other reader 0 rows, 14 changed cells unchanged, `contradiction` cells all reading `contradiction` after. `cairn_validate` exit 0; `document()` no diff; `devtools::test()` 0 failures / 4647 passing; `devtools::check()` 0 errors / 0 warnings / 0 notes. No criterion's evidence is disturbed — the fixes strengthen AC2's readers without changing what the grid contains or what any cell reports.

The fifteen remaining findings stay logged as recorded above.
