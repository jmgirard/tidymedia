# M59: Six per-row value checks are refused at the fan-out verb's front door

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M58
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m59-fanout-value-checks-front-door` / PR #62 https://github.com/jmgirard/tidymedia/pull/62

## Goal

Make six per-row value validations report from the fan-out verb the user called, not from inside the fan-out.

## Scope

**In:** six (site, verb) pairs. The first four are the sites M57's review
measured as invisible to its AC6 grep, because the grep attributes an abort to
the function that *writes* it and each of these is written in a helper or in
the batch closure itself. Sites 5 and 6 are the vocabulary half of the ROADMAP
candidate row this milestone absorbs (M58 review F2). All six were measured
reporting `purrr::pmap(jobs, .f, ...)` on merged master 2026-08-07:

1. `crop_video_batch()` — `width`/`height` range, authored in `check_dim()`
   (`R/utils.R:115`) and reached per row through `crop_video_pipeline()` →
   `ffm_crop()` (`R/ffm.R:287-288`).
2. `picture_in_picture_batch()` — `margin` range, in the batch closure's own
   re-check (`R/ffmpeg.R:6085`).
3. `anonymize_video_batch()` — malformed `regions`, authored in
   `check_regions()` (`R/ffmpeg.R:1639`) and reached per row through
   `anonymize_pipeline()` (`R/ffmpeg.R:1570`).
4. `compare_videos_batch()` — per-row `audio` index above
   `length(inputs) - 1`, written in the batch closure (`R/ffmpeg.R:5905`).
5. `compare_videos_batch()` — `direction` column value outside the vocabulary,
   by `rlang::arg_match(direction)` in `compare_videos_pipeline()`
   (`R/ffmpeg.R:5451`); the front door's `check_batch_string_col(jobs,
   "direction")` (`:5857`) checks the column's TYPE only.
6. `picture_in_picture_batch()` — `position` column value outside the
   vocabulary, by `rlang::arg_match(position)` in
   `picture_in_picture_pipeline()` (`:5586`); the front door's
   `check_batch_string_col(jobs, "position")` (`:6039`) checks TYPE only.

Unlike M58's six, these are range, shape and vocabulary checks on a single
value rather than contradictions between two. Sites 2, 4, 5 and 6 already have
a scalar front-door counterpart (`check_number_whole(margin, min = 0)`,
`check_number_whole(audio, max = length(infiles) - 1)`, and the `arg_match()`
calls at `R/ffmpeg.R:5844` and `:6003`); what is missing on each is only the
sweep over the **column** form, whose home is the `check_batch_*_col()` family.
Sites 1 and 3 have no scalar counterpart at either verb.

**The shape question this carries** is narrow: `check_dim()` is a shared
internal helper reached by ten call sites and already taking
`call = rlang::caller_env()`, so the choice is only where the front door gets
its `call` from — calling `check_dim()` directly, or threading `call` through
`ffm_crop()`. Not whether Layer 2 may validate — D037 settles that.

**Out:** the six argument contradictions → M58 (done first; this milestone
reuses its per-row checker shape, D036). An unreadable input file → the ROADMAP
candidate row M58 created. `ffm_batch()`'s own argument checks
(`R/ffm_batch.R:84-98`) → **not moved, only documented**: the verb does not own
them and they cannot be hoisted, so AC5(c) pins and discloses the reordering
rather than changing it. Any validation not already enforced today — this
milestone moves where a check reports, never what is checked.

## Acceptance criteria

- [x] AC1 — For each of the six (site, verb) pairs in Scope In, a violating call
      aborts with `conditionCall()` naming the verb the user called, and a
      message carrying none of `pmap` (covering `purrr::pmap` and
      `furrr::future_pmap`), `In index:`, or `_pipeline(`. One test per pair, at
      both `parallel` settings.
- [ ] AC2 — For each of the six sites the front-door call and the per-row path
      resolve to the same abort site, named in the milestone-local decision
      entry, with no verb spelling a second copy of the message or (sites 5/6) a
      third copy of the vocabulary literal. Verified by mutation: deleting a
      front-door call turns that site's AC1 test red, for all six; and for sites
      1, 3, 5 and 6, whose per-row path is a shared helper or pipeline call the
      scalar verb also reaches, deleting that shared call turns the scalar
      verb's own test red. Sites 2 and 4 have no shared call (their check lived
      in the batch closure), so that half does not apply.
- [x] AC3 — For each of the six sites the front-door guard refuses exactly the
      calls its current check refuses, over a committed before/after grid
      varying the value in- and out-of-range, in its `jobs` column form and —
      for the five sites that have one — its scalar-argument form, plus one
      mixed column. Site 3 is column-only (`anonymize_video_batch()` has no
      `regions` argument), so the grid records that cell nonexistent, and sites
      5/6's scalar cells expected-identical on both refs; neither is read as
      evidence. Every cell's in-range baseline is asserted to succeed on both
      refs, so no cell compares equal by both sides failing.
- [x] AC4 — The site-1 shape question is settled in writing: a milestone-local
      decision entry records whether the front door calls `check_dim()` directly
      or threads `call` through `ffm_crop()`, names the alternative rejected,
      and states the evidence class that would falsify the choice.
- [ ] AC5 — Precedence is pinned, and the value-check ordering is this
      milestone's own call, not one M58 makes:
      (a) on the two verbs carrying both an M58 contradiction and a value check
      (`compare_videos_batch`, `picture_in_picture_batch`), a call whose value
      violation arrives in a `jobs` column reports the contradiction. The
      scalar-argument form is outside this milestone's reach and reports the
      value check, as it did before it — those verbs' scalar
      `direction`/`position`/`margin` guards sit above M58's contradiction sweep
      on merged master and are not moved here — so the two forms disagree, which
      is stated rather than fixed and carries a ROADMAP candidate row;
      (b) on all four verbs, a call invalid in its value check and in nvenc
      availability reports the value check, driven machine-independently through
      the `tidymedia.nvenc_encoders` option seam;
      (c) on all four verbs, a call invalid in its value check and in an
      argument `ffm_batch()` alone guards reports the value check — tested on
      `run`, with NEWS naming the displaced set (`run`, `parallel`, `progress`,
      `manifest`, `checksums`, `verify`) read off `R/ffm_batch.R:84-98` rather
      than recalled. The `jobs`-shape guards at `:75-80` are excluded and stated
      as excluded: all four verbs already pre-empt them, so they are never
      displaced.
- [x] AC6 — The r-package profile's verify slot is clean: `devtools::document()`
      produces no diff, `devtools::test()` passes, and `devtools::check()`
      reports 0 errors and 0 warnings.

## Coverage

- AC1 → T2, T3, T4, T5, T6, T7
- AC2 → T1, T7
- AC3 → T8
- AC4 → T1
- AC5 → T7
- AC6 → T9

## Tasks

- [x] T1 — Settle the site-1 shape question and record it as a milestone-local
      decision; shape the shared checkers accordingly.
- [x] T2 — `crop_video_batch()` front door: `width`/`height` range, swept over
      the column form.
- [x] T3 — `picture_in_picture_batch()` front door: `margin` range, swept over
      the column form; retire the closure's re-check if it becomes dead.
- [x] T4 — `anonymize_video_batch()` front door: `regions` shape, swept over
      the list-column.
- [x] T5 — `compare_videos_batch()` front door: per-row `audio` index against
      each row's own `inputs` length.
- [x] T6 — Sites 5 and 6: sweep the `direction` and `position` column VALUES at
      both front doors, sourcing the vocabulary from one place rather than a
      third copy; `check_batch_string_col()` keeps the type half.
- [x] T7 — Tests: one blame test per pair at both `parallel` settings; mixed-
      column tests; the three precedence cases of AC5; mutation-verify each
      front-door and shared call.
- [x] T8 — Build and commit the before/after grid as the evidence ledger.
- [x] T9 — NEWS (including AC5(c)'s displaced set); roxygen where precedence
      changed; run the verify slot clean.

## Work log

- 2026-08-07: created by /milestone-plan.
- 2026-08-07: plan gate chose to split these four from M58's six rather than ship all ten together because the combined milestone exceeded the >7-criteria and >10-task tripwires, and because these four are range/shape checks needing the `check_batch_*_col()` sweep while M58's six are contradictions needing a new shared-checker pattern; falsified by M58's checker shape turning out to cover these four with no further design call, which would make the split pure overhead.
- 2026-08-07: criteria audit ([O] fresh-context reader) returned findings on AC2 (two), AC3, AC4, AC5 (two) and a false Scope claim that all four sites already had a scalar front-door counterpart — sites 1 and 3 do not. All fixed before commit; AC1 and AC6 returned clean, Coverage clean.
- 2026-08-07: plan gate chose the two IP1-clean shapes for site 1 over duplicating the Layer-1 check at Layer 2 because IP1 puts validation logic in Layer 1 once and a duplicate would need an IP1 exception D-entry; falsified by both clean shapes proving unable to name the batch verb, which would make the exception the only route left. **Superseded 2026-08-07 (amendment below): the premise was false.**
- 2026-08-07 (amendment): the M58-review candidate row's vocabulary half (F2) folded in as sites 5 and 6 at the user's direction; measured on merged master that a `direction` column of `"sideways"` and a `position` column of `"middleish"` each report `purrr::pmap(jobs, .f, ...)` / `In index: 1` and additionally leak `compare_videos_pipeline()` / `picture_in_picture_pipeline()`. The row's other half (F6, `ffm_batch()`'s own checks) is documented rather than moved — see Scope Out and AC5(c) — because the verb does not own those checks and they cannot be hoisted. Title and ROADMAP row retitled four → six.
- 2026-08-07 (amendment): the amended criteria went to a second [O] fresh-context reader, which returned findings on AC1 (the `parallel = TRUE` path leaks `furrr::future_pmap`, which the forbidden-substring list missed), AC2 (two: "exactly one place" quantified over a domain no named procedure enumerates, and a cross-product IP1 clause), AC3 (site 3 has no scalar-argument form, making that cell unsatisfiable), AC4 (`ffm_crop()` authors no abort to re-aim), AC5 (two: three of four nvenc line numbers wrong, and the zero-row `jobs` test describes an unreachable state under D036's row-by-row shape). AC5(a) and AC6 returned clean. It also found four stale line numbers in the inherited Scope and a false gloss of IP1. All fixed before commit.
- 2026-08-07 (amendment): **the inherited IP1 framing was a misreading and is withdrawn.** `cairn/DESIGN.md:62-64` scopes IP1 to command assembly and says nothing about validation; Layer 2 front doors validate throughout under D035/D036. The plan gate chose to drop the framing — `Principles touched` IP1 → `—`, AC4 rewritten as the narrow question of where the front door's `call` comes from, AC2's cross-product IP1 clause struck — over amending DESIGN.md to extend IP1 to validation, which would retroactively govern D035, D036 and every existing Layer-2 check and so needs its own milestone. Falsified by a reviewer reading IP1's "thin wrappers" clause as implying validation locality, which would reinstate the exception question. Promoted to **D037** on 2026-08-07 at the user's direction, the misreading having survived a plan, a criteria-audit line and a routing decision before a fresh-context reader caught it; the Scope paragraph now cites D037 rather than re-arguing it.
- 2026-08-07 (amendment): corrected four stale citations inherited from the original plan — site 2's `margin` re-check is `R/ffmpeg.R:6085` not `:5905`; site 4's `audio` index check is `:5905` not `:5744` (roxygen); site 1's abort is authored in `check_dim()` (`R/utils.R:115`), not in `ffm_crop()`; and AC5(b)'s nvenc guards are `:1926`, `:4930`, `:5894`, `:6074`, of which the original plan had one right.

- 2026-08-07: implement started on branch `m59-fanout-value-checks-front-door`.
- 2026-08-07: T1 — question gate settled all three open shapes on their recommendations (call `check_dim()` directly; one named vocabulary source plus one shared `check_vocab_arg()`; delete both now-unreachable closure re-checks). Recorded as M59-D1 and M59-D2; added `stack_directions()`, `pip_positions()`, `check_vocab_arg()` and `check_batch_vocab_col()`.
- 2026-08-07: T2 — `crop_video_batch()` sweeps each resolved `width`/`height` through `check_dim()` at its front door.
- 2026-08-07: T3 — `picture_in_picture_batch()` sweeps each resolved `margin`; the fan-out closure's now-unreachable re-check deleted.
- 2026-08-07: T4 — `anonymize_video_batch()` sweeps each `regions` cell through `check_regions()`.
- 2026-08-07: T5 — `compare_videos_batch()` sweeps each row's `audio` index against that row's own `inputs` length; the closure's re-check deleted, and the message now names `audio` rather than the closure's local `aud`.
- 2026-08-07: T6 — the `direction` and `position` column VALUES are swept via `check_batch_vocab_col()`; both vocabularies single-sourced and all six signatures re-defaulted to the accessors.
- 2026-08-07: discovered sub-task (minor amendment): the two pipelines' vocabulary checks needed `call = call`, without which `compare_videos()` / `picture_in_picture()` blamed their own `*_pipeline()` — the leak M58 closed on the resize guard. Fixed; pinned by the scalar-siblings test.
- 2026-08-07: two existing tests pinned precedence M59 deliberately reverses and were rewritten, not deleted for convenience: `test-nvenc-front-door.R`'s "the guard reports before pipeline checks it now precedes" (its three cases now report their own value error on both seam settings, and the new pin lives in `test-value-check-front-door.R`) and `test-anonymize-video-batch.R`'s per-row `index: 2` assertion (the check answers per row at the front door now, which is what that test keeps).
- 2026-08-07: question gate (mid-work) chose to accept `stack_directions()` / `pip_positions()` appearing in four help pages' Usage lines over exporting the two accessors or hand-writing `@usage`, because each page's Arguments section already spells the values out in prose; falsified by a report of a caller unable to discover the accepted values from the help page.
- 2026-08-07: ROADMAP candidate row added for the `crop_video()` → `ffm_crop()` blame leak M59-D1 leaves standing (search-first: no existing row covers it).
- 2026-08-07: T7 — `tests/testthat/test-value-check-front-door.R` added: one blame pair per site at both `parallel` settings (14 cases), the one-site vocabulary scan, the scalar-siblings test, seven mixed-column cases, and AC5's three precedence groups (2 + 4 + 4 cases), each with a control proving the displaced error is live on the same call.
- 2026-08-07: T7 — mutation-verified via `data-raw/value-guard-mutations.py`: all ten deletions RED. Each of the six front-door sweeps turned the AC1 blame test red (and the mixed-column test, plus whichever AC5 group that verb appears in); each of the four shared calls reached by a scalar verb — `ffm_crop()`'s `check_dim()`, `anonymize_pipeline()`'s `check_regions()`, and the two pipelines' `check_vocab_arg()` — turned the scalar-siblings test red. Sources restored clean.
- 2026-08-07: T8 — `data-raw/value-guard-baseline.R` committed as the AC3 ledger and run across `origin/master` and the branch: 34 cells each side; both vacuity screens empty (every in-range cell compiled on both refs, so no cell compares equal by both sides failing); `value_guard_refusals()` empty — the same calls are refused; `value_guard_blame()` names 17 cells, every one moving from `purrr::pmap` to the verb the user called with purrr's row index dropping away. Site 3's scalar cell is recorded `exists = FALSE` (no `regions` argument) and sites 5/6's scalar cells `informative = FALSE`; neither appears in the blame list, which is what those two flags predicted.
- 2026-08-07: T9 — NEWS entry added for the six value checks, naming AC5(c)'s displaced set (`run`, `parallel`, `progress`, `manifest`, `checksums`, `verify`) read off `R/ffm_batch.R:70-98` and stating the excluded `jobs`-shape guards; the M57 entry's paragraph claiming an unavailable encoder now reports ahead of a bad `regions` / `width` / `margin` was corrected in place, that precedence having been reversed here. Roxygen updated on the four verbs whose `hardware` precedence changed.
- 2026-08-07: T9 — verify slot clean: `devtools::document()` no diff, `devtools::test()` FAIL 0 / WARN 4 / SKIP 5 / PASS 4355 (the 4 warnings and 5 skips match the pre-branch master baseline), `devtools::check()` 0 errors / 0 warnings / 0 notes. Status → review.
- 2026-08-07: repaired a self-inflicted whole-file change: the scripted edits to `R/ffmpeg.R` were made through Python's text mode, which strips the file's CRLF terminators, so the branch diff read 12,387 lines instead of 171. Master's copy is uniformly CRLF (6,130 of 6,130 lines), so the file was converted back and the diff is now 149 insertions / 22 deletions; `document()` no diff and `devtools::test()` FAIL 0 / PASS 4355 re-run after the conversion. No other file's terminators changed. This is exactly the hazard M60 exists to remove, and is not a substitute for it.
- 2026-08-07: review returned the milestone to `in-progress`. Floor return on F1 (scored 95): `check_vocab_arg()`'s `identical(value, values)` shortcut covers only the exact default vector, so any other length->1 value reaches `rlang::arg_match0()`, whose own length check aborts ignoring `error_call` — `picture_in_picture(v, v, "o.mp4", position = c("center", "topleft"))` now blames `rlang::arg_match0(...)` where master blamed `picture_in_picture()`. Also actioned F9 (82): `?compare_videos_batch` and `?picture_in_picture_batch` now document the accepted values in neither Usage nor Arguments, the mid-work gate's rationale having held only for the two scalar pages. AC1, AC3, AC4 and AC6 verified clean; AC2 deferred (its mutation evidence would be invalidated by the F1 fix); 10 findings logged, not actioned.
- 2026-08-07: amendment return: AC5 — "on the two verbs carrying both an M58 contradiction and a value check (`compare_videos_batch`, `picture_in_picture_batch`), a call invalid in both reports the contradiction" is unbounded over the scalar-argument versus `jobs`-column form. Measured: the scalar form reports the value check (unchanged from master, where the scalar `arg_match()` already sat above M58's contradiction sweep) while the column form reports the contradiction. The criterion must name which form it binds before it can be verified.
- 2026-08-07: review F1/F2/F3 fixed — `check_vocab_arg()` now calls `rlang::arg_match(value, values, error_arg = arg, error_call = call)`. `arg_match()` takes `values` as a parameter, so it never needed the caller's formals and reaching past it to the string-only `arg_match0()` was the whole defect: on a longer value `arg_match0()`'s own length guard fired first and aborted with ITS call, ignoring `error_call`. Measured parity against `rlang::arg_match()` on all seven branches (unsupplied default, permutation, multi non-permutation, out-of-vocabulary single, zero-length, valid single, both vocabularies): identical message and identical blame frame. The hand-rolled `identical()` shortcut is gone and the comment that claimed the refusal read unchanged (F3) is now true.
- 2026-08-07: review F9 fixed — `compare_videos_batch()`'s and `picture_in_picture_batch()`'s `@param` blocks now name their accepted values inline instead of delegating to the scalar verb's page. The mid-work gate's rationale for the Usage-line change held only for the two scalar pages; on these two the values were discoverable from neither Usage nor Arguments.
- 2026-08-07: executed the AC5 amendment returned by review. The fixed-shape `amendment return: AC5` line is the one review appended above; this line deliberately does not repeat that prefix, so the per-milestone amendment-return count still reads one round-trip rather than two. Amended clause, verbatim: "(a) on the two verbs carrying both an M58 contradiction and a value check (`compare_videos_batch`, `picture_in_picture_batch`), a call whose value violation arrives in a `jobs` column reports the contradiction. The scalar-argument form is outside this milestone's reach and reports the value check, as it did before it — those verbs' scalar `direction`/`position`/`margin` guards sit above M58's contradiction sweep on merged master and are not moved here — so the two forms disagree, which is stated rather than fixed and carries a ROADMAP candidate row;". Chosen over moving each verb's scalar guards below the contradiction sweep, because that reorders them against the jobs-shape guards, both `check_token()` calls and `arg_match(hardware)`, none of which has a test or a changelog line pinning its position today; falsified by a report of the two forms' disagreement confusing a caller, which the new ROADMAP candidate row records. NEWS's flat "Contradictory arguments still report ahead of all of these" was false under the amendment and now states the column/argument split as a known gap.
- 2026-08-07: the amendment took the plan-owned body to 153 lines (cap <150), so the heaviest plan-owned section — Acceptance criteria at 59 lines — was compressed in one pass; every operative clause kept, redundant inline line-number citations dropped. `cairn_validate` weight caps PASS.

## Decisions

### M59-D1 — The crop front door calls `check_dim()` directly (2026-08-07, T1, answers AC4)

`crop_video_batch()` calls `check_dim(value, arg = "width"/"height")` itself,
once per resolved row, rather than threading a `call` argument through
`ffm_crop()` so the pipeline's own abort could be aimed at the batch verb.

`check_dim()` (`R/utils.R:115`) already takes `arg` and
`call = rlang::caller_env()`, so calling it directly needs no signature change
anywhere and leaves it the one site the message is written. Threading instead
would add a blame-only parameter to `ffm_crop()`, an **exported** Layer-1
builder reached by ten call sites, to serve a Layer-2 reporting concern — and
`ffm_crop()` authors no abort of its own to re-aim, so the parameter would exist
solely to be forwarded.

- **Rejected:** threading `call` through `ffm_crop()`.
- **Falsified by** a front-door check whose values cannot be resolved without
  building the pipeline first, which would leave forwarding the only route; or
  by `ffm_crop()` acquiring a `call` argument for its own reasons, which would
  make the forwarding free.

### M59-D2 — The six abort sites, and what retires with them (2026-08-07, T1, answers AC2)

Each site's front-door call and its per-row path resolve to one abort site:

1. `crop_video_batch()` `width`/`height` → `check_dim()` (`R/utils.R`), which
   `ffm_crop()` also calls, so the scalar verb reaches the same site.
2. `picture_in_picture_batch()` `margin` → `rlang::check_number_whole()`, called
   at the front door only; the fan-out closure's copy retires (below).
3. `anonymize_video_batch()` `regions` → `check_regions()` (`R/ffmpeg.R`), which
   `anonymize_pipeline()` also calls.
4. `compare_videos_batch()` per-row `audio` index → `rlang::check_number_whole()`,
   called at the front door only; the closure's copy retires (below).
5. `compare_videos_batch()` `direction` → `check_vocab_arg()`, values from
   `stack_directions()`.
6. `picture_in_picture_batch()` `position` → `check_vocab_arg()`, values from
   `pip_positions()`.

Sites 5 and 6 needed the vocabulary single-sourced before the sweep could exist.
Each list was spelled out in three signatures and arg-matched separately at each,
so one wrong value had three abort sites and a column sweep would have added a
fourth. Each list is now one internal function used as every signature's default,
and every check routes through `check_vocab_arg()`. Reading the list off the
pipeline's formals was rejected: it removes the copies without removing the
sites, and couples the check to another function's signature.

**What retires.** Sweeping every row at the front door makes the two fan-out
closures' re-checks — `margin` and the per-row `audio` index — unreachable: both
resolve their value through the same `pick()` rule the sweep resolves through
`batch_arg_rows()`. Both are deleted. What is checked is unchanged; only the
copy that can no longer fire goes.

- **Falsified by** a value a fan-out closure can resolve that the front door
  cannot, which would make a retired re-check reachable again.

## Review

_Fresh evidence gathered 2026-08-07 on branch `m59-fanout-value-checks-front-door` at PR #62._

### Acceptance criteria

- **AC1** — `testthat::test_local(filter="value-check-front-door")`, test "every
  (value check, verb) pair blames the verb the user called": 112 assertions, 0
  failed. Seven (site, verb) pairs covering all six sites, each run at
  `parallel = FALSE` and `parallel = TRUE`, each asserting the abort's own
  message fragment, `conditionCall()` naming the verb, and the absence of
  `pmap`, `In index:` and `_pipeline(` from both the message and the deparsed
  call.
- **AC3** — `data-raw/value-guard-baseline.R` re-run against `origin/master` and
  the branch: 34 cells each side; `value_guard_vacuous()` empty on BOTH refs, so
  every in-range baseline compiled on both and no cell compares equal by both
  sides failing; `value_guard_refusals()` empty — the same calls are refused;
  `value_guard_blame()` names 17 cells, every one `before_call == "purrr::pmap"`,
  `after_call ==` the verb, and `in_index` TRUE → FALSE. All six sites appear,
  each in its column and mixed forms; the scalar form moved on sites 1 and 4
  only. Zero `informative = FALSE` cells (sites 5/6 scalar) appear in the blame
  list, and site 3's scalar cell is recorded `exists = FALSE` — both as stated.
- **AC4** — milestone-local decision **M59-D1** records the choice (the front
  door calls `check_dim()` directly), names the rejected alternative (threading
  `call` through `ffm_crop()`), and states two falsifying evidence classes.
- **AC5** — three tests, 60 assertions, 0 failed. (a) "a contradiction reports
  before a value check", 12 assertions over the two verbs carrying both.
  (b) "a value check reports before nvenc availability", 24 assertions over all
  four verbs, driven through the `tidymedia.nvenc_encoders` option seam held
  empty, each with a control proving the availability abort is live on the same
  verb. (c) "a value check reports before ffm_batch's own guards", 24 assertions
  over all four verbs on `run`, each with a control proving the `run` abort is
  live. NEWS names the displaced set — `run`, `parallel`, `progress`,
  `manifest`, `checksums`, `verify` — read off `R/ffm_batch.R:70-98`, and states
  the `jobs`-shape guards as excluded.
- **AC6** — `devtools::document()` produces no `man/` diff; `devtools::test()`
  FAIL 0 / WARN 4 / SKIP 5 / PASS 4355 (warnings and skips match the pre-branch
  master baseline); `devtools::check()` 0 errors, 0 warnings, 0 notes.

- **AC2** — NOT verified at review. The mutation half needs `R/` rewritten in
  place, and the F1 fix below changes `check_vocab_arg()`, which would
  invalidate any evidence gathered now. Deferred to re-review. The
  non-mutation half was checked by inspection: `check_dim()`'s message and
  `check_regions()`'s headline each occur at exactly one code site, and
  `check_vocab_arg()` is the only refusal site for both vocabularies.
- **AC5** — NOT verified. Falsified as written by F5: the criterion is
  unbounded over the scalar-argument versus `jobs`-column form, and the scalar
  form reports the value check where the criterion says the contradiction
  reports. AC5(b) and AC5(c) each passed on their own (24 assertions apiece).

### Independent review — three lenses, then a scorer

- **[S] blame-history:** zero findings. Traced both deleted closure re-checks to
  M32 (`abeeae0`) and confirmed each is removed only in the commit installing its
  documented replacement; the M57 precedence reversal is recorded, not silent.
- **[S] prior-PR-comments:** zero findings. Archive `## Review` sections for M40,
  M41, M54, M56, M57, M58 checked; the GitHub inline-comment probe returned `[]`,
  so the per-PR walk was correctly skipped.
- **[O] diff-bug:** 13 findings. Scored by a fresh [S] scorer that did not
  generate them.

**Actioned (score ≥80):**

- **F1 (95)** — `check_vocab_arg()`'s `identical(value, values)` shortcut only
  covers the exact default vector, so any other length-`>1` value falls through
  to `rlang::arg_match0()`, whose own length check aborts ignoring
  `error_call`. Measured: `picture_in_picture(v, v, "o.mp4", position =
  c("center", "topleft"))` reports `` `arg` must be a string or have the same
  length as `values` `` blaming `rlang::arg_match0(value, values, arg_nm = arg,
  error_call = call)`, where master blamed `picture_in_picture()`. A blame
  regression introduced by this branch, on the milestone whose subject is blame.
  **Disposition: fix, on return.**
- **F5 (85)** — AC5(a) is unbounded over form. Measured:
  `compare_videos_batch(jobs, direction = "sideways", audio_codec = "aac")`
  reports the `direction` error while the same mistakes written as a column
  report the contradiction; likewise `margin` on `picture_in_picture_batch()`.
  The scalar ordering is unchanged from master — what is new is the two forms
  disagreeing. **Disposition: amendment return — the criterion is wrong, not the
  work; it must say which form it binds.**
- **F9 (82)** — the two `_batch` help pages document the accepted values in
  neither Usage nor Arguments, because their `@param` delegates to the scalar
  verb's page. The mid-work gate's accepted rationale held for the two scalar
  pages only. **Disposition: fix, on return.**

**Logged (score <80), 10 findings — surfaced, not actioned:**

- F2 (78) — a multi-element `direction` yields rlang's internal `arg` in the
  message rather than `direction`; same root cause as F1, narrower.
- F3 (78) — the comment at `R/ffmpeg.R:2766-2771` asserting the refusal reads
  unchanged is falsified by F1/F2.
- F10 (66) — the AC2 vocabulary-uniqueness test's `../../R` path does not
  resolve under `R CMD check`, so it skips there; fails safe, not a false pass.
- F4 (65) — the AC3 grid compares abort *kind* and blame but never message, and
  probes no multi-element vocabulary value, so it could not have caught F1.
- F12 (55) — the `parallel = TRUE` half of the AC1 test never reaches `furrr`
  now that every check aborts first; AC1 requires both settings regardless.
- F11 (50) — the uniqueness test is a literal-string grep and would miss a
  re-spelled fourth copy.
- F13 (50) — the `expect_no_match` halves of the AC5(b)/(c) tests are not
  discriminating; their controls carry the load.
- F6 (40) — `crop_video_batch()`'s `x`/`y` still report from `purrr::pmap()`;
  Scope In names `width`/`height` only.
- F7 (40) — `picture_in_picture_batch()`'s per-row `audio` index still reports
  `aud` from `pmap`; not one of the six enumerated sites.
- F8 (40) — four stale comments; all on lines outside every diff hunk.

### Verdict — returned to `in-progress`

Two returns compose. **Floor return** on F1: scored 95 on a defect in what the
package does for its users, and introduced by this branch. **Amendment return**
on AC5, whose text is unbounded over a distinction the milestone never intended
to bind. F9 is fixed on the same return. Defect-return count for this milestone:
1. Amendment-return count for AC5: 1. Neither thrash trigger is near.

### Consistency gate

- `cairn_validate.py` exit 0 — 16 checks PASS, 8 advisories OK, including
  `coverage complete` and `weight caps`.
- `cairn_impact.py` not applicable: `Principles touched: —`, no DESIGN.md
  principle changed.
- r-package profile `consistency-gate`: `document()` no diff; `NAMESPACE` and
  `man/` regenerate clean; README untouched (no `README.Rmd` change);
  `pkgdown::check_pkgdown()` "No problems found"; `NEWS.md` carries the entry;
  no new top-level files; `check()` clean.

