# M59: Six per-row value checks are refused at the fan-out verb's front door

- **Status:** review
- **Priority:** normal
- **Depends on:** M58
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m59-fanout-value-checks-front-door`

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

- [ ] AC1 — For each of the six (site, verb) pairs enumerated in Scope In, a
      call violating that check aborts with `conditionCall()` naming the verb
      the user called, and a message containing none of the substrings `pmap`
      (covering `purrr::pmap` and `furrr::future_pmap` alike), `In index:`, or
      `_pipeline(`. One test per pair, at both `parallel` settings.
- [ ] AC2 — For each of the six sites, the front-door call and the per-row path
      resolve to the same abort site, named in the milestone-local decision
      entry, with no verb spelling out a second copy of the message or (sites
      5/6) a third copy of the vocabulary literal. Verified by mutation:
      deleting a front-door call turns that site's AC1 test red for all six;
      and for sites 1, 3, 5 and 6, whose per-row path is a shared helper or
      pipeline call the scalar verb also reaches, deleting that shared call
      turns the scalar verb's own test red. Sites 2 and 4 have no shared call
      (their check lives in the batch closure), so that half does not apply.
- [ ] AC3 — For each of the six sites the front-door guard refuses exactly the
      calls its current check refuses, over a committed before/after grid
      varying the value at an in-range and an out-of-range setting, in its
      `jobs` column form and — for the five sites that have one — its
      scalar-argument form, plus one mixed column. Site 3 is column-only —
      `anonymize_video_batch()` has no `regions` argument
      (`R/ffmpeg.R:1803-1808`) — so the grid records that cell as nonexistent,
      and sites 5/6's scalar cells as expected-identical on both refs (those
      verbs already arg-match the scalar), so neither is read as evidence. Each
      cell's in-range baseline is asserted to succeed on both refs, so no cell
      compares equal by both sides failing.
- [ ] AC4 — The shape question for site 1 is settled in writing: a
      milestone-local decision entry records whether the front door calls
      `check_dim()` directly or threads `call` through `ffm_crop()`, names the
      alternative rejected, and states the evidence class that would falsify
      the choice.
- [ ] AC5 — Precedence is pinned, and the value-check ordering is this
      milestone's own call, not one M58 makes:
      (a) on the two verbs carrying both an M58 contradiction and a value check
      (`compare_videos_batch`, `picture_in_picture_batch`), a call invalid in
      both reports the contradiction;
      (b) on all four verbs, a call invalid in its value check and in nvenc
      availability reports the value check — the nvenc guard being live at
      `R/ffmpeg.R:1926`, `:4930`, `:5894`, `:6074`, driven machine-
      independently through the `tidymedia.nvenc_encoders` option seam;
      (c) on all four verbs, a call invalid in its value check and in an
      argument `ffm_batch()` alone guards reports the value check — tested on
      `run`, and NEWS names the displaced set as the arguments guarded at
      `R/ffm_batch.R:84-98` (`run`, `parallel`, `progress`, `manifest`,
      `checksums`, `verify`), read off that block rather than recalled. The
      `jobs`-shape guards at `:75-80` are excluded and stated as excluded: all
      four verbs already pre-empt them (`R/ffmpeg.R:1816`, `:4251`, `:4486`,
      `:6017`), so they are never displaced.
- [ ] AC6 — The r-package profile's verify slot is clean:
      `devtools::document()` produces no diff, `devtools::test()` passes, and
      `devtools::check()` reports 0 errors and 0 warnings.

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
