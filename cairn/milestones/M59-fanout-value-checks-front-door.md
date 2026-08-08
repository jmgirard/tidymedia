# M59: Six per-row value checks are refused at the fan-out verb's front door

- **Status:** planned
- **Priority:** normal
- **Depends on:** M58
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

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
`ffm_crop()`. Not whether Layer 2 may validate: IP1 (`cairn/DESIGN.md:62-64`)
scopes itself to command assembly and is silent on validation, which Layer 2
front doors do throughout under D035/D036.

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

- [ ] T1 — Settle the site-1 shape question and record it as a milestone-local
      decision; shape the shared checkers accordingly.
- [ ] T2 — `crop_video_batch()` front door: `width`/`height` range, swept over
      the column form.
- [ ] T3 — `picture_in_picture_batch()` front door: `margin` range, swept over
      the column form; retire the closure's re-check if it becomes dead.
- [ ] T4 — `anonymize_video_batch()` front door: `regions` shape, swept over
      the list-column.
- [ ] T5 — `compare_videos_batch()` front door: per-row `audio` index against
      each row's own `inputs` length.
- [ ] T6 — Sites 5 and 6: sweep the `direction` and `position` column VALUES at
      both front doors, sourcing the vocabulary from one place rather than a
      third copy; `check_batch_string_col()` keeps the type half.
- [ ] T7 — Tests: one blame test per pair at both `parallel` settings; mixed-
      column tests; the three precedence cases of AC5; mutation-verify each
      front-door and shared call.
- [ ] T8 — Build and commit the before/after grid as the evidence ledger.
- [ ] T9 — NEWS (including AC5(c)'s displaced set); roxygen where precedence
      changed; run the verify slot clean.

## Work log

- 2026-08-07: created by /milestone-plan.
- 2026-08-07: plan gate chose to split these four from M58's six rather than ship all ten together because the combined milestone exceeded the >7-criteria and >10-task tripwires, and because these four are range/shape checks needing the `check_batch_*_col()` sweep while M58's six are contradictions needing a new shared-checker pattern; falsified by M58's checker shape turning out to cover these four with no further design call, which would make the split pure overhead.
- 2026-08-07: criteria audit ([O] fresh-context reader) returned findings on AC2 (two), AC3, AC4, AC5 (two) and a false Scope claim that all four sites already had a scalar front-door counterpart — sites 1 and 3 do not. All fixed before commit; AC1 and AC6 returned clean, Coverage clean.
- 2026-08-07: plan gate chose the two IP1-clean shapes for site 1 over duplicating the Layer-1 check at Layer 2 because IP1 puts validation logic in Layer 1 once and a duplicate would need an IP1 exception D-entry; falsified by both clean shapes proving unable to name the batch verb, which would make the exception the only route left. **Superseded 2026-08-07 (amendment below): the premise was false.**
- 2026-08-07 (amendment): the M58-review candidate row's vocabulary half (F2) folded in as sites 5 and 6 at the user's direction; measured on merged master that a `direction` column of `"sideways"` and a `position` column of `"middleish"` each report `purrr::pmap(jobs, .f, ...)` / `In index: 1` and additionally leak `compare_videos_pipeline()` / `picture_in_picture_pipeline()`. The row's other half (F6, `ffm_batch()`'s own checks) is documented rather than moved — see Scope Out and AC5(c) — because the verb does not own those checks and they cannot be hoisted. Title and ROADMAP row retitled four → six.
- 2026-08-07 (amendment): the amended criteria went to a second [O] fresh-context reader, which returned findings on AC1 (the `parallel = TRUE` path leaks `furrr::future_pmap`, which the forbidden-substring list missed), AC2 (two: "exactly one place" quantified over a domain no named procedure enumerates, and a cross-product IP1 clause), AC3 (site 3 has no scalar-argument form, making that cell unsatisfiable), AC4 (`ffm_crop()` authors no abort to re-aim), AC5 (two: three of four nvenc line numbers wrong, and the zero-row `jobs` test describes an unreachable state under D036's row-by-row shape). AC5(a) and AC6 returned clean. It also found four stale line numbers in the inherited Scope and a false gloss of IP1. All fixed before commit.
- 2026-08-07 (amendment): **the inherited IP1 framing was a misreading and is withdrawn.** `cairn/DESIGN.md:62-64` scopes IP1 to command assembly and says nothing about validation; Layer 2 front doors validate throughout under D035/D036. The plan gate chose to drop the framing — `Principles touched` IP1 → `—`, AC4 rewritten as the narrow question of where the front door's `call` comes from, AC2's cross-product IP1 clause struck — over amending DESIGN.md to extend IP1 to validation, which would retroactively govern D035, D036 and every existing Layer-2 check and so needs its own milestone. Falsified by a reviewer reading IP1's "thin wrappers" clause as implying validation locality, which would reinstate the exception question.
- 2026-08-07 (amendment): corrected four stale citations inherited from the original plan — site 2's `margin` re-check is `R/ffmpeg.R:6085` not `:5905`; site 4's `audio` index check is `:5905` not `:5744` (roxygen); site 1's abort is authored in `check_dim()` (`R/utils.R:115`), not in `ffm_crop()`; and AC5(b)'s nvenc guards are `:1926`, `:4930`, `:5894`, `:6074`, of which the original plan had one right.

## Decisions

## Review
