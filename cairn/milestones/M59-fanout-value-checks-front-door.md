# M59: Four per-row value checks are refused at the fan-out verb's front door

- **Status:** planned
- **Priority:** normal
- **Depends on:** M58
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** —

## Goal

Make the four per-row value validations that still report from inside
`purrr::pmap()` report from the fan-out verb the user called.

## Scope

**In:** the four sites M57's review measured as invisible to its AC6 grep,
because the grep attributes an abort to the function that *writes* it and each
of these is written in a helper or in the batch closure itself. All four were
re-measured reporting `purrr::pmap(jobs, .f, ...)` on merged master
2026-08-07:

1. `crop_video_batch()` — `width`/`height` out of range, written in `ffm_crop()`
   (Layer 1) and reached per row through `crop_video_pipeline()`.
2. `picture_in_picture_batch()` — `margin` out of range, written in the batch
   closure's own re-check (`R/ffmpeg.R:5905`).
3. `anonymize_video_batch()` — malformed `regions`, written in `check_regions()`
   and reached per row through `anonymize_pipeline()`.
4. `compare_videos_batch()` — per-row `audio` index above `length(inputs) - 1`,
   written in the batch closure (`R/ffmpeg.R:5744`).

Unlike M58's six, these are range and shape checks on a single value rather
than contradictions between two. Sites 2 and 4 already have a scalar
front-door counterpart (`picture_in_picture()`'s `check_number_whole(margin,
min = 0)`, `compare_videos()`'s `check_number_whole(audio, max = length(
infiles) - 1)`) and what is missing is only the sweep over the column form,
whose existing home is the `check_batch_*_col()` family. Sites 1 and 3 have
none: `crop_video()` delegates entirely to `check_dim()` inside `ffm_crop()`
(`R/ffm.R:280`), and `anonymize_video()` reaches `check_regions()` only inside
`anonymize_pipeline()` (`R/ffmpeg.R:1585`).

**In, and the reason this is not mechanical:** site 1's check lives in Layer 1,
so the fix must not have a Layer-2 verb restate it — IP1 puts validation logic
in Layer 1 once. The two IP1-clean shapes are re-aiming `ffm_crop()`'s abort
via a threaded `call`, or having the front door call the same Layer-1 checker
directly; choosing between them is the design call this milestone carries.
M58's D-entry governs contradiction checks and does not reach this.

**Out:** the six argument contradictions → M58 (done first; this milestone
reuses its checker shape). An unreadable input file → the ROADMAP candidate
row M58 created. Any new validation not already enforced somewhere today: this
milestone moves where an existing check reports, never what is checked.

## Acceptance criteria

- [ ] AC1 — For each of the four (site, verb) pairs enumerated in Scope In, a
      call violating that check aborts with `conditionCall()` naming the verb
      the user called and a message containing neither `purrr::pmap` nor
      `In index:`. One test per pair.
- [ ] AC2 — Each of the four checks is authored in exactly one place after the
      change, no Layer-2 verb restating a Layer-1 check (IP1). Verified by
      mutation: deleting a front-door call turns that site's AC1 test red for
      all four; and for sites 1 and 3, whose per-row path is the shared
      Layer-1/pipeline call, deleting that call turns the scalar verb's own
      test red. Sites 2 and 4 have no such shared call — their per-row check
      lives in the batch closure and no scalar verb reaches it — so the second
      mutation does not apply to them.
- [ ] AC3 — For each of the four sites the front-door guard refuses exactly
      the calls its current check refuses, over a committed before/after grid
      varying: the value at an in-range and an out-of-range setting; its
      scalar-argument form and its `jobs` column form; and one mixed column.
      Each cell's in-range baseline is asserted to succeed on both refs, so no
      cell compares equal by both sides failing.
- [ ] AC4 — The IP1 question for site 1 is settled in writing: a
      milestone-local decision entry records which IP1-clean shape was chosen
      — re-aiming `ffm_crop()`'s abort via a threaded `call`, or the front
      door calling the Layer-1 checker directly — names the alternative
      rejected, and states the evidence class that would falsify the choice.
      Duplicating the check at Layer 2 is not among the options; were it ever
      chosen it would need an IP1 exception recorded as a D-entry, which this
      milestone does not plan.
- [ ] AC5 — Precedence is pinned, and the value-check-versus-contradiction
      order is this milestone's own call, not one M58 makes: on the two verbs
      carrying both an M58 contradiction and a value check
      (`compare_videos_batch`, `picture_in_picture_batch`), a call invalid in
      both reports the contradiction; on all four verbs, a call invalid in its
      value check and in nvenc availability reports the value check, the nvenc
      guard being live on all four (`R/ffmpeg.R:1941`, `:4784`, `:5733`,
      `:5894`).
- [ ] AC6 — The r-package profile's verify slot is clean:
      `devtools::document()` produces no diff, `devtools::test()` passes, and
      `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T2, T3, T4, T5, T6
- AC2 → T1, T6
- AC3 → T7
- AC4 → T1
- AC5 → T6
- AC6 → T8

## Tasks

- [ ] T1 — Settle the IP1 question for site 1 and record it as a
      milestone-local decision; shape the shared checkers accordingly.
- [ ] T2 — `crop_video_batch()` front door: `width`/`height` range, swept over
      the column form.
- [ ] T3 — `picture_in_picture_batch()` front door: `margin` range, swept over
      the column form; retire the closure's re-check if it becomes dead.
- [ ] T4 — `anonymize_video_batch()` front door: `regions` shape, swept over
      the list-column.
- [ ] T5 — `compare_videos_batch()` front door: per-row `audio` index against
      each row's own `inputs` length.
- [ ] T6 — Tests: one blame test per pair; mixed-column tests; precedence
      tests; mutation-verify each front-door and per-row call.
- [ ] T7 — Build and commit the before/after grid as the evidence ledger.
- [ ] T8 — NEWS; roxygen where precedence changed; run the verify slot clean.

## Work log

- 2026-08-07: created by /milestone-plan.
- 2026-08-07: plan gate chose to split these four from M58's six rather than ship all ten together because the combined milestone exceeded the >7-criteria and >10-task tripwires, and because these four are range/shape checks needing the `check_batch_*_col()` sweep while M58's six are contradictions needing a new shared-checker pattern; falsified by M58's checker shape turning out to cover these four with no further design call, which would make the split pure overhead.
- 2026-08-07: criteria audit ([O] fresh-context reader) returned findings on AC2 (two), AC3, AC4, AC5 (two) and a false Scope claim that all four sites already had a scalar front-door counterpart — sites 1 and 3 do not. All fixed before commit; AC1 and AC6 returned clean, Coverage clean.
- 2026-08-07: plan gate chose the two IP1-clean shapes for site 1 over duplicating the Layer-1 check at Layer 2 because IP1 puts validation logic in Layer 1 once and a duplicate would need an IP1 exception D-entry; falsified by both clean shapes proving unable to name the batch verb, which would make the exception the only route left.

## Decisions

## Review
