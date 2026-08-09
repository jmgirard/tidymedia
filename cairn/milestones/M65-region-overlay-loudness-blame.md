# M65: A region, overlay or loudness mistake names the verb the user called, in both forms

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M64
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** m65-region-overlay-loudness-blame

## Goal

Make `anonymize_video`, `picture_in_picture` and `normalize_audio` refuse their
own region, overlay-scale and loudness values at their front door, so the abort
names the verb the caller typed rather than the Layer-1 builder it reached.

## Scope

**In:** front-door sweeps on both forms of the three verbs — `check_dim()` per
region field for the values `check_regions()` deliberately leaves to
`ffm_drawbox()` (`R/ffmpeg.R:1637-1638`), the `scale` RANGE that today lives
only in `ffm_overlay()` (`R/ffm.R:931-933`), and the three loudness bounds
`ffm_loudnorm()` holds as body literals (`R/ffm.R:437-439`). The range rule and
the three bounds each become one internal binding both layers read, so neither
layer restates the other's number. `normalize_audio()`'s sweep goes ABOVE its
`two_pass` block (`R/ffmpeg.R:2087-2102`), not last, so a bad target is refused
before `run_loudnorm_analysis()` spawns FFmpeg — the one placement in these two
milestones that is not "last among the value guards", and the reason AC3 exists.

**Out:** threading `call` through the exported `ffm_*` builders (M59-D1; M64
re-rejected it). `ffm_drawbox()`'s `thickness`, which no verb exposes → stays a
Layer-1 error. Widening `check_regions()` beyond structure → not this milestone;
the value half is swept beside it, not folded into it.

**Evidence note:** the `two_pass = TRUE` cells run FFmpeg (D034's shape:
`run_loudnorm_analysis()` executes under `run = FALSE`), so they `skip_if` the
binary is absent and their evidence is local-only. The path whose blame is worst
today is the path CI cannot check; T5 records the local run.

## Acceptance criteria

- [ ] AC1: The `scale` range rule and the three loudness bounds each exist as one
      internal binding in `R/`, read by both the Layer-1 builder and the Layer-2
      front door. Verified by a test that reads each binding from the namespace
      and then probes the value either side of it at BOTH layers — a direct
      `ffm_overlay()`/`ffm_loudnorm()` call and the verb — asserting each layer's
      accept/refuse boundary sits at the binding's value. Comparing literals is
      not evidence: two restated numbers compare equal.
- [ ] AC2: The roxygen bounds at `R/ffmpeg.R:2009-2014` are generated from the
      same bindings by an inline helper, as `audio_stream_param()` already does,
      and `devtools::document()` produces no diff.
- [ ] AC3: For every cell of the spec list declared in
      `tests/testthat/helper-blame-specs-m65.R` — each naming (verb, form, delivery, argument
      or region field, violating value), with `two_pass` in {FALSE, TRUE} as an
      axis on `normalize_audio` and the bad region row and field varied within a
      multi-row `regions` frame — the call aborts with `conditionCall()` naming
      the verb called and a deparsed call containing none of `pmap`,
      `_pipeline(`, `ffm_`. Every `_batch` cell appears twice, argument delivery
      and column delivery. A completeness reader fails on a declared cell naming
      neither a formal, a resolved column, nor a `check_regions()` field. The
      list is closed by inspection and the file says so.
- [ ] AC4: For each crossing declared in `data-raw/blame-precedence-m65.R` —
      each new sweep crossed with each guard in that file's crossing list, which
      for `normalize_audio` names every guard the sweep now precedes
      (`channels`, `sample_rate`, `audio_stream`, `check_audio_codec_not_copy`,
      `check_token(audio_codec)`) — the guard that reports is recorded at the
      merge-base and on the branch, each cell carrying a control asserting the
      crossed guard is live. Every reordering is listed in a table in this file
      with the caller it changes the answer for. A cell whose control is dead
      fails; it is not excluded.
- [ ] AC5: `picture_in_picture`'s existing `check_number_decimal(scale)`
      (`R/ffmpeg.R:5921`) and the new range refusal are distinguished at range
      grain, not by error class: AC3 carries a cell for a non-numeric `scale` and
      a cell for an out-of-range one, and each asserts the other's wording is
      absent.
- [ ] AC6: `data-raw/blame-guard-mutations-m65.py` derives its mutation list from
      the branch diff's added checker call sites, removes each in the FILE, and
      records the reds. Deleting a Layer-2 sweep reddens AC3's grid; deleting a
      Layer-1 read of a binding reddens AC1's direct-builder probe instead —
      AC3's grid cannot see it, and a criterion claiming otherwise would assert
      an impossible redness. AC3's completeness reader and AC4's controls are
      themselves mutated and go red.
- [ ] AC7: Each site matched by `grep -rn 'ffm_drawbox\|ffm_overlay\|ffm_loudnorm\|anonymize_pipeline\|picture_in_picture_pipeline\|normalize_audio_pipeline' R/ tests/ man/ NEWS.md README.Rmd vignettes/ cairn/DESIGN.md cairn/ROADMAP.md`
      is read, and no matched site outside `cairn/milestones/archive/` retains a
      claim that one of these is the blamed call or the validating site for a
      value this milestone moves — `R/ffmpeg.R:1637-1638` and `R/ffm.R:437-439`
      included. Archives stay unedited. Each NEWS sentence cites, in a table in
      this file, the test whose redness AC6's run demonstrates.
      `devtools::test()` clean and `devtools::check()` `Status: OK`.

## Coverage

- AC1 → T1, T2
- AC2 → T2
- AC3 → T3, T4, T5
- AC4 → T6
- AC5 → T3, T4
- AC6 → T7
- AC7 → T8

## Tasks

- [x] T1: Extract the `scale` range rule and the three loudness bounds to one
      internal binding each; point `ffm_overlay()` (`R/ffm.R:931-933`) and
      `ffm_loudnorm()` (`:437-439`) at them. Red-first boundary probes at the
      builder layer.
- [x] T2: Generate the roxygen bounds at `R/ffmpeg.R:2009-2014` from the same
      bindings via an inline helper; `document()`.
- [ ] T3: Declare `tests/testthat/helper-blame-specs-m65.R` and extend M64's grid to read it —
      both forms, both deliveries, the `two_pass` axis, the region row/field
      variation, and AC5's two `scale` cells. Red first.
- [ ] T4: `anonymize_video()` (`R/ffmpeg.R:1533`) and `anonymize_video_batch()`
      (`:1806`) sweep each region field beside `check_regions()`;
      `picture_in_picture()` (`:5909`) and `_batch` (`:6282`) sweep the `scale`
      range at the front door.
- [ ] T5: `normalize_audio()` (`:2066`) sweeps the three loudness values ABOVE
      the `two_pass` block; `normalize_audio_batch()` (`:4119`) sweeps the same
      values per row, beside its existing type-only column sweep (`:4149-4156`).
      Record the local FFmpeg run backing the `two_pass = TRUE` cells.
- [ ] T6: `data-raw/blame-precedence-m65.R` — crossing list, live controls, both
      refs; write the reordering table.
- [ ] T7: `data-raw/blame-guard-mutations-m65.py` — diff-derived list, split
      Layer-1/Layer-2 redness targets, reader/control mutations.
- [ ] T8: AC7 sweep and corrections; NEWS entry + citation table; D-entry
      recording that a cheap value refusal precedes the analysis probe.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: T1 done — bindings `overlay_scale_range` + three `loudnorm_range_*` in R/utils.R, shared checkers `check_overlay_scale()`/`check_loudnorm_targets()`/`check_region_values()`; builders point at them; AC1 boundary probes in test-shared-range-bindings.R (both layers, bounds derived from the namespace bindings). Suite clean.
- 2026-08-08: T2 done — `loudnorm_bounds_rd()` inline helper; `normalize_audio()`'s AND `ffm_loudnorm()`'s roxygen bounds now render from the bindings (same helper, same words; only source line-wrap moved). `document()` stable after the commit.
- 2026-08-08: plan gate chose moving `normalize_audio()`'s loudness sweep above the `two_pass` analysis block over scoping the milestone to single-pass and disclosing the gap, because a disclosed ordering gap is the shape D038 recorded and D039 had to undo; falsified by a reordering that changes the reported guard for a caller the crossing table cannot enumerate.
- 2026-08-08: plan gate chose one internal binding read by both layers over restating each bound at the front door, because a restated number is exactly what the M40 stale-hint lesson bites on and no test comparing literals can see the drift; falsified by a bound whose two layers must legitimately differ.
- 2026-08-08: substantive amendment (gated): AC3/T3's spec list moved from `data-raw/blame-specs-m65.R` to `tests/testthat/helper-blame-specs-m65.R` — `^data-raw$` is in `.Rbuildignore`, so the grid test sourcing it there would skip under `R CMD check` (the M51/M59 lesson; M64's list lives in tests/ for the same reason). User approved "Move to tests/".
- 2026-08-08: criteria audit ([O], fresh context) returned defects on all seven drafted criteria — a bounds test that could not distinguish a shared binding from a restated literal, a "one site in `R/`" claim falsified by roxygen the criterion never mentioned, a `two_pass` path the grid never reached, an inherited unbounded crossing domain, a type/range conflation at error-class grain, and a mutation criterion asserting a redness that cannot occur. All seven rewritten before writing.
