# M65: A region, overlay or loudness mistake names the verb the user called, in both forms

- **Status:** review
- **Priority:** normal
- **Depends on:** M64
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** m65-region-overlay-loudness-blame · PR #68 https://github.com/jmgirard/tidymedia/pull/68

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

- [x] AC1: The `scale` range rule and the three loudness bounds each exist as one
      internal binding in `R/`, read by both the Layer-1 builder and the Layer-2
      front door. Verified by a test that reads each binding from the namespace
      and then probes the value either side of it at BOTH layers — a direct
      `ffm_overlay()`/`ffm_loudnorm()` call and the verb — asserting each layer's
      accept/refuse boundary sits at the binding's value. Comparing literals is
      not evidence: two restated numbers compare equal.
- [x] AC2: The roxygen bounds at `R/ffmpeg.R:2009-2014` are generated from the
      same bindings by an inline helper, as `audio_stream_param()` already does,
      and `devtools::document()` produces no diff.
- [x] AC3: For every cell of the spec list declared in
      `tests/testthat/helper-blame-specs-m65.R` — each naming (verb, form, delivery, argument
      or region field, violating value), with `two_pass` in {FALSE, TRUE} as an
      axis on `normalize_audio` and the bad region row and field varied within a
      multi-row `regions` frame — the call aborts with `conditionCall()` naming
      the verb called and a deparsed call containing none of `pmap`,
      `_pipeline(`, `ffm_`. Every `_batch` cell appears twice, argument delivery
      and column delivery. A completeness reader fails on a declared cell naming
      neither a formal, a resolved column, nor a `check_regions()` field. The
      list is closed by inspection and the file says so.
- [x] AC4: For each crossing declared in `data-raw/blame-precedence-m65.R` —
      each new sweep crossed with each guard in that file's crossing list, which
      for `normalize_audio` names every guard the sweep now precedes
      (`channels`, `sample_rate`, `audio_stream`, `check_audio_codec_not_copy`,
      `check_token(audio_codec)`) — the guard that reports is recorded at the
      merge-base and on the branch, each cell carrying a control asserting the
      crossed guard is live. Every reordering is listed in a table in this file
      with the caller it changes the answer for. A cell whose control is dead
      fails; it is not excluded.
- [x] AC5: `picture_in_picture`'s existing `check_number_decimal(scale)`
      (`R/ffmpeg.R:5921`) and the new range refusal are distinguished at range
      grain, not by error class: AC3 carries a cell for a non-numeric `scale` and
      a cell for an out-of-range one, and each asserts the other's wording is
      absent.
- [x] AC6: `data-raw/blame-guard-mutations-m65.py` derives its mutation list from
      the branch diff's added checker call sites, removes each in the FILE, and
      records the reds. Deleting a Layer-2 sweep reddens AC3's grid; deleting a
      Layer-1 read of a binding reddens AC1's direct-builder probe instead —
      AC3's grid cannot see it, and a criterion claiming otherwise would assert
      an impossible redness. AC3's completeness reader and AC4's controls are
      themselves mutated and go red.
- [x] AC7: Each site matched by `grep -rn 'ffm_drawbox\|ffm_overlay\|ffm_loudnorm\|anonymize_pipeline\|picture_in_picture_pipeline\|normalize_audio_pipeline' R/ tests/ man/ NEWS.md README.Rmd vignettes/ cairn/DESIGN.md cairn/ROADMAP.md`
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
- [x] T3: Declare `tests/testthat/helper-blame-specs-m65.R` and extend M64's grid to read it —
      both forms, both deliveries, the `two_pass` axis, the region row/field
      variation, and AC5's two `scale` cells. Red first.
- [x] T4: `anonymize_video()` (`R/ffmpeg.R:1533`) and `anonymize_video_batch()`
      (`:1806`) sweep each region field beside `check_regions()`;
      `picture_in_picture()` (`:5909`) and `_batch` (`:6282`) sweep the `scale`
      range at the front door.
- [x] T5: `normalize_audio()` (`:2066`) sweeps the three loudness values ABOVE
      the `two_pass` block; `normalize_audio_batch()` (`:4119`) sweeps the same
      values per row, last among the value guards above its own `two_pass`
      block. Record the local FFmpeg run backing the `two_pass = TRUE` cells.
- [x] T6: `data-raw/blame-precedence-m65.R` — crossing list, live controls, both
      refs; write the reordering table.
- [x] T7: `data-raw/blame-guard-mutations-m65.py` — diff-derived list, split
      Layer-1/Layer-2 redness targets, reader/control mutations.
- [x] T8: AC7 sweep and corrections; NEWS entry + citation table; D-entry
      recording that a cheap value refusal precedes the analysis probe.

## Decisions

- **M65-D1 (2026-08-08): the reordering table.** The six sweeps reassign 22
  reporting orders, measured by `data-raw/blame-precedence-m65.R` at the
  merge-base (`master`, ddc14be) versus the branch — 113 crossings, every
  control live, zero unresolved, on both refs. Every flip moves the winner
  crossed→sweep:

  | flips | crossings | caller whose answer changes |
  |---|---|---|
  | 1–3 | `anonymize_video/regions` × color-not-string, video_codec-token, pixel_format-token | `anonymize_video(f, "o.mp4", bad_regions, color = 1)` (or a bad `video_codec`/`pixel_format` token) heard the knob's complaint from the pipeline; it now hears the region refusal — the argument a caller is likeliest to get wrong (M47 F8) reports first. |
  | 4–5 | `anonymize_video_batch/regions` × color-arg-not-string, pixel_format-arg-token | Same two knobs on the batch verb — the only two whose sole check ran per row inside the fan-out (blaming `purrr::pmap()`); a bad region value now outranks them. |
  | 6–8 | `anonymize_video_batch/regions` × nvenc; `picture_in_picture_batch/scale` (both deliveries) × nvenc | A machine-independent value error now outranks a missing nvenc encoder — M64-D2's shape, D036's machine-independent-first rule. |
  | 9–18 | `normalize_audio/loudness` (two-pass AND single-pass) × channels, sample_rate, audio_stream, audio_codec-copy, audio_codec-token/-not-string | AC4's named five: a call wrong about a loudness target and a shaping knob is now told about the target, whichever pass; on two-pass, the target refusal also precedes the analysis spawn (the Scope's reason). |
  | 19–22 | `normalize_audio_batch/loudness` (both deliveries) × channels-col-whole-2p, audio_codec-token-2p | The batch two-pass block's own guards — the batch mirrors the scalar's flips exactly under the "last among the value guards" placement. |

  **Disclosed cross-form divergence:** `video_codec-token` — the batch checks
  the token at its front door (above the region sweep, unchanged), the scalar
  now reports the region value first (flip 2). Unavoidable without moving
  pre-existing guards: the batch's token check must precede its per-cell
  sweep, the scalar's sits in the pipeline below the sweep's mandated
  beside-`check_regions()` slot. The alternative placement diverged on
  `color`/`pixel_format` instead, two knobs rather than one.

- **M65-D2 (2026-08-08): NEWS citation table.** Each sentence of the M65 NEWS
  entry, and the test whose redness `data-raw/blame-guard-mutations-m65.py`'s
  run demonstrates (AC7):

  | NEWS sentence (by claim) | test AC6's run reddens |
  |---|---|
  | "refused by the function you called" + the builder/`pmap` history | `test-builder-blame-front-door.R` "a region, overlay or loudness value blames the verb the user called" — red under each of the six Layer-2 site deletions |
  | `_batch` refusal in both deliveries, before any row runs | the same test's `/arg` and `/column` cells — red under the three batch-site deletions |
  | two-pass target refused before the analysis pass | `test-builder-blame-front-door.R` "a two-pass loudness value blames the verb, before the analysis pass" — red under the `normalize_audio` site deletion |
  | non-numeric `scale` complaint unchanged; new refusal is range-grain | the AC5 type/range cell pairs (each asserting the other's wording absent) — range cells red under the two `picture_in_picture` site deletions; type cells pinned |
  | bad value outranks a missing nvenc encoder | `test-builder-blame-front-door.R` "a bad region or scale value reports before a missing nvenc encoder" |
  | one shared definition per range; docs cannot drift | `test-shared-range-bindings.R` (both tests) — red under the two Layer-1 site deletions; the roxygen bounds render from the same bindings at `document()` time |

- **M65-D3 (2026-08-08, from review return #1): corrected reordering table
  and full divergence disclosure — supersedes M65-D1's `normalize_audio_batch`
  rows, its mirror sentence, and its single-item divergence note.** The first
  crossing list omitted `channels`/`sample_rate` for `normalize_audio_batch`;
  the corrected grid (121 crossings, live controls, zero dead/unresolved on
  both refs) records **30 flips**. M65-D1's rows 1–18 stand. The batch rows
  become: `normalize_audio_batch/loudness` × channels-fractional,
  sample_rate-fractional (both passes, both deliveries — 8 flips) and ×
  channels-col-whole-2p, audio_codec-token-2p (both deliveries — 4 flips):
  **12 batch flips, not 4**; D043's "(10 scalar, 4 batch)" parenthetical reads
  per the superseded grid. The mirror sentence is corrected: the batch mirrors
  the scalar's flips **only for guards that sit below both sweeps**; a guard
  at the batch front door above its sweep but below the scalar's answers
  differently by form. **Full cross-form divergence disclosure:**
  `video_codec-token` on the anonymize pair (batch: token first; scalar:
  region value first); `audio_codec = "copy"` and `audio_stream` on the
  normalize pair (batch reports them first, scalar reports the target first —
  measured; on master both forms reported them first). Unavoidable within
  this milestone: the batch checks copy/`audio_stream` at its front door
  above the input sweep deliberately (M34/M45: fail before Phase 1 wastes an
  analysis pass per row), the scalar's plan-mandated above-the-block sweep
  precedes its own copies of those guards, and no batch placement mirrors
  both these and the input-first order M62 fixed. The residual is a candidate
  row.

## Work log

- 2026-08-08: created by /milestone-plan.

- 2026-08-08: return #1 fix pass done — S6 extended (channels/sample_rate crossings, both passes), grid re-run both refs (121 cells, 0 dead/unresolved, 30 flips), M65-D3 supersedes M65-D1's batch rows + mirror sentence + divergence note; F3/F4 comments fixed; F6/F7 fixed voluntarily; F10 + the divergence residual → grouped candidate row. Re-verified: suite 0 fail / 5588 pass; harness 11/11 red; `check()` 0/0/0; `cairn_validate` exit 0. Status → review; AC4 re-ticked on the corrected evidence.
- 2026-08-08: review return #1 (floor: finding F1 scored 92; defect-return count now 1) — AC4's "Every reordering is listed" clause failed: the S6 crossing list omitted guards the batch sweep now precedes (`channels`/`sample_rate`, measured flipping), so the table undercounted; and the batch form now answers `audio_codec = "copy"`/`audio_stream` crossings differently from the scalar (measured), undisclosed, with M65-D1's "mirrors exactly" sentence overclaiming. Status → in-progress for the fix pass.
- 2026-08-08: T1 done — bindings `overlay_scale_range` + three `loudnorm_range_*` in R/utils.R, shared checkers `check_overlay_scale()`/`check_loudnorm_targets()`/`check_region_values()`; builders point at them; AC1 boundary probes in test-shared-range-bindings.R (both layers, bounds derived from the namespace bindings). Suite clean.
- 2026-08-08: minor amendment: T5's batch sweep placed "last among the value guards above the `two_pass` block" rather than "beside the type-only column sweep" — the early placement would have put a loudness value error above the missing-input sweep, diverging from the scalar form and from M62's family-wide input-first order; the late placement mirrors the scalar's flips exactly (D042's siting rule).
- 2026-08-08: T2 done — `loudnorm_bounds_rd()` inline helper; `normalize_audio()`'s AND `ffm_loudnorm()`'s roxygen bounds now render from the bindings (same helper, same words; only source line-wrap moved). `document()` stable after the commit.
- 2026-08-08: T3 done — `helper-blame-specs-m65.R` (30 cells: 8 region row/field-varied, 8 scale incl. AC5's type/range pairs, 12 loudness with the scalar `two_pass` axis, + reader) and the M64 grid extended with four M65 blocks; observed red on blame before the sweeps (builder/pmap blamed), as declared.
- 2026-08-08: T4 done — region sweep via `check_region_values()` in `anonymize_pipeline()` (call threaded) + per cell at the batch front door; `check_overlay_scale()` in `picture_in_picture_pipeline()` below the contradiction/position checks (M61 ordering) + per resolved row at the batch front door above the nvenc probe.
- 2026-08-08: T8 done — AC7 sweep over the 237 grep matches: seven stale validating-site claims corrected (drawbox-loop and pipeline-loudnorm comments, batch list-column and knob-loop comments, three test comments/titles); NEWS entry added with M65-D2's per-sentence citation table (plus a new nvenc-ordering test backing its ordering sentence); D043 appended. `devtools::test()` 0 fail / 5581 pass; `devtools::check()` 0 errors / 0 warnings / 0 notes. Status → review.
- 2026-08-08: T7 done — mutation harness run clean, 11/11 required reds: 8 diff-derived sites (2 Layer-1 reddening AC1's builder probes only, 6 Layer-2 reddening their own verbs' grid cells only), the neutered reader caught by the planted-defect test, the removed `check_bool(two_pass)` guard reported dead by the controls and the report vanishing when the control check itself is neutered. Tree restored (git status clean).
- 2026-08-08: T6 done — `data-raw/blame-precedence-m65.R` (113 crossings over the six sweeps, `two_pass` axis on the normalize verbs; reuses M64's runner via a new `cells` parameter on `blame_precedence()`); both refs clean of dead controls and unresolved cells; 22 flips recorded as M65-D1's table. No scale-type crossing: one scalar cannot be non-numeric and out-of-range at once, so its control could never be live — AC5's grid cells carry that distinction.
- 2026-08-08: T5 done — `check_loudnorm_targets()` above `normalize_audio()`'s `two_pass` block and per resolved row above `normalize_audio_batch()`'s, below `check_batch_inputs()`. Local FFmpeg run (ffmpeg 8.1.2, macOS): full suite 0 fail / 5579 pass / 5 skips, none of them the two-pass blame block — the `two_pass = TRUE` cells executed and passed.
- 2026-08-08: plan gate chose moving `normalize_audio()`'s loudness sweep above the `two_pass` analysis block over scoping the milestone to single-pass and disclosing the gap, because a disclosed ordering gap is the shape D038 recorded and D039 had to undo; falsified by a reordering that changes the reported guard for a caller the crossing table cannot enumerate.
- 2026-08-08: plan gate chose one internal binding read by both layers over restating each bound at the front door, because a restated number is exactly what the M40 stale-hint lesson bites on and no test comparing literals can see the drift; falsified by a bound whose two layers must legitimately differ.
- 2026-08-08: substantive amendment (gated): AC3/T3's spec list moved from `data-raw/blame-specs-m65.R` to `tests/testthat/helper-blame-specs-m65.R` — `^data-raw$` is in `.Rbuildignore`, so the grid test sourcing it there would skip under `R CMD check` (the M51/M59 lesson; M64's list lives in tests/ for the same reason). User approved "Move to tests/".
- 2026-08-08: criteria audit ([O], fresh context) returned defects on all seven drafted criteria — a bounds test that could not distinguish a shared binding from a restated literal, a "one site in `R/`" claim falsified by roxygen the criterion never mentioned, a `two_pass` path the grid never reached, an inherited unbounded crossing domain, a type/range conflation at error-class grain, and a mutation criterion asserting a redness that cannot occur. All seven rewritten before writing.


## Review

Fresh evidence, this session (2026-08-08, branch head 544e954, PR #68):

- AC1: `test-shared-range-bindings.R` run fresh — both tests pass; each of the
  four bindings read from the namespace via `get()`, probed either side at the
  builder AND the verb (loudness also at the batch verb); combined
  shared-range+grid run: 645 pass / 0 fail / 0 skip.
- AC2: `devtools::document()` re-run at review — zero modified files under
  `man/` or `NAMESPACE`; the three `@param` bounds render through
  `loudnorm_bounds_rd()` from the same bindings (R/ffmpeg.R and R/ffm.R).
- AC3: the M65 grid (four blocks in `test-builder-blame-front-door.R`, cells
  from `tests/testthat/helper-blame-specs-m65.R`) run fresh — every cell
  asserts its own message, `blamed_verb()` = the verb, and no `pmap` /
  `_pipeline(` / `ffm_` in the deparsed call; `_batch` cells in both
  deliveries; `two_pass` axis ran (0 skips, local FFmpeg 8.1.2); completeness
  reader clean on the real list and red on all three planted defects. The
  spec file states the list is closed by inspection.
- AC4: `blame_precedence_m65()` run fresh at `master` (merge-base ddc14be)
  and the working tree: 113 cells, 0 dead controls and 0 unresolved on BOTH
  refs, 22 flips — identical to M65-D1's table.
- AC5: the four type/range `scale` cell pairs ran in the AC3 grid, each
  asserting the other's wording absent; type cells pinned.
- AC6: `data-raw/blame-guard-mutations-m65.py` re-run fresh at review:
  baseline green, 8 diff-derived sites, 11/11 required reds (2 Layer-1 →
  AC1 probes only; 6 Layer-2 → owning verbs' cells only; reader + control
  mutations caught); tree restored.
- AC7: sweep evidence in the T8 work-log line (237 matches read via
  keyword passes + full scans of NEWS/README/vignettes/man; 7 corrections);
  NEWS citations in M65-D2. Fresh at review: `devtools::test()` 0 fail /
  5581 pass / 5 pre-existing env skips; `devtools::check()` 0 errors /
  0 warnings / 0 notes (2m52s).

Consistency gate: `cairn_validate` exit 0, all checks PASS; no principle
changed (no `cairn_impact`); `document()` no diff; README.Rmd untouched;
`pkgdown::check_pkgdown()` no problems; NEWS entry present, no milestone
numbers; no new top-level files. Driving RR: — (projection check no-op).

Independent review (fresh-context: [O] diff-bug, [S] blame-history, [S]
prior-review-record; [S] scorer): 14 candidate findings, all from the
diff-bug lens (the other two: none / no prior-review evidence). Actioned
(≥80): F1 (92) normalize copy/audio_stream form divergence undisclosed +
M65-D1 mirror overclaim → review return #1; fixed by extending S6, re-running
the grid (121 cells, 30 flips), and superseding via M65-D3; F3 (90) stale
"validation lives once" comment → fixed; F4 (82) stale Phase-2 targets
comment → fixed; F10 (80) batch refusals name no row → follow-up candidate
row (grouped with the divergence residual). Logged <80, one line each:
F2 (78) S6 crossing-list omissions — absorbed into F1's fix; F7 (78) NEWS
regions-argument overclaim — fixed voluntarily (column-form parenthetical);
F11 (70) no `normalize_audio_batch(two_pass = TRUE)` grid cell — the batch
Phase-1 claim rests on the precedence grid's two-pass cells, not AC3's;
F5 (68) AC1 probes cannot detect a restated identical literal (deletion is
mutation-covered; restatement is not); F6 (55) `loudnorm_bounds_rd()` silent
on a typo'd key — fixed voluntarily (abort default + test); F13 (55) three
message-side leak assertions per cell cannot fail (call-side covers the
property); F9 (42) `check_overlay_scale()` assumes pre-typed input (comment
is the contract; unreachable today); F12 (30) `pinned` field read by nothing
(M64-inherited convention); F14 (28) nvenc-ordering test has no live control
(M64-inherited shape; the precedence grid carries the controls); F8 (25)
`scale` doc literal not binding-rendered (unmodified lines, outside AC2's
scope).

Post-return re-verification (2026-08-08, after the fix pass): AC4 fresh —
corrected grid at `master` and working tree: 121 cells, 0 dead controls and
0 unresolved on both refs, 30 flips = M65-D3's table. Full suite
`devtools::test()` and `devtools::check()` re-run clean (recorded in the
work log); mutation harness re-run 11/11 red.

