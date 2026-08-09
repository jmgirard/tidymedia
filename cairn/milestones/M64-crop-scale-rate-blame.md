# M64: A crop, scale or rate mistake names the verb the user called, in both forms

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Branch/PR:** m64-crop-scale-rate-blame · https://github.com/jmgirard/tidymedia/pull/67

## Goal

Make `crop_video`, `standardize_video` and `sample_frames` refuse their own
dimension, rate and pixel-format values at their front door, so the abort names
the verb the caller typed rather than the Layer-1 builder it reached.

## Scope

**In:** front-door sweeps calling the same shared checkers the builders already
call — `check_dim()` for `crop_video`'s `width`/`height`/`x`/`y` and
`standardize_video`'s `width`/`height`/`fps`, and `resolve_sample_fps()` per row
for `sample_frames_batch`. `standardize_video`'s `pixel_format` is the one
exception to the siting: the pipeline reads it after both codec seams, so it is
checked there with `call` threaded — its position in the reporting order
unmoved — while `standardize_video_batch` sweeps it at the front door like every
other batch value. Both forms of each verb: `crop_video_batch()`'s sweep
(`R/ffmpeg.R:5107`) extends to `x`/`y`, and `standardize_video_batch()`
(`:3708`) and `sample_frames_batch()` (`:3483`) gain one. Batch placement follows
`crop_video_batch()`: last among the value guards, immediately above
`check_nvenc_available()`. The `sample_frames()` scalar form already names
itself and is pinned, not changed.

**Out:** threading a `call` argument through the exported `ffm_*` builders —
rejected at M59-D1 and again here; the builders keep validating for their own
direct callers. `anonymize_video`'s per-region values, `picture_in_picture`'s
`scale` range and `normalize_audio`'s loudness ranges → M65. Every other
`ffm_*` builder abort reachable only by calling the builder directly → stays a
Layer-1 error, correctly.

## Acceptance criteria

- [ ] AC1: For every cell of the spec list declared in
      `tests/testthat/helper-blame-specs.R` — each naming (verb, form, delivery,
      argument, violating value) — the call aborts with `conditionCall()` whose
      function part is the verb called and whose deparsed call contains none of
      `pmap`, `_pipeline(`, `ffm_`. A completeness reader fails when a declared
      cell names something that is neither a formal of that verb nor a column its
      `batch_arg_rows()` resolves. The spec list is closed by inspection and the
      file says so. Every script that consumes the spec list reads it from this
      helper in the source tree — `data-raw/blame-baseline.R` is the one
      consumer — and no second copy of the list exists anywhere; the AC4 and
      AC5 scripts declare no cell list of their own.
- [ ] AC2: Every `_batch` cell appears twice — the violating value passed as the
      argument, and carried in the `jobs` column — and for each (verb, argument)
      the scalar and batch cells report the same guard, compared cell-for-cell by
      the grid rather than asserted independently. Both `sample_frames` forms
      report `resolve_sample_fps()`'s wording.
- [ ] AC3: `data-raw/blame-baseline.R` records `blamed_verb()` and
      `conditionMessage()` for every AC1 cell at the branch's merge-base and on
      the branch. `blamed_verb()` changes on every cell this milestone claims to
      fix; every cell whose message differs is listed in a Deviations table in
      this file with a reason, and a cell absent from that table has an identical
      message on both refs.
- [ ] AC4: For each crossing declared in `data-raw/blame-precedence.R` — each new
      sweep crossed with each guard in that file's crossing list, closed by
      inspection and stated as such — the guard that reports is recorded at the
      branch's merge-base and on the branch, and each cell carries a control
      asserting the crossed guard is live on that call. The crossings whose
      winner changes are exactly those where a new sweep now precedes
      `check_nvenc_available()` on a `_batch` verb; each is listed in a
      reordering table in this file naming the call whose answer it changes.
      Every other cell reports the same guard on both refs. A cell whose control
      is dead fails; it is not excluded.
- [ ] AC5: `data-raw/blame-guard-mutations.py` derives its mutation list from the
      branch diff's added checker call sites, removes each in the FILE, and
      records the reds. Each mutation's reds include at least one cell that
      sweep owns. AC1's completeness reader and AC4's controls are themselves
      mutated and go red.
- [ ] AC6: Each site matched by `grep -rn 'ffm_crop\|ffm_scale\|ffm_fps\|ffm_pixel_format\|crop_video_pipeline\|standardize_pipeline\|sample_frames_pipeline' R/ tests/ man/ NEWS.md README.Rmd vignettes/ cairn/DESIGN.md cairn/ROADMAP.md`
      is read, and no matched site outside `cairn/milestones/archive/` retains a
      claim that one of these is the blamed call for an argument this milestone
      fixes. Archived milestones are history and stay unedited. `R/ffmpeg.R:1367-1372`
      is corrected too, named here because it makes the claim without naming a
      builder and so the grep does not reach it.
- [ ] AC7: Each sentence of the NEWS entry cites, in a table in this file, the
      test file and `test_that()` title that AC5's mutation run turns red without
      it. `devtools::test()` clean and `devtools::check()` `Status: OK`.

## Coverage

- AC1 → T1, T2, T3, T4
- AC2 → T1, T4
- AC3 → T6
- AC4 → T5
- AC5 → T7
- AC6 → T8
- AC7 → T8

## Tasks

- [x] T1: Declare `tests/testthat/helper-blame-specs.R` and extend the M59 grid in
      `tests/testthat/test-value-check-front-door.R` to read it — cells for every
      M64 site, both deliveries, both forms — plus the completeness reader.
      Red first.
- [x] T2: `crop_video()` (`R/ffmpeg.R:1099`) sweeps `width`/`height`/`x`/`y`;
      `crop_video_batch()`'s sweep (`:5107`) extends to `x`/`y`. Rewrite the
      front-door comment at `:1109-1116`, whose reason ("which `ffm_crop()`
      validates") this task falsifies.
- [x] T3: `standardize_video()` (`:1355`) sweeps `width`/`height`/`fps` and
      `check_token(pixel_format)`; `standardize_video_batch()` (`:3708`) gains
      the same sweep over argument and column. Rewrite `:1367-1372`.
- [x] T4: `sample_frames_batch()` (`:3483`) sweeps each row through
      `resolve_sample_fps()`; add the pin holding `sample_frames()`'s existing
      self-naming.
- [x] T5: `data-raw/blame-precedence.R` — crossing list, live controls, run at
      the merge-base and on the branch.
- [x] T6: `data-raw/blame-baseline.R` — blame + message at both refs; write the
      Deviations table.
- [x] T7: `data-raw/blame-guard-mutations.py` — diff-derived mutation list,
      per-sweep ownership check, reader/control mutations.
- [x] T8: AC6 sweep and corrections; NEWS entry + citation table; roxygen where
      a `@param` describes who refuses the value; D-entry recording the
      front-door-sweep choice against threading `call`.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: plan gate chose a Layer-2 front-door sweep calling the shared checker over adding a `call` argument to the exported `ffm_*` builders because D037 licenses Layer-2 validation and M59-D1 already rejected the signature change; falsified by a checker whose abort cannot be aimed at a Layer-2 caller from the verb's own frame.
- 2026-08-08: plan gate chose reusing `resolve_sample_fps()` in `sample_frames_batch()` over sweeping with `check_dim()` because the two forms would otherwise word one complaint two ways; falsified by a row value the resolver accepts and `check_dim()` refuses.
- 2026-08-08: session checkpoint — T1-T4 and T6 done, suite green, branch pushed. Remaining: T5 (precedence crossings at both refs), T7 (mutation harness), T8 (AC6 sweep, NEWS + citation table, roxygen, D-entry). Resume from the branch; nothing is held in session state.
- 2026-08-08: T6 baseline at the merge-base and the branch — 30 cells, 0 vacuous either side, 24 blame moves, 0 pinned cells moved, 0 non-pinned cells failed to move, `In index:` 16 -> 0. Message drift is 3 cells, all the `format` -> `pixel_format` argument-name fix recorded as M64-D1.
- 2026-08-08: gated Scope amendment — Scope In promised a front-door sweep for `standardize_video`'s `pixel_format`; it is checked inside `standardize_pipeline()` with `call` threaded instead, siting it where the value was already read (after both codec seams) so its reporting order is unmoved. Blame lands on the verb either way. Gate chose that over the planned front-door siting, which would have made a bad pixel format outrank a bad codec — a reordering AC4 does not permit.
- 2026-08-08: T2-T4 done; full suite FAIL 0 / PASS 5260, the 4 warnings all the pre-existing dropped-audio-track diagnostic in unrelated files. `standardize_video()`'s `pixel_format` is checked inside `standardize_pipeline()` with `call` threaded rather than at the front door, because the pipeline reads it AFTER both codec seams and hoisting it would have moved a bad pixel format ahead of a bad codec; the batch sibling carries the front-door sweep instead, where the reordering is the one AC4 permits.
- 2026-08-08: T1 grid red first — 24 of 30 cells fail; the 6 green are `crop_video_batch`'s width/height (swept at M59) and `sample_frames`' two pinned scalar cells, which is the expected split. Completeness reader green.
- 2026-08-08: pre-implementation gate amended AC1 — `data-raw/` is in `.Rbuildignore`, so a test sourcing the spec list from there would skip under `R CMD check`, unenforced in exactly the run the release gate uses (LESSONS M51/M59). The list moves to `tests/testthat/helper-blame-specs.R` and the `data-raw/` scripts read it from the source tree; gate chose that over a second copy in the test tree, which no test could detect diverging.
- 2026-08-08: pre-implementation gate amended AC4 — the plan demanded unchanged precedence everywhere, but `standardize_video_batch()` reads its dimension values inside `pmap` today, AFTER `check_nvenc_available()` (`R/ffmpeg.R:3832`), so a front-door sweep necessarily flips that pair. Gate chose matching `crop_video_batch()`'s M59 placement (value above nvenc, `R/ffmpeg.R:5107` vs `:5118`) over preserving precedence by sweeping last, because a machine-independent refusal reporting before a machine-dependent one is the rule D036 already states and the alternative would make the two batch verbs disagree; falsified by a caller for whom the encoder's absence is the more actionable of the two.
- 2026-08-08: criteria audit ([O], fresh context) returned defects on all seven drafted criteria — a `formals()`-derived domain that enumerated the wrong set, a baseline recording `conditionMessage()` where blame lives in `conditionCall()`, an all-cells-excluded vacuity hole in the precedence criterion, an unbounded "each other front-door guard", a mutation criterion satisfiable by another sweep's red, an AC6 naming a site its own grep misses and reaching archived history, and an unlocated NEWS citation. All seven rewritten before writing; three gate-changed criteria re-asked the audit's three questions and passed.
- 2026-08-08: T5 precedence grid at the merge-base and the branch — 82 crossings, 0 dead controls and 0 unresolved cells on either ref; winners moved on exactly the 3 nvenc `_batch` crossings, recorded as M64-D2's reordering table. Suite FAIL 0 / PASS 5260.
- 2026-08-08: T7 mutation harness — 12 sites derived from the branch diff, each red on ≥1 cell its own verb's grid owns; a planted-defect reader test added (a neutered reader passed the empty check, since the real list has no defects), red under the reader mutation; the control mutation pair shows the dead-control report appears when a crossed guard is removed and vanishes when the control check is neutered. All 15 red; suite FAIL 0 / PASS 5262.
- 2026-08-08: T8 AC6 sweep — the criterion's grep matched 254 sites; six retained the inherited-from-the-builder claim (the crop and sample pipeline headers, standardize_pipeline's dim comment, both batch verbs' column-guard comments, and crop_video_batch's roxygen intro) and were rewritten, man/ regenerated. No `@param` text named a refuser, so the roxygen half of T8 is that one intro.
- 2026-08-08: T8 NEWS entry + M64-D3 citation table; the "one wording in both forms" sentence was cut rather than cited (the both-forms test normalizes the names away, so no AC5 mutation reds it); a value-before-nvenc ordering test and the `pixel_format` arg-name pin were added so the remaining sentences cite observed reds; harness re-run, all 15 red. D042 records the sweep-over-`call`-threading rule. `devtools::check()` Status: OK, 0/0/0.
- 2026-08-08: amendment return: AC1 — "Every script that consumes the spec list reads it from this helper in the source tree — `data-raw/blame-baseline.R` is the one consumer — and no second copy of the list exists anywhere; the AC4 and AC5 scripts declare no cell list of their own." The drafted sentence had all three evidence scripts reading the helper, written before T5/T7 existed to show two of them consume no cell list (one declares crossings, one derives sites from the diff, in Python). Same commit corrects the helper header, which named `blame-precedence.R` as a reader it never was. Re-review resumes immediately.

## Decisions

### M64-D3 — the NEWS citation table (2026-08-08, from T8)

AC7: each sentence of the NEWS entry, with the `test_that()` title that AC5's
mutation run turns red without the behavior it claims. Every title is in
`tests/testthat/test-builder-blame-front-door.R`.

| NEWS sentence (abbreviated) | red without it |
|---|---|
| "A bad crop, scale, rate or pixel-format value is now refused by the function you called." | "a builder-bound value blames the verb the user called" — red under all 12 diff-derived sweep mutations |
| "… used to be reported against an internal builder … or … against `purrr::pmap()` with an `In index:` prefix." | same title — its no-leak assertions (`ffm_`, `pmap`, `In index:`) are this sentence's claim |
| "Each `_batch` sibling refuses the value whether it is passed as the argument or carried in a `jobs` column, and before any row runs." | same title — the spec list carries both deliveries per batch cell and asserts `In index:` absent |
| "One message changes: a malformed `pixel_format` … now names `pixel_format`." | same title — the pixel-format cells' expected message pins the arg name; observed red under both pixel-format sweep mutations (2026-08-08) |
| "… a call that is also wrong about `hardware = "nvenc"` … is now told about the bad value first …" | "a bad batch value reports before a missing nvenc encoder" — observed red under the standardize dims and pixel-format sweep mutations (2026-08-08) |

One drafted sentence was cut instead of cited: "Both forms of each verb
refuse a bad value with one wording." The both-forms test compares the
guard's sentence from "must be" onward, deliberately excluding the names
before it, so no AC5 mutation turns it red — and the entry narrows to what a
named test enforces rather than citing a test that does not enforce it.

### M64-D2 — the reordering table: the three crossings the sweeps reassign (2026-08-08, from T5's grid)

AC4 permits winner changes exactly where a new sweep now precedes
`check_nvenc_available()` on a `_batch` verb, and the measured set is exactly
that (`data-raw/blame-precedence.R`: 82 crossings at the merge-base and on the
branch; 0 dead controls and 0 unresolved cells on both refs):

| Crossing | merge-base | branch | The call whose answer changes |
|---|---|---|---|
| `crop_video_batch/xy` × nvenc-unavailable | nvenc | sweep | `crop_video_batch(jobs, width = 160, height = 120, x = -1, hardware = "nvenc")` on a machine whose FFmpeg lists no nvenc encoder: it said the nvenc encoder is not available, it now says `` `x` must be a single FFmpeg expression or number `` |
| `standardize_video_batch/dims` × nvenc-unavailable | nvenc | sweep | same machine, `standardize_video_batch(jobs, width = 0, hardware = "nvenc")`: the nvenc abort gives way to `` `width` must be a single FFmpeg expression or number `` |
| `standardize_video_batch/pixel_format` × nvenc-unavailable | nvenc | sweep | same machine, `standardize_video_batch(jobs, pixel_format = "yuv 420p", hardware = "nvenc")`: the nvenc abort gives way to `` `pixel_format` must be a single clean token `` |

The other 79 crossings report the same guard on both refs. The reordering is
the one the gated AC4 amendment chose: a value wrong on every machine reports
before an encoder missing on this one (D036), matching `crop_video_batch()`'s
M59 width/height placement.

### M64-D1 — the pixel-format message names the caller's argument (2026-08-08, from T6's baseline)

AC3 requires every cell's message identical on both refs, with any difference
listed here and reasoned. Three cells differ, all the same difference:

| Cell | merge-base | branch |
|---|---|---|
| `standardize_video/pixel_format` | `` `format` must be a single clean token. `` | `` `pixel_format` must be a single clean token. `` |
| `standardize_video_batch/pixel_format/arg` | same | same |
| `standardize_video_batch/pixel_format/column` | same | same |

Not a rewritten message — the same sentence from the same `check_token()`, with
a different `arg`. `ffm_pixel_format()`'s parameter is named `format`, so
`caller_arg()` reported the builder's name for an argument the caller spells
`pixel_format`; the user was told to fix an argument that does not exist on the
verb they called. Checking at the verb's own layer names the verb's own
argument. The other 27 cells' messages are byte-identical.
