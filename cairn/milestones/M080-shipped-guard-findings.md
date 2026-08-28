<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M080: The guard says what is wrong, and refuses NA

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m080-shipped-guard-findings` / https://github.com/jmgirard/tidymedia/pull/84

## Goal

The four defects in SHIPPED behavior carried by the M62/M63/M64 finding row —
an `NA` that crashes bare, an `NA` that compiles, an abort naming a carrier
that is correct, and a duplication guard that hides the typo underneath it.

## Scope

Surface tier: **user-facing** — every item changes what a caller sees.

**In:** (a) `check_dim()` refuses `NA` of every type, closing both the
`NA_real_` bare crash (M64 F4) and the `NA_character_` passthrough that
compiles `crop=w=NA` — found while auditing this plan's criteria, not filed in
the row; (b) an enumerated NA sweep over the package's one-argument `check_*`
predicates, fixing the ones it reddens; (c) `check_batch_inputs()` names only
the carriers actually holding a bad path (M62 N3); (d) the derived-output
duplicated-input guard reports after the path sweep, so NEWS.md:552's
"one path typed wrong the same way in twenty rows is one missing file" is
observable off the explicit-output path (M62 N7).

**Out:** the nine instrument findings still in that candidate row — M62 N2,
M63 C1/A5/A8/A9, M64 F5/F7/F10/F11 — which stay there; the table-taking
`check_*` predicates the NA sweep's domain filter excludes, whose bare errors
no user call reaches, which stay unfixed and undocumented.

## Acceptance criteria

- [x] AC1: `check_dim()` signals a condition inheriting `rlang_error` and
      naming its `arg` on each of `NA`, `NA_integer_`, `NA_real_` and
      `NA_character_` — the whole of its NA domain. In particular
      `crop_video(f, o, NA_character_, 100, run = FALSE)`, which today returns
      `-vf "crop=w=NA:h=100:..."`, aborts instead.
- [x] AC2: over the exported verbs `tm_reaches(tm_call_graph(), v, "check_dim")`
      returns, each verb given `NA` of each of the four types in each carrier
      its declared call shapes name aborts with a condition inheriting
      `rlang_error`, blamed on the verb the caller typed, whose message names
      that carrier — the argument as the caller typed it, the `jobs` column, or
      the column of a `regions` frame — and refuses it in one of the five
      wordings a caller can reach: `check_dim()`'s "must be a single FFmpeg
      expression or number"; a column's NA guard ("must not contain NA", "must
      be numeric (no NA)"); a column's type guard ("must be numeric or
      character", "must be numeric"); the sampling-rate resolver's "must be a
      single positive number [or a string]"; or the scalar number checks on
      `scale` and `margin` ("must be a number", "must be a whole number"). A
      type-guard refusal counts only where the same call carrying a non-NA
      value of that type is refused the same way, so a type wording can never
      stand in for a missing NA refusal.
- [x] AC3: no predicate in the domain
      `ls(asNamespace("tidymedia"), all.names = TRUE, pattern = "^check_")`
      restricted to those with exactly one required formal not named `jobs`
      (15 names on 2026-08-28) signals a bare `simpleError` on `NA`,
      `NA_integer_`, `NA_real_` or `NA_character_`: every error signalled
      inherits `rlang_error`. The four reddening today are green —
      `check_dim` (`NA_real_`), `check_overlay_scale` (all four),
      `check_region_values` (all four), `check_codec_needs_reencode`
      (`NA_character_`).
- [x] AC4: `check_batch_inputs()` names in its abort only the carriers holding
      a path that cannot be read. `picture_in_picture_batch()` reports
      `` `jobs$overlay` names 1 file that can't be found or read. `` when only
      `overlay` is bad, `jobs$main` alone when only `main` is, and both when
      both are — each cell exercised with an absent path and with the verified
      mode-000 fixture `helper-input-paths.R` builds, since D041 made the
      predicate readability.
- [x] AC5: over the verbs `tm_reaches(tm_call_graph(), v, <the extracted
      duplicated-input helper>)` returns, a `jobs` table with no `output`
      column whose rows all name the same absent input reports the absent
      input, not the duplication. The abort's wording lives at one site, so a
      later verb inherits the order rather than restating it.
- [x] AC6: `devtools::check()` clean (0 errors, 0 warnings, no new notes).
      NEWS.md records three user-visible changes — `check_dim()`'s NA refusal
      including the `NA_character_` compilation it closes, the per-carrier
      naming, and the new guard order — and its existing paragraph stating
      that shape and column-type guards report before the path sweep is
      corrected to match.

## Coverage

- AC1 → T1, T2
- AC2 → T2, T3
- AC3 → T3
- AC4 → T1, T4
- AC5 → T1, T5, T6, T8
- AC6 → T7, T8

## Tasks

- [x] T1: red first — one failing test per finding: `check_dim(NA_real_)`'s
      bare `missing value where TRUE/FALSE needed`, the `crop=w=NA`
      compilation, `picture_in_picture_batch()`'s over-naming, and a
      derived-output table whose duplicated absent input reports the
      duplication rather than the path.
- [x] T2: `check_dim()` (`R/utils.R:207`) refuses NA of every type at its one
      site; record the blame spelling on both the scalar form and the `_batch`
      form, where `check_batch_cell()` wraps it.
- [x] T3: the AC3 sweep test over the `ls(asNamespace(...))`-enumerated domain;
      the declared per-verb `check_dim()` call shapes in
      `tests/testthat/helper-na-guards.R` plus a reader that re-derives the
      verb set from `tm_call_graph()`, errors on any verb it returns with no
      entry, and — deriving the carrier vocabulary as the union of the names
      the entries declare — errors on any verb whose formals, or whose body's
      `jobs`-column literals, carry a vocabulary name its entry omits; fix
      `check_overlay_scale()`, `check_region_values()` and
      `check_codec_needs_reencode()`.
- [x] T4: `check_batch_inputs()` (`R/ffmpeg.R:4672`) filters `col` to the
      carriers holding bad paths before calling `check_paths_readable()`,
      leaving D041's one abort site and one wording untouched.
- [x] T5: extract the three inline duplicated-input aborts (`R/ffmpeg.R:1958`,
      `3965`, `4421`) into one shared helper, and move each verb's
      `check_batch_inputs()` call above its auto-name block so the path
      reports first. `reject_duplicate_outputs()` is not moved: it runs on
      already-derived outputs and its collision message is the right one.
- [x] T6: add a derived-output axis to `data-raw/input-guard-baseline.R`'s
      form set — every cell today supplies an explicit `output`, which is why
      the grid never saw N7 — and re-run at both refs, recording which cells
      moved.
- [x] T7: D057 narrowing D040's ordering paragraph for the derived-output
      duplication guard, with its falsifier; the NEWS entries and the
      correction AC6 names; `devtools::document()`; `devtools::check()`.
- [x] T8 (added 2026-08-28, review return 1): the sweep and the derived-output
      block move down as a unit in `anonymize_video_batch()` and
      `standardize_video_batch()`, below the `audio_stream` column guard and
      the two scalar checks (F1, F2); two above-the-sweep crossings added to
      `data-raw/input-guard-baseline.R` that redden on the returned commit
      (F3); D058 fixing the sweep's upper bound; the NEWS ordering paragraph
      and the third bug-fix entry corrected to the shipped order.
- [x] T9 (added 2026-08-28, review return 2): the NEWS ordering paragraph and
      the third bug-fix entry corrected to the order measured off the package
      (A1); D058's rule narrowed to the sweep's position relative to what was
      already above it, with the both-ways split recorded (A2); the
      `scalar_arg` crossing's rationale narrowed to what that crossing proves
      (A3); the stale `input_guard_ordering()` docstring (A5) and the stale
      "END of this verb's front-door validation" comment at both named sites
      (A7); a below-the-sweep pin in `test-input-path-front-door.R`.

## Work log

- 2026-08-28: created by /milestone-plan; promotes the four shipped-behavior items from the M62/M63/M64 finding row, leaving its nine instrument findings in place.
- 2026-08-28: criteria audit ran in FULL mode (user-facing tier), fresh-context [O] reader over the step-2 draft. Returned findings on all five drafted criteria: AC1's `grep -n "check_dim(" R/*.R` domain was a proxy (7 comment hits, blind to the indirect `arg = dim` sites, and `R/` is absent under `R CMD check`) — replaced with the `tm_call_graph()` walk; AC1 probed only `NA_real_` where the promise said any NA — all four types now probed, which is how the `NA_character_` passthrough was found; AC1 left the `_batch` blame spelling undetermined — now stated per form; AC2's `ls()` lacked `all.names = TRUE` and its pass condition did not classify warnings — both fixed; AC2's domain mandated an unreachable refusal in `check_batch_inputs` — narrowed at the gate; AC3 never probed its own noun "unreadable" — mode-000 cell added; AC4's second sentence bound the baseline grid rather than the package — moved to T6; AC5 bound a D-entry's existence — moved to T7; and no task recorded the NA refusals in NEWS — AC6 now does.
- 2026-08-28: plan gate chose reordering the guards over amending NEWS.md's twenty-rows claim, because the duplication message never mentions the typo the caller can act on, which is D040's own argument for the path reporting first; falsified by a report preferring the duplication on a table that is both wrong about a path and duplicated.
- 2026-08-28: the gate's reorder option named `reject_duplicate_outputs()`; the reproduced case is refused by the three inline derived-output blocks instead, so T5 was scoped to those and `reject_duplicate_outputs()` left alone. Chosen over moving both, because that guard runs on already-derived outputs where the collision is the correct message; falsified by a report of an explicit-output table whose collision hid a missing path.
- 2026-08-28: plan gate chose the NA sweep domain "one required formal not named `jobs`" over an explicit list of scalar-value predicates, because a hand-list is not a procedure (M118) while the formal's name is mechanical; falsified by a table-taking predicate whose required formal is spelled something other than `jobs`.
- 2026-08-28: plan gate chose one milestone over splitting the NA family from the input-path items, because both are the front-door guard family and the baseline grid is re-run once; falsified by the plan-owned body or the review outgrowing one reviewable PR.
- 2026-08-28: implementation gate chose, for the three NA fixes the sweep reddens: the region-value checker re-calls `check_regions()` rather than restating a shape refusal; `check_codec_needs_reencode()` takes `rlang::check_bool(reencode)` rather than reading a non-flag as FALSE; and `check_dim()`/`check_overlay_scale()` reuse their existing refusal wording for NA rather than adding a second message each.
- 2026-08-28: T1 — four red tests, one per finding: `check_dim()` on all four NA types (`test-na-value-guards.R`), `crop_video(width = NA_character_)` compiling `crop=w=NA`, `picture_in_picture_batch()` reporting `` `jobs$main` and `jobs$overlay` `` when only `overlay` is bad, and `standardize_video_batch()` on a duplicated absent input reporting the duplication. Each fails as its finding describes; the suite is deliberately red at this commit.
- 2026-08-28: T2 — `check_dim()` refuses NA of every type by testing `!anyNA(x)` ahead of both halves of its predicate, at its one site and with its existing wording. Blame recorded on both forms: the scalar form names the argument the caller typed (`crop_video()` -> `` `width` ``); on the `_batch` form an NA CELL never reaches `check_dim()` at all — `crop_video_batch()` types its dimension columns first, so the caller sees `The width column of `jobs` must not contain NA.` — while an NA delivered as the verb's own argument reaches it through `check_batch_cell()` with no row locator.
- 2026-08-28: T4 — `check_batch_inputs()` tests each carrier separately and names only those holding a path that cannot be read, in one call, so both are still named when both are bad. `check_paths_readable()`'s predicate, wording and abort site are untouched (D041). Exercised on `picture_in_picture_batch()` over both halves of the predicate: an absent path and the verified mode-000 fixture.

- 2026-08-28: AC2 amended at a mini gate, and the amended wording audited twice by fresh-context [O] readers before it was written. The `_batch` clause it replaces was unsatisfiable: measured on the branch, no `_batch` verb routes an NA CELL into `check_dim()` — each verb's own column guard refuses it first, naming the column and carrying no `check_batch_cell()` row bullet — so honouring it meant deleting shipped column guards. The amended criterion promises the carrier is named and the refusal is one of five reachable wordings, with a control proving a type complaint is about the type. Audit round one returned four findings (the `regions`-frame carrier unnamed; "names the argument" satisfiable by a column-type abort that never mentions NA; the argument axis a hand-list no procedure enumerates; two sentences binding the test harness rather than the package — those moved to T3). Round two returned three (the sampling-rate resolver's two wordings missing from the list; the type-guard branch definitionally open; `picture_in_picture()`'s `scale`/`margin` wrongly declared to carry no value). All seven are answered in the wording above and in `helper-na-guards.R`.
- 2026-08-28: T3 — the sweep runs over the 15 formals-enumerated predicates and finds no bare `simpleError` and no warning on any of the four NA types; `check_overlay_scale()` refuses NA at its existing range wording, `check_region_values()` re-calls `check_regions()` for the shape rather than restating it, and `check_codec_needs_reencode()` takes `rlang::check_bool(reencode)`. The AC2 sweep covers 17 verbs and 44 declared carriers; its completeness reader caught six omissions in the first draft of the shapes (`crop_video_batch` height/x on both axes, `standardize_video_batch` height on both, `sample_frames_batch` interval as an argument), which is the reader working.
- 2026-08-28: T5 — the three inline duplicated-input aborts became `reject_duplicate_inputs()`, and in each of the three verbs `check_batch_inputs()` moved above the derived-output block. Task wording said to move the sweep; the first attempt moved the block down instead, which put the codec token check ahead of the duplication check and reddened `test-codec-arg-front-door.R`'s precedence pin in two verbs — the sweep moved up, as written, leaves that precedence intact and puts the path above both.
- 2026-08-28: T6 — the derived-output axis went into `data-raw/input-guard-baseline.R` as a CROSSING (`derived_output`, generated over the `dup` form) rather than a form: with no `output` column the control necessarily duplicates a present path and is refused by the duplication guard, which is a crossed cell's shape, not a `none` cell's. Which verbs carry it is derived from the same call-graph walk AC5 uses (reaching `reject_duplicate_inputs()`), not listed. Re-run at `origin/master` and HEAD: every reader empty — vacuous, refusals, message regressions, blame regressions, missing call, dead controls, misordered, unreported, unnamed, uncovered — and exactly three cells moved, `anonymize_video_batch`, `standardize_video_batch` and `normalize_audio_batch` at `dup`/`derived_output`, from `derived_output` to `input`. `picture_in_picture_batch`'s `one` cell changed wording (both carriers named to one), declared as an `INPUT_GUARD_WORDING` substitution the way M63's was. `input_guard_blame_unexpected()` reports all 30 unreadable cells on this ref pair: its expectation is M63's about the M62->M63 pair, an instrument limit filed as A10 on the finding row, not a regression here.
- 2026-08-28: T7 — D057 appended, narrowing D040's ordering paragraph for the derived-output duplication guard and stating what does not move (`reject_duplicate_outputs()`), with its falsifier. Three NEWS entries under Bug fixes — the NA refusal including the `crop=w=NA:h=100:x=(in_w-out_w)/2:y=(in_h-out_h)/2` command it used to compile (read off `origin/master`, not recalled), the per-carrier naming with both wordings, and the new guard order — and the existing ordering paragraph gained the clause AC6 asks for. `devtools::document()` clean; `devtools::check()` Status: OK, 0 errors / 0 warnings / 0 notes (one spelling NOTE appeared first, on a word this milestone introduced; the sentence was reworded rather than the wordlist widened).
- 2026-08-28: all tasks done, status to review.
- 2026-08-28: review returned M080 to in-progress (defect return 1). AC6 fails: T5's move of `check_batch_inputs()` above the auto-name block also carried it above `check_batch_audio_col(jobs, "audio_stream")` and above `check_token(video_codec)`/`check_number_whole(audio_stream)` in `anonymize_video_batch()` and `standardize_video_batch()`, so a column-type error and a scalar-argument error now report AFTER the path sweep, while `NEWS.md:594` still states that wrong column types report before it — the paragraph AC6 requires to be corrected to match. Reproduced at review on both refs. AC1-AC5 verified with fresh evidence and stand; the consistency gate passed; `devtools::check()` clean. Eight further findings (F4-F11) logged in the Review section, none falsifying a criterion.
- 2026-08-28: implementation gate on the returned defect chose restoring both guards above the sweep -- the sweep and the derived-output block move DOWN as a unit -- over restoring the column guard alone, because the alternative leaves a scalar-argument-versus-path reassignment standing on an axis this milestone never set out to touch. The cost, taken deliberately: on the two verbs carrying both, the duplication refusal is now below the scalar checks too, so a duplicated table with a bad `video_codec` reports the codec where it reported the duplication; recorded as D058, in NEWS, and re-pinned in `test-input-path-front-door.R`. Falsified by a report preferring the duplication on such a table.
- 2026-08-28: T8 -- in `anonymize_video_batch()` and `standardize_video_batch()` the sweep and the derived-output block moved down together, below `check_batch_audio_col()`, `check_token(video_codec)` and `check_number_whole(audio_stream)`, restoring every precedence F1 and F2 named. `normalize_audio_batch()` already had that shape and is untouched, which the new test's third verb holds. F3: two above-the-sweep crossings, `column_type:stream` and `scalar_arg`, derived from the guard calls in each verb's own body rather than listed, and gated on the verb having the sweep at all (five scalar verbs check the same argument against a different input guard). Grid re-run at `origin/master` and HEAD: every reader empty except the 30 pre-existing unreadable-cell blame rows (A10), the same three `derived_output` cells moved, 662 cells over 16 crossings. Run against the returned commit `14b0cd6` the two new crossings redden on 8 cells -- the two verbs, both crossings, the `all` and `one` forms -- so they can fail on the defect they were added for. The codec-versus-duplication cells left `test-codec-arg-front-door.R` for `test-input-path-front-door.R` and read the other way up, per the gate above. `devtools::test()`: 0 failures, 7957 passing, 5 skips, 12 warnings.
- 2026-08-28: `devtools::check()` Status: OK, 0 errors / 0 warnings / 0 notes, 3m 4s. T8 checked; status to review (defect return 1 answered).
- 2026-08-28: review returned M080 to in-progress (defect return 2). AC6 fails again: T8's correction to NEWS.md's ordering paragraph overshoots -- it now promises that "the verb's own checks on its scalar arguments still report before" the path sweep, and the third bug-fix entry that "the checks a verb makes on its own arguments all sit above the missing-file sweep", both false. Reproduced at review with controls: `standardize_video_batch(tibble(input = "gone.mp4"), width = NA_real_)` reports `jobs$input` where the readable-input control reports `` `width` must be a single FFmpeg expression or number. ``; likewise `anonymize_video_batch(color = 123)` and `pixel_format = 123`, `normalize_audio_batch(target_loudness = 999)`, `picture_in_picture_batch(margin = -5)` and `position = "nope"`, and `compare_videos_batch(direction = "nope")`. The same overshoot is written into D058, whose own falsifier it fires, into the `scalar_arg` crossing's rationale, and into a stale docstring and a stale comment (A1, A2, A3, A5, A7). AC1-AC5 re-verified with fresh evidence and stand; the consistency gate passed; `devtools::check()` clean and `devtools::test()` 0 failures / 7957 passing. A4, A6 and F4-F11 carry to the re-review gate. Thrash rule trigger (b) fires: AC6 has failed twice by the same shape of mechanism.

- 2026-08-28: implementation gate on defect return 2. Thrash trigger (b)'s remedy — reconsidering the plan gate's recorded alternative, reverting the reorder and amending NEWS's twenty-rows claim instead — was offered and declined; the shipped reorder stands and only the prose describing it is corrected. Chosen because both failures were in composing the claim, not in the code: AC1-AC5 verify, the reorder is what D040's own argument asks for, and reverting would undo AC5 and D057 to fix a sentence. Falsified by a third return whose defect is in the shipped order rather than in a description of it.
- 2026-08-28: gate also asked whether to pin the claim with a test. The scope offered first — a check reading NEWS.md and comparing it to measured behaviour — was withdrawn on the user's challenge: it is a new instrument built to catch a writing failure, the shape the M62/M63/M64 row already carries nine findings about, and D-064's detector-rendering doctrine warns that a guard restating the prose reads as assurance while asserting nothing. Recorded honestly: NO test catches a prose/code mismatch, and the pin added below would not have caught A1. What was pinned instead is the behaviour the corrected paragraph leaves promised.
- 2026-08-28: T9 — the correction was derived, not composed, and the derivation falsified two drafts before one survived. Draft 1 (the review's own characterisation, "type/token checks above the sweep, value and vocabulary below") is false: `picture_in_picture_batch(margin = "x")` is a TYPE check reporting AFTER the sweep. Draft 2, taken to the gate and approved, said "every other check a verb makes on its own arguments reports after it"; deriving it over every export reaching `check_batch_inputs()` falsified it before it shipped — `compare_videos_batch()` checks `resize` and both codec tokens above the sweep, `picture_in_picture_batch()` checks `scale`, `segment_video_batch()` checks `reencode`, `sample_frames_batch()` checks `outdir`. The shipped wording therefore promises NO ordering for a verb's own arguments and gives the both-ways case on one verb (`standardize_video_batch()`: `video_codec` before, `width` after). The one uniform claim the paragraph does keep was checked the same way: no shape guard and no column-type guard sits below the sweep in any verb.
- 2026-08-28: T9 — `devtools::test()`: 0 failures, 7967 passing (7957 before, +10 from the new pin), 5 skips, 12 warnings. `devtools::check()` Status: OK, 0 errors / 0 warnings / 0 notes, 2m 32s. T9 checked; status to review (defect return 2 answered).
- 2026-08-28: T9 — D058 narrowed to "the sweep is never lifted past a check that already sat above it", with the per-argument split recorded on four verbs and its falsifier re-cut so a check that was always below the sweep no longer fires it. A3's crossing rationale now claims only the `audio_stream` check; A5's docstring points at `INPUT_GUARD_ABOVE` rather than re-listing it; A7's comment at `R/ffmpeg.R` both named sites says what now follows it. The third site (`normalize_audio_batch()`'s `audio_codec`) was left alone — nothing but the fan-out follows it there, so its comment is still true. New pin: five below-the-sweep cells, each with a readable-path control proving the argument check fires at all.

## Decisions

## Review

PR: https://github.com/jmgirard/tidymedia/pull/84 (draft, opened 2026-08-28)

### Acceptance-criterion evidence

- **AC1 — verified.** `devtools::load_all()`, then `check_dim(v, arg = "width")` on each of `NA`, `NA_integer_`, `NA_real_`, `NA_character_`: all four signal a condition whose classes are `rlang_error, error, condition` and whose message is `` `width` must be a single FFmpeg expression or number. `` — the `arg` named in every case. `crop_video(f, o, NA_character_, 100, run = FALSE)` aborts with that same condition rather than returning the `crop=w=NA:h=100:...` filter string.

- **AC2 — verified.** `tm_reaches(tm_call_graph(), v, "check_dim")` over the exported verbs returns 17: `anonymize_video`, `anonymize_video_batch`, `crop_video`, `crop_video_batch`, `ffm_crop`, `ffm_drawbox`, `ffm_fps`, `ffm_overlay`, `ffm_scale`, `format_for_web`, `format_for_web_batch`, `picture_in_picture`, `picture_in_picture_batch`, `sample_frames`, `sample_frames_batch`, `standardize_video`, `standardize_video_batch`. Every one has a declared entry in `helper-na-guards.R` (`setdiff(verbs, names(specs))` empty), and the entries declare 52 carriers — 208 carrier x NA-type cells. Two of the 17, `format_for_web()` and `format_for_web_batch()`, declare zero: they reach `check_dim()` only with package-fixed dimensions and expose no caller-supplied carrier (no formal and no `jobs`-column literal in the vocabulary), which the completeness reader independently confirms. (The T3 work-log line records 44 carriers; the figure measured here at review is 52.) `test-na-value-guards.R`'s sweep runs all 208 and passes: each aborts with an `rlang_error`, blamed on the verb the caller typed (`conditionCall` matches `<verb>(`), naming the carrier as `` `arg` `` or `<arg> column`, in one of the five listed wordings. The type-guard control fires on every cell answered with a type wording alone and asserts message equality against the same call carrying a non-NA value of that type. The completeness reader — formals and `jobs`-column literals against the declared vocabulary — is green for all 17.

- **AC3 — verified.** `na_sweep_predicates()` enumerates the domain from `ls(asNamespace("tidymedia"), all.names = TRUE, pattern = "^check_")` filtered to exactly one required formal not named `jobs`: 15 names on 2026-08-28 — `check_audio_codec_not_copy`, `check_codec_needs_reencode`, `check_copy_map_conflict`, `check_dim`, `check_ffm`, `check_file_exists`, `check_file_readable`, `check_hardware_needs_encode`, `check_image_format`, `check_nvenc_available`, `check_overlay_scale`, `check_paths_readable`, `check_region_values`, `check_regions`, `check_token`. All four names the criterion calls out are in it. Run over 60 predicate x NA-type cells: 0 errors that are not `rlang_error`, 0 warnings. The four that reddened at `master` (`check_dim` on `NA_real_`, `check_overlay_scale` and `check_region_values` on all four, `check_codec_needs_reencode` on `NA_character_`) are green here; commit `f427f4f` holds their red form.

- **AC4 — verified.** `picture_in_picture_batch()` measured on all six cells. Absent path: overlay bad alone reports `` `jobs$overlay` names 1 file that can't be found or read. ``, main bad alone reports `jobs$main` alone, both bad reports `` `jobs$main` and `jobs$overlay` name 1 file … ``. Repeated with the mode-000 fixture `tm_unreadable_path()` builds in `helper-input-paths.R` (verified `file.access(p, 4) != 0`): the same three messages, so the per-carrier filter is over D041's readability predicate and not over existence.

- **AC5 — verified.** `tm_reaches(tm_call_graph(), v, "reject_duplicate_inputs")` returns 3 verbs: `anonymize_video_batch`, `normalize_audio_batch`, `standardize_video_batch`. For each, a `jobs` table with no `output` column whose two rows name the same absent input reports `` `jobs$input` names 1 file that can't be found or read. `` and the word "duplicated" does not appear. The control — the same table with a readable duplicated path — still reports `` `jobs` has duplicated input paths but no output column. ``, so the path report did not displace the duplication report. `tm_namespace_bodies()` finds the string `has duplicated` at exactly one name, `reject_duplicate_inputs`.

- **AC6 — NOT verified (see F1 below).** `devtools::check()` on the branch: `Status: OK`, 0 errors / 0 warnings / 0 notes, 2m 42s. `NEWS.md`'s Bug fixes section carries three new entries — the NA refusal (quoting the `crop=w=NA:h=100:x=(in_w-out_w)/2:y=(in_h-out_h)/2` command it used to compile and listing the verbs and builders the refusal covers), the per-carrier naming with both the one-carrier and both-carriers wordings, and the new guard order for the three deriving verbs. The existing ordering paragraph gained the clause the criterion names: the duplicated-input refusal on a verb deriving its own output names now reports after the path sweep. No milestone numbers appear in any of it. **But the second clause is unmet:** the existing ordering paragraph is not corrected to match — it still promises that wrong column types report before the path sweep, which the reorder made false for `anonymize_video_batch()` and `standardize_video_batch()`. See F1.

### Consistency gate

- `python3 cairn_validate.py` — exit 0; all 16 checks PASS, all 7 advisories OK (`release window` did not fire).
- No `DESIGN.md` principle changed on this branch, so `cairn_impact.py --changed` does not apply.
- Toolchain checks (`r-package` profile `consistency-gate` slot): `devtools::document()` produces no diff (working tree clean apart from this milestone file); `NAMESPACE`, `man/` and `data/` are untouched by the diff, so no generated file was hand-edited; `README.Rmd`/`README.md` are untouched and in sync; `pkgdown::check_pkgdown()` reports "No problems found" and the branch exports nothing new, so no `_pkgdown.yml` row is owed; `NEWS.md` carries this milestone's user-visible changes with no milestone numbers; no new top-level file, so no `.Rbuildignore` entry is owed; `devtools::check()` clean as recorded under AC6.
- `devtools::test()` on the branch: 0 failures, 7934 passing, 5 skips (absent hardware encoders), 12 warnings.

### Independent review — three lenses, fresh context

Surface tier is user-facing and the diff touches `R/`, so the full three-lens fan-out ran. Every finding reported is logged below with its disposition.

**[S] prior-review-record lens — no findings.** It read the M62/M63/M64 finding row in `ROADMAP.md` and the archived `## Review` sections touching these files, and checked each of the nine instrument findings the row keeps against the diff: none is reintroduced or contradicted. The probe `gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1` returned `[]`, so no GitHub thread walk was owed.

**[O] diff-bug lens — 11 findings.** **[S] blame-history lens — 2 findings**, one of which duplicates [O]'s F2 and one of which is out of scope. Ranked as reported, most severe first, with the review's own verification:

- **F1 — the path sweep now reports before a column-TYPE guard, and `NEWS.md` still says it does not. FLOOR RETURN.** Moving `check_batch_inputs()` above the auto-name block in `anonymize_video_batch()` (`R/ffmpeg.R:1955`) and `standardize_video_batch()` (`R/ffmpeg.R:3962`) also moved it above `check_batch_audio_col(jobs, "audio_stream")`, a column-type guard. Reproduced at review: `standardize_video_batch(tibble(input = "gone.mp4", output = o, audio_stream = "x"), run = FALSE)` reports `` `jobs$input` names 1 file that can't be found or read. `` on HEAD; the same call on `master` reports `The audio_stream column of 'jobs' must be numeric (NA to keep every audio track).` Same flip on `anonymize_video_batch()`. `NEWS.md:594` still reads "Malformed table shapes and wrong column types still report before it", which is now false for these two verbs — and it fires whether or not an `output` column is supplied, so D057, which narrows D040 only for the duplication guard, does not reach it. **AC6's second clause — the existing ordering paragraph "is corrected to match" — is therefore unmet.** Disposition: defect return.
- **F2 — the same move reassigns scalar-argument precedence, against the comment at the site.** `check_token(video_codec, allow_null = TRUE)` and `rlang::check_number_whole(audio_stream, ...)` now report after the path sweep in both verbs. Reproduced: `standardize_video_batch(tibble(input = "gone.mp4", output = o), video_codec = 123, run = FALSE)` reports `jobs$input` on HEAD where `master` reported `` `video_codec` must be a single string or `NULL` ``. The comment ten lines below the new call still says a guard moved up the function silently reassigns precedence, naming M41's review for catching exactly that. No NEWS entry, no D-entry. The [S] blame lens reported the same change independently and added that the crossing is pinned by no test. Disposition: to fix with F1 — the same decision settles both.
- **F3 — the baseline grid could not see F1 or F2, so "exactly three cells moved" understates the change.** `data-raw/input-guard-baseline.R:246`: `standardize_video_batch`'s crossings are `none, column_type, nvenc, run_guard, derived_output`, and `column_type` probes the verb's codec column, which is still above the sweep. No crossing supplies a bad scalar codec token or a bad `audio_stream` column, so both flips produce no moved cell. Disposition: to fix with F1 — a crossing that would have caught it.
- **F4 — `check_audio_codec_needs_reencode()` still crashes bare on NA.** Verified: `check_audio_codec_needs_reencode(NA, "aac")` raises a `simpleError`, `missing value where TRUE/FALSE needed` — the exact defect fixed one screen above in its twin (`R/ffmpeg.R:2815`), and called on the same line as that twin at `R/ffmpeg.R:3387`. It has two required formals, so AC3's domain filter excludes it and AC3 is not falsified. Not user-reachable today (the `reencode` column guard fires first) — the same reachability argument the milestone used to fix the twin anyway. Disposition: for triage at the re-review gate.
- **F5 — the Scope Out misdescribes what the domain filter excludes.** It calls the excluded set "the table-taking `check_*` predicates"; the filter excludes only predicates with more than one required formal, or whose sole one is named `jobs`. Verified: `check_regions()` (formal `regions`) and `check_region_values()` are both table-taking and both IN the 15-name domain. The plan gate's own falsifier for that choice — "a table-taking predicate whose required formal is spelled something other than `jobs`" — has already fired, unremarked. Scope is plan-owned, so this takes a gated amendment or a work-log correction, not a review edit. Disposition: for triage at the re-review gate.
- **F6 — the sweep probes predicates positionally.** `tests/testthat/test-na-value-guards.R:85` calls `f(vals[[i]])` after deriving the required formal by name and discarding it; a predicate whose required formal is not first would be probed on the wrong argument and pass vacuously. All 15 have it first today. Disposition: for triage at the re-review gate.
- **F7 — AC3's pass condition counts "no error at all" as a pass.** `check_audio_codec_not_copy`, `check_hardware_needs_encode` and `check_nvenc_available` accept all four NA types silently, so 12 of the 60 sweep cells assert nothing. Defensible for a character sentinel, less so for `NA_integer_`/`NA_real_` as a codec. AC3 asks only that no error be a bare `simpleError`, so this does not falsify it. Disposition: for triage at the re-review gate.
- **F8 — the carrier-completeness reader cannot detect a carrier omitted from every entry.** `tests/testthat/helper-na-guards.R:56-64` derives `vocab` from what the entries themselves declare. Related: `anonymize_video`/`_batch` declare only `x` and `width` of the `regions` carriers, because the `c("x","y","width","height")` literal lives in `check_region_values()` in `R/utils.R`, not in the verb body the column probe reads. The reviewer inspected all 17 verbs' formals and found no currently missing carrier. Disposition: for triage at the re-review gate.
- **F9 — `reject_duplicate_inputs()` hardcodes `jobs$input`** (`R/ffmpeg.R:4705`) where its sibling parameterizes the carrier (`check_batch_inputs(jobs, col = "input")`). Its own comment promises a later verb inherits the wording; a multi-input verb deriving outputs would inherit a guard checking the wrong column. Disposition: for triage at the re-review gate.
- **F10 — `check_bool(reencode, call = call)` uses the default `arg`** (`R/ffmpeg.R:2818`), so a reachable failure from `R/ffmpeg.R:3387` would read `` `reencode_rows[[i]]` must be `TRUE` or `FALSE` ``. Unreachable today. Disposition: for triage at the re-review gate.
- **F11 — every path is stat'd twice.** `R/ffmpeg.R:4684` computes `file.access(x, mode = 4)` to pick the bad carriers and `check_paths_readable()` recomputes it at `R/utils.R:83`. Correct, and the identical predicate is what keeps the filter from diverging from the abort — but it is a second copy of the predicate D041's one-predicate rule exists to prevent, kept in sync by hand. Disposition: for triage at the re-review gate.
- **[S] blame lens, item 1 — "AC2-AC6 unchecked while every task is checked" — rejected, out of scope.** Under AC fencing the review ticks each criterion box against its own fresh evidence; unticked boxes at review entry are the protocol, not a gap. All six were ticked here as their evidence landed.

The [O] lens also reported what it found sound: `check_dim()`'s `!anyNA(x)` covers all four NA types plus `NaN` without over-refusing; `check_overlay_scale()` and `check_region_values()` reuse existing wordings as the implementation gate decided; `reject_duplicate_inputs()`'s `call =` preserves the original blame frame; the per-carrier filter behaves correctly over both halves of D041's predicate; and the M080 tests assert specific messages rather than bare failure.

### Gate outcome

**Returned to `in-progress` on F1.** F1 demonstrates AC6 failing inside the domain of the procedure AC6 names: the `NEWS.md` ordering paragraph is not corrected to match the shipped guard order, because the reorder moved the path sweep above a column-type guard as well as above the duplication guard, and the paragraph still promises the opposite. F2 and F3 are the same defect seen from two other angles and are fixed with it. F4–F11 carry to the re-review gate for triage; none of them falsifies a criterion.

This is defect return 1 for M080.

---

## Re-review (round 2, 2026-08-28)

PR: https://github.com/jmgirard/tidymedia/pull/84 (draft). Branch synced: `origin/master` at `7a6f634`, unmoved since the branch was cut; `git rev-list --count HEAD..origin/master` = 0, so no merge was owed and the evidence below is off the current tip `17aa310`.

### Acceptance-criterion evidence (fresh, this round)

- **AC1 — verified.** `devtools::load_all()`, then `check_dim(v, arg = "width")` on `NA`, `NA_integer_`, `NA_real_`, `NA_character_`: all four signal classes `rlang_error, error, condition` with message `` `width` must be a single FFmpeg expression or number. `` — the `arg` named every time. `crop_video(f, o, NA_character_, 100, run = FALSE)` aborts with that same condition instead of returning the `crop=w=NA:h=100:...` filter string.
- **AC2 — verified.** The `tm_call_graph()` walk returns 17 verbs reaching `check_dim()`; `check_dim_specs()` declares an entry for all 17 (`setdiff` empty both ways) and 52 carriers = 208 carrier x NA-type cells. `format_for_web` and `format_for_web_batch` declare zero, the positive "no caller-supplied carrier" declaration. `test-na-value-guards.R` runs green.
- **AC3 — verified.** The domain re-derived at review from `ls(asNamespace("tidymedia"), all.names = TRUE, pattern = "^check_")` filtered to exactly one required formal not named `jobs`: 15 names, the same 15 recorded in round 1, including all four the criterion calls out. Over 60 predicate x NA-type cells: 0 errors that are not `rlang_error`, 0 warnings. (12 cells pass silently — recorded as F7, which AC3 does not forbid.)
- **AC4 — verified.** `picture_in_picture_batch()` on all six cells. Absent path: overlay-only → `` `jobs$overlay` names 1 file that can't be found or read. ``; main-only → `jobs$main` alone; both → `` `jobs$main` and `jobs$overlay` name 1 file … ``. Repeated with `tm_unreadable_path()` (fixture verified `file.access(p, 4) != 0`): identical three messages, so the filter is over D041's readability predicate.
- **AC5 — verified.** `tm_reaches()` returns 3 verbs. For each, a no-`output` table whose two rows name the same absent input reports `` `jobs$input` names 1 file that can't be found or read. `` with no mention of duplication; the readable-path control still reports `` `jobs` has duplicated input paths but no output column. `` `tm_namespace_bodies()` finds `has duplicated` at exactly one name.
- **AC6 — NOT verified (second round). See A1 below.** `devtools::check()`: `Status: OK`, 0 errors / 0 warnings / 0 notes, 2m 47s. `devtools::test()`: `FAIL 0 | WARN 12 | SKIP 5 | PASS 7957`. Round 1's F1 and F2 are fixed and re-verified: on both `anonymize_video_batch()` and `standardize_video_batch()` a wrong `audio_stream` column type, a bad `video_codec` and a non-whole `audio_stream` argument all report before the path sweep again. NEWS.md carries the three bug-fix entries and the ordering paragraph was rewritten. **But the rewritten paragraph is still not corrected to match the shipped order** — it now overshoots in the opposite direction, promising an ordering the package does not have, including on `standardize_video_batch()` itself. AC6's second clause remains unmet.

### Consistency gate

- `cairn_validate.py` — exit 0; 16 PASS, 7 advisories OK (`release window` did not fire).
- No `DESIGN.md` principle changed, so `cairn_impact.py --changed` does not apply.
- `r-package` toolchain slot: `devtools::document()` leaves the tree clean; `NAMESPACE`, `man/`, `data/` and `README*` untouched by the diff; `pkgdown::check_pkgdown()` "No problems found"; NEWS.md carries the user-visible changes with no milestone numbers; no new top-level file; `devtools::check()` clean as above.

### Independent review — three lenses, fresh context

**[S] blame-history lens — no new findings.** It traced the T8 reordering to `921a00f3e` (M41/M62-era), read the M41 comment at the site, and judged the inversion disclosed rather than silent: D058 states it, the M42 pin moved to `test-input-path-front-door.R` rather than being deleted, and D040/D041 are left standing. F9/F10/F11 confirmed unchanged, not worsened.

**[S] prior-review-record lens — no findings.** Probe `gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1` returned `[]`, so no thread walk was owed. It checked the nine instrument findings the M62/M63/M64 row keeps against the diff: none reintroduced or contradicted. It notes `INPUT_GUARD_WORDING` gained one substitution entry, additive to the mechanism A9 already critiques.

**[O] diff-bug lens — 7 findings (A1–A7), plus F4–F11 re-confirmed present.** Ranked as reported, with the review's own verification:

- **A1 — `NEWS.md` now claims an ordering the shipped code does not have; AC6's second clause is still unmet. FLOOR RETURN.** The rewritten paragraph reads "Malformed table shapes, wrong column types, **and the verb's own checks on its scalar arguments** still report before it", and the third bug-fix entry says "The checks a verb makes on its own arguments all sit above the missing-file sweep." Both are false. Reproduced at review, each with a control proving the check exists and fires on a good path: `standardize_video_batch(tibble(input = "gone.mp4"), width = NA_real_)` → `jobs$input`, where the readable-input control → `` `width` must be a single FFmpeg expression or number. ``; `anonymize_video_batch(..., color = 123)` and `pixel_format = 123` → `jobs$input`; `normalize_audio_batch(..., target_loudness = 999)` → `jobs$input`; `picture_in_picture_batch(..., margin = -5)` and `position = "nope"` → `jobs$main` and `jobs$overlay`, controls → `` `margin` must be a whole number … `` and the position vocabulary refusal; `compare_videos_batch(..., direction = "nope")` → `jobs$inputs`, control → `` `direction` must be one of "horizontal" or "vertical" … ``. Two of those are on `standardize_video_batch()`/`anonymize_video_batch()`, the verbs T8 touched, so this is not a distant-verb technicality. The split the code actually draws is type/token checks above the sweep, value and vocabulary checks below — D040's own split; "scalar arguments" erases it. The pre-M080 sentence ("shapes and wrong column types") was true package-wide; the correction introduced the falsehood. Disposition: **defect return**.
- **A2 — D058 states the same overshoot as a package-wide invariant, and its own falsifier has already fired.** `cairn/DECISIONS.md` D058: "A verb's own front-door checks on its jobs SHAPE, on its column TYPES, and on its SCALAR ARGUMENTS all report before the input sweep… the sweep keeps the last position in the front-door block." The sweep does not keep the last position: `check_number_whole(margin)`, `check_vocab_arg(position)` and `check_vocab_arg(direction)` sit below it, unmoved by this milestone. D058's declared falsifier is "a report preferring the missing path over … a bad scalar argument"; `standardize_video_batch(tibble(input = "gone.mp4"), width = NA_real_)` is that report, on a verb D058 was written for. Verified at review. Disposition: to fix with A1 — the entry needs narrowing to the type/token tier T8 actually restored.
- **A3 — the `scalar_arg` crossing's rationale overstates what it proves, so F3's blind spot is only half closed.** `data-raw/input-guard-baseline.R:33-36` says `audio_stream = NA` is "The LAST of each verb's scalar front-door checks, so a cell reading it proves the sweep sits below the whole scalar block." A1's evidence disproves that: `width`, `color`, `target_loudness` are scalar-argument checks below the sweep on those same verbs, and `check_bool(reencode)` follows `audio_stream` in `segment_video_batch()`. The crossing proves the sweep sits below the `audio_stream` check and nothing more — a future move past `color` would produce no moved cell, which is exactly F3's failure mode. Disposition: to fix with A1.
- **A4 — the two new crossings' verb membership is a fixed-string grep over deparsed bodies, and degrades silently to zero cells.** `column_type:stream` is gated on `grepl('check_batch_audio_col(jobs, "audio_stream"', body, fixed = TRUE)` and `scalar_arg` on `grepl("check_number_whole(audio_stream", …)`; both match 9 verbs today, but a reformat, a named-argument reorder or a wrapper drops a verb out with no reader complaining, since `input_guard_uncovered()` re-derives from the same declaration. The `derived_output` block twelve lines above uses `tm_reaches()` and cannot drift that way; the file's own comment claims membership is "derived, never listed", which a text match satisfies only in letter. Disposition: carries to the re-review gate for triage.
- **A5 — `input_guard_ordering()`'s docstring still enumerates the above-the-sweep crossings as "(`jobs_na`, `column_type`)"** (`data-raw/input-guard-baseline.R:1103`) while `INPUT_GUARD_ABOVE` twenty lines below now carries four names. Verified. Stale in the commit that widened it. Disposition: to fix with A1.
- **A6 — dead branch in `input_guard_error_crossing()`.** The `scalar_arg` classifier matches `` has("`video_codec` must be a single string") ``, but no crossing supplies a bad scalar `video_codec`, so that half can never fire. Harmless; it reads as coverage the grid does not have. Disposition: carries to the re-review gate for triage.
- **A7 — the "Placed at the END of this verb's front-door validation" comment on `check_token(video_codec)`** (`R/ffmpeg.R:1963`, `:3970`) is further from true after T8: four guards plus the duplication refusal now follow the guard that comment says is last. The comment's own point is that this precedence must not drift unremarked. Verified. Disposition: to fix with A1.

**F4–F11 re-confirmed present and unchanged by T8**, each carried forward to the re-review gate for triage exactly as round 1 dispositioned them: F4 (`check_audio_codec_needs_reencode(NA, "aac")` still a bare `simpleError` at `R/ffmpeg.R:2846`), F5 (Scope Out misdescribes the domain filter), F6 (positional probe in the sweep), F7 (12 silent-pass cells), F8 (carrier-vocabulary self-reference), F9 (`reject_duplicate_inputs()` hardcodes `jobs$input`), F10 (`check_bool(reencode)` default `arg`), F11 (double `file.access()` stat).

The [O] lens also recorded what it found sound: T8's own claims all verify at the code level; nothing between the derived-output block's old and new sites reads `jobs$output`, so the move is safe; `check_dim()`'s `!anyNA(x)` over-refuses nothing; the per-carrier filter handles an absent carrier column; and the `test-codec-arg-front-door.R` cells that moved were replaced rather than deleted. It could not verify the work log's "662 cells over 16 crossings" or "8 cells redden at `14b0cd6`", since re-running the grid at two refs needs tree mutation a shared-worktree reviewer is barred from.

### Gate outcome

**Returned to `in-progress` on A1.** A1 demonstrates AC6 failing inside the domain of the procedure AC6 names: the NEWS.md ordering paragraph is still not corrected to match the shipped guard order — round 1 it understated the change, round 2 it overstates it, and the overstatement is falsified on `standardize_video_batch()`, one of the two verbs the fix touched. A2, A3, A5 and A7 are the same overshoot written into DECISIONS, the baseline grid's rationale, a stale enumeration and a stale comment, and are fixed with it. A4, A6 and F4–F11 carry to the re-review gate for triage; none falsifies a criterion.

This is **defect return 2** for M080.

**Thrash rule, trigger (b) fires.** AC6 has now failed twice, each by a new mechanism of the same shape — the NEWS ordering paragraph not describing the shipped order. Its remedy is to reconsider the alternative the plan gate recorded against: the 2026-08-28 gate line "chose reordering the guards over amending NEWS.md's twenty-rows claim". That alternative is unspent. Trigger (a) has NOT fired — a third return would reach it, at which point descope-or-park becomes the recommended disposition.

---

## Re-review (round 3, 2026-08-28)

PR: https://github.com/jmgirard/tidymedia/pull/84 (draft). Branch synced: `git fetch` then `git rev-list --count HEAD..origin/master` = 0 — `origin/master` still at `7a6f634`, unmoved since the branch was cut, so no merge was owed; local `master` has no unpushed commits and the branch has none unpushed. Evidence below is off tip `ea72905`.

### Acceptance-criterion evidence (fresh, this round)

- **AC1 — verified.** `devtools::load_all()`, then `check_dim(v, arg = "width")` on `NA`, `NA_integer_`, `NA_real_`, `NA_character_`: all four signal classes `rlang_error, error, condition`, message `` `width` must be a single FFmpeg expression or number. `` — the `arg` named every time. `crop_video(f, o, NA_character_, 100, run = FALSE)` aborts with that same condition rather than returning the `crop=w=NA:h=100:...` filter string.
- **AC2 — verified.** The `tm_call_graph()` walk over the exports returns 17 verbs reaching `check_dim()`; `check_dim_specs()` declares an entry for all 17 (`setdiff` empty both ways) and 52 carriers = 208 carrier x NA-type cells, with `format_for_web`/`format_for_web_batch` the two positive zero-carrier declarations. `test-na-value-guards.R` runs green.
- **AC3 — verified.** `na_sweep_predicates()` re-derived at review from `ls(asNamespace("tidymedia"), all.names = TRUE, pattern = "^check_")` filtered to exactly one required formal not named `jobs`: the same 15 names as rounds 1 and 2, including all four the criterion calls out. The sweep over 60 predicate x NA-type cells is green — no bare `simpleError`, no warning.
- **AC4 — verified.** `picture_in_picture_batch()` measured on all six cells. Absent path: overlay-only → `` `jobs$overlay` names 1 file that can't be found or read. ``; main-only → `` `jobs$main` names 1 file … ``; both → `` `jobs$main` and `jobs$overlay` name 1 file … ``. Repeated with `tm_unreadable_path(tempdir())` (fixture verified `file.access(p, 4) == -1`): the identical three messages, so the filter is over D041's readability predicate, not over existence.
- **AC5 — verified.** `tm_reaches(tm_call_graph(), v, "reject_duplicate_inputs")` returns 3 verbs — `anonymize_video_batch`, `normalize_audio_batch`, `standardize_video_batch`. For each, a no-`output` table whose two rows name the same absent input reports `` `jobs$input` names 1 file that can't be found or read. `` with no mention of duplication; the readable-path control still reports `` `jobs` has duplicated input paths but no output column. `` `tm_namespace_bodies()` finds the string `has duplicated` at exactly one name, `reject_duplicate_inputs`.
- **AC6 — verified.** `devtools::check()`: `Status: OK`, 0 errors / 0 warnings / 0 notes, 2m 33s. `devtools::test()`: `FAIL 0 | WARN 12 | SKIP 5 | PASS 7967`. NEWS.md carries the three bug-fix entries the criterion names — the NA refusal (quoting the `crop=w=NA:h=100:x=(in_w-out_w)/2:y=(in_h-out_h)/2` command it used to compile, and listing the verbs and builders covered), the per-carrier naming with both wordings, and the new guard order — with no milestone numbers. **The clause that failed in rounds 1 and 2 is now met: the existing ordering paragraph is corrected to match the shipped order.** Every claim it makes was measured this round, not read:
  - *"Malformed table shapes and wrong column types still report before it."* Verified over all 15 exports reaching `check_batch_inputs()` by deparsing each body and locating every shape guard (`check_batch_jobs`, `check_fanin_jobs`) and every `check_batch_*_col()` call relative to the sweep. Two verbs place a `check_batch_*_col()` call below it — `picture_in_picture_batch()` and `compare_videos_batch()` — and in both it is `check_batch_vocab_col()`, a value guard; the same column's type guard (`check_batch_string_col()`) is above. Confirmed behaviourally: a numeric `position` column reports `The position column of `jobs` must be character (no NA).` ahead of the sweep, while a `"nope"` `position` column reports the sweep and only names `position` on a readable path. Same split on `compare_videos_batch()`'s `direction`. The claim holds as written.
  - *"`standardize_video_batch()` reports a bad `video_codec` before the sweep and a bad `width` after it."* Reproduced with controls: `video_codec = 123` on an absent input reports `` `video_codec` must be a single string or `NULL`, not the number 123. ``; `width = NA_real_` on an absent input reports `` `jobs$input` names 1 file that can't be found or read. `` and the readable-path control reports `` `width` must be a single FFmpeg expression or number. ``
  - *"Where a verb's checks on its own arguments fall relative to the sweep is not uniform and is not a promise."* The paragraph promises no ordering here, which is the correct scope — this is what rounds 1 and 2 got wrong in opposite directions.
  - *"The refusal of duplicated inputs on a verb deriving its own output names reports after it."* AC5's evidence above, on all three verbs.
  - Third entry's added sentence, *"the `video_codec` and `audio_stream` arguments are checked above the missing-file sweep; the duplication refusal now sits below it"*: all four cells reproduced — on both `anonymize_video_batch()` and `standardize_video_batch()`, a duplicated readable table with `video_codec = 123` or `audio_stream = 1.5` reports the argument. `normalize_audio_batch()` is correctly excluded from that sentence.

### Consistency gate

- `python3 cairn_validate.py` — exit 0; all 16 checks PASS, all 7 advisories OK (`release window` did not fire).
- No `DESIGN.md` principle changed on this branch, so `cairn_impact.py --changed` does not apply.
- Toolchain checks (`r-package` profile `consistency-gate` slot): `devtools::document()` leaves the tree clean; `NAMESPACE`, `man/`, `data/` and `README*` are untouched by the diff; `pkgdown::check_pkgdown()` reports "No problems found"; NEWS.md carries the user-visible changes with no milestone numbers (`grep -cE '^\+.*\bM[0-9]{2,3}\b'` on the NEWS diff = 0); no new top-level file, so no `.Rbuildignore` entry is owed; `devtools::check()` clean as recorded under AC6.

### Independent review — three lenses, fresh context

Surface tier is user-facing and the diff touches `R/`, so the full three-lens fan-out ran. Every reported finding is logged with its disposition; each was verified at review against the implementation, not against the reporter's account of it.

**[S] prior-review-record lens — no findings.** Probe `gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1` returned `[]`, so no thread walk was owed. It read the archived `## Review` sections touching these files and the M62/M63/M64 finding row, and confirmed rounds 1 and 2's findings (F1–F3, A1–A3, A5, A7) are each corrected as the reviews specified, with none reintroduced and none of the row's nine instrument findings contradicted.

**[O] diff-bug lens — 8 findings, of which 3 are new this round; [S] blame-history lens — 1 finding.** Ranked as reported, most severe first. **The [O] lens's own verdict on the returned clause: the NEWS ordering paragraph and the third bug-fix entry verify true against the shipped package, and it could not falsify any claim either makes.** The residual overshoot has migrated out of NEWS and into `cairn/DECISIONS.md` D058.

- **N1 — D058's rule is falsified by this milestone's own change, and its declared falsifier fires on a shipped call.** D058 now reads "the input sweep is never lifted past a check that already sat above it". That describes `anonymize_video_batch()` and `standardize_video_batch()` but not `normalize_audio_batch()`, where T5 did the opposite: on `origin/master` the auto-name/duplication block sat at deparsed body line 67 and `check_batch_inputs(jobs)` at line 85, and the sweep was lifted past it. D058's falsifier — "a report preferring the missing path over a check that sat above the sweep before the move that displaced it" — is `normalize_audio_batch(tibble(input = c("gone.mp4", "gone.mp4")), run = FALSE)`, which reports `` `jobs$input` names 1 file that can't be found or read. `` on HEAD where `master` reported the duplication. Verified at review on both refs. The behaviour is exactly what D057 asks for, so this is a wording defect and not a code defect — but it is the same failure shape that returned this milestone twice, now one file over. Needs a carve-out for the guard D057 relocates. Disposition: **fix now**.
- **N2 — D058's "measured" per-argument table is wrong on `picture_in_picture_batch()`'s `scale`.** D058 says the verb "checks `scale` and its codec tokens above the sweep but `margin` and `position` below it". Only `scale`'s type check is above: `picture_in_picture_batch(..., scale = "x")` on absent paths reports `` `scale` must be a number, not the string "x". ``, but `scale = 5` on the same table reports `` `jobs$main` and `jobs$overlay` name 2 files … `` and only gives `` `scale` must be greater than 0 and at most 1. `` on the readable-path control — `check_overlay_scale()` sits below the sweep. D058 draws "what sits above the sweep is per argument" from this table; the split is finer still, per check. Verified at review. Disposition: **fix now** (same edit).
- **N3 — D058 credits M42 for a pin M41 laid.** ([S] blame lens, independently.) D058 reads "That inverts a precedence M42 pinned". The codec-versus-duplication cases in `test-codec-arg-front-door.R` were added by `0a73edb8` "Front-door validation parity for the codec arguments (#43)", and `cairn/milestones/archive/M41-codec-arg-front-door-guards.md:3` records M41 as done via PR #43; M42 is the `NULL`/column-`NA` codec-semantics milestone. This branch's own rewritten code comments get it right ("when M41 placed it here"), so it is a slip in a permanent record rather than a reattribution. Verified at review. Disposition: **fix now** (same edit).
- **N4 — D058's uniformity claim names a function family wider than the invariant.** "no column-type guard (`check_batch_*_col()`) sits below the sweep in any verb": `check_batch_vocab_col()` matches that glob and sits below the sweep in `picture_in_picture_batch()` (body line 46) and `compare_videos_batch()` (line 34). The semantic claim survives — AC6's evidence above probes genuine wrong-type columns on every candidate and all report before the sweep — but the parenthetical is falsifiable as written. Verified at review. Disposition: **fix now** (same edit).
- **N5 — residual A3-shaped overstatement in the `scalar_arg` crossing's inline comment.** `data-raw/input-guard-baseline.R:323` still reads "The scalar argument the same verbs check last, before the sweep." The top-of-file rationale was correctly narrowed at T9; this second copy was not. It is not last in 3 of the 9 gated verbs: `reject_duplicate_outputs()` follows it in `crop_video_batch()` (22 → 26, sweep 27) and `format_for_web_batch()` (5 → 9, sweep 10), and `check_bool(reencode)` follows it in `segment_video_batch()` (30 → 31, sweep 35). Verified at review. Disposition: **fix now**.
- **N6 (= A4) — the two new crossings' verb membership is a fixed-string grep and degrades silently to zero cells.** `data-raw/input-guard-baseline.R:648,658`. Unchanged from round 2. Disposition: for triage at this gate.
- **N7 (= A6) — dead branch in `input_guard_error_crossing()`.** `data-raw/input-guard-baseline.R:387`; the only crossing supplying a `video_codec` supplies the valid string `"copy"`, so that half can never fire. Unchanged from round 2. Disposition: for triage at this gate.
- **N8 (= F4–F11) — the eight instrument and reachability findings from round 1, all re-confirmed present and unchanged.** F4 (`check_audio_codec_needs_reencode(NA, "aac")` still signals a bare `simpleError`, `missing value where TRUE/FALSE needed` — re-verified at review, while its twin got `rlang::check_bool()` in this diff), F5 (Scope Out misdescribes the domain filter), F6 (positional probe in the sweep), F7 (12 of 60 sweep cells pass silently), F8 (carrier vocabulary is self-referential), F9 (`reject_duplicate_inputs()` hardcodes `jobs$input`), F10 (`check_bool(reencode)` keeps the default `arg`), F11 (double `file.access()` stat). Disposition: for triage at this gate.

The lenses also recorded what they found sound: the first bug-fix entry's closing sentence holds (`check_regions()`'s own NA guard catches region-frame NAs before `check_dim()`, unchanged by this diff); A5's and A7's fixes are correct, and the third `END of this verb's front-door validation` comment at `normalize_audio_batch()`'s `audio_codec` was rightly left alone because nothing but the fan-out follows it there; `check_batch_inputs()`'s edge cases (empty carrier, absent carrier column, `NA_character_` path, list-columns) are unchanged and its new `invisible(jobs)` return is used as a statement at every call site; the new below-the-sweep pin carries a readable-path control on every cell, so it records an order rather than a guard that never fires; no assertthat was added and no roxygen changed. The [O] lens could not re-run the baseline grid at two refs, which needs tree mutation a shared-worktree reviewer is barred from.

### Gate outcome

**No return.** AC1–AC6 all verified with fresh evidence this round; the consistency gate passed in both halves. Applying the return floor: N1–N5 are defects in prose describing the change, not in the change — the shipped behaviour every one of them touches was measured correct at review — so none demonstrates an acceptance criterion failing inside the domain of the procedure it names (AC6's domain is NEWS.md, which verifies), and none is a load-bearing defect in what the package does for its users. N6–N8 are instrument and reachability findings that falsify no criterion. **Trigger (a) therefore does not fire: the defect-return count stands at 2.**

- 2026-08-28: re-review round 3 — AC1-AC6 all verified with fresh evidence off `ea72905`; consistency gate green in both halves; `devtools::check()` OK 0/0/0 and `devtools::test()` 0 failures / 7967 passing. The twice-failed AC6 clause is met: every claim the NEWS ordering paragraph and the third bug-fix entry make was measured against the package this round and holds. Nine findings logged (N1-N8 plus the prior-review lens's clean no-op); none falsifies a criterion and none is a floor return. Defect-return count stands at 2.

### Fix-now work directed at this gate (2026-08-28)

The maintainer triaged N1-N5 fix-now and N6-N8 to follow-up. Applied on the branch before the approval marker:

- **N1** — D058's rule gains an explicit carve-out for the derived-output duplication refusal, the one guard D057 puts below the sweep, with `normalize_audio_batch()`'s own case stated (on `origin/master` nothing but the derived-output block sat between its scalar checks and its sweep, verified by reading the master body, so M080 lifted the sweep past that block and nothing else). The falsifier is re-cut to exclude the carved-out guard both ways.
- **N2** — the `scale` cell is rewritten to record the split it actually has: `scale = "x"` refused above the sweep by the type check, `scale = 5` below it by `check_overlay_scale()`'s range check. The table's conclusion changes from "per argument" to "per CHECK — not per verb, not per category, and not even reliably per argument". Every other cell of that table was re-measured this round against a readable-path control before the edit, so the correction introduced no new claim: `picture_in_picture_batch()` codec tokens above / `margin`, `position` below; `compare_videos_batch()` `resize` and codec tokens above / `direction` below; `standardize_video_batch()` `video_codec`, `audio_stream` above / `width` below; `normalize_audio_batch()` `two_pass` above / `target_loudness` below.
- **N3** — the citation is corrected to M41 (PR #43, commit `0a73edb8`), naming the wrong attribution it replaces.
- **N4** — the uniformity claim names the three column-TYPE guards individually (`check_batch_audio_col()`, `check_batch_codec_col()`, `check_batch_string_col()`) instead of the `check_batch_*_col()` glob, and records why: the glob also catches `check_batch_vocab_col()`, a column VALUE guard that does sit below the sweep in two verbs.
- **N5** — `data-raw/input-guard-baseline.R`'s inline `scalar_arg` comment no longer claims the argument is checked last; it names the three verbs where a further check follows it and defers to the crossing's top-of-file entry.

D058's header records the third narrowing. Re-verified after the edits: `cairn_validate.py` exit 0, 16 PASS / 7 advisories OK; `data-raw/input-guard-baseline.R` parses; `devtools::check()` Status OK, 0 errors / 0 warnings / 0 notes, 2m 34s. The fixes are prose-only — no `R/` file changed — so the AC1-AC6 evidence above stands unaffected.

- 2026-08-28: gate triage — N1-N5 fixed on the branch (D058's carve-out, the `scale` cell, the M41 citation, the column-type guard names, the baseline grid's `scalar_arg` comment); N6, N7 and F4-F11 dispositioned to follow-up on the M62/M63/M64 finding row. No `R/` change, `check()` re-run clean.
