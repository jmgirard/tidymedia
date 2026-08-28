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
- AC5 → T1, T5, T6
- AC6 → T7

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

## Decisions

## Review

PR: https://github.com/jmgirard/tidymedia/pull/84 (draft, opened 2026-08-28)

### Acceptance-criterion evidence

- **AC1 — verified.** `devtools::load_all()`, then `check_dim(v, arg = "width")` on each of `NA`, `NA_integer_`, `NA_real_`, `NA_character_`: all four signal a condition whose classes are `rlang_error, error, condition` and whose message is `` `width` must be a single FFmpeg expression or number. `` — the `arg` named in every case. `crop_video(f, o, NA_character_, 100, run = FALSE)` aborts with that same condition rather than returning the `crop=w=NA:h=100:...` filter string.

- **AC2 — verified.** `tm_reaches(tm_call_graph(), v, "check_dim")` over the exported verbs returns 17: `anonymize_video`, `anonymize_video_batch`, `crop_video`, `crop_video_batch`, `ffm_crop`, `ffm_drawbox`, `ffm_fps`, `ffm_overlay`, `ffm_scale`, `format_for_web`, `format_for_web_batch`, `picture_in_picture`, `picture_in_picture_batch`, `sample_frames`, `sample_frames_batch`, `standardize_video`, `standardize_video_batch`. Every one has a declared entry in `helper-na-guards.R` (`setdiff(verbs, names(specs))` empty), and the entries declare 52 carriers — 208 carrier x NA-type cells. Two of the 17, `format_for_web()` and `format_for_web_batch()`, declare zero: they reach `check_dim()` only with package-fixed dimensions and expose no caller-supplied carrier (no formal and no `jobs`-column literal in the vocabulary), which the completeness reader independently confirms. (The T3 work-log line records 44 carriers; the figure measured here at review is 52.) `test-na-value-guards.R`'s sweep runs all 208 and passes: each aborts with an `rlang_error`, blamed on the verb the caller typed (`conditionCall` matches `<verb>(`), naming the carrier as `` `arg` `` or `<arg> column`, in one of the five listed wordings. The type-guard control fires on every cell answered with a type wording alone and asserts message equality against the same call carrying a non-NA value of that type. The completeness reader — formals and `jobs`-column literals against the declared vocabulary — is green for all 17.

- **AC3 — verified.** `na_sweep_predicates()` enumerates the domain from `ls(asNamespace("tidymedia"), all.names = TRUE, pattern = "^check_")` filtered to exactly one required formal not named `jobs`: 15 names on 2026-08-28 — `check_audio_codec_not_copy`, `check_codec_needs_reencode`, `check_copy_map_conflict`, `check_dim`, `check_ffm`, `check_file_exists`, `check_file_readable`, `check_hardware_needs_encode`, `check_image_format`, `check_nvenc_available`, `check_overlay_scale`, `check_paths_readable`, `check_region_values`, `check_regions`, `check_token`. All four names the criterion calls out are in it. Run over 60 predicate x NA-type cells: 0 errors that are not `rlang_error`, 0 warnings. The four that reddened at `master` (`check_dim` on `NA_real_`, `check_overlay_scale` and `check_region_values` on all four, `check_codec_needs_reencode` on `NA_character_`) are green here; commit `f427f4f` holds their red form.

- **AC4 — verified.** `picture_in_picture_batch()` measured on all six cells. Absent path: overlay bad alone reports `` `jobs$overlay` names 1 file that can't be found or read. ``, main bad alone reports `jobs$main` alone, both bad reports `` `jobs$main` and `jobs$overlay` name 1 file … ``. Repeated with the mode-000 fixture `tm_unreadable_path()` builds in `helper-input-paths.R` (verified `file.access(p, 4) != 0`): the same three messages, so the per-carrier filter is over D041's readability predicate and not over existence.

- **AC5 — verified.** `tm_reaches(tm_call_graph(), v, "reject_duplicate_inputs")` returns 3 verbs: `anonymize_video_batch`, `normalize_audio_batch`, `standardize_video_batch`. For each, a `jobs` table with no `output` column whose two rows name the same absent input reports `` `jobs$input` names 1 file that can't be found or read. `` and the word "duplicated" does not appear. The control — the same table with a readable duplicated path — still reports `` `jobs` has duplicated input paths but no output column. ``, so the path report did not displace the duplication report. `tm_namespace_bodies()` finds the string `has duplicated` at exactly one name, `reject_duplicate_inputs`.

- **AC6 — verified.** `devtools::check()` on the branch: `Status: OK`, 0 errors / 0 warnings / 0 notes, 2m 42s. `NEWS.md`'s Bug fixes section carries three new entries — the NA refusal (quoting the `crop=w=NA:h=100:x=(in_w-out_w)/2:y=(in_h-out_h)/2` command it used to compile and listing the verbs and builders the refusal covers), the per-carrier naming with both the one-carrier and both-carriers wordings, and the new guard order for the three deriving verbs. The existing ordering paragraph gained the clause the criterion names: the duplicated-input refusal on a verb deriving its own output names now reports after the path sweep. No milestone numbers appear in any of it.

### Consistency gate

- `python3 cairn_validate.py` — exit 0; all 16 checks PASS, all 7 advisories OK (`release window` did not fire).
- No `DESIGN.md` principle changed on this branch, so `cairn_impact.py --changed` does not apply.
- Toolchain checks (`r-package` profile `consistency-gate` slot): `devtools::document()` produces no diff (working tree clean apart from this milestone file); `NAMESPACE`, `man/` and `data/` are untouched by the diff, so no generated file was hand-edited; `README.Rmd`/`README.md` are untouched and in sync; `pkgdown::check_pkgdown()` reports "No problems found" and the branch exports nothing new, so no `_pkgdown.yml` row is owed; `NEWS.md` carries this milestone's user-visible changes with no milestone numbers; no new top-level file, so no `.Rbuildignore` entry is owed; `devtools::check()` clean as recorded under AC6.
- `devtools::test()` on the branch: 0 failures, 7934 passing, 5 skips (absent hardware encoders), 12 warnings.

