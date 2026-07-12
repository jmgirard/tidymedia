<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M19: Consistent schema for an all-silent two-pass batch

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** M18   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Principles touched:** —   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Branch/PR:** m19-all-silent-schema-consistency · https://github.com/jmgirard/tidymedia/pull/21   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Make `normalize_audios(two_pass = TRUE)` return the same opt-in schema (the
`verify=` `verified` column and the `manifest=` provenance manifest) when
*every* row is silent as it does for a mixed batch, closing the D011 schema
inconsistency the M18 review surfaced.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:**
- When every row is silent under `two_pass = TRUE, run = TRUE`, the all-silent
  branch of [`bind_two_pass_result()`](../../R/loudnorm_two_pass.R) (the
  `ok_res` is `NULL` path) synthesizes the same opt-in outputs a mixed batch
  produces:
  - a logical `verified` column of all `NA` when `verify=` was supplied;
  - a padded provenance manifest attribute when `manifest=` was supplied —
    one row per job, `input` paths recorded, every other column type-matched
    `NA` (identical to how `expand_manifest_rows()` pads a mixed batch's
    silent rows), with `input_md5`/`output_md5` columns present iff
    `checksums = TRUE`.
- Thread the `verify`/`manifest`/`checksums` intent from `normalize_audios()`'s
  two-pass block into `bind_two_pass_result()`.
- A shared manifest column-schema constructor so the synthesized empty manifest
  cannot drift from `build_manifest()`.
- Document the schema-consistency guarantee in `normalize_audios()`'s `@return`.

**Out:**
- Any change to single-pass, scalar (`normalize_audio()`), or mixed-batch
  behavior — those stay byte- and schema-for-schema unchanged (guarded by the
  existing characterization tests).
- The all-silent case under `run = FALSE` — a mixed batch adds no opt-in
  columns there either (they live inside `ffm_batch()`'s `if (run)`), so the
  all-silent path is already consistent; this milestone only touches
  `run = TRUE`.
- Generalizing the fix beyond the two known opt-ins (`verify`, `manifest`);
  `command`/`success` handling is unchanged.
- CRAN readiness → its ROADMAP candidate row.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [x] AC1 — Under `two_pass = TRUE, run = TRUE, verify = <spec>` with every row
      silent, the result carries a logical `verified` column that is all `NA`,
      and its full column set equals that of a mixed silent/non-silent batch
      run with the same `verify=`.
- [x] AC2 — Under `two_pass = TRUE, run = TRUE, manifest = TRUE` with every row
      silent, `ffm_manifest(result)` returns a one-row-per-job tibble whose
      column names/types match a mixed batch's manifest: `input` holds each
      job's input path, every other column is `NA`. With `checksums = TRUE` the
      `input_md5`/`output_md5` columns are present (and `NA`).
- [x] AC3 — The synthesized all-silent manifest's column names and types are
      identical to `build_manifest()`'s output for the same `checksums` flag
      (drift guard), because both derive from one shared schema constructor.
- [x] AC4 — Single-pass, scalar, and mixed-batch results are unchanged: the
      existing `normalize_audios`/`normalize_audio`/two-pass characterization
      and M18 mixed-batch tests still pass unmodified in behavior.
- [x] AC5 — `normalize_audios()`'s `@return` states that the two-pass batch
      returns the same columns and (when requested) manifest regardless of how
      many rows are silent; `man/` regenerated via `devtools::document()`.
- [x] AC6 — `devtools::check()` is clean (0 errors / 0 warnings / 0 notes) and
      `devtools::test()` passes.

## Coverage
<!-- owner: plan · create/amend-via-gate; each acceptance criterion → the
     task(s) satisfying it, by positional number (AC/Task counted
     top-to-bottom). Review reads to fence evidence — tracking-rules "AC fencing". -->

- AC1 → T2, T3, T5
- AC2 → T1, T2, T5
- AC3 → T1, T5
- AC4 → T5
- AC5 → T4
- AC6 → T5

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits); substantive
     change is amend-via-gate -->

- [x] T1 — In [R/ffm_manifest.R](../../R/ffm_manifest.R:52) add a shared
      manifest schema constructor (e.g. `manifest_schema(checksums)`) returning
      a 0-row tibble with the canonical columns (`command`, `input`, `output`,
      `output_size`, `ffmpeg_version`, `ffprobe_version`, `timestamp`, plus
      `input_md5`/`output_md5` when `checksums`); refactor `build_manifest()`
      so its column set derives from the same source (no drift).
- [x] T2 — Extend [`bind_two_pass_result()`](../../R/loudnorm_two_pass.R:245)
      to take the `verify`/`manifest`/`checksums` intent and, in the all-silent
      (`ok_res` `NULL`) branch under `run = TRUE`, add an all-`NA` logical
      `verified` column (when verify requested) and attach a padded manifest
      via `expand_manifest_rows()` over the empty schema from T1 (when manifest
      requested).
- [x] T3 — Thread `verify`/`manifest`/`checksums` presence from the two-pass
      block of `normalize_audios()`
      ([R/ffmpeg.R:1460](../../R/ffmpeg.R)) into the
      `bind_two_pass_result()` call (extract from `...`); update the existing
      M18 `bind_two_pass_result()` unit tests for the new signature.
- [x] T4 — Document the schema-consistency guarantee in `normalize_audios()`'s
      `@return` ([R/ffmpeg.R:1316](../../R/ffmpeg.R)); run
      `devtools::document()`.
- [x] T5 — Tests: pure unit tests of the all-silent synthesized schema
      (verify / manifest / manifest+checksums flags, no binary) in
      `tests/testthat/test-normalize-audios-two-pass.R`; an ffmpeg-gated
      execution test of a real all-silent batch with `verify=` and
      `manifest=` in `tests/testthat/test-normalize-audios.R`, mirroring the
      M18 mixed-batch test (`make_silent_audio()` helper). Update the spelling
      wordlist if new terms appear (M17 lesson); confirm `Status: OK`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan (promoted from the M18-review candidate, scored 78).
- 2026-07-12: T1 — added `manifest_schema()` as the canonical column template; `build_manifest()` now derives its column set from it (drift guard test passes).
- 2026-07-12: T2/T3 — all-silent branch of `bind_two_pass_result()` now synthesizes the `verified` column and padded manifest (via `expand_manifest_rows()` over the empty schema); `normalize_audios()` threads verify/manifest/checksums intent from `...`. Existing M18 unit tests unchanged (new params default off); 5 new pure unit tests pass.
- 2026-07-12: T4 — documented the schema-consistency guarantee in `normalize_audios()`'s `@return`; regenerated `man/normalize_audios.Rd`.
- 2026-07-12: T5 — added an ffmpeg-gated execution test comparing an all-silent batch's columns + manifest schema against a mixed batch (verify + manifest + checksums). Full test files green (no skips locally).
- 2026-07-12: all tasks complete; `devtools::check()` 0/0/0, raw `00check.log` `Status: OK`. Status → review.

## Decisions
<!-- owner: implement / review · append-only; milestone-local; promote
     cross-cutting ones to cairn/DECISIONS.md -->

## Review
<!-- owner: review · exclusive; evidence per criterion; consistency-gate
     results; independent-review findings and their triage -->

Reviewed 2026-07-12 on branch `m19-all-silent-schema-consistency` (PR #21).

### Acceptance-criterion evidence (fresh)

- **AC1** ✓ — `test-normalize-audios-two-pass.R` "adds an all-NA verified column
  for an all-silent verify batch" (unit) asserts `verified` is logical + all NA,
  ordered after `success`; `test-normalize-audios.R` "keeps the verify/manifest
  schema when every row is silent" (ffmpeg-gated) asserts `names(res)` equals a
  mixed batch's `names(ref)`. Both pass.
- **AC2** ✓ — unit tests "attaches a padded manifest…" and "manifest carries md5
  columns under checksums" plus the execution test's manifest assertions:
  one-row-per-job, `input` = each job's path, other columns NA, `input_md5`/
  `output_md5` present under `checksums=TRUE`, manifest column set == mixed
  batch's. Pass.
- **AC3** ✓ — `test-ffm-manifest.R` "manifest_schema() is the empty template
  build_manifest() fills (drift guard)" asserts identical names + column classes
  for `checksums` FALSE and TRUE. Pass.
- **AC4** ✓ — the M18 mixed-batch tests and single-pass characterization tests
  are unchanged and pass; full suite green (see AC6). New `bind_two_pass_result`
  params default off, so existing call sites are behavior-identical.
- **AC5** ✓ — `normalize_audios()` `@return` now states the two-pass schema is
  independent of the silent-row count; `man/normalize_audios.Rd` regenerated;
  `devtools::document()` produces no further diff.
- **AC6** ✓ — `devtools::check()` = 0 errors / 0 warnings / 0 notes; raw
  `00check.log` `Status: OK` (M17-lesson double-check). Targeted files:
  `test-ffm-manifest.R` 30 pass, `test-normalize-audios-two-pass.R` 59 pass,
  `test-normalize-audios.R` 89 pass — 0 fail / 0 skip.

### Consistency gate

- `cairn_validate.py` → exit 0, all checks pass (incl. coverage complete,
  principles slot valid).
- `devtools::document()` → no diff. `pkgdown::check_pkgdown()` → no problems.
- No `DESIGN.md` principle changed (`Principles touched: —`) → impact report skipped.
- NEWS.md → entry added under the development version's silent-input section
  (no milestone numbers in user-facing text).
- No new top-level files → no `.Rbuildignore` change.

### Independent review

_Pending — two fresh-context reviewers (diff-bug [O], blame-history [S]) running;
scorer + triage to follow._
