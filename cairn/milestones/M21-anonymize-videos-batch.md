<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M21: Batch fixed-region anonymization verb

- **Status:** review
- **Priority:** normal
- **Depends on:** M20
- **Principles touched:** IP1, IP2
- **Branch/PR:** m21-anonymize-videos-batch · https://github.com/jmgirard/tidymedia/pull/23

## Goal

Add the batch sibling `anonymize_videos(jobs, …)` that box-fills each of many
videos over an `ffm_batch` runner, one reproducible command per input.

## Scope

**In:** `anonymize_videos(jobs, color = "black", vcodec = "libx264",
pixel_format = "yuv420p", run = TRUE, parallel = FALSE, …)` fanning out over a
jobs data frame (one row per input, `input` column required; `output` derived
when absent, mirroring `standardize_videos()` R/ffmpeg.R:1143). Each job reuses
M20's `anonymize_pipeline()`, inheriting per-region validation for free (M13
lesson). Front-door guards cover only jobs-table column type/NA checks. Return
schema matches the normal `ffm_batch` path including opt-in `verified`/manifest
outputs (M19 parity lesson).

**Out:**
- Anything M20 already ships (the scalar verb, the shared pipeline, box-fill
  semantics) — this is the batch fan-out only.
- Region blur → still the candidate row; unaffected by this milestone.

## Acceptance criteria

- [x] `anonymize_videos()` is exported (NAMESPACE + `_pkgdown.yml` Layer-2 row)
      with roxygen docs and a runnable `run = FALSE` example.
- [x] A jobs data frame of N inputs compiles to N single-output commands, each
      with its own regions box-filled (per-input regions resolved from the
      jobs table — see open question below). (compilation test)
- [x] Front-door validation aborts via `cli::cli_abort()` on: `jobs` not a data
      frame, zero rows, and missing `input` column — parity with
      `standardize_videos()`. (tests)
- [x] Per-row region validation is inherited from `anonymize_pipeline()` (a bad
      region in any job is reported by row index, not re-implemented in the
      front door). (test)
- [x] Return schema (`names()` and types) matches the scalar/`ffm_batch` path,
      including opt-in `verified` column and manifest attribute when requested.
      (parity test, M19 lesson)
- [x] Executes end-to-end on a small multi-input jobs table built from
      `sample.mp4` (`skip_if` ffmpeg absent). (execution test)
- [x] `devtools::check()` is clean: 0 errors / 0 warnings / 0 notes.

## Coverage

- AC1 → T2, T5
- AC2 → T1, T2, T3
- AC3 → T2, T3
- AC4 → T1, T3
- AC5 → T2, T3
- AC6 → T4
- AC7 → T5

## Tasks

- [x] T1: Resolve the per-input regions interface (open question — decide at the
      implement gate): a list-column of region data frames on `jobs` is the
      likely tidy answer; confirm against the `ffm_batch` pmap-by-name contract
      (D007) before coding.
- [x] T2: Front door `anonymize_videos(jobs, …)` over `ffm_batch`, reusing M20's
      `anonymize_pipeline()`; jobs-table column type/NA guards only (mirror
      `standardize_videos()` R/ffmpeg.R:1143); output-path derivation for jobs
      lacking `output`. Roxygen `@family task verb functions`; `_pkgdown.yml`
      row; `devtools::document()`.
- [x] T3: Compilation + validation tests: N-input fan-out; per-input distinct
      regions; front-door abort branches; inherited per-region abort by index;
      schema-parity test vs the scalar path (M19 trick).
- [x] T4: Execution test (`skip_if` ffmpeg absent): multi-input jobs table from
      `sample.mp4`; assert each output exists and is valid.
- [x] T5: `devtools::document()`, `devtools::test()`, `devtools::check()` to OK
      (0/0/0); wordlist if needed; confirm `Status: OK` in `00check.log`.

## Work log

- 2026-07-12: created by /milestone-plan alongside M20 (batch sibling split off
  per the M14→M15 precedent; depends on M20's shared pipeline).
- 2026-07-12: T1 resolved at implement gate — regions is a required list-column
  (pmap unwraps each cell's data frame by name; purrr adds `In index: N` so a
  bad region reports by row for free). Per-row color/vcodec/pixel_format
  override columns (user-confirmed, parity with standardize_videos()).
- 2026-07-12: T2 — anonymize_videos() front door + derive_anonymized_names();
  reuses anonymize_pipeline(); NEWS, _pkgdown.yml row, document().
- 2026-07-12: T3/T4 — test-anonymize-videos.R (26 tests): fan-out, per-row +
  per-box color, knob overrides, auto-naming/collision, front-door aborts,
  inherited per-region validation reported by `In index: N`, schema parity vs
  a direct ffm_batch call, and binary-gated execution/verify/manifest. 60 pass.
- 2026-07-12: T5 — full suite 870 pass / 0 fail; `devtools::check()` Status: OK
  (0/0/0) after `spelling::update_wordlist()` added anonymization/anonymize/
  reproducibly (M17 masked-NOTE lesson). Status → review.

## Decisions

## Review

### Acceptance-criteria evidence (2026-07-12, fresh)

- AC1 — `anonymize_videos` in NAMESPACE (`export()`) and `_pkgdown.yml` Layer-2
  row (pkgdown::check_pkgdown() ✔ no problems); roxygen `@family task verb
  functions` + runnable `run = FALSE` example in `man/anonymize_videos.Rd`.
- AC2 — test-anonymize-videos.R: "returns one command per job with its own
  regions" (2 inputs → 2 commands, each its own drawbox) and "draws every box
  for a multi-region row" pass.
- AC3 — front-door abort tests (non-data-frame, empty table, missing `input`)
  pass; messages via `cli::cli_abort`, parity with standardize_videos().
- AC4 — "inherits per-region validation, reported by row": a row-2 malformed
  regions cell aborts with "missing" **and** purrr's "index: 2"; front door
  adds no region re-check. Size check inherited from ffm_drawbox likewise.
- AC5 — "return schema matches a direct ffm_batch call": `names()` and per-col
  `class()` identical to a raw ffm_batch run; regions list-column preserved;
  verify/manifest forwarding tests confirm `verified` col + manifest attr.
- AC6 — "writes anonymized outputs (binary-gated)": multi-input jobs table runs
  end-to-end, both outputs exist, non-empty, duration preserved. Ran (not
  skipped — ffmpeg present): 23 test blocks, 0 failed, 0 skipped.
- AC7 — `devtools::check()` **Status: OK**, 0 errors / 0 warnings / 0 notes
  (raw 00check.log, M17 masked-NOTE check applied).

### Consistency gate (2026-07-12)

- cairn_validate.py exit 0 (all 12 checks pass, incl. coverage complete).
- document() → no diff; README.Rmd/DESIGN.md untouched (no principle change →
  cairn_impact skipped); pkgdown ✔; NEWS entry present; no new top-level files.
