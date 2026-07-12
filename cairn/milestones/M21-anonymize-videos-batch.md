<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M21: Batch fixed-region anonymization verb

- **Status:** planned
- **Priority:** normal
- **Depends on:** M20
- **Principles touched:** IP1, IP2
- **Branch/PR:** —

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

- [ ] `anonymize_videos()` is exported (NAMESPACE + `_pkgdown.yml` Layer-2 row)
      with roxygen docs and a runnable `run = FALSE` example.
- [ ] A jobs data frame of N inputs compiles to N single-output commands, each
      with its own regions box-filled (per-input regions resolved from the
      jobs table — see open question below). (compilation test)
- [ ] Front-door validation aborts via `cli::cli_abort()` on: `jobs` not a data
      frame, zero rows, and missing `input` column — parity with
      `standardize_videos()`. (tests)
- [ ] Per-row region validation is inherited from `anonymize_pipeline()` (a bad
      region in any job is reported by row index, not re-implemented in the
      front door). (test)
- [ ] Return schema (`names()` and types) matches the scalar/`ffm_batch` path,
      including opt-in `verified` column and manifest attribute when requested.
      (parity test, M19 lesson)
- [ ] Executes end-to-end on a small multi-input jobs table built from
      `sample.mp4` (`skip_if` ffmpeg absent). (execution test)
- [ ] `devtools::check()` is clean: 0 errors / 0 warnings / 0 notes.

## Coverage

- AC1 → T2, T5
- AC2 → T1, T2, T3
- AC3 → T2, T3
- AC4 → T1, T3
- AC5 → T2, T3
- AC6 → T4
- AC7 → T5

## Tasks

- [ ] T1: Resolve the per-input regions interface (open question — decide at the
      implement gate): a list-column of region data frames on `jobs` is the
      likely tidy answer; confirm against the `ffm_batch` pmap-by-name contract
      (D007) before coding.
- [ ] T2: Front door `anonymize_videos(jobs, …)` over `ffm_batch`, reusing M20's
      `anonymize_pipeline()`; jobs-table column type/NA guards only (mirror
      `standardize_videos()` R/ffmpeg.R:1143); output-path derivation for jobs
      lacking `output`. Roxygen `@family task verb functions`; `_pkgdown.yml`
      row; `devtools::document()`.
- [ ] T3: Compilation + validation tests: N-input fan-out; per-input distinct
      regions; front-door abort branches; inherited per-region abort by index;
      schema-parity test vs the scalar path (M19 trick).
- [ ] T4: Execution test (`skip_if` ffmpeg absent): multi-input jobs table from
      `sample.mp4`; assert each output exists and is valid.
- [ ] T5: `devtools::document()`, `devtools::test()`, `devtools::check()` to OK
      (0/0/0); wordlist if needed; confirm `Status: OK` in `00check.log`.

## Work log

- 2026-07-12: created by /milestone-plan alongside M20 (batch sibling split off
  per the M14→M15 precedent; depends on M20's shared pipeline).

## Decisions

## Review
