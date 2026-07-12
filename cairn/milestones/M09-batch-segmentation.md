<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M09: Dataframe-driven batch segmentation

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Branch/PR:** m09-batch-segmentation   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Add `segment_videos()`, a Layer-2 verb that cuts segments across many input
files from one jobs tibble, sharing its cut logic with `segment_video()`.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:**
- A new exported verb `segment_videos(jobs, reencode = TRUE, run = TRUE,
  parallel = FALSE, ...)` where `jobs` is a data frame with columns
  `input`, `output`, `start`, `end` (one row per segment, spanning any number
  of input files); `...` is forwarded to `ffm_batch()` so `verify`, `manifest`,
  `checksums`, and `progress` all work.
- An internal helper (e.g. `segment_pipeline()`) holding the shared
  `ffm_seek()` + optional `ffm_copy()` cut logic, called by both
  `segment_videos()` and `segment_video()` (behaviour-preserving refactor of
  the latter's inline `.f` in `R/ffmpeg.R:492-502`).
- Column-contract validation: `jobs` is a non-empty data frame carrying the
  four required columns; a clear `cli::cli_abort()` naming any missing column.
- Roxygen docs with a runnable `run = FALSE` example; `_pkgdown.yml` reference
  row; NEWS entry.

**Out:**
- Auto-generating `output` paths when absent — required here (across many
  inputs, per-file segment numbering is a separate design) → **candidate row**.
- A per-row `reencode` column — scalar arg only for now → **candidate row**.
- Any change to the engine or to `ffm_batch()` (this is a thin Layer-2 wrapper,
  D007) — none needed.
- Renaming `segment_video()` or changing its public signature — untouched
  except for the internal `.f` extraction.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] `segment_videos()` is exported and, on a jobs tibble spanning ≥2 distinct
      input files, returns the `ffm_batch()` tibble with one `command` row per
      job; each command reflects its row's `input`/`output`/`start`/`end`
      (tested with `run = FALSE`, pure/CI-safe).
- [ ] `reencode = TRUE` (default) produces accurate output-seek commands
      (`-ss … -to …` after `-i`, no stream copy); `reencode = FALSE` produces
      the fast copy path (`-ss … -to … -i`, `-codec:v copy -codec:a copy`,
      `-avoid_negative_ts make_zero`) — parity with `segment_video()`,
      verified per-row on a table.
- [ ] Forwarded `...` reaches `ffm_batch()`: passing a `verify` spec adds a
      `verified` column and passing `manifest = TRUE` attaches a manifest
      readable by `ffm_manifest()` (binary-gated execution test).
- [ ] Input validation: a non-data-frame `jobs`, an empty `jobs`, and a `jobs`
      missing any of the four required columns each abort with a `cli` message
      naming the problem (tested for each branch).
- [ ] `segment_video()` still passes its existing test suite unchanged after
      the shared-helper refactor (behaviour preserved; no test edits required).
- [ ] Docs/index consistent: roxygen with a runnable example, a
      `_pkgdown.yml` row for `segment_videos`, and a NEWS bullet — all present
      in the same change; `devtools::document()` leaves `man/`/`NAMESPACE`
      clean.
- [ ] `devtools::check()` clean (0 errors / 0 warnings).

## Coverage
<!-- owner: plan · create/amend-via-gate; each acceptance criterion → the
     task(s) satisfying it, by positional number (AC/Task counted
     top-to-bottom). Review reads to fence evidence — tracking-rules "AC fencing". -->

- AC1 → T2, T4
- AC2 → T1, T2, T4
- AC3 → T2, T4
- AC4 → T2, T4
- AC5 → T1, T3
- AC6 → T5
- AC7 → T6

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits); substantive
     change is amend-via-gate -->

- [ ] T1: Extract the shared cut logic from `segment_video()`'s inline `.f`
      (`R/ffmpeg.R:492-502`) into an internal helper `segment_pipeline(input,
      output, start, end, reencode)`; rewrite `segment_video()` to call it.
      (tests-first: existing `segment_video()` tests must stay green.)
- [ ] T2: Write failing tests for `segment_videos()` — multi-file dry-run
      command construction, both `reencode` paths per row, and each validation
      branch (non-df / empty / missing column) — in
      `tests/testthat/test-ffmpeg.R` (or a new `test-segment-videos.R`).
- [ ] T3: Implement `segment_videos()` in `R/ffmpeg.R`: validate the four-column
      contract, build the jobs tibble, and call `ffm_batch(jobs, .f =
      segment_pipeline-wrapper, run=, parallel=, ...)`, forwarding `...`.
- [ ] T4: Add a binary-gated execution test proving `verify`/`manifest`
      forwarding (`verified` column present; `ffm_manifest()` reads the
      attached manifest) and that segments are written.
- [ ] T5: Roxygen docs for `segment_videos()` with a runnable `run = FALSE`
      example and `@seealso segment_video`/`ffm_batch`; run
      `devtools::document()`; add the `_pkgdown.yml` reference row.
- [ ] T6: Add the NEWS bullet; run `devtools::check()` to green; add the two
      deferred candidate rows (auto-naming, per-row reencode) to ROADMAP.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan. Name `segment_videos()`, columns
  `input/output/start/end`, shared internal helper, `...`→`ffm_batch` — all
  user-decided at the plan gate. Extends M03 (`segment_video`); consistent
  with D007.

## Decisions
<!-- owner: implement / review · append-only; milestone-local; promote
     cross-cutting ones to cairn/DECISIONS.md -->

## Review
<!-- owner: review · exclusive; evidence per criterion; consistency-gate
     results; independent-review findings and their triage -->
