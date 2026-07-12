# M09: Dataframe-driven batch segmentation

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Branch/PR:** m09-batch-segmentation · https://github.com/jmgirard/tidymedia/pull/10   <!-- owner: implement (branch) / review (PR URL) · create -->

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

- [x] `segment_videos()` is exported and, on a jobs tibble spanning ≥2 distinct
      input files, returns the `ffm_batch()` tibble with one `command` row per
      job; each command reflects its row's `input`/`output`/`start`/`end`
      (tested with `run = FALSE`, pure/CI-safe).
- [x] `reencode = TRUE` (default) produces accurate output-seek commands
      (`-ss … -to …` after `-i`, no stream copy); `reencode = FALSE` produces
      the fast copy path (`-ss … -to … -i`, `-codec:v copy -codec:a copy`,
      `-avoid_negative_ts make_zero`) — parity with `segment_video()`,
      verified per-row on a table.
- [x] Forwarded `...` reaches `ffm_batch()`: passing a `verify` spec adds a
      `verified` column and passing `manifest = TRUE` attaches a manifest
      readable by `ffm_manifest()` (binary-gated execution test).
- [x] Input validation: a non-data-frame `jobs`, an empty `jobs`, and a `jobs`
      missing any of the four required columns each abort with a `cli` message
      naming the problem (tested for each branch).
- [x] `segment_video()` still passes its existing test suite unchanged after
      the shared-helper refactor (behaviour preserved; no test edits required).
- [x] Docs/index consistent: roxygen with a runnable example, a
      `_pkgdown.yml` row for `segment_videos`, and a NEWS bullet — all present
      in the same change; `devtools::document()` leaves `man/`/`NAMESPACE`
      clean.
- [x] `devtools::check()` clean (0 errors / 0 warnings).

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

- [x] T1: Extract the shared cut logic from `segment_video()`'s inline `.f`
      (`R/ffmpeg.R:492-502`) into an internal helper `segment_pipeline(input,
      output, start, end, reencode)`; rewrite `segment_video()` to call it.
      (tests-first: existing `segment_video()` tests must stay green.)
- [x] T2: Write failing tests for `segment_videos()` — multi-file dry-run
      command construction, both `reencode` paths per row, and each validation
      branch (non-df / empty / missing column) — in
      `tests/testthat/test-ffmpeg.R` (or a new `test-segment-videos.R`).
- [x] T3: Implement `segment_videos()` in `R/ffmpeg.R`: validate the four-column
      contract, build the jobs tibble, and call `ffm_batch(jobs, .f =
      segment_pipeline-wrapper, run=, parallel=, ...)`, forwarding `...`.
- [x] T4: Add a binary-gated execution test proving `verify`/`manifest`
      forwarding (`verified` column present; `ffm_manifest()` reads the
      attached manifest) and that segments are written.
- [x] T5: Roxygen docs for `segment_videos()` with a runnable `run = FALSE`
      example and `@seealso segment_video`/`ffm_batch`; run
      `devtools::document()`; add the `_pkgdown.yml` reference row.
- [x] T6: Add the NEWS bullet; run `devtools::check()` to green; add the two
      deferred candidate rows (auto-naming, per-row reencode) to ROADMAP.
      (Candidate rows were added at plan time; verified present.)

## Work log

- 2026-07-12: created by /milestone-plan. Name `segment_videos()`, columns
  `input/output/start/end`, shared internal helper, `...`→`ffm_batch` — all
  user-decided at the plan gate. Extends M03 (`segment_video`); consistent
  with D007.
- 2026-07-12: T1 — extracted `segment_pipeline()` helper; `segment_video()`
  now wraps it. Existing test-ffmpeg.R suite green (80 pass, 0 fail).
- 2026-07-12: T2–T4 — `segment_videos()` implemented in `R/ffmpeg.R` (new
  `test-segment-videos.R`, 25 pass incl. binary-gated verify/manifest
  forwarding). Column contract validated with `cli::cli_abort()`; `...`
  forwards to `ffm_batch()`. Roxygen written (document() in T5).
- 2026-07-12: T5 — `document()` (export + `segment_videos.Rd`, family
  cross-links refreshed); added `_pkgdown.yml` reference row.
- 2026-07-12: T6 — NEWS bullet added (no milestone number, per tracking
  rules); `devtools::check()` clean (0 errors, 0 warnings, 0 notes). All
  tasks done → status review.
- 2026-07-12: review — all 7 ACs verified with fresh evidence; CI green all
  platforms. Pruned done rows M01–M03 (retention). Independent review: F1
  (weak test assertion, scored 80) fixed; F3 (start/end type validation,
  scored 68) → candidate; F2 (scored 20) rejected as by-design.

## Decisions — none (cross-cutting ones live in DECISIONS.md)

## Review

Reviewed 2026-07-12, branch `m09-batch-segmentation` (PR #10); main in sync.

**AC evidence (fresh, all PASS).** AC1–AC4: `test-segment-videos.R` 25/25
(multi-file per-row commands, both reencode paths, binary-gated verify/manifest
forwarding, each validation branch). AC5: `test-ffmpeg.R` 80/80 after the
shared-helper refactor. AC6: `document()` no-diff, `check_pkgdown()` clean, NEWS
present. AC7: `check()` clean (zero errors/warnings/notes). CI green all
platforms.

**Consistency gate PASS.** `cairn_validate.py` substantive checks pass (pruned
done rows M01–M03). Its `iso date format` FAIL is a known false positive — the
check-result shorthand in the M02–M08 archives (append-only, untouched) matches
its date regex. Coverage: 7 ACs → tasks; no principle changed; README no rebuild.

**Independent review** (2 reviewers + scorer): refactor confirmed
behaviour-preserving, D007/D008/IP1-faithful; no correctness defects. **F1 (80)
fixed** (weak missing-column test tightened to `"Missing column"`). **F3 (68) →
candidate** (no `start`/`end` type check vs `segment_video()`; AC4 scoped to
presence). **F2 (20) rejected** (extra columns ignored is by-design).
