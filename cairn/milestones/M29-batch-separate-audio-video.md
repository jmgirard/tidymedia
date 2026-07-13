<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M29: Batch sibling for separate_audio_video (fan-out)

- **Status:** planned   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** M28   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Principles touched:** IP1   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Branch/PR:** —   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal

Give `separate_audio_video` a table-driven `_batch` sibling: one jobs row →
two single-output pipelines (audio + video), mirroring `segment_video_batch`'s
fan-out handling (D007) and completing batch coverage of the fan-out verbs.

## Scope

**In:** a new exported `separate_audio_video_batch` — a thin Layer-2 fan-out
over `ffm_batch()` (D007) where each row carries one input and produces two named
outputs (`audiofile`, `videofile`), sharing the scalar verb's recipe. Optional
per-row `reencode` column; duplicate-path guard pooled across **both** output
columns (M26); return-schema parity with the existing `_batch` verbs (M19).

**Out:**
- Batch for the composite/fan-in verbs (`concatenate_videos`, `compare_videos`,
  `picture_in_picture`) → ROADMAP candidate (jobs-tibble input-shape design call).
- Any change to `separate_audio_video`'s scalar behavior — the shared-recipe
  extraction must be compile-preserving.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] AC1 — `separate_audio_video_batch` is exported and is a thin fan-out over
      `ffm_batch()`: each jobs row emits **two** single-output pipelines (one
      `0:a` → audiofile, one `0:v` → videofile) sharing the scalar verb's recipe,
      gluing no command strings of its own (IP1/D007). Evidence:
      `grep '^export' NAMESPACE`; a compile test showing N rows → 2N commands.
- [ ] AC2 — Jobs guards with `cli` errors: non-data-frame `jobs`, zero-row
      `jobs`, missing `input`, `NA` in `input`. An optional per-row `reencode`
      column overrides the `reencode` argument, falling back when absent.
      Evidence: one test per guard; a test showing a `reencode` column flips one
      row's copy-vs-re-encode while a column-less row uses the default.
- [ ] AC3 — Duplicate-path guard is pooled across both output columns: no two
      resolved output paths collide across the whole table (any audiofile ↔ any
      videofile, any row), and `audiofile` ≠ `videofile` within a row (M26).
      Outputs derive when a column is absent (`<base>_audio.<ext>` /
      `<base>_video.<ext>`, documented). Evidence: a test where a cross-column
      path collision is rejected.
- [ ] AC4 — Return-schema parity: the fan-out result carries the same
      `ffm_batch()` columns (`command`, opt-in `verified`/manifest) as the other
      `_batch` verbs, with the fan-out row shape defined and consistent (model on
      `segment_video_batch`'s return). Evidence: a test comparing `names()`/types
      against `segment_video_batch` on a matched call.
- [ ] AC5 — Docs & housekeeping synced (roxygen `@family`/`@seealso` scalar +
      `ffm_batch` + batch-disambiguation prose per M24; `man/`, `NAMESPACE`,
      `_pkgdown.yml`, spelling wordlist) and `devtools::check()` shows
      `Status: OK` in `00check.log` (M23/M24/M17). Evidence: `pkgdown::check_pkgdown()`
      clean; `00check.log` `Status: OK`.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1, T2
- AC2 → T2, T3
- AC3 → T2, T3
- AC4 → T2, T3
- AC5 → T4

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [ ] T1 — Extract a shared recipe from `separate_audio_video` (R/ffmpeg.R:308)
      so the scalar verb and the batch sibling build the two pipelines identically
      (the audio `0:a` map + optional `-c:a copy`, the video `0:v` map + optional
      `-c:v copy`); scalar behavior compile-preserved.
- [ ] T2 — Write `separate_audio_video_batch` as a fan-out over `ffm_batch()`,
      modeled on `segment_video_batch` (R/ffmpeg.R:1476) for the one-input →
      many-output return shape: jobs guards, per-row `reencode` column, pooled
      cross-column duplicate-path guard, and two-column output derivation.
- [ ] T3 — Tests (`tests/testthat/`): N rows → 2N commands (AC1), the jobs
      guards + `reencode` column override (AC2), pooled cross-column duplicate
      rejection and within-row equality rejection (AC3), schema parity against
      `segment_video_batch` (AC4). Execution tests `skip_if` ffmpeg absent (D004).
- [ ] T4 — Docs & housekeeping: roxygen (`@family`, `@seealso`, batch
      disambiguation), `document()`, `NAMESPACE`, `_pkgdown.yml` sync, spelling
      wordlist, `devtools::check()` → `Status: OK` (AC5).

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan (batch-coverage gap analysis — Tier 2, fan-out).

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->
