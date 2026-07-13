<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M27: Metadata scrubbing for de-identification (`strip_metadata` + `_batch`)

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** high   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Principles touched:** IP1, GP1   <!-- owner: plan · works under; adds no principle -->
- **Branch/PR:** `m27-strip-metadata`   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Add task verbs that strip identifying container/global metadata and chapters
via lossless stream-copy — the IRB/de-identification front door (M25 survey K2).

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:**

- `strip_metadata(infile, outfile, run = TRUE)` — a zero-config strip-all
  Layer-2 wrapper (IP1) over `ffm_files` → `ffm_copy` (stream-copy A/V + `-map 0`)
  → `ffm_output_options("-map_metadata -1", "-map_chapters -1", "-fflags +bitexact")`.
  Compile stays binary-free.
- `strip_metadata_batch(jobs, run = TRUE, parallel = FALSE, ...)` — the
  table-driven sibling (D014 `_batch` suffix), a thin `ffm_batch` wrapper reusing
  a shared `strip_metadata_pipeline()` so per-row parity is inherited (M13). `jobs`
  needs an `input` column; an optional `output` column overrides the derived
  `<base>_stripped.<ext>` name; duplicated **resolved** output paths are rejected
  (M26), not just duplicated inputs.
- Shared `strip_metadata_pipeline(input, output)` holding the recipe once.
- Empirical guard establishing **which tags actually clear** and that **stream-copy
  preserves A/V**: known global tags (`creation_time`, `location`, `make`, `model`,
  `title`, `comment`) and chapters are gone; `-fflags +bitexact` stops FFmpeg
  re-adding its own `creation_time`/`encoder` tag; video + audio streams and the
  display-matrix rotation survive unchanged.
- Docs: roxygen (`@family task verb functions`, `@seealso` to `anonymize_video()`
  as the visual de-id sibling and to the `probe_*`/`mediainfo_*` readers) that
  **documents the boundary** — per-stream tags (`handler_name`, `language`) and
  codec-embedded identifiers are *not* removed by a stream copy.

**Out:**

- Per-stream metadata clearing (`handler_name`, `language`, …) — would require
  probing stream count at compile time, breaking the binary-free/pure-compile
  guarantee (D002/D013). → documented limitation; a candidate row if later wanted.
- Re-encode-based scrubbing of codec-embedded identifiers → out (GP1/D001 scope;
  Layer-0 escape hatch).
- Visual de-identification (faces/regions) → already `anonymize_video()`
  (M18/M20/M21) + the region-blur candidate.
- New `_pkgdown.yml` sections or vignette prose beyond the two verbs' index
  entries → routine doc sync only (M23 lesson), not new narrative content.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] **AC1 (compile, binary-free):** `strip_metadata(infile, outfile, run = FALSE)`
      returns a command containing `-map_metadata -1`, `-map_chapters -1`,
      `-fflags +bitexact`, `-codec:v copy`, `-codec:a copy`, and `-map 0`, with no
      FFmpeg invocation. Tested purely.
- [ ] **AC2 (tags clear — the "which tags clear" guard):** on a fixture carrying
      known global tags (`creation_time`, `location`, `make`, `model`, `title`,
      `comment`) plus chapters, the stripped output's ffprobe `format` and stream
      `tags` contain **none** of those tags and **no** chapters, and carry **no**
      re-added `encoder` or `creation_time` tag. Execution test; `skip_if` no ffmpeg.
- [ ] **AC3 (A/V preserved — the "does copy preserve A/V" guard):** the stripped
      output keeps the source video and audio streams (codecs, dimensions,
      duration within tolerance) and, when the source carries a rotation display
      matrix, that rotation survives. Verified by probing. Execution test.
- [ ] **AC4 (batch):** `strip_metadata_batch(jobs, run = FALSE)` returns the jobs
      tibble plus a `command` column with one stream-copy strip command per row;
      output names derive to `<base>_stripped.<ext>` when no `output` column is
      given; duplicated resolved output paths are rejected (M26). Tested purely.
- [ ] **AC5 (suite + check):** `devtools::test()` passes with the new tests;
      `devtools::check()` reports **0 errors / 0 warnings / 0 notes** — confirm via
      `00check.log` `Status: OK` after `spelling::update_wordlist()` for any new
      technical terms (M17 lesson).
- [ ] **AC6 (docs):** both verbs `@export`ed and documented with the boundary
      prose and `@seealso` web above; `devtools::document()` clean and
      `pkgdown::check_pkgdown()` clean (M23 lesson); README regenerated only if a
      documented example changed (path-only churn reverted — M24 lesson).

## Coverage
<!-- owner: plan · create/amend-via-gate; each acceptance criterion → the
     task(s) satisfying it, by positional number. -->

- AC1 → T1
- AC2 → T1, T2
- AC3 → T2
- AC4 → T3
- AC5 → T2, T3, T4
- AC6 → T4

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] **T1 — scalar verb + shared pipeline.** In `R/ffmpeg.R`, add
      `strip_metadata_pipeline(input, output)` (`ffm_files` → `ffm_copy` →
      `ffm_output_options` with the three flags) and `strip_metadata(infile,
      outfile, run = TRUE)` as a thin wrapper with front-door guards
      (`check_file_exists`, `rlang::check_string`, `rlang::check_bool`) closing on
      `ffm_finish`. Write the pure compile test (AC1). Model on `format_for_web()`
      at `R/ffmpeg.R:435` and `extract_audio()` at `R/ffmpeg.R:268`.
- [x] **T2 — execution guard + fixture.** In a new
      `tests/testthat/test-strip-metadata.R`, build a tagged+rotated temp fixture
      from `inst/extdata/sample.mp4` (inject global tags + a rotation display
      matrix via a Layer-0 `ffmpeg()`/`ffm` call, `skip_if` no ffmpeg), run
      `strip_metadata`, and assert with ffprobe (`-show_format`/`-show_entries`)
      that tags + chapters are gone, no `encoder`/`creation_time` re-added (AC2),
      and video+audio streams + rotation survive (AC3).
- [ ] **T3 — batch sibling.** In `R/ffmpeg.R`, add `derive_stripped_names()` and
      `strip_metadata_batch(jobs, run = TRUE, parallel = FALSE, ...)` reusing
      `strip_metadata_pipeline()` via `ffm_batch` (model on
      `standardize_video_batch()` at `R/ffmpeg.R:1794`), with column type/NA guards
      and the duplicate-resolved-path guard (M26). Add
      `tests/testthat/test-strip-metadata-batch.R` compile + guard tests (AC4).
- [ ] **T4 — docs + gate.** Roxygen for both verbs (boundary prose + `@seealso`
      web + `@family`); `devtools::document()`; `pkgdown::check_pkgdown()`;
      `spelling::update_wordlist()`; confirm `devtools::check()` 0/0/0 via
      `00check.log` and full `devtools::test()` green (AC5, AC6); regenerate README
      only if an example changed.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-13: created by /milestone-plan (target chosen from ROADMAP K2 candidate;
  gate calls — global+chapters+`-fflags +bitexact` scrub depth, batch sibling
  included, zero-config strip-all surface; per-stream stripping ruled out to keep
  compile binary-free).
- 2026-07-13: T1 — added `strip_metadata_pipeline()` + `strip_metadata()`
  (`R/ffmpeg.R`) and the pure compile test (AC1). Recipe: `ffm_copy` +
  `ffm_output_options("-map_metadata -1", "-map_chapters -1", "-fflags +bitexact")`.
- 2026-07-13: T2 — added `make_tagged_video()`/`probe_format_tags()`/
  `probe_rotation()` helpers and the execution guard tests (AC2, AC3). Empirically
  confirmed: identifying tags + chapters + FFmpeg's re-added encoder/creation_time
  all clear; A/V streams + rotation display matrix survive the copy.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->
