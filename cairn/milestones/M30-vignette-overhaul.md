<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M30: Vignette overhaul — quality, clarity, realism + fuller verb coverage

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate -->
- **Principles touched:** IP1, GP1   <!-- worked under (docs restate them faithfully; add/change none) -->
- **Branch/PR:** m30-vignette-overhaul · https://github.com/jmgirard/tidymedia/pull/32   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal

Raise the vignettes' quality, clarity, and realism — reframe getting-started to
lead with the task-verb front door, and add a research-workflow vignette that
demonstrates every Layer-2 task verb inside a concrete dyadic-interaction
preprocessing pipeline.

## Scope

**In:**
- A **new** article vignette, `vignettes/workflow.Rmd`, framed as an end-to-end
  behavioral/affective-science pipeline (preprocessing a folder of recorded
  dyadic-interaction sessions for coding & annotation). It is the home for the
  task verbs no vignette shows today: `standardize_video`, `normalize_audio`,
  `convert_audio`, `sample_frames`, `extract_frame`, `strip_metadata`,
  `anonymize_video`, `format_for_web`, `concatenate_videos` — each inside a
  plausible research step, plus at least one `_batch` sibling to reinforce the
  batch story.
- Reframe `vignettes/tidymedia.Rmd` (getting-started) to **lead with a task
  verb** (the front door most users touch — IP1/D002), then descend to the
  Layer-1 builder; adds `extract_audio` and `crop_video` in context. Keep the
  Layer-0 escape-hatch note.
- Realism/clarity polish of `vignettes/batch.Rmd` and `vignettes/metadata.Rmd`:
  consistent sample framing, honest examples, cross-links between all four
  vignettes ("Where to next"). No structural rewrite of these two.
- Register `workflow.Rmd` in `_pkgdown.yml` (articles); NEWS Documentation entry.

**Out:**
- Any `R/` change — verbs, arguments, exports, roxygen `@examples` (those were
  handled by M23/M24; a future examples pass is its own scope).
- New verbs or verb behavior; realism must not imply capability the package
  refuses (GP1) — e.g. `anonymize_video` is a **fixed-region box-fill**, never
  face tracking, and must be described as such.
- CRAN release mechanics (win-builder/R-hub, cran-comments, version bump) →
  stays the standing CRAN candidate row.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] AC1 — `vignettes/workflow.Rmd` exists as a registered article, framed as a
      dyadic-interaction preprocessing pipeline, and builds cleanly.
- [ ] AC2 — Every exported Layer-2 task-verb family appears in ≥1 vignette,
      each inside a plausible research example (no contrived one-liners).
      Evidence: a grep of `vignettes/` for each family name in the NAMESPACE
      task-verb list, plus inspection confirming the context. Families:
      `extract_audio`, `convert_audio`, `crop_video`, `format_for_web`,
      `segment_video`, `separate_audio_video`, `sample_frames`, `extract_frame`,
      `strip_metadata`, `normalize_audio`, `standardize_video`,
      `anonymize_video`, `compare_videos`, `concatenate_videos`,
      `picture_in_picture`.
- [ ] AC3 — `vignettes/tidymedia.Rmd` opens on a Layer-2 task verb before any
      `ffm_*` builder call (front-door-first altitude; IP1/D002).
- [ ] AC4 — All vignettes build with **no** ffmpeg/ffprobe/mediainfo binary on
      PATH: every evaluated chunk is binary-free (compile-only `run = FALSE`) or
      guarded (`eval = has_*`/`eval = FALSE`). Evidence: build vignettes with the
      binaries masked off PATH; clean build. (Note: `normalize_audio(two_pass =
      TRUE)` runs a binary even under `run = FALSE` — D013 — so any evaluated
      chunk must use `two_pass = FALSE` or a guard.)
- [ ] AC5 — `workflow.Rmd` is listed in `_pkgdown.yml` and
      `pkgdown::check_pkgdown()` passes; all four vignettes cross-link.
- [ ] AC6 — `devtools::check()` is 0 errors / 0 warnings / 0 notes, existing
      tests unaffected, and NEWS.md gains a Documentation entry.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T2
- AC2 → T1, T2, T3 (audited in T5)
- AC3 → T1
- AC4 → T2, T5
- AC5 → T4, T5
- AC6 → T4, T5

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] T1 — Reframe `vignettes/tidymedia.Rmd`: open with a task verb
      (`extract_audio` on the shipped `sample.mp4`), state that task verbs are
      thin front-door wrappers (IP1), then descend to the builder as it reads
      now; add `crop_video` in context; tighten toy examples. Keep Layer-0 note.
- [x] T2 — Author `vignettes/workflow.Rmd`: a numbered dyadic-interaction
      pipeline (standardize → normalize/convert audio → sample/extract frames →
      de-identify via strip_metadata + fixed-region anonymize_video → concatenate
      session parts → format_for_web for coder sharing), with ≥1 `_batch` step.
      Directory-scan chunks `eval = FALSE`; evaluated chunks compile-only and
      binary-free (mind D013 for normalize_audio). Describe `anonymize_video`
      honestly as fixed-region box-fill (GP1).
- [x] T3 — Polish `vignettes/batch.Rmd` and `vignettes/metadata.Rmd` for realism
      and consistency; add "Where to next" cross-links tying all four vignettes
      together. No structural rewrite.
- [x] T4 — Register `workflow.Rmd` in `_pkgdown.yml` (articles / index); add a
      NEWS.md Documentation entry describing the vignette overhaul.
- [x] T5 — Verify: build vignettes with binaries masked off PATH (AC4);
      `pkgdown::check_pkgdown()`; grep-audit every task-verb family across
      `vignettes/` (AC2); `devtools::check()` 0/0/0.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan. Fulfills the "examples/vignette pass"
  portion of the CRAN candidate (release mechanics stay on that row); the M24
  docs gap-fill covered roxygen @seealso + the metadata reader table only.
- 2026-07-12: T1 reframed tidymedia.Rmd front-door-first (opens on
  `extract_audio`, adds `crop_video`); descends to the builder unchanged.
- 2026-07-12: T2 authored workflow.Rmd (dyadic-interaction pipeline) — homes the
  9 previously-unshown task verbs + `standardize_video_batch`; `anonymize_video`
  described honestly as fixed-region box-fill (GP1).
- 2026-07-12: T3 added "Where to next" cross-links to batch/metadata + a
  per-verb `*_batch` siblings section in batch.Rmd. Fixed jobs-tibble columns to
  `input`/`output` (not `infile`/`outfile`) after a masked-render error.
- 2026-07-12: T4 registered workflow.Rmd in `_pkgdown.yml` articles; NEWS
  Documentation entry.
- 2026-07-12: T5 verified — all 4 vignettes build with ffmpeg/ffprobe/mediainfo
  masked off PATH (AC4); grep-audit shows all 15 task-verb families present
  (AC2); `pkgdown::check_pkgdown()` clean; `devtools::check()` 0/0/0.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->
