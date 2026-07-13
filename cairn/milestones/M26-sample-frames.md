<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M26: Fixed-rate frame sampling (`sample_frames` + `_batch`)

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** high   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Principles touched:** IP1, IP2, GP1   <!-- works under; none changed -->
- **Branch/PR:** `m26-sample-frames`   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal

Add a `sample_frames()` task verb (and `sample_frames_batch()` sibling) that
samples a video at a fixed rate/interval into a numbered image sequence — the
front door to per-frame coding and CV feature pipelines (M25 survey §3 K1).

## Scope

**In:**
- A scalar `sample_frames()` Layer-2 verb: one video → a numbered image
  sequence written to an output directory at a fixed rate (`fps =`) or
  interval (`interval =`, seconds between frames).
- A `sample_frames_batch()` sibling fanning out over a jobs tibble (many
  recordings → many sequences), reusing a shared `sample_frames_pipeline()`
  helper (M11/M13 lesson: extract the scalar body first).
- Output naming: user passes an output **directory** (+ optional `prefix` /
  image `format`); the verb synthesizes the zero-padded `%0Nd` printf pattern
  and returns the resolved pattern — FFmpeg's `image2` muxer fills it. This is
  a **single command with one pattern output**, consistent with IP2 (one
  output target / one muxer), and deliberately distinct from the D007 fan-out
  model (`segment_video`/`extract_frame_batch` emit N separate commands): a
  fixed-rate sample can't know its frame count ahead of decoding.
- Guards: exactly one of `fps`/`interval`; image-`format` whitelist; create
  the output directory if absent; numeric rates coerced to double so valid
  values don't trip `check_dim()`/`ffm_fps()` (M20 lesson); batch-column
  type/NA checks; batch return-schema parity with the normal `ffm_batch` path
  (M19 lesson).

**Out:**
- `strip_metadata` (M25 §3 K2) — the other high candidate; stays a candidate,
  planned next as its own milestone (distinct goal, no shared code).
- Content-based / keyframe-only frame selection via a raw `select` expression
  → refused at the gate as GP1 surface creep; `ffm_*`/Layer 0 already serve it.
- Contact-sheet / thumbnail montage tiling (M25 §3 D3) → stays a grouped
  candidate; builds on this verb later.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] **AC1 — Compiles pure & linear.** `sample_frames(run = FALSE)` returns a
      single reproducible FFmpeg command, binary-free, built from Layer-1
      builders only (`ffm_files` + fps filter + image output options) with the
      `image2` printf pattern as its output — no hand-glued command strings
      (IP1). Evidence: a compile test asserting the command string shape.
- [ ] **AC2 — Rate mapping & exclusivity.** `fps = R` compiles the `fps=R`
      filter; `interval = N` compiles `fps=1/N`; supplying both or neither
      errors with a cli message. Evidence: compile tests for each mapping +
      error-branch tests.
- [ ] **AC3 — Numbered output on disk.** Executing on the bundled sample video
      writes zero-padded, sequentially numbered image files (chosen `format`
      extension) into `outdir`, and the file count matches the requested rate ×
      duration (±1). Evidence: execution test (`skip_if` ffmpeg absent) that
      counts output files and checks the numbering.
- [ ] **AC4 — Batch fan-out & schema parity.** `sample_frames_batch(jobs)` runs
      one sequence per input row through the shared pipeline helper and returns
      the jobs tibble plus `command` (and the opt-in `success`/`verified`/
      manifest outputs with the same schema as the normal `ffm_batch` path).
      Evidence: batch compile test + a `names()`/types parity test vs. the
      canonical path.
- [ ] **AC5 — Front-door guards.** Missing/unreadable input, a non-image
      `format`, an uncreatable `outdir`, and NA/wrong-type batch columns each
      abort with a cli message; a bare-integer `fps`/`interval` is accepted
      (coerced to double, M20). Evidence: one test per error branch + a
      bare-integer acceptance test.
- [ ] **AC6 — Documented, wired, clean.** Both verbs carry roxygen
      (`@family task verb functions`, `@seealso` to `extract_frame`/
      `extract_frame_batch`, `@examples` with `run = FALSE`), are added to
      `_pkgdown.yml` (M23 lesson), the spelling wordlist covers any new terms
      (M17 lesson), a NEWS entry is added, and `devtools::check()` reports
      `Status: OK` (0 errors/0 warnings/0 notes). Evidence: `00check.log`.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1, T2
- AC2 → T2, T4
- AC3 → T1, T2, T4
- AC4 → T3, T4
- AC5 → T2, T3, T4
- AC6 → T5

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] **T1 — Shared `sample_frames_pipeline()` helper.** Build one `ffm`
      pipeline from an already-resolved output **pattern** and rate string:
      `ffm_files(infile, pattern)` → fps filter (via `ffm_fps()`, rate coerced
      to double) → still-image output options (mirror `frame_pipeline()`'s
      quality flags where they apply to the image encoder). All per-value
      validation lives here so the batch sibling inherits it (M13). Add beside
      `frame_pipeline()` in `R/ffmpeg.R:82`.
- [x] **T2 — Scalar `sample_frames()`.** Args
      `(infile, outdir, fps = NULL, interval = NULL, format = "png",
      prefix = NULL, run = TRUE)`. Validate: input readable, exactly one of
      `fps`/`interval`, `format` in the image whitelist; create `outdir` if
      absent; synthesize `<outdir>/<prefix>_%0Nd.<format>` (fixed generous pad
      width, `start_number` 1); resolve `interval → 1/N`; call the helper;
      `ffm_finish(..., run)`. Return the resolved pattern with the command.
- [x] **T3 — Batch `sample_frames_batch(jobs, ...)`.** Per-row closure over the
      helper (column type/NA guards only — value checks come from the helper,
      M13); route through `ffm_batch` so `success`/`verified`/manifest outputs
      match the normal schema (M19). Follow the shape of
      `extract_frame_batch()` (`R/ffmpeg.R:1356`).
- [x] **T4 — Tests.** Compile purity & command shape (AC1); fps/interval
      mapping + XOR error (AC2); execution test counting numbered files at the
      requested rate (`skip_if` no ffmpeg, AC3); batch compile + schema-parity
      (AC4); all error branches + bare-integer acceptance (AC5).
- [ ] **T5 — Docs, wiring & check.** Roxygen for both verbs; `document()`; add
      both to `_pkgdown.yml`; update the spelling wordlist; NEWS entry; run
      `devtools::check()` to `Status: OK` and `pkgdown::check_pkgdown()`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-13: created by /milestone-plan (promotes candidate K1 from the M25
  survey; scope + design calls settled at the plan gate — output-dir+auto-pattern
  naming, dual fps/interval rate arg, batch sibling included).
- 2026-07-13: T1–T3 — added `sample_frames()`/`sample_frames_batch()` + shared
  `sample_frames_pipeline()` and `resolve_sample_fps`/`derive_frame_pattern`/
  `check_image_format`/`ensure_dir`/`derive_frames_dir` helpers in R/ffmpeg.R;
  documented. Verified execution frame counts (fps=2→4, fps=5→10, interval=1→2
  over a 2 s clip) and interval→fps reciprocal mapping.
- 2026-07-13: T4 — scalar tests in test-ffmpeg.R (compile shape, fps/interval
  mapping + XOR, bare-integer coercion, format/outdir/input guards, execution
  frame count) + new test-sample-frames-batch.R (per-row parity, auto/scalar
  outdir, column overrides, ffm_batch schema parity, guards, manifest). Full
  suite green (149 pass, 0 fail).

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->
