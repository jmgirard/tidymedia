<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M12: Video standardization verb

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Branch/PR:** m12-standardization-presets   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Add `standardize_video()`, a Layer-2 verb that re-encodes a video to a
reproducible, consistent format (resolution, framerate, codec, pixel format),
introducing the `ffm_fps()` Layer-1 filter it needs.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:** a new Layer-1 `ffm_fps(object, fps)` verb (appends the `fps=<n>`
video filter to `filter_video`, IP2-compliant single-input sequential
filter), and an exported `standardize_video(infile, outfile, ...)` that
composes `ffm_scale()` ([R/ffm.R:308](../../R/ffm.R)), `ffm_fps()`,
`ffm_codec()`, `ffm_pixel_format()`, and `+faststart` into one reproducible
command with documented, analysis-friendly defaults — a parameterized
generalization of `format_for_web()` ([R/ffmpeg.R:236](../../R/ffmpeg.R)).
Optional `width`/`height` (aspect preserved via scale's `-2` even-dimension
expression when only one is given), optional `fps`, overridable `vcodec` /
`pixel_format`. Composes existing Layer-1 verbs only (IP1) — no hand-glued
strings.

**Out:** all audio work — loudnorm, downmix, resample, sample-rate
standardization → the audio-normalization milestone (candidate; owns the new
Layer-1 audio-filter primitive). A batch sibling `standardize_videos()` →
candidate row (composable meanwhile via `ffm_batch()` directly). Named-preset
*bundles* (e.g. `preset = "archive"`) are not built here; the default
argument set is the one documented standard (see the API tripwire on T4).

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] AC1: `ffm_fps(object, fps)` appends exactly `fps=<fps>` to the video
      filter chain, compiling into the `-vf` list ([R/ffm.R:1006](../../R/ffm.R));
      a non-positive or non-numeric/non-string `fps` aborts with `cli::cli_abort`.
      Oracle: the compiled `-vf` string.
- [ ] AC2: `standardize_video()` with explicit `width`, `height`, `fps`,
      `vcodec`, `pixel_format` compiles to one command carrying `scale`,
      `fps`, `-c:v`, `-pix_fmt`, and `-movflags +faststart` in the engine's
      order. Oracle: the full compiled command string against a hand-written
      expected value.
- [ ] AC3: Aspect handling — only `width` (or only `height`) supplied scales
      preserving aspect with even output dimensions (the other dimension emits
      `-2`); both supplied forces exact dimensions; neither leaves resolution
      untouched. Oracle: compiled `scale=` expression.
- [ ] AC4: Defaults alone (`standardize_video(infile, outfile)`) produce a
      documented, deterministic standard — the same input yields a
      byte-identical command across runs, and the roxygen states what that
      standard is.
- [ ] AC5: Input validation — bad `fps`/`width`/`height` and a missing input
      file each abort with a `cli::cli_abort` message; `run = FALSE` returns
      the command without invoking FFmpeg.
- [ ] AC6 (execution; `skip_if` no ffmpeg): a standardized output file exists
      and, probed with the metadata verbs, has the requested framerate and
      width.
- [ ] `devtools::check()` clean (zero errors/warnings/notes).

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1, T2
- AC2 → T3, T4
- AC3 → T3, T4
- AC4 → T3, T4
- AC5 → T3, T4
- AC6 → T6
- check clean → T5, T6

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] T1: Tests first — `ffm_fps()` compilation/validation in
      `tests/testthat/test-ffm.R`: happy path emits `fps=<n>` in `-vf`, and
      bad `fps` aborts (AC1).
- [x] T2: Implement `ffm_fps()` in [R/ffm.R](../../R/ffm.R) (mirror
      `ffm_scale()` at line 308; write to `filter_video`); roxygen with a
      runnable example; add `ffm_fps` to `_pkgdown.yml` under "Layer 1"; a new
      exported builder needs its reference row in the same commit (guardrail);
      `devtools::document()`.
- [ ] T3: Tests first — `standardize_video()` compilation in
      `tests/testthat/test-ffmpeg.R`: full-argument command parity, aspect
      cases (width-only / height-only / both / neither), defaults determinism,
      and validation/`run = FALSE` (AC2–AC5).
- [ ] T4: Implement `standardize_video()` in [R/ffmpeg.R](../../R/ffmpeg.R) as
      a thin composition of `ffm_scale`/`ffm_fps`/`ffm_codec`/
      `ffm_pixel_format`/`ffm_output_options` (IP1). **(RB tripwire:
      irreversible-api)** — settle the exported signature and whether the
      "standard" is an explicit-parameter default set or a named-`preset`
      bundle before finalizing; the exported API is hard to change later.
- [ ] T5: Roxygen for `standardize_video()` documenting the default standard;
      add its row to `_pkgdown.yml` under "Layer 2: task verbs";
      `devtools::document()`; `devtools::test()`.
- [ ] T6: Execution test (`skip_if` no ffmpeg) proving the output exists with
      the requested fps/width via the probe verbs (AC6); `devtools::check()`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan (split from the four-family
  research-verbs candidate; standardization family, video-only per plan gate).
- 2026-07-12: T1+T2 — added `ffm_fps()` Layer-1 verb (uses `check_dim` so a
  positive number or FFmpeg framerate string is accepted, else aborts),
  pkgdown row, docs; 4 compile/validation tests. test-ffm.R clean (165 pass).

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

- 2026-07-12 (T4 tripwire, irreversible-api): the "standard" is an
  explicit-parameter default set, not a named-`preset` bundle. Chosen over
  escalation because it is additive-forward (a `preset=` arg can be layered on
  later without breaking callers), keeps every output-affecting knob visible in
  the compiled command (D001), and matches the plan Scope. Defaults:
  `vcodec = "libx264"`, `pixel_format = "yuv420p"` (mirrors `format_for_web()`),
  `width/height/fps = NULL` = leave untouched, `+faststart` always applied,
  audio untouched (audio work is out of scope). User delegated the call.

## Review
<!-- owner: review · exclusive -->
