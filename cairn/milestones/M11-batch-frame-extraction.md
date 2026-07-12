<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M11: Tibble-driven batch frame extraction

- **Status:** planned   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Branch/PR:** —   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Add `extract_frames()`, a jobs-table sibling of `extract_frame()` that grabs
one still image per row across many inputs, as a thin `ffm_batch()` wrapper.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:** an exported `extract_frames(jobs, ...)` verb mirroring
`segment_videos()` ([R/ffmpeg.R:589](../../R/ffmpeg.R)): one row = one frame
grab; required `input` column plus exactly one of `timestamp`/`frame`;
optional `output` column (auto-derived when absent, with an image extension);
one reproducible compiled command per row via `ffm_batch()`; `...` forwarded
to the runner (`verify`/`manifest`/`checksums`/`progress`/`parallel`).
Refactor the single-frame body of `extract_frame()`
([R/ffmpeg.R:51](../../R/ffmpeg.R)) into a shared `frame_pipeline()` helper so
both verbs build identical commands.

**Out:** any new frame-selection modes beyond `extract_frame()`'s current
timestamp/frame pair (e.g. every-Nth-frame, scene-change) → candidate row if
wanted. Audio/video format standardization → M12. No engine (Layer-1) change:
this is a Layer-2 fan-out only (D007).

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] AC1: `extract_frames(jobs)` with columns `input` + `timestamp` returns
      the `jobs` tibble plus a `command` column (and a derived `output` column
      when absent; a `success` column when `run = TRUE`) — one command per
      row, each **byte-identical** to the `extract_frame()` command for that
      row's arguments. Oracle: per-row parity against `extract_frame(..., run
      = FALSE)`.
- [ ] AC2: Table-level column exclusivity — exactly one of `timestamp` /
      `frame` present. Both columns, or neither, aborts with a `cli::cli_abort`
      message naming the offending columns; a missing `input` column aborts;
      a non-data-frame or zero-row `jobs` aborts.
- [ ] AC3: A `frame` column converts per row to a timestamp via that row's
      input framerate (`get_framerate()`), matching `extract_frame()`'s
      `frame`-path result.
- [ ] AC4: When `output` is absent, names are derived per row as
      `<input_sans_ext>_<n>.<img>` with numbering restarting per input file
      (parity with `derive_segment_names()`) and a configurable image
      extension (default `"png"`); the resolved column is carried on the
      returned tibble.
- [ ] AC5: `...` forwards `ffm_batch()` options to the runner without leaking
      into the per-row builder — a `manifest`/`verify`/`parallel` argument
      reaches `ffm_batch()`, and a stray `jobs` column does not break `.f`
      (guards the 2026-07-12 M09 `...`-forwarding lesson).
- [ ] AC6: R edge cases fire cleanly — `NA` in the `timestamp`/`frame` column
      is rejected with a clear message; a factor `input` column works as
      character; length-one `jobs` (single row) works.
- [ ] AC7 (execution; `skip_if` no ffmpeg): running a two-row `jobs` writes
      two image files that exist on disk.
- [ ] `devtools::check()` clean (zero errors/warnings/notes).

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1, T2, T3
- AC2 → T1, T2
- AC3 → T2, T3
- AC4 → T2, T4
- AC5 → T1, T3
- AC6 → T1, T3
- AC7 → T6
- check clean → T5, T6

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [ ] T1: Tests first — compilation/validation suite in
      `tests/testthat/test-extract-frames.R`: required-column and exclusivity
      checks (AC2), `NA`/factor/single-row edge cases (AC6), and `...`
      forwarding (AC5). No binaries; CI-safe (D004).
- [ ] T2: Tests first — parity + auto-naming: per-row command equals
      `extract_frame()` for both the `timestamp` and `frame` paths (AC1, AC3),
      and derived output names follow the per-input-restart rule with an image
      extension (AC4).
- [ ] T3: Refactor `extract_frame()`'s single-frame body
      ([R/ffmpeg.R:51](../../R/ffmpeg.R)) into an internal `frame_pipeline()`
      helper (mirrors `segment_pipeline()`); re-point `extract_frame()` at it
      so existing tests still pass.
- [ ] T4: Add `derive_frame_names(input, format = "png")` (mirrors
      `derive_segment_names()` at [R/ffmpeg.R:507](../../R/ffmpeg.R) but with a
      configurable image extension).
- [ ] T5: Implement `extract_frames()` as a thin `ffm_batch()` fan-out
      wrapping `frame_pipeline()` (the `segment_videos()` shape); roxygen with
      runnable `run = FALSE` example; add an `extract_frames` row to
      `_pkgdown.yml` under "Layer 2: task verbs"; `devtools::document()`.
- [ ] T6: Execution test (`skip_if` no ffmpeg) proving two frame files are
      written (AC7); run `devtools::test()` then `devtools::check()`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan (split from the four-family
  research-verbs candidate; frame-extraction family).

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->
