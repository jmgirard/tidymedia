# M14: Audio loudness normalization verb

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Branch/PR:** m14-audio-loudness-normalization

## Goal

Add `normalize_audio()`, a Layer-2 verb that normalizes a file's loudness to an
EBU R128 target via single-pass `loudnorm` (with optional downmix and resample),
plus the first Layer-1 audio-filter primitive `ffm_loudnorm()` that writes the
engine's until-now-unused `filter_audio`/`-af` slot.

## Scope

**In:**
- A Layer-1 audio-filter primitive `ffm_loudnorm()` that appends a `loudnorm=…`
  token to `object$filter_audio` — the first verb to populate that slot, which
  `ffm_compile()` already emits as `-af` (R/ffm.R:1043).
- A scalar Layer-2 verb `normalize_audio(infile, outfile, …)`: single-pass
  EBU R128 loudness normalization with primary-source-cited default targets
  (integrated loudness / true peak / loudness range), optional `channels`
  (downmix) and `sample_rate` (resample), and the video stream copied unchanged
  (`-c:v copy`) so only audio is touched. `run = FALSE` returns a pure compiled
  command (no binary).

**Out:**
- Two-pass measured/`linear` loudnorm (analyze → parse JSON → apply) → candidate
  row. It cannot produce the final command without running the binary, so it
  breaks the pure/CI-safe `run = FALSE` invariant — a new analyze-then-build
  execution pattern that is its own milestone.
- Batch sibling `normalize_audios()` → M15 (planned, depends on M14).
- Audio mixing / multi-input audio (`amix`) → stays deferred (D009).

## Acceptance criteria

- [ ] AC1 — `ffm_loudnorm()` appends a `loudnorm=…` token to `filter_audio` and
      a single-input pipeline compiles to `-af "loudnorm=I=…:TP=…:LRA=…"`;
      compilation is pure (no binary invoked). Evidence: passing compile test.
- [ ] AC2 — `normalize_audio(infile, outfile, run = FALSE)` returns one
      reproducible compiled command applying EBU R128 loudnorm with the default
      targets and `-c:v copy`, invoking no binary. Evidence: passing test.
- [ ] AC3 — The default target values (integrated loudness `I`, true peak `TP`,
      loudness range `LRA`) match a cited EBU R128 / ITU-R BS.1770 primary
      source, and that source is named in both the roxygen and this milestone.
      Evidence: citation present; default values equal the cited targets.
      (RB tripwire: no-oracle)
- [ ] AC4 — Supplying `channels` and/or `sample_rate` adds the corresponding
      downmix/resample instruction to the compiled command; leaving them `NULL`
      (default) omits them and preserves the source layout/rate. Evidence: test
      covering both the supplied and omitted branches.
- [ ] AC5 — Invalid arguments (missing `infile`, non-scalar or out-of-range
      `I`/`TP`/`LRA`, non-positive `channels`/`sample_rate`) raise cli/rlang
      errors; no new assertthat (D004). Evidence: passing validation tests.
- [ ] AC6 — An execution test (`skip_if` ffmpeg absent) runs `normalize_audio()`
      on `inst/extdata/sample.mp4` and verifies a non-empty output carrying a
      decodable audio stream. Evidence: passing skip-guarded test.
- [ ] AC7 — `devtools::check()` clean (zero errors/warnings/notes).

## Coverage

- AC1 → T1
- AC2 → T3
- AC3 → T2, T3
- AC4 → T3
- AC5 → T3
- AC6 → T4
- AC7 → T5

## Tasks

- [ ] T1 — Add `ffm_loudnorm()` Layer-1 audio-filter primitive to `R/ffm.R`
      (mirror the `ffm_scale()` pattern near R/ffm.R:308, but append to
      `object$filter_audio`). Tests-first for the compiled `-af "loudnorm=…"`
      output and pure compilation.
- [ ] T2 — Determine and record the EBU R128 / ITU-R BS.1770 default targets
      (`I`, `TP`, `LRA`) with a primary-source citation; capture the source and
      the numbers for the roxygen and this milestone. (RB tripwire: no-oracle)
- [ ] T3 — Add `normalize_audio(infile, outfile, channels = NULL,
      sample_rate = NULL, …targets…, run = TRUE)` Layer-2 verb to `R/ffmpeg.R`
      composing `ffm_loudnorm()` + `-c:v copy` + optional downmix/resample; a
      thin wrapper (IP1) that glues no strings of its own. Structure the body via
      a shared `normalize_audio_pipeline()` helper carrying per-value validation
      so M15's batch sibling inherits parity for free (M13 lesson). Parameter
      validation via cli/rlang. Tests-first for compile + validation, covering
      both the with- and without-option branches. (RB tripwire: irreversible-api
      — the exported signature soaks toward CRAN)
- [ ] T4 — Add an execution test in `tests/testthat/` (`skip_if` binary absent)
      verifying non-empty, audio-decodable output on the sample.
- [ ] T5 — Roxygen + `devtools::document()`; add to the `@family` lists and to
      DESIGN.md function families; `devtools::check()` clean.

## Work log

- 2026-07-12: created by /milestone-plan.

## Decisions

## Review
