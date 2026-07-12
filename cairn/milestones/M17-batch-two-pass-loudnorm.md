# M17: Batch two-pass (measured/linear) loudnorm

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M15, M16
- **Branch/PR:** m17-batch-two-pass-loudnorm

## Goal

Add accurate two-pass loudnorm to `normalize_audios()` via `two_pass = TRUE`,
fanning M16's analyze-then-build path across a jobs tibble — the audio-side
M14→M15 scalar→batch split applied to the correction pipeline.

## Scope

**In:**
- A `two_pass = FALSE` argument on `normalize_audios()`. Default keeps today's
  single-pass batch behavior and its pure `run = FALSE` compile, byte-for-byte.
- When `two_pass = TRUE`, a **two-phase fan-out**: Phase 1 runs M16's analysis
  pass per row (honoring `parallel`), parses each with M16's stderr parser, and
  appends `measured_I/TP/LRA/thresh/offset` as columns. Phase 2 reuses
  `ffm_batch()` over `normalize_audio_pipeline()` (threading M16's measured
  values + `linear=true`) to build & run one reproducible **correction** command
  per row — inheriting `verify`/`manifest`/`progress`/`parallel` and the
  reproducible `command` column for free.
- `run = FALSE` under two-pass mirrors the scalar M16 contract: Phase 1
  analysis still runs (needs binary + files); the correction commands are
  returned in the `command` column **unexecuted** (no `success`, no outputs).
- Fail-fast on a bad row: if any row's analysis fails or its measured block is
  absent/malformed, abort **before** Phase 2 with a message naming the offending
  row(s) — parity with the "resolve verify specs up front" fail-fast pattern.

**Out:**
- Any change to single-pass defaults, to the scalar `normalize_audio()`
  (M16 owns the scalar two-pass path), or to `ffm_batch()` itself — this
  milestone adds no engine change, only a Layer-2 fan-out.
- A per-row `two_pass` column (mixed single/two-pass batches) — deliberately
  not built; `two_pass` is a whole-batch mode. If ever wanted → new candidate.
- Loudness targets/ranges beyond what `ffm_loudnorm()` already validates.

## Acceptance criteria

- [ ] AC1 — With `two_pass = FALSE` (default), `normalize_audios()` compiles
      byte-for-byte identically to today, including pure `run = FALSE` (no binary
      touched). Evidence: characterization test pinning the `command` column
      unchanged from the current single-pass batch.
- [ ] AC2 — Given a jobs table pre-augmented with fixed measured-value columns
      (no binary), the correction fan-out builds one command per row carrying
      `measured_I/TP/LRA/thresh`, `offset`, and `linear=true`, with per-row knob
      columns (`target_loudness` etc.) and `channels`/`sample_rate`/`-c:v copy`
      preserved from the shared pipeline. Evidence: passing pure test over a
      fixed jobs+measured fixture.
- [ ] AC3 — The measured-table assembly (`assemble_measured()`), given per-row
      parse results (reusing M16's parser), populates the five measured columns
      per row and aborts naming the offending row when a row's block is
      absent/malformed. Evidence: passing pure test over fixture parse results
      including a malformed row.
- [ ] AC4 — With `two_pass = TRUE, run = FALSE`, the analysis passes run and the
      `command` column holds the correction commands unexecuted (no `success`
      column, no output files written). Evidence: passing skip-guarded test
      (`skip_if` ffmpeg absent) asserting return shape and that no outputs exist.
      (RB tripwire: irreversible-api — new exported arg + batch execution
      contract where `run = FALSE` no longer implies binary-free.)
- [ ] AC5 — An execution test (`skip_if` ffmpeg absent) runs full two-pass over a
      ≥2-row jobs table on the sample, then re-probes each output's integrated
      loudness and asserts each lands within ±1 LU of its per-row target.
      Evidence: passing skip-guarded test. Source: EBU R 128 (2014); ITU-R
      BS.1770-4.
- [ ] AC6 — `devtools::check()` clean (zero errors/warnings/notes); roxygen for
      the new arg updated; `@family` unchanged-correct; NEWS entry added.

## Coverage

- AC1 → T1, T4
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6

## Tasks

- [x] T1 — Characterization test first: pin today's single-pass
      `normalize_audios(run = FALSE)` `command` column so the `two_pass = FALSE`
      default is provably unchanged before touching anything.
- [x] T2 — Add the correction-phase builder (`.f`): thread per-row measured
      columns + `linear = TRUE` through `normalize_audio_pipeline()`
      ([R/ffmpeg.R:438](R/ffmpeg.R)) reusing M16's measured-value threading, so
      channels/`sample_rate`/`-c:v copy`/knob parity is inherited. Tests-first
      with a fixed jobs+measured fixture (AC2).
- [x] T3 — Add the analysis fan-out + `assemble_measured()`: run M16's analysis
      pass per row (honoring `parallel`), parse via M16's parser, and a pure
      `assemble_measured()` that maps per-row parse results to the five measured
      columns, aborting with the offending row index on an absent/malformed
      block. Pure test of `assemble_measured()` over fixture results incl. a
      malformed row (AC3).
- [x] T4 — Add `two_pass = FALSE` to `normalize_audios()`
      ([R/ffmpeg.R:1266](R/ffmpeg.R)) wiring Phase 1 (analysis fan-out → measured
      columns) → Phase 2 (`ffm_batch()` over the T2 builder), forwarding
      `run`/`parallel`/`verify`/`manifest`/`progress`; `run = FALSE` runs analysis
      and returns correction commands unexecuted. Skip-guarded return-shape +
      no-outputs test (AC4). (RB tripwire: irreversible-api.)
- [ ] T5 — Execution test (`skip_if` ffmpeg absent): full two-pass over a ≥2-row
      jobs table on `inst/extdata/sample.mp4`; re-probe each output's integrated
      loudness within ±1 LU of its per-row target (AC5).
- [ ] T6 — Roxygen for the new arg (document the analyze-then-build fan-out, its
      per-row nature, and the `run = FALSE`-not-binary-free semantics); NEWS
      entry; `devtools::document()`; `devtools::check()` clean. Record the
      milestone-local decisions (fail-fast on a bad analysis row; `two_pass`
      scalar-only, no per-row column) in the Decisions section; promote to a
      D-entry only if M16's analyze-then-build D-entry doesn't already cover the
      batch extension (AC6).

## Work log

- 2026-07-12: created by /milestone-plan (promoted from ROADMAP candidate, split
  from M16 on 2026-07-12). Planned ahead of M16 implementation; tasks reference
  M16's planned analysis-pass builder, stderr parser, and `ffm_loudnorm()`
  measured params, so may shift if M16's shape changes at implementation.
- 2026-07-12: T1 done — characterization test pins the single-pass
  `normalize_audios(run = FALSE)` command column (all five knobs) before the
  two-pass path is added.
- 2026-07-12: T2 done — added `run_normalize_correction()` (Phase 2 fan-out over
  `ffm_batch()`/`normalize_audio_pipeline()`, threading measured columns +
  `linear=true`); pure AC2 test over a fixed jobs+measured fixture.
- 2026-07-12: T3 done — added `run_loudnorm_analysis_batch()` (Phase 1 per-row
  analysis honoring `parallel`) and `assemble_measured()` (parse each via M16's
  parser → five measured columns, fail-fast naming offending rows); pure AC3
  tests over the recorded fixture incl. malformed + non-zero-exit rows.
- 2026-07-12: T4 done — wired `two_pass = FALSE` into `normalize_audios()`:
  Phase 1 fan-out → measured columns → Phase 2 correction, forwarding
  `run`/`parallel`/`...`; `run = FALSE` runs analysis only. Skip-guarded AC4 test
  asserts return shape and no outputs written.

## Decisions

- 2026-07-12: Measured values surface on the two-pass result as columns
  `measured_I/measured_TP/measured_LRA/measured_thresh/offset` (FFmpeg-arg
  spellings), for per-row provenance (question gate).
- 2026-07-12: `run = FALSE`-not-binary-free batch contract is a faithful
  fan-out of M16's scalar contract (D013) — proceeded without Fable escalation
  despite the `irreversible-api` tripwire (question gate).

## Review
