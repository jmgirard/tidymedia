# M16: Two-pass (measured/linear) EBU R128 loudnorm

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M14
- **Branch/PR:** m16-two-pass-loudnorm

## Goal

Add accurate two-pass loudnorm to `normalize_audio()` via `two_pass = TRUE`: an
analysis pass measures the input, and a second `linear=true` correction pass
hits the EBU R128 target precisely — introducing tidymedia's first
analyze-then-build execution pattern.

## Scope

**In:**
- A `two_pass = FALSE` argument on `normalize_audio()`. Default keeps today's
  single-pass (dynamic) behavior and its pure `run = FALSE` compile, byte-for-byte
  unchanged.
- When `two_pass = TRUE`: run an **analysis pass**
  (`loudnorm=…:print_format=json -f null -`), capture its stderr, parse the flat
  measured block (`input_i/tp/lra/thresh`, `target_offset`) with a small regex
  (no new JSON dependency — D011 spirit), then build and run a **correction pass**
  whose `loudnorm` filter carries `measured_I/TP/LRA/thresh`, `offset`, and
  `linear=true`.
- Extend `ffm_loudnorm()` to optionally accept the measured values, `linear`, and
  `print_format` so *all* `loudnorm=…` string assembly stays in the one builder
  (D002). No hand-glued filter strings at Layer 2.
- New execution orchestrator (analysis-run → parse → correction-build) as an
  internal helper alongside `ffm_run()`, not inside `ffm_compile()` (which stays
  pure). Reuses the shared `normalize_audio_pipeline()` for the correction pass so
  channels/`sample_rate`/`-c:v copy`/validation parity are inherited.
- `run = FALSE` semantics under two-pass: runs the analysis pass (needs binary +
  file) and returns the exact **correction command unexecuted** — the honest
  analyze-then-build contract; `run` only gates the correction pass.

**Out:**
- Batch two-pass (`normalize_audios(two_pass = TRUE)`) → new candidate row,
  depends on M15 + M16 (the M14→M15 fan-out pattern applied to this path).
- Any change to single-pass defaults or `normalize_audios()` (M15) — this
  milestone only adds the opt-in two-pass path to the scalar verb.
- Loudness targets/ranges beyond what `ffm_loudnorm()` already validates.

## Acceptance criteria

- [ ] AC1 — With `two_pass = FALSE` (default), `normalize_audio()` compiles and
      runs byte-for-byte identically to today, including pure `run = FALSE`
      (no binary touched). Evidence: passing test asserting the compiled command
      is unchanged from the current single-pass string.
- [ ] AC2 — The correction-command builder, given a fixed measured-values fixture
      (no binary), emits a `loudnorm` filter carrying `measured_I`, `measured_TP`,
      `measured_LRA`, `measured_thresh`, `offset`, and `linear=true` with the
      values mapped correctly from the analysis keys, while preserving
      `channels`/`sample_rate`/`-c:v copy` from the shared pipeline. Evidence:
      passing pure test. (RB tripwire: irreversible-api — new builder params +
      new execution contract.)
- [ ] AC3 — The analysis-pass command compiles with `print_format=json` and
      `-f null` and no output file. The stderr parser extracts the five measured
      values from a captured loudnorm JSON fixture and rejects/aborts cleanly when
      the block is absent or malformed. Evidence: passing pure test over a
      recorded-stderr fixture. (RB tripwire: no-oracle — parser correctness has no
      runtime oracle in CI.)
- [ ] AC4 — With `two_pass = TRUE, run = FALSE`, the analysis pass runs and the
      returned value is the correction command string (unexecuted; output file not
      written). Evidence: passing skip-guarded test (`skip_if` ffmpeg absent)
      asserting the return shape and that no output was produced.
- [ ] AC5 — An execution test (`skip_if` ffmpeg absent) runs full two-pass on a
      sample, then re-probes the output's integrated loudness and asserts it lands
      within ±1 LU of the target, and is closer to target than the single-pass
      output on the same input. Evidence: passing skip-guarded test. Source: EBU
      R 128 (2014); ITU-R BS.1770-4.
- [ ] AC6 — `devtools::check()` clean (zero errors/warnings/notes); roxygen
      updated, `@family` and DESIGN.md function families reflect the new arg.

## Coverage

- AC1 → T1, T5
- AC2 → T2, T3
- AC3 → T2, T4
- AC4 → T5
- AC5 → T6
- AC6 → T7

## Tasks

- [ ] T1 — Characterization test first: pin today's single-pass compiled command
      for `normalize_audio(run = FALSE)` so the `two_pass = FALSE` default is
      provably unchanged before touching anything.
- [ ] T2 — Extend `ffm_loudnorm()` ([R/ffm.R:390](R/ffm.R)) to optionally take
      `measured_i/tp/lra/thresh`, `offset`, `linear`, and `print_format`, appending
      them to the `loudnorm=…` string; all validation for the new params lives
      here. Tests-first for both the analysis variant (`print_format=json`) and the
      correction variant (measured + `linear=true`).
- [ ] T3 — Add the correction-pass builder path: thread measured values +
      `linear` through `normalize_audio_pipeline()`
      ([R/ffmpeg.R:438](R/ffmpeg.R)) so it reuses channels/`sample_rate`/`-c:v copy`
      parity. Pure test with a fixed measured-values fixture (AC2).
- [ ] T4 — Add the internal stderr parser: extract the five measured values from a
      loudnorm JSON block via regex, with a clean abort when absent/malformed.
      Test over a recorded-stderr fixture, including a malformed case (AC3).
- [ ] T5 — Add `two_pass = FALSE` to `normalize_audio()`
      ([R/ffmpeg.R:413](R/ffmpeg.R)) and the analyze-then-build orchestrator
      (analysis-run capturing stderr via `run_program(stderr = TRUE)` →
      parse → correction-build → `ffm_finish()`), wiring the `run = FALSE`
      return-correction-command-unexecuted contract. Skip-guarded return-shape
      test (AC4).
- [ ] T6 — Execution test (`skip_if` ffmpeg absent): full two-pass on
      `inst/extdata/sample.mp4`, re-probe integrated loudness (`ffprobe`/`loudnorm`
      analysis), assert within ±1 LU of target and closer than single-pass (AC5).
- [ ] T7 — Roxygen for the new arg + measured params (document the analyze-then-
      build behavior and the `run = FALSE` semantics explicitly); `@family`;
      DESIGN.md families; `devtools::document()`; `devtools::check()` clean. Author
      the cross-cutting D-entry (analyze-then-build pattern; `run = FALSE` no longer
      guarantees binary-free under `two_pass = TRUE`) at review.

## Work log

- 2026-07-12: created by /milestone-plan (promoted from ROADMAP candidate, split
  from M14 on 2026-07-12).
- 2026-07-12: implement start; branch m16-two-pass-loudnorm cut from master.

## Decisions

## Review
