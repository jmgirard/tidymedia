# M16: Two-pass (measured/linear) EBU R128 loudnorm

- **Status:** done · **Depends on:** M14 · **PR:** https://github.com/jmgirard/tidymedia/pull/18

## Goal

Add accurate two-pass loudnorm to `normalize_audio()` via `two_pass = TRUE`: an
analysis pass measures the input, a linear correction pass hits the EBU R128
target precisely — tidymedia's first analyze-then-build execution pattern.

## Outcome

Shipped `normalize_audio(two_pass = TRUE)`. `ffm_loudnorm()` gained
`measured_i/tp/lra/thresh`, `offset`, `linear`, `print_format` (numeric,
all-or-none set), keeping all `loudnorm=` assembly in the one builder (D002). New
`R/loudnorm_two_pass.R`: analysis-pass builder (`print_format=json` + `-f null
-`), a regex stderr parser (no JSON dep, D011) with a recorded real-FFmpeg
fixture, and the analyze-then-build orchestrator beside `ffm_run()` (compilation
stays pure). Correction pass reuses `normalize_audio_pipeline()` for channels/
`sample_rate`/`-c:v copy` parity; single-pass unchanged. Empirically two-pass
~0.01 LU vs single ~2.2 LU on a high-LRA source. `check()` 0/0/0; 667 tests pass;
two review lenses + scorer, no surviving defect.

- **D013:** analyze-then-build — `run = FALSE` no longer binary-free under `two_pass` (analysis always runs; `run` gates only correction).
- Follow-ons: batch two-pass = M17 (planned); graceful silent-input (`-inf`) handling → candidate (review F2, scored 68).
