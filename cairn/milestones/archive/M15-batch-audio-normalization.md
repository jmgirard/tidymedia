# M15: Batch audio normalization verb

- **Status:** done · **Depends on:** M14 · **PR:** https://github.com/jmgirard/tidymedia/pull/17

## Goal

Add `normalize_audios()`, the tibble-driven batch sibling of `normalize_audio()`
over `ffm_batch()` (D007) — the audio-side analogue of the M12→M13 split.

## Outcome

Shipped `normalize_audios(jobs, …)` in R/ffmpeg.R: a thin `ffm_batch()` fan-out
over the shared `normalize_audio_pipeline()`, modelled on `standardize_videos()`.
Per-row knob columns (`target_loudness`/`true_peak`/`loudness_range`/`channels`/
`sample_rate`) override scalar args; outputs auto-name `<base>_normalized.<ext>`
with a collision guard; front-door type/NA guards plus per-element value parity
inherited from the shared pipeline (M13 lesson); batch params forward via `...`
without leaking into `.f` (M09 lesson). Added `derive_normalized_names()`; tests
in test-normalize-audios.R (46 pass); NEWS + `_pkgdown.yml` entries. `check()`
0/0/0; two independent review lenses, zero findings.

- T1 was a no-op: M14 already put per-value validation in the shared pipeline.
- Follow-on: batch two-pass loudnorm → ROADMAP candidate (depends on M15 + M16).
