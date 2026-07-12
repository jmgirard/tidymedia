# M14: Audio loudness normalization verb — done 2026-07-12

**Goal:** add `normalize_audio()`, a Layer-2 verb that loudness-normalizes a
file's audio to an EBU R128 target (single-pass `loudnorm`), plus the Layer-1
audio-filter primitive `ffm_loudnorm()` it needs.

**Outcome:** two new exported functions (PR #16):
- `ffm_loudnorm(object, target_loudness=-23, true_peak=-1, loudness_range=7)` —
  first builder to write the `-af`/`filter_audio` slot; range-validated.
- `normalize_audio(infile, outfile, …targets…, channels, sample_rate, run)` — a
  thin wrapper (IP1) over a shared `normalize_audio_pipeline()` helper (M15 batch
  sibling inherits validation, M13 lesson); video stream-copied (`-c:v copy`),
  optional downmix (`-ac`) / resample (`-ar`).
  Full suite 279 pass; `devtools::check()` zero errors/warnings/notes; CI green.

**Key decisions:** single-pass (dynamic) loudnorm, keeping pure-compile /
`run=FALSE` (two-pass measured mode deferred → candidate). EBU R128 defaults
cited to EBU Rec. R 128 (2014) + ITU-R BS.1770-4 (no-oracle gate). Descriptive
param names over FFmpeg `I`/`TP`/`LRA`.

**Review:** Finding 1 (95, fixed) — docs/AC4 wrongly claimed `sample_rate=NULL`
preserves the source rate; single-pass loudnorm resamples (≤192 kHz, encoder-
capped; empirically 44100→96000). Fixed by honest docs + a gated AC4 amendment
(document, not probe) + a pinned-rate exec test. Finding 2 (72, downmix-after-
loudnorm drift) logged, not actioned.
