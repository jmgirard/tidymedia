# M44: Say something when audio tracks are dropped

**Status:** done (2026-07-30, PR #47 https://github.com/jmgirard/tidymedia/pull/47)

**Goal:** Warn a caller whose input carried audio tracks the output did not,
instead of losing them in silence.

**Outcome:** `extract_audio()`, `convert_audio()` and both `_batch` siblings emit
a `tidymedia_dropped_audio` warning naming the dropped count, `audio_stream`, and
the offset between `probe_audio()`'s absolute stream `index` and `audio_stream`'s
audio-relative one. `count_audio_streams()` (`R/ffprobe.R`) is the single FFprobe
token site; `warn_dropped_audio()` / `warn_dropped_audio_batch()` (`R/ffmpeg.R`)
build the message, the batch form probing unique inputs once, up front, before
`ffm_batch()` (untouched), and emitting ONE warning naming every affected row.
Gated on `isTRUE(run)` and a NULL `audio_stream`; fails open on a bad FFprobe.

**Decisions:** D024 — the pure surface is compilation and `run = FALSE` (D013's
two-pass path the sole exception); a `run = TRUE` call may run a binary whose
outcome changes nothing but a diagnostic condition. A clarification, not a
carve-out, per RR02.

**Review:** RB02/RR02 (Fable) reframed the premise before code landed and bound
six criteria, ingested as AC7–AC12 with three deviations. Fan-out: 15 findings,
2 actioned — F1 (96) cli glue-interpolated file paths in the bullets, F2 (82)
locator errors escaped the probe. F4 (72) graduated to a candidate row.
