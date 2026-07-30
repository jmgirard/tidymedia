# M45: Give a multi-track `separate_audio_video()` caller a way out

**Status:** done (2026-07-30, PR #48 https://github.com/jmgirard/tidymedia/pull/48)

**Goal:** Let a caller separate a multi-track file into a single-stream audio
container, and say why when FFmpeg refuses.

**Outcome:** `audio_stream` on `separate_audio_video()` + `_batch` (argument and
per-row column, `NA` = every track), reaching only the audio branch of
`separate_stream_pipeline()`; `audio_stream_map(null_map =)` keeps this verb's
`NULL` at `-map 0:a`, the extraction verbs at `0:a:0`. Layer-2
`run_separation_audio()` + `ffmpeg_exit_status()` re-raise a failed audio command as
`tidymedia_multitrack_separation` naming the count, `audio_stream` and `.mka`;
`warn_failed_separation_batch()` warns once post-fan-out; `check_batch_stream_values()`
validates each cell up front. Breaking: the argument precedes `run`.

**Decisions:** D025 — the `NULL` split across verb families (extends D023).
M45-D1/D2 — both FFprobe probes adopt D024's licence rather than stretch it: the
scalar's runs only after FFmpeg failed, the batch's only on failed rows.

**Review:** 3 lenses + scorer; blame-history and prior-review returned zero. CI red
first (1 return): ubuntu ffmpeg 6.1.1 writes 3 audio streams to `.aac` where macOS
8.1.2 refuses, so 7 tests saw no error. Fixed F4 (88), F3 (82), F2/F7 (false user
text), F9/F12/F13; F1+F5 → candidate row; rest logged. LESSONS: +2, M27's line folded in.
