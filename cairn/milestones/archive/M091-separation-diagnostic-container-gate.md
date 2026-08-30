# M091: The multi-track advice stops arriving when the caller is already following it

**Status:** done (2026-08-30, PR #95 https://github.com/jmgirard/tidymedia/pull/95)

**Goal:** `separate_audio_video()` and its batch sibling stop telling a caller to write a multi-stream container when their output already is one.

**Outcome:** `multi_audio_extensions` (`mka`, `m4a`, `mp4`, `mov`, `mkv`, `webm`, `ogg`,
`opus`, `ts`) and the case-insensitive `holds_multiple_audio()` sit beside the Layer-2
separation helpers in `R/ffmpeg.R`. `run_separation_audio()` falls open to `ffm_run()`'s own
condition on a listed `audiofile` — asked after the exit-status check, before the FFprobe
probe; `warn_failed_separation_batch()` drops listed rows before that probe, so such a row gets no
bullet and an all-listed batch warns not at all. An exclusion list: an unmeasured container keeps
today's diagnostic. Both help pages state when the report fires, that it may silently not fire, and
that it reports what the call did; the batch page states `ffm_batch()`'s own `success = FALSE`
condition, no per-row exit status surviving it.

**Decisions:** D069 (the gate and its measured basis), D070 (the video-written bullet rides every
fail-open branch), D071 (a refusal under one codec is not a capacity refusal).

**Review:** Four rounds, three amendment returns, no defect return. AC1 amended for M090's
video-written bullet; AC5's batch half narrowed to what `ffm_batch()` enforces; AC3/AC4 amended
after review measured `.ogg`/`.opus` as codec refusals and added them. Final round, three-lens
fan-out, five findings: two stale "seven" comments, an overclaimed source comment and a loose
fail-open clause fixed now; batch case-insensitivity to a candidate row; the work log's own "seven"
rejected. CI green but `codecov/project` at −0.03%, merged on a logged override.
