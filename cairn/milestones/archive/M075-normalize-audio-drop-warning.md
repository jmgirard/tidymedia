# M075: The silent narrowing announces itself

**Status:** done (2026-08-27, PR #79 https://github.com/jmgirard/tidymedia/pull/79)

**Goal:** Give the two loudness verbs the dropped-audio-track warning the rest of the audio-producing family already carries.

**Outcome:** Three new call sites of M44's builder — same `tidymedia_dropped_audio`
class and wording, same `isTRUE(run) && is.null(audio_stream)` gate as the five
existing ones. `normalize_audio()` carries TWO, mutually exclusive on `two_pass`
(`R/ffmpeg.R:2210`, `2253`), so the two-pass path warns BEFORE
`run_loudnorm_analysis()` — the scalar/batch divergence D039 prevents. That block falls
through, so without the `!two_pass` gate one drop warns twice. `normalize_audio_batch()`
warns once above its block, naming every row, its probes serial so `parallel` misses them.
`check_audio_codec_not_copy()` is hoisted onto the single-pass path (the pipeline's guard
sits inside `ffm_finish()`'s argument, after the probe), `channels`/`sample_rate` hoisted
above it so no precedence moves. Cost: one FFprobe call per distinct input.

**Decisions:** D054 — the discarded VIDEO stays silent on all six audio-producing verbs;
D030's first sentence is the disclosure channel.

**Review:** Two rounds. Round 1 returned on AC1 — the two-pass path signalled two
conditions, invisible to a test running only the default `two_pass`. Round 2 green on all
seven; two findings, one actioned (the copy hoist had moved `channels` precedence, M41
A3r3's shape) and fixed at the gate. The fresh-context fan-out could not be spawned (a
session instruction forbids the Agent tool); the lenses ran in-context, disclosed.
