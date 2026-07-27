# M38: `hardware=` nvenc on `separate_audio_video` (+ batch)

**Status:** done (2026-07-26, PR #40 https://github.com/jmgirard/tidymedia/pull/40)

**Goal:** Give `separate_audio_video()` and its `_batch` sibling the opt-in
`hardware = "nvenc"` GPU toggle the other re-encode verbs carry, on video only.

**Outcome:** Both verbs gained `hardware = c("none","nvenc")` + `fallback`,
passed positionally into `separate_stream_pipeline()`, whose video branch
forwards them to `apply_video_codec()` and whose audio branch reads neither, so
the audio command is byte-identical in all four combinations; both front doors
`arg_match(hardware)` first. Because `video_codec` defaults to `"copy"` here
(D020), a guard aborts when a video codec resolving to `"copy"` meets
`hardware != "none"`, mirroring `segment_pipeline()` — and `ffm_batch` builds all
pipelines before running any, so a mixed table fails before encoding. `hardware`
is batch-wide (D016); a `hardware` jobs column is dropped by the 2N reshape.

**Decisions:** none milestone-local; sits under D016, D020, D008.

**Review:** 3 lenses — blame-history 0, diff-bug 2, prior-PR 1 (comment probe
empty; archived `## Review` sections were the evidence). All >=80, all fixed:
F1 (88) the guard hint claimed `NULL` lets the container choose, but under nvenc
it assumes H.264 — proven by compiling `.webm` to `h264_nvenc`; rewritten and
regression-tested. F2 (85) same claim in NEWS. F3 (88) the GPU test's AAC-in-MP4
fixture could not tell copy from re-encode (M35) — now `make_mp3_audio_video()`.
