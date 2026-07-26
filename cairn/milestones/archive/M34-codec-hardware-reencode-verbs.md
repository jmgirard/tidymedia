# M34: `video_codec` + `hardware=` for the four codec-less re-encode verbs

**Status:** done (2026-07-26, PR #36 https://github.com/jmgirard/tidymedia/pull/36)

**Goal:** Give `crop_video`, `segment_video`, `compare_videos`, `picture_in_picture`
(+ `_batch` siblings) a `video_codec` arg plus M31's `hardware=`, changing no default output.

**Outcome:** All eight gained `video_codec = NULL` / `hardware` / `fallback` before `run`.
The `NULL` sentinel emits no `-codec:v`, so defaults compile byte-identically.
`resolve_hw_encoder()` gained a NULL branch ahead of `codec_family()` (nvenc→h264;
fallback→NULL, never an injected libx264). New Layer-2 helpers `apply_video_codec()`,
`check_batch_codec_col()`, `batch_codec_cell()`; `segment_pipeline()` aborts when a
stream copy meets a codec or `hardware != "none"`. Batch: `video_codec` per-row column
(NA→sentinel), `hardware`/`fallback` batch-wide. `R/ffm.R` zero diff (IP1/IP2).

**Decisions:** D016 (codec-arg API shape, from RR01). Session gate: formals go before
`run` for sibling parity under D014's clean break; the sentinel's nvenc fallback message
names the container default, not a codec absent on that path.

**Review:** 11/11 criteria verified; AC2 proven by diffing 12 default compilations against
master (zero bytes). Three findings — F2 (80) fixed, `check_batch_codec_col()` admitted an
all-NA column of any type; F1 (25) doc residue fixed on request; F3 (58) logged unactioned.
Hygiene: M32's all-NA-typing clause retired into the sharper M34 lesson, now test-enforced.
