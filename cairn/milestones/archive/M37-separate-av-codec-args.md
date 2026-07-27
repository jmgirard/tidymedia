# M37: codec args subsume `reencode` on `separate_audio_video` (+ batch)

**Status:** done (2026-07-26, PR #39 https://github.com/jmgirard/tidymedia/pull/39)

**Goal:** Replace `separate_audio_video()`'s `reencode` switch with per-stream
`audio_codec` / `video_codec` arguments defaulting to `"copy"`.

**Outcome:** Both verbs take `audio_codec` / `video_codec`: `"copy"` compiles the
pre-M37 `reencode = FALSE` commands byte for byte, `NULL` what `reencode = TRUE`
did, a name pins that stream alone. `separate_stream_pipeline()` takes a
per-stream `codec` dispatched by `stream` to `apply_audio_codec()` /
`apply_video_codec()`. The batch's 2N reshape collapses per-row `audio_codec` /
`video_codec` columns (`NA` = unset, `check_batch_codec_col`) into one resolved
`codec` column routed by the `stream` marker. `reencode` is gone in both
spellings — argument and jobs column — each aborting with a migration message,
as does a `codec` argument through `...`.

**Decisions:** D020 (subsumption, `"copy"` over D016's sentinel, D014 waiver,
stale-spelling guards, the single-`codec`-column reshape).

**Review:** blame-history and prior-PR 0 findings; diff-bug 4, all fixed with
regression tests — stale `reencode` *column* ignored (90), scalar codec args
skipping `check_string` when a column was present (92), flaky bare-`aac` match
on hex tempfile paths (80), `codec` via `...` setting both streams (75).
