# M43: Pick which audio track the extraction verbs take

**Status:** done (2026-07-30, PR #46 https://github.com/jmgirard/tidymedia/pull/46)

**Goal:** Let a caller name which audio track `extract_audio()` and `convert_audio()`
take from a multi-track file, instead of FFmpeg choosing invisibly.

**Outcome:** `audio_stream` on `extract_audio()`, `convert_audio()` and both `_batch`
siblings — 0-based among the input's audio streams, front-door
`check_number_whole(min = 0, allow_null = TRUE)`, resolved by `audio_stream_map()` to
`0:a:<n>`. Batch takes a per-row `audio_stream` column via a parameterized
`check_batch_audio_col(col, na_means)` and `batch_stream_cell()`, where `NA` keeps
that row on track 0. `ffm_map()` takes a character vector, appends, gains
`replace = TRUE`. Breaking: `extract_audio()` maps explicitly (was FFmpeg's
DEFAULT-disposition heuristic; measured `spa` → `eng`), dropping subtitle carriage.

**Decisions:** D023 (indexing basis vs D009's input index; `ffm_map()`'s append
contract). M43-D1: `ffm_copy()`/`ffm_concat()` losing idempotence documented in NEWS,
not code-fixed — deferred to a candidate so it lands with its own tests.

**Review:** Three lenses + scorer; 15 findings. Actioned: F5 (80, the scalar
front-door guard is unpinnable and its comment was false), F9 (82, NEWS omitted the
positional shift); 13 logged, F2 (76) and F1 (72) documented. No lesson retired.
