# M40: `audio_codec` subsumes `format` on `convert_audio` (+ batch), closing the codec sweep

**Status:** done (2026-07-27, PR #42 https://github.com/jmgirard/tidymedia/pull/42)

**Goal:** Rename `convert_audio()`'s `format` to `audio_codec` as a clean break,
give its batch column a way to spell "unset", and close the codec sweep.

**Outcome:** `convert_audio()`, `convert_audio_batch()`, and the shared
`convert_audio_pipeline()` take `audio_codec`; `format` removed, no `lifecycle`
shim (D014). `NULL` still compiles `-q:a 0`, byte-identical against master's
actual pre-rename function. Column guard moved from
`check_batch_string_col` to `check_batch_codec_col` + `batch_codec_cell`, so `NA`
spells the default. Both retired spellings abort naming the replacement (via
`...`, and a `format` jobs column); the scalar relies on R's `unused argument`.
Adds a front-door `check_string()` on the batch scalar arg, and `na_means` on
`check_batch_codec_col()` so its hint is true per caller.

**Decisions:** D021 closes the sweep — the rename, `convert_audio`'s deliberate
`NULL` = `-q:a 0` departure from D016's sentinel, the three fixed-recipe verbs
(`format_for_web`, `strip_metadata`, `concatenate_videos`) staying codec-less,
and `extract_audio`'s recorded no-`NULL`/no-`NA` asymmetry.

**Review:** Blame-history and prior-review-record lenses clean. Diff-bug: no
functional defect, 4 doc/consistency findings (85/82/87/85), all fixed — stale
`@param jobs`, D021 overclaiming uniform `NA`, a false guard hint, weak tests.
