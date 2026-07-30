# M41: Front-door validation parity for the codec arguments

**Status:** done (2026-07-29, PR #43 https://github.com/jmgirard/tidymedia/pull/43)

**Goal:** Every codec argument on every task verb rejects a non-string value at
the front door, naming its own argument and its own verb.

**Outcome:** Seven pairs repaired. `normalize_audio_batch` `audio_codec` silently
compiled the default on a scalar `NA` (`batch_codec_cell()` maps it to the `NULL`
sentinel); `standardize_video`, `standardize_video_batch`, `anonymize_video_batch`,
`extract_audio_batch`, `convert_audio` and `normalize_audio` aborted but leaked
Layer-1's `video`/`audio`, blamed a `*_pipeline()` helper, or fired inside
`purrr::pmap()` with `In index:`. Each guard sits at the END of its verb's
front-door validation, reassigning no other check's precedence; four `_batch` verbs
also refuse a bad scalar a matching `jobs` column used to override in silence.

**Decisions:** M41-D1 (hoist a duplicate `check_string()` rather than thread `call`
through a shared pipeline), M41-D2 (refuse the bad scalar even under a column,
following `separate_audio_video_batch`). Neither cross-cutting; D016–D021 untouched
and `NULL`/column-`NA` meaning stays M42's.

**Review:** Three rounds; AC4 returned twice as a global negative measurement kept
falsifying, then passed once amended to an enumerated changed-set (33 rows: 21+12).
Round 3: 19 findings, 3 at 80+ (two fixed, one declared in NEWS), 16 logged.
