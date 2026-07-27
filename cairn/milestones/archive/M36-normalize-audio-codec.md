# M36: `audio_codec` for `normalize_audio` (+ batch)

**Status:** done (2026-07-26, PR #38 https://github.com/jmgirard/tidymedia/pull/38)

**Goal:** Give `normalize_audio()` and `normalize_audio_batch()` an `audio_codec` argument so
the normalized output's encoder is named instead of falling to the container default.

**Outcome:** Both verbs take `audio_codec`, threaded through `normalize_audio_pipeline()` via
`apply_audio_codec()`. Default `NULL` is D016's sentinel, so every prior command compiles
byte-identically. New `check_audio_codec_not_copy()` refuses `"copy"` from three call sites —
a filtered stream cannot be copied — with Layer 1's `ffm_groups()` still enforcing (IP1 intact).
Batch takes a per-row `audio_codec` column (`NA` → unset) reusing `check_batch_codec_col(col =)`
/`batch_codec_cell()`. Two seams, not one: `normalize_audio_batch(two_pass = TRUE)` bypasses its
own `ffm_batch()` call and fans out through `run_normalize_correction()`, threaded too.

**Decisions:** D019 (D016's `NULL` sentinel, deliberately not D017's `"copy"` default, since copy
is impossible where the filter forces a re-encode). Local M36-D1: the Layer-2 `"copy"` refusal is
a front door for vocabulary and early failure, not a second implementation of a Layer-1 rule.

**Review:** 3 lenses — blame-history and prior-PR-comments clean (M35's CRLF trap did not recur;
M34's all-NA column lesson reused). Diff-bug found 4; fixed F1 (85) `{.val NULL}` rendering as a
quoted `"NULL"` that `check_token` accepts, and F3 (82) the token check not hoisted above the
two-pass analysis pass. F2 (68), F4 (55) logged below the bar. CI 9/9. No lesson retired.
