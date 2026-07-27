# M39: `audio_codec` for `standardize_video` and `anonymize_video` (+ batch)

**Status:** done (2026-07-26, PR #41 https://github.com/jmgirard/tidymedia/pull/41)

**Goal:** Give the two remaining configurable video transforms a user-facing `audio_codec`, so D017's documented remedy for the copy-into-an-incompatible-container trap actually exists on them.

**Outcome:** All four verbs gained `audio_codec = "copy"` beside `video_codec`
(gate call: codec pair adjacent, so `pixel_format` now separates the codecs from `hardware`
on these two alone). Both pipelines reuse M35's `apply_audio_codec()` seam, so `NULL` emits
no `-codec:a` and `check_token` keeps its attribution; `standardize_pipeline()` gained a
`call` formal at review. Batch: per-row `audio_codec` column via `check_batch_codec_col()` +
`batch_codec_cell()`, kept out of the neighbouring `str_cols` loop because `video_codec`
there has no sentinel. Defaults byte-identical, proven by compiling from master's own code;
`R/ffm.R` zero diff (IP1). Also corrected `format_for_web_batch`'s roxygen and both batch
verbs' `@param jobs` column enumerations, which M39 falsified.

**Decisions:** none milestone-local; sits under D016, D017, D014.

**Review:** 3 lenses — blame-history and prior-PR both clean (GH comment probe empty;
archived `## Review` sections were the evidence). Diff-bug found 4; scored 88/87/90/25.
F1 (88) `@param jobs` still said an `audio_codec` column was ignored. F2 (87)
`standardize_pipeline()` attributed token errors to itself, not the verb. F3 (90) the
batch-wide argument was untested — a mutation ignoring it left the suite green, while the
same mutation on `segment_video_batch` went red. All three fixed. F4 (25, positional shift
unstated in NEWS) logged unactioned, then fixed at the user's election at the merge gate.
