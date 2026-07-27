# M35: `audio_codec` for the four re-encode verbs — stream-copy by default

**Status:** done (2026-07-27, PR #37 https://github.com/jmgirard/tidymedia/pull/37)

**Goal:** Give `crop_video`, `segment_video`, `compare_videos`, `picture_in_picture`
(+ `_batch`) an `audio_codec` arg defaulting to `"copy"`, ending the silent re-encode.

**Outcome:** All eight gained `audio_codec = "copy"` beside `video_codec`; a named encoder
transcodes, `NULL` restores the unset behavior. New helpers `apply_audio_codec()` and
`check_batch_audio_col()`. `segment_pipeline()` aborts per row when a stream copy meets a
non-`"copy"` value; the composites emit `-codec:a` only inside the mapped-audio branch and
abort on a named encoder with `audio = NULL`. Batch: per-row `audio_codec` column
(`NA`→unset) reusing `check_batch_codec_col(col=)`. Closed three candidates: the composite
batch verbs' `audio` column guards (absent on compare, loose on pip) now share one helper,
and `format_for_web_batch` documents it reads no codec column. `R/ffm.R` zero diff (IP1/IP3).

**Decisions:** D017 (arg shape; the `"copy"` default is deliberately asymmetric with D016's
`NULL` sentinel), narrowed by D018 (GP2 traded on `segment_video`'s audio cut). Local: F5
actioned below the score bar, since AC fencing forbids claiming ungathered evidence.

**Review:** 3 lenses — blame and prior-review clean (M34's F2 lesson applied, not repeated;
RR01 Beyond-1/-3 closed); diff-bug found no functional defect. Fixed F1 (92) CRLF→LF rewrite
of `R/ffmpeg.R` inflating the diff to 4172/3999 from 209/36, F2 (85) cross-reference to a verb
with no audio knob, plus F5/F4/F6 (last two at the user's direction). F3 (45) logged. AC2:
15 defaults diffed vs master — 8 changed by one token, 7 identical, 0 otherwise. CI 9/9.
