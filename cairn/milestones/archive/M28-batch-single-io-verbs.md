# M28: Batch siblings for single-in/single-out verbs — done

**Goal:** Give the four remaining single-input transform verbs the table-driven
`_batch` sibling every other such verb already had, closing the batch-coverage
gap (D001).

**Outcome:** Added `extract_audio_batch()`, `convert_audio_batch()`,
`crop_video_batch()`, `format_for_web_batch()` — each a thin `ffm_batch()`
fan-out (IP1/D007) sharing an extracted `*_pipeline()` helper with its scalar
verb (M13), so scalar output is byte-for-byte unchanged. Three shared jobs-guard
helpers (`check_batch_jobs`/`reject_duplicate_outputs`/`check_batch_string_col`)
carry the M26 resolved-output duplicate guard. 86 new tests; `check()` OK; CI
green all platforms.

**Key decision (milestone-local):** the audio verbs **require** an explicit
`output` column (its extension picks container/codec, so it can't be auto-named);
the video verbs derive one (`<base>_cropped.<ext>` / `<base>_web.mp4`) —
deliberately unlike `normalize_audio_batch` (which derives for audio), per
roxygen. Per-row override columns follow `standardize_video_batch`'s convention.

**Review:** all 7 ACs passed on fresh evidence; three fresh-context reviewers →
zero actionable findings (one sub-threshold obs fixed: crop schema test).

**PR:** https://github.com/jmgirard/tidymedia/pull/30 · **Out →** fan-out
`separate_audio_video_batch` = M29; composite-verb batch = ROADMAP candidate.
