# M29: Batch sibling for separate_audio_video (fan-out) — done

**Goal:** Give `separate_audio_video` a table-driven `_batch` sibling, completing
batch coverage of the fan-out verbs (D001/D007).

**Outcome:** Added `separate_audio_video_batch()` — a thin `ffm_batch()` fan-out
(IP1/D007). Each input row (`input`/`audiofile`/`videofile`) reshapes into **2N**
single-output rows (one `audio`, one `video`) with a `stream` marker; commands
are byte-identical to the scalar via an extracted per-stream
`separate_stream_pipeline()`. Reuses M28's `check_batch_jobs`; a single
`reject_duplicate_outputs()` on the melted `output` column pools cross-column
collisions and catches within-row `audiofile == videofile`. 41 new tests;
`check()` 0/0/0; CI green all platforms.

**Key decisions (plan gate):** return is 2N rows + `stream` marker (models the
`segment_video` *scalar*'s reshape, since `ffm_batch` is 1-row→1-command, not
`segment_video_batch`); both output columns **required** (no derivation) for
scalar parity — a copy-safe-container derivation call was rejected.

**Review:** all 5 ACs passed on fresh evidence; three fresh-context reviewers →
zero findings (one cosmetic obs logged: dead `verb=` label, not actioned).

**PR:** https://github.com/jmgirard/tidymedia/pull/31 · **Out →** composite/fan-in
verb batch (`concatenate_videos`/`compare_videos`/`picture_in_picture`) remains a
ROADMAP candidate (input-shape design call).
