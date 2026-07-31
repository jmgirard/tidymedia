# M48: Carry the track selector to `crop_video()` and `segment_video()`, and make `ffm_copy()` idempotent again

**Status:** done (2026-07-30, PR #51 https://github.com/jmgirard/tidymedia/pull/51)

**Goal:** Put `crop_video()`/`segment_video()` (+ `_batch`) on M47's map rule, and stop a repeated `ffm_copy()` duplicating every output stream.

**Outcome:** `audio_stream` on `crop_video()`, `segment_video()` and both `_batch`
siblings via `pass_through_maps()`: `-map 0:v? -map 0:a?` unset, `-map 0:v?
-map 0:a:<n>` named, on all three paths. Subtitle/data carriage ends, fixing
`crop_video()` exit 8 into `.mp4`. `segment_pipeline()` narrows with
`ffm_map(replace = TRUE)`, kept *after* its `ffm_copy()` call or the new guard
fires. `ffm_copy()` assigns its map; `check_copy_map_conflict()` aborts
(`tidymedia_copy_map_conflict`) on a conflicting one, `ffm_map()` untouched. New
`make_multitrack_subtitle_video()` 5-stream fixture; map-count invariant now an
exact per-verb table over 13 entry points, pinning `format_for_web` /
`normalize_audio` at 0 as a known gap.

**Decisions:** D027 (`ffm_copy()` assigns; conflicting prior map aborts; from
RR03). M48-D1/D2 hold the RR03 triage and the ingest audit's BC6 deviation; D3
records that `segment_video(reencode = TRUE)` emitted no map at all and so had
M47's defect — missed by the plan, D026 and the first NEWS draft.

**Review:** 16/16 criteria, `check()` 0/0/0, CI green. 13 findings; F2 (93, NEWS
denied a real behavior change) and F1 (92, `purrr::pmap` blame leak) fixed, F3
(76) fixed too, 10 logged. T2 escalated to Fable (RB03/RR03), a first here.
