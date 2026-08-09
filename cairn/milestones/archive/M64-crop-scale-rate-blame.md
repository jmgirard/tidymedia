# M64: A crop, scale or rate mistake names the verb the user called, in both forms

**Status:** done (2026-08-08, PR #67 https://github.com/jmgirard/tidymedia/pull/67)

**Goal:** `crop_video`, `standardize_video` and `sample_frames` refuse their own
dimension, rate and pixel-format values at their front doors, naming the verb.

**Outcome:** Front-door sweeps calling the shared checkers: `crop_video()`
width/height/x/y; `standardize_video()` width/height/fps, its `pixel_format`
checked in `standardize_pipeline()` with `call` threaded; per-row sweeps at
`crop_video_batch()` (x/y join M59's), `standardize_video_batch()` (dims +
pixel format) and `sample_frames_batch()` (rate via `resolve_sample_fps()`),
each above `check_nvenc_available()` — a bad batch value now outranks a missing
nvenc encoder, the one reorder (M64-D2). Instruments: `helper-blame-specs.R` +
the blame grid tests; `data-raw/` blame-baseline, blame-precedence (82
crossings, live controls), blame-guard-mutations (diff-derived, 15/15 red).

**Decisions:** D042 promoted — a sweep re-calls the shared checker; `call`
never threads through an exported builder. Milestone-local: M64-D1 the
`format`→`pixel_format` naming fix; M64-D2 reordering table; M64-D3 citations.

**Review:** One gated AC1 amendment (one list consumer, not three). 15 findings,
5 actioned ≥80 all fixed — two stale pipeline-validates comments, a stale
consumer claim, a false NEWS clause, F12's vacuous both-forms normalizer; 10
logged 25–78. Hygiene: F12 extended the grid-vacuity lesson; nothing retired.
