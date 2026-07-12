# M13: Batch video standardization verb — done 2026-07-12

**Goal:** add `standardize_videos()`, a tibble-driven fan-out over
`standardize_video()` via `ffm_batch()` (D007), mirroring
`segment_videos()`/`extract_frames()`.

**Outcome (PR #15):** new exported verb `standardize_videos(jobs, width,
height, fps, vcodec, pixel_format, run, parallel, ...)` — required `input`
column; optional `output` (auto-named `<base>_standardized.<input-ext>`, with a
duplicate-input collision abort); all five knobs recognized as per-row override
columns (scalar arg = default); `...` forwards `ffm_batch()` options. Refactor:
extracted a shared `standardize_pipeline()` from `standardize_video()`'s inline
body, so batch commands are byte-identical to the scalar verb and inherit M12's
guards (`-c:a copy`, even-dimension floor-crop, `+faststart`) and per-value
validation by construction. Full suite 563 pass; check 0/0/0; CI green (7/7).

**Key decisions (plan gate):** `_standardized`-suffix auto-naming (over
sibling-style `_<n>` numbering) with a collision abort; all five knobs
overridable per row. None cross-cutting → no DECISIONS promotion.

**Review:** all 8 acceptance criteria verified fresh; two independent reviewers
(Opus diff-bug, Sonnet blame-history) — zero findings. Consistency gate fixed
two doc gaps on-branch (NEWS + `_pkgdown.yml` reference).
