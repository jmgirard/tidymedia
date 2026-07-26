# M32: Batch siblings for the fan-in verbs (`concatenate_videos`/`compare_videos`/`picture_in_picture` `_batch`)

**Status:** done (2026-07-26, PR #34 https://github.com/jmgirard/tidymedia/pull/34)

**Goal:** Add table-driven `_batch` siblings for the three fan-in (many-inputs → one-output) verbs, completing the batch-coverage family after M28 (single-io) and M29 (fan-out).

**Outcome:** Three new exported verbs — `concatenate_videos_batch()`, `compare_videos_batch()`, `picture_in_picture_batch()` — as thin `ffm_batch()` wrappers, each sharing an extracted pipeline helper (`concatenate_pipeline` / `compare_videos_pipeline` / `picture_in_picture_pipeline`) with its scalar sibling. Jobs-table shape per D015: concat/compare take an `inputs` list-column, PiP takes fixed `main`/`overlay` columns; optional per-row override columns (`direction`/`resize`/`audio`; `position`/`scale`/`margin`/`audio`) fall back to the args. No `ffm_batch` engine change: `purrr::pmap` passes list-columns row-wise and the manifest already `";"`-joins multi-input. New `check_fanin_jobs()` validator. Every task verb now has a `*_batch()` companion.

**Decisions:** D015 (fan-in batch input-shape, extends D007).

**Review:** 3 lenses + scorer. Fixed F1 (cli-pluralization crash in `check_fanin_jobs`, M18 regression, 88), F2 (PiP `margin` column bypassed the scalar's whole/`min=0` guard, 80), F3 (PiP rejected documented logical `audio=NA`, 74); F4 (no `check_file_exists`, 15) rejected as intentional batch convention. +5 regression tests; devtools::check() 0/0/0.
