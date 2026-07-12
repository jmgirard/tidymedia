# M09: Dataframe-driven batch segmentation (done 2026-07-12)

**Goal.** A Layer-2 verb cutting segments across many input files from one
jobs tibble, sharing its cut logic with `segment_video()`.

**Outcome.** Shipped:
- `segment_videos(jobs, reencode = TRUE, run = TRUE, parallel = FALSE, ...)` —
  `jobs` carries `input`/`output`/`start`/`end` (one row per segment, any number
  of input files); returns the `ffm_batch()` tibble. `...` forwards to
  `ffm_batch()` so `verify`/`manifest`/`checksums`/`progress` work.
- Internal `segment_pipeline()` helper (`ffm_seek` + optional `ffm_copy`) now
  shared by `segment_videos()` and `segment_video()` (byte-for-byte refactor of
  the latter's inline `.f`; no engine/`ffm_batch` change — D007).
- Column-contract validation via `cli::cli_abort()` (non-df/empty/missing col).

**Key decisions.** Plural name mirrors `concatenate_videos`; columns
`input/output/start/end` match `ffm_batch` convention; `reencode` scalar-only
(per-row deferred). No milestone-local D-entries.

**Evidence.** `test-segment-videos.R` 25/25 (incl. binary-gated verify/manifest
forwarding); `test-ffmpeg.R` 80/80 unchanged; `check()` clean; CI green all
platforms. 2 fresh-context reviewers + scorer: F1 (weak test) fixed; F3
(start/end type validation) → candidate; F2 rejected by-design.

**PR.** https://github.com/jmgirard/tidymedia/pull/10
