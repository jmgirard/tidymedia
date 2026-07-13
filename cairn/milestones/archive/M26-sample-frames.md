# M26: Fixed-rate frame sampling (`sample_frames` + `_batch`) — done

**Goal:** Add task verbs that sample a video at a fixed rate/interval into a
numbered image sequence — the front door to per-frame coding and CV feature
pipelines (M25 survey K1).

**Outcome:** Shipped `sample_frames(infile, outdir, fps=/interval=, format=,
prefix=)` and `sample_frames_batch(jobs, …)` — thin Layer-2 wrappers (IP1) over
`ffm_files`→`ffm_fps`→`ffm_output_options`, emitting a **single** `image2`-muxer
command whose output is a synthesized `<prefix>_%06d.<fmt>` printf pattern that
FFmpeg fills — consistent with IP2 (one output target), distinct from the D007
fan-out (a fixed-rate sample can't enumerate frames before decoding). `interval`
→ reciprocal frame rate. Shared `sample_frames_pipeline()` gives the batch
per-row checks for free (M13). Plan-gate calls: output-dir + auto-pattern (no
user-facing printf), dual `fps`/`interval` arg, batch sibling included.

**Review:** all 6 ACs passed on fresh evidence; suite 923 pass/0 fail; check
0/0/0; CI green (7 jobs). Independent diff-bug review fixed **F1** — batch now
aborts on colliding auto-derived output patterns (duplicated / same-basename
inputs under one `outdir`) instead of silently overwriting frames — and **F3**
(numeric-only `interval` column). **F2** (`ensure_dir` on `run=FALSE`) logged as
accepted documented behavior. Blame-history + prior-PR lenses clean.

**PR:** https://github.com/jmgirard/tidymedia/pull/28
