# M03: Task verbs rebuilt on the builder + batch support (done 2026-07-10)

**Goal:** make Layer 2 real — every task verb wraps the Layer 1 `ffm_*` builder
(D002) instead of gluing command strings — and ship the D001 headline: batch
processing over many files.

**Outcome (PR #3, squash-merged as 9d7a72e; CI green ×6):**
- New engine primitives: `ffm_seek()` (seek-based cut, frame-accurate by
  default; opt-in fast copy), `ffm_concat()` (concat-demuxer verb),
  `ffm_output_options()` (controlled passthrough).
- `ffm_batch(jobs, .f, …)`: tibble-in/tibble-out runner (pmap-style columns),
  returns `command` (+ `success`), opt-in furrr, dry-run.
- All 8 task verbs migrated onto the builder; `segment_video` rebuilt on
  batch+seek (returns a tibble), `separate_audio_video` → two single-output
  pipelines, `concatenate_videos` → `ffm_concat`. No command-string gluing left.
- Breaking (pre-release): `extract_audio(acodec=)`; `crop_video` drops `arg`,
  centers x/y; new tibble/vector returns. NEWS → 0.0.0.9002.
- Evidence: test 190 pass / 4 skip; check 0/0/0. Empirically fixed the broken
  copy-cut (was 3.08s + shifted pts for a 4.0s request).

**Key decisions:** D-M03-1..6 → promoted cross-cutting ones to **DECISIONS D007**
(batch model) and **D008** (cutting/seeking, accurate default).
**Deferred (Opus review):** F2 concat tempfile not cleaned (by design); F4
shell-quoting of paths with `"`/`` ` ``/`$` (= M02 F6); `separate_audio_video`
`-c copy` fast path — all tracked engine/verb follow-ups.
