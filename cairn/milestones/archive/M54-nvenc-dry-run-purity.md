# M54: Correct the `run = FALSE` purity claim for the nvenc encoder probe

**Status:** done (2026-08-07, PR #57 https://github.com/jmgirard/tidymedia/pull/57)

**Goal:** Make the package's stated purity contract true by recording that resolving
`hardware = "nvenc"` probes FFmpeg while building the pipeline, `run` notwithstanding.

**Outcome:** D034 supersedes D024's `run = FALSE` bullet, restating the rule as a condition
on probe shape — a probe whose result enters the compiled command runs when the pipeline is
built, D013's shape; `DESIGN.md`'s Conventions bullet matches. All 16 `hardware`-bearing Rd
topics state the probe, conditioned on re-encoding, four naming the stream-copy abort,
guarded by `test-nvenc-docs.R` from an Rd-derived topic list. The three purity tests gained
live nvenc cases counting `ffmpeg_encoders()`, replacing two nvenc-excluding comments, and
`call = call` at `R/ffmpeg.R:1143,1407` makes `standardize_video()`/`format_for_web()` blame
themselves. `helper-rd.R` gives M51's guard and this one one Rd reader.

**Decisions:** D034 (promoted). Two plan-gate calls: correct the record rather than make the
probe lazy, the only lazy seam being `ffm_finish()`/`ffm_batch()`, which needs the pipeline
hook D024/RR02 Q3 rejected; and leave the per-row re-probe cost out — ROADMAP candidate row.

**Review:** Three rounds, two defect returns, all on false prose about measured behavior:
round 1 on a NEWS `_batch` blame fix that never happened and an unconditional roxygen
sentence (both 92); round 2 on a `picture_in_picture_batch()` exclusion whose wrong column
name made a schema error read as correct blame (D1, 94) — a trap round 3 then found
surviving in an adjacent test control (F1, 86), fixed on the branch. Nine logged below.
