# M68: A failed run removes the broken output it wrote

**Status:** done (2026-08-09, PR #71 https://github.com/jmgirard/tidymedia/pull/71)

**Goal:** An FFmpeg run that fails leaves no output file behind.

**Outcome:** `ffm_run()` (`R/ffm.R`) snapshots the files its output designates
before running — `output_targets()` / `output_snapshot()`, path, size, epoch
mtime — and after a non-zero exit `remove_failed_output()` removes only what
the run created or changed, naming it in the abort: an output FFmpeg never
opened survives byte-for-byte, the zero-byte truncation goes.
`unlink(expand = FALSE)` stops an output named `a*.mp4` taking neighbours, and
an image2 `%0Nd` pattern is matched as an escaped regex over its own directory,
so a failed `sample_frames()` run loses its own frames only. `overwrite = FALSE`
keeps its own guard. Not reached: `ffmpeg()`, loudnorm's analysis pass, `verify =`.

**Decisions:** D046 — remove what the run wrote, not what it found; supersedes
D045's unconditional half, keeps its not-a-probe reasoning. Both written here,
D045 before review refuted it.

**Review:** Two rounds. Round 1 returned it: F1 (92) deleted an output FFmpeg
never opened, F2/F3 (90/88) `unlink()` globbing, F6 (84) frames left, P1 (85)
an outcome-keyed skip, F10 (80) the blindness hiding F1. Round 2 scored 34,
actioned F20 (82, the mtime half untested), fixed F1/F21/F22/F27 beside it,
logged 29. Extended M31's fixture-gate lesson at hygiene.
