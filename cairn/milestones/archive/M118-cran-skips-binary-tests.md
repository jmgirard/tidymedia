# M118: The binary-executing tests skip on CRAN's own check

**Status:** done (2026-09-09, PR #122 https://github.com/jmgirard/tidymedia/pull/122)

**Goal:** CRAN's own submission check runs none of the tests that spawn FFmpeg,
FFprobe or MediaInfo, while every other run of the suite still does.

**Outcome:** `testthat::skip_on_cran()` runs first in all five `skip_if_no_*()`
helpers in `tests/testthat/helper-skip.R` — including the two hardware-probe
ones, whose one-frame probe encode is itself a spawn. Three sites reaching a
binary outside any helper were closed too: a hand-rolled guard replaced by the
shared helper, and two `test-normalize-audio-batch.R` cases mocked to
`find_ffprobe = NULL`. `tools/cran_spawn_check.R` is the standing instrument —
PATH stand-ins that log then exec, driven through `R CMD INSTALL` plus
`test_check()`. Measured: 1226 spawns ordinarily against 0 under CRAN
conditions, skipped-test set unchanged, `R CMD check --as-cran` 7m47s to 4m33.2s.

**Decisions:** the measurement cannot run under `devtools::test()` or
`testthat::test_local()`, which force `NOT_CRAN="true"` inside the run; the
emptied-`PATH` mode is UNINSTRUMENTED, not evidence. Promoted: **D090**.

**Review:** three passes, three lenses each. No defect returns; three amendment
returns (AC1, AC3, AC4), each once, every one a criterion quantifying past what
its procedure could settle. Of sixteen findings in the last pass six were
repaired on the branch, one rejected, the rest went to a candidate row.
