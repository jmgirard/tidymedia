# M46: Stop the subtitle fixture hanging, and bound every fixture command

**Status:** done (2026-07-30, PR #49 https://github.com/jmgirard/tidymedia/pull/49)

**Goal:** Every FFmpeg fixture command in the test suite terminates: the subtitle
fixture stops deadlocking, and any fixture command that hangs fails fast.

**Outcome:** `make_subtitle_video()` in `helper-media.R` owns the subtitle-bearing
command and passes no `-shortest` — measured cause of an intermittent FFmpeg
deadlock (10 hangs in 25 runs, 8.1.2/macOS) needing the flag AND a mapped subtitle
stream; 25/25 clean after, 4 timeouts in 25 before. New test-only
`run_ffmpeg_fixture()` bounds every fixture command via base R `system(timeout=)`,
erroring (never skipping) at the limit; all twelve fixture sites route through it,
`stream_types()` is hoisted from an inline closure, and `ffmpeg()` gained direct
tests since those sites were its only exercise. No `R/` change, no NEWS entry.

**Decisions:** M46-D1 — two sub-threshold review findings (D 72, B 62) were fixed
rather than logged, at the maintainer's direction: the 80 threshold governs what
review actions automatically, not what the maintainer may elect.

**Review:** 3 lenses, 15 findings, 1 return (AC6). Fixed: E 92 (the comment "the
flag changes nothing" was false — `-shortest` tracked the 1 s subtitle, duration
1.021→2.023 s, meeting the plan gate's own never-evaluated falsifier), A 90 (AC6's
fixture-validity *skip* coded as an assertion), D 72, B 62. Eleven logged, ≤45.
