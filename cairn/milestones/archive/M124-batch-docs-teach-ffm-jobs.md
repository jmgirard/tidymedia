# M124: The batch docs teach ffm_jobs(), and four wrong doc statements are corrected

**Status:** done (2026-09-11, PR #128 https://github.com/jmgirard/tidymedia/pull/128)

**Goal:** The batch docs start from `ffm_jobs()`, and README.md, `?ffm_jobs`, `?find_ffmpeg` and
the vignettes stop stating things that are false or machine-specific today.

**Outcome:** `vignette("batch")` builds both jobs tables with `ffm_jobs()`; its crop example adds
an `output` column of bare names so a real run writes to the working directory. Every `ffm_jobs()`
chunk in the vignettes and README says the call stops with an error when no file matches.
`?ffm_jobs` says an extra column stops `ffm_batch()` with "unused argument" unless `.f` takes
`...`, and that `crop_video_batch()`/`extract_audio_batch()` keep an unread column and read an
argument-named one per row (tests in `test-ffm-jobs.R`). `?find_ffmpeg` says the pre-0.2.0 file is
read only when none exists under `tools::R_user_dir()`. README.Rmd gains a batch example and knits
in a scratch folder, so `build_readme()` is byte-identical run to run with no local path. No NEWS
entry (D091). Absorbed candidate items: `ffm_jobs()` (d)/(e), shipped docs (l)/(m), remembered location (c).

**Decisions:** plan gate replaced batch.Rmd's hand-built tables, folded in the README path fix, and
skipped NEWS; implement gate knit README in a scratch folder. No DECISIONS.md entry.

**Review:** three-lens fan-out, 0 returns; [O] raised 6, the other two lenses none. Fixed: crop
example output in the installed package; stale test header. Rejected: partial-argument-matching
edge, README paste into a working folder, two style items. A `test()` run under concurrent R load
failed 5 timing tests, 0 alone. CI 10/10 after one timed-out wait. LESSONS: README temp-path clause
trimmed as fixed; one line added on running the suite unloaded.
