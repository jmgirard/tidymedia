# M142: Two `ffm_batch()` refusals and the direct-command functions stop with their own message instead of an R error

**Status:** done (2026-10-18, PR #146 https://github.com/jmgirard/tidymedia/pull/146)

**Goal:** Callers who trip two `ffm_batch()` refusals, or who run a direct-command or capability-listing function with no program found, get tidymedia's own message in place of an R error that names nothing.

**Outcome:** `ffm_batch()`'s not-a-pipeline and invalid-`verify`-spec refusals (`R/ffm_batch.R`) put
`{cli::qty()}` before `Job{?s}`, so two or more bad jobs no longer stop cli with "length(object) == 1 is not TRUE",
and one bad job at row 2 reads "Job 2". `run_program()`'s refusal moved into `tm_require_location(location, program,
call)`, which `ffmpeg()`, `ffprobe()` and `mediainfo()` now call on their lookup, outside `guard_timeout()`, so a
missing program stops with "Could not locate FFmpeg." (or FFprobe, MediaInfo) and the lookup's warning keeps its
class. `ffmpeg_codecs()` and `ffmpeg_encoders()` keep calling `ffmpeg()`, the seam 33 test mocks replace, and
`tm_reblame_missing(expr, call)` raises that refusal again under their own frame, matched by the internal
`tm_missing_program` field (no class). The help text is one `missing_program_sentences()` in `R/task-doc.R` on five
pages. Two NEWS entries. Tests in `test-missing-program-refusal.R` and `test-ffm-batch.R`.

**Decisions:** none promoted. The plan chose reusing `run_program()`'s unclassed message over a new condition class.

**Review:** One three-lens pass, no returns, no amendments. Criteria audit (full) took 6 findings; claim audit read 37
claims and corrected 6. Review: 19 findings, 9 fixed on the branch (among them a timeout test that would have failed
on a machine without FFmpeg, found by both code lenses and confirmed by a no-binaries suite run), 9 rejected, 1
already in the "Argument classes refused below the verb" row. CI: all R and pkgdown checks green; merged past red
Codecov patch and project checks at the user's choice, the 8 misses being `R/task-doc.R`'s document-time lines.
Graduated the `ffm_batch()` two-refusals row and narrowed the "Unclassed aborts" row; no lesson retired.
