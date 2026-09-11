# M124: The batch docs teach ffm_jobs(), and four wrong doc statements are corrected

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — the vignettes, README and help pages are what readers of the package use
- **Branch/PR:** m124-batch-docs-teach-ffm-jobs

## Goal

The batch docs start from `ffm_jobs()`, and README.md, `?ffm_jobs`, `?find_ffmpeg` and the
vignettes stop stating things that are false or machine-specific today.

## Scope

**In:**
- `vignettes/batch.Rmd` builds both jobs tables (`:39`, `:64`) with `ffm_jobs()` over the
  package's sample folder instead of `tibble::tibble()`, and names it where it introduces
  `ffm_batch()`.
- Every chunk calling `ffm_jobs()` — `workflow.Rmd:40`, `metadata.Rmd:123` (called "a
  one-liner"), and the new ones — has prose saying the call stops with an error when no file
  matches, where the `list.files()` it replaced returned `character(0)`.
- `?ffm_jobs`'s details (`R/ffm_jobs.R:10-16`): the reason for the one-column table says an
  extra column "would become an argument every `.f` has to accept", where `?ffm_batch`
  (`R/ffm_batch.R:13-15`) gives the `...` remedy. Rewritten from executed calls.
- `?find_ffmpeg` (`R/program_management.R:133-134`) says the pre-0.2.0 file is read "when no
  file exists in the current directory"; `find_program()` (`:49-55`) reads it when none exists
  under `tools::R_user_dir()`.
- `README.Rmd` gains a batch example (`ffm_jobs()` into a batch runner, `run = FALSE`).
- README.md stops embedding a per-session install path: `README.md:193,201` print a
  `temp_libpath…` path today, so every `build_readme()` rewrites them and a local path ships.

**Out:**
- The other `ffm_jobs()` items — duplicate outputs, dotfiles, extension-list gaps, Windows
  links and hidden files, symlink escape: stay on their candidate row.
- The other shipped-docs gaps — `Sys.which()` guards vs `find_program()`, the Layer 1 tour's
  missing verbs, the capability family, the guard-tool hygiene items: stay on their row.
- The two remembered-location read defects in `find_program()` itself: stay on their row.
- A NEWS entry: none (plan gate). No function changes behaviour; `ffm_jobs()` is new this
  cycle, so no released wording is corrected (D091).
- A test over every `*_batch()` export: not planned; `?ffm_jobs` names only the verbs a test
  covers.

## Acceptance criteria

- [ ] AC1: `grep -nE 'tibble\(|tribble\(|data\.frame\(' vignettes/batch.Rmd` returns no line,
      and `vignettes/batch.Rmd` calls `ffm_jobs()` in at least one code chunk that is evaluated
      when the vignette builds.
- [ ] AC2: For each `ffm_jobs(` line that `grep -n 'ffm_jobs(' vignettes/*.Rmd README.Rmd`
      lists inside a code chunk, the text between the nearest heading of any level above that
      line and the next heading says the call stops with an error when no file matches rather
      than returning an empty table.
- [ ] AC3: `grep -n 'has to accept' R/ffm_jobs.R man/ffm_jobs.Rd` returns nothing. The sentence
      replacing it in `?ffm_jobs`'s details says what an extra jobs column does, naming
      `ffm_batch()` and only those `*_batch()` verbs a test covers, and says that a column named
      like one of a named verb's arguments is read per row. A test asserts that `ffm_batch()`
      stops with R's "unused argument" error on a table with an extra column when `.f` has no
      `...` argument and succeeds when it has one; that each `*_batch()` verb the sentence names
      returns an extra column it does not read identical to the one it was given; and that each
      reads a column named like one of its arguments per row.
- [ ] AC4: `grep -n 'current directory' R/program_management.R man/find_ffmpeg.Rd` returns
      nothing; `?find_ffmpeg` says the pre-0.2.0 file is read only when no file exists under
      `tools::R_user_dir("tidymedia", "config")`; and the precedence tests at
      `tests/testthat/test-program-status-and-unset.R:214-243` and
      `tests/testthat/test-program-location-repair.R:96` still pass.
- [ ] AC5: `README.Rmd` has a batch example: an evaluated chunk calls `ffm_jobs()` and passes
      its result to `ffm_batch()` or a `*_batch()` verb with `run = FALSE`, and that chunk's
      printed output shows no absolute path. `README.Rmd`'s setup comment describing its
      unguarded chunks agrees with the chunks the file carries. On a machine where ffmpeg,
      ffprobe and mediainfo are all on `PATH`, `Rscript tools/vignette_chunk_guards.R` and
      `Rscript tools/vignette_chunk_program_identity.R` both exit 0.
- [ ] AC6: On a machine where ffmpeg, ffprobe and mediainfo are all on `PATH`, two consecutive
      `devtools::build_readme()` runs write byte-identical `README.md` files (`cmp` of a copy
      saved after the first run against the file after the second exits 0), and
      `grep -nE 'Rtmp|temp_libpath|/var/folders|/tmp/|/Users/|/home/' README.md` finds no line.
- [ ] AC7: `devtools::document()` leaves no diff, `devtools::test()` is clean, and
      `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T3
- AC2 → T3, T4, T5
- AC3 → T1
- AC4 → T2
- AC5 → T5
- AC6 → T5
- AC7 → T6

## Tasks

- [x] T1: `?ffm_jobs` details. Run `ffm_batch()` on a table with an extra column, `.f` with and
      without `...`, and the chosen `*_batch()` verbs (`crop_video_batch()` and
      `extract_audio_batch()`) on an unread extra column and on an argument-named one; write the
      sentence from those outputs (`R/ffm_jobs.R:10-16`); add the tests (C locale is set by
      testthat; `test-ffmpeg.R:200` already matches "unused argument"); `devtools::document()`.
- [x] T2: `?find_ffmpeg`. Rewrite `R/program_management.R:133-134` against `:49-55`, confirmed by
      running the two precedence tests; `devtools::document()`.
- [x] T3: `vignettes/batch.Rmd`. Build both tables with
      `ffm_jobs(system.file("extdata", package = "tidymedia"), type = "video")` (one row:
      `sample.mp4`), adding `output` by assignment where a verb needs it; name `ffm_jobs()` in
      "The batch runner"; add the no-match sentence. Render in place from `vignettes/`
      (LESSONS: `build_vignettes()` knits in a copy and cannot see a chunk writing files).
- [x] T4: `vignettes/workflow.Rmd:40` and `vignettes/metadata.Rmd:123`: the no-match sentence
      beside each call, and drop "a one-liner" where it hides that stop.
- [x] T5: `README.Rmd`. Add the batch example under `## Examples`; make it, the `ffm_compile()`
      and `extract_audio()` chunks, and `probe_all()`'s `file` column print no absolute path;
      update the setup comment (`:14-20`); add the no-match sentence. Run `build_readme()` twice
      with `cmp` and the grep, then both `tools/` chunk scripts with all three binaries present.
- [ ] T6: `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Work log

- 2026-09-10: created by /milestone-plan, from the docs-pass pick at the plan's opening question; absorbs `ffm_jobs()` row items (d)/(e), shipped-docs row items (l)/(m), and remembered-location row item (c).
- 2026-09-10: criteria audit (full mode, [O] fresh reader) pass 1 returned 11: 6 fixed (AC2 domain to chunk lines and pre-heading text; AC3 claims narrowed to covered verbs, unread columns, and stated test assertions; AC4 cites the existing precedence tests), 2 posed at the gate (README path fix, AC6 shape), 3 clean.
- 2026-09-10: criteria audit (full mode, fresh [O]) pass 2 on the gate-revised wording returned 9: 6 fixed (AC5 no printed absolute path and setup-comment agreement; AC6 grep widened past macOS truncation and to clone paths; AC3 argument-named column half; AC2 heading of any level), 3 clean.
- 2026-09-10: plan gate chose replacing batch.Rmd's hand-built tables with `ffm_jobs()` over adding an `ffm_jobs()` section beside them, because two ways of building the table side by side teach the hand-built one first; falsified by a reader who needs a jobs table not derived from a folder and finds no example of one.
- 2026-09-10: plan gate chose folding the README temp-path fix in over leaving the two lines, because the new batch example would add more of them; falsified by the fix leaving README examples that no longer show a command a reader can follow.
- 2026-09-10: plan gate chose no NEWS entry over a one-line docs bullet, because no function changes behaviour and `ffm_jobs()` is new this cycle (D091); falsified by a 0.1.0 reader acting on the old `?find_ffmpeg` sentence.
- 2026-09-10: plan chose naming only test-covered `*_batch()` verbs in `?ffm_jobs` over a test looping every `*_batch()` export, as proportionate to a docs milestone; falsified by an uncovered verb dropping or reshaping an extra column a reader expected kept.
- 2026-09-10: implement started on branch `m124-batch-docs-teach-ffm-jobs`; question gate chose building the README examples in a scratch folder holding a copy of the sample clip, with the batch example cutting `ffm_jobs()`'s `input` to the file name on a visible commented line, over printing only the output column or a hidden path-stripping output hook.
- 2026-09-10: T1 verb pair changed from `strip_metadata_batch()`/`crop_video_batch()` to `crop_video_batch()`/`extract_audio_batch()` (minor task edit): measured, `strip_metadata_batch()` has no per-row argument, so it cannot back the argument-named half of AC3.
- 2026-09-10: checkpoint, half-done — T1-T5 edits written, none ticked: `ffm-jobs` tests 157 pass; batch.Rmd rendered in place; two `build_readme()` runs `cmp`-identical with the path grep empty; both `tools/` chunk sweeps exit 0; an AC2 section check passes here and fails on master. Pending: full `devtools::test()` (running), T2 `document()`, T6.
- 2026-09-10: full `devtools::test()` over the checkpoint: FAIL 0, WARN 12 (all in four untouched files), SKIP 5, PASS 13423.
- 2026-09-10: T1 done: `?ffm_jobs` sentence written from executed `ffm_batch()`, `crop_video_batch()`, `extract_audio_batch()` calls; three tests added to `test-ffm-jobs.R`; "has to accept" grep empty.
- 2026-09-10: T2 done: `?find_ffmpeg` names `tools::R_user_dir("tidymedia", "config")`, read against `find_program()`; `document()` rewrote only that sentence; the two precedence test files pass (402).
- 2026-09-10: T3 done: batch.Rmd builds both tables with `ffm_jobs()` over the sample folder (one row, `sample.mp4`), rendered in place from `vignettes/`; no-match stop confirmed by an executed call on a folder holding only a `.wav`.
- 2026-09-10: T4 done: no-match sentence beside workflow.Rmd's and metadata.Rmd's calls; "a one-liner" replaced.
- 2026-09-10: T5 done: README examples knit in a scratch folder holding a copy of the clip; batch example via `crop_video_batch()`; setup comment counts the five unguarded chunks the guard sweep lists besides setup.

## Decisions

## Review
