# M124: The batch docs teach ffm_jobs(), and four wrong doc statements are corrected

- **Status:** review
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

- [x] AC1: `grep -nE 'tibble\(|tribble\(|data\.frame\(' vignettes/batch.Rmd` returns no line,
      and `vignettes/batch.Rmd` calls `ffm_jobs()` in at least one code chunk that is evaluated
      when the vignette builds.
- [x] AC2: For each `ffm_jobs(` line that `grep -n 'ffm_jobs(' vignettes/*.Rmd README.Rmd`
      lists inside a code chunk, the text between the nearest heading of any level above that
      line and the next heading says the call stops with an error when no file matches rather
      than returning an empty table.
- [x] AC3: `grep -n 'has to accept' R/ffm_jobs.R man/ffm_jobs.Rd` returns nothing. The sentence
      replacing it in `?ffm_jobs`'s details says what an extra jobs column does, naming
      `ffm_batch()` and only those `*_batch()` verbs a test covers, and says that a column named
      like one of a named verb's arguments is read per row. A test asserts that `ffm_batch()`
      stops with R's "unused argument" error on a table with an extra column when `.f` has no
      `...` argument and succeeds when it has one; that each `*_batch()` verb the sentence names
      returns an extra column it does not read identical to the one it was given; and that each
      reads a column named like one of its arguments per row.
- [x] AC4: `grep -n 'current directory' R/program_management.R man/find_ffmpeg.Rd` returns
      nothing; `?find_ffmpeg` says the pre-0.2.0 file is read only when no file exists under
      `tools::R_user_dir("tidymedia", "config")`; and the precedence tests at
      `tests/testthat/test-program-status-and-unset.R:214-243` and
      `tests/testthat/test-program-location-repair.R:96` still pass.
- [x] AC5: `README.Rmd` has a batch example: an evaluated chunk calls `ffm_jobs()` and passes
      its result to `ffm_batch()` or a `*_batch()` verb with `run = FALSE`, and that chunk's
      printed output shows no absolute path. `README.Rmd`'s setup comment describing its
      unguarded chunks agrees with the chunks the file carries. On a machine where ffmpeg,
      ffprobe and mediainfo are all on `PATH`, `Rscript tools/vignette_chunk_guards.R` and
      `Rscript tools/vignette_chunk_program_identity.R` both exit 0.
- [x] AC6: On a machine where ffmpeg, ffprobe and mediainfo are all on `PATH`, two consecutive
      `devtools::build_readme()` runs write byte-identical `README.md` files (`cmp` of a copy
      saved after the first run against the file after the second exits 0), and
      `grep -nE 'Rtmp|temp_libpath|/var/folders|/tmp/|/Users/|/home/' README.md` finds no line.
- [x] AC7: `devtools::document()` leaves no diff, `devtools::test()` is clean, and
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
- [x] T6: `devtools::document()`, `devtools::test()`, `devtools::check()`.

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
- 2026-09-10: T6 started: `document()` no diff; two `build_readme()` runs `cmp`-identical, path grep empty; first `devtools::check()` stopped unfinished when the claim audit's corrections changed the tree.
- claim audit: 27 claims read, 4 corrected — R/ffm_jobs.R (a column sharing a name with one `ffm_batch()` returns is replaced; test added), vignettes/batch.Rmd (only `crop_video_batch()` named as taking the bare table and as rejecting colliding outputs), README.Rmd (some `*_batch()` verbs take the table as it is, others need a column first).
- 2026-09-10: the corrected claims' re-read went to a second fresh [O] reader rather than the first, because this session has no way to message a finished subagent.
- 2026-09-10: re-read of the 4 corrected claims: 2 held (README, batch.Rmd bare-table sentence); 2 still overstated and fixed without a further pass (stopping rule): `?ffm_jobs` now claims replacement only for `command` (only `command` is replaced on every call; `success`/`verified` only when `run`/`verify` write them), test renamed to match; batch.Rmd's auto-naming sentence names `crop_video_batch()` only (five verbs auto-name, `extract_audio_batch()` refuses). Second `devtools::check()` stopped unfinished for the same reason as the first.
- 2026-09-10: T6 done on `24d1810`: `document()` no diff, `ffm-jobs` tests 161 pass, `devtools::check()` 0 errors / 0 warnings / 0 notes (7m30s, tests included); README.Rmd unchanged since its last `cmp`-identical rebuild pair. Status → review.
- 2026-09-10: review: AC1-AC7 verified with fresh evidence and ticked; consistency gate clean; three fresh reviewers returned 6 findings (all from the diff lens), none showing a criterion failing; triage pending at the merge gate.
- 2026-09-10: merge gate: O1 and O4 fixed on the branch and re-checked; O2, O3, O5, O6 rejected with reasons in Review.
- step-7 approval: m124-batch-docs-teach-ffm-jobs approved for merge

## Decisions

## Review

Evidence gathered 2026-09-10 on `edeab25` (branch contains `origin/master`; no sync needed).

- AC1: `grep -nE 'tibble\(|tribble\(|data\.frame\(' vignettes/batch.Rmd` exits 1. `ffm_jobs()` is called in the chunks at batch.Rmd:45 and :72, both plain `{r}` with no `eval` option, and the setup chunk sets only `collapse` and `comment`. Knitting batch.Rmd in a scratch folder against `load_all()` printed both chunks' results: a 1 × 3 tibble from `ffm_batch()` and a 1 × 3 tibble from `crop_video_batch()`, each with input `sample.mp4`.

- AC2: a script over `git show <ref>:<file>` lists `ffm_jobs(` lines inside fenced chunks and searches the prose between the nearest heading above and the next heading for "stops with an error". HEAD: batch.Rmd:46 and :74, metadata.Rmd:124, workflow.Rmd:40, README.Rmd:216 and :217 all pass. master: metadata.Rmd:123 and workflow.Rmd:40 fail, so the script discriminates. workflow.Rmd has no markdown heading above :40 (its YAML title is the page heading); the sentence is at :55, before `## 1.`, so every start point above :40 includes it.
- AC3: `grep -n 'has to accept' R/ffm_jobs.R man/ffm_jobs.Rd` exits 1. The new details sentence names `ffm_batch()`, `crop_video_batch()` and `extract_audio_batch()` (both verbs covered by the new tests) and says an argument-named column is read row by row. `test-ffm-jobs.R`: 27 tests, 161 expectations, 0 failed; the tests assert R's "unused argument (notes = " message without `...` and success with it, an unread factor column returned identical by both verbs, and per-row `width` / `audio_codec` in each command. Plants in a scratch copy each turned exactly its own test red: `ffm_batch()` filtering columns to `.f`'s arguments (unused-argument test), `crop_video_batch()` dropping a `width` column (per-row test), `crop_video_batch()` coercing factors (unchanged-column test).
- AC4: `grep -n 'current directory' R/program_management.R man/find_ffmpeg.Rd` exits 1. `?find_ffmpeg` (R/program_management.R:133-135, man/find_ffmpeg.Rd:32-34) says the pre-0.2.0 file is read only when no file for the program exists under `tools::R_user_dir("tidymedia", "config")`. `test-program-status-and-unset.R`: 17 tests, 201 expectations, 0 failed, 1 skipped (the all-four-programs `program_status()` test, ffplay absent; outside :214-243, whose tests at :215 and :225 pass). `test-program-location-repair.R`: 13 tests, 195 expectations, 0 failed.
- AC5: README.Rmd:215-221 is a plain `{r}` chunk calling `ffm_jobs(".", type = "video")` and passing the table to `crop_video_batch(..., run = FALSE)`; its printed output in README.md:221-224 is a 1 × 3 tibble with input `sample.mp4`, output `sample_cropped.mp4` and a relative command, no absolute path. The setup comment (README.Rmd:14-22) says five chunks besides setup carry no guard (library, clip copy, builder, `extract_audio()`, batch example); `tools/vignette_chunk_guards.R` lists README chunks 2-6 unguarded and 7-9 guarded by `has_ffprobe`/`has_mediainfo`/`has_ffmpeg`, exit 0, "unguarded spawning chunks: none". `tools/vignette_chunk_program_identity.R` exit 0, "chunks starting a program their guard does not name: none". ffmpeg, ffprobe, mediainfo all at `/opt/homebrew/bin`.
- AC6: two consecutive `devtools::build_readme()` runs, both exit 0; `cmp` of a copy saved after the first against README.md after the second exits 0; `grep -nE 'Rtmp|temp_libpath|/var/folders|/tmp/|/Users/|/home/' README.md` exits 1; `git status --porcelain` empty afterwards (README.md also identical to the committed file).
- AC7: `devtools::document()` exit 0, `git status --porcelain` empty. `devtools::check()`: 0 errors, 0 warnings, 0 notes (7m48s; tests OK, vignettes re-built OK). `devtools::test()` run alone: FAIL 0, WARN 12, SKIP 5, PASS 13427. A first `test()` run, made while the plant runs, a scratch knit and a reviewer's R sessions were running, had FAIL 5: three in `test-parallel-option-carry.R` (audio-track check "timed out ... after 1 second") and two in `test-runtime-timeout.R` (partial output file missing); neither file nor the code under them is in the diff.

Consistency gate: `cairn_validate.py` all checks passed, exit 0 (no principle changed, so no impact report); `pkgdown::check_pkgdown()` "No problems found"; `git diff --diff-filter=A master...HEAD` adds no file, so no `.Rbuildignore` entry is owed; `document()` no diff and README.md in sync (AC6, AC7); no NEWS entry, per the plan gate and D091 — both the `ffm_jobs()` and config-directory entries sit under NEWS.md's development heading, so neither corrected sentence shipped in 0.1.0, and the `ffm_jobs()` entry does not repeat the replaced wording. `document()` ran under installed roxygen2 8.0.0 against `Config/roxygen2/version: 8.1.0`; it warns and still writes (a planted roxygen change in a scratch copy regenerated `man/find_ffmpeg.Rd`).

Independent review (three fresh reviewers). Prior-review lens: no regressions; no GitHub review comments exist, and the archived items on these files (M115 O9, M121 O4, M123's `?find_ffmpeg` item) are ones this branch fixes. Blame-history lens: no findings. Diff-bug lens, ranked:

- O1: `vignettes/batch.Rmd:72-80` — `crop_video_batch(ffm_jobs(folder, ...))` auto-names its output beside the input, in the package's `extdata` folder; a reader who drops `run = FALSE` as the page says writes `sample_cropped.mp4` into the R library, or fails where it is read-only. Confirmed: the scratch knit printed an `/Users/…` output; master's example gave relative output names.
- O2: `R/ffm_jobs.R:12-13` — "a column `.f` has no argument for stops the batch" misses R's partial argument matching: a column `outp` binds to `output` with no error. Confirmed with `do.call()`.
- O3: `README.Rmd:183-184, :216` — pasted by a reader, the example copies `sample.mp4` into their working folder, `ffm_jobs(".")` lists every video already there, and `file.copy()` (hidden by `invisible()`, `overwrite = FALSE`) silently keeps an existing `sample.mp4`.
- O4: `tests/testthat/test-ffm-jobs.R:3` — header comment still says "the one ffm_batch() call"; there are now three. Confirmed.
- O5: `R/ffm_jobs.R:13-21` — the new details sentence runs about 85 words with a dash aside holding a colon clause; "per-row" sits alone on a roxygen line.
- O6: `README.Rmd:18` — one setup-comment line runs to about 100 characters where its neighbours wrap at 80.

Triage at the merge gate (2026-09-10), no finding showing a criterion failing, so no status change:
- O1 fix now: the crop example builds `jobs` with `ffm_jobs()` and adds an `output` column of bare file names; the prose says outputs are otherwise named after the input in its own folder, confirmed by an executed `crop_video_batch()` call whose output shares the input's directory. Re-checked after the fix: AC1 grep empty, AC2 script passes at batch.Rmd's chunk lines, scratch knit prints a relative output (`sample_cropp…`, truncated by tibble), `test-ffm-jobs.R` passes (27 tests, 161 expectations).
- O4 fix now: header comment reads "the ffm_batch() calls".
- O2 reject: an edge of R's own partial argument matching, not tidymedia behaviour; the `...` advice holds either way.
- O3 reject: the scratch-folder README approach was chosen at the implement question gate, and the example runs with `run = FALSE`.
- O5 reject: readability only, and the sentence's claims passed two audits; rewording risks reopening them.
- O6 reject: cosmetic line length in a comment that does not render.
