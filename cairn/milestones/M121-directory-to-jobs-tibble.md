# M121: A directory becomes a jobs tibble

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP1
- **Resolves:** —
- **Surface tier:** user-facing — one new export
- **Branch/PR:** `m121-directory-to-jobs-tibble` — https://github.com/jmgirard/tidymedia/pull/125

## Goal

One export turns a directory of media files into the jobs tibble `ffm_batch()`
takes, so the batch story does not start with a hand-rolled `list.files()`.

## Scope

**In:** one new exported function, its refusals, its reference page and index row,
and the two vignette lines that stop hand-rolling the call.

**Out:** an `output` column convention or a path-deriving helper → not this
milestone; the demonstrated call derives its own. A `_batch` sibling → D014 requires
one for a task verb, and this is a metadata utility that produces the jobs table
rather than consuming it. Resume or skip-existing for long batches → ROADMAP
candidate row. Anything that grows toward FFmpeg feature coverage → GP1, D001.

## Acceptance criteria

- [ ] AC1: A newly exported function returns a tibble carrying an `input` column of
      full paths to the media files in a named directory, selected by a type or
      extension argument, and a demonstrated `ffm_batch()` call consumes that
      tibble's `input` column together with an `output` column derived from it,
      without reshaping the returned object.
- [x] AC2: These three refusals — a directory that does not exist, a type outside the
      accepted set, and a call matching no file — are each raised with the frame of
      the verb the caller typed, per D074 and D087.
- [x] AC3: `_pkgdown.yml` carries a reference-index row for the new export and `man/`
      a topic with a `\value{}` section and a runnable example, both in the commit
      that exports it.
- [x] AC4: `vignettes/workflow.Rmd:40` and `vignettes/metadata.Rmd:121` call the new
      export in place of their hand-rolled `list.files()`; those are the only two
      such calls in `vignettes/` as measured 2026-09-07.
- [x] AC5: `devtools::check()` reports 0 errors and 0 warnings, and the `verify` slot
      of `cairn/PROFILE.md` is clean.

## Coverage

- AC1 → T1, T2, T7, T9, T13, T15
- AC2 → T3, T8, T9, T15
- AC3 → T4, T14
- AC4 → T5, T10
- AC5 → T6, T12, T16

## Tasks

- [x] T1: Settle the export's name and its selector argument's name under D014 and
      D078's full-word compound rule. Note D079: if every candidate default for the
      type argument is one member of the set it ranges over, the argument takes no
      default.
- [x] T2: Implement, returning a tibble with `input` as a full path.
- [x] T3: Add the three refusals, threading `call` through an internal implementation
      per D087 — `tests/testthat/test-exported-call-formal.R` enforces that no export
      publishes the internal `call` formal. Fire every `cli_abort()` branch per the
      profile's test-doctrine, including the argument-form refusals beyond the three
      AC2 names.
- [x] T4: Roxygen, `devtools::document()`, `_pkgdown.yml` row.
- [x] T5: Rewrite the two vignette lines; rebuild.
- [x] T6: `devtools::test()`, `devtools::check()`, `pkgdown::check_pkgdown()`.
- [x] T7: Drop directories from the listing, so a subdirectory whose own name ends
      in a listed extension is not an `input` row and `recursive = TRUE` is a
      superset of `recursive = FALSE`. Regression tests for both.
- [x] T8: Refuse a multi-value `type` rather than silently using its first element,
      and make the extension refusal's noun phrase agree in number.
- [x] T9: Close the test gaps review named — a directory that looks like a file, a
      multi-value `type`, `extension` as a factor, `directory = ""` — and replace
      the assertion that `normalizePath()` is idempotent on its own output.
- [x] T10: Narrow `NEWS.md`'s claim about which batch verbs take the returned table
      unaltered, and correct the workflow vignette's faked paths and the silent
      broadening in the metadata vignette.
- [x] T11: ROADMAP candidate row for the three deferred findings (symlink
      resolution, hidden files, the `.ts`/`.ogv` gaps in the closed lists).
- [x] T12: Re-run `devtools::check()`, `pkgdown::check_pkgdown()`, and the claim
      audit over the branch's added lines.
- [x] T13: Complete the file predicate — keep only paths that exist and are not
      directories, so a dangling symbolic link is not a row and a relative
      `directory` still yields absolute paths. Regression tests for the broken
      link, for the all-broken directory, and for a link that does resolve.
- [x] T14: Correct the two overclaims about which `*_batch()` verbs take the
      returned table unaltered — `NEWS.md`'s clause and the same claim still
      standing in the roxygen — against a measurement of all fifteen verbs.
- [x] T15: Close the two test gaps: `type = NA_character_`, and a batch-verb
      test that stands behind the whole six-and-nine split the release note
      now states.
- [ ] T16: Re-run `devtools::check()`, `pkgdown::check_pkgdown()`, the `verify`
      slot, and the claim audit over the branch's added lines.

## Work log

- 2026-09-07: created by /milestone-plan.
- 2026-09-07: plan-gate criteria audit ran in FULL mode (declared tier user-facing), fresh-context [O] reader. Findings against this milestone: AC1's "accepts that tibble with no reshaping" was satisfiable by an `.f` that ignores its arguments (repaired — the demonstrated call now names the columns it consumes); AC2's "Each of its refusals" quantified over the function's whole refusal set while the em-dash list enumerated three by hand, leaving the argument-form refusals and D074's `resolve_timeout()` refusal outside it (repaired — narrowed to "These three refusals", with the remainder moved to T3 under the profile's every-branch doctrine). D079's no-default rule and D014/D078's naming constraints were flagged as unpriced by the criteria and moved to T1. AC4 passed all six questions clean.
- 2026-09-07: plan gate chose one directory-listing export over refusing the feature under GP1, because batch over many files is a stated differentiator in DESIGN.md's purpose and both batch-teaching vignettes hand-roll the same `list.files()` shape to feed `ffm_batch()`; falsified by the export going uncited in the vignettes and README a release later.
- 2026-09-10: implement session started; branch `m121-directory-to-jobs-tibble` cut from `origin/master` at cc4761b.
- 2026-09-10: T1 question gate settled three open choices, all at the recommendation. The export is `ffm_jobs(directory, type, extension = NULL, recursive = FALSE)`. Milestone-local decision **M121-1 — `ffm_jobs()` is Layer-1 surface, its `type` is required, and its extension vocabulary is closed** (applies D014's `ffm_*` rule, D078's naming rule and D079's no-default rule; leaves all three standing): the jobs-table constructor takes the `ffm_*` prefix, because DESIGN's Layer-1 family already holds `ffm_batch()` and `ffm_manifest()`, neither of which assembles a command — the family is the engine's surface, not command assembly alone, so `ffm_jobs()` sorts beside the runner that consumes it. `type` takes no default: its three values (`"video"`, `"audio"`, `"image"`) are the set it ranges over and any default would be one member of it, which is D079's rule exactly; `recursive = FALSE` keeps its default because `FALSE` is a toggle's off position, D079's named carve-out. `type`'s vocabulary and each type's extension list are closed, and `extension` may only narrow within the type it is given — a closed set is what lets both refusals name what they accept. The cost is a caller whose container is outside the lists: an `.mxf` folder gets "No video files were found" though FFmpeg would read it. That is GP1's trade taken deliberately — growing the lists toward FFmpeg's container coverage is the drift D001 refuses, and such a caller uses `list.files()` directly as they did before this export existed. The returned tibble carries `input` and nothing else, because `ffm_batch()` passes every column to `.f` by name and a second column would become an argument every `.f` has to accept. **Falsified by** a report of a media file the closed extension lists miss on a machine whose FFmpeg reads it, which turns the refusal into a wrong answer rather than a scope line.
- 2026-09-10: minor plan amendment — T1, T2 and T3 land in one commit rather than three. The decision, the body and the refusals are one file (`R/ffm_jobs.R`) written once; splitting them into three commits would have staged a function with no guards and no tests as an intermediate state. No criterion, scope line or task text changed.
- 2026-09-10: T2/T3 — `R/ffm_jobs.R` implements `ffm_jobs()` as a wrapper over the internal `tm_ffm_jobs(..., call)` per D087, plus the closed vocabulary in `media_types()` / `media_extensions()`. Eight `cli_abort()`/rlang branches, every one fired by `tests/testthat/test-ffm-jobs.R` (82 assertions): AC2's three named refusals, a path that exists but is not a directory, the argument-form refusals on `directory`, `type` (missing and non-string), `recursive` and `extension`, and an `extension` outside its type. Blame is read with `blamed_verb()` on each and is `ffm_jobs` throughout. AC1's hand-off is tested by an `ffm_batch(run = FALSE)` call over the unreshaped table plus a derived `output`, asserting each compiled command names its own row's input and output — an `.f` ignoring its arguments fails it.
- 2026-09-10: T4 — roxygen written, `devtools::document()` run (`NAMESPACE` gains `ffm_jobs`, `man/ffm_jobs.Rd` written with `\value{}` and a runnable example), `_pkgdown.yml` gains the reference row next to `ffm_batch`. `NEWS.md` gains the New-features entry the consistency gate requires.
- 2026-09-10: T5 — `vignettes/workflow.Rmd:40` and `vignettes/metadata.Rmd:121` now call `ffm_jobs()`; `grep -rn "list.files" vignettes/` returns nothing. The workflow vignette's downstream jobs-table block dropped its hand-built `tibble::tibble()` and adds `output` to the returned table instead.
- 2026-09-10: T6 — `devtools::test()` FAIL 0 | WARN 12 | SKIP 5 | PASS 13348; `devtools::check()` Status OK, 0 errors / 0 warnings / 0 notes (6m 3s, R CMD check on tidymedia 0.1.0.9000, vignettes re-built clean); `pkgdown::check_pkgdown()` "No problems found". AC5 met.
- 2026-09-10: claim audit: 55 claims read, 2 corrected — vignettes/workflow.Rmd, R/ffm_jobs.R. Fresh-context [O] reader, authored none of the diff's added lines; it ran `ffm_jobs()` over a fixture directory for all three types, both recursion settings, the dotted/undotted/upper-case `extension` forms and all eleven refusal paths, read `R/ffm_batch.R`'s column hand-off, and read D079/D087/D001/GP1 against the lines citing them. The two corrections: the workflow vignette's new paragraph said "Everything below" adds an `output` column and calls a `*_batch()` verb, true of one of the eleven chunks below it (narrowed to "The next section"); and `tm_ffm_jobs()`'s comment gave a false reason for siting the `type` presence check in the wrapper — the reader reproduced the alternative and got an identical refusal, so the causal clause was dropped for a statement of the site. Both corrections re-read once and returned TRUE-AS-WRITTEN. Deviation: the re-read was by a second fresh [O] reader rather than the first one, because this harness exposes no way to continue a finished subagent; a fresh reader is at least as independent, and the one-pass stopping rule was honored.
- 2026-09-10: **defect return #1 from /milestone-review.** AC1 fails: `ffm_jobs()` returns a *subdirectory* whose name ends in a listed extension as an `input` row — `list.files()` yields directories when `recursive = FALSE` and `R/ffm_jobs.R:94-98` filters none out, so a folder holding `a.mp4` beside a subdirectory `takes.mp4` returns two rows with `dir.exists()` TRUE on the second, against AC1's "full paths to the media files" and `@return`'s "one row per matching file". The same gap makes `recursive = TRUE` drop rows `recursive = FALSE` returns. AC2-AC5 each passed against fresh evidence and the consistency gate was clean (`cairn_validate` exit 0; `check()` 0/0/0; `document()` no diff; `pkgdown::check_pkgdown()` clean). Ten further [O] findings are logged unactioned in the Review section for triage at the next review. Status -> in-progress; PR #125 stays open in draft.
- 2026-09-10: `devtools::check()` re-run on the final tree (9487adb, the two claim-audit corrections included): Status OK, 0 errors / 0 warnings / 0 notes, 6m 23s. Status -> review.
- 2026-09-10: minor plan amendment — T7-T12 added for the returned defect and the review findings the T7 question gate dispositioned. No criterion, scope line or task text changed; existing `Tn:` labels keep their numbers and the Coverage lines gain the new tasks. Gate answers, all at the recommendation: fix the multi-value `type` acceptance and the refusal grammar on this branch; defer the three accept-set findings (symlink resolution, hidden files, the `.ts`/`.ogv` list gaps) to a candidate row, since each widens what the export accepts rather than repairing a broken promise and this milestone has already returned once; fix the release-note claim, the vignette prose and all four test gaps here.
- 2026-09-10: T7/T8/T9 — `R/ffm_jobs.R` drops directories from `list.files()`'s result before the zero-match check, checks `type` is a single string above `arg_match()`, and pluralizes the extension refusal's noun phrase. Six new tests in `tests/testthat/test-ffm-jobs.R` cover the extension-named subdirectory, the recursive superset, both multi-value `type` orders, `extension` as a factor, `directory = ""`, and the refusal's singular/plural forms; the idempotent-`normalizePath()` assertion is replaced by the fixture's own two paths spelled from the directory's normalized root. Discrimination checked: with the three code changes reverted the file reports 9 failures, with them 0. `devtools::test()` FAIL 0 | WARN 12 | SKIP 5 | PASS 13369; `devtools::document()` rewrote `man/ffm_jobs.Rd` for the `@return` line saying subdirectories are never rows.
- 2026-09-10: T10 — `NEWS.md`'s lead sentence narrowed: the returned tibble is what `ffm_batch()` takes, and the closing sentence now says the `*_batch()` verbs that derive their own output take it unaltered while the others refuse it until their columns are added. Measured on a two-row `ffm_jobs()` table with `run = FALSE`: `standardize_video_batch()`, `normalize_audio_batch()`, `format_for_web_batch()` and `strip_metadata_batch()` accept it; `extract_audio_batch()` refuses with `` must have an output column ``, `segment_video_batch()` with `` must have columns "input", "start", and "end" ``. A new test in `test-ffm-jobs.R` pins the two verbs the entry names by hand plus one refusal, so the entry narrows to what a named test enforces. `vignettes/workflow.Rmd` now says the paths come back absolute however the folder is spelled, which is what the faked `/data/study/raw/...` output was silently assuming; `vignettes/metadata.Rmd` says the call lists every video file rather than one container and names `extension = "mp4"` as the narrowing.
- 2026-09-10: T11 — one ROADMAP candidate row records the three deferred findings (symlink resolution collapsing two `basename()`s, hidden files reported as no files, `ts`/`ogv` missing beside their own siblings), each with its own promotion condition. Search-first: no existing candidate row or archive summary mentions `ffm_jobs()`, symlinks or hidden files. `ROADMAP.md` is now 59 lines / 38,389 bytes against its 60-line, 24,000-byte budget — the over-budget condition M120's hygiene stamp already records, worsened by this row; `/cairn-triage` remains the only remedy.
- 2026-09-10: claim audit: 40 claims read, 5 corrected — R/ffm_jobs.R, NEWS.md, vignettes/workflow.Rmd, tests/testthat/test-ffm-jobs.R. Fresh-context [O] reader, authored none of the diff's added lines. The corrections: the new `arg_match()` comment (and the test comment repeating it) named `identical()` as the gate where rlang's body reads `setequal()`, so both understated the hole — any permutation of the full set was reduced to its own first element, not only the identical spelling; the same test comment called a two-element vector an "order" and implied it used to pass, when it always aborted, from `arg_match()` rather than with the "single string" message the test now asserts; `NEWS.md`'s "an `output` column above all" reversed the distribution — of the eleven verbs that refuse the bare table, three name `output` and eight name a task-specific column (`start`/`end`, `regions`, `inputs`, `width`, `timestamp`/`frame`, `fps`/`interval`, `audiofile`/`videofile`); the workflow vignette's unqualified "absolute however you spell the folder" has a counterexample, since `normalizePath(mustWork = FALSE)` returns a broken symlink's path unchanged; and the test file's header still said "the one ffm_batch() call" where the file now makes four batch calls. All five re-read once and returned TRUE-AS-WRITTEN, each verified by execution. Deviation, as at the first audit: the re-read was by a second fresh [O] reader rather than the first, because this session has no tool for continuing a finished subagent; a fresh reader is at least as independent, and the one-pass stopping rule was honored.
- 2026-09-10: T12 — `devtools::check()` on the corrected tree: Status OK, 0 errors / 0 warnings / 0 notes, 5m 51.4s, `checking tests` and `re-building of vignette outputs` both OK. `pkgdown::check_pkgdown()` "No problems found". `verify` slot clean: `devtools::test()` FAIL 0 | WARN 12 | SKIP 5 | PASS 13373 (13348 at the return, +25); `devtools::document()` re-run produces no diff. AC5 met on the returned tree. Status -> review.
- 2026-09-10: **defect return #2 from /milestone-review.** AC1 fails again, by a new mechanism of the same shape: `ffm_jobs()` returns a *dangling symlink* as an `input` row, and from a relative `directory` that row is not even an absolute path. `files[!dir.exists(files)]` (`R/ffm_jobs.R:110`) drops directories but not broken symlinks, which `list.files()` also yields, and `normalizePath(mustWork = FALSE)` returns an unresolvable path unchanged — so a folder holding `real.mp4` beside `broken.mp4 -> ../gone.mp4`, called as `ffm_jobs("raw", type = "video")` from the parent, returns `raw/broken.mp4` with `file.exists()` FALSE, against AC1's "full paths to the media files" and `@return`'s "one row per matching file". Not the deferred candidate row's symlink item, which is about links that *do* resolve, into colliding `basename()`s. The complete predicate is `file.exists(files) & !dir.exists(files)`. AC2-AC5 each passed against fresh evidence this pass (16 refusal branches all blaming `ffm_jobs`; `check()` 0/0/0; `test()` PASS 13373; `document()` no diff) and the consistency gate was clean (`cairn_validate` exit 0, one `sizing` advisory at 12 tasks). Four further [O] findings are logged unactioned in the Review section for triage at the next review. **Defect-return count: 2**; thrash trigger (b) fires — same criterion, same shape, new mechanism — and the alternative the plan gate recorded against was refusing the feature under GP1. Status -> in-progress; PR #125 stays open in draft.
- 2026-09-10: implement session resumed on the returned tree; `origin/master` still 0 ahead of the cut, so no merge. Defect return #2's case reproduced first: fixture `raw/` holding `real.mp4` beside `broken.mp4 -> ../gone.mp4`, called as `ffm_jobs("raw", type = "video")` from the parent, returned `raw/broken.mp4` with `file.exists()` FALSE and `startsWith("/")` FALSE.
- 2026-09-10: **thrash trigger (b) gate** — the remedy the rule names is to reconsider the alternative the plan gate recorded against, refusing the feature under GP1. Presented with that alternative and with `/milestone-brief` escalation; the user chose to continue with the complete predicate. The ground offered for it, and the ground the choice rests on: both failing mechanisms met an incomplete blacklist, and `file.exists(files) & !dir.exists(files)` is a positive existence test that closes the class rather than subtracting one more member of it. Falsified by a third AC1 failure of the same shape — a returned `input` row that is not a readable media file — which would put the count at trigger (a)'s threshold and its descope-or-park remedy. The four findings pass 2 carried in unactioned were triaged at the same gate: fix [O]2 (the `NEWS.md` clause), [O]3 (the same overclaim still in the roxygen) and [O]4 (the two test gaps) on this branch; defer [O]5 (`cairn/DESIGN.md`'s Layer-1 enumeration naming neither `ffm_jobs` nor, already, `ffm_manifest`) as a pre-existing gap.
- 2026-09-10: minor plan amendment — T13-T16 added for the returned defect and the three findings the gate dispositioned to this branch; T13-T15 land in one commit rather than three, because the predicate, the roxygen it re-words and the tests that pin both are one file each and splitting them would stage a corrected function under uncorrected prose. No criterion, scope line or task text changed; existing `Tn:` labels keep their numbers and the Coverage lines gain the new tasks.
- 2026-09-10: T13/T14/T15 — `R/ffm_jobs.R:110` becomes `files[file.exists(files) & !dir.exists(files)]`, and `@return` now promises every row is a path that exists and is not a directory, naming both the extension-named subdirectory and the dead symbolic link. Three new tests: the dangling link is not a row and every row is absolute from a relative `directory`; a directory holding only a dangling link refuses with `No video files were found` rather than returning zero rows; and a link that *does* resolve is still a row, which discriminates against a predicate that drops every link. Discrimination checked: with the predicate reverted to `!dir.exists(files)` the file reports 6 failures, with it 0. T14's measurement over all fifteen `*_batch()` task verbs on a bare two-row `ffm_jobs()` table: four accept with no argument (`standardize_video_batch`, `normalize_audio_batch`, `format_for_web_batch`, `strip_metadata_batch`), two more accept the table unaltered once their argument is supplied (`crop_video_batch` with `width`/`height`/`x`/`y`, `sample_frames_batch` with `fps`), and nine refuse it — three naming `output` (`convert_audio_batch`, `extract_audio_batch`, `picture_in_picture_batch`) and six naming a task-specific column (`regions`, `inputs` twice, `timestamp`/`frame`, `start`/`end`, `audiofile`/`videofile`). `NEWS.md` and the roxygen both now state that split, and T15's rewritten test pins all fifteen verbs, so no clause of the entry stands without a test. `type = NA_character_` joins the argument-form refusal table. `devtools::document()` rewrote `man/ffm_jobs.Rd`; `devtools::test()` FAIL 0 | WARN 12 | SKIP 5 | PASS 13406 (13373 at the return, +33).

## Review

### First pass — 2026-09-10 (defect return #1)

Reviewed 2026-09-10 on branch `m121-directory-to-jobs-tibble` (3 commits ahead of
`origin/master`, 0 behind — the default branch had not moved, so no merge was needed).
Draft PR: https://github.com/jmgirard/tidymedia/pull/125.

**Fencing note.** All five acceptance boxes arrived at review already ticked, with no
Review section and so no recorded evidence. They were unticked and re-ticked here one
at a time as each criterion's fresh evidence landed.

- **AC1 — FAIL (see [O]1 below).** The positive half holds: Fresh fixture directory (`a.mp4`, `b.MOV`, `c.wav`, `d.png`, `e.txt`,
  `sub/f.mkv`). `ffm_jobs(d, type = "video")` returns a `tbl_df` whose only column is
  `input`, 2 rows, every path absolute and existing; case-insensitive matching picks up
  `b.MOV`. The `extension = ".mp4"` selector narrows to 1 row; `recursive = TRUE` adds
  `sub/f.mkv`; `type = "audio"` and `type = "image"` each return their own file and
  neither returns `e.txt`. Hand-off: `jobs$output` derived from `input`, then
  `ffm_batch(jobs, run = FALSE, .f = function(input, output, ...))` over the unreshaped
  table — the returned tibble is `input, output, command`, its `input` column
  `identical()` to the one passed in, and each row's `command` names that row's own
  input and its own output. **Discriminating control:** an `.f` that ignores its
  arguments and always builds row 1 produces a `command` naming row 2's input for 0 of
  the 2 rows — the criterion separates the two cases. **But the criterion says "full
  paths to the media files", and a row can be something that is not a media file:** a
  fixture directory holding `a.mp4` beside a *subdirectory* named `takes.mp4` returns 2
  rows, and `dir.exists()` is `TRUE` on the second. `list.files()` returns directories
  when `recursive = FALSE`, and `R/ffm_jobs.R:94-98` filters none out. The box is
  unticked.
- **AC2 — pass.** Every refusal caught with `rlang::catch_cnd()` and its
  `conditionCall()` read. The three AC2-named refusals blame `ffm_jobs`: a directory
  that does not exist (`` `directory` does not name an existing directory ``), a type
  outside the accepted set (`` `type` must be one of "video", "audio", or "image", not
  "sound" ``), and a call matching no file (`No video files were found in ...`, with an
  `i` bullet listing the extensions looked for). The eight further branches T3 required
  under the profile's every-branch doctrine also blame `ffm_jobs`: a path that exists
  but is not a directory, non-string `directory`, missing `type`, non-string `type`,
  non-logical `recursive`, non-character `extension`, zero-length `extension`, and an
  `extension` outside its type. D087 holds: `names(formals(ffm_jobs))` is
  `directory, type, extension, recursive` — no published `call` formal.
- **AC3 — pass.** `_pkgdown.yml:57` carries the `ffm_jobs` reference row;
  `man/ffm_jobs.Rd` (84 lines) has `\value{}` at :25 and `\examples{}` at :44.
  All three land in the same commit as the export: `git log origin/master..HEAD` names
  fc007d3 for `NAMESPACE`, `man/ffm_jobs.Rd` and `_pkgdown.yml` alike, and
  `NAMESPACE:30` is `export(ffm_jobs)`. The example is runnable — executed verbatim
  outside `R CMD check`, it returns the one-row `inst/extdata/sample.mp4` table and
  compiles a command through `ffm_batch(run = FALSE)`; `R CMD check`'s own
  `checking examples ... OK` is the second reading.
- **AC4 — pass.** `vignettes/workflow.Rmd:40` is
  `jobs <- ffm_jobs("study/raw", type = "video")` and `vignettes/metadata.Rmd:121` is
  `files <- ffm_jobs("my/videos", type = "video")$input`. `grep -rn "list\.files"
  vignettes/` returns nothing (exit 1), so the two rewritten calls were the only two
  and no third hand-rolled listing remains.
- **AC5 — pass.** `devtools::check()` on the branch head (9487adb tree plus this
  Review section): `Status: OK`, `0 errors | 0 warnings | 0 notes`, 6m 7.4s, R CMD check
  on tidymedia 0.1.0.9000, vignettes re-built clean, `checking examples ... OK`.
  `verify` slot clean: `devtools::test()` `FAIL 0 | WARN 12 | SKIP 5 | PASS 13348`;
  `devtools::document()` re-run produces no diff (`git status --porcelain` shows only
  this milestone file).

### Consistency gate — pass

- `cairn_validate.py` exit 0, all 16 PASS checks and all 7 advisories OK, including
  `coverage complete`, `binding criteria`, `scaffold present` and `release window`
  (the release-window advisory did **not** fire).
- `cairn_impact.py` skipped: no `DESIGN.md` principle changed (`git diff --name-only`
  lists no `cairn/DESIGN.md`).
- Toolchain checks from `cairn/PROFILE.md`'s `consistency-gate` slot:
  `document()` no diff ✔ · generated files not hand-edited (the 23 `man/*.Rd` touches
  are roxygen `@family` index regeneration) ✔ · README.Rmd/README.md untouched by this
  milestone and last written by the same commit 0df9835, so in sync ✔ ·
  `pkgdown::check_pkgdown()` "No problems found" ✔ · `NEWS.md` carries the
  user-visible entry at :262-272 and `grep -nE '\bM[0-9]{2,3}\b' NEWS.md` finds no
  milestone numbers ✔ · no new top-level files, so no `.Rbuildignore` entry needed ✔ ·
  `devtools::check()` 0/0/0 ✔.

### Independent review — full three-lens fan-out (surface tier: user-facing)

**[S] blame-history — no regression.** The only deleted/rewritten lines are the two
`list.files()` calls AC4 named (originating in 77f77f43, M30's vignette overhaul, and
untouched since); everything else is addition. No D-entry contradicted (D079's
no-default rule and its toggle-off carve-out, D087's unpublished `call`, GP1/D001 scope
discipline all hold); no `NEWS.md` entry restated or duplicated.

**[S] prior-review record — no regression.** LESSONS' M100/M110/M112 line (a threaded
`call` must reach `check_string()`/`check_bool()`/`arg_match()`, not only `cli_abort()`)
is followed at `R/ffm_jobs.R:61-63`. LESSONS' M109 line (jobs-table fixtures whose rows
share a derived output path let the collision guard stand in for the check) does not
recur — the test derives `output` from distinct basenames. M120's `NEWS.md` overclaim
pattern was already caught by this milestone's own claim audit. The GitHub probe
(`gh api .../pulls/comments?per_page=1`) returned `[]`, so the per-PR walk was skipped
per the recipe's probe gate. Zero findings.

**[O] diff-bug — 11 findings, ranked.** Each verified against the running
implementation, not the reviewer's account of it.

1. **CONFIRMED · floor-qualifying — a directory whose name ends in a listed extension
   is returned as an `input` row** (`R/ffm_jobs.R:94-98`). `a.mp4` beside a
   subdirectory `takes.mp4` → 2 rows, `dir.exists()` TRUE on the second; that path
   reaches FFmpeg at run time. Falsifies AC1 ("full paths to the media files") and the
   `@return` promise "one row per matching file" (`R/ffm_jobs.R:27-30`).
2. **CONFIRMED (a consequence of 1) — `recursive = TRUE` is not a superset of
   `recursive = FALSE`.** Same fixture: `FALSE` → `a.mp4`, `takes.mp4`; `TRUE` →
   `a.mp4`, `sub/deep.mp4`. `takes.mp4` disappears because `list.files(recursive =
   TRUE)` excludes directories. Fixing 1 fixes this.
3. **CONFIRMED — a multi-element `type` is silently accepted and only its first value
   used** (`R/ffm_jobs.R:63`). `type = c("video","audio","image")` returns only the
   video rows with no error, because `rlang::arg_match()` takes the first element when
   `arg` is `identical()` to `values`; `type = c("audio","video")` *does* error, so the
   behavior is inconsistent as well. No test covers it.
4. **CONFIRMED — `normalizePath()` resolves symlinks, so `input` can point outside
   `directory` under a different basename** (`R/ffm_jobs.R:107`). Two symlinks
   `link_a.mp4`/`link_b.mp4` into `corp/s1/master.mp4` and `corp/s2/master.mp4` return
   two paths whose `basename()` is `master.mp4` for both — so the `output` derivation
   the Rd example and the workflow vignette both teach collides and the second job
   silently overwrites the first.
5. **CONFIRMED — hidden files are invisible and the miss is reported as "no files
   found"** (`list.files(all.files = FALSE)`). A directory whose only content is
   `.s1.mp4` aborts with `No video files were found in ...` — a wrong answer rather
   than a scope refusal, which is the failure shape decision M121-1 names as its own
   falsifier.
6. **CONFIRMED as stated — the closed video list omits `.ts` while carrying `.mts` and
   `.m2ts`, and omits `.ogv` while audio carries `.ogg`/`.oga`**
   (`R/ffm_jobs.R:123-127`). A folder of `.ts` captures aborts with an `i` bullet
   naming `mts` and `m2ts`, which reads as a bug rather than a scope line. M121-1
   accepts that the lists are *closed*, not that they are internally inconsistent.
7. **PARTLY REFUTED — `NEWS.md:262-264`'s "the tibble `ffm_batch()` and the `*_batch()`
   verbs take".** The reviewer's own example is wrong: `standardize_video_batch(j, run
   = FALSE)` over the raw `ffm_jobs()` output raises no error — it derives its own
   output. But the claim behind it holds for other verbs: `extract_audio_batch()`
   aborts with `` `jobs` must have an output column `` and `segment_video_batch()` with
   `` must have columns "input", "start", and "end" ``. So the lead sentence overclaims
   for at least two `*_batch()` verbs, though the bullet's last sentence walks it back.
8. **Two near-tautological tests** (`tests/testthat/test-ffm-jobs.R:41-43`, `:180-186`).
   `expect_identical(normalizePath(jobs$input, mustWork = TRUE), jobs$input)` asserts
   `normalizePath()` is idempotent on its own output — true by construction of line 107,
   and it would pass with the wrong files returned. The `media_types()` /
   `media_extensions()` block tests internal helpers directly against the
   PROFILE test-doctrine's "indirect by default", and restates the implementation line
   for line; its no-overlap assertion is load-bearing and worth keeping.
9. **Test coverage gaps:** no test for a directory-that-looks-like-a-file (1), for
   multi-element `type` (3), for `extension` as a factor (the profile's test-doctrine
   names "factor vs. character" explicitly), or for `directory = ""`.
10. **Vignette prose nits** (`vignettes/workflow.Rmd:41-47`). The faked output shows
    `/data/study/raw/...` for a call written `ffm_jobs("study/raw", ...)`, implying
    `getwd()` is `/data`, and nothing says paths come back absolute — the one behavior
    change from the `list.files()` line it replaced. `vignettes/metadata.Rmd:121`
    silently broadens selection from `\.mp4$` to all twelve video extensions with the
    prose unchanged. Both chunks are `eval = FALSE`.
11. **Grammar in the extension refusal** (`R/ffm_jobs.R:87`): `"wav" and "png" are not
    one of them.` — `{?is/are}` pluralizes but the noun phrase does not.

Clean categories the [O] lens checked and cleared: no regex-injection path in the
`list.files` pattern (every element of `wanted` is validated against the closed
lowercase alnum vocabulary first); `ignore.case = TRUE` correct against the lowercase
set and dotted/upper-case `extension` input; `tolower`/dot-strip/`unique` normalization
correct including duplicates; D087 satisfied; the zero-match abort matches `@return`;
`media_types`/`media_extensions` clash with no existing internal; the `NEWS` entry sits
under the development-version heading.

### Disposition — defect return under the step-5 return floor

Finding 1 demonstrates AC1 failing inside its own domain, so it is floor-qualifying and
the milestone returns to `in-progress` rather than reaching the merge gate. Findings
2-11 are carried into that return unactioned; their triage (fix now / follow-up /
reject) is the maintainer's at the next review. **Defect-return count for M121: 1.**
No amendment return. PR #125 stays open in draft.

### Second pass — 2026-09-10

Reviewed on branch `m121-directory-to-jobs-tibble` at 6bd9fe4, 7 commits ahead of
`origin/master` and **0 behind** — the default branch had not moved since the cut, so
no merge was needed and the first pass's evidence base is the same tree plus T7-T12.
Branch pushed; PR #125 was already open in draft, so no `gh pr create`.

**Fencing note.** All five boxes were unticked before this pass: AC2-AC5 carried the
first pass's ticks, but the tree changed under them (T7-T12), so that evidence was
stale. Each box below was re-ticked as its own fresh evidence landed.

- **AC1 — pass (the returned defect is closed).** Fresh fixture (`a.mp4`, `b.MOV`,
  `c.wav`, `d.png`, `e.txt`, `sub/f.mkv`) **plus the returned defect's own case**: a
  subdirectory named `takes.mp4` holding `takes.mp4/g.mp4`.
  `ffm_jobs(d, type = "video")` returns a `tbl_df` whose only column is `input`,
  2 rows (`a.mp4`, `b.MOV`), every path absolute, `file.exists()` TRUE on all,
  and `any(dir.exists(input))` **FALSE** — `takes.mp4` is no longer a row.
  `recursive = FALSE` → {`a.mp4`, `b.MOV`}; `recursive = TRUE` → {`a.mp4`, `b.MOV`,
  `f.mkv`, `g.mp4`}; `all(FALSE_set %in% TRUE_set)` is TRUE, so `TRUE` is a superset
  (first-pass finding 2 closed with 1). `extension = ".mp4"` narrows to 1 row;
  `type = "audio"` returns `c.wav` and `type = "image"` `d.png`, neither returning
  `e.txt`. Hand-off: `jobs$output` derived from `input`, then
  `ffm_batch(jobs, run = FALSE, .f = function(input, output, ...))` over the
  unreshaped table returns `input, output, command` with `input` `identical()` to the
  column passed in, and each row's `command` names that row's own input and output
  (2 of 2). **Discriminating control:** an `.f` ignoring its arguments and always
  building row 1 matches 1 of 2 — the criterion separates the cases.
- **AC2 — pass.** Sixteen refusal paths fired and each condition's
  `conditionCall()` read; **every one blames `ffm_jobs`.** The three AC2 names:
  a directory that does not exist (`` `directory` does not name an existing
  directory ``), a type outside the set (`` `type` must be one of "video", "audio",
  or "image", not "sound" ``), and a call matching no file (`No video files were
  found in …` with an `i` bullet listing the twelve extensions). The further branches
  T3/T8 require: a path that exists but is not a directory, `directory` non-string,
  `directory = ""`, `type` missing, `type` non-string, **`type` multi-valued in both
  orders — `c("video","audio","image")` and `c("audio","video")` now both abort with
  `` `type` must be a single string, not a character vector `` (first-pass finding 3
  closed; the full-set permutation that `arg_match()` used to reduce silently is
  caught by the `check_string()` placed above it)**, `recursive` non-logical,
  `extension` non-character, `extension` as a **factor**, `extension` zero-length,
  and `extension` outside its type. The extension refusal's grammar now agrees:
  one unknown → `"wav" is not one of them.`, two → `"wav" and "png" are not among
  them.` (first-pass finding 11 closed). D087 holds:
  `names(formals(ffm_jobs))` is `directory, type, extension, recursive`.
- **AC3 — pass.** `_pkgdown.yml:57` carries the `ffm_jobs` reference row;
  `man/ffm_jobs.Rd` (86 lines) has `\value{}` at :25 and `\examples{}` at :46;
  `NAMESPACE:30` is `export(ffm_jobs)`. All four files' first branch commit is the
  same one — `git log origin/master..HEAD -- <file>` gives fc007d3 for `R/ffm_jobs.R`,
  `NAMESPACE`, `man/ffm_jobs.Rd` and `_pkgdown.yml` alike. `R CMD check`'s
  `checking examples ... OK` is the runnable reading.
- **AC4 — pass.** The criterion's line numbers are dated addresses ("as measured
  2026-09-07"), and at `origin/master` they are exactly the two `list.files()` sites:
  `git grep -n 'list\.files' origin/master -- vignettes/` returns
  `metadata.Rmd:121` and `workflow.Rmd:40`, and nothing else. Both are now
  `ffm_jobs()` calls: `workflow.Rmd:40` is
  `jobs <- ffm_jobs("study/raw", type = "video")` and the metadata call is
  `files <- ffm_jobs("my/videos", type = "video")$input`, which T10's two added prose
  lines moved from :121 to :123. `grep -rn "list\.files" vignettes/` returns nothing
  (exit 1), so no third hand-rolled listing remains.
- **AC5 — pass.** `devtools::check()` on the branch head: `Status: OK`,
  `0 errors ✔ | 0 warnings ✔ | 0 notes ✔`, 6m 17.2s, R CMD check on
  tidymedia 0.1.0.9000, `checking examples ... OK` and `checking tests ... OK`.
  `verify` slot clean: `devtools::test()` `FAIL 0 | WARN 12 | SKIP 5 | PASS 13373`;
  `devtools::document()` re-run produces no diff (`git status --porcelain` shows only
  this milestone file).

#### Consistency gate — pass

- `cairn_validate.py` exit 0. All 16 PASS checks pass — including `coverage complete`,
  `binding criteria`, `scaffold present` and `profile valid` — with one advisory
  warning, `sizing (split tripwires)`: M121 now carries 12 tasks against the 10-task
  tripwire. Advisory, not a gate failure. The `release window` advisory did **not**
  fire.
- `cairn_impact.py` skipped: no `DESIGN.md` principle changed
  (`git diff --name-only origin/master..HEAD` lists no `cairn/DESIGN.md`).
- Toolchain checks from `cairn/PROFILE.md`'s `consistency-gate` slot:
  `document()` no diff ✔ · generated files not hand-edited (the 22 other `man/*.Rd`
  touches are roxygen `@family` index regeneration) ✔ · `README.Rmd`/`README.md`
  untouched by this branch and both last written by 0df9835, so in sync ✔ ·
  `pkgdown::check_pkgdown()` "No problems found" ✔ · `NEWS.md` carries the
  user-visible entry under the development heading at :260-274 and
  `grep -nE '\bM[0-9]{2,3}\b' NEWS.md` finds no milestone numbers (exit 1) ✔ ·
  no new top-level files, so no `.Rbuildignore` entry needed ✔ ·
  `devtools::check()` 0/0/0 ✔.

#### Independent review — full three-lens fan-out (surface tier: user-facing)

**[S] blame-history — no regression.** Every commit fc007d3 → 6bd9fe4 traced against
the history of the code it touches. The branch's own later commits correct real
defects rather than undoing deliberate earlier work: `df0ea93`'s `!dir.exists()`
filter and `check_string(type)` guard repair the two behaviors the first pass found,
and `6bd9fe4`'s comment correction (`identical()` → `setequal()`) was verified against
`arg_match()`'s body. D079, D087, D014 (via the recorded M121-1 gate) and GP1/D001 all
hold; the three deferred findings are on a candidate row rather than silently dropped.
Zero regressions.

**[S] prior-review record — no prior-review evidence, zero findings.** No archived
`## Review` section and no `LESSONS.md` line names `R/ffm_jobs.R` or this pattern —
`ffm_jobs()` is new in M121, so the only precedent is this milestone's own first pass,
whose eight actioned findings were each verified present and unreverted at HEAD. The
GitHub probe (`gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1`) returned
`[]`, so the per-PR walk was skipped per the recipe's probe gate.

**[O] diff-bug — 5 findings, ranked.** The T7-T12 fixes themselves were re-verified by
execution and are correct: the `!dir.exists()` filter, the `recursive` superset
property, the multi-value `type` refusal in both orders, the pluralized refusal, the
corrected `setequal()` comment, and a binary-free test file (111 assertions, passing
with `ffmpeg` off `PATH`).

1. **CONFIRMED · floor-qualifying — a dangling symlink is still returned as an `input`
   row, and with a relative `directory` its path is not absolute**
   (`R/ffm_jobs.R:110`). `!dir.exists(files)` removes directories but not broken
   symlinks, which `list.files()` also yields, and `normalizePath(mustWork = FALSE)`
   returns an unresolvable path unchanged. Reproduced independently at review: a
   folder `raw/` holding `real.mp4` and a symlink `broken.mp4 → ../gone.mp4`, called
   as `ffm_jobs("raw", type = "video")` from the parent, returns
   `raw/broken.mp4` and `/private/.../raw/real.mp4` — `file.exists()` is FALSE and
   `startsWith("/")` is FALSE on row 1. Falsifies AC1's "full paths to the media
   files" twice over, and `@return`'s "one row per matching file … holding each
   file's full path" (`R/ffm_jobs.R:27-29`) and `NEWS.md`'s "the file's full path in
   an `input` column". **Not covered by the deferred candidate row**, whose symlink
   item is about `normalizePath()` *resolving* two links into colliding
   `basename()`s — the opposite case from a link that resolves to nothing. The
   branch's own T12 claim audit found this counterexample and hedged the workflow
   vignette's prose around it rather than repairing the function. The complete
   predicate is `file.exists(files) & !dir.exists(files)`.
2. **CONFIRMED — `NEWS.md:271-274`'s "the others refuse it until you add the columns
   they name" overstates two of the eleven refusing task verbs.** Measured over all
   15 `*_batch()` task verbs on a bare two-row `ffm_jobs()` table: 4 accept
   (`standardize_video_batch`, `normalize_audio_batch`, `format_for_web_batch`,
   `strip_metadata_batch`) and 11 refuse — but `crop_video_batch(j, width=, height=,
   x=, y=, run = FALSE)` and `sample_frames_batch(j, fps = 1, run = FALSE)` both
   accept the *unaltered* table once an argument is supplied, and their own messages
   say so (`` Pass `width` (applied to every row) or add a width column ``;
   `` exactly one of `fps` or `interval` (argument or column) ``). T10 narrowed the
   sentence's first half and left the second half overstating.
3. **CONFIRMED — the "an `output` column above all" claim T12's audit corrected in
   `NEWS.md` is still in the roxygen** (`R/ffm_jobs.R:13`, shipped at
   `man/ffm_jobs.Rd:43`). Of the 11 task verbs that refuse the bare table, 3 name
   `output` (`extract_audio_batch`, `convert_audio_batch`,
   `picture_in_picture_batch`) and 8 name a task-specific column. The release note
   and the reference page now say different things about the same behavior, and the
   reference page is the one users read first.
4. **PLAUSIBLE — test gaps the T9 sweep left open.** `type = NA_character_` fires a
   `check_string()` branch no test covers, though the profile's test-doctrine names
   `NA` explicitly and the sibling `directory` `NA` case *is* tested
   (`tests/testthat/test-ffm-jobs.R:166`). Separately, the new test at `:249-262`
   pins 2 of the 4 accepting verbs and 1 of the 11 refusing ones, so the `NEWS.md`
   clause finding 2 falsifies has no test standing behind it — which is what T10 said
   that test was for.
5. **PLAUSIBLE, low — `cairn/DESIGN.md:22-26`'s Layer-1 enumeration does not gain
   `ffm_jobs`.** M121-1 justified the `ffm_*` prefix by that section already holding
   `ffm_batch()` and `ffm_manifest()`, and the section names every Layer-1 export by
   hand. Pre-existing pattern rather than a regression: `ffm_manifest` (exported at
   `NAMESPACE:32`) is already absent from the same list.

Clean categories the [O] lens checked and cleared: the dir filter on `character(0)`
and on symlinked *directories*; the extension-refusal pluralization and the `{scope}`
interpolation; no cross-category extension overlap in the closed vocabulary; the
`_pkgdown.yml:57` row and the `man/` regeneration; the workflow vignette's faked
output against a real four-row tibble print, and its `sub("/raw/", "/std/", …)` on
absolute paths; the metadata vignette's prose against what the call now selects.

#### Disposition — defect return #2 under the step-5 return floor

Finding 1 demonstrates AC1 failing inside its own domain — a returned row that is not
a media file and, from a relative `directory`, not a full path — so it is
floor-qualifying and the milestone returns to `in-progress` rather than reaching the
merge gate. AC1's box is unticked; AC2-AC5 keep the ticks this pass's evidence earned.
Findings 2-5 are carried into the return unactioned; their triage (fix now /
follow-up / reject) is the maintainer's at the next review.

**Defect-return count for M121: 2.** No amendment return. **Thrash trigger (b) fires:**
AC1 has now failed twice, each by a new mechanism of the same shape — a path that is
not a media file returned as an `input` row (first a subdirectory matching the
extension pattern, now a dangling symlink). The remedy the rule names is to reconsider
the alternative the plan gate recorded against; the 2026-09-07 work-log entry records
it as **refusing the feature under GP1** — no directory-listing export, callers keep
`list.files()`. Trigger (a) has not fired: this is the second return, not the third.
PR #125 stays open in draft.
