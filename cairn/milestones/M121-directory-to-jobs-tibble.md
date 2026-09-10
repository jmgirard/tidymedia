# M121: A directory becomes a jobs tibble

- **Status:** review
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

- [x] AC1: A newly exported function returns a tibble carrying an `input` column of
      full paths to the non-hidden files in a named directory whose extension is one
      the `type` and `extension` arguments select from the closed vocabulary
      `media_extensions()` holds, and a demonstrated `ffm_batch()` call consumes that
      tibble's `input` column together with an `output` column derived from it,
      without reshaping the returned object. Every returned row is a path that exists
      and is not a directory — on macOS, Linux and Windows alike, with one disclosed
      exception: Windows reports a symbolic link whose target is absent as existing,
      so such a link may be a row there. That case exits to the `ffm_jobs()`
      candidate row as item (f).
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

- AC1 → T1, T2, T7, T9, T13, T15, T17
- AC2 → T3, T8, T9, T15
- AC3 → T4, T14
- AC4 → T5, T10
- AC5 → T6, T12, T16, T18

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
- [x] T16: Re-run `devtools::check()`, `pkgdown::check_pkgdown()`, the `verify`
      slot, and the claim audit over the branch's added lines.
- [x] T17: Land the descope. Guard the two dangling-symbolic-link test blocks
      with `skip_on_os("windows")` naming the candidate row that now holds the
      case, qualify `@return` and the `NEWS.md` entry so neither promises on
      Windows what the amended AC1 no longer promises, and extend the
      `ffm_jobs()` candidate row with item (f).
- [x] T18: Re-run `devtools::check()`, `pkgdown::check_pkgdown()`, the `verify`
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
- 2026-09-10: claim audit: 38 claims read, 1 corrected — NEWS.md. Fresh-context [O] reader, authored none of the diff's added lines; it ran the new predicate against a `takes.mp4` directory, a dangling link, a live link and both `recursive` settings, called all fifteen `*_batch()` verbs and read each condition, ran the roxygen example against `inst/extdata`, and ran the test file with `ffmpeg` and `mediainfo` off `PATH` (140 assertions, zero skips). The one correction: the entry's "add a column" was singular where two of the nine refusers name more than one — `picture_in_picture_batch()` wants `main`, `overlay` and `output` together, and `separate_audio_video_batch()` `audiofile` and `videofile` — so the clause now names those two by hand. Re-read once and returned TRUE-AS-WRITTEN; verified again here by firing all three refusals. The reader also noted, as a fact rather than a false claim, that `normalizePath()` gives a live symlink's row its target's path; nothing in the diff says otherwise.
- 2026-09-10: T16 — `devtools::check()` on the corrected tree: Status OK, 0 errors / 0 warnings / 0 notes, 8m 44.8s, `checking examples ... OK`, `checking tests ... OK` (417s), `re-building of vignette outputs ... OK`. `pkgdown::check_pkgdown()` "No problems found". `verify` slot clean: `devtools::test()` FAIL 0 | WARN 12 | SKIP 5 | PASS 13406; `devtools::document()` re-run produces no diff. AC5 met on the returned tree. Status -> review.
- 2026-09-10: third review pass — all five criteria pass against fresh evidence (AC1 verified against both returned defects' own cases plus a live symlink and a relative `directory`; 19 refusal branches all blaming `ffm_jobs`; `check()` 0/0/0, `test()` PASS 13406, `document()` no diff). Consistency gate clean, `cairn_validate` exit 0 with one `sizing` advisory at 16 tasks. Three-lens fan-out: blame-history and prior-review each zero findings, [O] 11 findings, none floor-qualifying. Awaiting triage and the merge gate.
- 2026-09-10: triage at the merge gate — [O]1-[O]4 filed as follow-ups by extending the existing `ffm_jobs()` candidate row (search-first: no new row; (a) extended, (d)/(e) added); [O]5-[O]11 rejected with reasons recorded in the Review section. PR conversation read: no reviews, no unresolved threads, one Codecov bot comment noted. **step-7 approval: PR #125 approved for merge.**
- 2026-09-10: merge marker written (`cairn/.merge-approved`, PR #125); PR marked ready. CI wait hit the harness ceiling — the foreground `gh pr checks 125 --watch --fail-fast` was moved to the background and stopped with `TaskStop`. Fresh `gh pr checks 125` at the stop: `pkgdown` pass (2m46s); `test-coverage` and the seven `R CMD check` legs (macOS release, Windows release, ubuntu devel/release/oldrel-1/4.1.0) all pending. Not merged. Re-run `/milestone-review M121` to re-derive the state and wait again.
- 2026-09-10: **defect return #3 from /milestone-review — red CI.** Merge was approved and the marker written; `gh pr checks 125` came back red before any merge attempt, and the marker was deleted unused. `windows-latest (release)` fails `R CMD check` with `Status: 1 ERROR`, `checking tests`, `FAIL 4 | WARN 0 | SKIP 314 | PASS 11755`; the other nine legs pass. AC1 fails on Windows by defect return #2's own mechanism: `file.symlink()` succeeds on the runner so T13's `skip_if_not()` guard never fires, and `file.exists()` is TRUE for a dangling symbolic link there, so `files[file.exists(files) & !dir.exists(files)]` (`R/ffm_jobs.R:110`) keeps `broken.mp4` as a row and the all-dangling-link directory returns a tibble where the zero-match refusal was asserted (`test-ffm-jobs.R:216,217,234,235`). `test-ffm-jobs.R:218` passing is the direct evidence for the `file.exists()` premise. **Defect-return count: 3**; thrash trigger (a) fires at its threshold and trigger (b) fires again, the recorded alternative already spent at return #2's gate. Status -> in-progress; PR #125 stays open.
- 2026-09-10: **thrash trigger (a) gate** — presented with descope-or-park, the `/milestone-brief` escalation that is what remains of trigger (b), and a never-recommended same-objective re-cut. The user chose **descope**: a gated AC1 amendment (`/milestone-implement` step 6) bounding the criterion's platform domain to the CI legs that pass, with the Windows dangling-symlink case exiting to M122 (planned, `macOS and Windows run the package's FFmpeg code`) or to a candidate row, then a re-review of the narrowed set. The ground: the repair `file.exists()` needs on Windows is a platform-semantics question M122 already owns, and three returns on one criterion are what the threshold exists to stop. Falsified by the narrowed AC1 failing on a passing leg, which would be a fourth return with the descope already spent.

- 2026-09-10: **substantive plan amendment — AC1 narrowed, executing the trigger (a) descope.** Two clauses narrow and nothing widens. The contents clause goes from "the media files" to the non-hidden files whose extension the `type`/`extension` arguments select from the closed vocabulary `media_extensions()` holds — which is what decision M121-1 actually decided, and what makes the deferred candidate items (b) (hidden files) and (c) (`ts`/`ogv`) scope lines rather than standing falsifiers of AC1 on the legs that pass. The platform clause states the exists-and-not-a-directory promise on macOS, Linux and Windows alike with one disclosed carve-out — Windows reports a symbolic link whose target is absent as existing, so such a link may be a row there — and names the `ffm_jobs()` candidate row item (f) as that case's exit. Mini gate: the third wording was chosen over a minimal carve-out-only fix and over `/milestone-brief` escalation. The audits' Q1(b) — that nothing in the amended criterion forces the four Windows assertions to stop failing — is closed by T17's `skip_on_os("windows")` guard and by `/milestone-review`'s merge step reading `gh pr checks 125`, not by a sixth criterion: D-118 makes widening the criteria set on a thrice-returned milestone the non-recommended direction, and a test-harness property is an instrument property.
- 2026-09-10: re-audit: AC1 (full) — four findings and a widening flag on the first amended wording. The binding clause was written as a `file.exists()` condition, which is the deliverable's own predicate at `R/ffm_jobs.R:117`, so no state of the world could falsify the dangling-link half; the domain "platforms" was enumerated by no procedure the criterion named; the clause stated a property of an instrument (`file.exists()`, which is also the evidence instrument return #3's diagnosis rested on) rather than of the returned tibble; the unconditional `@return` and `NEWS.md` promises about dead symbolic links would be false on Windows with no criterion requiring the limit be disclosed; and the wording was not a pure narrowing, since it asserted over every platform with POSIX `file.exists()` semantics while exempting by behavior rather than by leg. "This repo's ten CI legs" was also wrong for the axis it was counting: `.github/workflows/R-CMD-check.yaml` has six `R CMD check` legs, one of them Windows. All fixed before any text reached this file.
- 2026-09-10: re-audit: AC1 (full) — four findings on the second wording, the once re-entry, by its own fresh reader. Load-bearing: the second sentence removed Windows from the whole "media files" clause rather than from the one `file.exists()` case, so a Windows regression returning an extension-named subdirectory would also have satisfied AC1; "the media files" still quantified over a file set no named procedure enumerates and which candidate items (b)/(c) record as already false on the passing legs; `gh pr checks 125` inside the criterion made satisfaction a property of one PR's mutable dashboard, unreadable once the branch is deleted; and the descope's exit was recorded only in the work log, so the archived criterion would not carry the open edge. All four are folded into the wording above. Two `re-audit: AC1` lines now stand, so the once re-entry is spent and further churn on this criterion goes to the user.

- 2026-09-10: T17 — the two dangling-symbolic-link test blocks take `skip_on_os("windows")` above their existing `skip_if_not(isTRUE(linked))`, which never fires on the runner because `file.symlink()` succeeds there; each guard's comment names AC1's carve-out and candidate item (f). The first block's relative-directory absoluteness assertion was split out into its own unguarded test — AC1 still promises "full paths" on Windows, so guarding the whole block would have dropped the only test of that clause on the one platform it was measured failing others on. `@return` and the `NEWS.md` entry both stop promising unconditionally that a dead symbolic link is never a row: each now says so for macOS and Linux and states that Windows reports such a link as existing. `devtools::document()` rewrote `man/ffm_jobs.Rd`. The `ffm_jobs()` candidate row gains item (f) with the mechanism, the measurement, the descope, the repair shape and its own promotion condition; search-first found no other row or archive summary naming a Windows symlink. `ROADMAP.md` is now 59 lines / 40,926 bytes against its 60-line, 24,000-byte budget — the byte overrun M120's hygiene stamp records, worsened again by this item; `/cairn-triage` remains the only remedy. `testthat::test_local(filter = "ffm-jobs")` passes 143 assertions with no failures and no skips on macOS.

- 2026-09-10: claim audit: 63 claims read, 3 corrected — tests/testthat/test-ffm-jobs.R, R/ffm_jobs.R, NEWS.md, vignettes/metadata.Rmd. Fresh-context [O] reader, authored none of the diff's added lines; it ran `ffm_jobs()` over fixture directories for hidden files, unlisted containers and the symlink cases, called all fifteen `*_batch()` verbs and read each refusal, checked `man/ffm_jobs.Rd` line by line against the roxygen, and ran the test file with `ffmpeg`, `ffprobe` and `mediainfo` off `PATH` (143 assertions, zero skips). The three corrections: the test file's header said "the three `*_batch()` verbs" where the file now calls fifteen (stale since T12); `@return`'s "one row per matching file" and `NEWS.md`'s "lists the media files" both promised over hidden files, which `list.files(all.files = FALSE)` never returns — candidate item (b)'s own case, so the sentences were the promise it falsified; and `vignettes/metadata.Rmd`'s "lists every video file in the folder" is false of `.ogv`, `.3gp`, `.mxf` and `.vob`, which are video files the closed vocabulary omits. The Windows half of the new symlink qualification is marked UNVERIFIABLE-HERE and was not asserted by the reader — it was read against the recorded return-#3 evidence instead, where `test-ffm-jobs.R:218` passing is what shows `file.exists()` is TRUE for a dead link on that runner. Re-read once: claims 1 and 2 returned TRUE-AS-WRITTEN; claim 3's `NEWS.md` half was still overclaiming, since a `.ogv` is a non-hidden file of media type video and is not listed, so the entry took the re-read's narrowest true wording — the non-hidden files carrying one of the extensions it knows for a given type. The vignette half returned TRUE-AS-WRITTEN. Deviation, as at all three earlier audits: the re-read was by a second fresh [O] reader rather than the first, because this session exposes no tool for continuing a finished subagent; a fresh reader is at least as independent, and the one-pass stopping rule was honored.
- 2026-09-10: T18 — `devtools::check()` on the corrected tree: Status OK, 0 errors / 0 warnings / 0 notes, 7m 41.7s, `checking examples ... OK`, `checking tests ... OK`, `re-building of vignette outputs ... OK`. `pkgdown::check_pkgdown()` "No problems found". `verify` slot clean: `devtools::test()` FAIL 0 | WARN 12 | SKIP 5 | PASS 13409 (13406 at the return, +3 from the split absoluteness test); `devtools::document()` re-run produces no diff. `cairn_validate.py` exit 0 with one `sizing` advisory at 18 tasks; `binding criteria` passes on the amended AC1. AC5 met on the descoped tree. Status -> review.

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

### Third pass — 2026-09-10

Reviewed on branch `m121-directory-to-jobs-tibble` at abf66e2, 11 commits ahead of
`origin/master` and **0 behind** — `origin/master` is still cc4761b, the cut point, so
no merge was needed. Branch pushed; PR #125 was already open in draft, so no
`gh pr create`.

**Fencing note.** AC2-AC5 arrived carrying the second pass's ticks, but T13-T16 changed
the tree under them, so that evidence was stale. All five boxes were unticked before
this pass and re-ticked one at a time as each criterion's own fresh evidence landed.

- **AC1 — pass.** Fresh fixture directory holding both returned defects' cases at once:
  `a.mp4`, `b.MOV`, `c.wav`, `d.png`, `e.txt`, `sub/f.mkv`, a *subdirectory* named
  `takes.mp4` holding `takes.mp4/g.mp4` (return #1), a dangling symlink
  `broken.mp4 -> ../gone.mp4` (return #2), and a live symlink `link.mp4` into a sibling
  directory's `live.mp4`. `ffm_jobs(d, type = "video")` returns a `tbl_df` whose only
  column is `input`, 3 rows (`a.mp4`, `b.MOV`, `live.mp4`), `file.exists()` TRUE on all
  three, `any(dir.exists())` FALSE, every path absolute — the subdirectory and the
  dangling link are both gone. Called as `ffm_jobs("raw", type = "video")` from the
  parent with a **relative** `directory`, all three rows are still absolute and all
  three exist (return #2's second half). `recursive = FALSE` → 3 rows;
  `recursive = TRUE` → 5, adding `f.mkv` and `g.mp4`, and the `FALSE` set is a subset
  of the `TRUE` set. `extension = ".mp4"` narrows to 2 rows; `type = "audio"` returns
  `c.wav` and `type = "image"` `d.png`, neither returning `e.txt`. Hand-off:
  `jobs$output` derived from `input`, then
  `ffm_batch(jobs, run = FALSE, .f = function(input, output, ...))` over the
  **unreshaped** table returns `input, output, command`, its `input` column
  `identical()` to the column passed in, and each row's `command` names that row's own
  input and its own output — 3 of 3. **Discriminating control:** an `.f` ignoring its
  arguments and always building row 1 matches 1 of 3, so the criterion separates the
  two cases.
- **AC2 — pass.** Nineteen refusal branches fired and each condition's
  `conditionCall()` read with `rlang::call_name()`: **every one blames `ffm_jobs`**, 0
  exceptions and 0 silent successes. The three AC2 names: a directory that does not
  exist (`` `directory` does not name an existing directory ``), a type outside the set
  (`` `type` must be one of "video", "audio", or "image", not "sound" ``), and a call
  matching no file (`No video files were found in …`). The further branches T3/T8/T15
  require: an all-dangling-link directory (refuses rather than returning zero rows), a
  path that exists but is not a directory, `directory` non-string / `NA` / `""`, `type`
  missing / non-string / `NA_character_` / multi-valued in **both** orders
  (`c("video","audio","image")` and `c("audio","video")` alike abort with
  `` `type` must be a single string, not a character vector ``), `recursive`
  non-logical, `extension` non-character / a factor / zero-length / outside its type.
  The extension refusal's grammar agrees in number: one unknown →
  `"wav" is not one of them.`, two → `"wav" and "png" are not among them.`
  D087 holds: `names(formals(ffm_jobs))` is `directory, type, extension, recursive` —
  no published `call` formal.
- **AC3 — pass.** `_pkgdown.yml:57` carries the `ffm_jobs` reference row;
  `man/ffm_jobs.Rd` (89 lines) has `\value{}` at :25 and `\examples{}` at :49;
  `NAMESPACE:30` is `export(ffm_jobs)`. All four files' first branch commit is the same
  one — `git log --format=%h origin/master..HEAD -- <file> | tail -1` gives fc007d3 for
  `R/ffm_jobs.R`, `NAMESPACE`, `man/ffm_jobs.Rd` and `_pkgdown.yml` alike. `R CMD
  check`'s `checking examples ... OK` is the runnable reading.
- **AC4 — pass.** The criterion's line numbers are dated addresses ("as measured
  2026-09-07"), and at `origin/master` they are exactly the two `list.files()` sites:
  `git grep -n 'list\.files' origin/master -- vignettes/` returns `metadata.Rmd:121`
  and `workflow.Rmd:40`, and nothing else. Both are now `ffm_jobs()` calls —
  `workflow.Rmd:40` is `jobs <- ffm_jobs("study/raw", type = "video")`, and the
  metadata call is `files <- ffm_jobs("my/videos", type = "video")$input`, which T10's
  two added prose lines moved from :121 to :123. `grep -rn "list\.files" vignettes/`
  returns nothing (exit 1), so no third hand-rolled listing remains.
- **AC5 — pass.** `devtools::check()` on the branch head: `Status: OK`,
  `0 errors ✔ | 0 warnings ✔ | 0 notes ✔`, 7m 47.3s, R CMD check on
  tidymedia 0.1.0.9000, `checking examples ... OK`, `checking tests ... OK`
  (373s), `checking re-building of vignette outputs ... OK`. `verify` slot clean:
  `devtools::test()` `FAIL 0 | WARN 12 | SKIP 5 | PASS 13406`;
  `devtools::document()` re-run produces no diff (`git status --porcelain` empty).

#### Consistency gate — pass

- `cairn_validate.py` exit 0. All 16 PASS checks pass — including `coverage
  complete`, `binding criteria`, `scaffold present` and `profile valid` — with one
  advisory warning, `sizing (split tripwires)`: M121 carries 16 tasks against the
  10-task tripwire, the accumulation of two defect returns. Advisory, not a gate
  failure. The `release window` advisory did **not** fire.
- `cairn_impact.py` skipped: no `DESIGN.md` principle changed
  (`git diff --name-only origin/master..HEAD` lists no `cairn/DESIGN.md`).
- Toolchain checks from `cairn/PROFILE.md`'s `consistency-gate` slot:
  `document()` no diff ✔ · generated files not hand-edited (the 22 other `man/*.Rd`
  touches are roxygen `@family` index regeneration) ✔ · `README.Rmd`/`README.md`
  untouched by this branch and both last written by 0df9835, so in sync ✔ ·
  `pkgdown::check_pkgdown()` "No problems found" ✔ · `NEWS.md` carries the
  user-visible entry under the development heading and
  `grep -nE '\bM[0-9]{2,3}\b' NEWS.md` finds no milestone numbers (exit 1) ✔ ·
  no new top-level files, so no `.Rbuildignore` entry needed ✔ ·
  `devtools::check()` 0/0/0 ✔.

#### Independent review — full three-lens fan-out (surface tier: user-facing)

**[S] blame-history — no regression.** `R/ffm_jobs.R` is a new file, so the only
history to judge against is the branch's own: `df0ea93` added `!dir.exists(files)`
for return #1 and `73fdc1a` widened it to `file.exists(files) & !dir.exists(files)`
for return #2 — each strictly additive to the predicate, neither reverting the
other or the T1-T5 baseline. `media_types()`/`media_extensions()` collide with no
existing internal. D079 and D087 are real entries and the code complies with both.
No `LESSONS.md` line names this file or pattern. Zero findings.

**[S] prior-review record — no prior-review evidence, zero findings.** No archived
`## Review` section names `ffm_jobs`, `R/ffm_jobs.R` or the directory-listing
pattern; the function is new in M121. The two nearest LESSONS lines were checked
and are not violated (M103's `list.files(recursive = TRUE)` symlink-descent lesson
is about deletion, which this export never does; M109's shared-derived-output
fixture trap does not recur — the tests derive `output` from distinct basenames).
The GitHub probe (`gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1`)
returned `[]`, so the per-PR walk was skipped per the recipe's probe gate. The
lens also re-verified all five of pass 2's findings: 1-4 fixed at HEAD, 5 still
open as the pre-existing item pass 2 deferred.

**[O] diff-bug — 11 findings, ranked.** The T13-T16 fixes were re-verified by
execution and are correct. Each finding below was reproduced by the reviewer and
re-reproduced here at the gate.

1. **CONFIRMED — under `recursive = TRUE`, the `output` derivation the reference
   page and the workflow vignette both teach collapses two jobs onto one path.**
   `R/ffm_jobs.R:104-107` with the example at `:43-46` (`man/ffm_jobs.Rd:52-56`).
   Two same-named files in different subdirectories are two rows whose
   `basename()`-derived outputs are identical; nothing downstream refuses it and
   FFmpeg's `-y` makes the second overwrite the first. Reproduced at the gate:
   `s1/a.mp4` + `s2/a.mp4` → 2 rows, `length(unique(derived_output))` is 1 of 2.
   Not the deferred candidate row's symlink item — no symlink is involved and the
   trigger is the advertised `recursive` argument.
2. **CONFIRMED — a resolving symlink under `recursive = TRUE` returns the same
   path twice.** `R/ffm_jobs.R:117`: `normalizePath()` resolves the link, so link
   and target survive the predicate as identical strings. Reproduced at the gate:
   `sub/deep.mp4` plus `live.mp4 -> sub/deep.mp4`, `recursive = TRUE` → 2 rows,
   1 unique; `recursive = FALSE` → 1 row. The same job runs twice, against
   `@return`'s "one row per matching file". The branch's live-symlink test pins
   only `recursive = FALSE`.
3. **CONFIRMED — the `@details` rationale for the one-column tibble contradicts
   `ffm_batch()`'s own documented remedy.** `R/ffm_jobs.R:12` (shipped at
   `man/ffm_jobs.Rd:44`) says an extra column "would become an argument every
   `.f` has to accept"; `R/ffm_batch.R:14` and `vignettes/batch.Rmd:51` both say
   the remedy is a `...` argument, not a named one. The conclusion may stand; the
   reason as written does not.
4. **CONFIRMED — `vignettes/batch.Rmd`, the batch story, never cites the new
   export.** `:39` and `:64` still build the jobs tibble with
   `tibble::tibble(input = …, output = …)` and the file names `ffm_jobs()`
   nowhere (`grep -c` → 0), while `vignettes/workflow.Rmd:73` sends the reader
   there "for the batch model". AC4 is scoped to the two `list.files()` sites, so
   no criterion fails; but the Goal is that the batch story not start hand-rolled,
   and the plan gate's recorded falsifier is "the export going uncited in the
   vignettes and README a release later".
5. **CONFIRMED — `cairn/DESIGN.md:22-29`'s Layer-1 enumeration does not gain
   `ffm_jobs`** (`grep -n ffm_jobs cairn/DESIGN.md` → no hits). Decision M121-1
   justified the `ffm_*` prefix by that section's contents, and that argument
   lives only in the milestone file, which is archived at merge. Pass 2 raised
   this as its finding 5 and it was deferred as pre-existing (`ffm_manifest` is
   likewise absent).
6. **CONFIRMED — two internal helpers break the file's `tm_` naming convention
   and add a fourth hand-written container list.** `R/ffm_jobs.R:138,140`:
   `media_types()` / `media_extensions()` sit beside `tm_ffm_jobs()` unprefixed,
   and `cairn/DESIGN.md`'s Known-issues entry already tracks container names
   written by hand in three places outside the generated enumeration. Nothing
   cross-checks the new list against it.
7. **CONFIRMED — a user-facing help page cites an internal decision id.**
   `R/ffm_jobs.R:21` → `man/ffm_jobs.Rd:14`: "since any default would be one of
   the three (D079)". Pre-existing precedent (`R/program_management.R:293`,
   `man/concatenate_videos_batch.Rd:39`), so a convention question rather than a
   regression.
8. **CONFIRMED — `_pkgdown.yml:32-34`'s Layer-1 section description no longer
   covers its members:** "Assemble a reproducible FFmpeg command step by step"
   where `ffm_jobs` (like `ffm_batch` before it) assembles no command. Cosmetic,
   pre-existing for `ffm_batch`.
9. **CONFIRMED — `extension = "."` refuses by naming an empty string.**
   `R/ffm_jobs.R:88-92`: the `nzchar()` form check runs before the leading dot is
   stripped, so `"."` passes it and then fails the vocabulary check as
   `✖ "" is not one of them.` Cosmetic.
10. **CONFIRMED — `type` is case-sensitive while `extension` is
    case-insensitive.** `R/ffm_jobs.R:76` vs `:93`: `type = "Video"` is refused
    (with a "Did you mean" hint), `extension = "MP4"` accepted. Defensible, but
    the two selector arguments answer the same user slip differently and the docs
    say so for neither.
11. **Disclosed, not new — `cairn/ROADMAP.md` is further past its byte budget.**
    T11's ~1.9 KB candidate row on a file already at 38,389 bytes against 24,000.
    The milestone's own work log records it; `/cairn-triage` is the remedy.

Clean categories the [O] lens checked and cleared: all sixteen-plus refusal
branches blaming `ffm_jobs`, `directory` absent included; the roxygen example
running clean against `inst/extdata`; `list.files` gone from `vignettes/` and
`README.Rmd`; the new test file at 144 assertions with 0 failures, alongside
`test-exported-call-formal` and `test-ffm-batch*`.

#### Return-floor assessment

No finding is floor-qualifying. Findings 1 and 2 are the closest: neither
falsifies AC1 as written. AC1 promises an `input` column of full paths to the
media files plus a demonstrated unreshaped `ffm_batch()` hand-off — finding 1's
collision is in the *derived* `output`, and an output-column convention or
path-deriving helper is explicitly out of this milestone's scope; finding 2's
duplicate row is still a full path to a media file, contradicting `@return`'s
"one row per matching file" rather than the criterion. Both are real
user-visible hazards (a silent overwrite; a job run twice), so the load-bearing
half of the floor is the maintainer's judgment at the gate. No amendment return:
no finding shows a criterion itself to be wrong.

#### Triage at the merge gate — 2026-09-10

The maintainer chose to merge, filing findings 1-4 as candidate rows. Every
finding's disposition:

- **[O]1, [O]2 → follow-up.** Both absorbed into the existing `ffm_jobs()`
  candidate row's item (a), which already held the colliding-`basename()` output
  class from the symlink direction. Search-first: that row exists and is the
  right home, so it was extended rather than duplicated — (a) now names the
  `recursive = TRUE` route to the same collision with no symlink involved, and
  the link-plus-target duplicate row.
- **[O]3, [O]4 → follow-up.** Filed as items (d)/(e) on the same row (the
  60-line ROADMAP cap admitted no second line), with their own promotion
  condition: the next docs pass over either file, or a report of a reader
  hand-building a jobs table after reading `batch.Rmd`.
- **[O]5 → reject, pre-existing.** The `DESIGN.md` Layer-1 enumeration is
  already missing `ffm_manifest`; bringing it current is a sweep of its own
  rather than this milestone's to make. Pass 2 dispositioned it the same way.
- **[O]6 → reject, pre-existing.** `DESIGN.md`'s Known-issues entry already
  tracks hand-written container lists outside the generated enumeration; the new
  list joins a tracked class rather than opening one.
- **[O]7 → reject, pre-existing convention.** `R/program_management.R:293` and
  `man/concatenate_videos_batch.Rd:39` cite decision ids the same way; whether
  user-facing pages should is one decision to take once, not here.
- **[O]8 → reject, pre-existing and cosmetic.** The `_pkgdown.yml` Layer-1
  description already failed to cover `ffm_batch`.
- **[O]9 → reject, cosmetic.** `extension = "."` is refused; only the message's
  rendering of the empty string is odd.
- **[O]10 → reject, intentional.** `arg_match()` supplies `type`'s "Did you
  mean" hint on the exact-case refusal, and `extension`'s case-folding is what
  lets `"MP4"` and `".mp4"` both work — the asymmetry is the two arguments'
  different jobs.
- **[O]11 → reject, disclosed not new.** The ROADMAP byte overrun is recorded in
  the work log and in M120's hygiene stamp; `/cairn-triage` is the remedy and is
  the user's to run.

**PR conversation read** (PR #125, immediately before the merge chip):
`gh api .../pulls/125/reviews` returned no reviews; the `reviewThreads` GraphQL
query filtered to `isResolved: false` returned no threads; `issues/125/comments`
returned one comment.

- conversation: codecov[bot] PR — noted (reports all modified and coverable
  lines covered by tests; requests nothing). Author `type` is `Bot`, so the
  blocking rule does not apply.

No `CHANGES_REQUESTED` review, so merge stayed the recommended option.

#### Defect return #3 — red CI on `windows-latest (release)`

The merge was approved and the marker written, but `gh pr checks 125` came back
red: nine legs pass (`macos-latest release`, `ubuntu-latest` release/devel/
oldrel-1/4.1.0, `pkgdown`, `test-coverage`, both codecov contexts) and
`windows-latest (release)` fails after 16m4s with `Status: 1 ERROR`,
`checking tests`, `FAIL 4 | WARN 0 | SKIP 314 | PASS 11755`. The marker was
deleted unused; no merge was attempted.

**AC1 fails on Windows, by the mechanism defect return #2 was meant to close.**
T13's guard `skip_if_not(isTRUE(linked))` does not fire on the runner —
`file.symlink()` succeeds there — and `file.exists()` returns TRUE for a
dangling symbolic link on Windows, so `files[file.exists(files) &
!dir.exists(files)]` (`R/ffm_jobs.R:110`) keeps it. The four failures:

- `test-ffm-jobs.R:216` — `"broken.mp4" %in% basename(jobs$input)` is TRUE where
  FALSE was asserted.
- `test-ffm-jobs.R:217` — `basename(jobs$input)` is
  `"a.mp4", "b.MOV", "broken.mp4"` against an expected `"a.mp4", "b.MOV"`.
- `test-ffm-jobs.R:234` — the all-dangling-link directory returns a `tbl_df`
  where an `error` was asserted, so the zero-match refusal never fires.
- `test-ffm-jobs.R:235` — the consequent error, `conditionCall()` applied to
  that tibble.

`test-ffm-jobs.R:218` (`all(file.exists(jobs$input))`) **passed**, which is the
direct evidence that `file.exists()` is TRUE on the dead link there — the
predicate is not wrong about its own premise, the premise is not portable.

AC1 names no platform, so it quantifies over the platforms the package's own CI
matrix checks, Windows among them. The failure is inside that domain: a returned
`input` row that is not a readable media file. Floor-qualifying. Status returns
to `in-progress`; AC1's box is unticked, AC2-AC5 keep the ticks this pass's
evidence earned. PR #125 stays open.

**Defect-return count for M121: 3.** **Thrash trigger (a) fires** — the third
return, a threshold that now holds: no further retry under the current plan is
queued, and descope-or-park is the disposition. **Trigger (b) fires again** —
AC1 has now failed three times, each a path that is not a readable media file
returned as an `input` row (an extension-named subdirectory, a dangling symlink
on POSIX, the same dangling symlink on Windows). The alternative the plan gate
recorded against — refusing the feature under GP1 — was already reconsidered and
declined at return #2's gate, so what remains of (b) is the `/milestone-brief`
escalation offer, carried into the composed disposition. No re-plan or split has
been spent on this milestone, so a same-objective re-cut stays a present option,
never the recommended one.

### Fourth pass — 2026-09-10 (the descoped tree)

Reviewed on branch `m121-directory-to-jobs-tibble` at be2acff, 19 commits ahead of
`origin/master` and **0 behind** (`git rev-list --left-right --count
origin/master...HEAD` → `0 19`), so the default branch had not moved and no merge
was needed. Two commits (T17, T18) are unpushed; PR #125 stays open, so step 8
pushes without a `gh pr create`. This is the first pass over the **amended AC1**,
whose platform clause now discloses the Windows dangling-symbolic-link carve-out.

**Fencing note.** All five boxes were unticked before this pass — AC2-AC5 carried
the third pass's ticks, but T17/T18 changed the tree under them (test guards,
`@return`, `NEWS.md`, `man/ffm_jobs.Rd`), so that evidence was stale. Each box
below was re-ticked as its own fresh evidence landed.

- **AC1 — pass on the passing legs; the Windows leg is step 8's CI gate.** Fresh
  fixture: `a.mp4`, `b.MOV`, `c.wav`, `d.png`, `e.txt`, `.hidden.mp4`, `g.ogv`,
  `target.mp4`, a subdirectory `takes.mp4/` holding `inner.mp4`, `sub/f.mkv`, a
  live link `live.mp4 → target.mp4`, and a dangling link `broken.mp4` whose target
  was removed after linking (`file.symlink()` returned TRUE for both, so no guard
  silently skipped the case). `ffm_jobs(d, type = "video")` returns a `tbl_df`
  whose only column is `input`, 4 rows (`a.mp4`, `b.MOV`, `target.mp4`,
  `target.mp4`), **every path absolute, `all(file.exists())` TRUE,
  `any(dir.exists())` FALSE**. Each clause of the amended contents wording checked
  against its own control: the hidden `.hidden.mp4` is **not** a row; the
  extension-named subdirectory `takes.mp4` is **not** a row; the dangling
  `broken.mp4` is **not** a row; `g.ogv` — a video file whose extension the closed
  vocabulary omits — is **not** a row; and every returned extension (`mp4`, `mov`)
  is in `media_extensions("video")`. Selection: `extension = ".mp4"` narrows to 3
  rows, `recursive = FALSE` ⊂ `recursive = TRUE` (`all(FALSE_set %in% TRUE_set)`
  TRUE; `TRUE` adds `f.mkv` and `inner.mp4`), `type = "audio"` returns `c.wav` and
  `type = "image"` `d.png`, neither returning `e.txt`. The two `target.mp4` rows
  are the link-and-its-target duplicate that candidate item (a) already holds —
  both are full paths to existing non-directory files, so AC1 as written is not
  falsified by them. Hand-off: `jobs$output` derived from `input`, then
  `ffm_batch(jobs, run = FALSE, .f = function(input, output, ...))` over the
  **unreshaped** table returns `input, output, command`, its `input` column
  `identical()` to the one passed in, and each row's `command` names that row's own
  input and its own output — **4 of 4**. **Discriminating control:** an `.f` that
  ignores its arguments and always builds row 1 matches **1 of 4** — the criterion
  separates the two cases. Measured on macOS. The Linux and Windows halves of the
  platform clause are established by the six `R CMD check` legs of
  `.github/workflows/R-CMD-check.yaml`, which step 8's `gh pr checks 125 --watch`
  gates on: the tree under review guards the two dangling-link blocks with
  `skip_on_os("windows")`, which is what defect return #3's four failing assertions
  required, and no merge happens on a red Windows leg.
- **AC2 — pass.** Seventeen refusal paths fired with `rlang::catch_cnd()` and each
  condition's `conditionCall()` read; **every one blames `ffm_jobs`.** The three
  AC2 names: a directory that does not exist (`` `directory` does not name an
  existing directory ``), a type outside the accepted set (`` `type` must be one of
  "video", "audio", or "image", not "sound" ``), and a call matching no file
  (`No video files were found in …`, with an `i` bullet listing the twelve video
  extensions). The further branches T3/T8/T15 require, all likewise blaming
  `ffm_jobs`: a path that exists but is not a directory, `directory` non-string,
  `directory = ""`, `type` missing, `type` non-string, `type = NA_character_`,
  `type` multi-valued in both orders (`c("video","audio","image")` and
  `c("audio","video")` → `` `type` must be a single string, not a character
  vector ``), `recursive` non-logical, `extension` non-character, `extension` as a
  factor, `extension` zero-length, and `extension` outside its type. The extension
  refusal's number agreement holds in both directions: one unknown →
  `"wav" is not one of them.`, two → `"wav" and "png" are not among them.` D087
  holds: `names(formals(ffm_jobs))` is `directory, type, extension, recursive` —
  no published `call` formal.
- **AC3 — pass.** `_pkgdown.yml:57` carries the `ffm_jobs` reference row;
  `man/ffm_jobs.Rd` (91 lines after T17's `@return` rewrite) has `\value{}` at :25
  and `\examples{}` at :51; `NAMESPACE:30` is `export(ffm_jobs)`. All four files'
  first branch commit is the same one — `git log origin/master..HEAD --reverse --
  <file>` gives fc007d3 for `R/ffm_jobs.R`, `NAMESPACE`, `man/ffm_jobs.Rd` and
  `_pkgdown.yml` alike. The example is runnable: `devtools::run_examples()`
  executed it verbatim, returning the one-row `inst/extdata/sample.mp4` table and
  compiling a command through `ffm_batch(run = FALSE)`; `R CMD check`'s
  `checking examples ... OK` is the second reading.
- **AC4 — pass.** The criterion's line numbers are dated addresses ("as measured
  2026-09-07"), and at `origin/master` they are exactly the two `list.files()`
  sites: `git grep -n 'list\.files' origin/master -- vignettes/` returns
  `metadata.Rmd:121` and `workflow.Rmd:40`, and nothing else. Both are now
  `ffm_jobs()` calls: `workflow.Rmd:40` is
  `jobs <- ffm_jobs("study/raw", type = "video")` and the metadata call is
  `files <- ffm_jobs("my/videos", type = "video")$input`, which T10's two added
  prose lines moved from :121 to :123. `grep -rn "list\.files" vignettes/` returns
  nothing (exit 1), so no third hand-rolled listing remains.
- **AC5 — pass.** `devtools::check(document = FALSE)` on the branch head:
  `Status: OK`, `0 errors ✔ | 0 warnings ✔ | 0 notes ✔`, 5m 38.5s, R CMD check on
  tidymedia 0.1.0.9000, with `checking examples ... OK`, `checking tests ... OK`
  (252s) and `checking re-building of vignette outputs ... OK`. `verify` slot
  clean: `devtools::test()` `FAIL 0 | WARN 12 | SKIP 5 | PASS 13409`;
  `devtools::document()` re-run leaves `git status --porcelain` empty, so no diff.

#### Consistency gate — pass

- `cairn_validate.py` exit 0. All 16 PASS checks pass — including
  `coverage complete`, `binding criteria` (on the amended AC1), `scaffold present`
  and `profile valid` — with one advisory warning, `sizing (split tripwires)`:
  M121 carries 18 tasks against the 10-task tripwire. Advisory, not a gate
  failure. The `release window` advisory did **not** fire.
- `cairn_impact.py` skipped: no `DESIGN.md` principle changed
  (`git diff --name-only origin/master..HEAD` lists no `cairn/DESIGN.md`).
- Toolchain checks from `cairn/PROFILE.md`'s `consistency-gate` slot:
  `devtools::document()` no diff (`git status --porcelain` empty) ✔ · generated
  files not hand-edited — the 22 other `man/*.Rd` touches are roxygen `@family`
  index regeneration, and `man/ffm_jobs.Rd` regenerated from T17's `@return`
  rewrite ✔ · `README.Rmd`/`README.md` untouched by this branch and both last
  written by 0df9835, so in sync ✔ · `pkgdown::check_pkgdown()` "No problems
  found" ✔ · `NEWS.md` carries the user-visible entry at :260-278 and
  `grep -nE '\bM[0-9]{2,3}\b' NEWS.md` finds no milestone numbers ✔ · no new
  top-level files, so no `.Rbuildignore` entry needed ✔ ·
  `devtools::check()` 0 errors / 0 warnings / 0 notes ✔.

#### Independent review — full three-lens fan-out (surface tier: user-facing)

**[S] prior-review record — no regression.** Primary surface: this milestone's own
three prior `## Review` passes, its four claim audits, and `cairn/LESSONS.md`. The
diff matches each prescribed fix rather than contradicting it: the
`file.exists() & !dir.exists()` predicate return #2 specified, the Windows
carve-out disclosed in `@return`/`NEWS.md` per the descope, `skip_on_os("windows")`
above the `skip_if_not(isTRUE(linked))` guards per T17, the test header's
"fifteen" verbs, and the `setequal()` attribution in the `check_string()` comment.
The GitHub probe (`gh api .../pulls/comments?per_page=1`) returned `[]`, so the
per-PR walk was skipped. Zero findings.

**[S] blame-history — one finding, no regression of runtime code.** The branch
modifies no pre-existing runtime line; D079, D087 and D001/GP1 are followed as
cited; `NEWS.md` holds one new entry and restates nothing; the 22 `man/*.Rd`
touches are one-line `@family` link insertions.

- **S1 — `recursive = TRUE` descends through a directory symbolic link, so an
  `input` row can name a file outside `directory`** — the mechanism the 2026-09-03
  (M103) LESSONS line warns about, built into new exported surface with no test and
  no disclosure. **Verified:** `inside/a.mp4` plus `inside/link → ../outside`
  holding `escaped.mp4`; `ffm_jobs(inside, type = "video", recursive = TRUE)`
  returns `.../inside/a.mp4` and `.../outside/escaped.mp4`; `recursive = FALSE`
  returns `a.mp4` alone. Read-only here — no deletion hazard as in M103 — and the
  row exists, is not a directory, and carries a vocabulary extension.

**[O] diff-bug — 6 findings, ranked by the reviewer.** Each re-run against the
implementation here, not taken from the reviewer's account.

1. **O1 — CONFIRMED: `media_extensions("audio")` omits `.mka`, the container
   tidymedia itself tells users to write.** `R/ffm_jobs.R:147-148`'s audio list is
   `wav, mp3, m4a, aac, flac, ogg, oga, opus, wma, aiff, aif`, while
   `multi_audio_extensions` (`R/ffmpeg.R:671`) holds `mka` first and
   `separate_audio_video()`'s message at `R/ffmpeg.R:799` recommends Matroska
   (`.mka`). A folder of the package's own multi-track audio output is invisible to
   `ffm_jobs(type = "audio")`, and `extension = "mka"` is refused with an `i` bullet
   that omits it. A new mechanism beside candidate item (c): the gap contradicts a
   vocabulary the package already ships, not merely a neighbour's extension.
2. **O2 — PREMISE CONFIRMED, WINDOWS BEHAVIOUR UNVERIFIED HERE: "non-hidden" in
   `@return`, `NEWS.md` and AC1 is dot-prefix visibility only.** `?list.files`
   defines `all.files = FALSE` as "following Unix-style visibility, that is files
   whose name does not start with a dot" (read from `base`'s Rd), so a Windows file
   carrying the hidden *attribute* without a leading dot is returned. AC1's
   contents clause carries no platform carve-out and names no procedure defining
   "hidden". Not executed: no Windows machine in this environment.
3. **O3 — CONFIRMED: the `ffm_*` prefix on a non-engine utility is decided only in
   the work log.** D014 says "`ffm_*` marks Layer-1 engine surface only; nothing
   outside Layer 1 uses it"; milestone-local decision M121-1 places `ffm_jobs()`
   there, and `grep -n "M121-1\|ffm_jobs" cairn/DECISIONS.md cairn/DESIGN.md`
   returns nothing. The `DESIGN.md` Layer-1 enumeration half was rejected as
   pre-existing at pass 3 ([O]5, `ffm_manifest` already absent); the D-entry half
   is the new part.
4. **O4 — CONFIRMED: the vignettes' "one-liner" now aborts where `list.files()`
   returned `character(0)`** (`vignettes/metadata.Rmd:123`,
   `vignettes/workflow.Rmd:40`). A folder of only unlisted containers errors out of
   a chunk sold as a one-liner. Documented contract (`@return`: "aborts rather than
   returning zero rows"); both chunks are `eval = FALSE`.
5. **O5 — CONFIRMED, cosmetic: the extension refusal echoes the normalized form.**
   `extension = ".WAV"` → `✖ "wav" is not one of them.`
6. **O6 — by construction, no user-visible defect: `mustWork = FALSE` at
   `R/ffm_jobs.R:129` is unreachable leniency** on macOS/Linux after the
   `file.exists() & !dir.exists()` filter; its only remaining effect is the Windows
   dangling-link pass-through item (f) already holds.

Checked and clean by the [O] lens: the AC1 hand-off with `ffmpeg` off `PATH`; AC2
blame including `check_required()`'s frame; the `NEWS.md` six-and-nine split
against all fifteen `*_batch()` verbs; no overwrite hazard in the six advertised
verbs; the superset property; `_pkgdown.yml`, `\value{}`, the runnable example.

#### Return-floor assessment

No finding demonstrates AC1 failing on a measured case, so none is floor-qualifying
by demonstration; two sit close enough that the disposition is the maintainer's.

- **S1** returns an existing, non-directory, vocabulary-extension file — every
  property AC1's binding sentence names — at a path outside `directory`. The first
  pass confirmed the same class by the file-link route (pass 1 [O]4, "`input` can
  point outside `directory`") and it was deferred into candidate item (a), not
  returned; this is a second route to that class.
- **O2** is unverified by execution, and it bears on the word "non-hidden", which
  AC1 does not define. If the maintainer reads it as showing the criterion
  unbounded (it names no procedure deciding "hidden"), it is an **amendment
  return** — none is yet recorded on M121, so no second-occurrence stop applies.
  Otherwise its repair is a claim correction in `@return` and `NEWS.md`.
- **O1, O3-O6** are follow-up or reject material: none touches a returned row or a
  refusal's frame.

Defect-return count stays **3**; amendment-return count **0**.
