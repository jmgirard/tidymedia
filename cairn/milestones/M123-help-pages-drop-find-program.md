<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M123: Help pages stop naming the internal find_program()

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** high   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate; RR<NN> whose Binding criteria bind this milestone's ACs (binding-criteria check), or — -->
- **Principles touched:** —   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Resolves:** —   <!-- owner: plan · create/amend-via-gate; comma-separated GitHub issues the scope absorbs, each `#N closes` (the PR closes it at merge) or `#N partial` (the remainder gets a candidate row), or — ; skill conduct only — no validate check parses it -->
- **Surface tier:** user-facing — the installed help pages and the pkgdown reference site   <!-- owner: plan · create/amend-via-gate; user-facing | internal — <one-clause reason>; skill conduct only — no validate check parses it -->
- **Branch/PR:** m123-help-pages-drop-find-program   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

The shipped help pages send readers to `find_ffmpeg()` and its siblings wherever they sent them to the internal `find_program()`.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:** rename the find-a-program help topic from `find_program` to `find_ffmpeg` and drop its `find_program` alias; reword the seven roxygen references to `find_program()` in `R/program_management.R`; amend the `NEWS.md` bullet on that page. At `0c4299c`, man/ holds ten `\link[=find_program]{find_program()}` links (`program_status.Rd:22,51,56`; `set_program.Rd:41,64,68`; `unset_program.Rd:22,32,52`; `install_on_win.Rd:132`) and one plain mention (`unset_program.Rd:42`). Four of the ten are roxygen `@family` lists, whose link text is the topic's first alias, so rewording prose alone cannot remove them.

**Out:**
- A standing test that fails on any help-page link to an unexported function — declined at this plan gate; no row.
- A pkgdown `redirects:` entry for `reference/find_program.html` — declined at this plan gate; no row.
- The comment naming a nonexistent `write_mediainfo_template()` (`R/utils.R`, and its test copy) — fixed directly on master in `9899f26`.
- `find_program()` in R code comments (`R/program_management.R:228, 241, 261, 324-325, 359, 652`) — internal source, not shipped help; they stay.
- The remembered-location read defects inside `find_program()` → candidate row "Two remembered-location paths `find_program()` still reads wrong".

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets. -->

- [ ] AC1: `grep -rn 'find_program' man/ vignettes/*.Rmd README.Rmd README.md _pkgdown.yml` returns no line.
- [ ] AC2: With the package installed, `length(help("find_program", package = "tidymedia")) == 0L`, `length(help("find_ffmpeg", package = "tidymedia")) == 1L`, and the usage section of `tools::Rd_db("tidymedia")[["find_ffmpeg.Rd"]]` lists `find_ffmpeg()`, `find_ffprobe()`, `find_ffplay()` and `find_mediainfo()`.
- [ ] AC3: The "Other program management functions" list in each of `man/program_status.Rd`, `man/set_program.Rd`, `man/unset_program.Rd` and `man/install_on_win.Rd` contains `\link[=find_ffmpeg]{find_ffmpeg()}`.
- [ ] AC4: Of the lines `grep -n "#'.*find_program" R/program_management.R` returns at `0c4299c`, each one other than the four `@rdname find_program` tags (`:193`, `:218`, `:274`, `:283`, `:303`, `:431`, `:451`) has, in `git diff --word-diff 0c4299c -- R/program_management.R`, only its function reference replaced — by `find_ffmpeg()`, `find_ffmpeg()` and its siblings, or the `find_*()` function for the program the sentence's example names — plus the words that must agree with it and any re-wrapping.
- [ ] AC5: The `NEWS.md` bullet on the find-a-program reference page (`NEWS.md:1332-1335` at `0c4299c`) names that page `?find_ffmpeg` and says `?find_program` no longer opens it.
- [ ] AC6: `devtools::check()` reports 0 errors and 0 warnings, and `pkgdown::check_pkgdown()` reports no problems.

## Coverage
<!-- owner: plan · create/amend-via-gate; each acceptance criterion → the
     task(s) satisfying it, by positional number (AC/Task counted
     top-to-bottom). Review reads to fence evidence — tracking-rules "AC fencing". -->

- AC1 → T1, T2, T4
- AC2 → T1, T4
- AC3 → T1, T4
- AC4 → T2
- AC5 → T3
- AC6 → T4

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits); substantive
     change is amend-via-gate. -->

- [x] T1: Move the roxygen block (`R/program_management.R:38-75`) off `find_program()` onto `find_ffmpeg()`, dropping `@usage NULL`; place the `find_ffmpeg()` section first among the four wrappers; point the other three `@rdname` tags at `find_ffmpeg`; run `devtools::document()` and confirm `man/find_program.Rd` is gone and `man/find_ffmpeg.Rd` carries no `find_program` alias. T1 alone leaves seven dangling `find_program` links, which `R CMD check` reports as a WARNING, so run no `devtools::check()` until T2 lands.
- [x] T2: Reword the six `[find_program()]` links and the example comment (`R/program_management.R:193, 218, 274, 283, 303, 431, 451`) as AC4 states; run `devtools::document()`.
- [x] T3: Edit the `NEWS.md:1332-1335` bullet as AC5 states.
- [x] T4: Run the AC1 grep, the AC2 expressions on an installed copy, the AC3 check, `devtools::check()` and `pkgdown::check_pkgdown()`; fix what they report.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates. -->

- 2026-09-10: created by /milestone-plan; absorbs the `[high]` candidate row (M119 review [O]1, [O]7); that row's comment half was committed directly as `9899f26`.
- 2026-09-10: criteria audit, full mode, fresh [O] reader, first draft: 8 findings — T1's route would have named the topic `find_mediainfo`; AC1 bound a test harness; AC1 left `pkg::` link text undefined; T3's plant was one exemplar; AC4's pkgdown clause was already true at HEAD; T2 miscounted sites; a redirect question — fixed, or posed at the gate.
- 2026-09-10: criteria re-audit, full mode, fresh [O] reader, after the gate changed AC1-AC5: 5 findings — AC4 barred `find_mediainfo()` for the `:303` example comment; AC4 needed `--word-diff` to be decidable; D091 points AC5 at announcing the lost `?find_program`; T1 alone leaves a missing-link WARNING until T2; AC2 lacked an exact expression — all taken with the reader's wording.
- 2026-09-10: plan gate chose dropping the `find_program` alias over keeping it so `?find_program` still opens the page (user choice); falsified by a report of a reader failing to reach the page from `?find_program`.
- 2026-09-10: plan gate chose no pkgdown redirect over a `redirects:` entry for `reference/find_program.html` (user choice); falsified by a report of a broken outside link to that page.
- 2026-09-10: plan gate chose fixing the links with one-time greps over a standing test failing on any help-page link to an unexported function (user choice); falsified by another link to an unexported function reaching a shipped help page.
- 2026-09-10: plan chose moving the roxygen block onto `find_ffmpeg()`, placed first, over `@name find_ffmpeg` plus `@aliases NULL` on `find_program()`'s block, because the latter renders the family links as `find_mediainfo()` (scratch-copy measurement, roxygen2 8.1.0); falsified by the chosen route's roxygen output naming any other topic.
- 2026-09-10: implement started on branch `m123-help-pages-drop-find-program`; no open implementation choices, so no question gate.
- 2026-09-10: T1 done — doc block moved onto `find_ffmpeg()`, placed first; `document()` deleted `man/find_program.Rd`, wrote `man/find_ffmpeg.Rd` (`\name{find_ffmpeg}`, aliases ffmpeg/mediainfo/ffprobe/ffplay) and `\link[=find_ffmpeg]{find_ffmpeg()}` into all four family lists; roxygen and comments only, so `devtools::test()` deferred to T2's code-free checkpoint and T4.
- 2026-09-10: T2/T3 edits in; early checks on the working tree — AC1 grep empty; AC4 word-diff changes only the seven references plus agreeing words; AC3 one `find_ffmpeg()` family link in each of the four Rd files; `pkgdown::check_pkgdown()` "No problems found"; AC2 on a scratch-library install: `help("find_program")` length 0, `help("find_ffmpeg")` length 1, usage lists the four calls. A first AC2 run was discarded: `LIB` went in as an argument, not an environment variable, so it read the stale user-library install.
- 2026-09-10: T2, T3 done — `devtools::test()` FAIL 0 | WARN 12 | SKIP 5 | PASS 13409; all 12 warnings are `warn_dropped_audio()` from `test-audio-stream*.R`/`test-ffmpeg.R`, and the branch's only executable R change swaps the order of the `find_ffmpeg()`/`find_mediainfo()` definitions (identical bodies), so none comes from this milestone.
- 2026-09-10: claim audit: 32 claims read, 0 corrected — R/program_management.R, man/find_ffmpeg.Rd, man/install_on_win.Rd, man/program_status.Rd, man/set_program.Rd, man/unset_program.Rd, NEWS.md (reader's caveat on the `find_ffmpeg()` block-order comment: "?find_ffmpeg rather than ?find_mediainfo" holds for `\name` and the family links, while `?find_mediainfo` opens the page under either order as an alias).
- 2026-09-10: T4 done — `devtools::check()` 0 errors | 0 warnings | 0 notes (7m 38.7s) on the tree of `2accfa1`, which holds every R/man/NEWS change on the branch (later commits touch only `cairn/`); AC1-AC3 and `check_pkgdown()` results as logged above; status set to review.

## Decisions
<!-- owner: implement / review · append-only; milestone-local. -->

## Review
<!-- owner: review · exclusive -->
