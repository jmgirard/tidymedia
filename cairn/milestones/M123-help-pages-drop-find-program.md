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

- [x] AC1: `grep -rn 'find_program' man/ vignettes/*.Rmd README.Rmd README.md _pkgdown.yml` returns no line.
- [x] AC2: With the package installed, `length(help("find_program", package = "tidymedia")) == 0L`, `length(help("find_ffmpeg", package = "tidymedia")) == 1L`, and the usage section of `tools::Rd_db("tidymedia")[["find_ffmpeg.Rd"]]` lists `find_ffmpeg()`, `find_ffprobe()`, `find_ffplay()` and `find_mediainfo()`.
- [x] AC3: The "Other program management functions" list in each of `man/program_status.Rd`, `man/set_program.Rd`, `man/unset_program.Rd` and `man/install_on_win.Rd` contains `\link[=find_ffmpeg]{find_ffmpeg()}`.
- [x] AC4: Of the lines `grep -n "#'.*find_program" R/program_management.R` returns at `0c4299c`, each one other than the four `@rdname find_program` tags (`:193`, `:218`, `:274`, `:283`, `:303`, `:431`, `:451`) has, in `git diff --word-diff 0c4299c -- R/program_management.R`, only its function reference replaced — by `find_ffmpeg()`, `find_ffmpeg()` and its siblings, or the `find_*()` function for the program the sentence's example names — plus the words that must agree with it and any re-wrapping.
- [x] AC5: The `NEWS.md` bullet on the find-a-program reference page (`NEWS.md:1332-1335` at `0c4299c`) names that page `?find_ffmpeg` and says `?find_program` no longer opens it.
- [x] AC6: `devtools::check()` reports 0 errors and 0 warnings, and `pkgdown::check_pkgdown()` reports no problems.

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

Sync: branch merge-base equals `origin/master` (`5a4b7be`) on 2026-09-10; no merge needed. `devtools::document()` on HEAD `213df1f` left `git status` clean.

- AC1: the grep as written, run on HEAD `213df1f`, returned no line (exit 1) over 83 `man/` files plus 8 named files/globs, all present; the same grep over `master:man/program_status.Rd` returns two `find_program` lines, so it can fail.
- AC2: `R CMD INSTALL` of HEAD `213df1f` into a scratch library, then `R_LIBS=<scratch>` Rscript with `find.package("tidymedia")` confirming the scratch copy loaded: `help("find_program")` length 0, `help("find_ffmpeg")` length 1, `Rd_db()` has `find_ffmpeg.Rd` and no `find_program.Rd`, and the `\usage` section holds `find_ffmpeg()`, `find_mediainfo()`, `find_ffprobe()`, `find_ffplay()`.
- AC3: on HEAD `213df1f` after `document()`, the block from "Other program management functions:" to its closing `}` opens with `\code{\link[=find_ffmpeg]{find_ffmpeg()}}` in `program_status.Rd:57`, `set_program.Rd:68`, `unset_program.Rd:52` and `install_on_win.Rd:132`.
- AC4: the grep at `0c4299c` returns 11 lines: four `@rdname find_program` (`:153, 161, 169, 177`) and the seven the criterion names. In the word diff against HEAD `213df1f`: `:193` `[find_program()]'s` → `[find_ffmpeg()] and its siblings'`; `:218` and `:451` `[find_program()]` → `[find_ffmpeg()] and its siblings`; `:274` `goes` → `go` and `:431` `reads` → `read` beside the same swap; `:283` the same swap; `:303` `find_program()` → `find_mediainfo()`, the example being `unset_program("mediainfo")`. The only other changes on those lines move words across `#'` line breaks.
- AC5: `git diff 0c4299c -- NEWS.md` changes one hunk, the bullet at `:1332-1335`, now `:1332-1336`: it opens "The reference page for finding a program is now `?find_ffmpeg`" and ends "and `?find_program` no longer opens the page". The added lines hold no milestone id.
- AC6: `devtools::check(error_on = "never")` on HEAD `213df1f`, run 2026-09-10: 0 errors, 0 warnings, 0 notes. Rd files, Rd cross-references, missing documentation entries, tests (`testthat.R`, 470 s elapsed) and vignettes all OK. `pkgdown::check_pkgdown()` on the same tree: "No problems found."

Consistency gate (2026-09-10): `cairn_validate.py` exit 0, every check PASS/OK. No principle touched, so `cairn_impact` skipped. Toolchain slot: `document()` no diff; README.Rmd/README.md untouched by the branch; `check_pkgdown()` clean; NEWS.md carries the entry (AC5) with no milestone id; no new top-level files; `check()` 0/0/0 (AC6).

Independent review: user-facing tier, so all three lenses ran fresh.
- [S] blame-history: no conflicts. Dropping `@usage NULL` (added by M119 `0df9835`) cannot bring back the defect it hid, since the block now sits on an exported function. The definition reorder is the plan's recorded choice. Dropping the alias fits D014, and the NEWS wording fits D091.
- [S] prior-review: no regressions. All eleven M119 [O]7 sites now point at `find_ffmpeg()`; `gh api pulls/comments` probe returned `[]`, so PR threads were skipped.
- [O] diff-bug: no correctness defects, six ranked findings (dispositions logged at the approval gate):
  - [O]1 `R/program_management.R:118-121`: the topic name depends on `find_ffmpeg()`'s block coming first, and only a comment guards it. The reviewer's scratch reorder produced `\name{find_mediainfo}` and family links to `find_mediainfo()`, with no check warning.
  - [O]2 `R/program_management.R:133-134`: the help text says the pre-0.2.0 file "is read only when no file exists in the current directory". Code at `:54-55` reads it only when no file exists under `tools::R_user_dir()`. The wording was already there and moved verbatim; verified at review.
  - [O]3 `NEWS.md:1332`: "is now `?find_ffmpeg`" reads as new, but 0.1.0's page (`4b04fad`) already carried the `find_ffmpeg` alias; verified at review.
  - [O]4 `R/program_management.R:224-226`: the re-wrapped `@seealso` leaves "and" alone on a line.
  - [O]5 `R/program_management.R:199-200`: a possessive apostrophe after a linked name ("siblings': the") reads awkwardly.
  - [O]6 AC4's wording says "the four `@rdname find_program` tags" and then lists seven line numbers, which are the reworded lines. Evidence above names both sets explicitly.
