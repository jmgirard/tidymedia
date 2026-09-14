# M129: The prose sweep reads help pages correctly, and text repeated across the task function pages is written once, in plain English

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — shared help text that renders on 34 task function pages
- **Branch/PR:** `m129-sweep-shared-task-help`

## Goal

Text repeated across the task function help pages is written once, in plain English, and the prose sweep that checks it reads help pages correctly.

## Scope

**In:** the six parse gaps in `tools/doc_prose_report.R` (ROADMAP item (o) of the shipped-docs row, absorbed here), and a committed check script for them. A new script `tools/roxygen_repeats.R`. The paragraphs repeated across roxygen blocks in `R/ffmpeg.R`, written once and reused, in plain English under rules 1-6 of `cairn/references/plain-docs.md`. The tests that pin wording of that text. Rule 6 (D093) holds: the shared text keeps the claims it had. Review triage follows D093.

**Out:** page-specific prose on the 34 pages goes to M132, M133, M130 and M134. The `ffm_*()` pages go to M128 and M131. New sweep findings on the M126 and M127 files go to a candidate row. The other shipped-docs row items stay on that row. Code comments stay as they are. `NEWS.md` gets no entry (D091).

## Acceptance criteria

- [ ] AC1: At head, `Rscript tools/roxygen_repeats.R R/ffmpeg.R` lists no paragraph found in two or more roxygen blocks, or, for each paragraph it lists, a ledger row quotes the claim it makes in each block that holds it and names two blocks where those claims differ. A paragraph is the text of a run of `#'` lines that starts at a tag or after a blank `#'` line and ends before the next tag or blank `#'` line, with any leading tag and argument name removed and runs of white space made one space. Lines under `@examples` or `@examplesIf`, lines holding only `@export`, `@examples`, `@family …`, `@rdname …` or `@inheritParams …`, and a paragraph whose text is only one inline `` `r …` `` call, are not paragraphs.
- [ ] AC2: Every page on the M129 domain list exists at head, and the prose sweep over the M129 domain at head exits with status 0 or 1 and prints no `[<n> words]` or `[term …]` finding whose sentence text it also prints for another page of the M129 domain.
- [ ] AC3: On each page of the M129 domain, the prose sweep at head prints each finding no more times than at the base commit, and no more `[dash in Rd source]` lines.
- [ ] AC4: Every match of `\btidymedia[._][a-z_.]*[a-z_]` or `\btm_[a-z_]+` in the M129 domain pages at the base commit is found by `git grep -wF` in some `man/*.Rd` file at head.
- [ ] AC5: For every `man/*.Rd` file outside the M129 domain that exists at the base commit and at head and that `git diff --name-only <base> HEAD -- man/` lists, the prose sweep at head prints each finding no more times than at the base commit, and no more `[dash in Rd source]` lines. If its `--prose` output at head matches a glossary stem that its `--prose` output at the base commit did not match, the page names the glossary.
- [ ] AC6: `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T2, T4, T5
- AC2 → T1, T4, T5
- AC3 → T1, T3, T4, T5
- AC4 → T3, T5
- AC5 → T4, T5
- AC6 → T5

## Tasks

- [x] T1: Fix the six parse gaps of item (o) in `tools/doc_prose_report.R`, and add exit status 3 for a usage error or a missing file. Write `tools/test_doc_prose_report.R`, which plants each part of each gap and exits 0 only when every plant is read correctly. See each plant fail before its fix. Vary: a lowercase start with "tidymedia" and with another word; a wrapped argument line at two indents; a non-UTF-8 locale run; the empty file first, in the middle and last; no files and a missing file; ` -- ` and `---` in `.Rmd` prose and in a code span; a four-backtick fence, a `|` in a code span, and prose between `<` and `>`. Rerun the sweep over the M126 and M127 files, and send any new finding to a candidate row.
- [x] T2: Write `tools/roxygen_repeats.R` to list the paragraphs AC1 defines. Before trusting a clean result, plant repeats and see each listed: in `@param` (also under two argument names), `@return`, the description and `@section`; wrapped at different points, including inside an Rd macro; ended by a tag, by a blank line and by a `#'` line of spaces; in 2 and in 3 blocks. See none listed for example code, `@examplesIf`, `@export`, `@family`, `@rdname` and `@inheritParams` lines, and an inline `r` call.
- [x] T3: With the sweep from T1, record in a new M129 ledger section the base commit, the domain page list, the AC4 identifiers, the sweep output over the domain at the base commit, and the repeats listed at the base commit.
- [x] T4: For each repeated paragraph whose copies make the same claim, keep one copy in plain English and reuse it with `@inheritParams`, `@inheritSection`, a `man-roxygen/` template, or a string in an `R/` doc file. Copies that differ in what they claim stay separate, with a ledger row. A base claim found false keeps its meaning and goes to the follow-up row. After `devtools::document()`, read the rendered shared text on each page that receives it.
- [ ] T5: A test that pins changed wording now pins the new wording of the same property. A test named in a `cairn/DECISIONS.md` entry is rewritten, never removed. The claim audit reader also says, for each changed sentence, whether it makes the same claim as the base text; a sentence that adds or changes a claim about what the package does is put back to the base claim in plain words. Fill the ledger. Run `tools/test_doc_prose_report.R`, the repeats script, the sweep and its base comparisons, `devtools::document()`, `devtools::check()`, `devtools::test()` with no other R session working, and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-13: created by /milestone-plan (series M126-M130). The criteria audit and gate choices are logged in M126's work log.
- 2026-09-13: re-cut by /milestone-plan under D093 into M128, M129, M130 and M131-M134. M129 now holds the sweep fixes and the shared text. Its page rewrites moved to M132 and M133.
- 2026-09-13: criteria audit (full): 21 findings on the draft. 13 clear fixes applied (exit status, locale, recorded domains, paragraph definition, escape clause, plants, count comparison, regex, output format, counts). 4 judgment calls went to the gate.
- 2026-09-13: re-audit (full) of the gate-changed criteria: 22 findings. 13 clear fixes applied (AC5 by `git diff` and glossary, inline-r exclusion, exit 3, domain from the ledger, plants, rendered glossary check, base files by `git show`). Judgment calls settled: sweep tests stay a task, not a criterion; stems, locale pin and 0 notes kept.
- 2026-09-13: plan gate chose M129 fixing all text repeated across pages over merging identical copies only, because 35 repeated finding sentences account for 126 of the 393 findings on the 34 pages; falsified by a page milestone after M129 that must edit shared text to pass its own criteria.
- 2026-09-13: plan gate chose fixing the sweep's parse gaps in M129 over leaving them as M127 did, because task pages are mostly argument text, which the gaps misread; falsified by the fixed sweep reporting no new finding on the 34 pages.
- 2026-09-13: size: M129 changes rendered text on up to 34 pages, but its work is about 38 distinct paragraphs, which one reviewer can read against the code. This is its D093 size justification.
- 2026-09-13: implement started on branch `m129-sweep-shared-task-help`, cut from `db460d58`. Implement gate (user's choices): shared argument text with the same name stays in one owner block, and other blocks use `@inheritParams`. Other shared text comes from functions in a new `R/task-doc.R`. A family of close variants becomes one function with arguments. `@inherit`, `@template` and `@inheritSection` lines are not used, because AC1 counts a repeated tag line as a repeated paragraph.
- 2026-09-13: T1 done. The sweep splits before a lowercase word, switches to a UTF-8 character locale, reads an argument start only in the `Rd2txt()` label layout (a name holding a colon now reads right), exits 2 after printing every finding and 3 on a usage error or missing file, reports ` -- ` and `---` in `.Rmd` prose, closes a fence only on as many backticks, keeps `|` inside code spans in a table cell, and drops only HTML tags between `<` and `>`. `tools/test_doc_prose_report.R` passes 25 of 25 plants, and the base sweep passes 10. The two wrapped-argument plants (indent 10 and 14) pass on the base sweep too, because `Rd2txt()` never wraps an argument line at an indent under 10. Base and head sweeps both report nothing on the 6 M126 files and 28 M127 pages, so no candidate row.
- 2026-09-13: T2 done. `tools/roxygen_repeats.R` lists paragraphs as AC1 defines them, and `tools/test_roxygen_repeats.R` passes 15 of 15 plants. Three broken copies of the script (argument names kept, example and skip lines kept, inline `r` calls kept) each fail 2 or 3 plants. At the base commit it lists 33 paragraphs over the 34 blocks of `R/ffmpeg.R`.
- 2026-09-13: T3 done. The M129 ledger section in `cairn/references/plain-docs.md` records the base commit, the 34 domain pages, 11 AC4 identifiers on 9 pages, and the 33 repeated paragraphs. The base sweep printed 393 findings, and 35 of its sentences are on two or more pages. The ledger keeps per-page counts by kind and the command that prints the full output again.
- 2026-09-13: T4 checkpoint, not done. New `R/task-doc.R` holds the shared text, and 13 owner blocks keep shared arguments that others inherit. `roxygen_repeats.R` now lists nothing. The head sweep prints 252 findings, no page more than at base and no sentence on two pages. The full test suite was still running. The last `parallel` rewording is not yet documented into `man/`. Two doc tests now pin the reworded sentences.
- 2026-09-13: T4 done. `devtools::test()` gave 0 failures, 13866 passes and 5 skips. `man/` was regenerated after the `parallel` rewording. The repeats script exits 0, and the sweep comparison still finds no page with more findings and no cross-page finding. The rendered shared text was read on all 27 changed pages. No base claim was found false, so no follow-up row.

## Decisions

## Review
