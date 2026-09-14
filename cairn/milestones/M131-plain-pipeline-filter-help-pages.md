# M131: The filter and multi-input pipeline help pages read as plain English

- **Status:** review
- **Priority:** high
- **Depends on:** M129
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — shipped help pages for exported functions
- **Branch/PR:** `m131-plain-pipeline-filter-help-pages`

## Goal

The M131 help-page domain uses plain English for an R user who does not know FFmpeg.

## Scope

**In:** the roxygen text in `R/ffm.R` behind the M131 domain in `cairn/references/plain-docs.md`: `ffm_trim()`, `ffm_crop()`, `ffm_scale()`, `ffm_fps()`, `ffm_drawbox()`, `ffm_loudnorm()`, `ffm_hstack()`, `ffm_vstack()`, `ffm_overlay()` and `ffm_concat()` (10 pages, 218 sentences on 2026-09-13). The tests that pin wording on these pages. Rewrites follow rules 1-6, so they change form, not claims (D093). Review triage follows D093.

**Out:** the input, output and run `ffm_*()` pages go to M128. The task function pages go to M129, M132, M133, M130 and M134. Base claims found false go to this milestone's follow-up candidate row. Code comments stay as they are. `NEWS.md` gets no entry (D091).

## Acceptance criteria

- [x] AC1: Every page on the M131 domain list exists at head, and the prose sweep over the M131 domain at head exits with status 0 or 1 and prints no `[term …]` or `[dash in Rd source]` line.
- [x] AC2: Every page on the M131 domain list exists at head, and the prose sweep over the M131 domain at head exits with status 0 or 1 and prints no `[<n> words]` line.
- [x] AC3: Each page in the M131 domain whose `--prose` output at head matches a glossary stem names the glossary.
- [x] AC4: Every match of `\btidymedia[._][a-z_.]*[a-z_]` or `\btm_[a-z_]+` in the M131 domain pages at the base commit is found by `git grep -wF` in some `man/*.Rd` file at head.
- [x] AC5: For every `man/*.Rd` file outside the M131 domain that exists at the base commit and at head and that `git diff --name-only <base> HEAD -- man/` lists, the prose sweep at head prints each finding no more times than at the base commit, and no more `[dash in Rd source]` lines. If its `--prose` output at head matches a glossary stem that its `--prose` output at the base commit did not match, the page names the glossary.
- [x] AC6: `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T2, T3, T4
- AC2 → T2, T3, T4
- AC3 → T2, T3, T4
- AC4 → T1, T4
- AC5 → T4
- AC6 → T4

## Tasks

- [x] T1: Record in a new M131 ledger section the base commit, the domain page list, the AC4 identifiers and the sweep output over the domain at the base commit.
- [x] T2: Rewrite `ffm_trim()`, `ffm_crop()`, `ffm_scale()`, `ffm_fps()`, `ffm_drawbox()` and `ffm_loudnorm()`.
- [x] T3: Rewrite `ffm_hstack()`, `ffm_vstack()`, `ffm_overlay()` and `ffm_concat()`.
- [x] T4: A test that pins changed wording now pins the new wording of the same property. A test whose claim left the user docs is removed, with a ledger row. A test named in a `cairn/DECISIONS.md` entry is rewritten, never removed. The claim audit reader also says, for each changed sentence, whether it makes the same claim as the base text; a sentence that adds or changes a claim about what the package does is put back to the base claim in plain words, and a sentence that names the glossary stays. A base claim found false gets a ledger row and a line in the follow-up candidate row. Fill the ledger. Run the sweep and its base comparisons, `devtools::document()`, `devtools::check()`, `devtools::test()` with no other R session working, and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-13: created by /milestone-plan (re-cut of M128 under D093). The criteria audit and re-audit lines are in M129's work log, and cover this template.
- 2026-09-14: implement started on `m131-plain-pipeline-filter-help-pages` from `08a4df26`. No question gate, because the plan and the M128 and M130 ledgers settle every choice.
- 2026-09-14: T1 done. Base sweep over the 10 pages: 220 sentences, 14 findings (9 words, 5 term, no dash). `tidymedia_ffm` is the only AC4 identifier. Ledger section `### M131` added.
- 2026-09-14: T2 done. Six pages rewritten. The sweep over them prints no finding. `ffm_trim`, `ffm_fps` and `ffm_loudnorm` name the glossary. `devtools::test()` has 0 failures.
- 2026-09-14: T3 done. Four pages rewritten. The sweep over all 10 pages prints no finding. The 7 pages with a stem name the glossary. "Layer-2", "blessed" and "verb" are gone. `devtools::test()` has 0 failures.
- 2026-09-14: T4 checkpoint, not done. claim audit: 110 claims read, 6 corrected — R/ffm.R (concat, loudnorm, trim, vstack, overlay, crop and scale See Also); the same reader re-read all 6 as the base claim. Three false base claims are ledger rows and the M131 part of the `ffm_*()` help-text follow-up row. `pkgdown::check_pkgdown()` found no problems. `devtools::test()` and `devtools::check()` are still running.
- 2026-09-14: T4 done. `devtools::document()` writes nothing. `devtools::test()` has 0 failures. `devtools::check()` gives 0 errors, 0 warnings and 0 notes. Status set to review.
- 2026-09-14: review. Correction to the T3 line: 6 pages with a stem name the glossary, not 7 (review O3).
- 2026-09-14: review gate fixes O1-O7 committed on the branch. O11 extends the follow-up row by the maintainer's choice.
- step-7 approval: m131-plain-pipeline-filter-help-pages approved for merge

## Decisions

## Review

Evidence gathered 2026-09-14 on branch head `5e6d75e1`. Master had not moved since the base commit `08a4df26`, so no sync was needed.

- AC1: all 10 domain pages exist at head. No page was added under `man/` since the base commit. The sweep over the 10 pages at head reads 271 sentences, exits 0 and prints no `[term …]` or `[dash in Rd source]` line. The base run over the same 10 pages exits 1 with 5 term findings, so the sweep can fail on this domain.
- AC2: the same head run prints no `[<n> words]` line. The base run prints 9.
- AC3: the `--prose` output at head has a glossary stem on 6 pages: `ffm_concat`, `ffm_fps`, `ffm_loudnorm`, `ffm_overlay`, `ffm_trim` and `ffm_vstack`. Each of the 6 has a rendered sentence with `glossary` and `vignette("tidymedia")`. `ffm_crop`, `ffm_drawbox`, `ffm_hstack` and `ffm_scale` have no stem. The same glossary check on the base `ffm_trim` page reads false.
- AC4: the only match at the base commit is `tidymedia_ffm`, 20 times. `git grep -lwF tidymedia_ffm HEAD -- 'man/*.Rd'` finds it in 23 files.
- AC5: `git diff --name-only 08a4df26 HEAD -- man/` lists only the 10 domain pages, so no page outside the domain is in scope.
- AC6: `devtools::document()` exits 0 and leaves the tree clean. `devtools::test()` gives FAIL 0, WARN 12, SKIP 5, PASS 13866. `devtools::check()` gives 0 errors, 0 warnings and 0 notes. `pkgdown::check_pkgdown()` prints "No problems found".
- Consistency gate: `cairn_validate.py` exits 0 with all checks passed. No principle changed, so `cairn_impact` does not apply. The branch does not touch `README.Rmd`, `README.md`, `NEWS.md` (no entry, D091) or any top-level file.

Independent review: three fresh reviewers. The [S] blame-history and [S] prior-review lenses found nothing, and the GitHub probe found no review threads. The [O] diff-bug lens found no false claim the branch added and no code change. Its ranked findings:

- O1: `?ffm_concat` "use a direct command, such as `ffmpeg()`" hints that `ffprobe()` or `mediainfo()` could run the concat filter.
- O2: `?ffm_concat` "at once" replaces "immediately" and can read as "all together".
- O3: the ledger and the T3 work-log line say 7 pages with a stem name the glossary. The count is 6.
- O4: follow-up row item (c) says every multi-input function drops the audio filters. `ffm_concat()` aborts instead, and only hstack, vstack and overlay set `complex`.
- O5: `?ffm_loudnorm` Details "It makes one reproducible command": the subject "It" is unclear.
- O6: `?ffm_loudnorm` Details: splitting the encoder sentence makes "accept whatever frame" and "refuse to open" read as a contradiction.
- O7: the follow-up row's integer-refusal note leaves out `?ffm_overlay`, whose `x` and `y` also go through `check_dim()`.
- O8: `?ffm_fps` "like the other filters that take one input" can include `ffm_loudnorm()`, an audio filter. The base text had the same reach.
- O9: the second `?ffm_vstack` Description sentence moved to Details.
- O10: FFmpeg wording from the base text stays, for example `-filter_complex` and "compiles to `-af`".
- O11: `?ffm_loudnorm` `linear = FALSE` "leaves out the option, so the single-pass dynamic behavior does not change". FFmpeg 9.0.1 `-h filter=loudnorm` shows `linear` defaults to true. This is a base claim, not added by the branch.
- O12: the ledger table under the base-commit heading also has a head Result column.

Triage at the gate, 2026-09-14, as the maintainer chose. No finding shows a criterion failing, so there is no return.

- O1, O2: fixed. `?ffm_concat` now says "so use `ffmpeg()`" and "When you call `ffm_concat()`, it writes one to a temporary path".
- O5, O6: fixed. `?ffm_loudnorm` now says "The pipeline stays one reproducible command" and "Even those encoders refuse to open".
- O3: fixed in the ledger, marked corrected. The T3 work-log line is history, and a correction line follows it.
- O4, O7: fixed in the M131 part of the follow-up row, marked corrected.
- O11: follow-up. Item (d) of the M131 part of the follow-up row. The maintainer chose to extend that row at the gate.
- O8, O10: rejected, because each is base text that the rewrite keeps.
- O9: rejected, because rule 6 allows a move.
- O12: rejected, because the M128 and M130 ledgers use the same layout.
- After the fixes: `devtools::document()` leaves the tree clean after it writes the two pages. The sweep over all `man/ffm_*.Rd` pages prints no finding. `?ffm_concat` and `?ffm_loudnorm` still name the glossary. `devtools::test(filter = "shared-range|ffm")` gives FAIL 0. `pkgdown::check_pkgdown()` finds no problems. The full `devtools::check()` is left to CI on the PR.
