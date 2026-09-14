# M131: The filter and multi-input pipeline help pages read as plain English

- **Status:** in-progress
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

- [ ] AC1: Every page on the M131 domain list exists at head, and the prose sweep over the M131 domain at head exits with status 0 or 1 and prints no `[term …]` or `[dash in Rd source]` line.
- [ ] AC2: Every page on the M131 domain list exists at head, and the prose sweep over the M131 domain at head exits with status 0 or 1 and prints no `[<n> words]` line.
- [ ] AC3: Each page in the M131 domain whose `--prose` output at head matches a glossary stem names the glossary.
- [ ] AC4: Every match of `\btidymedia[._][a-z_.]*[a-z_]` or `\btm_[a-z_]+` in the M131 domain pages at the base commit is found by `git grep -wF` in some `man/*.Rd` file at head.
- [ ] AC5: For every `man/*.Rd` file outside the M131 domain that exists at the base commit and at head and that `git diff --name-only <base> HEAD -- man/` lists, the prose sweep at head prints each finding no more times than at the base commit, and no more `[dash in Rd source]` lines. If its `--prose` output at head matches a glossary stem that its `--prose` output at the base commit did not match, the page names the glossary.
- [ ] AC6: `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

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
- [ ] T4: A test that pins changed wording now pins the new wording of the same property. A test whose claim left the user docs is removed, with a ledger row. A test named in a `cairn/DECISIONS.md` entry is rewritten, never removed. The claim audit reader also says, for each changed sentence, whether it makes the same claim as the base text; a sentence that adds or changes a claim about what the package does is put back to the base claim in plain words, and a sentence that names the glossary stays. A base claim found false gets a ledger row and a line in the follow-up candidate row. Fill the ledger. Run the sweep and its base comparisons, `devtools::document()`, `devtools::check()`, `devtools::test()` with no other R session working, and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-13: created by /milestone-plan (re-cut of M128 under D093). The criteria audit and re-audit lines are in M129's work log, and cover this template.
- 2026-09-14: implement started on `m131-plain-pipeline-filter-help-pages` from `08a4df26`. No question gate, because the plan and the M128 and M130 ledgers settle every choice.
- 2026-09-14: T1 done. Base sweep over the 10 pages: 220 sentences, 14 findings (9 words, 5 term, no dash). `tidymedia_ffm` is the only AC4 identifier. Ledger section `### M131` added.
- 2026-09-14: T2 done. Six pages rewritten. The sweep over them prints no finding. `ffm_trim`, `ffm_fps` and `ffm_loudnorm` name the glossary. `devtools::test()` has 0 failures.
- 2026-09-14: T3 done. Four pages rewritten. The sweep over all 10 pages prints no finding. The 7 pages with a stem name the glossary. "Layer-2", "blessed" and "verb" are gone. `devtools::test()` has 0 failures.

## Decisions

## Review
