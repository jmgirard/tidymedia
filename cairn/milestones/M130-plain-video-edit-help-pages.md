# M130: The crop, web-format, standardize and strip-metadata help pages read as plain English

- **Status:** in-progress
- **Priority:** high
- **Depends on:** M129
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — shipped help pages for exported functions
- **Branch/PR:** `m130-plain-video-edit-help-pages`

## Goal

The M130 help-page domain uses plain English for an R user who does not know FFmpeg.

## Scope

**In:** the page-specific roxygen text in `R/ffmpeg.R` behind the M130 domain in `cairn/references/plain-docs.md`: `crop_video()`, `format_for_web()`, `standardize_video()` and `strip_metadata()` with their batch pages (8 pages, 322 sentences on 2026-09-13, before M129). The tests that pin wording on these pages. Rewrites follow rules 1-6, so they change form, not claims (D093). Review triage follows D093.

**Out:** the text shared across task pages, written once by M129. The other task function pages go to M132, M133 and M134. Base claims found false go to this milestone's follow-up candidate row. Code comments stay as they are. `NEWS.md` gets no entry (D091).

## Acceptance criteria

- [ ] AC1: Every page on the M130 domain list exists at head, and the prose sweep over the M130 domain at head exits with status 0 or 1 and prints no `[term …]` or `[dash in Rd source]` line.
- [ ] AC2: Every page on the M130 domain list exists at head, and the prose sweep over the M130 domain at head exits with status 0 or 1 and prints no `[<n> words]` line.
- [ ] AC3: Each page in the M130 domain whose `--prose` output at head matches a glossary stem names the glossary.
- [ ] AC4: Every match of `\btidymedia[._][a-z_.]*[a-z_]` or `\btm_[a-z_]+` in the M130 domain pages at the base commit is found by `git grep -wF` in some `man/*.Rd` file at head.
- [ ] AC5: For every `man/*.Rd` file outside the M130 domain that exists at the base commit and at head and that `git diff --name-only <base> HEAD -- man/` lists, the prose sweep at head prints each finding no more times than at the base commit, and no more `[dash in Rd source]` lines. If its `--prose` output at head matches a glossary stem that its `--prose` output at the base commit did not match, the page names the glossary.
- [ ] AC6: `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T2, T3, T4
- AC2 → T2, T3, T4
- AC3 → T2, T3, T4
- AC4 → T1, T4
- AC5 → T4
- AC6 → T4

## Tasks

- [x] T1: Record in a new M130 ledger section the base commit, the domain page list, the AC4 identifiers and the sweep output over the domain at the base commit.
- [x] T2: Rewrite `crop_video()` and `format_for_web()`, and their batch pages.
- [ ] T3: Rewrite `standardize_video()` and `strip_metadata()`, and their batch pages.
- [ ] T4: A test that pins changed wording now pins the new wording of the same property. A test whose claim left the user docs is removed, with a ledger row. A test named in a `cairn/DECISIONS.md` entry is rewritten, never removed. The claim audit reader also says, for each changed sentence, whether it makes the same claim as the base text; a sentence that adds or changes a claim about what the package does is put back to the base claim in plain words, and a sentence that names the glossary stays. A base claim found false gets a ledger row and a line in the follow-up candidate row. Fill the ledger. Run the sweep and its base comparisons, `devtools::document()`, `devtools::check()`, `devtools::test()` with no other R session working, and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-13: created by /milestone-plan (series M126-M130). The criteria audit and gate choices are logged in M126's work log.
- 2026-09-13: re-cut by /milestone-plan under D093: M130 keeps 8 of its 18 own pages, and the anonymize, segment, concatenate, compare and picture-in-picture pages moved to M134. The criteria audit and re-audit lines are in M129's work log, and cover this template.
- 2026-09-14: implement started on branch `m130-plain-video-edit-help-pages`, cut from `9353ac4f`. No open choices, so no question gate.
- 2026-09-14: T1 done. `### M130` ledger in `cairn/references/plain-docs.md`: base sweep over 8 pages read 380 sentences, 48 findings (31 words, 15 term, 2 dash), no AC4 identifiers, no page names the glossary.
- 2026-09-14: T2 done. `crop_video`, `format_for_web` and their batch pages rewritten, each names the glossary, and the sweep over the 4 pages exits 0. A first draft reworded `crop_video_batch`'s `video_codec`, which is M129 shared text inherited by two M134 pages, so it was put back. `devtools::test()`: 0 failed, 0 errors, 5 skipped.

## Decisions

## Review
