# M134: The anonymize, segment, concatenate, compare and picture-in-picture help pages read as plain English

- **Status:** in-progress
- **Priority:** high
- **Depends on:** M129
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — shipped help pages for exported functions
- **Branch/PR:** `m134-plain-video-composition-help-pages`

## Goal

The M134 help-page domain uses plain English for an R user who does not know FFmpeg.

## Scope

**In:** the page-specific roxygen text in `R/ffmpeg.R` behind the M134 domain in `cairn/references/plain-docs.md`: `anonymize_video()`, `segment_video()`, `concatenate_videos()`, `compare_videos()` and `picture_in_picture()` with their batch pages (10 pages, 453 sentences on 2026-09-13, before M129). The tests that pin wording on these pages. Rewrites follow rules 1-6, so they change form, not claims (D093). Review triage follows D093.

**Out:** the text shared across task pages, written once by M129. The other task function pages go to M132, M133 and M130. Base claims found false go to this milestone's follow-up candidate row. Code comments stay as they are. `NEWS.md` gets no entry (D091).

## Acceptance criteria

- [ ] AC1: Every page on the M134 domain list exists at head, and the prose sweep over the M134 domain at head exits with status 0 or 1 and prints no `[term …]` or `[dash in Rd source]` line.
- [ ] AC2: Every page on the M134 domain list exists at head, and the prose sweep over the M134 domain at head exits with status 0 or 1 and prints no `[<n> words]` line.
- [ ] AC3: Each page in the M134 domain whose `--prose` output at head matches a glossary stem names the glossary.
- [ ] AC4: Every match of `\btidymedia[._][a-z_.]*[a-z_]` or `\btm_[a-z_]+` in the M134 domain pages at the base commit is found by `git grep -wF` in some `man/*.Rd` file at head.
- [ ] AC5: For every `man/*.Rd` file outside the M134 domain that exists at the base commit and at head and that `git diff --name-only <base> HEAD -- man/` lists, the prose sweep at head prints each finding no more times than at the base commit, and no more `[dash in Rd source]` lines. If its `--prose` output at head matches a glossary stem that its `--prose` output at the base commit did not match, the page names the glossary.
- [ ] AC6: `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T2, T3, T4
- AC2 → T2, T3, T4
- AC3 → T2, T3, T4
- AC4 → T1, T5
- AC5 → T5
- AC6 → T5

## Tasks

- [x] T1: Record in a new M134 ledger section the base commit, the domain page list, the AC4 identifiers and the sweep output over the domain at the base commit.
- [ ] T2: Rewrite `anonymize_video()` and `segment_video()`, and their batch pages.
- [ ] T3: Rewrite `concatenate_videos()` and its batch page.
- [ ] T4: Rewrite `compare_videos()` and `picture_in_picture()`, and their batch pages.
- [ ] T5: A test that pins changed wording now pins the new wording of the same property. A test whose claim left the user docs is removed, with a ledger row. A test named in a `cairn/DECISIONS.md` entry is rewritten, never removed. The claim audit reader also says, for each changed sentence, whether it makes the same claim as the base text; a sentence that adds or changes a claim about what the package does is put back to the base claim in plain words, and a sentence that names the glossary stays. A base claim found false gets a ledger row and a line in the follow-up candidate row. Fill the ledger. Run the sweep and its base comparisons, `devtools::document()`, `devtools::check()`, `devtools::test()` with no other R session working, and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-13: created by /milestone-plan (re-cut of M130 under D093). The criteria audit and re-audit lines are in M129's work log, and cover this template.
- 2026-09-17: implement started on `m134-plain-video-composition-help-pages`, cut from `7b2f9b0d`. No question was open at the gate.
- 2026-09-17: T1 done. `### M134` in `cairn/references/plain-docs.md` records the base commit, the 10 pages and the base sweep: 553 sentences, 56 findings. The AC4 patterns match nothing at the base commit.

## Decisions

## Review
