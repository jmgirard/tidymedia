# M133: The separate-audio-video and normalize-audio help pages read as plain English

- **Status:** in-progress
- **Priority:** high
- **Depends on:** M129
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — shipped help pages for exported functions
- **Branch/PR:** `m133-plain-separate-normalize-help-pages`

## Goal

The M133 help-page domain uses plain English for an R user who does not know FFmpeg.

## Scope

**In:** the page-specific roxygen text in `R/ffmpeg.R` behind the M133 domain in `cairn/references/plain-docs.md`: `separate_audio_video()` and `normalize_audio()` with their batch pages (4 pages, 305 sentences on 2026-09-13, before M129). The tests that pin wording on these pages. Rewrites follow rules 1-6, so they change form, not claims (D093). The failure sections say what a user sees and can do, and the rest moves to code comments. Review triage follows D093.

**Out:** the text shared across task pages, written once by M129. The other task function pages go to M132, M130 and M134. Base claims found false go to this milestone's follow-up candidate row. Code comments stay as they are, apart from text moved into them. `NEWS.md` gets no entry (D091).

## Acceptance criteria

- [ ] AC1: Every page on the M133 domain list exists at head, and the prose sweep over the M133 domain at head exits with status 0 or 1 and prints no `[term …]` or `[dash in Rd source]` line.
- [ ] AC2: Every page on the M133 domain list exists at head, and the prose sweep over the M133 domain at head exits with status 0 or 1 and prints no `[<n> words]` line.
- [ ] AC3: Each page in the M133 domain whose `--prose` output at head matches a glossary stem names the glossary.
- [ ] AC4: Every match of `\btidymedia[._][a-z_.]*[a-z_]` or `\btm_[a-z_]+` in the M133 domain pages at the base commit is found by `git grep -wF` in some `man/*.Rd` file at head.
- [ ] AC5: For every `man/*.Rd` file outside the M133 domain that exists at the base commit and at head and that `git diff --name-only <base> HEAD -- man/` lists, the prose sweep at head prints each finding no more times than at the base commit, and no more `[dash in Rd source]` lines. If its `--prose` output at head matches a glossary stem that its `--prose` output at the base commit did not match, the page names the glossary.
- [ ] AC6: `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T2, T3, T4
- AC2 → T2, T3, T4
- AC3 → T2, T3, T4
- AC4 → T1, T4
- AC5 → T4
- AC6 → T4

## Tasks

- [x] T1: Record in a new M133 ledger section the base commit, the domain page list, the AC4 identifiers and the sweep output over the domain at the base commit.
- [x] T2: Rewrite `separate_audio_video()` and its batch page.
- [x] T3: Rewrite `normalize_audio()` and its batch page.
- [ ] T4: A test that pins changed wording now pins the new wording of the same property. A test whose claim left the user docs is removed, with a ledger row. A test named in a `cairn/DECISIONS.md` entry is rewritten, never removed. The claim audit reader also says, for each changed sentence, whether it makes the same claim as the base text; a sentence that adds or changes a claim about what the package does is put back to the base claim in plain words, and a sentence that names the glossary stays. A base claim found false gets a ledger row and a line in the follow-up candidate row. Fill the ledger. Run the sweep and its base comparisons, `devtools::document()`, `devtools::check()`, `devtools::test()` with no other R session working, and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-13: created by /milestone-plan (re-cut of M129 under D093). The criteria audit and re-audit lines are in M129's work log, and cover this template. The split from M132 is logged in M132's work log.
- 2026-09-17: implement started on `m133-plain-separate-normalize-help-pages`, cut from `4b932266`. No question gate: the plan left no choice open.
- 2026-09-17: T1 done. The `### M133` ledger section records the base commit, the 4 pages, the 10 AC4 identifiers and the base sweep (324 sentences, 63 `[<n> words]`, 33 `[term …]`, 7 dash lines).
- 2026-09-17: T2 done. `?separate_audio_video` and its batch page sweep clean (149 and 132 sentences). Both name the glossary. The four-condition sentences became lists. The phrase `not \emph{how} FFmpeg exited` stays on the batch page because `test-ffmpeg-exit-condition.R` pins it.
- 2026-09-17: T3 done. `?normalize_audio` and its batch page sweep clean (106 and 137 sentences). Both name the glossary. Three phrases that `test-ffmpeg-exit-condition.R` pins stay, each on one source line.

## Decisions

## Review
