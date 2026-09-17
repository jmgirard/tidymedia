# M132: The FFmpeg-capability, frame and audio conversion help pages read as plain English

- **Status:** in-progress
- **Priority:** high
- **Depends on:** M129
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — shipped help pages for exported functions
- **Branch/PR:** `m132-plain-capability-frame-audio-help-pages`

## Goal

The M132 help-page domain uses plain English for an R user who does not know FFmpeg.

## Scope

**In:** the page-specific roxygen text in `R/ffmpeg.R` behind the M132 domain in `cairn/references/plain-docs.md`: `ffmpeg()`, `ffmpeg_codecs()`, `ffmpeg_encoders()`, `hardware_encoder()`, and `extract_frame()`, `sample_frames()`, `extract_audio()` and `convert_audio()` with their batch pages (12 pages, 345 sentences on 2026-09-13, before M129). The tests that pin wording on these pages. Rewrites follow rules 1-6, so they change form, not claims (D093). Review triage follows D093.

**Out:** the text shared across task pages, written once by M129. The other task function pages go to M133, M130 and M134. Base claims found false go to this milestone's follow-up candidate row. Code comments stay as they are. `NEWS.md` gets no entry (D091).

## Acceptance criteria

- [ ] AC1: Every page on the M132 domain list exists at head, and the prose sweep over the M132 domain at head exits with status 0 or 1 and prints no `[term …]` or `[dash in Rd source]` line.
- [ ] AC2: Every page on the M132 domain list exists at head, and the prose sweep over the M132 domain at head exits with status 0 or 1 and prints no `[<n> words]` line.
- [ ] AC3: Each page in the M132 domain whose `--prose` output at head matches a glossary stem names the glossary.
- [ ] AC4: Every match of `\btidymedia[._][a-z_.]*[a-z_]` or `\btm_[a-z_]+` in the M132 domain pages at the base commit is found by `git grep -wF` in some `man/*.Rd` file at head.
- [ ] AC5: For every `man/*.Rd` file outside the M132 domain that exists at the base commit and at head and that `git diff --name-only <base> HEAD -- man/` lists, the prose sweep at head prints each finding no more times than at the base commit, and no more `[dash in Rd source]` lines. If its `--prose` output at head matches a glossary stem that its `--prose` output at the base commit did not match, the page names the glossary.
- [ ] AC6: `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T2, T3, T4, T5
- AC2 → T2, T3, T4, T5
- AC3 → T2, T3, T4, T5
- AC4 → T1, T5
- AC5 → T5
- AC6 → T5

## Tasks

- [x] T1: Record in a new M132 ledger section the base commit, the domain page list, the AC4 identifiers and the sweep output over the domain at the base commit.
- [ ] T2: Rewrite `ffmpeg()`, `ffmpeg_codecs()`, `ffmpeg_encoders()` and `hardware_encoder()`.
- [ ] T3: Rewrite `extract_frame()` and `sample_frames()`, and their batch pages.
- [ ] T4: Rewrite `extract_audio()` and `convert_audio()`, and their batch pages.
- [ ] T5: A test that pins changed wording now pins the new wording of the same property. A test whose claim left the user docs is removed, with a ledger row. A test named in a `cairn/DECISIONS.md` entry is rewritten, never removed. The claim audit reader also says, for each changed sentence, whether it makes the same claim as the base text; a sentence that adds or changes a claim about what the package does is put back to the base claim in plain words, and a sentence that names the glossary stays. A base claim found false gets a ledger row and a line in the follow-up candidate row. Fill the ledger. Run the sweep and its base comparisons, `devtools::document()`, `devtools::check()`, `devtools::test()` with no other R session working, and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-13: created by /milestone-plan (re-cut of M129 under D093). The criteria audit and re-audit lines are in M129's work log, and cover this template.
- 2026-09-13: plan gate chose splitting M129's 16 pages into this milestone and M133 over one 16-page milestone, because the 16 pages hold about 650 sentences; falsified by M129 shrinking these pages so far that one milestone would have been about 12 pages of work.
- 2026-09-17: implement started on `m132-plain-capability-frame-audio-help-pages`, cut from `efb2b2c4`. No question gate: the plan left nothing open.
- 2026-09-17: T1 done. Ledger section `### M132` records base `efb2b2c4`, 12 pages, 369 sentences, 45 findings (23 words, 22 term, 0 dash) and 3 identifiers.

## Decisions

## Review
