# M130: The video and composition task help pages read as plain English

- **Status:** planned
- **Priority:** high
- **Depends on:** M129
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — shipped help pages for exported functions
- **Branch/PR:** —

## Goal

All help pages from `R/ffmpeg.R` use plain English for an R user who does not know FFmpeg.

## Scope

**In:** the roxygen text in `R/ffmpeg.R` behind the M130 domain in `cairn/references/plain-docs.md` (34 pages on 2026-09-13). The pages not in M129 are rewritten: crop, web format, standardize, strip metadata, anonymize, segment, concatenate, compare and picture-in-picture, each with its batch page. The tests that pin wording on these pages are in scope too.

**Out:** the repeated text, already written once by M129. Code comments in `R/` stay as they are. `NEWS.md` gets no entry (D091).

## Acceptance criteria

- [ ] AC1: The prose sweep over the M130 domain prints no sentence that matches a maintainer term.
- [ ] AC2: The prose sweep over the M130 domain prints no sentence over 25 words.
- [ ] AC3: For each page in the domain without an M129 ledger row, take each glossary stem found in its `--prose` output. The page defines the term at its first use or names the glossary in `vignette("tidymedia")`. One ledger row per page records the stems and how each is met.
- [ ] AC4: Each page in the domain without an M129 ledger row has a ledger row that says what text left it and where that text went: moved (with the page), or deleted (with a reason). Every match of `\btidymedia[._][a-z_.]+` or `\btm_[a-z_]+` in the domain at the base commit is still found in some `man/*.Rd` file at head, or has a ledger row.
- [ ] AC5: `Rscript tools/roxygen_repeats.R R/ffmpeg.R` lists no paragraph that lacks an M129 or M130 ledger row. `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T2, T3, T4, T5
- AC2 → T2, T3, T4, T5
- AC3 → T2, T3, T4, T5
- AC4 → T1, T5
- AC5 → T5

## Tasks

- [ ] T1: Run the sweep over the domain at the base commit. Record the pages and the AC4 identifiers in a new M130 ledger section.
- [ ] T2: Rewrite `crop_video()`, `format_for_web()`, `standardize_video()` and `strip_metadata()`, and their batch pages.
- [ ] T3: Rewrite `anonymize_video()` and `segment_video()`, and their batch pages.
- [ ] T4: Rewrite `concatenate_videos()`, `compare_videos()` and `picture_in_picture()`, and their batch pages.
- [ ] T5: A test that pins changed wording now pins the new wording of the same property. A test whose claim left the user docs is removed, with a ledger row. A test named in a `cairn/DECISIONS.md` entry is rewritten, never removed. Fill the ledger. Run the sweep over all 34 pages, `tools/roxygen_repeats.R`, `devtools::document()`, `devtools::check()`, `devtools::test()` with no other R session working, and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-13: created by /milestone-plan (series M126-M130). The criteria audit and gate choices are logged in M126's work log.

## Decisions

## Review
