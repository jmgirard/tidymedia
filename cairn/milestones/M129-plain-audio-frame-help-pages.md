# M129: The audio, frame and FFmpeg-capability help pages read as plain English, and repeated text is written once

- **Status:** planned
- **Priority:** high
- **Depends on:** M126
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — shipped help pages for exported functions
- **Branch/PR:** —

## Goal

The M129 help-page domain uses plain English for an R user who does not know FFmpeg.

## Scope

**In:** the roxygen text in `R/ffmpeg.R` behind the M129 domain in `cairn/references/plain-docs.md` (16 pages on 2026-09-13). The paragraphs repeated across roxygen blocks in all of `R/ffmpeg.R` are in scope, including those on M130's pages. They are written once and reused. The script `tools/roxygen_repeats.R` and the tests that pin wording on these pages are in scope too.

**Out:** the video and composition pages go to M130, apart from their share of the repeated text. Code comments in `R/` stay as they are. `NEWS.md` gets no entry (D091).

## Acceptance criteria

- [ ] AC1: The prose sweep over the M129 domain prints no sentence that matches a maintainer term.
- [ ] AC2: The prose sweep over the M129 domain prints no sentence over 25 words.
- [ ] AC3: For each page in the domain, take each glossary stem found in its `--prose` output. The page defines the term at its first use or names the glossary in `vignette("tidymedia")`. One ledger row per page records the stems and how each is met.
- [ ] AC4: Each page in the domain has a ledger row that says what text left it and where that text went: moved (with the page), or deleted (with a reason). Every match of `\btidymedia[._][a-z_.]+` or `\btm_[a-z_]+` in the domain at the base commit is still found in some `man/*.Rd` file at head, or has a ledger row.
- [ ] AC5: At head, `Rscript tools/roxygen_repeats.R R/ffmpeg.R` lists no paragraph found in two or more roxygen blocks, or a ledger row explains each one it lists. A paragraph is a run of `#'` lines ended by a blank `#'` line or a tag. An inline `` `r ...` `` call counts as text written once. A line holding only a tag (`@export`, `@family`, `@examplesIf`, `@rdname`) and example code are not paragraphs.
- [ ] AC6: `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T3, T4, T5, T6
- AC2 → T3, T4, T5, T6
- AC3 → T3, T4, T5, T6
- AC4 → T1, T6
- AC5 → T1, T2, T6
- AC6 → T6

## Tasks

- [ ] T1: Write `tools/roxygen_repeats.R`. Plant a repeated paragraph and see it listed before trusting a clean result. Run the sweep and the script at the base commit. Record the pages, the AC4 identifiers and the repeats in a new M129 ledger section.
- [ ] T2: Write each repeated paragraph once, in plain English. Reuse it with `@inheritParams`, `@inheritSection` or a `man-roxygen/` template across `R/ffmpeg.R`. Candidates are the hardware and fallback text, the dropped-track warning, `parallel`, the batch return value, and the unset-codec text.
- [ ] T3: Rewrite `ffmpeg()`, `ffmpeg_codecs()`, `ffmpeg_encoders()` and `hardware_encoder()`.
- [ ] T4: Rewrite `extract_frame()`, `sample_frames()`, `extract_audio()` and `convert_audio()`, and their batch pages.
- [ ] T5: Rewrite `separate_audio_video()` and `normalize_audio()`, and their batch pages. Replace the failure sections with what a user sees and can do, and move the rest to code comments.
- [ ] T6: A test that pins changed wording now pins the new wording of the same property. A test whose claim left the user docs is removed, with a ledger row. A test named in a `cairn/DECISIONS.md` entry is rewritten, never removed. Fill the ledger. Run the sweep, the repeats script, `devtools::document()`, `devtools::check()`, `devtools::test()` with no other R session working, and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-13: created by /milestone-plan (series M126-M130). The criteria audit and gate choices are logged in M126's work log.

## Decisions

## Review
