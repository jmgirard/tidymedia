# M127: ?tidymedia and the setup, metadata, timeout and batch help pages read as plain English

- **Status:** in-progress
- **Priority:** high
- **Depends on:** M126
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — shipped help pages, including the package's landing page
- **Branch/PR:** `m127-plain-setup-help-pages`

## Goal

The M127 help-page domain, including `?tidymedia`, uses plain English for an R user who does not know FFmpeg.

## Scope

**In:** the roxygen text behind the M127 domain in `cairn/references/plain-docs.md` (28 pages on 2026-09-13). That includes `?tidymedia`, whose timeout detail moves to `?with_timeout` and `?local_timeout`. It also includes the generated audio-stream sentences in `R/audio-stream-doc.R` that other pages reuse. The tests that pin wording on these pages are in scope too.

**Out:** the `ffm_*()` pages go to M128. The task-function pages go to M129 and M130. Code comments in `R/` stay as they are. `NEWS.md` gets no entry (D091). Some pages describe the settings location that version 0.1.0 used. If that text leaves them, the ledger records where users of 0.1.0 find it.

## Acceptance criteria

- [ ] AC1: The prose sweep over the M127 domain prints no sentence that matches a maintainer term.
- [ ] AC2: The prose sweep over the M127 domain prints no sentence over 25 words.
- [ ] AC3: For each page in the domain, take each glossary stem found in its `--prose` output. The page defines the term at its first use or names the glossary in `vignette("tidymedia")`. One ledger row per page records the stems and how each is met.
- [ ] AC4: `tools::Rd2txt()` output for `man/tidymedia-package.Rd` is at most 80 lines. Its first paragraph says what the package does and names `vignette("tidymedia")` as the place to start.
- [ ] AC5: Each page in the domain has a ledger row that says what text left it and where that text went: moved (with the page), or deleted (with a reason). Every match of `\btidymedia[._][a-z_.]+` or `\btm_[a-z_]+` in the domain at the base commit is still found in some `man/*.Rd` file at head, or has a ledger row.
- [ ] AC6: `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T2, T3, T4, T5, T6, T7
- AC2 → T2, T3, T4, T5, T6, T7
- AC3 → T2, T3, T4, T5, T6, T7
- AC4 → T2
- AC5 → T1, T7
- AC6 → T7

## Tasks

- [x] T1: Run the sweep over the domain at the base commit. Record the pages and the AC5 identifiers in a new M127 ledger section.
- [ ] T2: Rewrite `R/tidymedia-package.R`. Keep a short overview, where to start, and the session options. Move the timeout detail to `R/timeout.R` and the error-class lists to short end sections.
- [ ] T3: Rewrite `R/timeout.R`. Keep what a user sets and what a user sees. Move measured timings to code comments.
- [ ] T4: Rewrite `R/program_management.R`, with `install_on_win()` and `program_status()` first.
- [ ] T5: Rewrite `R/mediainfo.R`, `R/ffprobe.R` and `R/audio-stream-doc.R`. For the generated sentences, check the task-function pages that reuse them after `devtools::document()`.
- [ ] T6: Rewrite `R/ffm_batch.R`, `R/ffm_jobs.R`, `R/cache.R`, `R/verify.R`, `R/ffm_manifest.R`, `R/utils-tidy-eval.R` and `R/ffm_oop.R`. In every `R/` file, rename the `@family` labels "escape hatch functions", "builder functions" and "task verb functions" to the names in rule 5.
- [ ] T7: A test that pins changed wording now pins the new wording of the same property. A test whose claim left the user docs is removed, with a ledger row. A test named in a `cairn/DECISIONS.md` entry is rewritten, never removed. Fill the ledger. Run the sweep, `devtools::document()`, `devtools::check()`, `devtools::test()` with no other R session working, and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-13: created by /milestone-plan (series M126-M130). The criteria audit and gate choices are logged in M126's work log.
- 2026-09-13: implement gate: class lists split, old settings location on one page, sweep gaps not fixed here (see Decisions).
- 2026-09-13: minor amendment: each of T2-T6 updates the tests that pin its own pages, because the verify slot needs `devtools::test()` clean per task. T7 keeps the final sweep and the checks.
- 2026-09-13: T1: base sweep over 28 pages printed 185 findings. Ledger section `### M127` added to `cairn/references/plain-docs.md` with the pages and 27 identifiers.

## Decisions

- Implement gate (user's choice): the timeout condition classes go to `?with_timeout` with the other timeout detail. `?tidymedia` keeps one short end section with one line for each FFmpeg-exit class and its fields.
- Implement gate (user's choice): only `?find_ffmpeg` explains the settings file that version 0.1.0 used, in a short end section. `?program_status` and `?unset_program` link to it.
- Implement gate (user's choice): M127 does not fix the six prose-sweep parse gaps (shipped-docs candidate row, item (o)). The rewrite avoids the text shapes that the gaps misread.

## Review
