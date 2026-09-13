# M128: The pipeline function (ffm_*) help pages read as plain English

- **Status:** planned
- **Priority:** high
- **Depends on:** M126
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — shipped help pages for exported functions
- **Branch/PR:** —

## Goal

The M128 help-page domain, the `ffm_*()` pages from `R/ffm.R`, uses plain English for an R user who does not know FFmpeg.

## Scope

**In:** the roxygen text in `R/ffm.R` behind the M128 domain in `cairn/references/plain-docs.md` (20 pages on 2026-09-13). The tests that pin wording on these pages are in scope too.

**Out:** the `?ffm_batch` and `?ffm_jobs` pages go to M127. The task-function pages go to M129 and M130. Code comments in `R/` stay as they are. `NEWS.md` gets no entry (D091).

## Acceptance criteria

- [ ] AC1: The prose sweep over the M128 domain prints no sentence that matches a maintainer term.
- [ ] AC2: The prose sweep over the M128 domain prints no sentence over 25 words.
- [ ] AC3: For each page in the domain, take each glossary stem found in its `--prose` output. The page defines the term at its first use or names the glossary in `vignette("tidymedia")`. One ledger row per page records the stems and how each is met.
- [ ] AC4: Each page in the domain has a ledger row that says what text left it and where that text went: moved (with the page), or deleted (with a reason). Every match of `\btidymedia[._][a-z_.]+` or `\btm_[a-z_]+` in the domain at the base commit is still found in some `man/*.Rd` file at head, or has a ledger row.
- [ ] AC5: `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T2, T3, T4, T5
- AC2 → T2, T3, T4, T5
- AC3 → T2, T3, T4, T5
- AC4 → T1, T5
- AC5 → T5

## Tasks

- [ ] T1: Run the sweep over the domain at the base commit. Record the pages and the AC4 identifiers in a new M128 ledger section.
- [ ] T2: Rewrite the input and output pages: `ffm_files()`, `ffm_copy()`, `ffm_seek()`, `ffm_map()`, `ffm_drop()`, `ffm_codec()`, `ffm_pixel_format()` and `ffm_output_options()`.
- [ ] T3: Rewrite the filter pages: `ffm_trim()`, `ffm_crop()`, `ffm_scale()`, `ffm_fps()`, `ffm_drawbox()` and `ffm_loudnorm()`.
- [ ] T4: Rewrite the multi-input and run pages: `ffm_hstack()`, `ffm_vstack()`, `ffm_overlay()`, `ffm_concat()`, `ffm_compile()` and `ffm_run()`. Move the exit-status detail in `?ffm_run` to a short end section.
- [ ] T5: A test that pins changed wording now pins the new wording of the same property. A test whose claim left the user docs is removed, with a ledger row. A test named in a `cairn/DECISIONS.md` entry is rewritten, never removed. Fill the ledger. Run the sweep, `devtools::document()`, `devtools::check()`, `devtools::test()` with no other R session working, and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-13: created by /milestone-plan (series M126-M130). The criteria audit and gate choices are logged in M126's work log.

## Decisions

## Review
