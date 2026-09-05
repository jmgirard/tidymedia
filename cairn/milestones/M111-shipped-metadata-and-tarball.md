# M111: The shipped metadata and the tarball say what CRAN's incoming checks expect

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — DESCRIPTION and the tarball's contents are what an installer and the CRAN page show
- **Branch/PR:** `m111-shipped-metadata-and-tarball`

## Goal

Rewrite `DESCRIPTION`'s `Title` and `Description` to the form CRAN's incoming
checks accept, and stop shipping four files and one URL that should not be in
the tarball.

## Scope

**In:** the `Title` and `Description` fields; removing
`inst/extdata/ffmpeg_location.rds`, `inst/extdata/mediainfo_location.rds` and
`tests/testthat/_problems/`; the retired Homebrew branch in `README.Rmd:74`;
`_pkgdown.yml`'s duplicate `hardware_encoder`/`has_hardware_encoder` rows and
the alias-only sections that hide `set_mediainfo` and `probe_video` from the
reference index.

**Out:** `cran-comments.md`, the version bump, win-builder and R-hub → the
CRAN readiness candidate row, promoted when a window is declared. README's
macOS dead-end and its unguarded chunks → M114. `SystemRequirements`'
URL-bearing form, which is legal and stays.

## Acceptance criteria

- [ ] AC1: `R CMD check --as-cran` over the built tarball, run with
      `_R_CHECK_CRAN_INCOMING_=TRUE` and `_R_CHECK_CRAN_INCOMING_REMOTE_=FALSE`,
      reports no NOTE naming the `Title` field or the `Description` field.
      Evidence: the check's complete NOTE list, quoted.
- [ ] AC2: `Title` is in title case and writes 'tidyverse' in single quotes;
      `Description` writes 'FFmpeg', 'MediaInfo' and 'tidyverse' in single
      quotes, contains neither the substring "The goal of" nor a leading
      "tidymedia", and runs to at least two sentences. Evidence: both fields
      quoted verbatim, with each clause checked against the quoted bytes.
- [ ] AC3: The tarball `R CMD build` produces contains no path matching
      `extdata/ffmpeg_location.rds`, `extdata/mediainfo_location.rds`, or
      `testthat/_problems/`. Evidence: `tar -tzf` over the built tarball,
      grepped for the three patterns, showing no hits and a non-zero total.
- [ ] AC4: `urlchecker::url_check()` over the package reports no URL needing a
      change. Evidence: the checker's full output.
- [ ] AC5: No topic name appears more than once across `_pkgdown.yml`'s
      `contents:` entries, verified by a script that parses the file and
      reports every repeated entry, and `pkgdown::check_pkgdown()` passes.
      Evidence: both outputs.
- [ ] AC6: `devtools::document()` produces no diff and `devtools::test()` and
      `devtools::check()` are clean — 0 errors, 0 warnings — with each NOTE
      the check reports quoted and justified. Evidence: the three tails.

## Coverage

- AC1 → T1, T6
- AC2 → T1
- AC3 → T2, T3
- AC4 → T4
- AC5 → T5
- AC6 → T6

## Tasks

- [x] T1: Rewrite `Title` and `Description` in `DESCRIPTION:2-13`. The
      Description says what the package does with 'FFmpeg' and 'MediaInfo' —
      batch transformation and metadata extraction as tibbles — rather than
      restating the Title.
- [ ] T2: Delete `inst/extdata/ffmpeg_location.rds` and
      `inst/extdata/mediainfo_location.rds`. Confirm first that nothing
      resolves either name at runtime: grep `R/` and `tests/` for
      `system.file`, `extdata` and `location` together, not for the basenames
      alone, which a `paste0()` would defeat.
- [ ] T3: Delete `tests/testthat/_problems/`; record its one file's fate in
      the work log so the deletion is not silent.
- [x] T4: Point `README.Rmd:74`'s Homebrew URL at the branch Homebrew
      documents today, re-knit with `devtools::build_readme()`, and run
      `urlchecker::url_check()`.
- [ ] T5: Collapse `_pkgdown.yml:120-123`'s duplicate rows to one topic and
      name `set_program`/`find_program`/`probe_container`'s aliases in their
      sections so `set_mediainfo` and `probe_video` are findable by scanning.
- [ ] T6: Build the tarball, run the `--as-cran` check with the two
      environment variables set, then `devtools::check()` clean.

## Work log

- 2026-09-05: created by /milestone-plan.
- 2026-09-05: criteria audit ran in FULL mode (user-facing tier), fresh-context [O] reader. Returned four findings against this milestone's draft: the `tools:::.check_package_description()` criteria were vacuous, "not a restatement of the Title" was unmechanizable, the NOTE-count baseline was unfixed, and the `*_location.rds` basename grep already passed before any deletion. All four fixed before writing; none needed a gate question.
- 2026-09-05: plan gate chose `R CMD check --as-cran` with the incoming checks forced on over `tools:::.check_package_description()`, because the latter runs no title-case or boilerplate check at all — measured 2026-09-05, it returns empty against the current defective DESCRIPTION under both `strict = FALSE` and `strict = TRUE`. Falsified by an `--as-cran` run that stays silent on a Title CRAN then rejects by hand.
- 2026-09-05: plan gate chose to assert the removals against the BUILT TARBALL's contents over a grep for the removed basenames in the sources, because the sources never name them — the grep passes today, before any deletion. Falsified by a `.Rbuildignore` rule that hides a file from the tarball while it stays on disk and in the installed package.
- 2026-09-05: implement started on `m111-shipped-metadata-and-tarball`. Gate chose the Title "Media File Preprocessing and Metadata for the 'tidyverse'" over a title-cased copy of the shipped wording, and chose page names (`find_program`, `set_program`) over the `find_ffmpeg`/`set_ffmpeg` aliases in the reference index so the MediaInfo functions are visible when scanning.
- 2026-09-05: T3's deleted file, `tests/testthat/_problems/test-timeout-silence-55.R`, held a stale copy of `test-timeout-silence.R:59`'s `tm_timeout_absorbers()` assertion, omitting `separate_audio_video` — the live assertion supersedes it, so the deletion loses nothing.
- 2026-09-05: discovered sub-task under T3 — `_problems/` is in `.gitignore` but was not in `.Rbuildignore`, so `R CMD build` shipped it. Added `^tests/testthat/_problems$` so a later failing run cannot put it back in the tarball; the directory is deleted as well, not merely hidden.
- 2026-09-05: checkpoint, work half-done. T1-T5 edits are in the tree; no task checked off, `devtools::test()` still running past 10 minutes and README not yet re-knitted.
- 2026-09-05: T1 done. `devtools::test()` clean over the rewritten DESCRIPTION (FAIL 0 | WARN 10 | SKIP 18 | PASS 12614, 22m34s), `devtools::document()` produces no diff, `spelling::spell_check_package()` finds nothing.
- 2026-09-05: T4 done. Homebrew's installer URL moved from `install/master/` to `install/HEAD/`; README re-knitted. `urlchecker::url_check()` then found a second URL needing a change, the lifecycle badge's `https://www.tidyverse.org/lifecycle/#experimental` (301). Its redirect target `https://tidyverse.org/lifecycle/` returns 404 — measured 2026-09-05 with `curl -sI` — so urlchecker's own suggestion was not taken; the badge now points at `https://lifecycle.r-lib.org/articles/stages.html#experimental` (200). Re-run over 24 URLs: "All URLs are correct!".

## Decisions

## Review
