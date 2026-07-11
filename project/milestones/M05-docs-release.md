# M05: Docs, vignettes, pkgdown, release prep

- **Status:** planned <!-- mirror of ROADMAP.md; ROADMAP wins on conflict -->
- **Created:** 2026-07-10
- **Completed:** —

## Goal

The three-layer engine (M02–M04) is feature-complete but under-documented: only
2 of ~60 exports carry `@examples`, `@family` tags are sparse, there are no
vignettes, and no pkgdown site. This milestone brings the reference,
long-form docs, and packaging metadata up to a **release-ready** bar — a
`0.1.0` version, an `R CMD check --as-cran`-clean build, a deployed pkgdown
site, and a tagged GitHub release — without actually submitting to CRAN.

## Scope

**In:** roxygen polish (worked `@examples` on every export, `@family` on every
export), three vignettes (pipeline builder, metadata-as-tibbles, batch
processing), `_pkgdown.yml` + GitHub Pages deploy workflow, DESCRIPTION
metadata (URL, BugReports, VignetteBuilder), version bump to `0.1.0`,
`cran-comments.md`, spelling check, README regeneration, NEWS `0.1.0` heading.

**Out:** actual CRAN submission / `submit_cran()`; any new features or API
changes (docs describe the API as it stands after M04); changing the lifecycle
badge from experimental; reverse-dependency checks; new command capabilities.

## Acceptance criteria

Each criterion verifiable with evidence at review time.

- [ ] `devtools::check(cran = TRUE)` → 0 errors, 0 warnings; examples run
      (binary-gated ones wrapped so they are skipped, not failed).
- [ ] Every exported function has a worked `@examples` block (`\dontrun{}` only
      where a binary is required). Evidence: no `checkDocFiles`/missing-example
      notes; spot-grep shows `@examples` on all exported topics.
- [ ] Every export carries a `@family` tag; `_pkgdown.yml` reference index lists
      every topic with no "uncategorized" leftovers (pkgdown build reports none).
- [ ] Three vignettes build under `R CMD check` (binary-gated content is
      `eval=FALSE` or pre-rendered so CI has no binary dependency).
- [ ] `pkgdown::build_site()` succeeds locally; deploy workflow committed and
      green on CI; `URL`/`BugReports` set in DESCRIPTION.
- [ ] `spelling::spell_check_package()` returns no unexpected words.
- [ ] Version is `0.1.0`; NEWS.md has a `0.1.0` heading; `cran-comments.md`
      present; README.md regenerated from README.Rmd and current.

## Plan

Tasks sized to one working session or less, ordered by dependency.

- [ ] T1: DESCRIPTION + infra — add `URL`, `BugReports`, `VignetteBuilder:
      knitr`; add `knitr`/`rmarkdown`/`pkgdown` to Suggests; bump to `0.1.0`;
      scaffold `usethis::use_pkgdown()`. `check()` still clean.
- [ ] T2: Roxygen pass A (Layer 0/1) — `@family` + worked `@examples` for
      `ffmpeg`/`ffprobe`/`mediainfo` raw wrappers and the `ffm_*` builder
      (builder examples runnable via `ffm_compile()`, no binaries). `document()`.
- [ ] T3: Roxygen pass B (Layer 2 + helpers) — `@family` + `@examples` for task
      verbs, `probe_*`/`get_*`/`mediainfo_*`, program-management, and utils;
      `\dontrun{}` for binary-gated calls. `document()`; examples check clean.
- [ ] T4: Vignette "Building ffmpeg pipelines" — builder + compile, binary-free
      so it renders on CI.
- [ ] T5: Vignette "Media metadata as tibbles" — `probe_*`/`mediainfo_*`,
      binary-gated content `eval=FALSE`/pre-rendered.
- [ ] T6: Vignette "Batch processing" — `ffm_batch` + fan-out verbs.
- [ ] T7: pkgdown — `_pkgdown.yml` reference index grouped by the three layers +
      articles; `use_pkgdown_github_pages()` deploy workflow.
- [ ] T8: Release prep — regenerate README (`build_readme()`), `cran-comments.md`,
      spelling check, NEWS `0.1.0` heading, final `check(cran = TRUE)`.

## Work log

Append-only; newest last. One line per session: date, what happened, next.

- 2026-07-10: Milestone planned (release-ready not CRAN-submit; 3 vignettes;
  pkgdown auto-deploy; full example coverage). Next: `/milestone implement M05`.

## Decisions

Milestone-local decisions; promote cross-cutting ones to ../DECISIONS.md.

- 2026-07-10: Release target is "release-ready" (0.1.0 + tagged GitHub release +
  `--as-cran`-clean), not an actual CRAN submission — package is still
  lifecycle-experimental.
- 2026-07-10: Vignettes must not require binaries on CI: builder vignette uses
  binary-free `ffm_compile()`; metadata/batch vignettes gate binary output with
  `eval=FALSE` or pre-rendered chunks.

## Review

Filled in by `/milestone review`.

- Criteria verification:
- check()/test()/coverage results:
- Follow-ups spawned:
