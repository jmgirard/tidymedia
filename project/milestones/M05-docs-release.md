# M05: Docs, vignettes, pkgdown, release prep

- **Status:** review <!-- mirror of ROADMAP.md; ROADMAP wins on conflict -->
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

- [x] T1: DESCRIPTION + infra — `URL`/`BugReports`/`VignetteBuilder`; Suggests
      knitr/rmarkdown; `Config/Needs/website: pkgdown`; drop stray `LazyData`;
      bump to `0.1.0`; add `inst/extdata/sample.mp4` fixture. (2026-07-10)
- [x] T2: Roxygen — builder (`ffm.R`/`ffm_batch`/`print`): `@family builder` +
      runnable binary-free `@examples`; `ffm_run` gated. (2026-07-10)
- [x] T3: Roxygen — raw wrappers, task verbs, `probe_*`/`get_*`/`mediainfo_*`,
      program-mgmt, utils. Verbs use `run = FALSE` (binary-free); raw/query verbs
      `@examplesIf`; families reconciled to the layer taxonomy. (2026-07-10)
- [x] T4: Vignette `tidymedia.Rmd` (Get started / builder) — binary-free, is the
      package index vignette. (2026-07-10)
- [x] T5: Vignette `metadata.Rmd` — `probe_*`/`mediainfo_*`, per-chunk
      `eval = nzchar(Sys.which(...))` guards. (2026-07-10)
- [x] T6: Vignette `batch.Rmd` — `ffm_batch` + fan-out verbs, `run = FALSE`
      (binary-free). All three render clean. (2026-07-10)
- [x] T7: pkgdown — `_pkgdown.yml` reference grouped by layer; deploy workflow
      (installs binaries so the metadata article renders). `check_pkgdown()` and
      `build_site()` clean; workflow strips the internal CLAUDE.md page. (2026-07-10)
- [x] T8: Release prep — README modernized to the current API + fixture (real
      output), `cran-comments.md`, WORDLIST + spelling clean, NEWS `0.1.0`
      heading, drawbox typo fixed. `check(cran = TRUE)`: 0/0/0. (2026-07-10)

## Work log

Append-only; newest last. One line per session: date, what happened, next.

- 2026-07-10: Milestone planned (release-ready not CRAN-submit; 3 vignettes;
  pkgdown auto-deploy; full example coverage). Next: `/milestone implement M05`.
- 2026-07-10: T1 done — DESCRIPTION metadata + 0.1.0, dropped LazyData (no
  data/), added sample.mp4 fixture. Next: T2 roxygen examples (Layer 0/1).
- 2026-07-10: T2+T3 done — @family + @examples across all ~48 exported topics
  (verbs demoed with run = FALSE; binary queries @examplesIf). test() 234 pass /
  0 fail, examples run clean. Next: T4 vignettes.
- 2026-07-10: T4-T6 done — three vignettes (get-started/metadata/batch); all
  render clean; binary chunks eval-guarded. Next: T7 pkgdown.
- 2026-07-10: T7 done — _pkgdown.yml (layer-grouped reference) + Pages deploy
  workflow; check_pkgdown/build_site clean. Next: T8 release prep.
- 2026-07-10: T8 done — README modernized, cran-comments, WORDLIST, NEWS 0.1.0.
  `check(cran = TRUE)` 0/0/0; test() 246 pass. All tasks complete → review.

## Decisions

Milestone-local decisions; promote cross-cutting ones to ../DECISIONS.md.

- 2026-07-10: Release target is "release-ready" (0.1.0 + tagged GitHub release +
  `--as-cran`-clean), not an actual CRAN submission — package is still
  lifecycle-experimental.
- 2026-07-10: Vignettes must not require binaries on CI: builder vignette uses
  binary-free `ffm_compile()`; metadata/batch vignettes gate binary output with
  `eval=FALSE` or pre-rendered chunks.
- 2026-07-10 (gate): Reference index & `@family` tags organized by architecture
  layer per D002 (escape-hatch / builder / task-verbs / metadata /
  program-management / utils); user deferred, Opus chose to match the design.
- 2026-07-10 (gate): Examples use committed fixture `inst/extdata/sample.mp4`
  (video+audio) as input; binary-free builder/`ffm_compile()` examples run
  everywhere; binary-invoking examples gate with
  `@examplesIf nzchar(Sys.which("<bin>"))` (skip-not-fail when CLI absent).

## Review

- Criteria verification (2026-07-10, fresh):
  - `check(cran = TRUE)`: 0 errors / 0 warnings / 0 notes; vignettes re-build OK.
  - Example coverage: 0 non-internal man topics missing `@examples`.
  - `@family` on 48 topics; `pkgdown::check_pkgdown()` clean (no missing/dupe).
  - `spelling::spell_check_package()`: clean. `build_site()` clean.
  - Version 0.1.0; NEWS 0.1.0 heading; cran-comments + deploy workflow present.
- check()/test() results: check 0/0/0; `test()` 246 pass / 0 fail / 0 skip.
- PR: https://github.com/jmgirard/tidymedia/pull/5 (draft).
- Opus diff review: no blocking findings. 4 minor items triaged — fixed the
  redundant `.Rbuildignore` pkgdown-workflow line; rejected 3 (concat writes to
  tempdir not the check dir; `find_*` example correctly shows the NULL+warning
  return; single pkgdown alias per shared topic is correct usage).
- Follow-ups spawned: none.
- 2026-07-10: CI caught a Linux-only `checking examples` ERROR — the `ffm_run`
  example's real encode let FFmpeg drain R CMD check's stdin (examples fed via
  stdin), desyncing the parser. Fixed `ffmpeg()` with `input = ""` (≈ -nostdin);
  local check still 0/0/0. Re-pushed for CI.
