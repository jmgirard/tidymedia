# M55: A package landing topic, and a `parallel` enumeration that matches the code

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Give `?tidymedia` a topic to resolve to, and make every vignette line mentioning
`parallel` true of the shipped package.

## Scope

**In:** a `"_PACKAGE"` sentinel in `R/tidymedia-package.R` so `?tidymedia` and
`help(package = "tidymedia")` resolve, with the generated topic catalogued in
`_pkgdown.yml`; a correction to `vignettes/batch.Rmd:103`, which both over-claims
(`separate_audio_video()` has no `parallel` formal — only its `_batch` sibling does) and
under-claims (it omits the 16 `*_batch` verbs and M53's five `probe_*()` readers); a
mention of `parallel =` in `vignettes/metadata.Rmd`'s "Batching over many files" section,
whose `probe_all(files)` example is silent about it; and a test pinning the ground-truth
set so a future export gaining or losing `parallel` goes red instead of rotting the prose.

**Out:**
- DESCRIPTION `Title` title-casing → CRAN-readiness ROADMAP row (release mechanics).
- The roxygen `@examples` pass → CRAN-readiness ROADMAP row.
- Vignette claims about arguments other than `parallel` → not swept by this milestone;
  a future drift of the same shape gets its own row.
- Any change to which functions take `parallel` → docs only, no API change.

## Acceptance criteria

- [ ] AC1 `R/tidymedia-package.R` carries a `"_PACKAGE"` sentinel; `devtools::document()`
      generates `man/tidymedia-package.Rd`; in an `R CMD INSTALL`ed copy, `?tidymedia`
      and `help(package = "tidymedia")` both resolve to it.
- [ ] AC2 The topic is catalogued in `_pkgdown.yml`, and `pkgdown::check_pkgdown()`
      reports no uncatalogued topic.
- [ ] AC3 Every line in `vignettes/` containing the string `parallel` — the domain
      enumerated by `grep -rn "parallel" vignettes/`, which returns 3 hits today
      (`batch.Rmd:101,103,112`) — makes only claims true of the shipped package. Each hit's
      disposition (corrected / already true / not a claim about the argument set) is
      recorded in the work log, so a hit left alone is visibly a decision.
- [ ] AC4 `vignettes/metadata.Rmd`'s "Batching over many files" section (`:115-126`,
      the `probe_all(files)$container` example) names `parallel =` and says what it does.
- [ ] AC5 A test computes, for every name in `getNamespaceExports("tidymedia")` that is a
      function under `get(name, envir = asNamespace("tidymedia"))`, whether
      `"parallel" %in% names(formals())`, and asserts the resulting set equals a literal
      expected vector of the 22 names verified at plan time — the 16 `*_batch` verbs, the
      5 `probe_*()` readers, and `segment_video`. The function filter is required:
      `.data` is exported and non-function, and `formals()` warns on it.
- [ ] AC6 PROFILE.md's verify slot clean — `devtools::check()` 0 errors / 0 warnings, read
      from `<pkg>.Rcheck/00check.log`'s `Status:` line rather than devtools' summary
      (LESSONS M17) — and the vignettes build with the ffmpeg/ffprobe/mediainfo binaries
      masked off PATH (LESSONS M30).
- [ ] AC7 `devtools::test()` passes.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6
- AC7 → T5, T6

## Tasks

- [ ] T1 Add `#' @keywords internal` + `"_PACKAGE"` to `R/tidymedia-package.R`. Place it
      **above** the two existing usethis namespace blocks, never between a roxygen block
      and what it documents (LESSONS M28). Run `devtools::document()`.
- [ ] T2 Catalogue `tidymedia-package` in `_pkgdown.yml`, whose reference index
      (`:6-118`) enumerates every topic explicitly; run `pkgdown::check_pkgdown()`.
- [ ] T3 Run `grep -rn "parallel" vignettes/`, disposition each hit in the work log, and
      rewrite `vignettes/batch.Rmd:103` to state the rule (every `*_batch` verb, the five
      `probe_*()` readers, and `segment_video`) rather than a prose gesture at
      "the fan-out verbs".
- [ ] T4 Extend `vignettes/metadata.Rmd`'s batching section per AC4.
- [ ] T5 Add the `parallel`-formals test per AC5; run `devtools::test()`.
- [ ] T6 Run `devtools::document()` and `devtools::check()`; build the vignettes with the
      binaries masked off PATH; confirm the `00check.log` `Status:` line.

## Work log

- 2026-08-06: created by /milestone-plan.
- 2026-08-06: criteria audit ([O], fresh context) returned 5 findings on this milestone's criteria: AC3's grep cannot see a claim that is wrong by *omission* (prose listing batch verbs without the word `parallel` contains no token to match), so the promise was narrowed to lines containing the string and the known omission split out as AC4; AC5's `get(name)` resolving outside the namespace and warning on the non-function export `.data`; AC4's target section unanchored; and AC6 naming `devtools::check()`'s summary, which does not print the `Status:` line it demanded. All fixed before AC wording was written; none needed a gate question. AC1 passed all three questions.
- 2026-08-06: plan gate kept DESCRIPTION `Title` title-casing out, because it is release mechanics owned by the standing CRAN-readiness row and folding it in would blur a docs-only milestone into release-shaped work; falsified by the Title NOTE blocking something other than a submission.

## Decisions

## Review
