# M55: A package landing topic, and a `parallel` enumeration that matches the code

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m55-package-topic-parallel-docs` / https://github.com/jmgirard/tidymedia/pull/58

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

- [x] AC1 `R/tidymedia-package.R` carries a `"_PACKAGE"` sentinel; `devtools::document()`
      generates `man/tidymedia-package.Rd`; in an `R CMD INSTALL`ed copy, `?tidymedia`
      and `help(package = "tidymedia")` both resolve to it.
- [x] AC2 The topic is catalogued in `_pkgdown.yml`, and `pkgdown::check_pkgdown()`
      reports no uncatalogued topic.
- [x] AC3 Every line in `vignettes/` containing the string `parallel` — the domain
      enumerated by `grep -rn "parallel" vignettes/`, which returns 3 hits today
      (`batch.Rmd:101,103,112`) — makes only claims true of the shipped package. Each hit's
      disposition (corrected / already true / not a claim about the argument set) is
      recorded in the work log, so a hit left alone is visibly a decision.
- [x] AC4 `vignettes/metadata.Rmd`'s "Batching over many files" section (`:115-126`,
      the `probe_all(files)$container` example) names `parallel =` and says what it does.
- [x] AC5 A test computes, for every name in `getNamespaceExports("tidymedia")` that is a
      function under `get(name, envir = asNamespace("tidymedia"))`, whether
      `"parallel" %in% names(formals())`, and asserts the resulting set equals a literal
      expected vector of the 22 names verified at plan time — the 16 `*_batch` verbs, the
      5 `probe_*()` readers, and `segment_video`. The function filter is required:
      `.data` is exported and non-function, and `formals()` warns on it.
- [x] AC6 PROFILE.md's verify slot clean — `devtools::check()` 0 errors / 0 warnings, read
      from `<pkg>.Rcheck/00check.log`'s `Status:` line rather than devtools' summary
      (LESSONS M17) — and the vignettes build with the ffmpeg/ffprobe/mediainfo binaries
      masked off PATH (LESSONS M30).
- [x] AC7 `devtools::test()` passes.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6
- AC7 → T5, T6

## Tasks

- [x] T1 Add a `"_PACKAGE"` sentinel to `R/tidymedia-package.R`, with **no**
      `@keywords internal` — that keyword is what excludes the generated topic from the
      installed `INDEX`, and so from `help(package = "tidymedia")`, which AC1 requires.
      Place it **above** the two existing usethis namespace blocks, never between a
      roxygen block and what it documents (LESSONS M28). Run `devtools::document()`.
- [x] T2 Catalogue `tidymedia-package` in `_pkgdown.yml`, whose reference index
      (`:6-118`) enumerates every topic explicitly; run `pkgdown::check_pkgdown()`.
- [x] T3 Run `grep -rn "parallel" vignettes/`, disposition each hit in the work log, and
      rewrite `vignettes/batch.Rmd:103` to state the rule (every `*_batch` verb, the five
      `probe_*()` readers, and `segment_video`) rather than a prose gesture at
      "the fan-out verbs".
- [x] T4 Extend `vignettes/metadata.Rmd`'s batching section per AC4.
- [x] T5 Add the `parallel`-formals test per AC5; run `devtools::test()`. Discovered
      sub-task: also add `tests/testthat/test-package-topic.R`, so the NEWS claim about
      `?tidymedia` is enforced by a test rather than by AC1's one-time hand check.
- [x] T6 Run `devtools::document()` and `devtools::check()`; build the vignettes with the
      binaries masked off PATH; confirm the `00check.log` `Status:` line.

## Work log

- 2026-08-06: created by /milestone-plan.
- 2026-08-06: criteria audit ([O], fresh context) returned 5 findings on this milestone's criteria: AC3's grep cannot see a claim that is wrong by *omission* (prose listing batch verbs without the word `parallel` contains no token to match), so the promise was narrowed to lines containing the string and the known omission split out as AC4; AC5's `get(name)` resolving outside the namespace and warning on the non-function export `.data`; AC4's target section unanchored; and AC6 naming `devtools::check()`'s summary, which does not print the `Status:` line it demanded. All fixed before AC wording was written; none needed a gate question. AC1 passed all three questions.
- 2026-08-06: plan gate kept DESCRIPTION `Title` title-casing out, because it is release mechanics owned by the standing CRAN-readiness row and folding it in would blur a docs-only milestone into release-shaped work; falsified by the Title NOTE blocking something other than a submission.
- 2026-08-07: implement gate: package topic goes in a new `Package` section at the TOP of the pkgdown reference index (over folding it into `Concepts`, whose stated subject is recurring arguments, or a bottom section); `vignettes/metadata.Rmd` gets prose naming `parallel =` plus a short non-run example and a pointer to `batch.Rmd` for the `plan()` setup, rather than repeating the setup block in a second place.
- 2026-08-07: T1 done. Two plan assumptions falsified by measurement on an `R CMD INSTALL`ed copy. (1) `@keywords internal` — which T1 mandated — is exactly what excludes the topic from the installed `INDEX`, so `help(package = "tidymedia")` did not list it; stripping only that line from the `.Rd` and reinstalling restored the row (index 104 entries -> the `tidymedia-package` row present). Gate chose delivering AC1 as written over keeping the usethis template line; T1 amended to forbid the keyword. (2) A bare `"_PACKAGE"` with no roxygen block above it is not seen by roxygen at all — `document()` reported `Deleting 'tidymedia-package.Rd'` — so the sentinel carries an `@details` landing body (three-layer orientation + vignette pointers) instead. Both halves of AC1 then verified: `?tidymedia` -> `tidymedia-package`, and the index row is present.
- 2026-08-07: T2 done. New `Package` reference section at the top of `_pkgdown.yml` holding `tidymedia-package`; `pkgdown::check_pkgdown()` reports "No problems found."
- 2026-08-07: T3/T4 done. Dispositions of the three pre-existing `grep -rn "parallel" vignettes/` hits: `batch.Rmd:101` (`## Running in parallel`) — a section heading, not a claim about the argument set, left alone; `batch.Rmd:103` — CORRECTED, the "Both `ffm_batch()` and the fan-out verbs" sentence now names `ffm_batch()`, every `*_batch` verb, `segment_video()`, and the five `probe_*()` readers, and states that the scalar verbs do not take it (`separate_audio_video()` named explicitly, since the old wording implied it did); `batch.Rmd:112` — an `ffm_batch(jobs, parallel = TRUE, ...)` call, already true of the shipped package, left alone. T4 added a `parallel =` paragraph plus a two-line non-run `probe_all(files, parallel = TRUE)` example to `metadata.Rmd`'s "Batching over many files" section, pointing at `batch.Rmd` for the `plan()` setup rather than repeating it.
- 2026-08-07: T5 done, plus a minor amendment adding a discovered sub-task. `test-parallel-surface.R` pins the 22-name set; AC5's `get(name, envir = ns)` needed the INHERITING lookup, not `inherits = FALSE` — `.data` is a re-export bound in the imports env, not the namespace, so the strict form errors "object '.data' not found" before the function filter ever runs. Discriminated by mutation: deleting `parallel` from `probe_video()`'s signature turns it red naming `probe_video`. Sub-task: `test-package-topic.R` pins both halves of AC1 (the topic exists with a `\alias{tidymedia}`; it carries no `\keyword{internal}`), so this milestone's NEWS claim about `?tidymedia` is test-enforced rather than hand-checked once. Discriminated by two mutations, each reddening only its own guard: appending `\keyword{internal}` to the `.Rd` fails the second test alone; deleting the roxygen block above the sentinel makes `document()` delete the `.Rd` and fails the first alone. NEWS gains two Documentation bullets. Full `devtools::test()`: FAIL 0 | WARN 4 | SKIP 5 | PASS 3502, the 4 warnings pre-existing in `test-audio-stream.R` and `test-ffmpeg.R`, untouched here.
- 2026-08-07: T6 done; milestone to `review`. LESSONS M17 fired exactly as written: `devtools::check()` reported "0 notes" while `00check.log` read `Status: 1 NOTE` — a spelling NOTE on `ORCID`, a word the generated `tidymedia-package.Rd` author block introduced. `spelling::update_wordlist()` added it (and dropped the now-unused `unselected`); the re-run's `00check.log` reads `Status: OK`, read from the file under an explicit `check_dir`. `devtools::document()` leaves no diff. Vignettes: all four re-built clean against an `R CMD INSTALL`ed copy of this branch with `ffmpeg`/`ffprobe`/`mediainfo` all absent from `PATH` (asserted via `Sys.which() == ""` before the build, LESSONS M30). Plan-owned body at 88 lines, under the 150 cap.
- 2026-08-07: review. All seven criteria executed with fresh evidence (Review section). Three fresh-context lenses returned 13 findings; the scorer actioned 1. F2 (85) fixed on the branch: the `@details` landing text said the `get_*()` helpers read metadata "as tibbles" alongside `probe_all()`, where they route through `mediainfo_parameter()` to MediaInfo — a different back end — and return a single value. Separately, the prior-review lens caught that the M55 NEWS bullets had been inserted over the opening line of M54's nvenc bullet, splicing the new text onto M54's sentence and leaving `it too. that asking for "nvenc" queries your`; verified against the file and repaired, so NEWS.md is now purely additive against master. Twelve sub-threshold findings logged in the Review section, four of them real inaccuracies in this milestone's own prose (F3 68, F4 68, F5 78, F12 65) carried to the merge gate for the maintainer's call.

## Decisions

## Review

Reviewed 2026-08-07 on branch `m55-package-topic-parallel-docs`, PR #58.

### Acceptance criteria — fresh evidence

- **AC1** — `R CMD INSTALL`ed into a temp library: `utils::help("tidymedia", package = "tidymedia")`
  resolves to the topic `tidymedia-package`; the `help(package = "tidymedia")` index carries the
  row `tidymedia-package   tidymedia: Tools for working with media files`. `man/tidymedia-package.Rd`
  present in the source tree and regenerated by `document()` with no diff.
- **AC2** — `_pkgdown.yml:7-11` holds a new top-level `Package` section listing `tidymedia-package`;
  `pkgdown::check_pkgdown()` reports "No problems found."
- **AC3** — `grep -rn "parallel" vignettes/` now returns 8 hits (3 before). Dispositions of the three
  pre-existing hits: `batch.Rmd:101` (`## Running in parallel`) — a heading, not a claim, left alone;
  `batch.Rmd:103` — corrected, now naming `ffm_batch()`, every `*_batch` verb, `segment_video()`, and
  the five `probe_*()` readers; `batch.Rmd:112` (now `:116`) — an `ffm_batch(jobs, parallel = TRUE, ...)`
  call, already true, left alone. Every claim re-verified against the namespace: the 22-name set is
  exactly the vignettes' enumeration, and all five exported `probe_*` functions take the argument.
  One inaccuracy survives on the corrected line and is logged below (F4, scored 68).
- **AC4** — `vignettes/metadata.Rmd:128-138`, inside "Batching over many files", names `parallel = TRUE`,
  says it fans the per-file probes out with furrr, carries a non-run `probe_all(files, parallel = TRUE)`
  example, and points at `vignette("batch")` for the plan setup.
- **AC5** — `tests/testthat/test-parallel-surface.R` computes the set off `getNamespaceExports()`
  filtered to functions and asserts it equals the literal 22-name vector; it passes, and it
  discriminates — deleting `parallel` from `probe_video()`'s signature turns it red naming
  `probe_video`. The function filter is exercised by a second test pinning that `.data` is not a
  function.
- **AC6** — `devtools::check(check_dir = ...)`, `00check.log` read from disk: `Status: OK` (51 checks).
  All four vignettes re-built clean against an `R CMD INSTALL`ed copy of this branch with
  `ffmpeg`/`ffprobe`/`mediainfo` absent from `PATH`, asserted via `Sys.which() == ""` before the build.
- **AC7** — `devtools::test()`: FAIL 0 | WARN 4 | SKIP 5 | PASS 3505. The same totals under `R CMD check`
  (`testthat.Rout`), which is how the two new Rd-reading guards are shown to run in the check shape
  rather than skip there (LESSONS M51). The 4 warnings pre-date this branch, in `test-audio-stream.R`
  and `test-ffmpeg.R`, neither touched here.

### Consistency gate

`cairn_validate` exit 0, all 16 checks PASS and all 8 advisories OK. No DESIGN principle changed, so
`cairn_impact` is skipped. Profile `consistency-gate` slot: `document()` no diff; `NAMESPACE`/`man/`
not hand-edited; README.Rmd untouched by this diff so README.md stays in sync; `check_pkgdown()` clean;
NEWS.md has entries for both user-visible changes; no new top-level files, so no `.Rbuildignore` work;
full `check()` `Status: OK`. CI green on all 9 jobs at the pre-fix commit; re-run required after the
review commits.

### Independent review

Three fresh-context lenses (diff-bug [O], blame-history [S], prior-PR-comments [S]) reported 13
findings; a fresh [S] scorer holding the diff and this milestone file scored them.

**Actioned (>= 80), 1 of 13:**

- **F2 (85) — the `@details` landing text misattributed the `get_*()` helpers.** It said container and
  stream metadata "are read as tibbles by `probe_all()` and the `get_*()` helpers"; the `get_*()`
  helpers route through `mediainfo_parameter()` to MediaInfo, a different back end from FFprobe, and
  return a single value rather than a tibble. **Fixed on the branch:** the sentence now names the two
  back ends separately and points at `mediainfo_query()` and `get_duration()` for the MediaInfo side.
  Re-verified: every cross-reference resolves, `document()` regenerates, `check()` `Status: OK`.

**Below threshold, logged not actioned (12):** F1 (15) NEWS.md corruption — the M55 bullets were
inserted over the opening line of M54's nvenc bullet, splicing the new text onto M54's sentence; found
by the prior-review lens, verified against the file, and fixed during review, so it scored low as
already-repaired rather than as unreal. F5 (78) the new `metadata.Rmd` example warns under the default
sequential plan (`warn_if_sequential_plan()`, D033) and the prose describes a quiet no-op instead.
F3 (68) `batch.Rmd` calls `separate_audio_video()` a "scalar verb" 25 lines after its own `## Fan-out
verbs` heading classes it as a fan-out verb. F4 (68) both vignettes state the `probe_*()` rule without
D033's caveat that `parallel` is discarded, silently, when a `probe` object is passed instead of
`infile`. F12 (65) the landing topic names three of the package's four vignettes, omitting `workflow`.
F6 (55) the test's inheriting `get()` could in principle resolve a same-named object from
`.GlobalEnv`. F11 (35) NEWS's "Previously neither reached anything" is ambiguous. F7 (35) the
`expect_gte` floor can never fire before the adjacent `expect_identical`. F8 (30) the formals test
cannot pin the prose it protects. F9 (30) the `\keyword{internal}` assertion is a proxy that never
reads the installed `INDEX`. F10 (20) `helper-rd.R`'s `Rd_db` fallback passes no `lib.loc =`
(pre-existing). F13 (15) M51's archived work-log account of why `unselected` entered `inst/WORDLIST`
does not hold up, which is a fact about a past record rather than about this diff.

Return floor: no actioned finding demonstrates an acceptance criterion failing, and F2 is a
documentation defect rather than one in what the package does for users, so the milestone stays at
`review`.
