# M119: The documentation defects the pre-CRAN audit measured are corrected

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP1
- **Resolves:** —
- **Surface tier:** user-facing — every artifact here ships or is read by a user
- **Branch/PR:** `m119-shipped-docs-defects`

## Goal

The six shipped-documentation defects measured on 2026-09-07 are corrected, and
building the vignettes writes nothing into the source tree.

## Scope

**In:** the six measured defects — the vignette's false return-value claim, the
vignette chunk that writes into `vignettes/`, `find_program`'s documented-but-
unexported page, `SUPPORT.md`'s 2020 placeholders, the never-run URL check, and the
missing `inst/CITATION`.

**Out:** a README batch example → ROADMAP candidate row (declined at this plan's
gate for the pre-release set). The eleven docs gaps of the existing merged candidate
row → that row, unchanged. Exporting `find_program` → refused here under GP1; a new
permanent export is a decision-level act, not a docs fix. `cairn/DESIGN.md`'s stale
naming surface → its own candidate row.

## Acceptance criteria

- [x] AC1: The sentence at `vignettes/tidymedia.Rmd:43` stating what a task verb
      returns agrees with what `extract_audio()` returns in the chunk above it. Today
      the sentence says the verb "returns the path it wrote" while `extract_audio()`'s
      `@return` says the compiled FFmpeg command, invisibly when `run = TRUE`.
- [x] AC2: Run with the three binaries on `PATH`, from a tree whose single
      before-listing — `ls -A vignettes/`, not `git status`, which `.gitignore`
      blinds here — names only paths tracked by git: `rmarkdown::render()` over
      each `.Rmd` in `vignettes/`, at the format that vignette's YAML declares and
      each completing without error, leaves `vignettes/` holding those paths plus
      one `.html` per vignette, five in all, and no other path. `R CMD build`
      builds in a temporary copy and cannot observe this; the in-place render is
      the discriminating instrument.
- [x] AC3: `man/find_program.Rd` carries no `\usage{}` entry for `find_program`,
      `_pkgdown.yml` does not list `find_program` in its reference index, and
      `pkgdown::check_pkgdown()` passes.
- [x] AC4: A grep over `.github/SUPPORT.md` for `with $`, ` !` and `{{` returns
      nothing, and the package references derived from the template's placeholders
      name `tidymedia`.
- [x] AC5: `urlchecker::url_check()` over the built package reports no dead URL, and
      every redirect it reports is either followed in the source or recorded with the
      reason it is kept.
- [x] AC6: `inst/CITATION` exists and `citation("tidymedia")` returns it rather than
      the auto-generated fallback.
- [x] AC7: `devtools::check()` reports 0 errors and 0 warnings, and the `verify` slot
      of `cairn/PROFILE.md` is clean.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6
- AC7 → T7

## Tasks

- [x] T1: Settle which side of `vignettes/tidymedia.Rmd:43` is wrong by running the
      chunk above it, then correct the prose (or the chunk) to match.
- [x] T2: Give `vignettes/tidymedia.Rmd`'s setup chunk a `root.dir = tempdir()`
      the way `verification.Rmd:33` does, remove the untracked
      `vignettes/audio.m4a` the current chunk left behind, and render in place to
      confirm nothing new lands.
- [x] T3: Drop `find_program` from the Rd `\usage{}` and from `_pkgdown.yml:127`,
      keeping the four exported `find_*()` wrappers documented as they are.
- [x] T4: Fill `.github/SUPPORT.md`'s placeholders (`:1` and `:3`) and point its
      support path at this package's issue tracker.
- [x] T5: Run `urlchecker::url_check()`; resolve the redirecting Contributor Covenant
      link and settle the FFmpeg anchor family, which spells the same filters both
      `#concat` and `#toc-concat`.
- [x] T6: Write `inst/CITATION` from DESCRIPTION's author block and ORCID.
- [x] T7: `devtools::document()`, `devtools::check()`, `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-07: created by /milestone-plan.
- 2026-09-07: plan-gate criteria audit ran in FULL mode (declared tier user-facing), two rounds, fresh-context [O] reader. Findings against this milestone: the round-1 title quantified over all vignettes, all of `man/`, README and pkgdown while AC1 bound one sentence (repaired — title narrowed to what the audit measured); AC3's "`find_program` is not exported" conjunct was already true at head and could not fail (repaired — dropped, and `pkgdown::check_pkgdown()` added, since removing the index row while keeping the topic can trip PROFILE's consistency gate); AC3's round-1 `_pkgdown.yml` clause was unparseable (repaired); AC4's "every sentence naming a package names tidymedia" was falsified by the file's own reprex and tidyverse sentences and named no procedure (repaired — narrowed to the placeholder-derived references); AC5's dead-URL clause gained a disposition arm. AC1, AC2, AC5 and AC6 passed all six questions clean.
- 2026-09-07: plan gate chose unexporting `find_program` from its docs over exporting the function, because GP1 prefers refusing surface to growing it and D014 makes a new permanent export a decision-level act rather than a documentation repair; falsified by a report of a caller needing the generic that the four wrappers cannot serve.
- 2026-09-09: T1 — measured the chunk rather than guessing: `withVisible(extract_audio(video, "audio.m4a"))` returns the compiled command with `visible = FALSE`, and a knit of the vignette renders that chunk with no output block at all (FFmpeg's stderr does not reach the document). The prose was the wrong side; corrected to say the verb returns the command it ran, invisibly.
- 2026-09-09: T2 — `root.dir = tempdir()` added to `vignettes/tidymedia.Rmd`'s setup chunk; the untracked `vignettes/audio.m4a` removed. Three procedures measured today against the unfixed tree: `R CMD build` no leak, `devtools::build_vignettes()` no leak, in-place `rmarkdown::render()` from `vignettes/` **reproduced** `audio.m4a`. Rendering all five in place with the fix leaves the five `.html` outputs and no media artifact.
- 2026-09-09: AMENDMENT (substantive) — AC2's procedure changed from `R CMD build` to the in-place render, at the mini gate, recommendation taken. `R CMD build` copies the package tree to a temporary directory before building vignettes (`tools:::.build_packages` does `Tdir <- tempfile("Rbuild")` then `setwd(Tdir)`), so it reported the same clean result at head and after the fix: the criterion as planned could not fail. Instrument swap at the same promise size, not a widening; T2's wording followed as a minor amendment.
- 2026-09-09: re-audit: AC2 (full) — not clean, four repairs returned: name the build-path substitution rather than leaving it invisible; state the `.html` count so an empty `vignettes/*.Rmd` glob cannot pass vacuously; name the enumerating command and pin it to one listing, since `.gitignore` blinds `git status` to every artifact kind at issue; restate the tracked-only clause as a precondition rather than an arm of the promise. All four taken. A fifth suggestion — extending the check to paths outside `vignettes/` — was declined as a domain widening past Scope In, which names the chunk that writes into `vignettes/`.
- 2026-09-09: T3 — `@usage NULL` on the `find_program` block drops it from the topic's `\usage{}`, leaving the four exported wrappers; `@param program` went with it, since no documented usage takes it any more, and the description now names the four wrappers instead. `\alias{find_program}` stays, so `?find_program` and the package's own `[find_program()]` cross-references still resolve. `_pkgdown.yml:127` lists `find_ffmpeg` in place of `find_program` (gate choice): pkgdown resolves the alias to the same topic, so the topic stays indexed. Positive control run — deleting that index line makes `pkgdown::check_pkgdown()` abort in `check_missing_topics()`, so AC3's third conjunct discriminates rather than passing vacuously.
- 2026-09-09: T4 — `.github/SUPPORT.md`'s two stripped placeholders now name `tidymedia` (`# Getting help with tidymedia`, `Thanks for using tidymedia!`); all three AC4 greps (`with $`, ` !`, `{{`) return nothing. T4's support-path clause was already true at head — line 19 pointed at `jmgirard/tidymedia/issues/new` — so nothing there changed. Gate choice also took the two tidyverse claims the template left: community.rstudio.com → Posit Community, and the "development of tidyverse packages" maintenance paragraph rewritten for a single maintainer. The reprex and tidyverse.org/help links stay: they are real external guides, not claims about this package. The file is `.Rbuildignore`d and holds no R code, so the `verify` slot was not re-run for this task; T7's `devtools::check()` covers the tree.
- 2026-09-09: T5 — `urlchecker::url_check()` over the built package (`R CMD build` then the extracted tree) reports **All URLs are correct**: no dead URL, and no redirect, so AC5's disposition arm holds over an empty set. Positive control: a 404 injected into a valid Rd section of the built copy is caught (`✖ 404: Not Found`), so the dead-URL conjunct discriminates. **Second control is a negative result worth recording** — a known 2-hop redirect injected beside it was *not* reported; this urlchecker follows redirects silently and flags only failures, so AC5's redirect arm cannot fire under its own instrument.
- 2026-09-09: T5 — the redirecting Contributor Covenant link was therefore found with `curl -IL`, not with the criterion's instrument: `README.Rmd:218` pointed at `https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html`, two hops from the live page. Repointed at the terminal URL (`https://www.contributor-covenant.org/version/2/0/code_of_conduct/`, 200, 0 redirects) and `README.md` rebuilt.
- 2026-09-09: T5 — the anchor family is measured, not guessed: `ffmpeg-filters.html` was fetched and every anchor the tree uses resolves, `#crop`/`#toc-crop` and `#concat`/`#toc-concat` alike, so no link was dead. Per the gate, only the one shipped `@references` using a table-of-contents anchor changed (`R/ffm.R:261`, `#toc-crop` → `#crop`) so that a reader lands on the section; the 22-line `#toc-` comment block at `R/ffm.R:1678-1699` is source, not shipped documentation, and was left alone.
- 2026-09-09: T6 — `inst/CITATION` written from DESCRIPTION's author block and ORCID, with year and version read from `meta` at `citation()` time (gate choice), so neither goes stale between releases; `Date/Publication` exists only in a CRAN-installed copy, so a source install falls back to the current year. AC6's second conjunct has a natural control: the run just before the install returned the auto-generated fallback (plain title from DESCRIPTION's old `Title`, `<https://github.com/jmgirard/tidymedia>`), and the run after returns the file — braced `{tidymedia}:` title and the pkgdown URL, neither of which the fallback can produce. The ORCID is carried on the author (`Jeffrey Girard (ORCID: <https://orcid.org/0000-0002-7359-3746>)`).
- 2026-09-09: T7 — `devtools::document()` leaves no diff; `devtools::check()` **0 errors, 0 warnings, 0 notes** in 5m47.3s (`checking Rd \usage sections`, `checking Rd contents` and `checking files in 'vignettes'` all OK, so T3's and T2's changes are clean at the check surface); `pkgdown::check_pkgdown()` no problems; `devtools::test()` FAIL 0, PASS 13266.
- 2026-09-09: T7 — `NEWS.md` gained three Documentation entries for this milestone's user-visible changes (the citation, the vignette's corrected return-value sentence and its tempdir chunks, and the reference page dropping the internal generic), per the profile's changelog slot; `spelling::spell_check_package()` clean.
- 2026-09-09: status → review.
