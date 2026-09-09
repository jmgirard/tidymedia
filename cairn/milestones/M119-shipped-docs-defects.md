# M119: The documentation defects the pre-CRAN audit measured are corrected

- **Status:** in-progress
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
- [ ] AC2: After `R CMD build` on a machine with the three binaries on `PATH`,
      `vignettes/` in the working tree holds no file it did not hold before the build,
      and the before listing contains only files tracked by git.
- [ ] AC3: `man/find_program.Rd` carries no `\usage{}` entry for `find_program`,
      `_pkgdown.yml` does not list `find_program` in its reference index, and
      `pkgdown::check_pkgdown()` passes.
- [ ] AC4: A grep over `.github/SUPPORT.md` for `with $`, ` !` and `{{` returns
      nothing, and the package references derived from the template's placeholders
      name `tidymedia`.
- [ ] AC5: `urlchecker::url_check()` over the built package reports no dead URL, and
      every redirect it reports is either followed in the source or recorded with the
      reason it is kept.
- [ ] AC6: `inst/CITATION` exists and `citation("tidymedia")` returns it rather than
      the auto-generated fallback.
- [ ] AC7: `devtools::check()` reports 0 errors and 0 warnings, and the `verify` slot
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
- [ ] T2: Give `vignettes/tidymedia.Rmd:39-41` a `root.dir = tempdir()` the way
      `verification.Rmd:33` does, remove the untracked `vignettes/audio.m4a` the
      current chunk left behind, and build to confirm nothing new lands.
- [ ] T3: Drop `find_program` from the Rd `\usage{}` and from `_pkgdown.yml:127`,
      keeping the four exported `find_*()` wrappers documented as they are.
- [ ] T4: Fill `.github/SUPPORT.md`'s placeholders (`:1` and `:3`) and point its
      support path at this package's issue tracker.
- [ ] T5: Run `urlchecker::url_check()`; resolve the redirecting Contributor Covenant
      link and settle the FFmpeg anchor family, which spells the same filters both
      `#concat` and `#toc-concat`.
- [ ] T6: Write `inst/CITATION` from DESCRIPTION's author block and ORCID.
- [ ] T7: `devtools::document()`, `devtools::check()`, `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-07: created by /milestone-plan.
- 2026-09-07: plan-gate criteria audit ran in FULL mode (declared tier user-facing), two rounds, fresh-context [O] reader. Findings against this milestone: the round-1 title quantified over all vignettes, all of `man/`, README and pkgdown while AC1 bound one sentence (repaired — title narrowed to what the audit measured); AC3's "`find_program` is not exported" conjunct was already true at head and could not fail (repaired — dropped, and `pkgdown::check_pkgdown()` added, since removing the index row while keeping the topic can trip PROFILE's consistency gate); AC3's round-1 `_pkgdown.yml` clause was unparseable (repaired); AC4's "every sentence naming a package names tidymedia" was falsified by the file's own reprex and tidyverse sentences and named no procedure (repaired — narrowed to the placeholder-derived references); AC5's dead-URL clause gained a disposition arm. AC1, AC2, AC5 and AC6 passed all six questions clean.
- 2026-09-07: plan gate chose unexporting `find_program` from its docs over exporting the function, because GP1 prefers refusing surface to growing it and D014 makes a new permanent export a decision-level act rather than a documentation repair; falsified by a report of a caller needing the generic that the four wrappers cannot serve.
- 2026-09-09: T1 — measured the chunk rather than guessing: `withVisible(extract_audio(video, "audio.m4a"))` returns the compiled command with `visible = FALSE`, and a knit of the vignette renders that chunk with no output block at all (FFmpeg's stderr does not reach the document). The prose was the wrong side; corrected to say the verb returns the command it ran, invisibly.
