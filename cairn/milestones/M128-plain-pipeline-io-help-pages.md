# M128: The input, output and run pipeline help pages read as plain English

- **Status:** review
- **Priority:** high
- **Depends on:** M129
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — shipped help pages for exported functions
- **Branch/PR:** `m128-plain-pipeline-io-help-pages`

## Goal

The M128 help-page domain uses plain English for an R user who does not know FFmpeg.

## Scope

**In:** the roxygen text in `R/ffm.R` behind the M128 domain in `cairn/references/plain-docs.md`: `ffm_files()`, `ffm_copy()`, `ffm_seek()`, `ffm_map()`, `ffm_drop()`, `ffm_codec()`, `ffm_pixel_format()`, `ffm_output_options()`, `ffm_compile()` and `ffm_run()` (10 pages, 184 sentences on 2026-09-13). The tests that pin wording on these pages. Rewrites follow rules 1-6, so they change form, not claims (D093). Review triage follows D093.

**Out:** the filter and multi-input `ffm_*()` pages go to M131. The task function pages go to M129, M132, M133, M130 and M134. Base claims found false go to this milestone's follow-up candidate row. Code comments stay as they are. `NEWS.md` gets no entry (D091).

## Acceptance criteria

- [x] AC1: Every page on the M128 domain list exists at head, and the prose sweep over the M128 domain at head exits with status 0 or 1 and prints no `[term …]` or `[dash in Rd source]` line.
- [x] AC2: Every page on the M128 domain list exists at head, and the prose sweep over the M128 domain at head exits with status 0 or 1 and prints no `[<n> words]` line.
- [x] AC3: Each page in the M128 domain whose `--prose` output at head matches a glossary stem names the glossary.
- [x] AC4: Every match of `\btidymedia[._][a-z_.]*[a-z_]` or `\btm_[a-z_]+` in the M128 domain pages at the base commit is found by `git grep -wF` in some `man/*.Rd` file at head.
- [x] AC5: For every `man/*.Rd` file outside the M128 domain that exists at the base commit and at head and that `git diff --name-only <base> HEAD -- man/` lists, the prose sweep at head prints each finding no more times than at the base commit, and no more `[dash in Rd source]` lines. If its `--prose` output at head matches a glossary stem that its `--prose` output at the base commit did not match, the page names the glossary.
- [ ] AC6: `devtools::document()` leaves `man/` unchanged. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures. `pkgdown::check_pkgdown()` reports no problems.

## Coverage

- AC1 → T2, T3, T4
- AC2 → T2, T3, T4
- AC3 → T2, T3, T4
- AC4 → T1, T4
- AC5 → T4
- AC6 → T4

## Tasks

- [x] T1: Record in a new M128 ledger section the base commit, the domain page list, the AC4 identifiers and the sweep output over the domain at the base commit.
- [x] T2: Rewrite `ffm_files()`, `ffm_copy()`, `ffm_seek()`, `ffm_map()` and `ffm_drop()`.
- [x] T3: Rewrite `ffm_codec()`, `ffm_pixel_format()`, `ffm_output_options()`, `ffm_compile()` and `ffm_run()`. Move the exit-status detail in `?ffm_run` to a short end section.
- [x] T4: A test that pins changed wording now pins the new wording of the same property. A test whose claim left the user docs is removed, with a ledger row. A test named in a `cairn/DECISIONS.md` entry is rewritten, never removed. The claim audit reader also says, for each changed sentence, whether it makes the same claim as the base text; a sentence that adds or changes a claim about what the package does is put back to the base claim in plain words, and a sentence that names the glossary stays. A base claim found false gets a ledger row and a line in the follow-up candidate row. Fill the ledger. Run the sweep and its base comparisons, `devtools::document()`, `devtools::check()`, `devtools::test()` with no other R session working, and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-13: created by /milestone-plan (series M126-M130). The criteria audit and gate choices are logged in M126's work log.
- 2026-09-13: re-cut by /milestone-plan under D093: M128 keeps 10 of its 20 pages, and the filter and multi-input pages moved to M131. It now depends on M129 for the fixed sweep. The criteria audit and re-audit lines are in M129's work log, and cover this template.
- 2026-09-13: plan gate chose meeting the glossary criterion only by naming the glossary over defining a term on the page, because a definition is a new claim under D093; falsified by a reader who cannot follow a page without the definition on it.
- 2026-09-13: plan gate chose about 12 pages per milestone over keeping 20 pages, because M127's 28 pages took four review rounds; falsified by a 10-page milestone under D093 that still takes more than two rounds.
- 2026-09-14: implement started on branch `m128-plain-pipeline-io-help-pages` from `dc6cbd20`. The question gate was skipped, because the plan leaves no choice open.
- 2026-09-14: T1 done. The base sweep over the 10 pages read 185 sentences and printed 26 findings. The ledger is `### M128` in `cairn/references/plain-docs.md`.
- 2026-09-14: T2 done. The five pages print no sweep finding, and the four with a glossary stem name the glossary. The audio-index, ffm and D014 tests pass.
- 2026-09-14: T3 done. The sweep over the 10 pages exits 0. The exit-status detail on `?ffm_run` stays in its own section after Value, now split into two lists. The exit-condition and audio-index tests pass.
- 2026-09-14: claim audit: 125 claims read, 6 corrected — R/ffm.R, man/ffm_seek.Rd, man/ffm_map.Rd, man/ffm_output_options.Rd, man/ffm_run.Rd. The same [O] reader re-read the 6 once and found them true and the same claim as base. Four base claims found false went to a new M128 follow-up row. The one-line `unset_*()` row was merged into the declined-exports row, so `ROADMAP.md` stays under 60 lines.
- 2026-09-14: T4 checkpoint, half done: ledger filled. The full test, check and pkgdown run is not yet read.
- 2026-09-14: T4 done. `devtools::test()` 0 failures (13866 passed, 5 skipped). `devtools::check()` 0 errors, 0 warnings, 0 notes. `pkgdown::check_pkgdown()` no problems. `devtools::document()` at head writes nothing. No test pinned changed wording. Status set to review.
- 2026-09-14: review checkpoint, half done: AC1-AC5 evidence recorded and ticked, and `cairn_validate` passes. The AC6 run and the three reviewers are still running.

## Decisions

## Review

Review run 2026-09-14 at `b2ab5add`, base `dc6cbd20`. The branch contains `origin/master`, so no sync merge was needed.

- AC1: the M128 filter over `man/*.Rd` headers naming `R/ffm.R` finds all 10 domain pages at head, and no `man/*.Rd` file was added since base. `LC_ALL=en_US.UTF-8 Rscript tools/doc_prose_report.R` over the 10 pages reads 241 sentences, prints no finding and exits 0. So it prints no `[term …]` line and no `[dash in Rd source]` line.
- AC2: the same 10 pages exist, and the same run exits 0 with no `[<n> words]` line.
- AC3: the glossary stems, matched without case on each page's `--prose` output at head, hit six pages: `ffm_copy`, `ffm_seek`, `ffm_map`, `ffm_drop`, `ffm_codec` and `ffm_pixel_format`. Each of the six has a `tools::Rd2txt()` sentence with `glossary` and `vignette("tidymedia")`. `ffm_files`, `ffm_output_options`, `ffm_compile` and `ffm_run` match no stem and do not name the glossary, so the check can read false.
- AC4: the two patterns over the 10 pages at `dc6cbd20` match 7 distinct identifiers: `tidymedia_ffm`, `tidymedia_ffmpeg_exit`, `tidymedia_loudnorm_no_measurement`, `tidymedia_multitrack_separation`, `tm_row_status`, `tm_rows` and `tm_status`. `git grep -l -wF` at head finds each in 3 to 23 `man/*.Rd` files.
- AC5: `git diff --name-only dc6cbd20 HEAD -- man/` lists exactly the 10 domain pages, so no `man/*.Rd` file outside the domain is in the set the criterion quantifies over. The criterion holds with nothing to compare.
