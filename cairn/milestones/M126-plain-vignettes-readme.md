# M126: The vignettes and README read as plain English for an R user

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — the vignettes, README and pkgdown index are what users read first
- **Branch/PR:** `m126-plain-vignettes-readme`

## Goal

The README, the five vignettes and the pkgdown reference index use plain English for an R user who does not know FFmpeg.

## Scope

**In:** the prose of `README.Rmd`, `vignettes/tidymedia.Rmd`, `workflow.Rmd`, `batch.Rmd`, `metadata.Rmd` and `verification.Rmd`. The `title:` and `desc:` text in `_pkgdown.yml`. A glossary section in `vignette("tidymedia")`. The prose sweep `tools/doc_prose_report.R`. This work also takes items (g) and (h) from the shipped-docs candidate row. Item (g) is the four pipeline functions that the tour leaves out. Item (h) is the capability functions that only the README shows. The rules, terms and sweep are defined in `cairn/references/plain-docs.md`.

**Out:** roxygen help pages go to M127, M128, M129 and M130. Comments inside code chunks and hidden setup chunks are for maintainers and stay as they are. `NEWS.md` gets no entry, because no caller-visible behavior changes (D091). The other items on the shipped-docs candidate row stay there. CRAN release steps stay on the CRAN readiness candidate row.

## Acceptance criteria

- [x] AC1: The prose sweep over `README.Rmd` and the five vignettes prints no sentence that matches a maintainer term. Over `_pkgdown.yml`, `grep -n -E` of the first maintainer-term pattern and `grep -n -i -E` of each other pattern find no match.
- [x] AC2: The prose sweep over `README.Rmd` and the five vignettes prints no sentence over 25 words.
- [x] AC3: `vignette("tidymedia")` has a glossary section that defines each of the 13 glossary terms in at most two sentences. For each of the six files, take each glossary stem found in its `--prose` output. The first sentence that uses the stem defines the term or links to the glossary. One ledger row per file and stem records the result.
- [x] AC4: Each `##` or `###` heading in the six files at the base commit has a ledger row. The row says where the content went: kept, moved (with the file and section), or deleted (with a reason).
- [x] AC5: Take every name from `getNamespaceExports("tidymedia")` that a search for `\bname\(` finds in the six files at the base commit. Add every match of `\btidymedia[._][a-z_.]+` or `\btm_[a-z_]+` there. Each one is still found in the six files or in `man/*.Rd` at head, or has a ledger row that says why it was dropped.
- [x] AC6: The pipeline tour in `vignette("tidymedia")` calls `ffm_fps()`, `ffm_drawbox()`, `ffm_loudnorm()` and `ffm_output_options()` in code chunks. A vignette calls `ffmpeg_codecs()`, `ffmpeg_encoders()` and `hardware_encoder()` in a code chunk.
- [x] AC7: With FFmpeg, FFprobe and MediaInfo present, `devtools::build_readme()` and `devtools::build_vignettes()` succeed. `Rscript tools/build_vignettes_without_binaries.R both` exits 0. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures.

## Coverage

- AC1 → T1, T3, T4, T5, T6, T7
- AC2 → T1, T3, T4, T5, T6, T7
- AC3 → T3, T4, T5, T6, T7
- AC4 → T2, T7
- AC5 → T2, T7
- AC6 → T3, T5
- AC7 → T7

## Tasks

- [x] T1: Write `tools/doc_prose_report.R` to the definition in `cairn/references/plain-docs.md`. Before trusting a clean result, plant a maintainer term and a 26-word sentence in a temporary `.Rmd` and `.Rd`. Put the term in body text, in an argument or list item, and across a line wrap. Put a ` -- ` aside in the `.Rd` source. See each one reported. Show that the sweep reads a non-empty set of sentences from each real file.
- [x] T2: Record the base-commit inventory in a new M126 ledger section: the `##` and `###` headings of the six files, and the names and identifiers that AC5 lists.
- [x] T3: Rewrite `vignettes/tidymedia.Rmd`. Add the glossary. Replace the layer and escape-hatch wording with the names in rule 5. Add the four pipeline functions to the tour. Write each new claim from a run's output.
- [x] T4: Rewrite `vignettes/verification.Rmd`. Put the basic check first. Move the timeout measurements and ordering detail out, and record where they went.
- [x] T5: Rewrite `vignettes/workflow.Rmd`, `batch.Rmd` and `metadata.Rmd`. Add a guarded chunk that calls the three capability functions, in the hardware section of `workflow.Rmd`.
- [x] T6: Rewrite the prose of `README.Rmd` and the `title:` and `desc:` text in `_pkgdown.yml`, including "three layers" at `_pkgdown.yml:9`. Run `devtools::build_readme()`.
- [x] T7: Fill the ledger rows for AC3, AC4 and AC5. Run the sweep, both builds, `tools/vignette_chunk_guards.R` and `tools/vignette_chunk_program_identity.R`. Run `devtools::check()`, and run `devtools::test()` with no other R session working.

## Work log

- 2026-09-13: created by /milestone-plan (series M126-M130).
- 2026-09-13: criteria audit (full mode, fresh [O] reader) returned 16 findings on the draft. The 11 clear ones were fixed before the gate: whole-sentence term matching, Rd dash forms, dropped family lists, planted checks moved to T1, `both` for the no-binaries build, anchored domains and counts, whole-word names and `tm_` fields, `###` headings, and two terms dropped from the list. Judgment items went to the gate.
- 2026-09-13: plan gate chose a 25-word sentence cap over a 35-word cap or no cap because it matches the user's plain-English rules; falsified by a page where a 25-word cap forces a false or unclear statement.
- 2026-09-13: plan gate chose to keep user facts (error classes, options) in short end sections and move history and timings out, over an "Advanced details" section on each page or a reference vignette; falsified by a user report of a fact they needed that the docs no longer give.
- 2026-09-13: plan gate chose glossary links from help pages over defining terms on each page, at the user's choice against the recommendation; falsified by a help page that cannot be understood without opening the vignette.
- 2026-09-13: checkpoint commit. The re-audit of the revised criteria by the same fresh reader is still running.
- 2026-09-13: re-audit (full mode) returned 8 findings, all applied: argument names left out of `--prose`, an Rd-source dash scan, `@family` labels renamed (M127 T6) with list headers swept, tag lines not paragraphs (M129 AC5), case-insensitive `_pkgdown.yml` grep, `\bname\(` search, tests named in DECISIONS.md rewritten never removed, and a repeats re-check in M130 AC5.
- 2026-09-13: implement started on `m126-plain-vignettes-readme`. Question gate: the glossary is one `## Glossary` section with a bulleted list and one anchor (recommended). The README links to it by full website address (recommended). The README install section keeps one install route and one check per platform and leaves the rest to the help pages. The user chose this over the recommended keep-all rewrite.
- 2026-09-13: T1 done. In a scratch `.Rmd` and `.Rd`, `tools/doc_prose_report.R` reported each planted term (body text, list item, argument item, across a line wrap), each 26-word sentence and the ` -- ` in the `.Rd` source. It stayed silent on terms in a chunk, an HTML comment, the YAML title, an `.Rd` family list, `\usage` and `\examples`, and on a 25-word sentence. At the base commit it reads 116, 54, 62, 36, 56 and 75 sentences from the six files and reports 135 lines. The plant run found two parse bugs, now fixed: abbreviations split sentences, and `**` stayed in the text.
- 2026-09-13: T2 done. The M126 ledger in `cairn/references/plain-docs.md` lists 39 headings, 66 exports and 4 identifiers from `d5c53674`.
- 2026-09-13: T3 done. `vignettes/tidymedia.Rmd` has a Glossary section, the rule 5 names and a "More pipeline steps" section with the four missing pipeline functions. The sweep is clean on it and it renders. Each new command output was read from a run first. One draft claim was wrong and was fixed before commit: one-input pipeline functions do not keep every audio track, but task functions such as `crop_video()` do.
- 2026-09-13: T4 done. `vignettes/verification.Rmd` puts the basic `verify_media()` call first and the check rules in a "What the checks cover" section after it. The timeout signal schedule and the 42.0 s measurement left the page. Both stay in the code comment at `R/timeout.R:5-10`, and the schedule is also in `?with_timeout` Details. The per-function list of errors and warnings now points to `?tidymedia`, which holds it. The sweep is clean and the page renders.
- 2026-09-13: T5 done. `workflow.Rmd`, `batch.Rmd` and `metadata.Rmd` are rewritten. The hardware part of `workflow.Rmd` is now a "Using video hardware" section with a chunk guarded on `has_ffmpeg`. That chunk calls `ffmpeg_codecs()`, `ffmpeg_encoders()`, `hardware_encoder()` and `has_hardware_encoder()`, and its output was read from a render. The sweep is clean on all three, and all three render.
- 2026-09-13: T6 done. `README.Rmd` prose is rewritten, and the install section now has one install line for each platform and one `program_status()` check, per the gate. The macOS line `brew install ffmpeg media-info` was checked against this machine's Homebrew list. `_pkgdown.yml` titles now use the rule 5 names, and "three layers" is gone. The AC1 greps find nothing, `devtools::build_readme()` ran, and `pkgdown::check_pkgdown()` finds no problems.
- 2026-09-13: T7 done. The ledger in `cairn/references/plain-docs.md` has the AC3 stem rows (26), the AC4 heading results (39), and the AC5 result: all 70 names are still in the six files, so no dropped rows. The spelling test flagged MKV and WAV from the new glossary, so both were added to `inst/WORDLIST`. On the final tree the sweep exits 0, both builds and the no-binaries build succeed, and both chunk scripts report none. `devtools::check()` gives 0 errors, 0 warnings and 0 notes, and `devtools::test()` gives FAIL 0, PASS 13858.
- 2026-09-13: claim audit: 95 claims read, 6 corrected — vignettes/tidymedia.Rmd, vignettes/metadata.Rmd, vignettes/verification.Rmd, vignettes/workflow.Rmd, tools/doc_prose_report.R
- 2026-09-13: The claim corrections were: `ffm_concat()` keeps audio, frame-rate fractions stay strings under `typed = TRUE`, `get_*()` has no `file` column, and `verified` can be `NA`. Two were narrowed: `format_for_web()` no longer says "small", and `concatenate_videos()` parts also need the same picture size and frame rate. The single re-read by the same reader confirmed five and refined the `verified` wording, which was applied. Two of the four wrong claims came from the base text.
- 2026-09-13: status set to review.

## Decisions

## Review

Evidence gathered 2026-09-13 on `m126-plain-vignettes-readme` at `c2b555a2`. The branch contains `origin/master` (`d5c53674`), so no sync merge was needed.

- AC1: `Rscript tools/doc_prose_report.R` over the six files exits 0 and prints no sentence. In the order above, it reads 58, 115, 101, 51, 70 and 93 sentences. A scratch `.Rmd` with "Layer 1" and a 26-word sentence was reported with exit 1. A 25-word sentence in it stayed silent. Over `_pkgdown.yml`, each of the ten greps returns 1 with no match. The same greps match "Layer 0/1/2" at `d5c53674`, so they can find a match.
- AC2: The same sweep run as AC1 prints no sentence over 25 words and exits 0. The scratch 26-word sentence was reported as `[26 words]`. At `d5c53674`, the sweep prints 35 lines for `README.Rmd` and `tidymedia.Rmd` alone.
- AC3: `## Glossary` in `vignettes/tidymedia.Rmd` has 13 entries, one per glossary term. The `--prose` output gives each entry one or two sentences. For each file, the first `--prose` sentence with each stem was found by `grep -i -E -m1`. The source line of each was read. All 26 match the ledger rows in `cairn/references/plain-docs.md` by file, stem and line. Each one links to `#glossary` or is the glossary entry. `batch.Rmd` uses no stem.
- AC4: An `awk` pass lists the `##` and `###` headings outside code chunks in the six files at `d5c53674`. It finds 39. Each has a matching `| file | heading |` row in the M126 ledger, and none is missing. Every "Kept, as" name is a heading at head. The moved timeout schedule and the 42.0 s measurement were read at `R/timeout.R:5-10`. The schedule was read in `man/with_timeout.Rd` Details. The per-function error list was read in the "Bounding a run that hangs" section of `man/tidymedia-package.Rd`.
- AC5: `getNamespaceExports("tidymedia")` gives 90 names, the same count as `export(` lines in the base `NAMESPACE`. A `\bname\(` search of the six files at `d5c53674` matches 66 of them. The two identifier patterns add 4. At head, a `\bname\b` search of the six files finds all 70, so no name falls back to `man/*.Rd` and no dropped row is needed. The 66 exports are still called as `name(` at head. The 4 condition and option names are named without a call.
- AC6: An `awk` pass over lines inside `{r` chunks finds `ffm_fps(`, `ffm_drawbox(`, `ffm_loudnorm(` and `ffm_output_options(` in `vignettes/tidymedia.Rmd` at lines 148, 149, 160 and 170. It finds `ffmpeg_codecs(`, `ffmpeg_encoders(` and `hardware_encoder(` in `vignettes/workflow.Rmd` at lines 106, 108, 111 and 112. That chunk is `eval = has_ffmpeg`.
- AC7: FFmpeg 9.0.1, FFprobe and MediaInfo were found in `/opt/homebrew/bin`. `devtools::build_readme()` and `devtools::build_vignettes()` each exit 0, and `README.md` has no diff after the build. `Rscript tools/build_vignettes_without_binaries.R both` exits 0 and reports no error or warning lines. `devtools::check()` reports 0 errors, 0 warnings and 0 notes in 7m 43.1s. `devtools::test()`, run alone after it, reports FAIL 0, WARN 12, SKIP 5, PASS 13858.

Consistency gate:

- `cairn_validate.py` exits 0 with all checks passed. No principle changed, so `cairn_impact.py` was skipped.
- `devtools::document()` exits 0 with no diff. It did not regenerate, because the installed roxygen2 is 8.0.0 and the package uses 8.1.0. The branch changes no roxygen comment, so no `man/` drift can come from it.
- `README.md` is in sync with `README.Rmd` after `build_readme()`.
- `pkgdown::check_pkgdown()` finds no problems.
- `NEWS.md` has no entry, as Scope Out and D091 state.
- The branch adds no top-level file. `tools/` is covered by `^tools$` in `.Rbuildignore`.
- `tools/vignette_chunk_guards.R` and `tools/vignette_chunk_program_identity.R` each report none.

Independent review (three fresh reviewers):

- [S] history: no finding. It traced the three largest deletions: the install digest note, the 42.0 s timing and the fractional-limit note. Each destination carries the content.
- [S] prior reviews: no finding. The archived reviews for M114, M115, M119, M123 and M124 were checked. GitHub has no PR review comments.
- [O] diff: 13 findings, ranked below. O1 to O3 were confirmed against the files.
  - O1: `README.Rmd:91-94` lost the `brew --prefix ffmpeg` step for a `NA` location, and no help page has it.
  - O2: `README.Rmd:83-86` says to unzip MediaInfo but no longer names a folder. The next line assumes `C:/Program Files/MediaInfo`.
  - O3: `verification.Rmd:273-281` implies the condition classes cover the metadata readers' `NA`-row warning. `warn_unreadable()` gives that warning with no class. The base text had the same gap.
  - O4: the sweep needs a capital letter after a period, so a sentence that starts with "tidymedia" joins the one before it (`tidymedia.Rmd:165`, `verification.Rmd:158`).
  - O5: in an Rd Arguments section, a wrapped line with `": "` and a short indent counts as a new argument, and the text before the colon is dropped.
  - O6: Rd code spans are found only as curly quotes, so a non-UTF-8 locale gives other results.
  - O7: exit codes overlap. A usage error and findings both exit 1, and exit 2 for an empty file drops reports already found.
  - O8: in `.Rmd` prose, ` -- ` and `---` are not flagged. None is in the six files.
  - O9: parse edge cases: a four-backtick fence, a `|` inside a code span, and prose between `<` and `>`. None is in the six files.
  - O10: some lines are over 80 characters, and "A field name that you give by name" repeats itself.
  - O11: the `NA` fix-up sentence does not name a setter for the `ffplay` row.
  - O12: "nearest keyframe" means the keyframe at or before the time, which `?ffm_seek` also says loosely.
  - O13: the page no longer says `ffm_manifest(res, path =)` returns invisibly.
