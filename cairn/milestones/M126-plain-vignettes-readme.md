# M126: The vignettes and README read as plain English for an R user

- **Status:** in-progress
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

- [ ] AC1: The prose sweep over `README.Rmd` and the five vignettes prints no sentence that matches a maintainer term. Over `_pkgdown.yml`, `grep -n -E` of the first maintainer-term pattern and `grep -n -i -E` of each other pattern find no match.
- [ ] AC2: The prose sweep over `README.Rmd` and the five vignettes prints no sentence over 25 words.
- [ ] AC3: `vignette("tidymedia")` has a glossary section that defines each of the 13 glossary terms in at most two sentences. For each of the six files, take each glossary stem found in its `--prose` output. The first sentence that uses the stem defines the term or links to the glossary. One ledger row per file and stem records the result.
- [ ] AC4: Each `##` or `###` heading in the six files at the base commit has a ledger row. The row says where the content went: kept, moved (with the file and section), or deleted (with a reason).
- [ ] AC5: Take every name from `getNamespaceExports("tidymedia")` that a search for `\bname\(` finds in the six files at the base commit. Add every match of `\btidymedia[._][a-z_.]+` or `\btm_[a-z_]+` there. Each one is still found in the six files or in `man/*.Rd` at head, or has a ledger row that says why it was dropped.
- [ ] AC6: The pipeline tour in `vignette("tidymedia")` calls `ffm_fps()`, `ffm_drawbox()`, `ffm_loudnorm()` and `ffm_output_options()` in code chunks. A vignette calls `ffmpeg_codecs()`, `ffmpeg_encoders()` and `hardware_encoder()` in a code chunk.
- [ ] AC7: With FFmpeg, FFprobe and MediaInfo present, `devtools::build_readme()` and `devtools::build_vignettes()` succeed. `Rscript tools/build_vignettes_without_binaries.R both` exits 0. `devtools::check()` reports 0 errors, 0 warnings and 0 notes. `devtools::test()` reports 0 failures.

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
- [ ] T2: Record the base-commit inventory in a new M126 ledger section: the `##` and `###` headings of the six files, and the names and identifiers that AC5 lists.
- [ ] T3: Rewrite `vignettes/tidymedia.Rmd`. Add the glossary. Replace the layer and escape-hatch wording with the names in rule 5. Add the four pipeline functions to the tour. Write each new claim from a run's output.
- [ ] T4: Rewrite `vignettes/verification.Rmd`. Put the basic check first. Move the timeout measurements and ordering detail out, and record where they went.
- [ ] T5: Rewrite `vignettes/workflow.Rmd`, `batch.Rmd` and `metadata.Rmd`. Add a guarded chunk that calls the three capability functions, in the hardware section of `workflow.Rmd`.
- [ ] T6: Rewrite the prose of `README.Rmd` and the `title:` and `desc:` text in `_pkgdown.yml`, including "three layers" at `_pkgdown.yml:9`. Run `devtools::build_readme()`.
- [ ] T7: Fill the ledger rows for AC3, AC4 and AC5. Run the sweep, both builds, `tools/vignette_chunk_guards.R` and `tools/vignette_chunk_program_identity.R`. Run `devtools::check()`, and run `devtools::test()` with no other R session working.

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

## Decisions

## Review
