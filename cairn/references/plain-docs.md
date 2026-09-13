# Plain-English documentation rules and ledger (M126-M130)

**Provenance.** Ingested 2026-09-13 by the M126-M130 plan, from the files in `vignettes/`, `R/` and `man/`. The inputs were three read-only surveys of `README.Rmd`, `vignettes/*.Rmd` and the `R/` roxygen blocks, and a criteria audit. All read commit `d7fa7058`.
Pagination: —.
Extraction: first-hand record, nothing to re-verify against — observed 2026-09-13.

M126 to M130 read this page. The rules below define the terms their criteria use. Each milestone adds its own ledger section at the end.

## Reader

The reader knows R, data frames and the pipe. The reader does not know FFmpeg, and does not know how the package is built inside.

## Rules

1. A sentence has at most 25 words, as the prose sweep counts them.
2. Use active voice and simple tenses. Name the actor.
3. Put the basic task first. Put error classes, option names and limits in a short section after it, or on the page that owns them.
4. History, measured timings, internal ordering and test notes do not go in user docs. Move them to a code comment, or delete them.
5. Use these names for the three kinds of function: "task functions" (for example `extract_audio()`), "pipeline functions" (the `ffm_*()` functions), and "direct commands" (`ffmpeg()`, `ffprobe()`, `mediainfo()`).

## Maintainer terms

The user prose must not match any of these regular expressions. The match runs on each sentence after runs of white space become one space. Only the first pattern is case-sensitive.

- `\b(D|M|RR)[0-9]{2,3}\b` (decision, milestone and review ids)
- `\bLayer [012]\b`, `escape hatch`, `front door`
- `\bseams?\b`, `\bsentinel`, `\bmemo\b`, `\bspawn`, `best-effort`
- the em-dash character, and in `.Rd` source outside `\usage` and `\examples`, the strings ` -- ` and `---`

## Glossary terms

`vignette("tidymedia")` defines these 13 terms: codec, container, stream, encoder, re-encode, stream copy, pixel format, keyframe, frame rate, sample rate, LUFS, true peak, hardware encoder.

The criteria find a term by its stem, matched without regard to case on the `--prose` output: `codec`, `container`, `stream`, `encod`, `pixel format`, `key ?frame`, `frame rate`, `sampl(e|ing) rate`, `LUFS`, `true peak`.

## Prose sweep

The prose sweep is `Rscript tools/doc_prose_report.R <files>`. M126 writes it.

- For an `.Rmd` file, it drops the YAML header, the code chunks and the HTML comments.
- For an `.Rd` file, it renders the file with `tools::Rd2txt()`. It drops the Usage and Examples sections and the "Other ... functions:" lists under See Also.
- A code span counts as one word. The `--prose` output removes code spans.
- A heading, an argument item, a list item, a table cell and the end of a paragraph each end a sentence.
- It prints each sentence over 25 words and each sentence that matches a maintainer term. If it prints a sentence, it exits with status 1.
- With `--prose`, it prints the swept prose, one sentence per line.

## Domains

Each help-page domain is the set of files that `grep -l -E "^% Please edit documentation in R/(<files>)\.R$" man/*.Rd` returns.

- M127: `program_management|mediainfo|tidymedia-package|timeout|ffprobe|audio-stream-doc|ffm_batch|ffm_jobs|cache|verify|ffm_manifest|utils-tidy-eval|ffm_oop` (28 files on 2026-09-13).
- M128: `ffm` (20 files on 2026-09-13).
- M129: `ffmpeg`, then only base names matching `audio|frame|^ffmpeg|hardware` (16 files on 2026-09-13).
- M130: `ffmpeg` (34 files on 2026-09-13, which include M129's 16).

## Ledger

<!-- One section per milestone, added by that milestone's implement phase. -->
