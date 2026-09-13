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
5. Use these names for the three kinds of function: "task functions" (for example `extract_audio()`), "pipeline functions" (the `ffm_*()` functions), and "direct commands" (`ffmpeg()`, `ffprobe()`, `mediainfo()`). The `@family` labels use the same names: "task functions", "pipeline functions" and "direct command functions".

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
- For an `.Rd` file, it renders the file with `tools::Rd2txt()`. It drops the Usage and Examples sections. From each "Other ... functions:" list under See Also, it keeps the header line and drops the names.
- For an `.Rd` file, it also scans the source outside `\usage` and `\examples` for ` -- ` and `---`, and prints each line that has one.
- A code span counts as one word. The `--prose` output removes code spans and the argument names at the start of each argument item.
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

### M126

The base commit is `d5c53674`. The six files are `README.Rmd` and `vignettes/{tidymedia,workflow,batch,metadata,verification}.Rmd`. Chunks are left out of the heading list. T7 fills the Result column.

#### Headings at the base commit (AC4)

| File | Heading | Result |
|---|---|---|
| README.Rmd | `## Installation` | |
| README.Rmd | `### Dependencies` | |
| README.Rmd | `## Examples` | |
| README.Rmd | `### Build reproducible FFmpeg commands` | |
| README.Rmd | `### Process a folder in batch` | |
| README.Rmd | `### Read metadata as tibbles` | |
| README.Rmd | `### Query FFmpeg's capabilities` | |
| README.Rmd | `## Code of Conduct` | |
| tidymedia.Rmd | `## Start with a task verb` | |
| tidymedia.Rmd | `### Choosing an audio track` | |
| tidymedia.Rmd | `## The three layers` | |
| tidymedia.Rmd | `## Building a pipeline` | |
| tidymedia.Rmd | `## Copy versus re-encode` | |
| tidymedia.Rmd | `## Combining multiple inputs` | |
| tidymedia.Rmd | `## Where to next` | |
| workflow.Rmd | `## 1. Standardize the recordings` | |
| workflow.Rmd | `## 2. Prepare the audio` | |
| workflow.Rmd | `## 3. Frames for visual coding` | |
| workflow.Rmd | `## 4. De-identify before sharing` | |
| workflow.Rmd | `## 5. Assemble and share` | |
| workflow.Rmd | `## Reproducibility` | |
| workflow.Rmd | `## Where to next` | |
| batch.Rmd | `## The batch runner` | |
| batch.Rmd | `## Per-verb batch siblings` | |
| batch.Rmd | `## Fan-out verbs` | |
| batch.Rmd | `## Running in parallel` | |
| batch.Rmd | `## Where to next` | |
| metadata.Rmd | `## Which reader?` | |
| metadata.Rmd | `## Probing with FFprobe` | |
| metadata.Rmd | `## Querying with MediaInfo` | |
| metadata.Rmd | `## Batching over many files` | |
| metadata.Rmd | `## Where to next` | |
| verification.Rmd | `## Check the output against what you asked for` | |
| verification.Rmd | `### Checking every job in a batch` | |
| verification.Rmd | `## Record how the files were made` | |
| verification.Rmd | `## Bound a run that hangs` | |
| verification.Rmd | `### What the limit actually bounds` | |
| verification.Rmd | `### A reached limit is never silent` | |
| verification.Rmd | `## Where to next` | |

#### Names and identifiers at the base commit (AC5)

These 66 exports match `\bname\(` in the six files: `anonymize_video`, `compare_videos`, `concatenate_videos`, `convert_audio`, `convert_audio_batch`, `crop_video`, `crop_video_batch`, `extract_audio`, `extract_audio_batch`, `extract_frame`, `ffm_batch`, `ffm_codec`, `ffm_compile`, `ffm_concat`, `ffm_copy`, `ffm_crop`, `ffm_drop`, `ffm_files`, `ffm_hstack`, `ffm_jobs`, `ffm_manifest`, `ffm_map`, `ffm_overlay`, `ffm_pixel_format`, `ffm_run`, `ffm_scale`, `ffm_seek`, `ffm_trim`, `ffm_vstack`, `ffmpeg`, `ffmpeg_codecs`, `ffprobe`, `format_for_web`, `get_duration`, `get_frame_rate`, `get_height`, `get_sample_rate`, `get_width`, `has_hardware_encoder`, `install_on_win`, `local_timeout`, `mediainfo`, `mediainfo_parameter`, `mediainfo_query`, `mediainfo_template`, `normalize_audio`, `normalize_audio_batch`, `picture_in_picture`, `probe_all`, `probe_audio`, `probe_container`, `probe_streams`, `probe_video`, `program_status`, `sample_frames`, `segment_video`, `separate_audio_video`, `separate_audio_video_batch`, `set_ffmpeg`, `set_ffprobe`, `set_mediainfo`, `standardize_video`, `standardize_video_batch`, `strip_metadata`, `verify_media`, `with_timeout`.

These 4 identifiers match `\btidymedia[._][a-z_.]+` or `\btm_[a-z_]+`: `tidymedia_batch_timeout`, `tidymedia_probe_timeout`, `tidymedia_timeout`, `tidymedia.timeout`.

T7 records here each name that is in neither the six files nor `man/*.Rd` at head, with the reason it was dropped.
