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

The base commit is `d5c53674`. The six files are `README.Rmd` and `vignettes/{tidymedia,workflow,batch,metadata,verification}.Rmd`. Chunks are left out of the heading list. The Result column was filled at T7. "Kept" means the heading and its content are still in the same file, under the name given.

#### Headings at the base commit (AC4)

| File | Heading | Result |
|---|---|---|
| README.Rmd | `## Installation` | Kept. |
| README.Rmd | `### Dependencies` | Kept. Content trimmed at the implement gate: one install line for each platform and one `program_status()` check. Deleted: the Windows and macOS download steps for MediaInfo, the evermeet.cx manual FFmpeg route, the `brew --prefix` recovery step, and the `install_on_win()` digest details. Reason: the user chose a short install section, and `?set_program` and `?install_on_win` carry the details. |
| README.Rmd | `## Examples` | Kept. |
| README.Rmd | `### Build reproducible FFmpeg commands` | Kept. |
| README.Rmd | `### Process a folder in batch` | Kept. |
| README.Rmd | `### Read metadata as tibbles` | Kept. |
| README.Rmd | `### Query FFmpeg's capabilities` | Kept. |
| README.Rmd | `## Code of Conduct` | Kept. |
| tidymedia.Rmd | `## Start with a task verb` | Kept, as `## Start with a task function`. |
| tidymedia.Rmd | `### Choosing an audio track` | Kept. |
| tidymedia.Rmd | `## The three layers` | Kept, as `## Three kinds of function`. |
| tidymedia.Rmd | `## Building a pipeline` | Kept. New `### More pipeline steps` added under it. |
| tidymedia.Rmd | `## Copy versus re-encode` | Kept, as `## Fast cuts and exact cuts`. |
| tidymedia.Rmd | `## Combining multiple inputs` | Kept. |
| tidymedia.Rmd | `## Where to next` | Kept. |
| workflow.Rmd | `## 1. Standardize the recordings` | Kept. Its hardware paragraphs moved to a new `### Using video hardware` under it. |
| workflow.Rmd | `## 2. Prepare the audio` | Kept. |
| workflow.Rmd | `## 3. Frames for visual coding` | Kept. |
| workflow.Rmd | `## 4. De-identify before sharing` | Kept. |
| workflow.Rmd | `## 5. Assemble and share` | Kept. |
| workflow.Rmd | `## Reproducibility` | Kept. |
| workflow.Rmd | `## Where to next` | Kept. |
| batch.Rmd | `## The batch runner` | Kept. |
| batch.Rmd | `## Per-verb batch siblings` | Kept, as `## Batch task functions`. |
| batch.Rmd | `## Fan-out verbs` | Kept, as `## One input, many outputs`. |
| batch.Rmd | `## Running in parallel` | Kept. |
| batch.Rmd | `## Where to next` | Kept. |
| metadata.Rmd | `## Which reader?` | Kept. |
| metadata.Rmd | `## Probing with FFprobe` | Kept. |
| metadata.Rmd | `## Querying with MediaInfo` | Kept. |
| metadata.Rmd | `## Batching over many files` | Kept. |
| metadata.Rmd | `## Where to next` | Kept. |
| verification.Rmd | `## Check the output against what you asked for` | Kept. The check rules moved to a new `### What the checks cover` under it. |
| verification.Rmd | `### Checking every job in a batch` | Kept. |
| verification.Rmd | `## Record how the files were made` | Kept. |
| verification.Rmd | `## Bound a run that hangs` | Kept. |
| verification.Rmd | `### What the limit actually bounds` | Kept, as `### Limits of the time limit`. Moved: the signal schedule and the 42.0 s measurement, to the code comment at `R/timeout.R:5-10`. The schedule is also in `?with_timeout` Details. Deleted: the note that `local_timeout()` and the option also refuse `0.5`, because the one example shows the rule. |
| verification.Rmd | `### A reached limit is never silent` | Kept, as `### What happens when the limit is reached`. Moved: the per-function list of errors and warnings, to the "Bounding a run that hangs" section of `?tidymedia`. Deleted: the reason `verify_media()` gives an error, as design history. |
| verification.Rmd | `## Where to next` | Kept. |

#### Names and identifiers at the base commit (AC5)

These 66 exports match `\bname\(` in the six files: `anonymize_video`, `compare_videos`, `concatenate_videos`, `convert_audio`, `convert_audio_batch`, `crop_video`, `crop_video_batch`, `extract_audio`, `extract_audio_batch`, `extract_frame`, `ffm_batch`, `ffm_codec`, `ffm_compile`, `ffm_concat`, `ffm_copy`, `ffm_crop`, `ffm_drop`, `ffm_files`, `ffm_hstack`, `ffm_jobs`, `ffm_manifest`, `ffm_map`, `ffm_overlay`, `ffm_pixel_format`, `ffm_run`, `ffm_scale`, `ffm_seek`, `ffm_trim`, `ffm_vstack`, `ffmpeg`, `ffmpeg_codecs`, `ffprobe`, `format_for_web`, `get_duration`, `get_frame_rate`, `get_height`, `get_sample_rate`, `get_width`, `has_hardware_encoder`, `install_on_win`, `local_timeout`, `mediainfo`, `mediainfo_parameter`, `mediainfo_query`, `mediainfo_template`, `normalize_audio`, `normalize_audio_batch`, `picture_in_picture`, `probe_all`, `probe_audio`, `probe_container`, `probe_streams`, `probe_video`, `program_status`, `sample_frames`, `segment_video`, `separate_audio_video`, `separate_audio_video_batch`, `set_ffmpeg`, `set_ffprobe`, `set_mediainfo`, `standardize_video`, `standardize_video_batch`, `strip_metadata`, `verify_media`, `with_timeout`.

These 4 identifiers match `\btidymedia[._][a-z_.]+` or `\btm_[a-z_]+`: `tidymedia_batch_timeout`, `tidymedia_probe_timeout`, `tidymedia_timeout`, `tidymedia.timeout`.

At head, a `\bname\b` search finds all 70 of these names in the six files, so no name needs a dropped row. This was found on 2026-09-13 at T7.

#### Glossary stems (AC3)

Each row is one glossary stem found in a file's `--prose` output. Line is the line of the first sentence that uses the stem. "Links" means that sentence links the stem's word to the glossary. "Defines" means that sentence is the glossary entry itself. `batch.Rmd` uses no stem.

| File | Stem | Line | Result |
|---|---|---|---|
| README.Rmd | `codec` | 174 | Links (full website address). |
| README.Rmd | `container` | 154 | Links (full website address). |
| README.Rmd | `stream` | 154 | Links (full website address). |
| tidymedia.Rmd | `codec` | 29 | Links. |
| tidymedia.Rmd | `stream` | 29 | Links. |
| tidymedia.Rmd | `frame rate` | 143 | Links. |
| tidymedia.Rmd | `LUFS` | 153 | Links. |
| tidymedia.Rmd | `true peak` | 153 | Links. |
| tidymedia.Rmd | `encod` | 176 | Links ("re-encodes"). |
| tidymedia.Rmd | `key ?frame` | 176 | Links. |
| tidymedia.Rmd | `container` | 236 | Defines. |
| tidymedia.Rmd | `pixel format` | 249 | Defines. |
| tidymedia.Rmd | `sampl(e\|ing) rate` | 254 | Defines. |
| workflow.Rmd | `codec` | 67 | Links. |
| workflow.Rmd | `frame rate` | 67 | Links. |
| workflow.Rmd | `encod` | 67 | Links ("re-encodes"). |
| workflow.Rmd | `LUFS` | 140 | Links. |
| workflow.Rmd | `stream` | 145 | Links. |
| workflow.Rmd | `container` | 213 | Links. |
| metadata.Rmd | `container` | 31 | Links. |
| metadata.Rmd | `stream` | 31 | Links. |
| metadata.Rmd | `frame rate` | 85 | Links. |
| verification.Rmd | `codec` | 80 | Links. |
| verification.Rmd | `sampl(e\|ing) rate` | 84 | Links. |
| verification.Rmd | `stream` | 89 | Links. |
| verification.Rmd | `container` | 97 | Links. |

### M127

The base commit is `264afff4`. The domain grep returned 28 pages. At that commit, the prose sweep over the 28 pages printed 185 findings and exited with status 1. The Found column counts those findings for each page. The Result column is filled at T7.

#### Pages and what left them (AC5)

| Page | Source | Found | Result |
|---|---|---|---|
| audio_stream | R/audio-stream-doc.R | 13 | |
| ffm_batch | R/ffm_batch.R | 5 | |
| ffm_jobs | R/ffm_jobs.R | 12 | |
| ffm_manifest | R/ffm_manifest.R | 2 | |
| find_ffmpeg | R/program_management.R | 4 | |
| ffprobe | R/ffprobe.R | 3 | |
| get_sample_rate | R/mediainfo.R | 0 | |
| get_height | R/mediainfo.R | 0 | |
| get_frame_rate | R/mediainfo.R | 0 | |
| get_duration | R/mediainfo.R | 0 | |
| local_timeout | R/timeout.R | 23 | |
| install_on_win | R/program_management.R | 24 | |
| get_width | R/mediainfo.R | 0 | |
| mediainfo_template | R/mediainfo.R | 0 | |
| mediainfo_parameter | R/mediainfo.R | 1 | |
| mediainfo_query | R/mediainfo.R | 0 | |
| mediainfo | R/mediainfo.R | 3 | |
| probe_container | R/ffprobe.R | 1 | |
| probe_all | R/ffprobe.R | 4 | |
| program_status | R/program_management.R | 8 | |
| print.tidymedia_ffm | R/ffm_oop.R | 0 | |
| set_program | R/program_management.R | 3 | |
| refresh_ffmpeg_capabilities | R/cache.R | 11 | |
| verify_media | R/verify.R | 2 | |
| unset_program | R/program_management.R | 6 | |
| tidymedia-package | R/tidymedia-package.R | 47 | |
| tidyeval | R/utils-tidy-eval.R | 1 | |
| with_timeout | R/timeout.R | 12 | |

#### Identifiers at the base commit (AC5)

These 27 identifiers match `\btidymedia[._][a-z_.]+` or `\btm_[a-z_]+` in the 28 `.Rd` files: `tidymedia.check_tracks`, `tidymedia.hardware_encoders`, `tidymedia.timeout`, `tidymedia_archive_unreadable`, `tidymedia_batch_timeout`, `tidymedia_checksum_mismatch`, `tidymedia_checksum_unavailable`, `tidymedia_confirmation_unavailable`, `tidymedia_download_unavailable`, `tidymedia_dropped_audio`, `tidymedia_ffm`, `tidymedia_ffmpeg_exit`, `tidymedia_location_gone`, `tidymedia_location_unreadable`, `tidymedia_loudnorm_no_measurement`, `tidymedia_multitrack_separation`, `tidymedia_probe_timeout`, `tidymedia_program_not_extracted`, `tidymedia_program_unusable`, `tidymedia_timeout`, `tidymedia_wrong_platform`, `tm_file`, `tm_location`, `tm_program`, `tm_row_status`, `tm_rows`, `tm_status`.
