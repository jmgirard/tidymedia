# Plain-English documentation rules and ledger (M126-M134)

**Provenance.** Ingested 2026-09-13 by the M126-M130 plan, from the files in `vignettes/`, `R/` and `man/`. The inputs were three read-only surveys of `README.Rmd`, `vignettes/*.Rmd` and the `R/` roxygen blocks, and a criteria audit. All read commit `d7fa7058`.
Pagination: —.
Extraction: first-hand record, nothing to re-verify against — observed 2026-09-13.

M126 to M134 read this page. The rules below define the terms their criteria use. Each milestone adds its own ledger section at the end. The series was re-cut on 2026-09-13 under D093 (corrected: it was M126-M130).

## Reader

The reader knows R, data frames and the pipe. The reader does not know FFmpeg, and does not know how the package is built inside.

## Rules

1. A sentence has at most 25 words, as the prose sweep counts them.
2. Use active voice and simple tenses. Name the actor.
3. Put the basic task first. Put error classes, option names and limits in a short section after it, or on the page that owns them.
4. History, measured timings, internal ordering and test notes do not go in user docs. Move them to a code comment, or delete them.
5. Use these names for the three kinds of function: "task functions" (for example `extract_audio()`), "pipeline functions" (the `ffm_*()` functions), and "direct commands" (`ffmpeg()`, `ffprobe()`, `mediainfo()`). The `@family` labels use the same names: "task functions", "pipeline functions" and "direct command functions".
6. Change form, not claims (D093, from M128 on). A rewrite changes how a sentence says a fact, not what it claims. It can split, shorten, reorder, move or delete a claim. It adds no claim about what the package does that the base text did not make. A sentence whose only claim is that `vignette("tidymedia")` has a glossary is not such a claim. A base claim found false keeps its meaning. It gets a ledger row with its evidence and goes to the milestone's follow-up candidate row.

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

### Sweep terms for M128-M134

These definitions assume the sweep as M129 leaves it.

- The prose sweep runs as `LC_ALL=en_US.UTF-8 Rscript tools/doc_prose_report.R <files>`.
- Report mode prints each finding as `<path>:<line>: [<n> words] <sentence>` or `<path>:<line>: [term <regex>] <sentence>`, with the line of the rendered text. A dash finding prints as `<path>:<line>: [dash in Rd source] <source line>`, with the source line. In `.Rmd` prose, a sentence with ` -- ` or `---` outside a code span prints as `<path>:<line>: [dash in Rmd prose] <sentence>`.
- It exits 0 when it prints no finding, 1 when it prints one, 2 when a file parses to no sentences, and 3 on a usage error or a missing file.
- A comparison with the base commit runs the head version of the sweep over each page's text at the base commit. `git show <base>:man/<page>.Rd` writes that text to a file of the same name in a temporary folder, and the outputs are paired by file name.
- Two sweep outputs are compared per page. A `[<n> words]` or `[term …]` finding counts once each time it is printed, keyed by its kind and sentence text. Line numbers are ignored. `[dash in Rd source]` lines are compared as one count per page.
- A page names the glossary when its `tools::Rd2txt()` rendering has a sentence that contains the word `glossary` and the text `vignette("tidymedia")`.

## Domains

A milestone's domain is the page list its ledger section records at the base commit, plus every `man/*.Rd` file added since that commit. The page milestones after M129 also include every `man/*.Rd` file that M129 added. A page list is made with `grep -l -E "^% Please edit documentation in R/<file>\.R$" man/*.Rd`, kept only where the base name matches the pattern.

- M127: `program_management|mediainfo|tidymedia-package|timeout|ffprobe|audio-stream-doc|ffm_batch|ffm_jobs|cache|verify|ffm_manifest|utils-tidy-eval|ffm_oop`, no name filter (28 files on 2026-09-13).
- M129: `ffmpeg`, no name filter (34 files on 2026-09-13).
- M128: `ffm`, names `^ffm_(files|copy|seek|map|drop|codec|pixel_format|output_options|compile|run)$` (10 files on 2026-09-13).
- M131: `ffm`, names `^ffm_(trim|crop|scale|fps|drawbox|loudnorm|hstack|vstack|overlay|concat)$` (10 files on 2026-09-13).
- M132: `ffmpeg`, names `^(ffmpeg|ffmpeg_codecs|ffmpeg_encoders|hardware_encoder|extract_frame|sample_frames|extract_audio|convert_audio)(_batch)?$` (12 files on 2026-09-13).
- M133: `ffmpeg`, names `^(separate_audio_video|normalize_audio)(_batch)?$` (4 files on 2026-09-13).
- M130: `ffmpeg`, names `^(crop_video|format_for_web|standardize_video|strip_metadata)(_batch)?$` (8 files on 2026-09-13).
- M134: `ffmpeg`, names `^(anonymize_video|segment_video|concatenate_videos|compare_videos|picture_in_picture)(_batch)?$` (10 files on 2026-09-13).

The domains were re-cut on 2026-09-13 under D093 (corrected: M128 was all 20 `ffm` files, M129 was 16 files, and M130 was all 34 `ffmpeg` files).

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

#### Pages and what left them (author's account)

The Result column is the author's account of what left each page. It is not a complete list of removed text, and AC5 does not check it (corrected M127 amendment return: AC5 now checks only the identifiers below).

| Page | Source | Found | Result |
|---|---|---|---|
| audio_stream | R/audio-stream-doc.R | 13 | Deleted: how the every-track map is written, because it is internal. The outcome stays: no audio in gives no audio out. The generated `audio_stream` and `audio_input` argument text is rewritten, and it lands on 25 task function pages (M129 and M130 domains). Nothing else left. |
| ffm_batch | R/ffm_batch.R | 5 | Nothing left. Gained a link to `?with_timeout` for job time limits. |
| ffm_jobs | R/ffm_jobs.R | 12 | Moved: the decision id behind `type` having no default, to the comment on `rlang::check_required(type)` in `R/ffm_jobs.R`. Deleted: "hand-rolled" and "deliberately", which are tone and not facts. The 70-word column sentence became a list of three items that keeps every fact. |
| ffm_manifest | R/ffm_manifest.R | 2 | Nothing left. |
| find_ffmpeg | R/program_management.R | 4 | Nothing left. Received the section "Locations saved by earlier versions" from `?program_status`, `?set_program` and `?unset_program`. |
| ffprobe | R/ffprobe.R | 3 | Nothing left. |
| get_sample_rate | R/mediainfo.R | 0 | Deleted: "quickly", as filler. |
| get_height | R/mediainfo.R | 0 | Deleted: "quickly", as filler. |
| get_frame_rate | R/mediainfo.R | 0 | Deleted: "quickly", as filler. |
| get_duration | R/mediainfo.R | 0 | Deleted: "quickly", as filler. |
| local_timeout | R/timeout.R | 23 | Moved: the 42.0 s timing, to the comment at the top of `R/timeout.R`. The 40 s bound links to `?with_timeout`. Moved: the withr measurement dates for the `local_timeout()` inside `with_timeout()` case, to the comment above `withr::defer()` in `local_timeout()`. The `on.exit()` dates were already in that comment. Deleted: that the undo step is set before the limit, and why `NULL` is refused. Reason: internal order, and the comments in the two function bodies keep both. |
| install_on_win | R/program_management.R | 24 | Moved: the class list in the Value section, to a new "Errors" section with one item for each class. Deleted: the cleanup rules repeated in the Value section, because the new section "What a failed install leaves behind" has them. |
| get_width | R/mediainfo.R | 0 | Deleted: "quickly", as filler. |
| mediainfo_template | R/mediainfo.R | 0 | Nothing left. |
| mediainfo_parameter | R/mediainfo.R | 1 | Nothing left. |
| mediainfo_query | R/mediainfo.R | 0 | Nothing left. |
| mediainfo | R/mediainfo.R | 3 | Nothing left. |
| probe_container | R/ffprobe.R | 1 | Nothing left. |
| probe_all | R/ffprobe.R | 4 | Nothing left. |
| program_status | R/program_management.R | 8 | Moved: the folder that versions before 0.2.0 used, and which file an unreadable-location warning names, to the section "Locations saved by earlier versions" of `?find_ffmpeg`. Users of 0.1.0 find it there. |
| print.tidymedia_ffm | R/ffm_oop.R | 0 | Nothing left. |
| set_program | R/program_management.R | 3 | Moved: that the old folder is no longer read once the new file exists, to the section "Locations saved by earlier versions" of `?find_ffmpeg`. |
| refresh_ffmpeg_capabilities | R/cache.R | 11 | Nothing left. The parallel-worker text is now a section "Parallel workers". |
| verify_media | R/verify.R | 2 | Nothing left. |
| unset_program | R/program_management.R | 6 | Moved: the old-folder text, to the section "Locations saved by earlier versions" of `?find_ffmpeg`. Moved: the decision id behind `program` having no default, to the comment above `unset_program <- function(program)`. |
| tidymedia-package | R/tidymedia-package.R | 47 | Moved: the section "Bounding a run that hangs", to `?with_timeout` (Details and two sections). Moved: the FFmpeg 6.1.1 and 9.0.1 timings, to the comment at the top of `R/timeout.R`. Deleted: the note that a test derives the lists from the call graph (a test note, kept in `NEWS.md` and `helper-timeout-sweep.R`). Moved: which checks come before the limit, in short, to `?with_timeout` Details. This was first recorded as deleted (corrected M127 review return 1). The `output` column example was dropped, because at head that column is checked before the limit. Deleted: what the FFprobe readers return (tibbles for the container and streams) and what the MediaInfo readers return (one value). `?probe_all` and `?get_duration` say this. Deleted: that the full function list is on the reference index, which `_pkgdown.yml` builds. Reworded: the three-layer description is now the task, pipeline and direct command functions, without layer numbers. Deleted: the full order of checks (internal order, kept in the comment above `resolve_timeout()`). Moved: the two calls that read no limit, to `?with_timeout` Details as examples of a function that starts no program (corrected M127 review round 4). Deleted from Session options: the cost timing details (on the task function pages), that the encoder answer is kept for the session (on `?refresh_ffmpeg_capabilities`), and the withr sentence (general R). Kept in Session options: when the track check runs, and that a bad value gives an error naming the option. That refusal was first dropped with no row (corrected M127 review return 1). The error-class section keeps one item for each class. Deleted: the zero-exit case with no fields and the batch warning with no exit status (on `?normalize_audio_batch` and `?separate_audio_video_batch`). |
| tidyeval | R/utils-tidy-eval.R | 1 | Nothing left. |
| with_timeout | R/timeout.R | 12 | Moved: the 42.0 s timing, to the comment at the top of `R/timeout.R`. Received the reached-limit detail from `?tidymedia`, and the check-order paragraph (see the `tidymedia-package` row). |

#### Identifiers at the base commit (AC5)

These 27 identifiers match `\btidymedia[._][a-z_.]+` or `\btm_[a-z_]+` in the 28 `.Rd` files: `tidymedia.check_tracks`, `tidymedia.hardware_encoders`, `tidymedia.timeout`, `tidymedia_archive_unreadable`, `tidymedia_batch_timeout`, `tidymedia_checksum_mismatch`, `tidymedia_checksum_unavailable`, `tidymedia_confirmation_unavailable`, `tidymedia_download_unavailable`, `tidymedia_dropped_audio`, `tidymedia_ffm`, `tidymedia_ffmpeg_exit`, `tidymedia_location_gone`, `tidymedia_location_unreadable`, `tidymedia_loudnorm_no_measurement`, `tidymedia_multitrack_separation`, `tidymedia_probe_timeout`, `tidymedia_program_not_extracted`, `tidymedia_program_unusable`, `tidymedia_timeout`, `tidymedia_wrong_platform`, `tm_file`, `tm_location`, `tm_program`, `tm_row_status`, `tm_rows`, `tm_status`.

At head, a whole-word search over `man/*.Rd` finds all 27 identifiers. This was found on 2026-09-13 at T7, and again with `git grep -wF` in review round 3 (corrected M127 amendment return: identifiers have no ledger rows).

#### Glossary stems (AC3)

Each row is one page whose `--prose` output has a glossary stem. "Names the glossary" means the page has a sentence that names the glossary in `vignette("tidymedia")`. The other 19 pages use no stem (corrected M127 review return 2: `with_timeout` gained a stem in round 2).

| Page | Stems | Result |
|---|---|---|
| audio_stream | `codec`, `container`, `encod`, `stream` | Names the glossary. |
| get_frame_rate | `frame rate` | Names the glossary. |
| get_sample_rate | `sampl(e\|ing) rate` | Names the glossary. |
| probe_all | `container`, `stream` | Names the glossary. |
| probe_container | `container`, `stream` | Names the glossary. |
| refresh_ffmpeg_capabilities | `encod` | Names the glossary. |
| tidymedia-package | `encod` | Names the glossary. |
| with_timeout | `encod` | Names the glossary. |
| verify_media | `codec`, `container`, `key ?frame`, `sampl(e\|ing) rate`, `stream` | Names the glossary. |

#### Tests (T7)

No test was removed. These tests now pin new wording of the same property. `test-package-topic.R` checks that the `tidymedia_ffmpeg_exit` item says "exited non-zero" before it names the `loudnorm` pass. `test-check-tracks-docs.R` reads the dropped-track item on `?with_timeout`. `test-runtime-timeout.R` reads the reached-limit error and the 40 s arithmetic on `?with_timeout`, and the 42.0 s timing in `NEWS.md`. `test-timeout-silence.R` reads "never silent" on `?with_timeout`, and the call-graph note in `NEWS.md`. `helper-rd.R` now reads `?with_timeout`. `test-audio-index-docs.R` pins two rewritten phrases of the generated `audio_input` text. After review return 1, three guards changed so that they can fail. Each went red on a planted defect. The "never silent" guard pins the error to its `tidymedia_timeout` class. The M69 disclosure guard also reads `?tidymedia`. The `loudnorm` check finds no full stop between its two phrases.

### M129

The base commit is `db460d58`. At that commit, the domain is the 34 `man/*.Rd` files whose header names `R/ffmpeg.R`: `anonymize_video`, `anonymize_video_batch`, `compare_videos`, `compare_videos_batch`, `concatenate_videos`, `concatenate_videos_batch`, `convert_audio`, `convert_audio_batch`, `crop_video`, `crop_video_batch`, `extract_audio`, `extract_audio_batch`, `extract_frame`, `extract_frame_batch`, `ffmpeg`, `ffmpeg_codecs`, `ffmpeg_encoders`, `format_for_web`, `format_for_web_batch`, `hardware_encoder`, `normalize_audio`, `normalize_audio_batch`, `picture_in_picture`, `picture_in_picture_batch`, `sample_frames`, `sample_frames_batch`, `segment_video`, `segment_video_batch`, `separate_audio_video`, `separate_audio_video_batch`, `standardize_video`, `standardize_video_batch`, `strip_metadata` and `strip_metadata_batch`.

#### Sweep output at the base commit (AC2, AC3, AC5)

The T1 sweep ran as `LC_ALL=en_US.UTF-8 Rscript tools/doc_prose_report.R *.Rd` over the 34 files that `git show db460d58:man/<page>.Rd` wrote to a temporary folder. It exited 1 with 393 findings: 256 `[<n> words]`, 118 `[term …]` and 19 `[dash in Rd source]`. The same command gives the full output again. 35 finding sentences are printed for two or more pages, 132 findings in all. Identifiers are the AC4 matches in each page.

| Page | Words | Term | Dash | Identifiers (AC4) |
|---|---|---|---|---|
| anonymize_video | 9 | 1 | 1 | — |
| anonymize_video_batch | 17 | 6 | 0 | — |
| compare_videos | 9 | 2 | 0 | — |
| compare_videos_batch | 10 | 6 | 0 | — |
| concatenate_videos | 1 | 1 | 0 | — |
| concatenate_videos_batch | 3 | 3 | 0 | — |
| convert_audio | 1 | 4 | 2 | `tidymedia.check_tracks`, `tidymedia_dropped_audio` |
| convert_audio_batch | 6 | 5 | 1 | `tidymedia.check_tracks`, `tidymedia_dropped_audio` |
| crop_video | 6 | 1 | 0 | — |
| crop_video_batch | 11 | 5 | 2 | — |
| extract_audio | 2 | 2 | 2 | `tidymedia.check_tracks`, `tidymedia_dropped_audio` |
| extract_audio_batch | 7 | 4 | 1 | `tidymedia.check_tracks`, `tidymedia_dropped_audio` |
| extract_frame | 0 | 0 | 0 | — |
| extract_frame_batch | 5 | 1 | 0 | — |
| ffmpeg | 1 | 2 | 0 | — |
| ffmpeg_codecs | 0 | 2 | 0 | — |
| ffmpeg_encoders | 0 | 2 | 0 | — |
| format_for_web | 4 | 0 | 0 | — |
| format_for_web_batch | 9 | 3 | 0 | — |
| hardware_encoder | 4 | 1 | 0 | `tidymedia.hardware_encoders` |
| normalize_audio | 12 | 4 | 5 | `tidymedia.check_tracks`, `tidymedia_dropped_audio`, `tidymedia_ffmpeg_exit`, `tidymedia_loudnorm_no_measurement`, `tm_status` |
| normalize_audio_batch | 17 | 10 | 3 | `tidymedia.check_tracks`, `tidymedia_dropped_audio`, `tidymedia_ffmpeg_exit`, `tidymedia_loudnorm_no_measurement`, `tm_row_status`, `tm_rows`, `tm_status` |
| picture_in_picture | 7 | 1 | 0 | — |
| picture_in_picture_batch | 9 | 5 | 0 | — |
| sample_frames | 3 | 3 | 0 | — |
| sample_frames_batch | 4 | 2 | 0 | — |
| segment_video | 11 | 3 | 1 | — |
| segment_video_batch | 13 | 4 | 0 | — |
| separate_audio_video | 22 | 11 | 0 | `tidymedia_ffmpeg_exit`, `tidymedia_multitrack_separation`, `tidymedia_timeout`, `tm_status`, `tm_video_error` |
| separate_audio_video_batch | 21 | 13 | 1 | `tidymedia_ffmpeg_exit`, `tidymedia_multitrack_separation`, `tm_status` |
| standardize_video | 9 | 1 | 0 | — |
| standardize_video_batch | 15 | 4 | 0 | — |
| strip_metadata | 4 | 4 | 0 | — |
| strip_metadata_batch | 4 | 2 | 0 | — |

#### Repeated paragraphs at the base commit (AC1)

`Rscript tools/roxygen_repeats.R R/ffmpeg.R` at the base commit read 405 paragraphs in 34 blocks and listed 33. Each row gives the paragraph's first words and the blocks that held it. The Result column was filled at T5. "Kept in X" means block X holds the text and the other blocks take it with `@inheritParams`. A function name in the form `f()` is a function in `R/task-doc.R` that the blocks call inline. No row has copies that differ in what they claim.

| Row | Blocks | First words | Held by | Result |
|---|---|---|---|---|
| R1 | 12 | A logical: run the command through FFmpeg | `extract_frame`, `sample_frames`, `extract_audio`, `convert_audio`, `crop_video`, `format_for_web`, `strip_metadata`, `standardize_video`, `anonymize_video`, `concatenate_videos`, `compare_videos`, `picture_in_picture` | Kept in `crop_video`. |
| R2 | 12 | The compiled FFmpeg command (invisibly | the same 12 as R1 | `command_return()`. |
| R3 | 8 | A logical: map over jobs in parallel | `extract_audio_batch`, `convert_audio_batch`, `crop_video_batch`, `format_for_web_batch`, `separate_audio_video_batch`, `concatenate_videos_batch`, `compare_videos_batch`, `picture_in_picture_batch` | Kept in `extract_audio_batch`, reworded as "run the jobs in parallel … or one at a time". |
| R4 | 8 | A logical: run each command through FFmpeg | the same 8 as R3 | Kept in `extract_audio_batch`. |
| R5 | 8 | Additional arguments forwarded to `ffm_batch` (e.g. | the same 8 as R3 | Kept in `extract_audio_batch`. |
| R6 | 7 | A string containing the path to a video file. | `extract_frame`, `sample_frames`, `crop_video`, `format_for_web`, `standardize_video`, `anonymize_video`, `segment_video` | Kept in `crop_video`. |
| R7 | 7 | Additional arguments forwarded to `ffm_batch`, such as | `anonymize_video_batch`, `segment_video_batch`, `extract_frame_batch`, `sample_frames_batch`, `standardize_video_batch`, `strip_metadata_batch`, `normalize_audio_batch` | Kept in `anonymize_video_batch`. |
| R8 | 7 | The `jobs` tibble with an added `command` column | R3's 8 less `separate_audio_video_batch` | `jobs_return()`. |
| R9 | 5 | A string containing the path of the video file to write. | `separate_audio_video`, `crop_video`, `format_for_web`, `standardize_video`, `anonymize_video` | `write_path_param("video")` in `crop_video` and `separate_audio_video`, the others inherit from `crop_video`. |
| R10 | 5 | The tibble returned by `ffm_batch`: `jobs` with | `anonymize_video_batch`, `segment_video_batch`, `extract_frame_batch`, `standardize_video_batch`, `strip_metadata_batch` | `batch_return()`, also used by `normalize_audio_batch` and, with `"outdir"`, `sample_frames_batch`. |
| R11 | 4 | A logical: run each input's command | `anonymize_video_batch`, `sample_frames_batch`, `standardize_video_batch`, `strip_metadata_batch` | Kept in `anonymize_video_batch`. |
| R12 | 4 | A logical: when a non-`"none"` `hardware` … encode in software | `crop_video`, `segment_video`, `compare_videos`, `picture_in_picture` | `fallback_param()` in `crop_video`. |
| R13 | 4 | A string containing the path to a media file. | `extract_audio`, `separate_audio_video`, `convert_audio`, `strip_metadata` | Kept in `extract_audio`. |
| R14 | 3 | A string containing the path of the audio file to write. | `extract_audio`, `separate_audio_video`, `convert_audio` | `write_path_param("audio")` in `extract_audio` and `separate_audio_video`, `convert_audio` inherits. |
| R15 | 3 | A string naming the output video codec, applied to every row | `crop_video_batch`, `compare_videos_batch`, `picture_in_picture_batch` | Kept in `crop_video_batch`, split into two sentences. |
| R16 | 3 | A string naming the output video codec, or `NULL` | `crop_video`, `compare_videos`, `picture_in_picture` | `video_codec_unset_param()` in `crop_video`, also used by `segment_video`. |
| R17 | 3 | https://ffmpeg.org/ffmpeg-utils.html#time-duration-syntax | `segment_video`, `segment_video_batch`, `extract_frame_batch` | `time_duration_reference()`. |
| R18 | 3 | Switch the check off, and skip its FFprobe call | `extract_audio`, `convert_audio`, `normalize_audio` | `check_tracks_off_paragraph()`. |
| R19 | 3 | Switch the check off, and skip the whole sweep | `normalize_audio_batch`, `extract_audio_batch`, `convert_audio_batch` | `check_tracks_off_paragraph(batch = TRUE)`. |
| R20 | 3 | The encoder backend: … the H.264 family is assumed | `crop_video`, `compare_videos`, `picture_in_picture` | `hardware_param()` in `crop_video`, also used by `segment_video`. |
| R21 | 2 | A logical indicating whether the tibble should be sorted | `ffmpeg_codecs`, `ffmpeg_encoders` | Kept in `ffmpeg_codecs`. |
| R22 | 2 | A logical passed to `ffm_batch`: cut segments in parallel | `segment_video`, `segment_video_batch` | Kept in `segment_video`. |
| R23 | 2 | A logical: … re-encode with software libx264 | `format_for_web`, `format_for_web_batch` | `fallback_param("libx264")` in `format_for_web`. |
| R24 | 2 | A logical: … re-encode with the software `video_codec` | `standardize_video`, `anonymize_video` | `fallback_param("video_codec", reproducible = TRUE)` in `standardize_video`. |
| R25 | 2 | A string naming the codec for the carried audio track, applied to every row | `compare_videos_batch`, `picture_in_picture_batch` | Kept in `compare_videos_batch`. |
| R26 | 2 | A string naming the codec for the carried audio track. | `compare_videos`, `picture_in_picture` | Kept in `compare_videos`, with `audio_codec_copy_sentences("it")`. |
| R27 | 2 | A string naming the output audio codec (default `"copy"` | `standardize_video`, `anonymize_video` | Kept in `standardize_video`. |
| R28 | 2 | A string naming the output pixel format | `standardize_video`, `anonymize_video` | Kept in `standardize_video`. |
| R29 | 2 | A string naming the output video codec (default `"libx264"`) | `standardize_video`, `anonymize_video` | Kept in `standardize_video`. |
| R30 | 2 | EBU Recommendation R 128 (2014) | `normalize_audio`, `normalize_audio_batch` | `ebu_r128_reference()`. |
| R31 | 2 | The encoder backend: … Applies to video only | `standardize_video`, `anonymize_video` | `hardware_param(null_default = FALSE, video_only = TRUE)` in `standardize_video`. |
| R32 | 2 | When a row names no `audio_stream` | `extract_audio_batch`, `convert_audio_batch` | `dropped_audio_paragraph(batch = TRUE)`. |
| R33 | 2 | When no `audio_stream` is named | `extract_audio`, `convert_audio` | `dropped_audio_paragraph()`. |

#### Results at head (T5)

- AC1: `Rscript tools/roxygen_repeats.R R/ffmpeg.R` read 279 paragraphs in 34 blocks and listed none.
- AC2, AC3: the sweep over the 34 pages prints 252 findings. Against the base comparison, no page prints a finding more times, no page has more dash lines, and no finding sentence is printed for two pages.
- AC4: each of the 11 identifiers above is found by `git grep -wF` in at least one `man/*.Rd` file.
- AC5: no `man/*.Rd` file outside the domain changed.

#### Base claim found false

| Pages | Claim | Evidence |
|---|---|---|
| `extract_audio`, `convert_audio`, `normalize_audio` and their batch forms | The dropped-track check is skipped silently when FFprobe is missing or the input cannot be probed. | A probe that reaches its limit gives a `tidymedia_probe_timeout` warning (`R/ffprobe.R:313-327`). The base text said the same. It keeps its meaning and is item (h) of the M127 help-text follow-up row in `cairn/ROADMAP.md`. |

### M128

The base commit is `dc6cbd20`. At that commit, the domain is the 10 `man/*.Rd` files whose header names `R/ffm.R` and whose base name matches the M128 filter: `ffm_codec`, `ffm_compile`, `ffm_copy`, `ffm_drop`, `ffm_files`, `ffm_map`, `ffm_output_options`, `ffm_pixel_format`, `ffm_run` and `ffm_seek`.

#### Sweep output at the base commit (AC1, AC2, AC3, AC4)

The T1 sweep ran as `LC_ALL=en_US.UTF-8 Rscript tools/doc_prose_report.R *.Rd` over the 10 files that `git show dc6cbd20:man/<page>.Rd` wrote to a temporary folder. It read 185 sentences and exited 1 with 26 findings: 13 `[<n> words]`, 13 `[term …]` and no `[dash in Rd source]`. The same command gives the full output again. Stems are the glossary stems in each page's `--prose` output. Identifiers are the AC4 matches in each page. No page named the glossary.

| Page | Words | Term | Dash | Stems (AC3) | Identifiers (AC4) | Result |
|---|---|---|---|---|---|---|
| ffm_codec | 0 | 1 | 0 | `codec`, `stream` | `tidymedia_ffm` | No finding. Names the glossary. |
| ffm_compile | 0 | 0 | 0 | — | `tidymedia_ffm` | No finding. No stem. |
| ffm_copy | 1 | 1 | 0 | `codec`, `encod`, `key ?frame`, `stream` | `tidymedia_ffm` | No finding. Names the glossary. |
| ffm_drop | 0 | 0 | 0 | `stream` | `tidymedia_ffm` | No finding. Names the glossary. The title typo "Steams" is fixed. |
| ffm_files | 0 | 0 | 0 | — | `tidymedia_ffm` | No finding. No stem. |
| ffm_map | 3 | 3 | 0 | `stream` | `tidymedia_ffm` | No finding. Names the glossary. |
| ffm_output_options | 1 | 3 | 0 | — | `tidymedia_ffm` | No finding. No stem. |
| ffm_pixel_format | 0 | 0 | 0 | `pixel format` | `tidymedia_ffm` | No finding. Names the glossary. |
| ffm_run | 8 | 4 | 0 | `encod` | `tidymedia_ffm`, `tidymedia_ffmpeg_exit`, `tidymedia_loudnorm_no_measurement`, `tidymedia_multitrack_separation`, `tm_row_status`, `tm_rows`, `tm_status` | No finding. No stem, because "encodes the signal" became "stands for the signal". The exit-status section is split into two lists. |
| ffm_seek | 1 | 0 | 0 | `encod`, `key ?frame`, `stream` | `tidymedia_ffm` | No finding. Names the glossary. |

#### Results at head (T4)

- AC1, AC2: the sweep over the 10 pages at head reads 241 sentences, prints no finding and exits 0.
- AC3: the six pages with a stem at head each name the glossary. `ffm_run` lost its only stem.
- AC4: each of the 7 identifiers above is found by `git grep -wF` in at least one `man/*.Rd` file at head.
- AC5: `git diff --name-only dc6cbd20 HEAD -- man/` lists only the 10 domain pages.
- Tests: no test pins wording that changed, so no test was changed or removed. The exit-condition tests still find every class and field name on `?ffm_run`, and the audio-index tests still find the `audio_stream` link on `?ffm_codec` and `?ffm_copy`.
- Claim audit: a fresh reader read 125 claims and found 6 changed sentences that did not make the base claim. All 6 were put back to the base claim, and the reader found the new wording correct.

#### Base claims found false

Each claim keeps its meaning at head. All are items of the M128 follow-up row in `cairn/ROADMAP.md`.

| Page | Claim | Evidence |
|---|---|---|
| `ffm_map` | It is the only pipeline function that adds to earlier calls. | `ffm_output_options()` appends to `object$output_opts`, and the filter functions append to `filter_video` and `filter_audio` (`R/ffm.R`). |
| `ffm_run` | Value: the output has a `status` attribute on a non-zero exit. | `ffm_run()` gives a `tidymedia_ffmpeg_exit` error before it returns, so no caller sees the attribute. |
| `ffm_run` | Value: the pipeline never runs through a shell. | `run_program()` calls `system2()` (`R/program_management.R`), which passes the quoted command to `sh` on Unix. |
| `ffm_map` | A mapping is added beside `-map "[vout]"` for a function with several inputs. | `ffm_concat()` takes several inputs but sets no `[vout]` map. |
