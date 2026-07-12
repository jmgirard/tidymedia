# Changelog

## tidymedia (development version)

### Graceful handling of silent input in two-pass loudnorm

- Two-pass normalization now handles digitally silent input honestly.
  Silence measures as `-inf` loudness, which cannot be normalized to a
  target. Previously this surfaced as a misleading “could not parse the
  loudnorm measurement” error. Now `normalize_audio(two_pass = TRUE)`
  aborts with a clear message that names silence as the cause, and
  `normalize_audios(two_pass = TRUE)` no longer lets one silent row
  abort the whole batch: the non-silent rows are normalized, the silent
  rows are marked in a new logical `silent` column (with
  `success = FALSE` and no output written), and a warning names them.
  Genuine analysis failures still abort fail-fast. (Near-silent but
  non-empty audio is unaffected.)
- The two-pass batch’s result schema no longer depends on how many rows
  are silent: when `verify` or `manifest` is requested, the `verified`
  column and the provenance manifest are now returned even when *every*
  row is silent (silent rows carry `NA` for those outputs), matching a
  batch with some non-silent rows.

### Accurate two-pass loudness normalization

- [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  gained `two_pass`. With `two_pass = TRUE` it runs an analysis pass to
  measure the input’s loudness, then a linear correction pass that feeds
  those measurements back, hitting the EBU R128 target far more
  precisely than the single-pass default on material with a wide
  loudness range. Because it must measure the input, two-pass always
  calls FFmpeg — even under `run = FALSE`, where the analysis still runs
  and the returned value is the exact correction command, left
  unexecuted. The single-pass default is unchanged and stays binary-free
  under `run = FALSE`.
- [`normalize_audios()`](https://jmgirard.github.io/tidymedia/reference/normalize_audios.md)
  gained `two_pass` too, applying the same accurate measured/linear
  normalization across a whole jobs table. With `two_pass = TRUE` it
  measures every input (honoring `parallel` and each row’s targets),
  then builds and runs one linear correction per row, surfacing the five
  measured values as `measured_I`/`measured_TP`/`measured_LRA`/
  `measured_thresh`/`offset` columns. As with the scalar verb the
  analysis pass always runs — even under `run = FALSE`, which then gates
  only the correction pass — and a row whose analysis yields no usable
  measurement aborts the call, naming the offending row. `two_pass` is a
  whole-table switch, not a per-row column. The single-pass default is
  unchanged.

### Audio loudness normalization

- Added
  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
  a task verb that normalizes a file’s perceived loudness to an EBU R128
  target using FFmpeg’s single-pass `loudnorm` filter. By default it
  targets -23 LUFS integrated loudness with a -1 dBTP true-peak ceiling
  (EBU R128, measured per ITU-R BS.1770-4), copies the video stream
  unchanged, and preserves the source channel layout, so the same input
  always yields one reproducible command. Supply `target_loudness`,
  `true_peak`, and `loudness_range` to retarget, and
  `channels`/`sample_rate` to downmix or resample the audio. Note that
  single-pass `loudnorm` resamples its output (up to 192 kHz,
  encoder-capped), so set `sample_rate` to pin the output rate.
- Added
  [`ffm_loudnorm()`](https://jmgirard.github.io/tidymedia/reference/ffm_loudnorm.md),
  a builder that appends FFmpeg’s EBU R128 `loudnorm` audio filter to a
  pipeline — the first builder to write the audio filter chain (`-af`).

### Batch audio normalization across files

- Added
  [`normalize_audios()`](https://jmgirard.github.io/tidymedia/reference/normalize_audios.md),
  a table-driven companion to
  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md).
  Pass a jobs tibble with one row per input (only an `input` column is
  required) to loudness-normalize many files in one call, each to an EBU
  R128 target. It returns the tibble plus one reproducible `command` per
  row. The five loudness knobs — `target_loudness`, `true_peak`,
  `loudness_range`, `channels`, and `sample_rate` — may each appear as a
  column to vary per row, and outputs are auto-named
  `<base>_normalized.<ext>` when no `output` column is given. It is a
  thin wrapper over
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  so `...` forwards batch options such as `verify`, `manifest`,
  `checksums`, `progress`, and `parallel`.

### Video standardization

- Added
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  a task verb that re-encodes a video to a reproducible,
  analysis-friendly format in one call. By default it produces H.264
  video with `yuv420p` and `+faststart`, stream-copies the audio
  unchanged, and keeps the source resolution and frame rate (rounding
  odd dimensions down to the nearest even value so the codec can
  encode), so the same input always yields a byte-identical command.
  Supply `width`/`height` to set the output size (giving only one
  preserves the aspect ratio with an even output dimension), `fps` to
  resample the frame rate, and `vcodec`/`pixel_format` to override the
  codec or pixel format.
- Added
  [`ffm_fps()`](https://jmgirard.github.io/tidymedia/reference/ffm_fps.md),
  a builder that appends an `fps` filter to a pipeline, accepting either
  a number of frames per second or an FFmpeg framerate expression such
  as `"30000/1001"`.

### Batch standardization across files

- Added
  [`standardize_videos()`](https://jmgirard.github.io/tidymedia/reference/standardize_videos.md),
  a table-driven companion to
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md).
  Pass a jobs tibble with an `input` column — one row per video — to
  re-encode many files to a reproducible format in one call. It is a
  thin wrapper over
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  so `...` forwards batch options such as `verify`, `manifest`,
  `checksums`, and `progress`, and each row compiles to a command
  byte-identical to the equivalent
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
  call. Any of `width`, `height`, `fps`, `vcodec`, and `pixel_format`
  may appear as a column to override that setting per row, otherwise the
  function argument applies to every row. The `output` column is
  optional: when absent, names are derived per input as
  `<basename>_standardized.<ext>` (keeping the source extension), and a
  duplicated `input` with no `output` column is rejected rather than
  silently overwritten.

### Frame extraction across files

- Added
  [`extract_frames()`](https://jmgirard.github.io/tidymedia/reference/extract_frames.md),
  a table-driven companion to
  [`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md).
  Pass a jobs tibble with an `input` column and exactly one of a
  `timestamp` or `frame` column — one row per frame — to grab still
  images spanning many input files in one call. It is a thin wrapper
  over
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  so `...` forwards batch options such as `verify`, `manifest`,
  `checksums`, and `progress`. The `output` column is optional: when
  absent, names are derived per input file as `<basename>_<n>.<format>`
  (default `format = "png"`), the frame number restarting for each
  input.

### Bug fixes

- [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  (and the `parallel = TRUE` path of
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  /
  [`segment_videos()`](https://jmgirard.github.io/tidymedia/reference/segment_videos.md))
  now warns when parallel processing is requested but no parallel
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  is active. Previously such calls ran one job at a time with no speedup
  and no indication; the warning points to
  `future::plan(future::multisession)`.

### Batch segmentation across files

- Added
  [`segment_videos()`](https://jmgirard.github.io/tidymedia/reference/segment_videos.md),
  a table-driven companion to
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md).
  Pass a jobs tibble with `input`, `output`, `start`, and `end` columns
  — one row per segment — to cut segments spanning many input files in
  one call. It is a thin wrapper over
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  so `...` forwards batch options such as `verify`, `manifest`,
  `checksums`, and `progress`; `reencode` selects accurate re-encoding
  (default) or the fast keyframe-snapping copy path, as in
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md).
- [`segment_videos()`](https://jmgirard.github.io/tidymedia/reference/segment_videos.md)
  now reaches full parity with
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md):
  the `output` column is optional (when absent, names are derived per
  input file as `<basename>_<n>.<ext>`, numbering restarting for each
  input), a per-row `reencode` column overrides the scalar `reencode`
  argument, and non-numeric/character `start`/`end` (or a non-logical
  `reencode`) columns are rejected with a clear error instead of an
  opaque FFmpeg failure.

### Verification & provenance (M08)

- Added
  [`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md),
  a probe-backed checker that confirms an output really has the
  properties you asked for. It returns a tidy tibble with one row per
  check (`file`, `check`, `expected`, `actual`, `pass`) covering
  `duration`, `width`, `height`, `video_codec`, `audio_codec`, and
  `sample_rate`, plus any other FFprobe field passed by name through
  `...`. Numeric checks use an absolute `tolerance` (default `0.1`, so
  integer dimensions match exactly while duration gets a little slack);
  codec checks match exactly.
- Verification is wired into execution. `ffm_run(verify = <named list>)`
  probes the output after a successful run and aborts, listing the
  failed checks, if any assertion fails.
  `ffm_batch(verify = <list or function>)` instead records the outcome
  in a logical `verified` column (one spec for all jobs, or a
  `pmap`-style function of the job columns) without aborting.
- Added a batch provenance manifest. `ffm_batch(manifest = TRUE)`
  attaches a per-job record — command, FFmpeg/FFprobe versions,
  timestamp, and output size — read back with
  [`ffm_manifest()`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md),
  which can also write it to CSV via `path =`. `checksums = TRUE`
  additionally records input/output md5 checksums.
- `ffm_batch(progress = TRUE)` shows a `cli` progress bar as the jobs
  run (following the `future` plan on the parallel path).

### Multi-input verbs (M07)

- Completed the blessed multi-input builder set with
  [`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md)
  (stack videos top to bottom, the vertical companion to
  [`ffm_hstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md))
  and
  [`ffm_overlay()`](https://jmgirard.github.io/tidymedia/reference/ffm_overlay.md)
  (composite one video over another at an `x`/`y` position given as
  pixels or an FFmpeg expression).
  [`ffm_overlay()`](https://jmgirard.github.io/tidymedia/reference/ffm_overlay.md)
  also takes an optional `scale` to resize the overlay to a fraction of
  the main video’s width.
- Added two research task verbs built on these:
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  for a side-by-side or stacked comparison video, and
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
  for an inset overlay (corner or center `position`, `scale`, `margin`).
  Both drop audio by default; pass `audio =` an input index to carry
  that track.

### Safe execution (M06)

- Pipelines are now executed as argument vectors (via
  [`system2()`](https://rdrr.io/r/base/system2.html)), never through a
  shell string, so input and output paths containing spaces, quotes,
  `$`, or backticks are handled correctly. This applies to
  [`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  and every task verb;
  [`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
  still returns the same reproducible command string. The Layer 0 escape
  hatches
  ([`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md),
  [`ffprobe()`](https://jmgirard.github.io/tidymedia/reference/ffprobe.md),
  [`mediainfo()`](https://jmgirard.github.io/tidymedia/reference/mediainfo.md))
  keep their raw-string interface.
- Raw output options added with
  [`ffm_output_options()`](https://jmgirard.github.io/tidymedia/reference/ffm_output_options.md)
  are tokenized on whitespace at execution time; option values
  themselves must not contain spaces (they never worked reliably
  before).

### Breaking changes

- [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  now stream-copies by default — separation is lossless and fast, but
  each output container must support the source codec. Use the new
  `reencode = TRUE` argument for the previous re-encoding behavior.
- [`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md)
  and
  [`ffm_pixel_format()`](https://jmgirard.github.io/tidymedia/reference/ffm_pixel_format.md)
  now reject values that are not a single clean token (no whitespace or
  shell metacharacters, and starting with a letter or digit).
- [`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md)
  — and every task verb built on it — now aborts with FFmpeg’s exit
  status when an encode fails, instead of returning silently (the old
  shell path only emitted a warning).
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  still records failures in its `success` column without aborting.
- [`ffm_output_options()`](https://jmgirard.github.io/tidymedia/reference/ffm_output_options.md)
  now rejects option groups containing quote characters: options are
  split on whitespace into arguments at execution, so quoting cannot
  group tokens (previously such commands executed with a different
  meaning than printed).

### Bug fixes

- An explicit
  [`ffm_map()`](https://jmgirard.github.io/tidymedia/reference/ffm_map.md)
  on a multi-input pipeline
  (e.g. [`ffm_hstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md))
  is now emitted alongside the automatic `-map "[vout]"` instead of
  being silently ignored, so e.g. `ffm_map(p, "0:a")` keeps the first
  input’s audio next to the stacked video.
- Test coverage is measured again: an empty `R/zzz.R` triggered a `covr`
  bug that silently reported 0% package coverage.

## tidymedia 0.1.0

First tagged release, bringing the metadata, builder, and task-verb work
of the 0.0.0.900x development series to a documented, release-ready
state.

### Documentation

- Every exported function now carries a worked example and an
  architecture-layer `@family` tag, and there is a [pkgdown
  site](https://jmgirard.github.io/tidymedia/) whose reference index is
  grouped by the three layers (escape hatch, builder, task verbs).
- Added three vignettes: *Get started* (building pipelines,
  [`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)),
  *Media metadata as tibbles*
  ([`vignette("metadata")`](https://jmgirard.github.io/tidymedia/articles/metadata.md)),
  and *Batch processing*
  ([`vignette("batch")`](https://jmgirard.github.io/tidymedia/articles/batch.md)).
- A small sample clip now ships in `inst/extdata/sample.mp4` so examples
  and vignettes are runnable.

### Metadata layer

- The MediaInfo and FFprobe readers now **accept a vector of files** and
  return one stacked tibble keyed by a leading `file` column, so
  metadata for a whole batch is ready for `dplyr` joins and filters.
  This covers
  [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md),
  the `probe_*()` shortcuts,
  [`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md),
  [`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md),
  [`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md),
  and the `get_*()` convenience helpers.
- **Typed output is now the default.** Every reader gains a `typed`
  argument (default `TRUE`) that converts numeric columns to
  integers/doubles and turns missing markers (FFprobe’s `"N/A"`,
  MediaInfo’s empty values) into `NA`; fractions, ratios, hex
  identifiers, and text stay as strings. Pass `typed = FALSE` for the
  previous all-character behavior. This replaces
  [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)’s
  `convert` argument.
- Readers are **resilient to unreadable files**: a missing file, or one
  that cannot be probed, among several yields an all-`NA` row (or `NA`
  value) plus a warning, instead of aborting the whole call. Malformed
  *arguments* still abort.
- Arguments are now passed to the CLIs through argument vectors
  ([`system2()`](https://rdrr.io/r/base/system2.html)) rather than
  interpolated into a shell string, so file paths and MediaInfo
  `--Inform` templates containing spaces, quotes, `;`, `%`, or `$` work
  correctly. The Layer 0 escape hatches
  [`mediainfo()`](https://jmgirard.github.io/tidymedia/reference/mediainfo.md)
  /
  [`ffprobe()`](https://jmgirard.github.io/tidymedia/reference/ffprobe.md)
  keep their raw-string signatures.
- Output column schemas are unified: readers lead with a `file` column
  and the two built-in MediaInfo templates now emit snake_case column
  names. User-supplied names (`mediainfo_query(names =)`, custom
  template headers) are kept verbatim.

### Bug fixes

- [`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
  no longer lets FFmpeg read the calling process’s standard input, so
  running a pipeline (e.g. via
  [`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md))
  inside a script that is itself fed through `stdin` no longer swallows
  the rest of that input. Equivalent to FFmpeg’s `-nostdin`.
- [`probe_container()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
  [`probe_streams()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
  [`probe_video()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
  and
  [`probe_audio()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)
  now return the requested tibble when called with `infile =`; they
  previously returned `NULL`.
- [`convert_fractions()`](https://jmgirard.github.io/tidymedia/reference/convert_fractions.md)
  parses fractions directly instead of via `eval(parse())`, passes `NA`
  through, and errors on values that are neither a number nor a
  fraction.
- FFprobe’s `key=value` output is split on the first `=` only, so values
  that contain `=` are no longer truncated; the superseded
  [`tidyr::separate()`](https://tidyr.tidyverse.org/reference/separate.html)
  call is gone. Files with zero streams no longer trip the stream loop.

## tidymedia 0.0.0.9002

### Batch processing

- Added
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  the tidymedia batch entry point: it maps a pipeline-building function
  over every row of a jobs data frame (columns are passed to the
  function by name,
  [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html)-style),
  compiles one reproducible command per job, and optionally runs them.
  It returns the jobs as a tibble with an added `command` column and,
  when run, a `success` column. Set `parallel = TRUE` to map with
  `furrr` following the active `future` plan.

### Task verbs rebuilt on the builder

- Every task verb is now a thin wrapper over the Layer 1 `ffm_*` builder
  and no longer assembles its own FFmpeg command string. Each gains a
  `run` argument and returns its compiled, reproducible command
  (invisibly when run):
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`audio_as_mp3()`](https://jmgirard.github.io/tidymedia/reference/audio_as_mp3.md),
  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
  [`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md),
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
  and
  [`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md).
- [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  is now built on
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
  it returns the job tibble (one row per segment with its command and
  run status) and gains `reencode` and `parallel` arguments.
- **Cutting is frame-accurate by default.**
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  and the new
  [`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md)
  default to `reencode = TRUE`, which re-encodes so cuts land on the
  exact requested frames. `reencode = FALSE` selects a fast, lossless
  copy that snaps to the nearest keyframes (so the output duration may
  differ by up to one group-of-pictures). The previous copy-based
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  cut at the wrong point and shifted timestamps; that behavior is gone.
- Breaking:
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)’s
  free-form `options` string is replaced by an `acodec` argument;
  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md)
  drops its `arg` argument and now centers the crop by default;
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  returns a named vector of two commands;
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  returns a tibble.

### Pipeline engine

- Added
  [`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md),
  a seek-based cut using `-ss`/`-to` (distinct from the `trim` filter of
  [`ffm_trim()`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md)),
  so cuts can stream-copy. Accurate seeks output-seek and re-encode;
  fast copy seeks input-seek and add `-avoid_negative_ts`.
- Added
  [`ffm_concat()`](https://jmgirard.github.io/tidymedia/reference/ffm_concat.md),
  a blessed multi-input verb that concatenates the pipeline’s inputs via
  FFmpeg’s concat demuxer (fast, lossless, same-format).
- Added
  [`ffm_output_options()`](https://jmgirard.github.io/tidymedia/reference/ffm_output_options.md),
  a controlled passthrough for raw output options that
  [`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
  still positions and quotes.

## tidymedia 0.0.0.9001

### Pipeline engine

- Reworked the Layer 1 `ffm_*` builder onto a structured command model:
  [`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
  is now the single place that assembles, positions, and quotes every
  option. Single-input filter chains compile to `-vf`/`-af`; multi-input
  stacking compiles to a valid `-filter_complex` graph with explicit
  stream labels and an automatic `-map`.
- Fixed four builder bugs: `ffm_trim(setpts = FALSE)` no longer forces a
  `setpts` filter;
  [`ffm_drop()`](https://jmgirard.github.io/tidymedia/reference/ffm_drop.md)
  flags are now output options placed after the input (not before `-i`);
  [`ffm_pixel_format()`](https://jmgirard.github.io/tidymedia/reference/ffm_pixel_format.md)
  no longer runs into the output filename; and the previously invalid
  `-filter_complex:v` output is gone.
- [`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
  now errors early when a stream is set to codec `copy` while a filter
  targets that same stream, instead of failing cryptically in ffmpeg.
- [`ffm_hstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md)
  must be applied before other video filters and now produces a runnable
  command (verified end-to-end against ffmpeg).

### Infrastructure

- Added a testthat (3rd edition) test suite covering the `ffm_*`
  pipeline builder and
  [`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
  output, plus binary-gated tests for the ffmpeg/ffprobe/mediainfo task
  functions.
- Added GitHub Actions workflows for `R CMD check` (macOS, Windows,
  Linux) and test coverage; the Linux jobs install ffmpeg and mediainfo
  so execution tests run in CI.
- All input validation and user-facing messages now use rlang’s
  `check_*` helpers and cli
  ([`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  /
  [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html));
  the assertthat dependency has been removed. Added `dplyr`, `tidyr`,
  `purrr`, and `cli` to Imports (the first three were already used but
  undeclared).
- Enumerated arguments (e.g. `units`, `unit`, `section`, `template`,
  `program`) are now matched exactly via
  [`rlang::arg_match()`](https://rlang.r-lib.org/reference/arg_match.html)
  instead of the partial matching of
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html); pass the full
  value.

### Bug fixes

- [`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md)
  (and the helpers built on it:
  [`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md),
  [`get_framerate()`](https://jmgirard.github.io/tidymedia/reference/get_framerate.md),
  [`get_width()`](https://jmgirard.github.io/tidymedia/reference/get_width.md),
  [`get_height()`](https://jmgirard.github.io/tidymedia/reference/get_height.md),
  [`get_samplingrate()`](https://jmgirard.github.io/tidymedia/reference/get_samplingrate.md))
  now shell-quote the `--Inform` argument, so they work on POSIX shells
  where the `;` was previously parsed as a command separator.

## tidymedia 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
