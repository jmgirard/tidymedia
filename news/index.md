# Changelog

## tidymedia (development version)

### Breaking changes

- [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and
  [`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md)
  rename the `format` argument to `audio_codec`. The argument was always
  an audio codec — its own documentation said so, and its value has only
  ever been passed to FFmpeg’s `-c:a` — so this brings the last of the
  codec arguments onto the package’s `audio_codec` / `video_codec`
  naming, and every codec argument in the package is now spelled the
  same way.

  Only the name changes: `audio_codec = NULL` is still the default and
  still compiles `-q:a 0`, letting the output extension pick the codec
  at highest VBR quality, so existing default calls produce
  byte-identical commands. Note that `NULL` means something different
  here than on the other transform verbs, where it leaves the codec
  unset — on this verb it selects `-q:a 0`.

  `format` is removed rather than deprecated, in line with this
  package’s pre-1.0 clean-break policy. Calls passing it to
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  get R’s usual `unused argument` error;
  [`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md),
  whose `...` would otherwise ignore it in silence, aborts and names the
  replacement, as it does for a stale `format` column in a jobs table.

  In a jobs table, the per-row column is likewise now `audio_codec`, and
  it gains the ability to spell “unset”: `NA` in a cell keeps that row
  on the `-q:a 0` default, which the old `format` column could not
  express.

- [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  and
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md)
  replace the `reencode` argument with per-stream `audio_codec` and
  `video_codec` arguments, so you can name the encoder for each output
  file instead of choosing between “copy everything” and “let the
  container decide everything”. Both default to `"copy"`, which compiles
  exactly the commands `reencode = FALSE` compiled before;
  `audio_codec = NULL, video_codec = NULL` reproduces `reencode = TRUE`;
  and a codec name (`audio_codec = "libmp3lame"`) transcodes that stream
  alone. Each argument governs only its own output file.

  `reencode` is removed rather than deprecated, in line with this
  package’s pre-1.0 clean-break policy. Calls passing it to
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  get R’s usual `unused argument` error;
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
  whose `...` would otherwise ignore it in silence, aborts and names the
  replacement.

  In a jobs table, `audio_codec` and `video_codec` may be per-row
  columns where `NA` means “leave that stream’s codec unset”. They
  replace the per-row `reencode` column. Because each input row fans out
  into an audio row and a video row, the returned table collapses the
  two into one `codec` column carrying each row’s encoder for its own
  stream.

- [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
  and
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
  (and their `_batch` siblings) no longer re-encode the audio they pass
  through. They now stream-copy it, matching what
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
  and
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  have always done: previously these four left the audio codec unset, so
  whatever encoder your FFmpeg build defaults to for the output
  container silently re-encoded the audio — a quality loss, and a result
  that depended on the machine. Their compiled commands therefore gain
  `-codec:a copy`.

  The new `audio_codec` argument controls this. `"copy"` is the default;
  name an encoder (e.g. `audio_codec = "aac"`) to transcode instead, or
  pass `audio_codec = NULL` for the old behavior of leaving the codec
  unset. Note that a stream copy fails if the output container cannot
  hold the source audio codec (FLAC in `.mp4`, say) — name an encoder in
  that case. In a jobs table, `audio_codec` may be a per-row column,
  where `NA` means “leave it unset”.

  Cutting with `segment_video(reencode = FALSE)` copies every stream by
  definition, so any `audio_codec` other than `"copy"` is an error
  there, as is naming an audio encoder on a composite that carries no
  audio at all.

- [`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md)
  now rejects a wrongly typed `audio` column up front with a clear
  message instead of failing partway through the batch, and
  [`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md)’s
  equivalent check no longer accepts an all-`NA` column of the wrong
  type.

### New features

- [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
  and
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  (and their `_batch` siblings) gain an `audio_codec` argument. Both
  verbs re-encode video and stream-copy audio, but the copy was fixed in
  place, so there was no way to say otherwise — which mattered because a
  stream copy fails outright when the output container cannot hold the
  source audio codec, and the documented remedy for that (“name an
  encoder”) had no argument to name one. `audio_codec = "copy"` is the
  default and compiles exactly the commands these verbs compiled before,
  so calls that name their arguments (or take the defaults) produce
  identical output; `audio_codec = "aac"` transcodes the audio instead;
  `audio_codec = NULL` emits no audio codec at all and lets the output
  container choose.

  The new argument sits beside `video_codec` rather than at the end, so
  the arguments after it have all shifted one position: **calls that
  pass `pixel_format`, `hardware`, `fallback`, or `run` by position
  rather than by name must be updated.**
  `standardize_video(f, out, 1280, 720, 30, "libx264", "yuv420p")` now
  reads `"yuv420p"` as the audio codec, not the pixel format. In line
  with this package’s pre-1.0 clean-break policy the argument is placed
  where it belongs rather than appended for compatibility; naming your
  arguments avoids the problem entirely.

  In a jobs table, `audio_codec` may be a per-row column where `NA`
  means “leave that row’s codec unset”. `hardware` remains batch-wide
  and applies to video only — audio is never hardware-accelerated.

- [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  and
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md)
  gain the `hardware` and `fallback` arguments the other re-encoding
  verbs already carry, so a video stream that is being re-encoded on the
  way out can be encoded on an NVIDIA GPU: `hardware = "nvenc"`. Only
  the video output is affected — nvenc encodes video, so the audio file
  is byte-for-byte what it would have been otherwise, whatever you pass.

  Because this verb copies the video by default, and a copy runs no
  encoder at all, `hardware = "nvenc"` on its own is an error rather
  than a silent switch from a lossless copy to a GPU re-encode. Pair it
  with `video_codec = NULL`, which assumes the H.264 family, or name a
  codec (`video_codec = "libx265"`) to pin a different one — a non-H.264
  container such as `.webm` needs that explicit name. As on the other
  verbs, `hardware` applies to a whole batch rather than row by row, so
  a jobs table mixing copied and re-encoded video must be split into
  separate calls.

- [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  and
  [`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md)
  gain an `audio_codec` argument naming the output audio encoder.
  Loudness normalization filters the audio, so it must be re-encoded —
  and until now it was re-encoded to whatever encoder your FFmpeg build
  defaults to for the output container, which made the result depend on
  the machine. `audio_codec = "aac"` (say) pins it. The default `NULL`
  leaves the codec unset, so existing calls compile exactly the commands
  they did before, and `"copy"` is an error, since a filtered stream
  cannot be copied. In a jobs table, `audio_codec` may be a per-row
  column (`NA` means “leave it unset”), and it applies to the two-pass
  path as well.

- [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
  and
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
  (and their `_batch` siblings) gain a `video_codec` argument, alongside
  the `hardware`/`fallback` GPU toggle. The default `video_codec = NULL`
  leaves the codec unset, so these verbs compile exactly the commands
  they did before and each output keeps its container’s default encoder.
  In a jobs table, `video_codec` may be a per-row column (`NA` means
  “leave it unset”); `hardware` and `fallback` apply to the whole batch.
  Naming a codec (or a hardware backend) while cutting with
  `segment_video(reencode = FALSE)` is an error — a stream copy runs no
  encoder.

- Opt-in NVIDIA GPU (nvenc) video encoding.
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
  and
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  (and their `_batch` siblings) gain a `hardware` argument:
  `hardware = "nvenc"` re-encodes on the GPU, choosing the nvenc encoder
  for the codec family (e.g. `h264_nvenc`). By default an unavailable
  GPU is an error (so output stays reproducible); `fallback = TRUE`
  re-encodes in software with a message instead.
  [`has_nvenc()`](https://jmgirard.github.io/tidymedia/reference/nvenc_encoder.md)
  reports whether an nvenc encoder is available in your FFmpeg build and
  [`nvenc_encoder()`](https://jmgirard.github.io/tidymedia/reference/nvenc_encoder.md)
  names it. Hardware *decoding* and GPU filter pipelines remain out of
  scope — use
  [`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
  for those.

- [`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md)
  samples a video at a fixed rate (`fps`) or interval (`interval`,
  seconds between frames) into a numbered image sequence — the front
  door to per-frame coding and computer-vision feature pipelines.
  [`sample_frames_batch()`](https://jmgirard.github.io/tidymedia/reference/sample_frames_batch.md)
  does the same across many videos from a jobs table.

- Batch (`_batch`) siblings for the remaining single-input transform
  verbs:
  [`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
  [`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md),
  [`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md),
  and
  [`format_for_web_batch()`](https://jmgirard.github.io/tidymedia/reference/format_for_web_batch.md)
  process many files from one jobs table, each a thin wrapper over
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).
  The audio verbs require an `output` column; the video verbs auto-name
  outputs (`_cropped`, `_web.mp4`) when it is absent, and all four
  reject two rows that resolve to the same output path.

### Standardized function and argument names

The public API was renamed to a single, predictable scheme. These are
breaking changes with no deprecation shims (the package is still pre-1.0
and soaking).

- **Batch verbs now use a `_batch` suffix** instead of a plural noun:
  `segment_videos()` →
  [`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
  `standardize_videos()` →
  [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
  `normalize_audios()` →
  [`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
  `anonymize_videos()` →
  [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
  and `extract_frames()` →
  [`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md)
  (which also removes the confusion with grabbing “many frames” from one
  video).
- **FFmpeg capability queries moved out of the `get_*` namespace:**
  `get_codecs()` →
  [`ffmpeg_codecs()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_codecs.md)
  and `get_encoders()` →
  [`ffmpeg_encoders()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md).
  `get_*` is now reserved for per-file metadata getters.
- **`audio_as_mp3()` is now
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)**,
  with a new `format` argument. The default (`format = NULL`) reproduces
  the old behavior exactly (the output format follows the file
  extension); pass `format` to pin the audio codec.
- **Metadata getters renamed** to match the argument vocabulary:
  `get_samplingrate()` →
  [`get_sample_rate()`](https://jmgirard.github.io/tidymedia/reference/get_sample_rate.md)
  and `get_framerate()` →
  [`get_frame_rate()`](https://jmgirard.github.io/tidymedia/reference/get_frame_rate.md).
- **Codec and time-bound arguments harmonized:** `acodec`/`vcodec` (and
  the matching jobs-table columns) are now `audio_codec`/`video_codec`,
  and
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)’s
  `ts_start`/`ts_stop` are now `start`/`end` (matching the batch
  columns).
- **Removed unintended exports:** the unused tidy-eval reexports
  (`enquo()`, `enquos()`, `as_label()`, `as_name()`, `:=`) and two
  internal helpers (`pad_integers()`, `convert_fractions()`) are no
  longer exported. `.data` remains reexported.

### Documentation

- Help pages now cross-reference each other: every task verb links to
  the `ffm_*` pipeline builders it is built on (and each builder back to
  the verbs that use it), and the three metadata reader families
  (`probe_*()`, `mediainfo_*()`, `get_*()`) link to one another so you
  can find the alternative backend.
- Each metadata help page now states its backend (FFprobe or MediaInfo)
  and what it returns (a tibble, a value, or a single scalar per file),
  and the “Media metadata as tibbles” vignette gains a table comparing
  the reader families at a glance.
- New “A research preprocessing workflow” vignette walks an end-to-end
  pipeline — standardizing recordings, normalizing and extracting audio,
  sampling frames, de-identifying, and packaging for sharing —
  demonstrating the task verbs on a realistic dyadic-interaction study.
  The “Get started” vignette now leads with the task verbs (the front
  door most users need) before descending to the builder, and every
  vignette cross-links to the others.

### Fixed-region anonymization

- New
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  covers one or more fixed rectangular regions of a video with opaque
  filled boxes — for redacting a face, a name badge, or any area that
  stays in one place for the whole clip (there is no motion tracking).
  Regions are given as a data frame of `x`, `y`, `width`, `height`
  (numbers or FFmpeg expressions), with an optional per-row `color`. The
  video is re-encoded reproducibly (H.264 / `yuv420p` by default) and
  audio is stream-copied unchanged.
- New `anonymize_videos()` applies the same box-fill redaction across
  many videos from one jobs tibble — each row names an `input` and
  carries its own `regions` (a list-column of boxes data frames), with
  optional per-row `output`, `color`, `vcodec`, and `pixel_format`
  columns. Like the other table-driven verbs it is a thin wrapper over
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  returning one reproducible command per input and supporting `verify`,
  `manifest`, and parallel execution.

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
- `normalize_audios()` gained `two_pass` too, applying the same accurate
  measured/linear normalization across a whole jobs table. With
  `two_pass = TRUE` it measures every input (honoring `parallel` and
  each row’s targets), then builds and runs one linear correction per
  row, surfacing the five measured values as
  `measured_I`/`measured_TP`/`measured_LRA`/ `measured_thresh`/`offset`
  columns. As with the scalar verb the analysis pass always runs — even
  under `run = FALSE`, which then gates only the correction pass — and a
  row whose analysis yields no usable measurement aborts the call,
  naming the offending row. `two_pass` is a whole-table switch, not a
  per-row column. The single-pass default is unchanged.

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

- Added `normalize_audios()`, a table-driven companion to
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

- Added `standardize_videos()`, a table-driven companion to
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

- Added `extract_frames()`, a table-driven companion to
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

- `normalize_audio_batch(audio_codec = NA)` now aborts instead of
  quietly compiling the default command. A scalar `NA` was resolved the
  same way as an `NA` cell in a jobs-table column — where it
  legitimately means “leave this row’s codec unset” — so an accidental
  `NA` argument produced a command with no `-codec:a` and no indication
  that anything had been ignored.

- Every `video_codec` and `audio_codec` argument now reports a bad value
  against the argument and the verb you actually called. Several
  previously blamed an internal helper, named FFmpeg’s own `video` /
  `audio` parameter instead of the argument you passed, or — on the
  `_batch` verbs — surfaced the complaint from inside the row loop with
  an `In index: 1` prefix, as though one row’s data were at fault rather
  than a whole-table argument. Affected
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
  [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
  [`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
  and
  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md).

- A bad `video_codec` / `audio_codec` **argument** on a `_batch` verb is
  now refused even when `jobs` carries a column of the same name. The
  column takes precedence over the argument, so a non-string value
  passed as the argument used to be discarded in silence;
  [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
  [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
  [`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md)
  and
  [`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md)
  now report it, matching
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
  which already refused it. Values these verbs *accept* are unchanged —
  a codec string, and `NULL` where it was already legal, behave exactly
  as before.

- One knock-on for
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md):
  a call that passes both a bad `video_codec` and an invalid `width` /
  `height` / `fps` now reports the codec problem first, where it
  previously reported the dimension problem. Both complaints are real
  and fixing the codec argument reveals the other; no value that was
  accepted before is refused now. The other verbs keep their previous
  ordering.

- [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  (and the `parallel = TRUE` path of
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  / `segment_videos()`) now warns when parallel processing is requested
  but no parallel
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  is active. Previously such calls ran one job at a time with no speedup
  and no indication; the warning points to
  `future::plan(future::multisession)`.

### Batch segmentation across files

- Added `segment_videos()`, a table-driven companion to
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md).
  Pass a jobs tibble with `input`, `output`, `start`, and `end` columns
  — one row per segment — to cut segments spanning many input files in
  one call. It is a thin wrapper over
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  so `...` forwards batch options such as `verify`, `manifest`,
  `checksums`, and `progress`; `reencode` selects accurate re-encoding
  (default) or the fast keyframe-snapping copy path, as in
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md).
- `segment_videos()` now reaches full parity with
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md):
  the `output` column is optional (when absent, names are derived per
  input file as `<basename>_<n>.<ext>`, numbering restarting for each
  input), a per-row `reencode` column overrides the scalar `reencode`
  argument, and non-numeric/character `start`/`end` (or a non-logical
  `reencode`) columns are rejected with a clear error instead of an
  opaque FFmpeg failure.

### Verification & provenance

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

### Multi-input verbs

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

### Safe execution

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
- `convert_fractions()` parses fractions directly instead of via
  `eval(parse())`, passes `NA` through, and errors on values that are
  neither a number nor a fraction.
- FFprobe’s `key=value` output is split on the first `=` only, so values
  that contain `=` are no longer truncated; the superseded
  `tidyr::separate()` call is gone. Files with zero streams no longer
  trip the stream loop.

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
  `audio_as_mp3()`,
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
  `get_framerate()`,
  [`get_width()`](https://jmgirard.github.io/tidymedia/reference/get_width.md),
  [`get_height()`](https://jmgirard.github.io/tidymedia/reference/get_height.md),
  `get_samplingrate()`) now shell-quote the `--Inform` argument, so they
  work on POSIX shells where the `;` was previously parsed as a command
  separator.

## tidymedia 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
