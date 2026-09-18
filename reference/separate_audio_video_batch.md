# Separate Audio and Video for Many Files From a Jobs Table

Split the audio and video streams of many input files from a single jobs
tibble. This is the **batch** (table-driven) form of
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
for when you have more than one file. Each row is one input that gives
**two** outputs. The `input`, `audiofile` and `videofile` columns are
all required. The function is a thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).
It turns every input row into two single-output jobs, one per stream. So
a jobs table of `N` rows returns `2N` rows, with one reproducible
compiled command per stream. Each stream uses the same map and
stream-copy pipeline as
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md).
The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as codec, container and stream copy.

## Usage

``` r
separate_audio_video_batch(
  jobs,
  audio_codec = "copy",
  video_codec = "copy",
  hardware = c("none", "nvenc", "videotoolbox"),
  fallback = FALSE,
  quality = NULL,
  audio_stream = NULL,
  run = TRUE,
  parallel = FALSE,
  ...
)
```

## Arguments

- jobs:

  A data frame with one row per input. It has at least an `input` column
  (source path), plus `audiofile` and `videofile` columns that name the
  two destinations. All three are **required**. Like
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  this function derives no output paths, because the container extension
  of a copied stream is the instruction. That extension must match the
  source codec. No two destinations in a table can be the same path.
  That covers an `audiofile` and a `videofile` in one row, and any two
  across rows. The function refuses such a table before any row runs.
  Optional `audio_codec` and `video_codec` columns override the
  arguments of the same name per row. They are character, with `NA` to
  set no codec option for that stream. Rows that omit a column fall back
  to that argument. An optional numeric `audio_stream` column likewise
  overrides the `audio_stream` argument per row. There, `NA` keeps every
  audio track in that row's `audiofile`. A numeric `quality` column
  overrides the `quality` argument per row and applies to that row's
  `videofile` (see `quality`). Any other columns are ignored, with one
  exception. A `reencode` column, retired with the argument of the same
  name, is an error and not a silent no-op.

- audio_codec:

  A string that names the encoder for every `audiofile`, unless `jobs`
  carries an `audio_codec` column. The default `"copy"` copies the audio
  stream with no quality loss. `NULL` sets no `-codec:a`. See
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md).

- video_codec:

  A string that names the encoder for every `videofile`, unless `jobs`
  carries a `video_codec` column. The default `"copy"` copies the video
  stream with no quality loss. `NULL` sets no `-codec:v`. See
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md).

- hardware, fallback:

  The encoder backend for every `videofile` and its fallback behavior.
  They apply to the whole batch. They are a property of the machine, not
  of a row, so neither is read as a `jobs` column. See
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md).
  `hardware` is batch-wide, and a stream copy runs no encoder. So a
  `hardware` other than `"none"` conflicts with any row whose video
  codec resolves to `"copy"`. That includes the default. So split a jobs
  table that mixes copied and re-encoded video into separate calls.
  Resolving a hardware backend asks this FFmpeg build which encoders it
  has. So the first such call that re-encodes the video runs FFmpeg
  while the command is built, even under `run = FALSE`. The answer is
  remembered for the rest of the R session. See
  [`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
  to discard it. This function checks that the encoder is available
  before any row runs. So an unavailable encoder aborts naming this
  function, not the internal step that runs the rows. A call can also
  contradict itself by asking for GPU encoding alongside a stream copy.
  Such a call is refused for the contradiction first, whether or not
  this machine has the encoder. The stream-copy conflict above is caught
  first, so such a call aborts without asking FFmpeg.

- quality:

  A number, or `NULL` (default), applied to every `videofile` unless
  `jobs` carries a numeric `quality` column. In that column, `NA` leaves
  that row's encoder default in place, whatever the argument says. The
  value is the encoder's own rate-control value, passed through
  unchanged. Each cell is checked against the encoder its own row
  resolves to. A wrong cell is refused before any row runs, and the
  error names this function and the row. See
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  for the encoders, their flags and ranges, and the values it refuses.
  The `audiofile` never takes it. A cell on a row whose video codec is
  `"copy"`, the default, is refused, because no encoder runs.

- audio_stream:

  The audio track to write to each `audiofile`, as a number that counts
  from `0` among the *audio tracks* of each row's input. `0` is the
  first audio track and `1` is the second. Other streams in the file,
  such as video, do not count. `NULL` (default) keeps **every** audio
  track. Without an `audio_stream` column, the argument applies to every
  row. An `NA` cell in that column means `NULL` for that row. It does
  not fall back to the argument. The every-track family reads `NULL` as
  every audio track:
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
  [`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  and
  [`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
  and their `_batch` forms. The first-track family reads it as the first
  audio track only:
  [`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and
  [`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
  and their `_batch` forms. A container that holds several audio streams
  (`.mka`, `.m4a`) gets them all. A container for one stream only
  (`.aac`, `.mp3`, `.wav`) makes FFmpeg fail, so name a track to write
  one of those. Count only the input's *audio* streams. Do not use the
  `index` column of
  [`probe_audio`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
  which counts every stream. An input with no audio at all is an FFmpeg
  error here, because this function writes an audio file. Functions that
  write only a video file, such as
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  do not fail in that case. `videofile` is never affected. A track the
  input does not have gives an FFmpeg error, not an R one. See
  [`audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md)
  for how this differs from `audio_input`, the input index on
  [`compare_videos`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  and
  [`picture_in_picture`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md).
  (default = `NULL`)

- run:

  A logical: run each command through FFmpeg (`TRUE`, default) or only
  compile them for inspection (`FALSE`).

- parallel:

  A logical: process the jobs in parallel with furrr (`TRUE`) or one at
  a time (`FALSE`, default). See
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  for the future plan requirement.

- ...:

  Additional arguments forwarded to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  (e.g. `verify`, `manifest`, `progress`).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with **two rows per input**, one per stream. It has the reshaped
`input`, a single `output` path, a `stream` marker (`"audio"` or
`"video"`), and an added `command` column. When `run = TRUE`, it also
has a `success` column. A run also gives `verified` and the provenance
manifest, each when requested via `...`. When `jobs` supplies either
codec column, a single `codec` column carries each row's resolved
encoder for its own stream (`NA` where none is set). When `audio_stream`
is supplied as either the argument or a `jobs` column, an `audio_stream`
column likewise carries each row's resolved track. That is the selected
index on an audio row. It is `NA` on every video row, which takes no
audio, and on an audio row that named no track. So `NA` does not by
itself mark a video row. Read the `stream` column for that. The columns
match the output of the other `_batch` functions, plus the `stream`
marker. See
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).

## Failed audio outputs

A row whose audio command does not finish cleanly is recorded as
`success = FALSE`, and the batch does not abort. One warning for the
whole batch, emitted **once**, names such rows. It lists every affected
input row and the ways out.

A row reaches that warning only when all four of these hold:

- It named no `audio_stream`.

- The row is recorded `success = FALSE`.

- Its input carries more than one audio track.

- The extension of its `audiofile` is not among the containers named
  here as holding several audio streams.

Those containers are `.mka`, `.m4a`, `.mp4`, `.mov`, `.mkv`, `.webm`,
`.ogg`, `.opus` and `.ts`. No exit status is among those conditions, and
the difference from
[`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
is deliberate. The batch runner records *whether* a row succeeded and
not how. So it records a non-zero exit, a hard error and a reached limit
the same way. It treats alike a row put here by any of them. The nine
are an exclusion list and not a survey. FFmpeg writes several audio
streams into other containers too, `.avi` and `.nut` among them. A row
that fails on one of those is still named. The container condition keeps
a row off the list when it already does what the warning advises. Such a
row is silently not named. A batch whose failed audio rows all write to
those nine does not warn at all. The headline count follows the rows
actually named.

Each bullet of the warning states what that row *did*: its track count,
and that every track was mapped into one output. It never states why
FFmpeg refused. Several causes look alike from here. Examples are a
stream copy into a container that will not hold the source codec, an
unknown encoder and a missing output directory.

The check runs FFprobe on the failed rows only. So the function emits
the warning when FFprobe is available and the input can be probed, and
skips it silently otherwise. The warning may not appear, and its absence
is never itself a second failure. The check never runs under
`run = FALSE` and never changes any compiled command. Suppress the
warning with
`suppressWarnings(classes = "tidymedia_multitrack_separation")`.

The warning names the same event as the error of
[`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
and answers to the same class. But it carries no exit status: no
`tm_status` field, and no `tidymedia_ffmpeg_exit` class. The batch
runner records, per row, *whether* the row succeeded, not *how* FFmpeg
exited. The `success` column holds that record. So the exit number is
gone by the time this warning is assembled. To catch a specific row's
exit status, use
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md).

## See also

[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
the one-file function it wraps.
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
the batch runner.
[`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
for the `hardware` argument.
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md)
for the other batch function where one input file can give several
outputs.

Other task functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md),
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
[`concatenate_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos_batch.md),
[`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
[`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md),
[`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`format_for_web_batch()`](https://jmgirard.github.io/tidymedia/reference/format_for_web_batch.md),
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md),
[`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md),
[`sample_frames_batch()`](https://jmgirard.github.io/tidymedia/reference/sample_frames_batch.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
[`strip_metadata()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata.md),
[`strip_metadata_batch()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata_batch.md)

Other audio selection functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
[`audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md),
[`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
[`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`format_for_web_batch()`](https://jmgirard.github.io/tidymedia/reference/format_for_web_batch.md),
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
jobs <- tibble::tibble(
  input     = c(video, video),
  audiofile = c("a1.aac", "a2.aac"),
  videofile = c("v1.mp4", "v2.mp4")
)
# run = FALSE compiles two commands per input without calling FFmpeg
separate_audio_video_batch(jobs, run = FALSE)
#> # A tibble: 4 × 4
#>   input                                                    output stream command
#>   <chr>                                                    <chr>  <chr>  <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extdata/sampl… a1.aac audio  "-y -i…
#> 2 /home/runner/work/_temp/Library/tidymedia/extdata/sampl… v1.mp4 video  "-y -i…
#> 3 /home/runner/work/_temp/Library/tidymedia/extdata/sampl… a2.aac audio  "-y -i…
#> 4 /home/runner/work/_temp/Library/tidymedia/extdata/sampl… v2.mp4 video  "-y -i…
```
