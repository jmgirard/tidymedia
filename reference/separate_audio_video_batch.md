# Separate Audio and Video for Many Files From a Jobs Table

Split the audio and video streams of many input files from a single jobs
tibble — the **batch** (table-driven) sibling of
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
for when you have more than one file. Each row is one input that fans
out into **two** outputs; `input`, `audiofile`, and `videofile` columns
are all required. This is a thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
every input row is reshaped into two single-output jobs (one per
stream), so a jobs table of `N` rows returns `2N` rows — one
reproducible compiled command per stream — sharing the same per-stream
map/stream-copy pipeline as the scalar verb.

## Usage

``` r
separate_audio_video_batch(
  jobs,
  audio_codec = "copy",
  video_codec = "copy",
  hardware = c("none", "nvenc", "videotoolbox"),
  fallback = FALSE,
  audio_stream = NULL,
  run = TRUE,
  parallel = FALSE,
  ...
)
```

## Arguments

- jobs:

  A data frame with one row per input and (at least) an `input` column
  (source path) plus `audiofile` and `videofile` columns naming the two
  destinations. All three are **required** — like
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  this verb derives no output paths, because a copied stream's container
  extension is the instruction (it must match the source codec).
  Optional `audio_codec` and `video_codec` columns (character; `NA` to
  emit no codec option for that stream) override the arguments of the
  same name per row; rows omitting a column fall back to that argument.
  An optional numeric `audio_stream` column (`NA` to keep every audio
  track in that row's `audiofile`) likewise overrides the `audio_stream`
  argument per row. Any other columns are ignored — except a `reencode`
  column, retired with the argument of the same name, which is an error
  rather than a silent no-op.

- audio_codec:

  A string naming the encoder for every `audiofile` unless `jobs`
  carries an `audio_codec` column. The default `"copy"` stream-copies
  the audio losslessly; `NULL` emits no `-codec:a`. See
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md).

- video_codec:

  A string naming the encoder for every `videofile` unless `jobs`
  carries a `video_codec` column. The default `"copy"` stream-copies the
  video losslessly; `NULL` emits no `-codec:v`. See
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md).

- hardware, fallback:

  The encoder backend for every `videofile` and its fallback behavior,
  applied to the whole batch (a property of the machine, not of a row,
  so neither is read as a `jobs` column). See
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md).
  Because `hardware` is batch-wide, and a stream copy runs no encoder, a
  non-`"none"` `hardware` conflicts with any row whose video codec
  resolves to `"copy"` — including the default — so a jobs table mixing
  copied and re-encoded video must be split into separate calls.
  Resolving a hardware backend asks this FFmpeg build which encoders it
  has, so the first such call that re-encodes the video runs the binary
  while the command is built, even under `run = FALSE`. The answer is
  remembered for the rest of the R session; see
  [`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
  to discard it. Availability is checked at this verb's own front door,
  before any row runs, so an unavailable encoder aborts naming this
  function rather than the internal fan-out it would otherwise be
  reported against. A call that also contradicts itself — asking for GPU
  encoding alongside a stream copy — is refused for the contradiction
  first, whether or not this machine has the encoder. The stream-copy
  conflict above is caught first, so such a call aborts without probing.

- audio_stream:

  The 0-based index of the audio track to write to each `audiofile`,
  counted *among that row's input's audio streams* – `0` is the first
  audio track, `1` the second, whatever their positions among the file's
  streams. `NULL` (default) keeps **every** audio track. The argument
  applies to every row lacking an `audio_stream` column; an `NA` cell in
  that column means the same as `NULL` for that row, rather than falling
  back to the argument. The every-track family reads `NULL` this way –
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
  [`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  and
  [`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
  plus their `_batch` siblings. The first-track family takes one track
  only:
  [`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and
  [`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
  plus theirs. A container that holds several audio streams (`.mka`,
  `.m4a`) receives them all, while a single-stream container (`.aac`,
  `.mp3`, `.wav`) makes FFmpeg fail – name a track to write one of
  those. Count among the input's *audio* streams, not the `index` column
  of
  [`probe_audio`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
  which counts every stream. Unlike the verbs that pass video through,
  an input carrying no audio at all is an FFmpeg error here, because
  this verb's product is the audio file. `videofile` is never affected.
  Naming a track the input does not have is an FFmpeg error, not an R
  one. See
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

  A logical: map over jobs in parallel with furrr (`TRUE`) or
  sequentially (`FALSE`, default). See
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  for the future plan requirement.

- ...:

  Additional arguments forwarded to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  (e.g. `verify`, `manifest`, `progress`).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with **two rows per input** (one per stream): the reshaped `input`, a
single `output` path, a `stream` marker (`"audio"` or `"video"`), and an
added `command` column — plus, when `run = TRUE`, a `success` column
(and `verified` / provenance manifest when requested via `...`). When
`jobs` supplies either codec column, a single `codec` column carries
each row's resolved encoder for its own stream (`NA` where none is
emitted). When `audio_stream` is supplied as either the argument or a
`jobs` column, an `audio_stream` column likewise carries each row's
resolved track: the selected index on an audio row, and `NA` both on
every video row (which takes no audio) and on an audio row that named no
track — so `NA` does not by itself mark a video row; read the `stream`
column for that. The columns match the other `_batch` verbs' output plus
the `stream` marker. See
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).

## Failed audio outputs

A row whose audio command does not finish cleanly is recorded as
`success = FALSE` rather than aborting the batch. Such a row is named in
a warning emitted **once** for the whole batch, listing every affected
input row and the ways out.

A row reaches that warning only when all four of these hold: it named no
`audio_stream`, the row is recorded `success = FALSE`, its input carries
more than one audio track, and its `audiofile`'s extension is not among
the containers named here as holding several — `.mka`, `.m4a`, `.mp4`,
`.mov`, `.mkv`, `.webm`, `.ogg`, `.opus` and `.ts`. No exit status is
among those conditions, and the difference from
[`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
is deliberate: the batch runner records *whether* a row succeeded and
not how, so a non-zero exit, a hard error and a reached limit are all
recorded the same way, and a row put here by any of them is treated
alike. The nine are an exclusion list and not a survey: FFmpeg writes
several audio streams into other containers too (`.avi` and `.nut` among
them), and a row failing on one of those is still named. The container
condition keeps a row off the list when it is already doing what the
warning would advise; such a row is silently not named, and a batch
whose failed audio rows all write to those nine warns not at all. The
headline count follows the rows actually named.

What each bullet states is what that row *did* — its track count, and
that every track was mapped into one output — never why FFmpeg refused.
A stream copy into a container that will not hold the source codec, an
unknown encoder and a missing output directory all look alike from here.

The check runs FFprobe on the failed rows only, so it is emitted when
FFprobe is available and the input can be probed, and skipped silently
otherwise — so the warning may simply not appear, and its absence is
never itself a second failure. It never runs under `run = FALSE` and
never changes any compiled command. Suppress it with
`suppressWarnings(classes = "tidymedia_multitrack_separation")`.

The warning names the same event as
[`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)'s
error and answers to the same class, but it carries no exit status: no
`tm_status` field, and no `tidymedia_ffmpeg_exit` class. The batch
runner records, per row, *whether* the row succeeded — the `success`
column — not *how* FFmpeg exited, so by the time this warning is
assembled the exit number is gone. Catch a specific row's exit status
with the scalar verb instead.

## See also

[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
the scalar verb it wraps;
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
the batch runner;
[`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
for the `hardware` toggle;
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md)
for the other fan-out batch verb.

Other task verb functions:
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
