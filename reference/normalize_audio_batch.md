# Normalize Many Files' Audio Loudness From a Jobs Table

Normalize the audio loudness of many input files (EBU R128) from a
single jobs tibble. This is the **batch** (table-driven) form of
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
for when you have more than one file to normalize. Each row is one
input, and the only required column names its source. The function is a
thin wrapper over
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).
It gives one reproducible compiled command per input. Each row uses the
same `loudnorm` pipeline, and the same check of each value, as
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md).
Set `two_pass = TRUE` for accurate measured/linear normalization across
the whole table (see `two_pass`). The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as LUFS, encoder and sample rate.

## Usage

``` r
normalize_audio_batch(
  jobs,
  target_loudness = -23,
  true_peak = -1,
  loudness_range = 7,
  channels = NULL,
  sample_rate = NULL,
  audio_codec = NULL,
  two_pass = FALSE,
  audio_stream = NULL,
  run = TRUE,
  parallel = FALSE,
  ...
)
```

## Arguments

- jobs:

  A data frame with one row per input and (at least) an `input` column
  (source path). An optional `output` column names the destination.
  Without it, the function derives one per row. It appends `_normalized`
  to the basename of each input and keeps the extension of the input
  (e.g. `clip.mkv` becomes `clip_normalized.mkv`). The derived name
  keeps a *video* extension while the file itself holds audio only. So
  name an `output` column yourself when that matters. The function
  refuses two rows that name the same output path before any row runs.
  With `two_pass = TRUE`, that is before the analysis pass. The refusal
  covers a path repeated in the `output` column, or a repeated `input`
  when there is no `output` column. Five loudness arguments can also
  appear as a column that overrides the argument per row. They are
  `target_loudness`, `true_peak`, `loudness_range`, `channels` and
  `sample_rate`. Rows that omit the column fall back to the value of the
  argument. An optional `audio_codec` column (character) names the
  output audio encoder of each row. There, `NA` means "leave the encoder
  unset". Rows that omit it fall back to the `audio_codec` argument. An
  optional numeric `audio_stream` column likewise overrides the
  `audio_stream` argument per row. There, `NA` normalizes the first
  audio track of that row. Any other columns are ignored.

- target_loudness, true_peak, loudness_range:

  The EBU R128 loudness targets applied to every row, unless `jobs`
  carries a column of the same name (see `jobs`). Defaults follow EBU
  Recommendation R 128 (2014): `target_loudness = -23` LUFS,
  `true_peak = -1` dBTP, `loudness_range = 7` LU.

- channels:

  The output channel count applied to every row, unless `jobs` carries a
  `channels` column, e.g. `1` to downmix to mono. `NULL` (default) keeps
  each source's channel layout.

- sample_rate:

  The output sample rate in Hz applied to every row, unless `jobs`
  carries a `sample_rate` column. `NULL` (default) lets `loudnorm`
  choose. It resamples, up to 192 kHz and capped by the encoder, and
  does not keep the source rate. Set this argument to fix the output
  rate.

- audio_codec:

  The output audio encoder applied to every row, unless `jobs` carries
  an `audio_codec` column, e.g. `"aac"`. `NULL` (default) sets no
  `-codec:a`, which leaves the default encoder of the output container
  in place. `"copy"` is an error. Loudness normalization filters the
  audio, so it must be re-encoded. See
  [`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md).

- two_pass:

  A logical that selects the normalization mode for *every* row. It
  applies to the whole table and is not a per-row column. `FALSE`
  (default) keeps the single-pass `loudnorm` pipeline. `TRUE` runs
  accurate two-pass (measured/linear) normalization in two phases. An
  *analysis pass* first measures the loudness of every input. It honors
  `parallel` and the targets of each row. A *correction pass* then feeds
  those measurements back with `linear=true`, so each output hits its
  EBU R128 target precisely. This is the table-wide form of `two_pass`
  in
  [`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md).
  The result shows the five measured values as columns `measured_I`,
  `measured_TP`, `measured_LRA`, `measured_thresh` and `offset`.
  Two-pass must measure each input. So it **always runs the analysis
  pass through FFmpeg**, even when `run = FALSE`. It needs the binary
  and readable inputs. If the analysis of any row fails or gives no
  measurement that can be parsed, the call aborts and names those rows.
  It aborts before it builds any correction command. That abort has
  class `tidymedia_loudnorm_no_measurement`, the same class that
  [`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  raises for this event. It carries the same row numbers on `tm_rows`,
  alongside `tm_row_status`. That field has the FFmpeg exit status of
  each row, or `NA` where the row exited zero but printed nothing that
  can be parsed. It carries no single exit status on `tm_status`, and it
  does not have class `tidymedia_ffmpeg_exit`. The reason is that it
  also fires for rows that exited zero. A batch can mix causes, so there
  is no one number to report. The one-file form carries both only where
  FFmpeg exited non-zero. Where FFmpeg exited zero and printed nothing
  that can be parsed, the one-file abort carries the shared class alone,
  with no `tm_status` either. **Silent** rows are the exception. A
  silent input (analysis loudness `-inf`) cannot be normalized to a
  target, but one silent row does not abort the batch. The function
  normalizes the rows that are not silent. It marks the silent rows in a
  logical `silent` column, with `success = FALSE` and no output written,
  and a warning names them. This is where the batch form and the
  one-file form differ.
  [`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  aborts on a silent input, because one silent input is the whole call.
  Here, the other rows still have work to do. The single-pass default
  touches no binary under `run = FALSE`.

- audio_stream:

  The audio track to normalize, as a number that counts from `0` among
  the *audio tracks* of each row's input. `0` is the first audio track
  and `1` is the second. Other streams in the file, such as video, do
  not count. `NULL` (default) normalizes the **first** audio track.
  Without an `audio_stream` column, the argument applies to every row.
  An `NA` cell in that column means `NULL` for that row. It does not
  fall back to the argument. The first-track family reads `NULL` as the
  first audio track only:
  [`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and
  [`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
  and their `_batch` forms. The every-track family reads it as every
  audio track:
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
  [`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  and
  [`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
  and their `_batch` forms. This function reads `NULL` as the first
  track only. The two-pass analysis measures each audio track, but the
  correction uses one set of measurements. Normalizing several tracks at
  once would apply one track's measurements to all of them. Under
  `two_pass = TRUE`, the analysis pass measures this same track. Only
  the named track reaches the output, and no video does, whatever the
  container. So an output name with a video extension gives a video file
  that holds only audio. An input with no audio at all is an FFmpeg
  error. A track the input does not have gives an FFmpeg error, not an R
  one. See
  [`audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md)
  for how this differs from `audio_input`, the input index on
  [`compare_videos`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  and
  [`picture_in_picture`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md).
  (default = `NULL`)

- run:

  A logical: run each input's command through FFmpeg (`TRUE`, default)
  or only compile them for inspection (`FALSE`). Under `two_pass = TRUE`
  this gates only the correction pass; the analysis pass runs regardless
  (see `two_pass`).

- parallel:

  A logical passed to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
  normalize in parallel with furrr (`TRUE`) or sequentially (`FALSE`,
  default). Parallelism follows the active
  [`future`](https://future.futureverse.org/reference/plan.html) plan.
  `TRUE` under the default sequential plan runs one input at a time and
  warns. Set a plan first, e.g. `future::plan(future::multisession)`.

- ...:

  Additional arguments forwarded to
  [`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  such as `verify`, `manifest`, `checksums`, and `progress`.

## Value

The [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
returned by
[`ffm_batch`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
`jobs` with an added `command` column. When `output` was derived, it
also has the resolved `output` column. When `run = TRUE`, it has a
`success` column, plus any columns the forwarded arguments add, such as
`verified`. Under `two_pass = TRUE`, the result also carries the five
measured columns (`measured_I` etc.) and a logical `silent` column. The
`command` column then holds the linear correction commands. It is `NA`
for silent rows, which carry `NA` measurements and are not normalized.
The columns of the two-pass result do not depend on how many rows are
silent. The `verified` column (under `verify`) and the provenance
manifest (under `manifest`, read with
[`ffm_manifest`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md))
are present whenever requested. That holds even when *every* row is
silent. Silent rows carry `NA` for those outputs.

## Details

The function warns **once** for the whole batch when a row names no
`audio_stream` and its input carries tracks that the output will not.
The warning names every affected row. Naming a track silences it. Use
the `audio_stream` argument, or an `audio_stream` cell on every row.
`suppressWarnings(classes = "tidymedia_dropped_audio")` silences it too.
The check costs **one FFprobe call per distinct input** it has to probe.
A repeated input is probed once, and a row that names a track is not
probed at all. The warning is given when FFprobe is available and the
input can be probed. Otherwise the check is skipped silently. Those
probes run **one at a time, before any row starts**, so `parallel` does
not reach them. A sweep long enough to look like a hang reports its
progress. The check never runs under `run = FALSE` and never changes any
compiled command. It is skipped entirely when every row names a track.
Under `two_pass = TRUE`, the warning comes *before* the analysis pass.
So it arrives while adding `audio_stream` can still save that pass.

To switch the check off and skip the whole sweep, use
`options(tidymedia.check_tracks = FALSE)` for the session. Use
`withr::local_options(tidymedia.check_tracks = FALSE)` for the rest of
one function.

## References

EBU Recommendation R 128 (2014), *Loudness normalisation and permitted
maximum level of audio signals*; ITU-R BS.1770-4.

## See also

[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
for the single-input form.
[`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
for the batch runner and the arguments forwarded through `...`.
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
for the table-driven form on the video side.

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
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md),
[`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md),
[`sample_frames_batch()`](https://jmgirard.github.io/tidymedia/reference/sample_frames_batch.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
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
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
jobs <- tibble::tibble(
  input           = c(video, video),
  output          = c("a.mp4", "b.mp4"),
  target_loudness = c(-23, -16)
)
# run = FALSE compiles one command per input without calling FFmpeg
normalize_audio_batch(jobs, run = FALSE)
#> # A tibble: 2 × 4
#>   input                                           output target_loudness command
#>   <chr>                                           <chr>            <dbl> <chr>  
#> 1 /home/runner/work/_temp/Library/tidymedia/extd… a.mp4              -23 "-y -i…
#> 2 /home/runner/work/_temp/Library/tidymedia/extd… b.mp4              -16 "-y -i…
# Accurate two-pass (measured/linear) normalization across the whole table
# (runs FFmpeg to measure each input, so needs the binary):
if (FALSE) { # \dontrun{
normalize_audio_batch(jobs, two_pass = TRUE)
} # }
```
