# Extract a single frame from a video

Save one frame of a video to an image file, selected either by timestamp
or by frame number. Provide exactly one of `timestamp` or `frame`.

## Usage

``` r
extract_frame(infile, outfile, timestamp = NULL, frame = NULL, run = TRUE)
```

## Arguments

- infile:

  A string containing the path to a video file.

- outfile:

  A string containing the path of the image file to write.

- timestamp:

  Either a number of seconds, a time-duration-syntax string, or `NULL`.
  Provide exactly one of `timestamp` or `frame`.

- frame:

  Either an integerish frame number or `NULL`. Provide exactly one of
  `timestamp` or `frame`.

- run:

  A logical: run the command through FFmpeg (`TRUE`, default) or return
  the compiled command without running it (`FALSE`).

## Value

The compiled FFmpeg command (invisibly when `run = TRUE`).

## See also

Other task verb functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
[`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
# run = FALSE returns the reproducible command instead of executing it
extract_frame(video, "frame.png", timestamp = 0.5, run = FALSE)
#> [1] "-y -ss 0.5 -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -avoid_negative_ts make_zero -qmin 1 -q:v 1 -qscale:v 2 -frames:v 1 -huffman optimal \"frame.png\""
```
