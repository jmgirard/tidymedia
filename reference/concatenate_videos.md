# Combine video files using the concat demuxer

Combine multiple video files one after another without needing to
re-encode them by using the [concat
demuxer](https://ffmpeg.org/ffmpeg-formats.html#concat-1). This will be
much faster than re-encoding but requires that the files have the same
parameters (width, height, etc.) and formats/codecs. To concatenate
videos using re-encoding, see the [concat video
filter](https://ffmpeg.org/ffmpeg-filters.html#concat)

## Usage

``` r
concatenate_videos(infiles, outfile, run = TRUE)
```

## Arguments

- infiles:

  A character vector containing the file paths to video files.

- outfile:

  A string containing the desired file path to write the new,
  concatenated video file to.

- run:

  A logical: run the command through FFmpeg (`TRUE`, default) or return
  the compiled command without running it (`FALSE`).

## Value

The compiled FFmpeg command (invisibly when `run = TRUE`).

## See also

Other task verb functions:
[`audio_as_mp3()`](https://jmgirard.github.io/tidymedia/reference/audio_as_mp3.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md),
[`extract_frames()`](https://jmgirard.github.io/tidymedia/reference/extract_frames.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_videos()`](https://jmgirard.github.io/tidymedia/reference/segment_videos.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_videos()`](https://jmgirard.github.io/tidymedia/reference/standardize_videos.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
concatenate_videos(c(video, video), "joined.mp4", run = FALSE)
#> [1] "-y -f concat -safe 0 -i \"/tmp/RtmpgmjFuI/ffm-concat1f80649f7e1a.txt\" -codec:v copy -codec:a copy -map 0 \"joined.mp4\""
```
