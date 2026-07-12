# Extract the audio stream from a media file

Extract the audio stream from a media file

## Usage

``` r
extract_audio(infile, outfile, acodec = "copy", run = TRUE)
```

## Arguments

- infile:

  A string containing the path to a media file.

- outfile:

  A string containing the path of the audio file to write.

- acodec:

  A string naming the audio codec for the output stream. (default =
  `"copy"`, i.e. remux without re-encoding)

- run:

  A logical: run the command through FFmpeg (`TRUE`, default) or return
  the compiled command without running it (`FALSE`).

## Value

The compiled FFmpeg command (invisibly when `run = TRUE`).

## See also

Other task verb functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`anonymize_videos()`](https://jmgirard.github.io/tidymedia/reference/anonymize_videos.md),
[`audio_as_mp3()`](https://jmgirard.github.io/tidymedia/reference/audio_as_mp3.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md),
[`extract_frames()`](https://jmgirard.github.io/tidymedia/reference/extract_frames.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
[`normalize_audios()`](https://jmgirard.github.io/tidymedia/reference/normalize_audios.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_videos()`](https://jmgirard.github.io/tidymedia/reference/segment_videos.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_videos()`](https://jmgirard.github.io/tidymedia/reference/standardize_videos.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
extract_audio(video, "audio.aac", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:a copy -vn \"audio.aac\""
```
