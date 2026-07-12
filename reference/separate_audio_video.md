# Split a media file into separate audio and video files

By default the streams are copied, not re-encoded (`reencode = FALSE`):
separation is lossless and fast, but each output container must support
the source codec (e.g. write AAC audio from an MP4 to `.aac` or `.m4a`,
not `.mp3`). Set `reencode = TRUE` to let FFmpeg re-encode each stream
to whatever the output extension implies.

## Usage

``` r
separate_audio_video(
  infile,
  audiofile,
  videofile,
  reencode = FALSE,
  run = TRUE
)
```

## Arguments

- infile:

  A string containing the path to a media file.

- audiofile:

  A string containing the path of the audio file to write.

- videofile:

  A string containing the path of the video file to write.

- reencode:

  A logical: stream-copy the audio and video losslessly (`FALSE`,
  default) or re-encode them to match the output extensions (`TRUE`).

- run:

  A logical: run the commands through FFmpeg (`TRUE`, default) or return
  the compiled commands without running them (`FALSE`).

## Value

A named character vector of the two compiled commands (`audio`,
`video`); invisible when `run = TRUE`.

## See also

Other task verb functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`audio_as_mp3()`](https://jmgirard.github.io/tidymedia/reference/audio_as_mp3.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md),
[`extract_frames()`](https://jmgirard.github.io/tidymedia/reference/extract_frames.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
[`normalize_audios()`](https://jmgirard.github.io/tidymedia/reference/normalize_audios.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_videos()`](https://jmgirard.github.io/tidymedia/reference/segment_videos.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_videos()`](https://jmgirard.github.io/tidymedia/reference/standardize_videos.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
separate_audio_video(video, "audio.aac", "video.mp4", run = FALSE)
#>                                                                                                         audio 
#> "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:a copy -map 0:a \"audio.aac\"" 
#>                                                                                                         video 
#> "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:v copy -map 0:v \"video.mp4\"" 
```
