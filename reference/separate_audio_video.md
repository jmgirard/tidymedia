# Split a media file into separate audio and video files

Split a media file into separate audio and video files

## Usage

``` r
separate_audio_video(infile, audiofile, videofile, run = TRUE)
```

## Arguments

- infile:

  A string containing the path to a media file.

- audiofile:

  A string containing the path of the audio file to write.

- videofile:

  A string containing the path of the video file to write.

- run:

  A logical: run the commands through FFmpeg (`TRUE`, default) or return
  the compiled commands without running them (`FALSE`).

## Value

A named character vector of the two compiled commands (`audio`,
`video`); invisible when `run = TRUE`.

## See also

Other task verb functions:
[`audio_as_mp3()`](https://jmgirard.github.io/tidymedia/reference/audio_as_mp3.md),
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
separate_audio_video(video, "audio.aac", "video.mp4", run = FALSE)
#>                                                                                           audio 
#> "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -map 0:a \"audio.aac\"" 
#>                                                                                           video 
#> "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -map 0:v \"video.mp4\"" 
```
