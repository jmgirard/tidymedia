# Inset one video over another (picture-in-picture)

Composite a smaller `overlay` video onto a `main` video in one corner
(or the center) — the classic picture-in-picture layout for pairing a
speaker with a screen recording, or a stimulus with a webcam. Built on
the blessed
[`ffm_overlay`](https://jmgirard.github.io/tidymedia/reference/ffm_overlay.md)
verb, which resizes the overlay to a fraction of the main video's width
and positions it.

## Usage

``` r
picture_in_picture(
  main,
  overlay,
  outfile,
  position = c("topright", "topleft", "bottomright", "bottomleft", "center"),
  scale = 0.25,
  margin = 16,
  audio = NULL,
  run = TRUE
)
```

## Arguments

- main:

  A string giving the path to the background (full-size) video.

- overlay:

  A string giving the path to the inset video.

- outfile:

  A string giving the path to write the result to.

- position:

  Where to place the inset: one of `"topright"` (default), `"topleft"`,
  `"bottomright"`, `"bottomleft"`, or `"center"`.

- scale:

  The inset's width as a fraction of the main video's width, aspect
  preserved (`0 < scale <= 1`). (default = `0.25`)

- margin:

  The gap in pixels between the inset and the video edges (ignored for
  `position = "center"`). (default = `16`)

- audio:

  The 0-based index of the input whose audio to keep, or `NULL` to drop
  audio. (default = `NULL`)

- run:

  A logical: run the command through FFmpeg (`TRUE`, default) or return
  the compiled command without running it (`FALSE`).

## Value

The compiled FFmpeg command (invisibly when `run = TRUE`).

## Details

Audio is dropped unless `audio` names an input to carry (`0` = the main
video, `1` = the overlay).

## See also

Other task verb functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`anonymize_videos()`](https://jmgirard.github.io/tidymedia/reference/anonymize_videos.md),
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
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_videos()`](https://jmgirard.github.io/tidymedia/reference/segment_videos.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_videos()`](https://jmgirard.github.io/tidymedia/reference/standardize_videos.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
picture_in_picture(video, video, "pip.mp4", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -filter_complex \"[1:v][0:v]scale2ref=w='main_w*0.25':h='main_w*0.25*ih/iw'[pip][bg];[bg][pip]overlay=x=main_w-overlay_w-16:y=16:shortest=0[vout]\" -map \"[vout]\" \"pip.mp4\""
```
