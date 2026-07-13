# Sample frames from a video at a fixed rate

Sample a video at a fixed rate (`fps`) or interval (`interval`, seconds
between frames) into a numbered image sequence — the front door to
per-frame coding and computer-vision feature pipelines. Provide exactly
one of `fps` or `interval`.

## Usage

``` r
sample_frames(
  infile,
  outdir,
  fps = NULL,
  interval = NULL,
  format = "png",
  prefix = NULL,
  run = TRUE
)
```

## Arguments

- infile:

  A string containing the path to a video file.

- outdir:

  A string naming the directory to write the image sequence to. It is
  created (recursively) if it does not exist.

- fps:

  The sampling rate, in frames per second: either a positive number or
  an FFmpeg framerate expression string (for example `"30000/1001"`).
  Provide exactly one of `fps` or `interval`.

- interval:

  The number of seconds between sampled frames (a positive number); the
  reciprocal is used as the frame rate. Provide exactly one of `fps` or
  `interval`.

- format:

  A string giving the output image file extension (one of `"png"`,
  `"jpg"`, `"jpeg"`, `"bmp"`, `"tif"`, `"tiff"`, `"webp"`). (default =
  `"png"`)

- prefix:

  A string used as the basename stem of each image, or `NULL` to derive
  it from `infile`'s basename. (default = `NULL`)

- run:

  A logical: run the command through FFmpeg (`TRUE`, default) or return
  the compiled command without running it (`FALSE`).

## Value

The compiled FFmpeg command (invisibly when `run = TRUE`).

## Details

Unlike
[`extract_frame`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md)
(one frame) and
[`extract_frame_batch`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md)
(a caller-enumerated set of frames), this verb emits a *single* FFmpeg
command whose output is a printf-style pattern that FFmpeg's `image2`
muxer fills — the frame count is decided at decode time, not enumerated
by the caller. Frames are written to `outdir` as
`<prefix>_<n>.<format>`, where `<n>` is a zero-padded integer starting
at 1.

## See also

[`ffm_fps()`](https://jmgirard.github.io/tidymedia/reference/ffm_fps.md),
the builder it uses to set the sampling rate;
[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md)
for a single frame and
[`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md)
for a caller-enumerated set;
[`sample_frames_batch()`](https://jmgirard.github.io/tidymedia/reference/sample_frames_batch.md)
for the many-file form.

Other task verb functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
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
[`sample_frames_batch()`](https://jmgirard.github.io/tidymedia/reference/sample_frames_batch.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
[`strip_metadata()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata.md),
[`strip_metadata_batch()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
# run = FALSE returns the reproducible command instead of executing it
sample_frames(video, tempdir(), fps = 2, run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"fps=2\" -qscale:v 2 \"/tmp/RtmpVCFvJz/sample_%06d.png\""
```
