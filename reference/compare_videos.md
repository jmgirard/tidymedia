# Build a side-by-side comparison video

Stack two or more videos into a single comparison video — side-by-side
(`direction = "horizontal"`) or one above the other
(`direction = "vertical"`) — a common need when reviewing annotations or
before/after processing. Built on the blessed stacking verbs
([`ffm_hstack`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md)
/
[`ffm_vstack`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md)).

## Usage

``` r
compare_videos(
  infiles,
  outfile,
  direction = c("horizontal", "vertical"),
  resize = TRUE,
  audio = NULL,
  run = TRUE
)
```

## Arguments

- infiles:

  A character vector of two or more video file paths.

- outfile:

  A string giving the path to write the comparison video to.

- direction:

  Either `"horizontal"` (side-by-side, the default) or `"vertical"`
  (stacked top to bottom).

- resize:

  A logical indicating whether to resize the inputs to share an edge.
  Only supported for exactly two inputs. (default = `TRUE`)

- audio:

  The 0-based index of the input whose audio to keep in the output, or
  `NULL` to drop audio entirely. (default = `NULL`)

- run:

  A logical: run the command through FFmpeg (`TRUE`, default) or return
  the compiled command without running it (`FALSE`).

## Value

The compiled FFmpeg command (invisibly when `run = TRUE`).

## Details

By default the two inputs are resized to share an edge (equal heights
for a horizontal stack, equal widths for a vertical one); resizing
currently supports exactly two inputs, so pass `resize = FALSE` to
compare more. Audio is dropped unless `audio` names an input to carry.

## See also

[`ffm_hstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md)
and
[`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md),
the builders it wraps;
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
for insetting instead of stacking.

Other task verb functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
[`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md),
[`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md),
[`sample_frames_batch()`](https://jmgirard.github.io/tidymedia/reference/sample_frames_batch.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
[`strip_metadata()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata.md),
[`strip_metadata_batch()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
compare_videos(c(video, video), "compare.mp4", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -filter_complex \"[0:v][1:v]scale2ref='oh*mdar':'if(lt(main_h,ih),ih,main_h)'[0s][1s];[1s][0s]scale2ref='oh*mdar':'if(lt(main_h,ih),ih,main_h)'[1s][0s];[0s][1s]hstack,setsar=1[vout]\" -map \"[vout]\" \"compare.mp4\""
```
