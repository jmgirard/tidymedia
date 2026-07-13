# Strip identifying metadata from a media file

Remove a media file's container and global metadata tags (creation time,
GPS/location, device make and model, title, comment, and the like)
together with any chapters, writing a de-identified copy — the front
door for IRB/de-identification of research recordings. The audio and
video streams are **stream-copied**, not re-encoded, so the operation is
lossless and fast and the picture and sound are bit-for-bit unchanged
(including any rotation display matrix, which is stream side data, not a
metadata tag).

## Usage

``` r
strip_metadata(infile, outfile, run = TRUE)
```

## Arguments

- infile:

  A string containing the path to a media file.

- outfile:

  A string containing the path of the de-identified file to write. Use
  the same container extension as `infile` so the copied streams remux
  cleanly.

- run:

  A logical: run the command through FFmpeg (`TRUE`, default) or return
  the compiled command without running it (`FALSE`).

## Value

The compiled FFmpeg command (invisibly when `run = TRUE`).

## Details

The output is muxed bit-exactly (`-fflags +bitexact`) so FFmpeg does not
re-stamp the container with a fresh `creation_time` or an `encoder` tag
naming its own version — either of which would defeat de-identification
and reproducibility.

Because the streams are copied rather than re-encoded, identifiers
embedded **inside** the encoded bitstream, and per-stream metadata such
as `handler_name` or `language`, are not removed. Removing those would
require re-encoding (out of scope; use the
[`ffmpeg`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
escape hatch) or per-stream metadata mapping that must probe the file
first.

## See also

[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
to remove faces or regions from the picture (the visual
de-identification sibling);
[`probe_container()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)
and
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md)
to inspect a file's metadata before and after;
[`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
and
[`ffm_output_options()`](https://jmgirard.github.io/tidymedia/reference/ffm_output_options.md),
the builders it wraps;
[`strip_metadata_batch()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata_batch.md)
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
[`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md),
[`sample_frames_batch()`](https://jmgirard.github.io/tidymedia/reference/sample_frames_batch.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
[`strip_metadata_batch()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
strip_metadata(video, "clean.mp4", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:v copy -codec:a copy -map_metadata -1 -map_chapters -1 -fflags +bitexact -map 0 \"clean.mp4\""
```
