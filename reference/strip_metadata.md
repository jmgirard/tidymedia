# Strip identifying metadata from a media file

Remove a media file's container and global metadata tags, together with
any chapters, and write a de-identified copy. The tags include creation
time, GPS or other location, device make and model, title and comment.
Use it to de-identify research recordings, for example for an IRB (a
research ethics board). The audio and video streams are
**stream-copied**, not re-encoded. So the operation is lossless and
fast, and the picture and sound are bit-for-bit unchanged. That includes
any rotation display matrix, which is stream side data and not a
metadata tag. The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as container, stream and stream copy.

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

The output is written bit-exactly (`-fflags +bitexact`). So FFmpeg does
not stamp the container again with a fresh `creation_time`, or with an
`encoder` tag that names its own version. Either tag would defeat
de-identification and reproducibility.

Because the streams are copied and not re-encoded, some data is not
removed. Those are identifiers **inside** the encoded bitstream, and
per-stream metadata such as `handler_name` or `language`. Removing them
would require one of two things. One is re-encoding, which is out of
scope for this function, so use the direct command
[`ffmpeg`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md) for
it. The other is per-stream metadata mapping that must probe the file
first.

## See also

[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
removes faces or regions from the picture, for visual de-identification.
[`probe_container()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)
and
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md)
inspect a file's metadata before and after. This function wraps the
pipeline functions
[`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
and
[`ffm_output_options()`](https://jmgirard.github.io/tidymedia/reference/ffm_output_options.md).
[`strip_metadata_batch()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata_batch.md)
is the many-file form.

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
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
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
[`strip_metadata_batch()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
strip_metadata(video, "clean.mp4", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:v copy -codec:a copy -map_metadata -1 -map_chapters -1 -fflags +bitexact -map \"0\" \"clean.mp4\""
```
