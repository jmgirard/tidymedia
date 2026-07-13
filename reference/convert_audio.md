# Extract or convert a media file's audio track

Maps the audio stream of `infile` into `outfile`. By default
(`format = NULL`) the output format follows the `outfile` file extension
at highest VBR quality (`-q:a 0`) — e.g. an `.mp3` extension yields an
MP3. Pass `format` to pin the output audio codec explicitly, regardless
of the extension.

## Usage

``` r
convert_audio(infile, outfile, format = NULL, run = TRUE)
```

## Arguments

- infile:

  A string containing the path to a media file.

- outfile:

  A string containing the path of the audio file to write.

- format:

  An optional string naming the output audio codec (e.g. `"libmp3lame"`,
  `"aac"`, `"flac"`), passed to FFmpeg's `-c:a`. When `NULL` (default),
  the format is inferred from the `outfile` extension.

- run:

  A logical: run the command through FFmpeg (`TRUE`, default) or return
  the compiled command without running it (`FALSE`).

## Value

The compiled FFmpeg command (invisibly when `run = TRUE`).

## See also

[`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md)
and
[`ffm_map()`](https://jmgirard.github.io/tidymedia/reference/ffm_map.md),
the builders it wraps;
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
to copy audio without re-encoding.

Other task verb functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
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
convert_audio(video, "audio.mp3", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -q:a 0 -map a \"audio.mp3\""
convert_audio(video, "audio.m4a", format = "aac", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:a aac -map a \"audio.m4a\""
```
