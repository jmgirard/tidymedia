# Extract or convert a media file's audio track

Maps the audio stream of `infile` into `outfile`. By default
(`audio_codec = NULL`) the output format follows the `outfile` file
extension at highest VBR quality (`-q:a 0`) — e.g. an `.mp3` extension
yields an MP3. Pass `audio_codec` to pin the output audio codec
explicitly, regardless of the extension.

## Usage

``` r
convert_audio(
  infile,
  outfile,
  audio_codec = NULL,
  audio_stream = NULL,
  run = TRUE
)
```

## Arguments

- infile:

  A string containing the path to a media file.

- outfile:

  A string containing the path of the audio file to write.

- audio_codec:

  An optional string naming the output audio codec (e.g. `"libmp3lame"`,
  `"aac"`, `"flac"`), passed to FFmpeg's `-c:a`. When `NULL` (default),
  the codec is inferred from the `outfile` extension and encoded at
  highest VBR quality. Unlike the other transform verbs, `NULL` here is
  *not* the "leave the codec unset" sentinel — it selects `-q:a 0`.

- audio_stream:

  The 0-based index of the audio track to take, counted *among the
  input's audio streams* — `0` is the first audio track, `1` the second,
  whatever their positions among the file's streams. `NULL` (default)
  takes the first audio track. Naming a track the input does not have is
  an FFmpeg error, not an R one.

- run:

  A logical: run the command through FFmpeg (`TRUE`, default) or return
  the compiled command without running it (`FALSE`).

## Value

The compiled FFmpeg command (invisibly when `run = TRUE`).

## Details

When `infile` carries more than one audio track, `audio_stream` names
which one to take; with no selector the **first** one is taken.

## See also

[`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md)
and
[`ffm_map()`](https://jmgirard.github.io/tidymedia/reference/ffm_map.md),
the builders it wraps;
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
to copy audio without re-encoding;
[`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md)
for the many-file form.

Other task verb functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md),
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
[`concatenate_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos_batch.md),
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
[`strip_metadata()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata.md),
[`strip_metadata_batch()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
convert_audio(video, "audio.mp3", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -q:a 0 -map 0:a:0 \"audio.mp3\""
convert_audio(video, "audio.m4a", audio_codec = "aac", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:a aac -map 0:a:0 \"audio.m4a\""
# Convert the second audio track instead of the first
convert_audio(video, "audio.mp3", audio_stream = 1, run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -q:a 0 -map 0:a:1 \"audio.mp3\""
```
