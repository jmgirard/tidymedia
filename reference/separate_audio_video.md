# Split a media file into separate audio and video files

By default each stream is copied, not re-encoded
(`audio_codec = "copy"`, `video_codec = "copy"`): separation is lossless
and fast, but each output container must support the source codec (e.g.
write AAC audio from an MP4 to `.aac` or `.m4a`, not `.mp3`). Name an
encoder instead (`audio_codec = "libmp3lame"`) to transcode that stream,
or pass `NULL` to emit no codec option at all and let the output
extension pick the encoder. Each argument governs only its own output
file. Where the video is re-encoded, `hardware = "nvenc"` moves that
encode onto an NVIDIA GPU; the audio output is never affected.

## Usage

``` r
separate_audio_video(
  infile,
  audiofile,
  videofile,
  audio_codec = "copy",
  video_codec = "copy",
  hardware = c("none", "nvenc"),
  fallback = FALSE,
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

- audio_codec:

  A string naming the encoder for `audiofile`, passed to FFmpeg's
  `-codec:a`. The default `"copy"` stream-copies the audio losslessly; a
  codec name (e.g. `"libmp3lame"`) transcodes it; `NULL` emits no
  `-codec:a`, leaving the encoder to the `audiofile` extension.

- video_codec:

  A string naming the encoder for `videofile`, passed to FFmpeg's
  `-codec:v`. The default `"copy"` stream-copies the video losslessly; a
  codec name (e.g. `"libx264"`) transcodes it; `NULL` emits no
  `-codec:v`, leaving the encoder to the `videofile` extension.

- hardware:

  The encoder backend for `videofile`: `"none"` (default, the software
  `video_codec`) or `"nvenc"` for NVIDIA GPU encoding, which uses the
  nvenc encoder for `video_codec`'s family (e.g. `"libx264"` becomes
  `"h264_nvenc"`), assuming the H.264 family when `video_codec = NULL`.
  Only video is encoded on the GPU, so this never affects `audiofile`.
  Because this verb's video default is a stream copy, which runs no
  encoder at all, `hardware = "nvenc"` alongside `video_codec = "copy"`
  is an error: name an encoder or pass `video_codec = NULL`. See
  [`has_nvenc`](https://jmgirard.github.io/tidymedia/reference/nvenc_encoder.md)
  for availability and its caveats.

- fallback:

  A logical: when `hardware = "nvenc"` but nvenc is unavailable, encode
  in software with a message (`TRUE`) instead of aborting (`FALSE`,
  default). With `video_codec = NULL` the fallback leaves the codec
  unset rather than injecting one.

- run:

  A logical: run the commands through FFmpeg (`TRUE`, default) or return
  the compiled commands without running them (`FALSE`).

## Value

A named character vector of the two compiled commands (`audio`,
`video`); invisible when `run = TRUE`.

## See also

[`ffm_map()`](https://jmgirard.github.io/tidymedia/reference/ffm_map.md)
and
[`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md),
the builders it wraps;
[`has_nvenc()`](https://jmgirard.github.io/tidymedia/reference/nvenc_encoder.md)
for the `hardware = "nvenc"` toggle;
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
to pull out just the audio.

Other task verb functions:
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
[`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
[`strip_metadata()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata.md),
[`strip_metadata_batch()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
separate_audio_video(video, "audio.aac", "video.mp4", run = FALSE)
#>                                                                                                         audio 
#> "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:a copy -map 0:a \"audio.aac\"" 
#>                                                                                                         video 
#> "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:v copy -map 0:v \"video.mp4\"" 
# transcode the audio to MP3 while copying the video through untouched
separate_audio_video(video, "audio.mp3", "video.mp4",
                     audio_codec = "libmp3lame", run = FALSE)
#>                                                                                                               audio 
#> "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:a libmp3lame -map 0:a \"audio.mp3\"" 
#>                                                                                                               video 
#>       "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:v copy -map 0:v \"video.mp4\"" 
```
