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
  audio_stream = NULL,
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
  for availability and its caveats. Resolving `"nvenc"` asks this FFmpeg
  build which encoders it has, so the first `"nvenc"` call that
  re-encodes the video runs the binary while the command is built, even
  under `run = FALSE`. The answer is remembered for the rest of the R
  session; see
  [`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
  to discard it. The stream-copy conflict above is caught first, so such
  a call aborts without probing.

- fallback:

  A logical: when `hardware = "nvenc"` but nvenc is unavailable, encode
  in software with a message (`TRUE`) instead of aborting (`FALSE`,
  default). With `video_codec = NULL` the fallback leaves the codec
  unset rather than injecting one.

- audio_stream:

  The 0-based index of the audio track to write to `audiofile`, counted
  *among the input's audio streams* – `0` is the first audio track, `1`
  the second, whatever their positions among the file's streams. `NULL`
  (default) keeps **every** audio track. The every-track family reads
  `NULL` this way – `separate_audio_video`,
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
  [`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  and
  [`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
  plus their `_batch` siblings. The first-track family takes one track
  only:
  [`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and
  [`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
  plus theirs. A container that holds several audio streams (`.mka`,
  `.m4a`) receives them all, while a single-stream container (`.aac`,
  `.mp3`, `.wav`) makes FFmpeg fail – name a track to write one of
  those. Count among the input's *audio* streams, not the `index` column
  of
  [`probe_audio`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
  which counts every stream. Unlike the verbs that pass video through,
  an input carrying no audio at all is an FFmpeg error here, because
  this verb's product is the audio file. `videofile` is never affected.
  Naming a track the input does not have is an FFmpeg error, not an R
  one. See
  [`audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md)
  for how this differs from `audio`, the input index on
  [`compare_videos`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  and
  [`picture_in_picture`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md).
  (default = `NULL`)

- run:

  A logical: run the commands through FFmpeg (`TRUE`, default) or return
  the compiled commands without running them (`FALSE`).

## Value

A named character vector of the two compiled commands (`audio`,
`video`); invisible when `run = TRUE`. Under `run = TRUE` the audio
command runs first and the video command runs second, whether or not the
audio command succeeded. A failed audio command still aborts the call,
and the video command has written `videofile` by then unless it failed
too; see *When the audio output fails*.

## When the audio output fails

The two commands run in order — audio first, video second — and the
video command runs even when the audio one has already failed, so a
failed audio half does not cost you the video. On that path the call
still aborts with the audio failure, and that error carries one added
line naming the video file that was written. When the video command
fails as well, the added line is not there, the audio failure is still
the error you get, and FFmpeg's own output for the failed video command
is printed above it.

What a failed command leaves at its own output path is the same rule on
either path: a partial file that run wrote is removed, while a file that
was already at that path and that FFmpeg never wrote to is left exactly
as it was. So neither failure path promises the path is empty afterwards
— only that nothing half-written is left there. The audio failure's own
error says which of the two happened to `audiofile`; nothing reports
`videofile`'s fate on the both-fail path, because the video command's
error is not the one you get.

Because the default keeps every audio track, writing a multi-track input
to a container that holds only one (`.aac`, `.mp3`, `.wav`) makes FFmpeg
fail. When that happens and no `audio_stream` was named, the error
additionally reports how many audio tracks `infile` carries and names
the two ways out — `audio_stream` to write one track, or a container
such as `.mka` or `.m4a` to keep them all. FFmpeg's own error and exit
status are still reported beneath it, and remain the authority on why
the command failed: the extra report is attached to *any* failing audio
command on a multi-track input, not only to a container refusal.

The condition carries two class names, so a caller can catch it at
either width. It is `tidymedia_ffmpeg_exit`, the class every non-zero
FFmpeg exit raises, which is what an exit-status handler catches — the
number is on the condition's `tm_status` field. It is also
`tidymedia_multitrack_separation`, the class of the enriched diagnostic
itself, which is what to catch when it is this failure in particular you
want:


    tryCatch(
      separate_audio_video("three-tracks.mkv", "audio.mp3", "video.mp4"),
      tidymedia_ffmpeg_exit = function(cnd) cnd$tm_status
    )

When the report is omitted, the error that reaches the caller is the one
the run itself raised, unchanged: a non-zero exit still answers to
`tidymedia_ffmpeg_exit`, and a failure that is not an exit at all
answers to neither class here: an FFmpeg the package cannot locate
raises an error carrying no `tidymedia_` class at all, and a reached
limit raises `tidymedia_timeout`.

Counting the tracks means running FFprobe, so this is **best-effort**:
it is added when FFprobe is available and `infile` can be probed, and
omitted silently otherwise, leaving FFmpeg's own error alone. It never
runs under `run = FALSE`, never changes the compiled commands, and is
skipped entirely when `audio_stream` names a track — with one track
mapped, the track count cannot be what FFmpeg objected to.

## See also

[`ffm_map()`](https://jmgirard.github.io/tidymedia/reference/ffm_map.md)
and
[`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md),
the builders it wraps;
[`has_nvenc()`](https://jmgirard.github.io/tidymedia/reference/nvenc_encoder.md)
for the `hardware = "nvenc"` toggle;
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
to pull out just the audio;
[`probe_audio()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)
to list an input's audio tracks.

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

Other audio selection functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
[`audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md),
[`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
[`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`format_for_web_batch()`](https://jmgirard.github.io/tidymedia/reference/format_for_web_batch.md),
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
separate_audio_video(video, "audio.aac", "video.mp4", run = FALSE)
#>                                                                                                             audio 
#> "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:a copy -map \"0:a\" \"audio.aac\"" 
#>                                                                                                             video 
#> "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:v copy -map \"0:v\" \"video.mp4\"" 
# transcode the audio to MP3 while copying the video through untouched
separate_audio_video(video, "audio.mp3", "video.mp4",
                     audio_codec = "libmp3lame", run = FALSE)
#>                                                                                                                   audio 
#> "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:a libmp3lame -map \"0:a\" \"audio.mp3\"" 
#>                                                                                                                   video 
#>       "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:v copy -map \"0:v\" \"video.mp4\"" 
# write only the second audio track (this sample has one, so compile only)
separate_audio_video(video, "audio.aac", "video.mp4",
                     audio_stream = 1, run = FALSE)
#>                                                                                                               audio 
#> "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:a copy -map \"0:a:1\" \"audio.aac\"" 
#>                                                                                                               video 
#>   "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:v copy -map \"0:v\" \"video.mp4\"" 
```
