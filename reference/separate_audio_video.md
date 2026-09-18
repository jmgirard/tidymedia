# Split a media file into separate audio and video files

By default, each stream is copied, not re-encoded
(`audio_codec = "copy"`, `video_codec = "copy"`). A copy loses no
quality and is fast. Each output container must then support the source
codec. For example, write AAC audio from an MP4 to `.aac` or `.m4a`, not
to `.mp3`. To re-encode a stream, name an encoder
(`audio_codec = "libmp3lame"`). Pass `NULL` to set no codec option, so
the output extension picks the encoder. Each argument governs only its
own output file. Where the video is re-encoded, `hardware = "nvenc"` or
`"videotoolbox"` moves that encode onto a GPU. The audio output is never
affected. The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as codec, container and stream copy.

## Usage

``` r
separate_audio_video(
  infile,
  audiofile,
  videofile,
  audio_codec = "copy",
  video_codec = "copy",
  hardware = c("none", "nvenc", "videotoolbox"),
  fallback = FALSE,
  quality = NULL,
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

  A string that names the encoder for `audiofile`. It goes to FFmpeg's
  `-codec:a`. The default `"copy"` copies the audio stream with no
  quality loss. A codec name (e.g. `"libmp3lame"`) re-encodes it. `NULL`
  sets no `-codec:a`, so the `audiofile` extension picks the encoder.

- video_codec:

  A string that names the encoder for `videofile`. It goes to FFmpeg's
  `-codec:v`. The default `"copy"` copies the video stream with no
  quality loss. A codec name (e.g. `"libx264"`) re-encodes it. `NULL`
  sets no `-codec:v`, so the `videofile` extension picks the encoder.

- hardware:

  The encoder backend for `videofile`. `"none"` (default) uses the
  software `video_codec`. `"nvenc"` uses NVIDIA GPU encoding, and
  `"videotoolbox"` uses Apple GPU encoding. Each uses its own encoder
  for the family of `video_codec`. For example, `"libx264"` becomes
  `"h264_nvenc"` or `"h264_videotoolbox"`. With `video_codec = NULL`,
  the family is H.264. Only video is encoded on the GPU, so this never
  affects `audiofile`. The default `video_codec = "copy"` is a stream
  copy, which runs no encoder at all. So a `hardware` other than
  `"none"` with `video_codec = "copy"` is an error. Name an encoder or
  pass `video_codec = NULL`. See
  [`has_hardware_encoder`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
  for availability and its caveats. Resolving a hardware backend asks
  this FFmpeg build which encoders it has. So the first such call that
  re-encodes the video runs FFmpeg while the command is built, even
  under `run = FALSE`. The answer is remembered for the rest of the R
  session. See
  [`refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md)
  to discard it. The stream-copy conflict above is caught first, so such
  a call aborts without asking FFmpeg.

- fallback:

  A logical. When a `hardware` other than `"none"` is requested but its
  encoder is unavailable, `TRUE` encodes in software with a message.
  `FALSE` (default) aborts instead. With `video_codec = NULL`, the
  fallback leaves the codec unset rather than injecting one. A
  `video_codec` in a family that the backend has no encoder for is a
  wrong argument, not an absent encoder. So it aborts whatever
  `fallback` says.

- quality:

  A number, or `NULL` (default) to leave the encoder's own default in
  place. It is the encoder's own rate-control value, passed through
  unchanged. `libx264` and `libx265` read it as `-crf` (0 to 51). The
  nvenc encoders read it as `-cq` (0 to 51), and the videotoolbox
  encoders as `-q:v` (1 to 100). Each scale is its own: the same number
  means something different on each encoder. A value outside the
  encoder's range is refused. An encoder outside those seven, such as
  `libvpx-vp9`, is refused with `quality` set. So is a `video_codec` of
  `"copy"`, or of `NULL` under `hardware = "none"`. When
  `fallback = TRUE` falls back to software, the value is dropped and the
  message says so, because it belonged to the hardware encoder's scale.
  It applies to `videofile` only.

- audio_stream:

  The audio track to write to `audiofile`, as a number that counts from
  `0` among the *audio tracks* of the input. `0` is the first audio
  track and `1` is the second. Other streams in the file, such as video,
  do not count. `NULL` (default) keeps **every** audio track. The
  every-track family reads `NULL` this way: `separate_audio_video`,
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
  [`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  and
  [`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
  and their `_batch` forms. The first-track family reads it as the first
  audio track only:
  [`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and
  [`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
  and their `_batch` forms. A container that holds several audio streams
  (`.mka`, `.m4a`) gets them all. A container for one stream only
  (`.aac`, `.mp3`, `.wav`) makes FFmpeg fail, so name a track to write
  one of those. Count only the input's *audio* streams. Do not use the
  `index` column of
  [`probe_audio`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
  which counts every stream. An input with no audio at all is an FFmpeg
  error here, because this function writes an audio file. Functions that
  write only a video file, such as
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  do not fail in that case. `videofile` is never affected. A track the
  input does not have gives an FFmpeg error, not an R one. See
  [`audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md)
  for how this differs from `audio_input`, the input index on
  [`compare_videos`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  and
  [`picture_in_picture`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md).
  (default = `NULL`)

- run:

  A logical: run the commands through FFmpeg (`TRUE`, default) or return
  the compiled commands without running them (`FALSE`).

## Value

A named character vector of the two compiled commands (`audio`,
`video`). It is invisible when `run = TRUE`. Under `run = TRUE`, the
audio command runs first and the video command runs second. The video
command runs whether or not the audio command succeeded. A failed audio
command still aborts the call. By then, the video command has written
`videofile`, unless it failed too. See *When the audio output fails*.

## When the audio output fails

The two commands run in order: audio first, video second. The video
command runs even when the audio command failed, so a failed audio half
does not cost you the video. In that case, the call still aborts with
the audio failure. That error carries one added line that names the
video file that was written. When the video command fails too, the added
line is not there. The audio failure is still the error you get.
FFmpeg's own output for the failed video command is printed above it.

A time limit reached on the audio command is held like any other audio
failure, so the video command still runs. The video command gets a fresh
limit of its own, because
[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
limits each program that the call starts, not the call. So a call whose
audio half reaches the limit can wait up to two limits, not one.

A failed command treats its own output path by the same rule on either
path. It removes a partial file that the run wrote. It leaves a file
that was already at that path, and that FFmpeg never wrote to, exactly
as it was. So neither failure path promises that the path is empty
afterwards. It promises only that nothing half-written is left there.
The audio failure's own error says which of the two happened to
`audiofile`. The same error's `tm_video_error` field says what became of
the video command. It holds the condition that command raised when it
failed too, and `NULL` when it succeeded.

The default keeps every audio track. So FFmpeg fails when it writes a
multi-track input to a container that holds only one track (`.aac`,
`.mp3`, `.wav`). When that happens, the error also reports how many
audio tracks `infile` carries, and it names the two ways out. Use
`audio_stream` to write one track, or use a container such as `.mka` or
`.m4a` to keep them all.

The error carries that extra report only when all four of these hold:

- No `audio_stream` was named.

- FFmpeg returned a non-zero exit status.

- `infile` carries more than one audio track.

- The extension of `audiofile` is not among the containers named here as
  holding several audio streams.

Those containers are `.mka`, `.m4a`, `.mp4`, `.mov`, `.mkv`, `.webm`,
`.ogg`, `.opus` and `.ts`. The nine are an exclusion list and not a
survey. FFmpeg writes several audio streams into other containers too,
`.avi` and `.nut` among them. A failure on one of those still gets the
report. The container condition keeps the report off a call that already
does what the report advises. When a call writes to one of the nine, the
failure cannot be the container refusing a second audio stream. The
report would then leave unnamed whatever FFmpeg did object to.

If any of the four does not hold, the error you get is the one the run
itself raised, whatever that error is. It has the same class, the same
status field and the same message. The one difference is the line saying
that the video output was written. A failing audio half carries that
line when the video command wrote its file and the audio failure is an
rlang condition. When the exit status is the one that does not hold,
there is no exit status to carry. A run that never reached FFmpeg has
none.

The report states what the call *did*: the track count, and that every
track was mapped into one output. It never states why FFmpeg refused.
FFmpeg's own error and exit status are printed beneath it and carried on
the condition. They remain the only authority on the cause. Several
causes look alike from here. A stream copy into a container that will
not hold the source codec fails on a multi-track input too. The default
`audio_codec = "copy"` into `.mp3` is one example. An unknown encoder
and a missing output directory fail the same way.

The condition carries two class names, so a caller can catch it at
either width. It is `tidymedia_ffmpeg_exit`, the class that every
non-zero FFmpeg exit raises. An exit-status handler catches that class,
and the number is on the condition's `tm_status` field. It is also
`tidymedia_multitrack_separation`, the class of this report itself.
Catch that class when it is this failure in particular you want:


    tryCatch(
      separate_audio_video("three-tracks.mkv", "audio.mp3", "video.mp4"),
      tidymedia_ffmpeg_exit = function(cnd) cnd$tm_status
    )

When the report is omitted, the error that reaches the caller is the one
the run itself raised, apart from that video-output line. A non-zero
exit still answers to `tidymedia_ffmpeg_exit`. A failure that is not an
exit answers to neither class here. An FFmpeg that the package cannot
locate raises an error with no `tidymedia_` class at all. A reached
limit raises `tidymedia_timeout`.

Counting the tracks means running FFprobe, so the report is not
guaranteed. The error has it when FFprobe is available and `infile` can
be probed. Otherwise the package omits it silently and leaves FFmpeg's
own error alone. So the report may not appear, and its absence is never
itself a second failure. The count never runs under `run = FALSE` and
never changes the compiled commands. It is skipped when `audio_stream`
names a track, or when `audiofile` names one of the multi-stream
containers above. With one track mapped, the track count cannot be what
FFmpeg objected to.

## See also

[`ffm_map()`](https://jmgirard.github.io/tidymedia/reference/ffm_map.md)
and
[`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md),
the pipeline functions it wraps.
[`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
for the `hardware` argument.
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
to pull out just the audio.
[`probe_audio()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)
to list an input's audio tracks.

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
