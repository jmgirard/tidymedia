# Normalize a file's audio loudness (EBU R128)

Normalize the perceived loudness of a file's audio toward an EBU R128
target. The function uses FFmpeg's single-pass `loudnorm` filter. It can
also downmix the channel count and resample. The output holds **one
audio stream and no video**, whatever the input and whatever container
`outfile` names. So the output of this function is audio, as with
[`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
and
[`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md).
It does not carry the other streams through. To normalize a recording's
soundtrack *and* keep its picture, first normalize to an audio file.
Then put the audio back with the picture using the
[`ffmpeg`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
direct command. The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as LUFS, true peak and sample rate.

## Usage

``` r
normalize_audio(
  infile,
  outfile,
  target_loudness = -23,
  true_peak = -1,
  loudness_range = 7,
  channels = NULL,
  sample_rate = NULL,
  audio_codec = NULL,
  two_pass = FALSE,
  audio_stream = NULL,
  run = TRUE
)
```

## Arguments

- infile:

  A string containing the path to a media file (with audio). An input
  with no audio stream is an FFmpeg error, not a silent copy of the
  video.

- outfile:

  A string containing the path of the audio file to write. The function
  accepts any container that FFmpeg can write. The compiled command does
  not depend on which container it is. An audio container (`.wav`,
  `.flac`) holds the result exactly as a video container (`.mkv`) does.
  The video container carries one audio stream and nothing else.

- target_loudness:

  The target integrated loudness, in LUFS (a number in `-70`..`-5`;
  default `-23`, the EBU R128 target).

- true_peak:

  The maximum true peak, in dBTP (a number in `-9`..`0`; default `-1`,
  the EBU R128 ceiling).

- loudness_range:

  The target loudness range, in LU (a number in `1`..`50`; default `7`).

- channels:

  The output channel count, e.g. `1` to downmix to mono (a positive
  whole number), or `NULL` (default) to keep the source layout.

- sample_rate:

  The output sample rate in Hz, e.g. `48000` (a positive whole number).
  `NULL` (default) lets `loudnorm` choose. It resamples, up to 192 kHz
  and capped by the encoder, and does not keep the source rate. Set this
  argument to fix the output rate.

- audio_codec:

  An optional string naming the output audio encoder (e.g. `"aac"`,
  `"libmp3lame"`, `"flac"`), passed to FFmpeg's `-codec:a`. `NULL`
  (default) sets no `-codec:a`, which leaves the default encoder of the
  output container in place. `"copy"` is an error. Loudness
  normalization filters the audio, so the stream must be re-encoded and
  cannot be copied.

- two_pass:

  A logical. When `TRUE`, the function uses accurate two-pass
  (measured/linear) normalization. The default (`FALSE`) is single-pass.
  A first *analysis pass* measures the loudness of the input. A second
  *correction pass* feeds those measurements back with `linear=true`, so
  the output hits the EBU R128 target precisely. So two-pass **always
  runs the analysis pass through FFmpeg**, even when `run = FALSE`. It
  needs the binary and a readable input. Under `run = FALSE`, the
  analysis still runs. The returned value is the exact correction
  command, which is not run. The single-pass default touches no binary
  under `run = FALSE`. If the input is **silent**, the analysis pass
  measures its loudness as `-inf`. Normalizing silence to a target is
  undefined, so two-pass aborts with a clear error. The single-pass
  default leaves silence untouched. The batch form differs here.
  [`normalize_audio_batch`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md)
  does not abort on a silent row. It sets that row aside, marks it in a
  `silent` column, and normalizes the rest. When the analysis pass gives
  no usable measurement at all, the abort has class
  `tidymedia_loudnorm_no_measurement`. The batch form raises the same
  class, so one handler covers both. Where FFmpeg exited non-zero, the
  abort also carries `tidymedia_ffmpeg_exit`, and the exit number on
  `tm_status`. Where FFmpeg exited zero but printed no measurement block
  that can be parsed, the abort carries the shared class alone. The
  silence abort above is neither: a silent input *was* measured.

- audio_stream:

  The audio track to normalize, as a number that counts from `0` among
  the *audio tracks* of the input. `0` is the first audio track and `1`
  is the second. Other streams in the file, such as video, do not count.
  `NULL` (default) normalizes the **first** audio track. The first-track
  family reads `NULL` this way:
  [`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and `normalize_audio`, and their `_batch` forms. The every-track
  family reads it as every audio track:
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
  [`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  and
  [`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
  and their `_batch` forms. This function reads `NULL` as the first
  track only. The two-pass analysis measures each audio track, but the
  correction uses one set of measurements. Normalizing several tracks at
  once would apply one track's measurements to all of them. Under
  `two_pass = TRUE`, the analysis pass measures this same track. Only
  the named track reaches the output, and no video does, whatever the
  container. So an output name with a video extension gives a video file
  that holds only audio. An input with no audio at all is an FFmpeg
  error. A track the input does not have gives an FFmpeg error, not an R
  one. See
  [`audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md)
  for how this differs from `audio_input`, the input index on
  [`compare_videos`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  and
  [`picture_in_picture`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md).
  (default = `NULL`)

- run:

  A logical: run the (correction) command through FFmpeg (`TRUE`,
  default) or return the compiled command without running it (`FALSE`).
  Under `two_pass = TRUE` this gates only the correction pass; the
  analysis pass runs regardless (see `two_pass`).

## Value

The compiled FFmpeg command (invisibly when `run = TRUE`). Under
`two_pass = TRUE` this is the correction command built from the measured
values.

## Details

The default targets follow EBU Recommendation R 128 (2014). They are
`target_loudness = -23` LUFS and `true_peak = -1` dBTP. Loudness is
measured per ITU-R BS.1770-4. The default `loudness_range` is `7`. This
is single-pass (dynamic) `loudnorm`. The same input and arguments always
compile to one reproducible command, with no separate measurement pass.
The filter changes the audio, so FFmpeg re-encodes it. Set `audio_codec`
to name the output encoder, or leave it `NULL` to use the default of the
output container. Leaving `channels` at `NULL` keeps the source channel
layout. FFmpeg's `loudnorm` filter resamples its output, up to 192 kHz,
capped by the encoder. So the output sample rate is *not* the source
rate unless you set it. Set `sample_rate` to control the output rate.

The function warns when no `audio_stream` is named and `infile` carries
tracks that the output will not.
[`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
and
[`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
emit the same warning. Naming a track with `audio_stream` silences it,
as does `suppressWarnings(classes = "tidymedia_dropped_audio")`. The
check costs **one FFprobe call per distinct input**, which is one call
here, because this function takes a single `infile`. The warning is
given when FFprobe is available and the input can be probed. Otherwise
the check is skipped silently. It never runs under `run = FALSE`, and
never changes the compiled command. Under `two_pass = TRUE`, the warning
comes *before* the analysis pass. So it arrives while adding
`audio_stream` can still save that pass.

To switch the check off and skip its FFprobe call, use
`options(tidymedia.check_tracks = FALSE)` for the session. Use
`withr::local_options(tidymedia.check_tracks = FALSE)` for the rest of
one function.

## References

EBU Recommendation R 128 (2014), *Loudness normalisation and permitted
maximum level of audio signals*; ITU-R BS.1770-4.

## See also

[`ffm_loudnorm()`](https://jmgirard.github.io/tidymedia/reference/ffm_loudnorm.md),
the pipeline function it wraps.
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md)
for the many-file form.
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
and
[`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
the other task functions whose output is one audio stream.

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
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
# The output holds audio only, so name an audio file for it
normalize_audio(video, "normalized.wav", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -af \"loudnorm=I=-23:TP=-1:LRA=7,asetnsamples=n=4096:p=0\" -map \"0:a:0\" \"normalized.wav\""
# Normalize to a streaming target and downmix to mono
normalize_audio(video, "mono.wav", target_loudness = -16, channels = 1,
                run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -af \"loudnorm=I=-16:TP=-1:LRA=7,asetnsamples=n=4096:p=0\" -ac 1 -map \"0:a:0\" \"mono.wav\""
# Name the output audio encoder instead of taking the container's default
normalize_audio(video, "normalized.m4a", audio_codec = "aac", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -af \"loudnorm=I=-23:TP=-1:LRA=7,asetnsamples=n=4096:p=0\" -codec:a aac -map \"0:a:0\" \"normalized.m4a\""
```
