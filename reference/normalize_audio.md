# Normalize a file's audio loudness (EBU R128)

Normalize the perceived loudness of a file's audio toward an EBU R128
target using FFmpeg's single-pass `loudnorm` filter, optionally
downmixing the channel count and resampling. The output holds **one
audio stream and no video**, whatever the input and whatever container
`outfile` names – so this is an audio-producing verb like
[`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
and
[`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
not a pass-through one. To normalize a recording's soundtrack *and* keep
its picture, normalize to an audio file and mux it back with the
[`ffmpeg`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
escape hatch.

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

  A string containing the path of the audio file to write. Any container
  FFmpeg can write is accepted and the compiled command does not depend
  on which – an audio container (`.wav`, `.flac`) holds the result
  exactly as a video container (`.mkv`) does, the latter simply carrying
  one audio stream and nothing else.

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

  The output sample rate in Hz, e.g. `48000` (a positive whole number),
  or `NULL` (default) to let `loudnorm` choose (it resamples, up to 192
  kHz encoder-capped – not the source rate). Set this to pin the output
  rate.

- audio_codec:

  An optional string naming the output audio encoder (e.g. `"aac"`,
  `"libmp3lame"`, `"flac"`), passed to FFmpeg's `-codec:a`. `NULL`
  (default) emits no `-codec:a`, leaving the output container's default
  encoder in place. `"copy"` is an error: loudness normalization filters
  the audio, so the stream must be re-encoded and cannot be copied.

- two_pass:

  A logical: when `TRUE`, use accurate two-pass (measured/linear)
  normalization instead of the default single-pass (`FALSE`). A first
  *analysis pass* measures the input's loudness, and a second
  *correction pass* feeds those measurements back with `linear=true` so
  the output hits the EBU R128 target precisely. Two-pass therefore
  **always runs the analysis pass through FFmpeg** (it needs the binary
  and readable input), even when `run = FALSE`: in that case the
  analysis still runs and the returned value is the exact correction
  command, left unexecuted. The single-pass default touches no binary
  under `run = FALSE`. If the input is **silent**, the analysis pass
  measures its loudness as `-inf`; normalizing silence to a target is
  undefined, so two-pass aborts with a clear error (the single-pass
  default leaves silence untouched).

- audio_stream:

  The 0-based index of the audio track to normalize, counted *among the
  input's audio streams* – `0` is the first audio track, `1` the second,
  whatever their positions among the file's streams. `NULL` (default)
  normalizes the **first** audio track. The first-track family reads
  `NULL` this way –
  [`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and `normalize_audio`, plus their `_batch` siblings. The every-track
  family keeps them all instead:
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
  [`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  and
  [`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
  plus theirs. This verb reads `NULL` the first-track way because the
  two-pass analysis produces one measurement per audio track while the
  correction takes a single set, so normalizing several tracks at once
  would apply one track's measurements to all of them. Under
  `two_pass = TRUE` the analysis pass measures this same track. Only the
  named track reaches the output, and no video does – whatever the
  container, so an output name that keeps a video extension yields a
  video file carrying audio alone. An input with no audio at all is an
  FFmpeg error. Naming a track the input does not have is an FFmpeg
  error, not an R one. See
  [`audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md)
  for how this differs from `audio`, the input index on
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

The default targets follow EBU Recommendation R 128 (2014) –
`target_loudness = -23` LUFS and `true_peak = -1` dBTP, loudness
measured per ITU-R BS.1770-4 – with `loudness_range = 7`. This is
single-pass (dynamic) `loudnorm`: the same input and arguments always
compile to one reproducible command, with no separate measurement pass.
Because the audio is filtered it is re-encoded; set `audio_codec` to
name the output encoder, or leave it `NULL` to use the output
container's default. Leaving `channels` at `NULL` preserves the source
channel layout. Note that FFmpeg's `loudnorm` filter resamples its
output (up to 192 kHz, capped by the encoder), so the output sample rate
is *not* the source rate unless you pin it: set `sample_rate` to control
the output rate.

When no `audio_stream` is named and `infile` turns out to carry tracks
the output will not, the verb warns – the same warning
[`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
and
[`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
emit. Naming a track with `audio_stream` silences it, as does
`suppressWarnings(classes = "tidymedia_dropped_audio")`. The check is
**best-effort** and costs **one FFprobe call per distinct input** – one,
here, since this verb takes a single `infile`: it is emitted when
FFprobe is available and the input can be probed, and skipped silently
otherwise. It never runs under `run = FALSE`, and never changes the
compiled command. Under `two_pass = TRUE` it lands *before* the analysis
pass, so it arrives while adding `audio_stream` can still save that
pass.

## References

EBU Recommendation R 128 (2014), *Loudness normalisation and permitted
maximum level of audio signals*; ITU-R BS.1770-4.

## See also

[`ffm_loudnorm()`](https://jmgirard.github.io/tidymedia/reference/ffm_loudnorm.md),
the builder it wraps;
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md)
for the many-file form;
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
and
[`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
the other verbs whose output is one audio stream.

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
