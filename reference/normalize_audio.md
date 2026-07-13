# Normalize a file's audio loudness (EBU R128)

Normalize the perceived loudness of a file's audio toward an EBU R128
target using FFmpeg's single-pass `loudnorm` filter, optionally
downmixing the channel count and resampling. The video stream is copied
unchanged (`-c:v copy`), so only the audio is touched – the audio-side
complement to
[`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
which leaves audio alone.

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
  two_pass = FALSE,
  run = TRUE
)
```

## Arguments

- infile:

  A string containing the path to a media file (with audio).

- outfile:

  A string containing the path of the file to write.

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
Because the audio is filtered it is re-encoded (the container's default
audio encoder). Leaving `channels` at `NULL` preserves the source
channel layout. Note that FFmpeg's `loudnorm` filter resamples its
output (up to 192 kHz, capped by the encoder), so the output sample rate
is *not* the source rate unless you pin it: set `sample_rate` to control
the output rate.

## References

EBU Recommendation R 128 (2014), *Loudness normalisation and permitted
maximum level of audio signals*; ITU-R BS.1770-4.

## See also

[`ffm_loudnorm()`](https://jmgirard.github.io/tidymedia/reference/ffm_loudnorm.md),
the builder it wraps;
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md)
for the many-file form;
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
its video-side complement.

Other task verb functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md),
[`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md),
[`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md),
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
[`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
[`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)

## Examples

``` r
video <- system.file("extdata", "sample.mp4", package = "tidymedia")
normalize_audio(video, "normalized.mp4", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -af \"loudnorm=I=-23:TP=-1:LRA=7\" -codec:v copy \"normalized.mp4\""
# Normalize to a streaming target and downmix to mono
normalize_audio(video, "mono.mp4", target_loudness = -16, channels = 1,
                run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -af \"loudnorm=I=-16:TP=-1:LRA=7\" -codec:v copy -ac 1 \"mono.mp4\""
```
