# Get started with tidymedia

``` r

library(tidymedia)
```

tidymedia runs [FFmpeg](https://ffmpeg.org/) and
[MediaInfo](https://mediaarea.net/en/MediaInfo) from R. It helps you
prepare media files for research in a way you can repeat. It trims,
crops and converts files, often many at once. It also reads media
metadata into tibbles.

tidymedia does not try to cover everything FFmpeg can do. The words that
FFmpeg uses, such as [codec](#glossary) and [stream](#glossary), are
defined in the [glossary](#glossary) at the end of this page.

This page uses a short sample clip that comes with the package:

``` r

video <- system.file("extdata", "sample.mp4", package = "tidymedia")
```

## Start with a task function

Most jobs need one call to a task function. For example, you may need
the audio of a recording for a transcription tool.
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
writes the audio to its own file:

``` r

extract_audio(video, "audio.m4a")
```

A task function runs FFmpeg at once. It returns the FFmpeg command it
ran, but invisibly, so R prints nothing. A function that writes two
files, such as
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
returns both commands.

To see the command without running it, add `run = FALSE`. The function
then returns the command as a string that you can read, log or save:

``` r

extract_audio(video, "audio.m4a", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:a copy -vn -map \"0:a:0\" \"audio.m4a\""
```

This is the main idea of the package. You can read each command before
you run it. Cropping works the same way:

``` r

crop_video(video, "cropped.mp4", width = 160, height = 120, run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"crop=w=160:h=120:x=(in_w-out_w)/2:y=(in_h-out_h)/2\" -codec:a copy -map \"0:v?\" -map \"0:a?\" \"cropped.mp4\""
```

### Choosing an audio track

Some recordings have more than one audio track, for example a room
microphone and a lapel microphone. If you do not choose a track, FFmpeg
chooses one for you.

Each task function that reads one input file and picks an audio track
has an `audio_stream` argument. It counts from 0, and it counts only the
audio tracks. So `audio_stream = 1` is the second audio track, wherever
it sits in the file:

``` r

extract_audio(video, "lapel.m4a", audio_stream = 1, run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:a copy -vn -map \"0:a:1\" \"lapel.m4a\""
```

If you leave `audio_stream` out, the default depends on the function. A
function that writes exactly one audio track, such as
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
takes the first track. A function that passes the audio through, such as
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
keeps every track. Compare the `-map` parts of the two commands above
and below:

``` r

crop_video(video, "cropped.mp4", width = 160, height = 120, run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"crop=w=160:h=120:x=(in_w-out_w)/2:y=(in_h-out_h)/2\" -codec:a copy -map \"0:v?\" -map \"0:a?\" \"cropped.mp4\""
```

The help page
[`?audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md)
lists which functions use each default. It also explains `audio_input`,
which the functions for several input files use. That argument counts
input files from 0, not audio tracks.

Each task function has a batch version for a folder of files, such as
[`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md)
and
[`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md).
See
[`vignette("batch")`](https://jmgirard.github.io/tidymedia/articles/batch.md).
For a full research example that uses many task functions, see
[`vignette("workflow")`](https://jmgirard.github.io/tidymedia/articles/workflow.md).

## Three kinds of function

tidymedia has three kinds of function:

- **Task functions**, such as
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  and
  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  do one common job in one call. Start here.
- **Pipeline functions**, whose names start with `ffm_`, build an FFmpeg
  command one step at a time. Each task function uses them. Use them
  when no task function does the job you need.
- **Direct commands**,
  [`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md),
  [`ffprobe()`](https://jmgirard.github.io/tidymedia/reference/ffprobe.md)
  and
  [`mediainfo()`](https://jmgirard.github.io/tidymedia/reference/mediainfo.md),
  pass your own arguments to the program. Use them for anything that
  tidymedia does not cover.

The rest of this page shows the pipeline functions.

## Building a pipeline

A pipeline starts with
[`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md),
which names the input and output files. You add steps with `|>`. Each
step adds an instruction, and nothing runs yet.
[`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
turns the pipeline into the FFmpeg command:

``` r

ffm_files(video, "output.mp4") |>
  ffm_trim(start = 1, end = 5) |>
  ffm_crop(width = 160, height = 120) |>
  ffm_codec(video = "libx264") |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"trim=start=1:end=5,setpts=PTS-STARTPTS,crop=w=160:h=120:x=(in_w-out_w)/2:y=(in_h-out_h)/2\" -codec:v libx264 \"output.mp4\""
```

When you print a pipeline, R shows the same command. So you can look at
a pipeline at any point:

``` r

ffm_files(video, "output.mp4") |>
  ffm_scale(width = 320, height = 240) |>
  ffm_pixel_format("yuv420p")
#> tidymedia ffmpeg pipeline:
#> 
#>  -y -i "/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4" -vf "scale=w=320:h=240" -pix_fmt yuv420p "output.mp4"
```

To run the command and write the output file, use
[`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md)
in place of
[`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md).

### More pipeline steps

[`ffm_fps()`](https://jmgirard.github.io/tidymedia/reference/ffm_fps.md)
changes the [frame rate](#glossary).
[`ffm_drawbox()`](https://jmgirard.github.io/tidymedia/reference/ffm_drawbox.md)
draws a box on the picture, which can hide a name on screen:

``` r

ffm_files(video, "boxed.mp4") |>
  ffm_fps(15) |>
  ffm_drawbox(x = 10, y = 10, width = 60, height = 40, color = "black") |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"fps=15,drawbox=x=10:y=10:w=60:h=40:c=black:t=fill\" \"boxed.mp4\""
```

[`ffm_loudnorm()`](https://jmgirard.github.io/tidymedia/reference/ffm_loudnorm.md)
makes audio a set loudness, in [LUFS](#glossary), with a limit on its
[true peak](#glossary). Here
[`ffm_drop()`](https://jmgirard.github.io/tidymedia/reference/ffm_drop.md)
also leaves the video out of the output:

``` r

ffm_files(video, "speech.m4a") |>
  ffm_drop("video") |>
  ffm_loudnorm(target_loudness = -23, true_peak = -1) |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -af \"loudnorm=I=-23:TP=-1:LRA=7,asetnsamples=n=4096:p=0\" -vn \"speech.m4a\""
```

[`ffm_output_options()`](https://jmgirard.github.io/tidymedia/reference/ffm_output_options.md)
adds FFmpeg output options that have no pipeline function of their own.
tidymedia still puts them in the right place in the command:

``` r

ffm_files(video, "web.mp4") |>
  ffm_output_options("-movflags +faststart") |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -movflags +faststart \"web.mp4\""
```

## Fast cuts and exact cuts

You can cut a clip in two ways. An exact cut [re-encodes](#glossary) the
video, which is slower. A fast cut uses a [stream copy](#glossary),
which keeps the quality but starts at the nearest [keyframe](#glossary).

[`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md)
does both. Set `reencode = FALSE` and add
[`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
for a fast cut:

``` r

# Fast cut with no loss of quality
ffm_files(video, "output.mp4") |>
  ffm_seek(start = 1, end = 5, reencode = FALSE) |>
  ffm_copy() |>
  ffm_compile()
#> [1] "-y -ss 1 -to 5 -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:v copy -codec:a copy -avoid_negative_ts make_zero -map \"0\" \"output.mp4\""
```

[`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md)
uses FFmpeg’s `-ss` and `-to` options.
[`ffm_trim()`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md)
uses FFmpeg’s `trim` filter. Only
[`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md)
can make a fast cut.

## Combining multiple inputs

Some pipeline functions take more than one input. Give
[`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md)
a vector of files. Then use
[`ffm_hstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md)
to put the videos side by side, or
[`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md)
to put one above the other.
[`ffm_overlay()`](https://jmgirard.github.io/tidymedia/reference/ffm_overlay.md)
puts one video on top of another, and
[`ffm_concat()`](https://jmgirard.github.io/tidymedia/reference/ffm_concat.md)
joins them end to end:

``` r

ffm_files(c(video, video), "side_by_side.mp4") |>
  ffm_hstack() |>
  ffm_compile()
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -filter_complex \"[0:v][1:v]hstack=inputs=2:shortest=0[vout]\" -map \"[vout]\" \"side_by_side.mp4\""
```

[`ffm_hstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md),
[`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md)
and
[`ffm_overlay()`](https://jmgirard.github.io/tidymedia/reference/ffm_overlay.md)
leave the audio out. To keep it, add `ffm_map("0:a")`.
[`ffm_concat()`](https://jmgirard.github.io/tidymedia/reference/ffm_concat.md)
keeps all the streams, audio included. One-input task functions that
pass audio through, such as
[`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
do the opposite and keep every audio track.

Two task functions cover the common cases.
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
puts videos side by side or one above the other.
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
puts a smaller copy of one video on top of another. Both take
`audio_input`, which names the input whose audio to keep. They copy that
audio unchanged unless you choose an audio [encoder](#glossary) with
`audio_codec`.

``` r

compare_videos(c(video, video), "compare.mp4", audio_input = 0, run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -filter_complex \"[0:v][1:v]scale2ref='oh*mdar':'if(lt(main_h,ih),ih,main_h)'[0s][1s];[1s][0s]scale2ref='oh*mdar':'if(lt(main_h,ih),ih,main_h)'[1s][0s];[0s][1s]hstack,setsar=1[vout]\" -codec:a copy -map \"[vout]\" -map \"0:a\" \"compare.mp4\""
```

A pipeline has one input chain, a list of filters in order, and one
output. It cannot build an FFmpeg filter graph with branches. For that,
use the direct command
[`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md):

``` r

# Your own arguments, passed to FFmpeg as they are
ffmpeg("-version")[1]
#> [1] "ffmpeg version 6.1.1-3ubuntu5 Copyright (c) 2000-2023 the FFmpeg developers"
```

## Glossary

- **Codec**: a way to compress audio or video, such as H.264 or AAC.
  FFmpeg names each codec with a short string, such as `"h264"` or
  `"aac"`.
- **Container**: the file format that holds the streams, such as MP4,
  MKV or WAV. The file extension usually names the container.
- **Stream**: one track in a file, such as the video or one audio track.
  A file can hold more than one stream of each kind.
- **Encoder**: the part of FFmpeg that writes a stream in a codec. One
  codec can have several encoders, such as `libx264` and
  `h264_videotoolbox` for H.264.
- **Re-encode**: to decode a stream and write it again with an encoder.
  This is slower and can lose some quality, but it lets FFmpeg change
  the picture or sound.
- **Stream copy**: to put a stream into the output file without decoding
  it. This is fast and keeps the quality, but it cannot change the
  picture or sound.
- **Pixel format**: how a video stores the color of each pixel, such as
  `"yuv420p"`. Many players need `"yuv420p"` to play H.264 video.
- **Keyframe**: a video frame that is stored whole, not as a change from
  the frames before it. A stream copy can start a cut only at a
  keyframe.
- **Frame rate**: the number of video frames per second.
- **Sample rate**: the number of audio samples per second, in hertz,
  such as
  48000. 
- **LUFS**: a unit of loudness that follows how loud people hear the
  sound. The EBU R 128 broadcast standard uses -23 LUFS.
- **True peak**: the highest level the sound wave reaches, including
  between samples. A limit on the true peak stops the sound from
  clipping.
- **Hardware encoder**: an encoder that runs on a graphics card or a
  video chip instead of the main processor. NVIDIA nvenc and Apple
  videotoolbox are the two that tidymedia supports.

## Where to next

- [`vignette("workflow")`](https://jmgirard.github.io/tidymedia/articles/workflow.md)
  shows a full research example.
- [`vignette("batch")`](https://jmgirard.github.io/tidymedia/articles/batch.md)
  shows how to run a task function over many files.
- [`vignette("metadata")`](https://jmgirard.github.io/tidymedia/articles/metadata.md)
  shows how to read metadata into tibbles.
- [`vignette("verification")`](https://jmgirard.github.io/tidymedia/articles/verification.md)
  shows how to check outputs and limit run time.
