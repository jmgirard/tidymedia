# A research preprocessing workflow

``` r

library(tidymedia)
```

This page follows one research project from raw recordings to shared
files. The study records conversations between two people, with one
camera on each person. The recordings need the same steps before coding
and sound analysis.

tidymedia suits this job. Each step runs the same way on every file, and
each step gives you a command that you can save and run again.

Most examples below use `run = FALSE`. The function then returns the
FFmpeg command without running it, so you can read what it would do.
Leave out `run = FALSE` to process the files.

Some examples work on a whole folder, so they are shown but not run. The
others use the short sample clip that comes with the package:

``` r

session <- system.file("extdata", "sample.mp4", package = "tidymedia")
```

Say the study folder has one file for each camera in each session:

``` r

jobs <- ffm_jobs("study/raw", type = "video")
jobs
#> # A tibble: 4 × 1
#>   input
#>   <chr>
#> 1 /data/study/raw/session01_camA.mp4
#> 2 /data/study/raw/session01_camB.mp4
#> 3 /data/study/raw/session02_camA.mp4
#> 4 /data/study/raw/session02_camB.mp4
```

[`ffm_jobs()`](https://jmgirard.github.io/tidymedia/reference/ffm_jobs.md)
returns a jobs table for the batch functions. It has one row for each
file, with the file’s full path in an `input` column. The full path is
why the output above shows `/data/study/raw` and not `study/raw`.

If the folder has no video files,
[`ffm_jobs()`](https://jmgirard.github.io/tidymedia/reference/ffm_jobs.md)
stops with an error. The next section adds an `output` column to the
table and passes it to a `*_batch()` function.

## 1. Standardize the recordings

Cameras often differ in picture size, [frame
rate](https://jmgirard.github.io/tidymedia/articles/tidymedia.html#glossary)
and
[codec](https://jmgirard.github.io/tidymedia/articles/tidymedia.html#glossary).
Those differences make later steps harder.
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
[re-encodes](https://jmgirard.github.io/tidymedia/articles/tidymedia.html#glossary)
a file to one common format:

``` r

standardize_video(
  session, "session01_camA_std.mp4",
  width = 1280, height = 720, fps = 30,
  run = FALSE
)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"scale=w=1280:h=720,fps=30\" -codec:v libx264 -codec:a copy -pix_fmt yuv420p -movflags +faststart -map \"0:v?\" -map \"0:a?\" \"session01_camA_std.mp4\""
```

To do the same for the whole folder, use the batch version with the jobs
table. Each task function has a `*_batch()` version like this one. See
[`vignette("batch")`](https://jmgirard.github.io/tidymedia/articles/batch.md)
for how batches work.

``` r

jobs$output <- sub("/raw/", "/std/", jobs$input)
standardize_video_batch(jobs, width = 1280, height = 720, fps = 30)
```

### Using video hardware

Re-encoding a large study on the main processor can be slow. Functions
that re-encode video, such as
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
and
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
take a `hardware` argument. It moves the work to a [hardware
encoder](https://jmgirard.github.io/tidymedia/articles/tidymedia.html#glossary).

There are two choices. Use `"nvenc"` for an NVIDIA graphics card, with
an FFmpeg build that supports nvenc. Use `"videotoolbox"` for Apple
hardware on macOS.

These functions show what your FFmpeg can do.
[`ffmpeg_codecs()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_codecs.md)
and
[`ffmpeg_encoders()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)
list its codecs and encoders.
[`hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
gives the encoder name for a codec and a hardware choice.
[`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
says whether your FFmpeg has that encoder:

``` r

head(ffmpeg_codecs())
#> # A tibble: 6 × 8
#>   name       details           type  decoding encoding intraframe lossy lossless
#>   <chr>      <chr>             <fct> <lgl>    <lgl>    <lgl>      <lgl> <lgl>   
#> 1 012v       Uncompressed 4:2… Video TRUE     FALSE    TRUE       FALSE TRUE    
#> 2 4xm        4X Movie          Video TRUE     FALSE    FALSE      TRUE  FALSE   
#> 3 8bps       QuickTime 8BPS v… Video TRUE     FALSE    TRUE       FALSE TRUE    
#> 4 a64_multi  Multicolor chars… Video FALSE    TRUE     TRUE       TRUE  FALSE   
#> 5 a64_multi5 Multicolor chars… Video FALSE    TRUE     TRUE       TRUE  FALSE   
#> 6 aasc       Autodesk RLE      Video TRUE     FALSE    FALSE      FALSE TRUE

encoders <- ffmpeg_encoders()
head(encoders[encoders$type == "Video", c("name", "details")])
#> # A tibble: 6 × 2
#>   name      details                                                             
#>   <chr>     <chr>                                                               
#> 1 a64multi  Multicolor charset for Commodore 64 (codec a64_multi)               
#> 2 a64multi5 Multicolor charset for Commodore 64, extended with 5th color (colra…
#> 3 alias_pix Alias/Wavefront PIX image                                           
#> 4 amv       AMV Video                                                           
#> 5 apng      APNG (Animated Portable Network Graphics) image                     
#> 6 asv1      ASUS V1

hardware_encoder("h264", "nvenc")
#> [1] "h264_nvenc"
hardware_encoder("h264", "videotoolbox")
#> [1] "h264_videotoolbox"
has_hardware_encoder("h264", "videotoolbox")
#> [1] FALSE
```

[`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
checks how FFmpeg was built. A `TRUE` result does not prove that the
hardware works. When it is `TRUE`, name that hardware in the batch:

``` r

standardize_video_batch(
  jobs, width = 1280, height = 720, fps = 30,
  hardware = "videotoolbox"
)
```

If the hardware you name is not available, the call stops with an error.
So a shared script never changes the codec without telling you. Add
`fallback = TRUE` to use the main processor instead.

The two choices support different codecs. NVIDIA nvenc supports H.264,
HEVC and AV1. Apple videotoolbox supports H.264 and HEVC. A codec that
the hardware does not support is an error that names both.

A hardware encoder trades some quality for speed. At the same bit rate,
a hardware encoder gives a picture that is a little worse than the
software encoder `libx264` or `libx265`. So it suits previews and bulk
conversion, and the software encoder suits a copy that you will keep or
analyze frame by frame.

tidymedia does not do hardware decoding or run filters on a graphics
card. For those, use the direct command
[`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md).

## 2. Prepare the audio

Sound analysis and transcription tools work best with clean audio at an
even loudness.
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
sets a file’s loudness to a target. Here the target is -23
[LUFS](https://jmgirard.github.io/tidymedia/articles/tidymedia.html#glossary),
the level that the EBU R 128 broadcast standard uses.

The output of
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
has one audio
[stream](https://jmgirard.github.io/tidymedia/articles/tidymedia.html#glossary)
and no video. So give it an audio file name. An `.mp4` name would give
you a video file with sound and no picture:

``` r

normalize_audio(session, "session01_camA_norm.wav",
                target_loudness = -23, run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -af \"loudnorm=I=-23:TP=-1:LRA=7,asetnsamples=n=4096:p=0\" -map \"0:a:0\" \"session01_camA_norm.wav\""
```

To set the loudness and keep the picture, first write the audio file as
above. Then use the direct command
[`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
to put it back with the video.

A transcription tool needs the audio in its own file.
[`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
converts the audio to the format that the output extension names. Here
that is a `.wav` file for a speech recognition tool:

``` r

convert_audio(session, "session01_camA.wav", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -q:a 0 -map \"0:a:0\" \"session01_camA.wav\""
```

If you need the audio as it is,
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
copies it without converting it. See
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md).

## 3. Frames for visual coding

Coding facial expressions or gestures frame by frame needs still images.
[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md)
saves one frame at a given time. This is useful for a picture in a
coding manual:

``` r

extract_frame(session, "session01_camA_t30.png", timestamp = 30, run = FALSE)
#> [1] "-y -ss 30 -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -avoid_negative_ts make_zero -qmin 1 -q:v 1 -qscale:v 2 -frames:v 1 -huffman optimal \"session01_camA_t30.png\""
```

[`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md)
saves frames at a fixed rate, as numbered image files. Here it saves one
frame per second, for coding by hand or by a computer vision tool:

``` r

sample_frames("session01_camA_std.mp4", outdir = "frames/session01_camA",
              fps = 1)
```

## 4. De-identify before sharing

Before you share recordings with coders, you often must remove
information that identifies people. Two task functions cover the common
cases.

[`strip_metadata()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata.md)
removes the file’s metadata, such as the camera model, GPS location and
recording time. It does not change the audio or the video:

``` r

strip_metadata(session, "session01_camA_clean.mp4", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:v copy -codec:a copy -map_metadata -1 -map_chapters -1 -fflags +bitexact -map \"0\" \"session01_camA_clean.mp4\""
```

[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
fills one or more rectangles with a solid color. It does not find or
follow faces. You give the position of an area that does not move, such
as a name caption on screen. You give the areas as a data frame, with
one row for each box:

``` r

regions <- tibble::tibble(
  x = 16, y = 640, width = 360, height = 64  # lower-left name caption
)
anonymize_video(session, "session01_camA_deid.mp4", regions = regions,
                run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"crop=w=floor(in_w/2)*2:h=floor(in_h/2)*2:x=(in_w-out_w)/2:y=(in_h-out_h)/2,drawbox=x=16:y=640:w=360:h=64:c=black:t=fill\" -codec:v libx264 -codec:a copy -pix_fmt yuv420p -map \"0:v?\" -map \"0:a?\" \"session01_camA_deid.mp4\""
```

## 5. Assemble and share

If a session was recorded in parts,
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md)
joins the parts into one file. The parts must have the same codec,
[container](https://jmgirard.github.io/tidymedia/articles/tidymedia.html#glossary),
picture size and frame rate. Here they do, because step 1 made them the
same:

``` r

concatenate_videos(
  c("session01_camA_part1.mp4", "session01_camA_part2.mp4"),
  "session01_camA_full.mp4"
)
```

Last,
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md)
makes an H.264 file that most browsers can play. It moves the file’s
index to the start, so coders can watch it in a browser before it has
fully downloaded:

``` r

format_for_web(session, "session01_camA_share.mp4", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"crop=w=floor(in_w/2)*2:h=floor(in_h/2)*2:x=(in_w-out_w)/2:y=(in_h-out_h)/2\" -codec:v libx264 -codec:a aac -pix_fmt yuv420p -movflags +faststart -map \"0:v?\" -map \"0:a?\" \"session01_camA_share.mp4\""
```

## Reproducibility

Each task function returns the FFmpeg command it used. A batch run also
keeps these commands, in a `command` column. Save the commands, and you
have a full record of how each file was made. You can then run the same
steps on the next group of recordings.

The command records what you asked for. A manifest records what
happened: the FFmpeg version, the time, and checksums of the files. Run
the batch with `manifest = TRUE` and read the manifest with
[`ffm_manifest()`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md).

To check that each output has the duration, size and codecs you asked
for, use
[`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md).
[`vignette("verification")`](https://jmgirard.github.io/tidymedia/articles/verification.md)
covers both, and also shows how to stop a file that hangs.

## Where to next

- [`vignette("batch")`](https://jmgirard.github.io/tidymedia/articles/batch.md)
  explains the batch functions in more detail.
- [`vignette("metadata")`](https://jmgirard.github.io/tidymedia/articles/metadata.md)
  shows how to read each file’s metadata into a tibble.
- [`vignette("verification")`](https://jmgirard.github.io/tidymedia/articles/verification.md)
  shows how to check outputs, record how files were made and limit run
  time.
- [`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
  explains the task functions and the pipeline functions.
