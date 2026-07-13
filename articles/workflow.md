# A research preprocessing workflow

``` r

library(tidymedia)
```

This vignette walks a realistic end-to-end pipeline: preparing a set of
recorded **dyadic-interaction sessions** — two people in conversation,
one camera each — for behavioral coding and acoustic analysis. It is the
kind of job tidymedia is built for: the same handful of steps applied
identically to every file, with a saved command for each so the whole
pipeline is reproducible.

Every step below uses `run = FALSE`, which **compiles** the FFmpeg
command without executing it, so you can read exactly what each verb
would do. Drop that argument to actually process the files. Chunks that
describe operating over a whole folder are shown but not evaluated;
where a step *is* evaluated we use the tiny sample clip that ships with
the package:

``` r

session <- system.file("extdata", "sample.mp4", package = "tidymedia")
```

Picture a study folder with one file per camera per session:

``` r

recordings <- list.files("study/raw", pattern = "\\.mp4$", full.names = TRUE)
recordings
#> [1] "study/raw/session01_camA.mp4" "study/raw/session01_camB.mp4"
#> [2] "study/raw/session02_camA.mp4" "study/raw/session02_camB.mp4"
#> ...
```

## 1. Standardize the recordings

Cameras rarely agree on resolution, frame rate, or codec, and
inconsistent inputs make everything downstream harder.
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
re-encodes a file to a common target:

``` r

standardize_video(
  session, "session01_camA_std.mp4",
  width = 1280, height = 720, fps = 30,
  run = FALSE
)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"scale=w=1280:h=720,fps=30\" -codec:v libx264 -codec:a copy -pix_fmt yuv420p -movflags +faststart \"session01_camA_std.mp4\""
```

Because this runs the same way for every file, reach for the batch
sibling to do the whole folder from a jobs tibble (see
[`vignette("batch")`](https://jmgirard.github.io/tidymedia/articles/batch.md)
for the batch model). Every task verb has a `*_batch()` companion like
this one:

``` r

jobs <- tibble::tibble(
  input  = recordings,
  output = sub("/raw/", "/std/", recordings)
)
standardize_video_batch(jobs, width = 1280, height = 720, fps = 30)
```

## 2. Prepare the audio

Acoustic and transcription tools want clean, consistently loud audio.
[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
brings a file to a target integrated loudness (here −23 LUFS, the EBU
R128 broadcast reference):

``` r

normalize_audio(session, "session01_camA_norm.mp4",
                target_loudness = -23, run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -af \"loudnorm=I=-23:TP=-1:LRA=7\" -codec:v copy \"session01_camA_norm.mp4\""
```

To hand the speech to a transcription or acoustic-analysis tool, pull
the audio into a standalone file.
[`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
transcodes to whatever the output extension implies — a `.wav` for an
automatic-speech-recognition pipeline:

``` r

convert_audio(session, "session01_camA.wav", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -q:a 0 -map a \"session01_camA.wav\""
```

(If you only need the audio *without* re-encoding,
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
stream-copies it instead — see
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md).)

## 3. Frames for visual coding

Frame-by-frame coding of facial behavior or gesture needs stills.
[`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md)
grabs a single frame at a timestamp — handy for a coding-manual
illustration:

``` r

extract_frame(session, "session01_camA_t30.png", timestamp = 30, run = FALSE)
#> [1] "-y -ss 30 -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -avoid_negative_ts make_zero -qmin 1 -q:v 1 -qscale:v 2 -frames:v 1 -huffman optimal \"session01_camA_t30.png\""
```

[`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md)
samples at a fixed rate into a numbered image sequence — one frame per
second here, ready for a manual or computer-vision coding pass:

``` r

sample_frames("session01_camA_std.mp4", outdir = "frames/session01_camA",
              fps = 1)
```

## 4. De-identify before sharing

Sharing recordings with remote coders usually means removing identifying
information first. Two verbs cover the common cases.

[`strip_metadata()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata.md)
removes container-level metadata — camera model, GPS coordinates,
recording timestamps — without touching the audio or video streams:

``` r

strip_metadata(session, "session01_camA_clean.mp4", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -codec:v copy -codec:a copy -map_metadata -1 -map_chapters -1 -fflags +bitexact -map 0 \"session01_camA_clean.mp4\""
```

[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
box-fills one or more **fixed rectangles** you specify. This is not face
tracking — you give it the coordinates of a region that stays put, such
as an on-screen name caption or a fixed nameplate, and it paints a solid
box over it. Regions are a data frame, one row per box:

``` r

regions <- tibble::tibble(
  x = 16, y = 640, width = 360, height = 64  # lower-left name caption
)
anonymize_video(session, "session01_camA_deid.mp4", regions = regions,
                run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"crop=w=floor(in_w/2)*2:h=floor(in_h/2)*2:x=(in_w-out_w)/2:y=(in_h-out_h)/2,drawbox=x=16:y=640:w=360:h=64:c=black:t=fill\" -codec:v libx264 -codec:a copy -pix_fmt yuv420p \"session01_camA_deid.mp4\""
```

## 5. Assemble and share

If a session was recorded in parts,
[`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md)
joins them back into one file (the parts must share a codec and
container — they do here, since step 1 standardized them):

``` r

concatenate_videos(
  c("session01_camA_part1.mp4", "session01_camA_part2.mp4"),
  "session01_camA_full.mp4"
)
```

Finally,
[`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md)
produces a lightweight, broadly compatible H.264 file with fast-start
enabled, so coders can stream it in a browser without a large download:

``` r

format_for_web(session, "session01_camA_share.mp4", run = FALSE)
#> [1] "-y -i \"/home/runner/work/_temp/Library/tidymedia/extdata/sample.mp4\" -vf \"crop=w=floor(in_w/2)*2:h=floor(in_h/2)*2:x=(in_w-out_w)/2:y=(in_h-out_h)/2\" -codec:v libx264 -codec:a aac -pix_fmt yuv420p -movflags +faststart \"session01_camA_share.mp4\""
```

## Reproducibility

Every verb returns the exact FFmpeg command it compiled. Capturing those
strings — or running the pipeline through the batch runner, which
collects them in a `command` column — gives you a complete, re-runnable
record of how each processed file was produced. That record is the
point: the pipeline above is not a one-off, but a specification you can
re-apply to the next cohort of recordings.

## Where to next

- [`vignette("batch")`](https://jmgirard.github.io/tidymedia/articles/batch.md)
  — the jobs-tibble batch runner in depth.
- [`vignette("metadata")`](https://jmgirard.github.io/tidymedia/articles/metadata.md)
  — reading each file’s metadata as a tibble.
- [`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
  — the task verbs and the builder beneath them.
