# Audio track and audio input indices

Two audio arguments in this package count different things:
`audio_stream` and `audio_input`. Both count from `0`, so `0` means the
first one. This page explains which is which.

The glossary in
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
explains media terms such as stream, container and codec.

## The two indices

`audio_stream` counts **the audio tracks of one input file**. On
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
`audio_stream = 1` is the second audio track of the file. Where that
track sits among all the streams of the file does not matter. So
`audio_stream` is not the `index` column of
[`probe_audio()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
which counts every stream, audio or not.

`audio_input` counts **the input files of a function**. The functions
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
and
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
combine several files into one output, so they must choose whose sound
to keep. On these functions, `audio_input = 1` is the second *file*. It
says nothing about which track of that file is used.

You cannot work out one index from the other. So the package keeps two
names, rather than one argument whose meaning depends on how many inputs
a function takes.

## What `NULL` means

`audio_stream = NULL` still selects audio. It does not mean "no audio".
How much audio it selects depends on the function.

- The first-track family reads `NULL` as the first audio track only:
  [`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and
  [`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
  and their `_batch` forms. The every-track family reads it as every
  audio track:
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
  [`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  and
  [`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
  and their `_batch` forms.

- The two readings have a reason. A function that writes one audio
  stream must pick one track when you name none. A function that carries
  audio through can keep all the tracks its container holds.

- On the functions that pass video through, an input with no audio gives
  an output with no audio, not an error. On
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  and
  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
  whose output *is* audio, that input gives an FFmpeg error.

`audio_input = NULL` is different: it selects no audio at all, so the
output has **no audio**. A silent output is the default for
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
and
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md).
With several inputs, no choice of which one to hear is better than
another.

The two arguments also fail in different ways when a number is too
large. An `audio_input` that names an input you did not pass gives an R
error, before FFmpeg runs. An `audio_stream` that names a track the
input does not have gives an FFmpeg error. The reason is that the number
of tracks is a fact about the file, not about the call.

## In a `_batch` jobs table

On a `_batch` function, both arguments follow one rule. The argument you
pass is the default, and a `jobs` column with the same name overrides it
row by row.

This rule is about these two arguments only. The arguments `hardware`,
`parallel` and `two_pass` apply to the whole batch, and the function
reads no column for them.

If the column is absent, the argument applies to every row. If the
column is present, each row uses its own cell. An `NA` cell means `NULL`
for that row. It does not fall back to the argument. So
`audio_stream = 2` with an `NA` cell in an `audio_stream` column gives
that row the `NULL` reading of its family, not track 2.

## The name `audio` alone is not an index

The pipeline functions use `audio` for two things that are not counts:

- an audio codec name on
  [`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md),
  where `audio = "aac"` names an encoder;

- a logical on
  [`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md),
  where `audio = TRUE` copies the audio stream without re-encoding it.

The input index is called `audio_input`, so that its name says what it
counts, as `audio_stream` does.

## See also

[`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
and
[`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
read `NULL` as the first audio track.
[`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
and
[`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md)
read it as every audio track.
[`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
and
[`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
take the input index.
[`probe_audio()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)
shows which audio tracks a file has.

Other audio selection functions:
[`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
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
[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
[`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
