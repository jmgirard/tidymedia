# Audio track and audio input indices

tidymedia has two 0-based audio arguments that count different things.
This page says which is which, so that meeting one after the other is
not a trap.

## The two indices

`audio_stream` counts **one input's audio streams**. On
[`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
`audio_stream = 1` is that file's second audio track, whatever position
it holds among the file's streams overall (it is not the `index` column
of
[`probe_audio`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
which counts every stream, audio or not).

`audio_input` counts **a verb's inputs**. On
[`compare_videos`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
and
[`picture_in_picture`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
which combine several files into one output and must choose whose sound
to keep, `audio_input = 1` is the second *file*, and says nothing about
which of its tracks is taken.

Neither can be computed from the other, which is why they stay separate
names rather than one argument meaning two things depending on the
verb's arity.

## What `NULL` means, and it is not the same thing

`audio_stream = NULL` is a selection rather than an absence: the verb
still emits a stream map. What differs is how much it selects.

- The first-track family reads `NULL` this way –
  [`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and
  [`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
  plus their `_batch` siblings. The every-track family keeps them all
  instead:
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  [`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
  [`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  and
  [`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
  plus theirs.

- The two readings exist because a verb that writes one audio stream by
  construction must pick one track when you name none, while a verb that
  carries audio through can keep whatever its container holds.

- On the verbs that pass video through, the every-track map is written
  so that it matches nothing rather than failing, so an input with no
  audio at all simply yields an output with none. On
  [`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  and
  [`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
  whose product *is* audio, that same case is an FFmpeg error.

`audio_input = NULL` is different in kind: it emits no audio map at all,
so the output carries **no audio**. A silent output is the default for
[`compare_videos`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
and
[`picture_in_picture`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
because there is no non-arbitrary answer to which of several inputs
should be heard.

Out of range, the two also fail differently. An `audio_input` beyond the
inputs you passed is an R error raised before FFmpeg runs; an
`audio_stream` beyond the input's tracks is an FFmpeg error, because the
track count is a property of the file rather than of the call.

## In a `_batch` jobs table

Both arguments follow one rule on a `_batch` verb: the scalar argument
is the default, and a `jobs` column of the same name overrides it row by
row. (This is how these two behave; it is not a claim about every
`_batch` argument — `hardware`, `parallel` and `two_pass` are batch-wide
and read no column.) An **absent column** means the scalar argument
applies to every row. A **present column** overrides it row by row, and
an `NA` cell is that column's spelling of `NULL` – it does not fall back
to the scalar argument. So `audio_stream = 2` with an `audio_stream`
column holding `NA` puts that row on its family's `NULL` reading, not on
track 2.

## The bare name `audio` is not an index

Layer 1 keeps `audio` for two things that count nothing:

- an audio *codec* string on
  [`ffm_codec`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md),
  where `audio = "aac"` names an encoder;

- a *logical* on
  [`ffm_copy`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md),
  where `audio = TRUE` stream-copies the audio instead of re-encoding
  it.

The input index is `audio_input`, so that its name says what it counts,
as `audio_stream` does.

## See also

[`extract_audio`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
[`convert_audio`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
and
[`normalize_audio`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
for the first-track reading;
[`separate_audio_video`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
[`standardize_video`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
[`anonymize_video`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
[`crop_video`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
[`segment_video`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
and
[`format_for_web`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md)
for the every-track one;
[`compare_videos`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
and
[`picture_in_picture`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
for the input index;
[`probe_audio`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)
to see what tracks a file actually holds.

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
