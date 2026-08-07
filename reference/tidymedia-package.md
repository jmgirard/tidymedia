# tidymedia: Tools for working with media files within R and the tidyverse

The goal of tidymedia is to provide tools for easily working with media
(e.g., image, audio, and video) files within R and the tidyverse.

## Details

tidymedia is organized in three layers. Layer 0 passes raw arguments to
the command-line tools:
[`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md),
[`ffprobe()`](https://jmgirard.github.io/tidymedia/reference/ffprobe.md),
and
[`mediainfo()`](https://jmgirard.github.io/tidymedia/reference/mediainfo.md).
Layer 1 is the pipeline builder, which assembles an FFmpeg command step
by step and then compiles or runs it:
[`ffm()`](https://jmgirard.github.io/tidymedia/reference/ffm.md) and the
`ffm_*()` functions. Layer 2 is the task verbs, thin wrappers over the
builder for common preprocessing jobs such as
[`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
and
[`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md).
Metadata is read by two independent back ends: FFprobe, through
[`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
and the other `probe_*()` readers, which return container and stream
tibbles; and MediaInfo, through
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md)
and the `get_*()` helpers such as
[`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md),
which return a single value.

See
[`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)
for the guided tour,
[`vignette("batch")`](https://jmgirard.github.io/tidymedia/articles/batch.md)
for running a verb over many files,
[`vignette("metadata")`](https://jmgirard.github.io/tidymedia/articles/metadata.md)
for the readers, and
[`vignette("workflow")`](https://jmgirard.github.io/tidymedia/articles/workflow.md)
for an end-to-end research preprocessing pipeline. The full function
list is on the package's reference index.

## See also

Useful links:

- <https://github.com/jmgirard/tidymedia>

- <https://jmgirard.github.io/tidymedia/>

- Report bugs at <https://github.com/jmgirard/tidymedia/issues>

## Author

**Maintainer**: Jeffrey Girard <me@jmgirard.com>
([ORCID](https://orcid.org/0000-0002-7359-3746))

Authors:

- Jeffrey Girard <me@jmgirard.com>
  ([ORCID](https://orcid.org/0000-0002-7359-3746))
