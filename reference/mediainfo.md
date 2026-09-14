# Run a MediaInfo command

`mediainfo()` runs the MediaInfo program with the arguments in `command`
and returns its output. MediaInfo reads information about media files.

## Usage

``` r
mediainfo(command)
```

## Arguments

- command:

  A string with the arguments to give MediaInfo.

## Value

A character vector with the text that MediaInfo writes to standard
output, one element for each line. Messages on standard error are not
returned. On macOS and Linux, a shell redirect such as `2>&1` in
`command` returns them too.

## Details

`mediainfo()` is a direct command. The package passes `command` to
MediaInfo exactly as you wrote it, so you must add any quotes that it
needs. To get a tibble or a value instead, use
[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md),
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md)
or
[`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md).
These functions quote their arguments for you.

## See also

[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md),
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md)
and
[`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md)
for a tibble or a value.
[`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md)
and the other `get_*()` functions for common single values.

Other direct command functions:
[`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md),
[`ffprobe()`](https://jmgirard.github.io/tidymedia/reference/ffprobe.md)

## Examples

``` r
mediainfo("--Version")
#> [1] "MediaInfo Command line, " "MediaInfoLib - v24.01"   
```
