# Run MediaInfo CLI

Run the command through the MediaInfo command line interface (CLI) and
return its output as a string. This is the Layer 0 escape hatch:
`command` is passed to MediaInfo verbatim, so you are responsible for
quoting it. For structured, tibble-returning output use
[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md),
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md),
or
[`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md),
which quote their arguments safely.

## Usage

``` r
mediainfo(command)
```

## Arguments

- command:

  A string containing a mediainfo command.

## Value

A string containing the command line output from mediainfo.

## See also

[`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md),
[`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md),
and
[`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md)
for structured output, and
[`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md)
and friends for common scalars.

Other escape hatch functions:
[`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md),
[`ffprobe()`](https://jmgirard.github.io/tidymedia/reference/ffprobe.md)

## Examples

``` r
mediainfo("--Version")
#> [1] "MediaInfo Command line, " "MediaInfoLib - v24.01"   
```
