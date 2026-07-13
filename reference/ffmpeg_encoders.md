# Get a data frame of all installed encoders

Query a list of installed encoders from FFmpeg and construct a tidy data
frame containing information about these encoders.

## Usage

``` r
ffmpeg_encoders(sort_by_type = TRUE)
```

## Arguments

- sort_by_type:

  A logical indicating whether the tibble should be sorted by type and
  then by name (`TRUE`) or just by name (`FALSE`). (default = `TRUE`)

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with the following variables:

- name:

  A character vector including the name/code of each encoder

- details:

  A character vector including details about each encoder

- type:

  A factor vector indicating whether each encoder supports `"Video"`,
  `"Audio"` or `"Subtitles"`

- frame_mt:

  A logical vector indicating whether each encoder supports frame-level
  multithreading

- slice_mt:

  A logical vector indicating whether each encoder supports slice-level
  multithreading

- experimental:

  A logical vector indicating whether each encoder is experimental

- horiz_band:

  A logical vector indicating whether each encoder supports
  draw_horiz_band

- direct_render:

  A logical vector indicating whether each encoders supports direct
  rending method 1

## See also

[`ffmpeg_codecs()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_codecs.md)
for the codec list,
[`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md)
to set a codec in a pipeline, and
[`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
for the Layer 0 escape hatch.

Other capability functions:
[`ffmpeg_codecs()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_codecs.md)

## Examples

``` r
head(ffmpeg_encoders())
#> # A tibble: 6 × 8
#>   name     details type  frame_mt slice_mt experimental horiz_band direct_render
#>   <chr>    <chr>   <fct> <lgl>    <lgl>    <lgl>        <lgl>      <lgl>        
#> 1 a64multi Multic… Video FALSE    FALSE    FALSE        FALSE      TRUE         
#> 2 a64mult… Multic… Video FALSE    FALSE    FALSE        FALSE      TRUE         
#> 3 alias_p… Alias/… Video FALSE    FALSE    FALSE        FALSE      TRUE         
#> 4 amv      AMV Vi… Video FALSE    FALSE    FALSE        FALSE      FALSE        
#> 5 apng     APNG (… Video FALSE    FALSE    FALSE        FALSE      TRUE         
#> 6 asv1     ASUS V1 Video FALSE    FALSE    FALSE        FALSE      TRUE         
ffmpeg_encoders(sort_by_type = FALSE)
#> # A tibble: 223 × 8
#>    name    details type  frame_mt slice_mt experimental horiz_band direct_render
#>    <chr>   <chr>   <fct> <lgl>    <lgl>    <lgl>        <lgl>      <lgl>        
#>  1 a64mul… Multic… Video FALSE    FALSE    FALSE        FALSE      TRUE         
#>  2 a64mul… Multic… Video FALSE    FALSE    FALSE        FALSE      TRUE         
#>  3 aac     AAC (A… Audio FALSE    FALSE    FALSE        FALSE      TRUE         
#>  4 ac3     ATSC A… Audio FALSE    FALSE    FALSE        FALSE      TRUE         
#>  5 ac3_fi… ATSC A… Audio FALSE    FALSE    FALSE        FALSE      TRUE         
#>  6 adpcm_… SEGA C… Audio FALSE    FALSE    FALSE        FALSE      TRUE         
#>  7 adpcm_… ADPCM … Audio FALSE    FALSE    FALSE        FALSE      TRUE         
#>  8 adpcm_… ADPCM … Audio FALSE    FALSE    FALSE        FALSE      TRUE         
#>  9 adpcm_… ADPCM … Audio FALSE    FALSE    FALSE        FALSE      TRUE         
#> 10 adpcm_… ADPCM … Audio FALSE    FALSE    FALSE        FALSE      TRUE         
#> # ℹ 213 more rows
```
