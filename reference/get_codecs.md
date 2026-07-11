# Get a data frame of all installed codecs

Query a list of installed codecs from FFmpeg and construct a tidy data
frame containing information about these codecs.

## Usage

``` r
get_codecs(sort_by_type = TRUE)
```

## Arguments

- sort_by_type:

  A logical indicating whether the tibble should be sorted by type and
  then by name (`TRUE`) or just by name (`FALSE`). (default = `TRUE`)

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with the following variables:

- name:

  A character vector including the name/code of each codec

- details:

  A character vector including details about each codec

- type:

  A factor vector indicating whether each codec supports `"Video"`,
  `"Audio"` or `"Subtitles"`

- decoding:

  A logical vector indicating whether each codec supports decoding

- encoding:

  A logical vector indicating whether each codec supports encoding

- intraframe:

  A logical vector indicating whether each codec is an intra-frame-only
  codec

- lossy:

  A logical vector indicating whether each codec supports lossy
  compression

- lossless:

  A logical vector indicating whether each codec supports lossless
  compression

## See also

Other capability functions:
[`get_encoders()`](https://jmgirard.github.io/tidymedia/reference/get_encoders.md)

## Examples

``` r
head(get_codecs())
#> # A tibble: 6 × 8
#>   name       details           type  decoding encoding intraframe lossy lossless
#>   <chr>      <chr>             <fct> <lgl>    <lgl>    <lgl>      <lgl> <lgl>   
#> 1 012v       Uncompressed 4:2… Video TRUE     FALSE    TRUE       FALSE TRUE    
#> 2 4xm        4X Movie          Video TRUE     FALSE    FALSE      TRUE  FALSE   
#> 3 8bps       QuickTime 8BPS v… Video TRUE     FALSE    TRUE       FALSE TRUE    
#> 4 a64_multi  Multicolor chars… Video FALSE    TRUE     TRUE       TRUE  FALSE   
#> 5 a64_multi5 Multicolor chars… Video FALSE    TRUE     TRUE       TRUE  FALSE   
#> 6 aasc       Autodesk RLE      Video TRUE     FALSE    FALSE      FALSE TRUE    
get_codecs(sort_by_type = FALSE)
#> # A tibble: 517 × 8
#>    name       details          type  decoding encoding intraframe lossy lossless
#>    <chr>      <chr>            <fct> <lgl>    <lgl>    <lgl>      <lgl> <lgl>   
#>  1 012v       Uncompressed 4:… Video TRUE     FALSE    TRUE       FALSE TRUE    
#>  2 4gv        4GV (Fourth Gen… Audio FALSE    FALSE    TRUE       TRUE  FALSE   
#>  3 4xm        4X Movie         Video TRUE     FALSE    FALSE      TRUE  FALSE   
#>  4 8bps       QuickTime 8BPS … Video TRUE     FALSE    TRUE       FALSE TRUE    
#>  5 8svx_exp   8SVX exponential Audio TRUE     FALSE    TRUE       TRUE  FALSE   
#>  6 8svx_fib   8SVX fibonacci   Audio TRUE     FALSE    TRUE       TRUE  FALSE   
#>  7 a64_multi  Multicolor char… Video FALSE    TRUE     TRUE       TRUE  FALSE   
#>  8 a64_multi5 Multicolor char… Video FALSE    TRUE     TRUE       TRUE  FALSE   
#>  9 aac        AAC (Advanced A… Audio TRUE     TRUE     TRUE       TRUE  FALSE   
#> 10 aac_latm   AAC LATM (Advan… Audio TRUE     FALSE    TRUE       TRUE  FALSE   
#> # ℹ 507 more rows
```
