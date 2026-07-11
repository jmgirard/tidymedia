# Convert string fractions to doubles

This is useful for columns such as frame rates, which FFprobe often
lists as fractions such as `"30000/1001"` (this converts to 29.97003).

## Usage

``` r
convert_fractions(x)
```

## Arguments

- x:

  A character vector containing fractions (`"a/b"`) or plain numbers to
  evaluate. Surrounding whitespace is ignored; `NA` passes through.

## Value

A numeric vector with each fraction evaluated to a double.

## See also

Other utility functions:
[`pad_integers()`](https://jmgirard.github.io/tidymedia/reference/pad_integers.md)

## Examples

``` r
# FFprobe often reports frame rates as fractions
convert_fractions(c("30000/1001", "25", NA))
#> [1] 29.97003 25.00000       NA
```
