# Pad Integers

Takes in a numeric vector of integerish values and returns a character
vector of the same length with padding. The width of padding can be
specified or intuited from the maximum value. The padding flag can be
configured.

## Usage

``` r
pad_integers(x, width = NULL, flag = "0")
```

## Arguments

- x:

  A numeric vector of integerish values (i.e., either R integers or
  integer-like doubles). See
  [`rlang::is_integerish()`](https://rlang.r-lib.org/reference/is_integerish.html)
  for details.

- width:

  Either NULL or a single integerish value specifying the width of
  padding to use. If NULL, the width of the maximum value in `x` is used
  (i.e., the minimum padding needed to standardize the width of all
  values).

- flag:

  A string specifying what to pad `x` with. (default = "0")

## Value

A character vector the same length as `x` but with padding added with
the specifying width and flag.

## See also

Other utility functions:
[`convert_fractions()`](https://jmgirard.github.io/tidymedia/reference/convert_fractions.md)

## Examples

``` r
# Width is intuited from the largest value by default
pad_integers(c(1, 22, 333))
#> [1] "001" "022" "333"
# ... or set it explicitly
pad_integers(1:3, width = 4)
#> [1] "0001" "0002" "0003"
```
