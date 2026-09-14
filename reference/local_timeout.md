# Set a time limit for the rest of a function

`local_timeout()` sets a time limit for the rest of the function that
calls it. The limit applies to each FFmpeg, FFprobe or MediaInfo program
that the function starts after this call. When the function returns, or
stops with an error, the caller's own limit is back, except in the cases
in Details.

Use
[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
to set a limit on one expression. Use `local_timeout()` to set a limit
on the rest of a function, or on several calls that are hard to wrap in
one expression.

## Usage

``` r
local_timeout(seconds, .local_envir = parent.frame())
```

## Arguments

- seconds:

  A whole number of seconds. `0` means no limit, so `local_timeout(0)`
  removes a session limit for the rest of the function. A fraction, a
  negative number, a string or `NULL` gives an error, and the limit does
  not change.

- .local_envir:

  The environment that holds the limit. The default is the function that
  calls `local_timeout()`. Change it only when you write a helper that
  sets a limit for its own caller. Inside a function, the environment
  must belong to a function that is still running. Suppose it belongs to
  a function that has returned, or it is an environment such as
  [`new.env()`](https://rdrr.io/r/base/environment.html). Then the limit
  stays set with no error.
  [`withr::local_options()`](https://withr.r-lib.org/reference/with_options.html)
  works the same way. At the top level of a script or the console,
  [`withr::defer()`](https://withr.r-lib.org/reference/defer.html)
  decides when the limit is undone.

## Value

The caller's earlier setting, invisibly. It is a list with one element,
the same form that
[`withr::local_options()`](https://withr.r-lib.org/reference/with_options.html)
returns.

## Details

The limit applies to each program, not to the whole function. In a
100-row batch after `local_timeout(600)`, each program that a row starts
gets 600 seconds, plus the delay that
[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
describes. The workers of a `parallel = TRUE` run use the same limit.

R can wait up to 40 seconds past the limit, and
[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
explains why. It also explains what happens when a limit is reached.

Two calls in one function work like any two `local_*()` calls. The
second limit applies until the function ends. Then both are undone, and
the caller's limit is back.

In three cases, the caller's limit is not back when the function ends,
and there is no error.

- The function calls [`on.exit()`](https://rdrr.io/r/base/on.exit.html)
  without `add = TRUE`. That call removes the undo step. Write
  `on.exit(..., add = TRUE)` instead.

- The `.local_envir` belongs to a function that has returned, or it is
  an environment such as
  [`new.env()`](https://rdrr.io/r/base/environment.html).

- `local_timeout()` is called directly inside the expression of
  [`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md).
  There, `local_timeout()` belongs to the function around it. So when
  that function ends, the limit that
  [`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
  set is in force. Put the inner limit in a function of its own, or use
  only one of the two.

The first two cases also apply to
[`withr::local_options()`](https://withr.r-lib.org/reference/with_options.html),
because R's exit handlers work this way. They do not apply to
[`withr::with_options()`](https://withr.r-lib.org/reference/with_options.html),
which puts the option back itself.

## See also

[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
to set a limit for one expression.
[tidymedia-package](https://jmgirard.github.io/tidymedia/reference/tidymedia-package.md)
describes the session options.

## Examples

``` r
bounded <- function() {
  local_timeout(30)
  getOption("tidymedia.timeout")
}

# In force for the rest of that function...
bounded()
#> [1] 30

# ...and gone once it has returned.
getOption("tidymedia.timeout", default = "unset")
#> [1] "unset"

if (FALSE) { # \dontrun{
# Bound every program a whole function starts, at five minutes.
convert_all <- function(files) {
  local_timeout(300)
  for (f in files) extract_audio(f, sub("[.][^.]*$", ".wav", f))
}
} # }
```
