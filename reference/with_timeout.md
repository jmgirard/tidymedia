# Bound one call's wall-clock time

Run `expr` under a wall-clock limit of your own, without changing the
limit the rest of the session runs under. Every FFmpeg, FFprobe or
MediaInfo program started while `expr` is being evaluated is bounded by
`seconds`; when the call ends, by any route, whatever the session had
set before is back.

The session-wide setting, `options(tidymedia.timeout = )`, answers "how
long may anything in this session take". This answers "how long may
*this* take" — a five-minute bound on one exploratory conversion in a
session whose limit is an hour, or an hour for one long encode in a
session bounded at five minutes.

## Usage

``` r
with_timeout(expr, seconds)
```

## Arguments

- expr:

  An expression to evaluate. It is evaluated once, where you wrote it,
  and its value is returned.

- seconds:

  A whole number of seconds. `0` means no limit, so
  `with_timeout(expr, 0)` lifts a session limit for one call. A value
  the underlying limit could not use — a fraction of a second, a
  negative number, a string — is refused before `expr` runs.

## Value

The value of `expr`.

## Details

The limit applies per spawned program, not per call: a `with_timeout()`
around a 100-row batch bounds each row at `seconds`, not the batch. It
reaches a `parallel = TRUE` fan-out as well, because the worker is
handed the limit in force when the fan-out starts.

What a reached limit does — abort or warning, by call — is described
under "Bounding a run that hangs" in
[tidymedia-package](https://jmgirard.github.io/tidymedia/reference/tidymedia-package.md);
setting the limit this way changes none of it.

## See also

[tidymedia-package](https://jmgirard.github.io/tidymedia/reference/tidymedia-package.md)
for the session-wide setting and what a reached limit does.

## Examples

``` r
# Inside the call, the limit is the one you gave.
with_timeout(getOption("tidymedia.timeout"), 30)
#> [1] 30

# Outside it, the session's own setting is untouched.
getOption("tidymedia.timeout", default = "unset")
#> [1] "unset"

if (FALSE) { # \dontrun{
# Bound one conversion at five minutes, whatever the session is set to.
with_timeout(extract_audio("in.mp4", "out.wav"), 300)
} # }
```
