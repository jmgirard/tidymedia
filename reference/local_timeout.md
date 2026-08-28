# Bound the rest of a function's wall-clock time

Set a wall-clock limit for the remainder of the function you call this
from, without changing the limit the rest of the session runs under.
`seconds` bounds how long each FFmpeg, FFprobe or MediaInfo program
started between this call and the end of that function is waited for —
not how long it runs; a program that ignores the first two signals is
waited for up to 40 seconds longer, described under Details. When the
function ends, by any route, whatever the caller had set before is back
— unless that function discards the undo itself, which is possible and
is described under Details.

This is the statement form of
[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md).
Use
[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
when there is one expression to bound and you can wrap it; use
`local_timeout()` when the thing to bound is the rest of a function
body, or several calls that would be awkward to wrap together.

## Usage

``` r
local_timeout(seconds, .local_envir = parent.frame())
```

## Arguments

- seconds:

  A whole number of seconds. `0` means no limit, so `local_timeout(0)`
  lifts a session limit for the rest of the frame. A value the
  underlying limit could not use — a fraction of a second, a negative
  number, a string — is refused before anything is set.

- .local_envir:

  The environment to bind the limit to. Defaults to the calling frame,
  which is what you want unless you are writing your own helper that
  sets a limit on behalf of *its* caller. It must be a frame that is
  still on the call stack: an environment that never exits — a plain
  [`new.env()`](https://rdrr.io/r/base/environment.html), or a frame
  that has already returned — takes the undo with it, and the limit then
  stays set with no error anywhere.
  [`withr::local_options()`](https://withr.r-lib.org/reference/with_options.html)
  behaves the same way.

## Value

The caller's prior setting, invisibly, as the one-element list
[`options()`](https://rdrr.io/r/base/options.html) returns — the same
shape
[`withr::local_options()`](https://withr.r-lib.org/reference/with_options.html)
gives back.

## Details

The limit applies per spawned program, not per frame: a
`local_timeout()` above a 100-row batch waits `seconds` on each row, not
on the batch. It reaches a `parallel = TRUE` fan-out as well, because
the worker is handed the limit in force when the fan-out starts.

`seconds` bounds the wait, and the wait can exceed it, by the same
arithmetic
[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
describes: `seconds` + 40 for a program that answers none of R's three
signals, measured at 42.0 s under a 2 s limit on Linux and on macOS
alike.

Two calls in one frame stack the way any pair of `local_*()` calls does:
the second is in force until the frame ends, and both are undone
together, back to what the caller had.

There are two ways the restore can be lost, and neither is this function
failing quietly at something it could have done. The undo is registered
as an exit handler on the calling frame, so a frame that writes
`on.exit(...)` of its own *without* `add = TRUE` discards every handler
already registered — this one included — and the limit stays set after
the frame returns, silently and with no error anywhere. Write
`on.exit(..., add = TRUE)` and it does not happen. The second is a
`.local_envir` that is not a live frame, described under that argument
above. This is not particular to this function:
[`withr::defer()`](https://withr.r-lib.org/reference/defer.html) and
[`withr::local_options()`](https://withr.r-lib.org/reference/with_options.html)
lose their undo both ways (measured 2026-08-27 on withr 2.5.0, the
oldest this package accepts, and on 3.0.3, with the same result on
each), because that is how R's exit handlers work. What cannot happen is
the limit being set and the undo never registered: the undo goes on the
frame first, and only then is the limit written.

Written *directly inside* a
[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
expression, `local_timeout()` binds to the frame that wrote the call,
not to the wrapper — `expr` is evaluated in the caller's frame — so its
undo runs after the wrapper's, and the wrapper's limit is what the frame
leaves behind. Put the inner limit in a function of its own, or use one
form or the other. This is what `with_*()` and `local_*()` do together
anywhere in R, not something particular to these two (measured
2026-08-27 against
[`withr::with_options()`](https://withr.r-lib.org/reference/with_options.html)
and
[`withr::local_options()`](https://withr.r-lib.org/reference/with_options.html),
which behave identically, on withr 2.5.0 and 3.0.3 alike).

`seconds` is refused by the rule `options(tidymedia.timeout = )`
applies, with one deliberate exception. Setting the option to `NULL`
REMOVES it, leaving the session unset and therefore unlimited;
`local_timeout(NULL)` is a caller naming no limit at all, and is refused
rather than read as "no limit". Write `local_timeout(0)` for that.

What a reached limit does — abort or warning, by call — is described
under "Bounding a run that hangs" in
[tidymedia-package](https://jmgirard.github.io/tidymedia/reference/tidymedia-package.md);
setting the limit this way changes none of it.

## See also

[`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
for the expression form, and
[tidymedia-package](https://jmgirard.github.io/tidymedia/reference/tidymedia-package.md)
for the session-wide setting and what a reached limit does.

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
