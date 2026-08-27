# Timeout probe values, shared by the refusal tests -----------------------------
#
# `with_timeout()` and `local_timeout()` must not disagree with the option --
# or with each other -- about what a usable limit is: a caller who can pass 0.5
# to one and not the others has three rules to learn, and whichever accepted it
# would hand base R a value it reads as "no limit" (M69/D047). So the probe
# vector lives here, once, and each refusal test scores itself against the
# option's own verdict rather than against a hand-written list of expectations.

tm_seconds_probes <- list(
  0, 1L, 60, 0.5, -1, NA, NA_real_, "2", c(1, 2), Inf, TRUE,
  integer(0), factor("2")
)

tm_probe_label <- function(v) paste(class(v)[[1]], format(v)[1], length(v))

tm_accepts <- function(f) {
  tryCatch({
    f()
    TRUE
  }, error = function(e) FALSE)
}

# The option's own verdict on one probe value -- the reference every wrapper is
# scored against.
tm_option_accepts <- function(v) {
  tm_accepts(function() {
    withr::with_options(list(tidymedia.timeout = v), resolve_timeout())
  })
}
