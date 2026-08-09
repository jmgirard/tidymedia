# Runtime timeout seam -------------------------------------------------------

# The package's second option seam (after `tidymedia.nvenc_encoders`), and the
# only one that changes what happens rather than what is reported. Every process
# tidymedia spawns passes resolve_timeout() to base R's `timeout=`, so a hung
# FFmpeg stops the call instead of the session (M69/D047).
#
# The default is 0 -- base R's sentinel for "no limit" -- so every existing call
# behaves exactly as it did. A ceiling default would abort a legitimate
# multi-hour transcode that finishes today, which is why the seam is opt-in.

# resolve_timeout(): read the option, refuse anything base R would mishandle,
# and return the limit in whole seconds (0 meaning no limit).
#
# The whole-number rule is not fussiness. base R's `timeout=` truncates toward
# zero, so a value below 1 becomes 0 -- and 0 is the "no limit" sentinel, so
# `options(tidymedia.timeout = 0.5)` would silently leave the call UNBOUNDED
# (measured on R 4.6.1: a 6 s child ran to completion under a 0.5 s limit).
# Refusing the fractional value is the only reading that cannot surprise; the
# alternative, rounding up, silently substitutes a limit the caller did not ask
# for. Nothing downstream will catch a bad value for us either: system2()
# accepts both "2" and c(1, 2) without complaint.
resolve_timeout <- function(call = rlang::caller_env()) {
  limit <- getOption("tidymedia.timeout", default = 0)
  # `min = 0` covers the negative case; check_number_whole() covers NA, the
  # string form, the length-2 form and the fractional form, and its own message
  # already says "a whole number", which the caller needs to hear.
  rlang::check_number_whole(
    limit,
    min = 0,
    arg = "tidymedia.timeout",
    call = call
  )
  as.numeric(limit)
}

# is_timeout(): did this result come back because the limit killed the child?
#
# Keyed on the `status` attribute, NEVER on the text of R's timeout warning.
# That warning is translated under a non-English locale -- under LANGUAGE=de it
# reads "Zeitüberschreitung bei Kommando ..." -- so a text match is a guard that
# silently stops working abroad (M46's lesson, paid for once already).
#
# `limit > 0` is load-bearing, not belt-and-braces: 124 is an ordinary exit
# status a program may return for its own reasons, and it only means "killed by
# the timeout" when a timeout was actually in force.
is_timeout <- function(out, limit) {
  if (limit <= 0) return(FALSE)
  status <- attr(out, "status")
  # system() and system2() are not consistent about typing the status, so
  # compare as integer rather than with identical() on the raw value.
  !is.null(status) && identical(as.integer(status), 124L)
}

# abort_timeout(): the one refusal every spawn site shares.
#
# Names the program and the limit and NOTHING ELSE. R's own timeout warning
# embeds the full command line, including the `input=` temp-file path; the spawn
# sites drop that warning and this message replaces it, so the path never
# reaches the caller (M46 review finding B, same trap).
abort_timeout <- function(program, limit, call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "{program} timed out after {limit} second{?s}.",
      "i" = "Raise or remove the limit with \\
             {.code options(tidymedia.timeout = )}; {.code 0} means no limit."
    ),
    class = "tidymedia_timeout",
    call = call
  )
}
