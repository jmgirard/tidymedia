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
#
# `extra` appends caller-supplied bullets (ffm_run() adds D046's output
# disposition). `.envir` is what makes that safe: those bullets carry cli fields
# like `{.file {output}}` that resolve only in the CALLER's frame, and cli
# interpolates every bullet in one environment. The caller therefore passes its
# own frame and defines `program`/`limit` there -- which is why the handler in
# ffm_run() reads them off the condition into locals rather than reusing the
# already-formatted message. Re-interpolating that message would re-run glue
# over user data, which is M44's brace trap.
#
# `program` and `limit` also ride on the condition so a handler can rebuild this
# refusal without parsing its text.
abort_timeout <- function(program, limit, extra = NULL,
                          call = rlang::caller_env(),
                          .envir = rlang::current_env()) {
  cli::cli_abort(
    c(
      "{program} timed out after {limit} second{?s}.",
      "i" = "Raise or remove the limit with \\
             {.code options(tidymedia.timeout = )}; {.code 0} means no limit.",
      extra
    ),
    class = "tidymedia_timeout",
    tm_program = program,
    tm_limit = limit,
    call = call,
    .envir = .envir
  )
}

# guard_timeout(): the one wrapper every spawn site shares.
#
# Evaluates `expr` (a system()/system2() call, lazily, inside the handler),
# holds every warning it signals, and turns a timeout kill into abort_timeout().
#
# Warnings are HELD rather than filtered in the handler because the timeout is
# identified by the status, which is not known until the call returns -- the
# same ordering constraint the test-side helper hit at M46. They are held
# unconditionally and dropped on the timeout path, because R's timeout warning
# carries the full command line and the `input=` temp path with it.
#
# `suppress` follows the site's EXISTING behavior rather than imposing one:
# run_program() has always wrapped its system2() in suppressWarnings(), while
# the three Layer 0 hatches have always let a non-zero exit warn. Changing
# either would be a behavior change this milestone did not promise.
guard_timeout <- function(program, limit, expr, suppress = FALSE,
                          call = rlang::caller_env()) {
  held <- character()
  out <- withCallingHandlers(
    expr,
    warning = function(w) {
      held <<- c(held, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  if (is_timeout(out, limit)) abort_timeout(program, limit, call = call)
  if (!suppress) for (msg in held) warning(msg, call. = FALSE)
  out
}

# absorb_timeout(): let a resilient reader count a timeout as one more file it
# could not read.
#
# The metadata readers each document an NA row (or NA value) plus one
# end-of-call warning for a file they cannot read, and D047 makes a timeout no
# exception -- the readers absorb it exactly as they absorb any other failure.
# Without this the abort escapes probe_all()'s purrr::map() and the MediaInfo
# readers' per-file loop, so ONE hung file in a 500-file corpus discards every
# other file's result and falsifies the @return the caller read.
#
# Scoped to `tidymedia_timeout` alone: every other error still propagates, so
# this is a narrow absorber and not a blanket try().
#
# What comes back is a SENTINEL rather than the `NULL` a reader already uses for
# "unreadable", because those two outcomes are not the same fact and the reader
# has to be able to tell them apart. Returning `NULL` for both made a hung file
# indistinguishable from a corrupt one, which is how `ffm_run(verify = )` came
# to report a hung FFprobe as "width: expected 1920, got NA" -- blaming a
# successful encode for the wrong reason. The sentinel carries the program and
# the limit off the condition so a caller that must not absorb (verify_media())
# can rebuild the refusal without parsing anyone's message.
absorb_timeout <- function(expr) {
  rlang::try_fetch(
    expr,
    tidymedia_timeout = function(cnd) {
      structure(
        list(program = cnd$tm_program, limit = cnd$tm_limit),
        class = "tidymedia_absorbed_timeout"
      )
    }
  )
}

is_absorbed_timeout <- function(x) inherits(x, "tidymedia_absorbed_timeout")

# reraise_absorbed(): turn a sentinel back into the abort it stands for.
#
# For the one caller that must not absorb. `verify_media()` asks whether a file
# HAS given properties; a probe that never answered is not an answer of "no",
# so it refuses rather than reporting every property as a mismatch.
reraise_absorbed <- function(x, call = rlang::caller_env()) {
  abort_timeout(x$program, x$limit, call = call)
}
