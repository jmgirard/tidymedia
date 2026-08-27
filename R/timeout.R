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

# The caller's per-call limit ------------------------------------------------

#' Bound one call's wall-clock time
#'
#' @description
#' Run `expr` under a wall-clock limit of your own, without changing the limit
#' the rest of the session runs under. Every FFmpeg, FFprobe or MediaInfo
#' program started while `expr` is being evaluated is bounded by `seconds`; when
#' the call ends, by any route, whatever the session had set before is back.
#'
#' The session-wide setting, `options(tidymedia.timeout = )`, answers "how long
#' may anything in this session take". This answers "how long may *this* take" —
#' a five-minute bound on one exploratory conversion in a session whose limit is
#' an hour, or an hour for one long encode in a session bounded at five minutes.
#'
#' @param expr An expression to evaluate. It is evaluated once, where you wrote
#'   it, and its value is returned.
#' @param seconds A whole number of seconds. `0` means no limit, so
#'   `with_timeout(expr, 0)` lifts a session limit for one call. A value the
#'   underlying limit could not use — a fraction of a second, a negative number,
#'   a string — is refused before `expr` runs.
#'
#' @return The value of `expr`.
#'
#' @details
#' The limit applies per spawned program, not per call: a `with_timeout()`
#' around a 100-row batch bounds each row at `seconds`, not the batch. It
#' reaches a `parallel = TRUE` fan-out as well, because the worker is handed the
#' limit in force when the fan-out starts.
#'
#' What a reached limit does — abort or warning, by call — is described under
#' "Bounding a run that hangs" in [tidymedia-package]; setting the limit this
#' way changes none of it.
#'
#' @seealso [local_timeout()] for the statement form — bounding the rest of a
#'   function body rather than a wrapped expression — and [tidymedia-package] for
#'   the session-wide setting and what a reached limit does.
#'
#' @examples
#' # Inside the call, the limit is the one you gave.
#' with_timeout(getOption("tidymedia.timeout"), 30)
#'
#' # Outside it, the session's own setting is untouched.
#' getOption("tidymedia.timeout", default = "unset")
#'
#' \dontrun{
#' # Bound one conversion at five minutes, whatever the session is set to.
#' with_timeout(extract_audio("in.mp4", "out.wav"), 300)
#' }
#'
#' @export
with_timeout <- function(expr, seconds) {
  # Both formals are guarded here, and both before the option is written. Left
  # to base R, an omitted `expr` said `argument "expr" is missing, with no
  # default` -- naming this function's internal parameter at a caller who wrote
  # a call, not a definition, while an omitted `seconds` already got rlang's
  # own refusal. check_required() does not force the promise (measured
  # 2026-08-27 on rlang 1.3.0), so `expr` is still evaluated once, later, in
  # the caller's frame, under the option.
  rlang::check_required(expr)
  # Eagerly, and BEFORE the option is written: a caller who passed a limit base
  # R cannot use should hear about the limit rather than watch `expr` run
  # unbounded. `arg = "seconds"` because that is the name they wrote --
  # resolve_timeout() names the option instead, for the caller who set one.
  # The check is the same one resolve_timeout() applies, so this function
  # accepts exactly the values the option accepts -- with one exception, and it
  # is deliberate: `options(tidymedia.timeout = NULL)` REMOVES the name, so
  # resolve_timeout() then reads the 0 default and accepts it, while
  # with_timeout(expr, NULL) is a caller naming no limit at all and is refused.
  rlang::check_number_whole(seconds, min = 0, arg = "seconds")
  # options() returns the prior value of exactly the name being set, and
  # on.exit() puts it back on the erroring path as well as the returning one --
  # the pair carry_options() already uses below, for the same reason. A name
  # that was UNSET comes back as a NULL entry, and options(list(x = NULL))
  # removes the name rather than storing NULL, so an unset option is unset
  # again afterwards (measured on R 4.6.1).
  prior <- options(tidymedia.timeout = as.numeric(seconds))
  on.exit(options(prior), add = TRUE)
  # `expr` is a promise, so forcing it here evaluates it once, in the caller's
  # frame, under the option just established -- and the restore above runs
  # after that value is in hand.
  expr
}

#' Bound the rest of a function's wall-clock time
#'
#' @description
#' Set a wall-clock limit for the remainder of the function you call this from,
#' without changing the limit the rest of the session runs under. Every FFmpeg,
#' FFprobe or MediaInfo program started between this call and the end of that
#' function is bounded by `seconds`; when the function ends, by any route,
#' whatever the caller had set before is back — unless that function discards
#' the undo itself, which is possible and is described under Details.
#'
#' This is the statement form of [with_timeout()]. Use `with_timeout()` when
#' there is one expression to bound and you can wrap it; use `local_timeout()`
#' when the thing to bound is the rest of a function body, or several calls that
#' would be awkward to wrap together.
#'
#' @param seconds A whole number of seconds. `0` means no limit, so
#'   `local_timeout(0)` lifts a session limit for the rest of the frame. A value
#'   the underlying limit could not use — a fraction of a second, a negative
#'   number, a string — is refused before anything is set.
#' @param .local_envir The environment to bind the limit to. Defaults to the
#'   calling frame, which is what you want unless you are writing your own
#'   helper that sets a limit on behalf of *its* caller. It must be a frame that
#'   is still on the call stack: an environment that never exits — a plain
#'   [new.env()], or a frame that has already returned — takes the undo with it,
#'   and the limit then stays set with no error anywhere. [withr::local_options()]
#'   behaves the same way.
#'
#' @return The caller's prior setting, invisibly, as the one-element list
#'   `options()` returns — the same shape [withr::local_options()] gives back.
#'
#' @details
#' The limit applies per spawned program, not per frame: a `local_timeout()`
#' above a 100-row batch bounds each row at `seconds`, not the batch. It reaches
#' a `parallel = TRUE` fan-out as well, because the worker is handed the limit in
#' force when the fan-out starts.
#'
#' Two calls in one frame stack the way any pair of `local_*()` calls does: the
#' second is in force until the frame ends, and both are undone together, back
#' to what the caller had.
#'
#' There are two ways the restore can be lost, and neither is this function
#' failing quietly at something it could have done. The undo is registered as an
#' exit handler on the calling frame, so a frame that writes `on.exit(...)` of
#' its own *without* `add = TRUE` discards every handler already registered —
#' this one included — and the limit stays set after the frame returns, silently
#' and with no error anywhere. Write `on.exit(..., add = TRUE)` and it does not
#' happen. The second is a `.local_envir` that is not a live frame, described
#' under that argument above. This is not particular to this function:
#' [withr::defer()] and [withr::local_options()] lose their undo both ways
#' (measured 2026-08-27 on withr 2.5.0, the oldest this package accepts, and on
#' 3.0.3, with the same result on each), because that is how R's exit handlers
#' work. What cannot happen is the limit being set and the undo never
#' registered: the undo goes on the frame first, and only then is the limit
#' written.
#'
#' Written *directly inside* a [with_timeout()] expression, `local_timeout()`
#' binds to the frame that wrote the call, not to the wrapper — `expr` is
#' evaluated in the caller's frame — so its undo runs after the wrapper's, and
#' the wrapper's limit is what the frame leaves behind. Put the inner limit in a
#' function of its own, or use one form or the other. This is what `with_*()`
#' and `local_*()` do together anywhere in R, not something particular to these
#' two (measured 2026-08-27 against [withr::with_options()] and
#' [withr::local_options()], which behave identically, on withr 2.5.0 and 3.0.3
#' alike).
#'
#' `seconds` is refused by the rule `options(tidymedia.timeout = )` applies, with
#' one deliberate exception. Setting the option to `NULL` REMOVES it, leaving the
#' session unset and therefore unlimited; `local_timeout(NULL)` is a caller
#' naming no limit at all, and is refused rather than read as "no limit". Write
#' `local_timeout(0)` for that.
#'
#' What a reached limit does — abort or warning, by call — is described under
#' "Bounding a run that hangs" in [tidymedia-package]; setting the limit this way
#' changes none of it.
#'
#' @seealso [with_timeout()] for the expression form, and [tidymedia-package] for
#'   the session-wide setting and what a reached limit does.
#'
#' @examples
#' bounded <- function() {
#'   local_timeout(30)
#'   getOption("tidymedia.timeout")
#' }
#'
#' # In force for the rest of that function...
#' bounded()
#'
#' # ...and gone once it has returned.
#' getOption("tidymedia.timeout", default = "unset")
#'
#' \dontrun{
#' # Bound every program a whole function starts, at five minutes.
#' convert_all <- function(files) {
#'   local_timeout(300)
#'   for (f in files) extract_audio(f, sub("[.][^.]*$", ".wav", f))
#' }
#' }
#'
#' @export
local_timeout <- function(seconds, .local_envir = parent.frame()) {
  # The same check with_timeout() applies, before anything is written, so a
  # caller who mistyped a limit hears about it rather than running the rest of
  # the frame under a limit they did not ask for.
  rlang::check_number_whole(seconds, min = 0, arg = "seconds")
  # The prior value is READ, the undo REGISTERED, and only then the new value
  # WRITTEN -- withr::local_options()'s own order, and the reason for it is that
  # defer() can fail. A `.local_envir` that is not an environment aborts inside
  # defer(), and with the write already done there is nothing left to put the
  # caller's value back: the limit would stay set for the rest of the session
  # (measured 2026-08-27, option left at this function's value where the caller
  # had 99, against 99 through withr::local_options() given the same bad
  # argument). Registering first makes any failure below leave the session as it
  # was found.
  #
  # `list(name = getOption(name))` is the same shape options() returns for the
  # name being set -- length 1, the entry NULL when the option is unset
  # (verified identical on R 4.6.1) -- and feeding a NULL entry back to
  # options() REMOVES the name rather than storing NULL, so an unset option is
  # unset again afterwards. Same pair with_timeout() uses.
  prior <- list(tidymedia.timeout = getOption("tidymedia.timeout"))
  # withr::defer() rather than base on.exit(): it PREPENDS its handler
  # (`after = FALSE`), which is what makes two calls in one frame restore to the
  # CALLER's state rather than to the first call's, where a plain
  # `on.exit(add = TRUE)` appends and restores to the first call's. It also
  # handles a global or knitr target environment, which a hand-rolled
  # `do.call(on.exit, ..., envir = )` does not.
  #
  # What it does NOT buy is an unclobberable restore, and an earlier version of
  # this comment claimed it did. defer() ends in
  # `do.call(base::on.exit, list(thunk, TRUE, after), envir = envir)`, so a
  # calling frame writing its own `on.exit()` without `add = TRUE` discards this
  # exactly as it would a base one -- measured 2026-08-27 on withr 3.0.3 and on
  # 2.5.0, the declared floor, with the option left at this function's value
  # where the caller had 99 on both. That hole is stated in the @details above
  # rather than papered over.
  #
  # The version spread is measured, not assumed: withr 3.0.0 rewrote defer()'s
  # globalenv() branch, but local_timeout() does not reach it -- parent.frame()
  # is a live function frame in an ordinary call, and at the top level of a
  # source()d file it is source()'s own eval frame. data-raw/withr-floor.R
  # re-runs the whole comparison; D053 records what it found.
  withr::defer(options(prior), envir = .local_envir)
  options(tidymedia.timeout = as.numeric(seconds))
  invisible(prior)
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

# Carrying the caller's settings into a worker --------------------------------

# tidymedia's two option seams are read in the process that evaluates the call,
# and a `parallel = TRUE` fan-out evaluates its mapped call somewhere else. A
# `future` worker starts from that worker's own options -- `future` exports the
# closure's globals, not the parent's option list -- so a limit or an encoder
# override the caller set was read as UNSET inside the worker and the parallel
# path silently diverged from the sequential one (measured 2026-08-26 on future
# 1.75.0 / furrr 0.4.0: a multisession worker read `tidymedia.timeout` as unset
# against `42` in the parent).
#
# The fix is a wrapper, captured in the PARENT at fan-out time and shipped to
# the worker as part of the mapped closure. It re-establishes values the caller
# set; it does not author values of its own.

# carried_option_values(): what a fan-out carries, resolved in the parent.
#
# The timeout is carried RESOLVED rather than raw, so a value base R would
# mishandle is refused once, here, in the process that can name the caller --
# rather than N times inside workers, below the per-job tryCatch that turns an
# error into a bare `success = FALSE`. Resolving has a consequence worth naming:
# resolve_timeout() answers 0 for an unset option, so a parent with no limit
# carries the no-limit SENTINEL rather than the unset state. That is the one
# value here the package chooses rather than the caller, and it makes the two
# seams asymmetric -- an unset encoder override is carried as unset, an unset
# limit as `0`. The effect is the same for a worker with no limit of its own,
# and it displaces one that had its own limit set through a plan hook (D050's
# named falsifier), which is why it is stated rather than left to be inferred.
#
# The encoder override is carried as-is, including its unset state. What is NOT
# carried is the session capability memo (`R/cache.R`): a worker with no
# override still asks its own FFmpeg -- D044's per-process gap, unchanged.
#
# Adding a third seam is one line here; carry_options() itself is generic over
# whatever named list it is handed.
carried_option_values <- function(call = rlang::caller_env()) {
  list(
    tidymedia.timeout = resolve_timeout(call = call),
    tidymedia.nvenc_encoders = getOption("tidymedia.nvenc_encoders")
  )
}

# carry_options(): wrap a mapped function so it runs under `values`.
#
# `options()` is the whole mechanism, in both directions: it returns the prior
# values of exactly the names being set, and a NULL entry REMOVES an option
# rather than storing NULL (measured on R 4.6.1). So a name carried as unset is
# unset in the worker for the duration of the call, and a name the worker had
# set for itself comes back on the way out -- one rule, no split behavior. What
# counts as "unset" is decided above, in carried_option_values(): the encoder
# override is carried raw, the limit is carried resolved, so only the former can
# reach here unset.
#
# on.exit() rather than a trailing restore, because the restore has to happen on
# the error path too: a mapped call that aborts (a timeout is one) must not
# leave the parent's settings behind for whatever the next chunk maps.
carry_options <- function(.f, values = carried_option_values(call = call),
                          call = rlang::caller_env()) {
  force(.f)
  force(values)
  function(...) {
    prior <- options(values)
    on.exit(options(prior), add = TRUE)
    .f(...)
  }
}
