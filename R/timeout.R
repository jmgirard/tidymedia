# Runtime timeout seam -------------------------------------------------------

# The package's second option seam (after `tidymedia.hardware_encoders`), and the
# first that changes what happens rather than what is reported --
# `tidymedia.check_tracks`, below, is the second. Every process tidymedia spawns
# passes resolve_timeout() to base R's `timeout=`, so a hung FFmpeg stops the
# CALL instead of the session (M69/D047). What it does not do
# is bound the program: base R's `timeout=` bounds how long R waits, and the
# program outlives the limit by up to 40 s -- measured at 42.0 s under a 2 s
# limit on Linux and on macOS alike (M078/D056). The FFmpeg build matters too:
# the same blocked input that took 42.0 s against FFmpeg 6.1.1 took 2.0 s
# against 9.0.1, which answers the first signal. A shell child that ignores
# both signals took 42.0 s on macOS. These timings stood in ?tidymedia until
# M127 moved user docs to plain English; ?with_timeout keeps the 40 s bound.
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
#
# Where it is called from is the second half of the rule (M094). This checker
# used to be reached from wherever the limit happened to be read first, which is
# the spawn site -- so `extract_audio()` aborted naming `ffm_run(object)` and
# `probe_all()` naming `purrr::map(infile, probe_one)`, functions the caller
# never typed. Every export in the timeout domain now re-calls it at its own
# front door, applying D042's rule for a builder-bound value: the shared checker
# is called again from the frame that can name the caller, rather than a `call`
# argument being threaded through an exported builder. D074 is where that siting
# is decided, and the `D074:` comment at each site points there rather than at
# its premise (M094 review G6). Three properties come with the siting and are
# worth stating once here rather than at each site:
#
#   * It goes as LATE as the verb allows, but never after a probe or a spawn.
#     In practice that is after the front-door guards and, where the verb builds
#     its pipeline before running anything, after the builder's argument
#     validation too -- so a refusal the VERB itself can reach still fires first
#     and only the blame for this one moves. Not every refusal that fired before
#     it does: a check inside the per-row fan-out (segment_video()'s outfiles, a
#     _batch job table's output column) still loses to the limit, and is
#     disclosed in NEWS.md and ?tidymedia and carried on the ROADMAP rather than
#     fixed (M094 review H1/H3). The OTHER class M094 disclosed -- a check
#     sitting below the build-time nvenc probe, which reads the limit -- is
#     fixed: D075 sites that probe below every check whose answer cannot depend
#     on it, so the argument error is reached before the limit is read (M095).
#     Ordering it against the front door
#     ALONE was measured wrong (M094 review F1): four verbs deliberately keep no
#     front-door guard for `video_codec`/`pixel_format`/`regions`, and a call
#     above ffm_finish() reported the limit where the argument error used to be.
#     A verb reaching a spawn by more than one path carries a call on each
#     (normalize_audio_batch()'s two-pass branch returns above the other).
#     Where the check that has to report first lives in a CALLEE, below the
#     callee's own site, the verb runs that check itself -- the five get_*()
#     scalars call check_path_vector() and resolve_probe()'s infile branch runs
#     probe_all_impl()'s three checks, both above the re-call. That is the same
#     measurement one round later (M094 review G1), at nine exports the first
#     fix did not reach.
#   * It goes ABOVE the `run` gate, so a `run = FALSE` compile is refused too --
#     the batch form already did this (R/ffm_batch.R) and the scalar/batch split
#     was itself the defect.
#   * It is not sited on a path that reads no limit. Two such paths exist, and
#     neither refuses. `has_hardware_encoder()` under a set `tidymedia.hardware_encoders`
#     answers from that option; the memo is NOT part of the carve-out, since
#     inside the fall-through the call sits above cached_encoder_names(), so a
#     warm session memo still refuses and the answer does not depend on what
#     this session happened to ask earlier (M094 review F5). A probe_*()
#     shortcut handed a `probe` object rather than an `infile` reprobes nothing,
#     which is why resolve_probe()'s call sits inside the infile branch. Calling
#     these two the ONE carve-out was measured wrong (M094 review G2).
#
# One probe runs while a command is BUILT rather than at a front door -- the
# nvenc capability lookup, reached from resolve_hw_encoder() inside the pipeline
# and from check_hardware_available() at the fan-out verbs. hardware_encoder_available()
# (R/ffmpeg.R) is has_hardware_encoder()'s body with `call` threaded so that probe refuses
# in the VERB's name; it builds no reached-limit condition, so D049 is untouched.
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

# Dropped-track check seam ---------------------------------------------------

# The package's third option seam. `options(tidymedia.check_tracks = FALSE)`
# switches off D024's dropped-audio-track probe, and with it the one FFprobe
# call that probe costs per distinct input -- the only cost the diagnostic has,
# and one a caller who already knows their inputs' tracks gets nothing for. A
# call that names an `audio_stream` pays nothing either way: the scalar sites
# gate on is.null(audio_stream) and the batch form returns above the seam when
# every row named a track, so those calls never reach the probe at all.
#
# The default is TRUE, so every existing call behaves exactly as it did. That is
# the opposite default from resolve_timeout()'s 0, and for the opposite reason:
# this seam turns an existing behavior OFF, so the reversible default is the
# behavior already shipped.
#
# check_bool() rather than isTRUE(), following resolve_timeout()'s refusal
# rather than base R's coercion. isTRUE() reads every malformed value as FALSE,
# so `options(tidymedia.check_tracks = "yes")` would silently REMOVE the check
# from a session that asked to keep it; check_bool() refuses once, naming the
# option, in the process that can name the caller.
resolve_check_tracks <- function(call = rlang::caller_env()) {
  check <- getOption("tidymedia.check_tracks", default = TRUE)
  rlang::check_bool(check, arg = "tidymedia.check_tracks", call = call)
  check
}

# The caller's per-call limit ------------------------------------------------

#' Set a time limit for one call
#'
#' @description
#' `with_timeout()` runs `expr` with a time limit of its own. The limit applies
#' to each FFmpeg, FFprobe or MediaInfo program that `expr` starts. When
#' `with_timeout()` returns, or stops with an error, the session's own limit is
#' back.
#'
#' The session limit, `options(tidymedia.timeout = )`, applies to every call in
#' the session. `with_timeout()` applies to one call. For example, you can give
#' one test conversion five minutes in a session with a one-hour limit.
#'
#' @param expr An expression. It is run once, where you wrote it, and its value
#'   is returned.
#' @param seconds A whole number of seconds. `0` means no limit, so
#'   `with_timeout(expr, 0)` removes a session limit for one call. A fraction, a
#'   negative number, a string or `NULL` gives an error before `expr` runs.
#'
#' @return The value of `expr`.
#'
#' @details
#' The limit applies to each program, not to the whole call. In a 100-row batch
#' inside `with_timeout(expr, 600)`, each program that a row starts gets 600
#' seconds, plus the delay in "How long the wait can be". The workers of a
#' `parallel = TRUE` run use the same limit.
#'
#' The limit is a whole number of seconds. The package does not round a
#' fraction, because R would read a limit below one second as no limit.
#'
#' A limit set with `options(tidymedia.timeout = )` follows the same rule. The
#' function you called gives the error, even when `run = FALSE`. [ffm_batch()]
#' gives the error before it starts any job.
#'
#' @section How long the wait can be:
#' The limit sets how long R waits for a program, and the wait can be longer.
#' When the limit is reached, R asks the program to stop. R asks again 20
#' seconds later, and kills the program 20 seconds after that. So R can wait up
#' to 40 seconds past the limit. For example, five hung files under a 1-second
#' limit can take about three and a half minutes.
#'
#' R does not guarantee that the program stops. A program can survive the
#' attempts to stop it. How fast a program stops also depends on its version.
#'
#' @section What happens when the limit is reached:
#' A reached limit is never silent. The call gives an error or a warning.
#'
#' These functions give an error with the class `tidymedia_timeout`, which names
#' the program and the limit:
#'
#' * the task functions whose names do not end in `_batch`
#' * [ffm_run()], [ffmpeg()], [ffprobe()] and [mediainfo()]
#' * [verify_media()], because a check with no answer is not a "no"
#'
#' These functions give a warning instead, so that one hung file does not lose
#' the rest of the work:
#'
#' * [probe_all()], the other `probe_*()` functions, [mediainfo_parameter()],
#'   [mediainfo_query()], [mediainfo_template()] and the `get_*()` functions
#'   give `NA` for that file. One warning at the end says how many files timed
#'   out.
#' * [ffm_batch()] and the `_batch` task functions set `success = FALSE` for
#'   that job. One warning at the end says how many jobs timed out. It has the
#'   class `tidymedia_batch_timeout`.
#' * The dropped-track check of [extract_audio()], [convert_audio()],
#'   [normalize_audio()] and their `_batch` forms warns that it could not check.
#'   The track count that [separate_audio_video()] reads after a failed run
#'   warns the same way. A batch manifest, see [ffm_manifest()], warns when it
#'   cannot read a program version. These warnings have the class
#'   `tidymedia_probe_timeout`, and the call goes on as it would for an
#'   unreadable input.
#'
#' `suppressWarnings(classes = "tidymedia_dropped_audio")` hides the
#' dropped-track warning, but not the warning that the check timed out. To hide
#' both, add `"tidymedia_probe_timeout"` to `classes`.
#'
#' The task functions and [ffm_run()] delete a part-written output file after a
#' timeout, as they do after any failed run. [ffmpeg()] cannot tell which of
#' your arguments is the output, so it leaves that file. Check the output of a
#' timed-out [ffmpeg()] call yourself.
#'
#' @seealso [local_timeout()] to set a limit for the rest of a function.
#'   [tidymedia-package] describes the session options.
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

#' Set a time limit for the rest of a function
#'
#' @description
#' `local_timeout()` sets a time limit for the rest of the function that calls
#' it. The limit applies to each FFmpeg, FFprobe or MediaInfo program that the
#' function starts after this call. When the function returns, or stops with an
#' error, the caller's own limit is back.
#'
#' Use [with_timeout()] to set a limit on one expression. Use `local_timeout()`
#' to set a limit on the rest of a function, or on several calls that are hard
#' to wrap in one expression.
#'
#' @param seconds A whole number of seconds. `0` means no limit, so
#'   `local_timeout(0)` removes a session limit for the rest of the function. A
#'   fraction, a negative number, a string or `NULL` gives an error, and the
#'   limit does not change.
#' @param .local_envir The environment that holds the limit. The default is the
#'   function that calls `local_timeout()`. Change it only when you write a
#'   helper that sets a limit for its own caller. The environment must belong to
#'   a function that is still running. If it does not, the limit stays set with
#'   no error. [withr::local_options()] works the same way.
#'
#' @return The caller's earlier setting, invisibly. It is a list with one
#'   element, the same form that [withr::local_options()] returns.
#'
#' @details
#' The limit applies to each program, not to the whole function. In a 100-row
#' batch after `local_timeout(600)`, each program that a row starts gets 600
#' seconds, plus the delay that [with_timeout()] describes. The workers of a
#' `parallel = TRUE` run use the same limit.
#'
#' R can wait up to 40 seconds past the limit, and [with_timeout()] explains
#' why. It also explains what happens when a limit is reached.
#'
#' Two calls in one function work like any two `local_*()` calls. The second
#' limit applies until the function ends. Then both are undone, and the caller's
#' limit is back.
#'
#' In two cases, the limit stays set after the function ends, with no error.
#' The first case is a function that calls `on.exit()` without `add = TRUE`.
#' That call removes the undo step. Write `on.exit(..., add = TRUE)` instead.
#' The second case is a `.local_envir` that belongs to no running function.
#' [withr::local_options()] has the same two limits, because R's exit handlers
#' work this way.
#'
#' Do not call `local_timeout()` directly inside the expression of
#' [with_timeout()]. There, `local_timeout()` belongs to the function around
#' it, so the limit that [with_timeout()] set stays set after that function
#' ends. Put
#' the inner limit in a function of its own, or use only one of the two. The
#' functions [withr::with_options()] and [withr::local_options()] work the same
#' way.
#'
#' @seealso [with_timeout()] to set a limit for one expression.
#'   [tidymedia-package] describes the session options.
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
  # globalenv() branch, and local_timeout() hands it globalenv() from both
  # top-level forms -- parent.frame() is globalenv() at the top level of a file
  # run by Rscript and of a source()d file alike, measured TRUE on 2.5.0 and on
  # 3.0.3. Only the Rscript form's undo actually lands there, though:
  # deferred_run(globalenv()) restores the caller's value at an Rscript top
  # level and finds nothing to run inside a source()d file, on both versions.
  # That is the measured part; withr's own sources say why (both redirect the
  # handler to source()'s own frame first, by different routes), read from those
  # sources rather than by the harness -- D053 records the reading and where it
  # came from. So the rewritten branch is reached from one of the two forms, and
  # what the caller observes is the same on both versions either way.
  # data-raw/withr-floor.R re-runs the whole comparison -- including the withr::
  # calls the @details above compare this one to -- and D053 records what it
  # found, the one form where the two versions part included.
  #
  # Written directly inside a with_timeout() expression, this call binds to the
  # frame that wrote it, so its undo runs after the wrapper's and leaves the
  # wrapper's limit set. withr::with_options() and withr::local_options() do the
  # same (measured 2026-08-27 on withr 2.5.0 and 3.0.3; the ?local_timeout text
  # carried the dates until M127).
  withr::defer(options(prior), envir = .local_envir)
  options(tidymedia.timeout = as.numeric(seconds))
  invisible(prior)
}

# is_timeout(): did this result come back because the limit ended the WAIT?
#
# Not the same as "the limit killed the child": R stops waiting either way, and
# whether the program died depends on whether it answered a signal (M078/D056).
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
# holds every warning it signals, and turns a reached limit into abort_timeout().
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
# The track check joins as the third seam (M082), carried RAW like the encoder
# override rather than resolved like the limit. Resolving buys nothing here:
# the dropped-track probe runs at the front door, in this very process, so a
# malformed value has already been refused by the time a worker could see one --
# and on the one path that skips the front-door probe entirely (a batch whose
# every row named a track) no worker reads the option at all. Carrying it raw
# also keeps the unset state unset, so a worker that has its own answer through
# a plan hook keeps it, which the resolved limit cannot do.
carried_option_values <- function(call = rlang::caller_env()) {
  list(
    tidymedia.timeout = resolve_timeout(call = call),
    tidymedia.hardware_encoders = getOption("tidymedia.hardware_encoders"),
    tidymedia.check_tracks = getOption("tidymedia.check_tracks")
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
