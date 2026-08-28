#' Pad Integers
#'
#' Takes in a numeric vector of integerish values and returns a character vector
#' of the same length with padding. The width of padding can be specified or
#' intuited from the maximum value. The padding flag can be configured.
#'
#' @param x A numeric vector of integerish values (i.e., either R integers or
#'   integer-like doubles). See [rlang::is_integerish()] for details.
#' @param width Either NULL or a single integerish value specifying the width of
#'   padding to use. If NULL, the width of the maximum value in \code{x} is used
#'   (i.e., the minimum padding needed to standardize the width of all values).
#' @param flag A string specifying what to pad \code{x} with. (default = "0")
#' @return A character vector the same length as \code{x} but with padding
#'   added with the specifying width and flag.
#' @noRd
pad_integers <- function(x, width = NULL, flag = "0") {
  if (!rlang::is_integerish(x)) {
    cli::cli_abort("{.arg x} must be a vector of integerish values.")
  }
  if (!is.null(width)) rlang::check_number_whole(width)
  rlang::check_string(flag)
  if (is.null(width)) width <- floor(log10(max(x))) + 1
  formatC(x, width = width, flag = flag)
}

# check_paths_readable() --------------------------------------------------

# THE site the package's bad-input abort is written (M62, predicate widened at
# M63). Every FRONT DOOR reaches it -- the single-input verbs through
# check_file_readable() below, the fan-out verbs' per-row sweeps and the two
# scalar fan-in verbs (concatenate_videos(), compare_videos()) through
# check_batch_inputs() directly, and ffm_files() itself -- so no wording and no
# firing condition exists in two places to drift apart. This is D035's shape,
# not its licence: D035's rule is conditioned on a probe whose result enters
# the compiled command, and a file's readability never does (see the M62 and
# M63 D-entries).
#
# The predicate is `file.access(mode = 4)`, which is ffm_files()' own and is
# strictly wider than file.exists(): it refuses a file that is there but cannot
# be opened for reading. M62 left the two predicates split, with the pipeline
# the only place an unreadable input was refused; M63 closes that by giving
# ffm_files() this site rather than by copying its test up to the front door.
# The two agree on every path by construction now, there being one predicate.
#
# `x` is a character vector of ALREADY RESOLVED paths, so a caller sweeping a
# jobs column passes the column and a scalar verb passes its one argument.
#
# The message branches on `multiple` -- what the ARGUMENT's contract admits --
# never on how many paths it happened to receive or how many turned out to be
# bad. A column or vector leads with the count, and does so at one row as well
# as at fifty, because "`jobs$input` can't be found or read" would misdescribe
# a column and because a one-row batch must not answer differently from a
# two-row one.
#
# The wording says neither "does not exist" nor "is not readable", because one
# call's carrier can hold both kinds of bad path and the abort names them in
# one list. What it may not do is assert absence of a file that is there, which
# is what M62's wording did to every unreadable input (M63 AC3).
#
# One bad path is reported ONCE however many rows name it: `bad` is
# deduplicated, so a single typo shared by twenty rows reads as one file, not
# twenty (M62 review F3, matching reject_duplicate_outputs() and every other
# sibling guard, all of which unique() before they count).
#
# `arg` may name more than one carrier -- picture_in_picture_batch() sweeps
# `main` and `overlay` in ONE call, so a row missing both names both (M62 review
# F2). cli collapses the vector and the verb agrees with its length.
#
# Pluralization is driven off the scalar `length(bad)` via cli::qty(), never
# off the `{.file {bad}}` vector: a `{?}` governed by a `{.val {vector}}`
# throws `length(object) == 1` with 2+ items (M18).
check_paths_readable <- function(x, arg = rlang::caller_arg(x),
                                 multiple = length(x) != 1L,
                                 call = rlang::caller_env()) {
  # A path carrier can arrive as a factor (paths as levels) or as any other
  # atomic vector; coerce before the predicate so file.access() cannot raise its
  # unattributed base error `invalid 'names' argument` from inside a front-door
  # guard (M62 review F1). This is check_batch_jobs()'s coercion, at the one
  # site every sweep reaches, so a verb that validates its table inline gets it
  # too. Coercing here decides only what THIS guard reads: a verb whose own
  # column contract rejects the type still rejects it downstream, unmoved.
  x <- as.character(x)
  bad <- unique(x[file.access(x, mode = 4) != 0])
  if (length(bad) == 0) {
    return(invisible(x))
  }
  if (!multiple) {
    cli::cli_abort("{.arg {arg}} can't be found or read: {.file {bad}}.",
                   call = call)
  }
  cli::cli_abort(c(
    "{.arg {arg}} {cli::qty(length(arg))}{?names/name} {length(bad)} \\
     file{?s} that can't be found or read.",
    "x" = "Missing or unreadable: {.file {bad}}."
  ), call = call)
}

# check_file_readable() ---------------------------------------------------

# Validate that `x` is a single string naming a readable file. Replaces the
# recurring `is_character(x, n = 1)` + `file.exists(x)` validation pair at every
# site whose argument is a pipeline INPUT. The predicate half delegates to
# check_paths_readable() above; the string check stays here because this
# spelling is the one that promises a SINGLE file.
check_file_readable <- function(x, arg = rlang::caller_arg(x),
                                call = rlang::caller_env()) {
  rlang::check_string(x, arg = arg, call = call)
  check_paths_readable(x, arg = arg, call = call)
  invisible(x)
}

# check_file_exists() -----------------------------------------------------

# Existence, not readability, and deliberately so: this spelling is left for the
# two callers whose file is NOT a pipeline input -- verify_media()'s `file` and
# write_mediainfo_template()'s `templatefile`. Neither has a downstream
# counterpart that would refuse an unreadable file, so widening them here would
# be a front-door abort refusing a call nothing else refuses, which is the shape
# D040 says needs its own decision entry rather than a sweep (M63 Scope Out).
# The abort is written here and nowhere else, as check_paths_readable()'s is at
# its own site.
check_file_exists <- function(x, arg = rlang::caller_arg(x),
                              call = rlang::caller_env()) {
  rlang::check_string(x, arg = arg, call = call)
  if (!file.exists(x)) {
    cli::cli_abort("{.arg {arg}} does not exist: {.file {x}}.", call = call)
  }
  invisible(x)
}

# check_token() -----------------------------------------------------------

# Validate that `x` is a single clean CLI token: a codec, pixel-format, or
# similar name made of letters, digits, and `_ + . -`. Cheap sanity check only
# (D-M06-3) — whether the token names a real codec/format stays FFmpeg's call.
#
# `allow_null` exists so a caller whose argument takes D016's NULL sentinel gets
# a message that SAYS so: the alternative spelling, `if (!is.null(x))
# check_token(x)`, accepts exactly the same values but leaves check_string()
# reporting "must be a single string, not `NA`" — telling a user that NULL is
# illegal on an argument where it is the documented escape hatch (M42/D022).
#
# Every codec-family verb that FANS OUT — the ten `_batch` siblings and
# segment_video(), which spreads one input over several outfiles — calls this at
# its front door on `video_codec` / `audio_codec`, in place of the plain
# check_string() M41 put there. A fan-out verb cannot inherit the blame from the
# pipeline seams the way its scalar sibling does: the seam runs inside
# purrr::pmap(), so its message arrives wrapped in "In index: 1" and blamed on
# pmap rather than on the verb the user called (M56). The upgrade sits at M41's
# site, deliberately: check_string() runs first inside this function, so the
# non-string messages and the precedence those guards were placed for are
# unmoved, and only the token case is new. The seam-routed scalar verbs keep
# check_string() here for that same reason — their token blame already comes
# from apply_audio_codec() / apply_video_codec() with `call` threaded.
check_token <- function(x, arg = rlang::caller_arg(x), allow_null = FALSE,
                        call = rlang::caller_env()) {
  rlang::check_string(x, arg = arg, allow_null = allow_null, call = call)
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9_+.-]*$", x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single clean token.",
        "x" = "{.val {x}} contains whitespace or shell characters,
               or does not start with a letter or digit.",
        "i" = "Allowed: letters, digits, and {.code _ + . -}."
      ),
      call = call
    )
  }
  invisible(x)
}

# type_columns() ----------------------------------------------------------

# Coerce every column of a character metadata tibble to its natural R type,
# treating the CLIs' missing markers ("N/A", "") as NA. Genuinely numeric
# columns become integer/double; fractions ("10/1"), ratios ("1:1"), hex
# identifiers ("0x1f"), the lowercase booleans ffprobe emits, and free text all
# stay character. The `file` key column is never coerced. Shared by the ffprobe
# and mediainfo readers so their typed output is consistent (D-M04-6).
type_columns <- function(df, na_strings = c("N/A", "")) {
  cols <- names(df) != "file"
  df[cols] <- lapply(df[cols], coerce_column, na_strings = na_strings)
  df
}

# Coerce one character column; see type_columns(). Non-character input is
# returned untouched.
coerce_column <- function(col, na_strings = c("N/A", "")) {
  if (!is.character(col)) return(col)
  col[col %in% na_strings] <- NA_character_
  non_na <- col[!is.na(col)]
  # Preserve hex identifiers that type.convert() would silently turn into
  # meaningless decimals (e.g. ffprobe codec_tag = 0x31637661, id = 0x1).
  if (length(non_na) && any(grepl("^0[xX][0-9a-fA-F]+$", non_na))) return(col)
  utils::type.convert(col, as.is = TRUE)
}

# check_dim() -------------------------------------------------------------

# Validate a dimension/position argument that may be either an FFmpeg
# expression (a length-1 character) or a single number. `inclusive = TRUE`
# permits zero (positions); otherwise the number must be strictly positive
# (sizes). Internal helper for the crop/scale/drawbox verbs.
check_dim <- function(x, inclusive = FALSE,
                      arg = rlang::caller_arg(x),
                      call = rlang::caller_env()) {
  # NA is refused first, and both halves need it. An NA_real_ reached the
  # comparison, whose NA made `if (!ok)` raise base R's `missing value where
  # TRUE/FALSE needed` with no argument name in it (M64 F4); an NA_character_
  # satisfied is_character(n = 1) and was written into the command, so
  # `crop_video(width = NA_character_)` compiled `crop=w=NA` (M80). NA is
  # neither an FFmpeg expression nor a number, so the refusal below already
  # says what is wrong and gains no second wording here.
  ok <- !anyNA(x) &&
    (rlang::is_character(x, n = 1) ||
       (rlang::is_double(x, n = 1) && (if (inclusive) x >= 0 else x > 0)))
  if (!ok) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single FFmpeg expression or number.",
        "i" = "Numbers must be {if (inclusive) 'non-negative' else 'positive'}."
      ),
      call = call
    )
  }
  invisible(x)
}

# Builder-bound value ranges ----------------------------------------------

# ONE binding per builder-bound range, read by BOTH the Layer-1 builder and the
# Layer-2 front doors (M65). Restating a number at a second site is what the
# stale-hint lesson bites on: two restated literals compare equal until one is
# edited, and no test comparing them can see the drift. The refusal messages
# below interpolate these values, so the wording tracks the binding too.
#
# overlay_scale_range: exclusive lower bound, inclusive upper (0 < scale <= 1).
# The loudnorm ranges are inclusive at both ends (rlang min/max semantics).
overlay_scale_range <- c(0, 1)
loudnorm_range_target_loudness <- c(-70, -5)
loudnorm_range_true_peak <- c(-9, 0)
loudnorm_range_loudness_range <- c(1, 50)

# The `r ...` inline source for a loudnorm target's documented range: the
# roxygen blocks paste this in at document() time (as audio_stream_param()
# already does for the audio-index prose), so the man pages cannot drift from
# the binding the validators read.
loudnorm_bounds_rd <- function(which) {
  range <- switch(
    which,
    target_loudness = loudnorm_range_target_loudness,
    true_peak = loudnorm_range_true_peak,
    loudness_range = loudnorm_range_loudness_range,
    # A typo'd key would otherwise return character(0) and render a man page
    # with the range silently missing -- document() would not complain.
    cli::cli_abort("Unknown loudnorm range key {.val {which}}.")
  )
  sprintf("a number in \\code{%s}..\\code{%s}", range[[1]], range[[2]])
}

# check_overlay_scale(): the overlay `scale` RANGE rule, at its one site.
# ffm_overlay() calls it for its own direct callers;
# picture_in_picture_pipeline() (call threaded) and picture_in_picture_batch()
# re-call it so the abort names the verb the caller typed (M65, D042). Range
# only: each caller type-checks `scale` first, at its own precedence slot.
check_overlay_scale <- function(scale, call = rlang::caller_env()) {
  # anyNA() first, and it has to come first: an NA reached the comparison,
  # whose NA made `if` raise base R's `missing value where TRUE/FALSE needed`
  # with no argument name in it (M80). NA is not a number in the range, so the
  # refusal below already says what is wrong.
  if (!is.null(scale) &&
      (anyNA(scale) ||
       scale <= overlay_scale_range[[1]] || scale > overlay_scale_range[[2]])) {
    cli::cli_abort(
      "{.arg scale} must be greater than {overlay_scale_range[[1]]} and at
       most {overlay_scale_range[[2]]}.",
      call = call
    )
  }
  invisible(scale)
}

# check_loudnorm_targets(): the three loudness target ranges, at their one
# site. ffm_loudnorm() calls it for its own direct callers; normalize_audio()
# and normalize_audio_batch() (per resolved row) re-call it so the abort names
# the verb -- and, on the two-pass path, fires before the analysis pass spawns
# FFmpeg (M65, D042).
check_loudnorm_targets <- function(target_loudness, true_peak, loudness_range,
                                   call = rlang::caller_env()) {
  rlang::check_number_decimal(target_loudness,
                              min = loudnorm_range_target_loudness[[1]],
                              max = loudnorm_range_target_loudness[[2]],
                              call = call)
  rlang::check_number_decimal(true_peak,
                              min = loudnorm_range_true_peak[[1]],
                              max = loudnorm_range_true_peak[[2]],
                              call = call)
  rlang::check_number_decimal(loudness_range,
                              min = loudnorm_range_loudness_range[[1]],
                              max = loudnorm_range_loudness_range[[2]],
                              call = call)
  invisible(NULL)
}

# check_region_values(): per-value sweep of a `regions` frame -- the values
# check_regions() deliberately leaves to ffm_drawbox()'s check_dim(). Same
# checker, same messages, re-called from Layer 2 so the abort names the verb
# (M65, D042): anonymize_pipeline() (call threaded) covers the scalar verb,
# anonymize_video_batch() sweeps each cell at its front door. Numeric values
# are coerced to double exactly as anonymize_pipeline() does before
# ffm_drawbox(), so an integer/integerish frame is not rejected here either.
check_region_values <- function(regions, call = rlang::caller_env()) {
  # The shape first, at the site that owns its wording: nrow() of a non-frame
  # is NULL, and seq_len(NULL) raised base R's `argument must be coercible to
  # non-negative integer` from inside a front-door guard (M80). Every caller
  # has already run this check, so it re-refuses nothing -- it keeps this
  # predicate from crashing when called on its own, without a second copy of
  # the shape wording.
  check_regions(regions, call = call)
  for (i in seq_len(nrow(regions))) {
    for (field in c("x", "y", "width", "height")) {
      value <- regions[[field]][[i]]
      if (is.numeric(value)) value <- as.double(value)
      check_dim(value, inclusive = field %in% c("x", "y"),
                arg = field, call = call)
    }
  }
  invisible(regions)
}

# check_batch_cell(): the batch row locator, at its one site (M66). A `_batch`
# front-door sweep that refuses a jobs-COLUMN value wraps the per-row checker
# call in this helper, which appends one first-offender bullet naming the
# caller's 1-indexed row and re-raises the SAME condition -- message head,
# class, and blamed call all byte-preserved, so every wording still lives at
# the checker's own site (D042) and the blame grid's markers still match.
# Callers apply it only when the value's carrier column is present in `jobs`:
# an argument-delivered refusal applies to every row and carries no locator.
# The bullet is plain text (no cli markup): it is appended to an
# already-thrown condition, which no cli formatter revisits. Wording
# constraints (M66 AC2): first offender only; pluralization-free; no
# substring "index" (test-separate-av-multitrack.R bans it); must match no
# blame-instrument marker.
# An NA `row` evaluates the expression with no locator: the sweep sites pass
# `i` when the value's carrier column is present in `jobs` and NA when the
# value arrived as the verb's own argument -- an argument applies to every
# row, so naming one would mislead (AC2's complement).
check_batch_cell <- function(row, expr) {
  # A malformed `row` (wrong length, NULL) degrades to the pass-through: the
  # refusal must never be destroyed by its own locator (M66 review F6).
  if (length(row) != 1L || is.na(row)) return(expr)
  rlang::try_fetch(expr, error = function(cnd) {
    locator <- sprintf("First offending jobs row: %d.", as.integer(row))
    if (rlang::is_condition(cnd) && inherits(cnd, "rlang_error")) {
      if (is.function(cnd$body)) {
        # A function-valued `body` cannot take an appended element — c()
        # would yield a list rlang refuses to render, replacing the refusal
        # with an internal error (M66 review F3) — and it renders LAST, so
        # appending to the message would bury the locator mid-message.
        # Materialize it, then append; if it will not materialize, drop to
        # the character branch with what the condition already shows.
        cnd$body <- tryCatch(cnd$body(cnd), error = function(e) NULL)
      }
      cnd$body <- c(cnd$body, c("x" = locator))
    } else {
      cnd$message <- paste0(conditionMessage(cnd), "\nx ", locator)
    }
    stop(cnd)
  })
}
