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

# check_paths_exist() -----------------------------------------------------

# THE site the package's missing-input abort is written (M62). Every front
# door reaches it -- the single-input verbs through check_file_exists() below,
# the fan-out verbs' per-row sweeps directly, and ffm_files() as the pipeline's
# own backstop -- so no wording and no firing condition exists in two places to
# drift apart. This is D035's shape, not its licence: D035's rule is
# conditioned on a probe whose result enters the compiled command, and a file's
# existence never does (see the M62 D-entry).
#
# `x` is a character vector of ALREADY RESOLVED paths, so a caller sweeping a
# jobs column passes the column and a scalar verb passes its one argument.
#
# The message branches on `multiple` -- what the ARGUMENT's contract admits --
# never on how many paths it happened to receive or how many turned out to be
# missing. A single-file argument renders exactly the string check_file_exists()
# emitted before this function existed (pinned byte-for-byte in
# test-input-path-front-door.R); a column or vector leads with the count, and
# does so at one row as well as at fifty, because "`jobs$input` does not exist"
# would misdescribe a column and because a one-row batch must not answer
# differently from a two-row one.
#
# Pluralization is driven off the scalar `length(missing)` via cli::qty(), never
# off the `{.file {missing}}` vector: a `{?}` governed by a `{.val {vector}}`
# throws `length(object) == 1` with 2+ items (M18).
check_paths_exist <- function(x, arg = rlang::caller_arg(x),
                              multiple = length(x) != 1L,
                              call = rlang::caller_env()) {
  missing <- x[!file.exists(x)]
  if (length(missing) == 0) {
    return(invisible(x))
  }
  if (!multiple) {
    cli::cli_abort("{.arg {arg}} does not exist: {.file {missing}}.", call = call)
  }
  cli::cli_abort(c(
    "{.arg {arg}} names {length(missing)} file{?s} that \\
     {cli::qty(length(missing))}{?does/do} not exist.",
    "x" = "Missing: {.file {missing}}."
  ), call = call)
}

# check_file_exists() -----------------------------------------------------

# Validate that `x` is a single string naming an existing file. Replaces the
# recurring `is_character(x, n = 1)` + `file.exists(x)` validation pair. The
# existence half delegates to check_paths_exist() above (M62); the string check
# stays here because this spelling is the one that promises a SINGLE file.
check_file_exists <- function(x, arg = rlang::caller_arg(x),
                              call = rlang::caller_env()) {
  rlang::check_string(x, arg = arg, call = call)
  check_paths_exist(x, arg = arg, call = call)
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
  ok <- rlang::is_character(x, n = 1) ||
    (rlang::is_double(x, n = 1) && (if (inclusive) x >= 0 else x > 0))
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
