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

# check_file_exists() -----------------------------------------------------

# Validate that `x` is a single string naming an existing file. Replaces the
# recurring `is_character(x, n = 1)` + `file.exists(x)` validation pair.
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
check_token <- function(x, arg = rlang::caller_arg(x),
                        call = rlang::caller_env()) {
  rlang::check_string(x, arg = arg, call = call)
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
