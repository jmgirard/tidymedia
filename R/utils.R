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
#' @export
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
# recurring `is_character(x, n = 1)` + `file.exists(x)` assertthat pair.
check_file_exists <- function(x, arg = rlang::caller_arg(x),
                              call = rlang::caller_env()) {
  rlang::check_string(x, arg = arg, call = call)
  if (!file.exists(x)) {
    cli::cli_abort("{.arg {arg}} does not exist: {.file {x}}.", call = call)
  }
  invisible(x)
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
