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
  assert_that(rlang::is_integerish(x))
  assert_that(is.null(width) || rlang::is_integerish(width, n = 1))
  assert_that(rlang::is_character(flag, n = 1))
  if (is.null(width)) width <- floor(log10(max(x))) + 1
  formatC(x, width = width, flag = flag)
}
