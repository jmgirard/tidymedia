#' Tidy eval helpers
#'
#' @description
#' The `.data` pronoun is reexported from rlang. It represents the current
#' slice of data inside data-masking verbs. If you have a column name stored in
#' a string, use `.data[["var"]]` to refer to that column. See the [rlang
#' reference](https://rlang.r-lib.org/reference/dot-data.html) for details.
#'
#' @returns This page documents no function and returns no value. `.data` is
#'   not called either: it is an object, of class `rlang_fake_data_pronoun`,
#'   that has meaning only inside a data-masking verb, where it stands for the
#'   current slice of data. Subsetting it there, as `.data[["var"]]`, gives
#'   that column.
#' @md
#' @name tidyeval
#' @keywords internal
#' @importFrom rlang .data
#' @aliases .data
#' @export .data
NULL
