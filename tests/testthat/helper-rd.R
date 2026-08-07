# Shared Rd-reading helpers for the documentation guards (lifted out of
# test-audio-index-docs.R at M54, when a second guard -- the nvenc probe
# sentence -- needed the same two-shape Rd source and the same comma-splitting
# item parser). One implementation, so a fix to either reaches both.

# The Rd sources these tests read. Two shapes, because the package is checked in
# two: under devtools::test() the source tree's man/ is right there, while under
# R CMD check the tests run against an INSTALLED package with no man/ at all --
# there the parsed Rd database is the same content by another route. Reading
# both is what keeps this guard running in CI rather than skipping there.
#
# Each element is the .Rd file's text, named by topic. `../../man` is the only
# source-tree path accepted: a looser fallback such as `../../../man` resolves,
# from <pkg>.Rcheck/tests/testthat, to the directory holding the .Rcheck dir --
# so a check run inside the package source would silently validate the working
# tree instead of the tarball under check.
rd_sources <- function() {
  if (dir.exists("../../man")) {
    files <- list.files("../../man", pattern = "\\.Rd$", full.names = TRUE)
    out <- vapply(files, function(p) {
      paste(readLines(p, warn = FALSE), collapse = "\n")
    }, character(1))
    names(out) <- basename(files)
    return(out)
  }
  db <- tryCatch(tools::Rd_db("tidymedia"), error = function(e) NULL)
  if (is.null(db) || !length(db)) return(NULL)
  vapply(db, function(rd) paste(as.character(rd), collapse = ""), character(1))
}

# Argument names documented by an .Rd file. roxygen renders a shared block as
# `\item{direction, resize, audio}{...}`, so each item is split on commas.
rd_param_names <- function(txt) {
  items <- regmatches(txt, gregexpr("\\\\item\\{[^}]*\\}", txt))[[1]]
  names <- sub("^\\\\item\\{", "", sub("\\}$", "", items))
  unique(trimws(unlist(strsplit(names, ","))))
}

topics_documenting <- function(rd, param) {
  rd[vapply(rd, function(txt) param %in% rd_param_names(txt), logical(1))]
}
