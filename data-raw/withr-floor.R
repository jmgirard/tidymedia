# M074 -- measure whether local_timeout() behaves as documented on the oldest
# `withr` DESCRIPTION permits, so the Imports floor states what was measured.
#
# Reproduce (from the package root):
#
#   Rscript data-raw/withr-floor.R                 # the declared floor vs 3.0.3
#   Rscript data-raw/withr-floor.R 2.5.0 3.0.3     # any two versions
#
# It installs each `withr` from CRAN (Archive first, then the current contrib
# dir) into its own library under a temporary root -- withr's only Imports are
# `graphics` and `grDevices`, so each install is self-contained -- and then
# runs, once per version, in a FRESH Rscript session with that library
# prepended to `.libPaths()`:
#
#   * the two timeout-wrapper test files, one verdict per test_that() block;
#   * AC2's two top-level forms (a file run by Rscript, and a file passed to
#     source() with its default globalenv());
#   * AC4's four documented behavioral claims.
#
# Every child session prints the `withr` it actually loaded. That control is
# the point of the harness: a pinned library that silently resolved the user
# library's version would make every result below vacuous.

PKG <- normalizePath(".")
if (!file.exists(file.path(PKG, "DESCRIPTION"))) {
  stop("run this from the tidymedia package root", call. = FALSE)
}

versions <- commandArgs(trailingOnly = TRUE)
if (!length(versions)) {
  declared <- read.dcf(file.path(PKG, "DESCRIPTION"), "Imports")[[1]]
  floor <- sub(".*withr \\(>= ([^)]+)\\).*", "\\1", gsub("\n", " ", declared))
  versions <- c(floor, "3.0.3")
}

LIBROOT <- file.path(tempdir(), "withr-floor-libs")
SCRATCH <- file.path(tempdir(), "withr-floor-scripts")
dir.create(LIBROOT, recursive = TRUE, showWarnings = FALSE)
dir.create(SCRATCH, recursive = TRUE, showWarnings = FALSE)

# --- install one version into its own library ---------------------------------

install_withr <- function(ver) {
  lib <- file.path(LIBROOT, paste0("withr-", ver))
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(file.path(lib, "withr"))) return(lib)
  tgz <- file.path(SCRATCH, sprintf("withr_%s.tar.gz", ver))
  urls <- c(
    sprintf("https://cran.r-project.org/src/contrib/Archive/withr/withr_%s.tar.gz", ver),
    sprintf("https://cloud.r-project.org/src/contrib/withr_%s.tar.gz", ver)
  )
  ok <- FALSE
  for (u in urls) {
    got <- tryCatch({
      utils::download.file(u, tgz, quiet = TRUE)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (isTRUE(got) && file.exists(tgz) && file.size(tgz) > 1000L) { ok <- TRUE; break }
  }
  if (!ok) stop("could not fetch withr ", ver, call. = FALSE)
  utils::install.packages(tgz, lib = lib, repos = NULL, type = "source",
                          INSTALL_opts = "--no-test-load", quiet = TRUE)
  lib
}

# --- run one child script under a pinned library ------------------------------
#
# A fresh Rscript, not a `source()` into this session: AC2's first form IS a
# file run by Rscript, and pinning by R_LIBS is what makes the child's
# `.libPaths()` start at the requested version. NOT_CRAN=true so the two
# `skip_on_cran()` blocks in test-with-timeout.R actually run.

run_under <- function(lib, script) {
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), shQuote(script),
    env = c(sprintf("R_LIBS=%s", lib), "NOT_CRAN=true"),
    stdout = TRUE, stderr = TRUE
  ))
  cat(out, sep = "\n")
  cat("\n")
}

write_script <- function(name, lines) {
  path <- file.path(SCRATCH, name)
  writeLines(lines, path)
  path
}

preamble <- c(
  sprintf('suppressMessages(pkgload::load_all("%s", quiet = TRUE, export_all = FALSE))', PKG),
  'cat("  withr actually loaded:", as.character(packageVersion("withr")), "\\n")',
  'show <- function(label, value) cat(sprintf("  %-56s %s\\n", label, format(value)))'
)

# --- the suite, one verdict per test_that() block ------------------------------

suite <- write_script("suite.R", c(
  preamble,
  sprintf('setwd("%s")', PKG),
  'files <- c("tests/testthat/test-local-timeout.R", "tests/testthat/test-with-timeout.R")',
  'for (f in files) {',
  '  cat("  ---- ", f, "\\n", sep = "")',
  '  res <- testthat::test_file(f, reporter = "silent", package = "tidymedia")',
  '  for (block in res) {',
  '    bad <- vapply(block$results, function(r)',
  '      inherits(r, "expectation_failure") || inherits(r, "expectation_error"), logical(1))',
  '    skipped <- vapply(block$results, inherits, logical(1), "expectation_skip")',
  '    verdict <- if (any(bad)) "FAIL" else if (any(skipped)) "SKIP" else "PASS"',
  '    cat(sprintf("  %-4s  %s\\n", verdict, block$test))',
  '    for (r in block$results[bad]) cat("        > ", conditionMessage(r), "\\n", sep = "")',
  '  }',
  '}'
))

# --- AC2 form 1: the top level of a file run by Rscript ------------------------
#
# The file IS the session here, so "after the file ends" has to be observed
# from a hook that outlives the top level: .Last, and a finalizer registered
# after withr's own. Both are reported rather than one, because which of them
# runs first is not this package's to promise.

formA <- write_script("formA.R", c(
  preamble,
  'options(tidymedia.timeout = 99)',
  'local_timeout(30)',
  'show("A during the script (the limit in force)", getOption("tidymedia.timeout", "UNSET"))',
  '.Last <- function() show("A at .Last, after the top level ended", getOption("tidymedia.timeout", "UNSET"))',
  'invisible(reg.finalizer(globalenv(), function(e)',
  '  show("A at a finalizer registered after withr\'s", getOption("tidymedia.timeout", "UNSET")),',
  '  onexit = TRUE))'
))

# --- AC2 form 2: the top level of a file passed to source() --------------------

inner <- write_script("formB-inner.R", c(
  'local_timeout(30)',
  'show("B inside the sourced file", getOption("tidymedia.timeout", "UNSET"))'
))
formB <- write_script("formB.R", c(
  preamble,
  'options(tidymedia.timeout = 99)',
  sprintf('source("%s")', inner),
  'show("B after source() returns", getOption("tidymedia.timeout", "UNSET"))'
))

# --- AC4: the four documented behavioral claims --------------------------------

ac4 <- write_script("ac4.R", c(
  preamble,
  '# 1 -- two calls in one frame unwind to the CALLER\'s state, not the first call\'s.',
  'options(tidymedia.timeout = 99)',
  'f1 <- function() { local_timeout(30); local_timeout(45); getOption("tidymedia.timeout") }',
  'show("1 two calls, inside the frame (documented 45)", f1())',
  'show("1 two calls, after it returns (documented 99)", getOption("tidymedia.timeout"))',
  '# 2 -- a frame writing its own on.exit() WITHOUT add = TRUE discards the undo.',
  'options(tidymedia.timeout = 99)',
  'f2 <- function() { local_timeout(30); on.exit(invisible(NULL)); invisible(NULL) }',
  'f2()',
  'show("2 clobbering on.exit(), after return (documented 30)", getOption("tidymedia.timeout"))',
  '# 3 -- a .local_envir that is not a live frame takes the undo with it.',
  'options(tidymedia.timeout = 99)',
  'f3 <- function() { e <- new.env(); local_timeout(30, .local_envir = e); invisible(NULL) }',
  'f3()',
  'show("3 dead .local_envir, after return (documented 30)", getOption("tidymedia.timeout"))',
  '# 4 -- a local_timeout() written directly inside with_timeout()\'s expr binds to',
  '#      the frame that wrote it, so its undo runs AFTER the wrapper\'s and the',
  '#      wrapper\'s limit is what the frame leaves behind.',
  'options(tidymedia.timeout = 99)',
  'f4 <- function() {',
  '  with_timeout({ local_timeout(45); getOption("tidymedia.timeout") }, seconds = 30)',
  '  getOption("tidymedia.timeout")',
  '}',
  'show("4 inside f4, once the wrapper has returned (documented 99)", f4())',
  'show("4 what f4\'s frame leaves behind (documented 30, the wrapper\'s)", getOption("tidymedia.timeout"))'
))

# --- drive it ------------------------------------------------------------------

for (ver in versions) {
  lib <- install_withr(ver)
  cat("\n================ withr", ver, "================\n")
  cat("library:", lib, "\n\n")
  cat("-- the two timeout-wrapper test files --\n");   run_under(lib, suite)
  cat("-- AC2 form 1: top level of an Rscript file --\n"); run_under(lib, formA)
  cat("-- AC2 form 2: top level of a source()d file --\n"); run_under(lib, formB)
  cat("-- AC4: the four documented claims --\n");      run_under(lib, ac4)
}
