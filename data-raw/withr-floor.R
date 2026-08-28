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
#     source() with its default globalenv()), each also reporting what
#     `parent.frame()` -- local_timeout()'s default target -- actually is there;
#   * whether the Rscript form has an undo SCHEDULED that its observation
#     points simply run before, and whether the source() form's undo is on
#     globalenv() at all or was redirected to source()'s own frame;
#   * source(local = TRUE), the one 3.0.0 behavior change this dependency has
#     that touches the same seam;
#   * AC4's four documented behavioral claims;
#   * the withr:: calls the documentation compares local_timeout() to --
#     defer(), local_options(), with_options() -- so the comparison is measured
#     on each version rather than asserted.
#
# Every child session prints the `withr` it actually loaded and the library it
# came FROM, and asserts both against what it was handed. The provenance
# assertion is the load-bearing one: the user library holds the current release,
# so a version-string check alone cannot catch a failed install of that arm. That
# control is the point of the harness -- a pinned library that silently resolved
# the user library's copy would make every result below vacuous.
#
# `pkgload` is a harness dependency: run this with devtools installed.

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

# --- fetch one version's tarball ----------------------------------------------

# THE ONE TEST OF "is this a package tarball". Three things clear a size floor
# without being a package: a gzip truncated by an interrupted download, an HTTP
# error body, and a well-formed tarball of something that carries no
# `DESCRIPTION`. This fetch used to accept on size alone, so any of the three --
# left in SCRATCH by an earlier run, or written by a download that reported
# success -- went straight to `R CMD INSTALL`.
is_package_tarball <- function(tgz) {
  if (!file.exists(tgz) || file.size(tgz) <= 1000L) return(FALSE)
  inside <- tryCatch(suppressWarnings(utils::untar(tgz, list = TRUE)),
                     error = function(e) NULL)
  if (is.null(inside)) return(FALSE)
  # The listing alone is not enough: `untar(list = TRUE)` shells out to `tar`,
  # and a gzip truncated PAST the DESCRIPTION entry still prints what it read
  # before the end, then exits non-zero. That status is the truncation.
  st <- attr(inside, "status")
  if (!is.null(st) && !identical(as.integer(st), 0L)) return(FALSE)
  any(basename(inside) == "DESCRIPTION")
}

fetch_withr_tarball <- function(ver) {
  tgz <- file.path(SCRATCH, sprintf("withr_%s.tar.gz", ver))
  if (file.exists(tgz)) {
    if (is_package_tarball(tgz)) return(tgz)
    cat(sprintf("  cached withr %s is not a readable package tarball -- refetching\n", ver))
    unlink(tgz)
  }
  urls <- c(
    sprintf("https://cran.r-project.org/src/contrib/Archive/withr/withr_%s.tar.gz", ver),
    sprintf("https://cloud.r-project.org/src/contrib/withr_%s.tar.gz", ver)
  )
  for (u in urls) {
    got <- tryCatch({
      utils::download.file(u, tgz, quiet = TRUE, mode = "wb")
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (isTRUE(got) && is_package_tarball(tgz)) return(tgz)
    unlink(tgz)
  }
  stop("could not fetch withr ", ver, call. = FALSE)
}

# --- install one version into its own library ---------------------------------

install_withr <- function(ver) {
  lib <- file.path(LIBROOT, paste0("withr-", ver))
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(file.path(lib, "withr"))) return(lib)
  tgz <- fetch_withr_tarball(ver)
  utils::install.packages(tgz, lib = lib, repos = NULL, type = "source",
                          INSTALL_opts = "--no-test-load", quiet = TRUE)
  # install.packages() signals a failed source install as a warning, not an
  # error, so a silent failure would otherwise leave an empty library that the
  # children then fall through -- see the provenance assertion in `preamble`.
  if (!dir.exists(file.path(lib, "withr"))) {
    stop("install of withr ", ver, " produced no library entry in ", lib,
         call. = FALSE)
  }
  lib
}

# --- run one child script under a pinned library ------------------------------
#
# A fresh Rscript, not a `source()` into this session: AC2's first form IS a
# file run by Rscript, and pinning by R_LIBS is what makes the child's
# `.libPaths()` start at the requested version. NOT_CRAN=true so the two
# `skip_on_cran()` blocks in test-with-timeout.R actually run.

run_under <- function(lib, ver, script) {
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), shQuote(script),
    env = c(sprintf("R_LIBS=%s", lib), sprintf("WITHR_EXPECT=%s", ver),
            sprintf("WITHR_LIB=%s", lib), "NOT_CRAN=true"),
    stdout = TRUE, stderr = TRUE
  ))
  cat(out, sep = "\n")
  cat("\n")
  status <- attr(out, "status")
  if (!is.null(status) && !identical(as.integer(status), 0L)) {
    stop(sprintf("%s exited %s under withr %s", basename(script), status, ver),
         call. = FALSE)
  }
  invisible(out)
}

write_script <- function(name, lines) {
  path <- file.path(SCRATCH, name)
  writeLines(lines, path)
  path
}

preamble <- c(
  sprintf('suppressMessages(pkgload::load_all("%s", quiet = TRUE, export_all = FALSE))', PKG),
  'loaded <- as.character(packageVersion("withr"))',
  'cat("  withr actually loaded:", loaded, "\\n")',
  '# The pin is by R_LIBS PRECEDENCE, not isolation: the user library stays on',
  '# .libPaths() because pkgload and testthat live there. So the control is an',
  '# assertion, not a printed line a human has to read -- an install that failed',
  '# to yield a loadable withr would otherwise fall through to the user library',
  '# and report a green result for the wrong version.',
  '#',
  '# The version string alone does not close that hole: the user library here',
  '# holds the CURRENT release, so a failed install of the current-release arm',
  '# would load the user copy and still match. Assert WHERE withr came from, and',
  '# the version match becomes a redundant second check rather than the only one.',
  'stopifnot(identical(loaded, Sys.getenv("WITHR_EXPECT")))',
  'from <- normalizePath(dirname(find.package("withr")), winslash = "/")',
  'want <- normalizePath(Sys.getenv("WITHR_LIB"), winslash = "/")',
  'cat("  withr loaded from:", from, "\\n")',
  'stopifnot(identical(from, want))',
  'show <- function(label, value) cat(sprintf("  %-56s %s\\n", label, format(value)))',
  '# What local_timeout()\'s default .local_envir = parent.frame() actually is,',
  '# probed the same way local_timeout() sees it: from inside a call.',
  'caller_is_globalenv <- function() identical(parent.frame(), globalenv())'
)

# --- the suite, one verdict per test_that() block ------------------------------

suite <- write_script("suite.R", c(
  preamble,
  sprintf('setwd("%s")', PKG),
  'files <- c("tests/testthat/test-local-timeout.R", "tests/testthat/test-with-timeout.R")',
  'failures <- character()',
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
  '    if (verdict == "FAIL") failures <<- c(failures, block$test)',
  '  }',
  '}',
  '# AC1 is "zero failures", so a FAIL has to stop the run rather than scroll',
  '# past in 35 lines of stdout that a human is trusted to read.',
  'if (length(failures)) {',
  '  stop(sprintf("%d test_that() block(s) failed under withr %s: %s",',
  '               length(failures), Sys.getenv("WITHR_EXPECT"),',
  '               paste(failures, collapse = "; ")), call. = FALSE)',
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
  'show("A parent.frame() at an Rscript top level IS globalenv()", caller_is_globalenv())',
  '.Last <- function() show("A at .Last, after the top level ended", getOption("tidymedia.timeout", "UNSET"))',
  'invisible(reg.finalizer(globalenv(), function(e)',
  '  show("A at a finalizer registered after withr\'s", getOption("tidymedia.timeout", "UNSET")),',
  '  onexit = TRUE))'
))

# --- AC2 form 2: the top level of a file passed to source() --------------------

inner <- write_script("formB-inner.R", c(
  'local_timeout(30)',
  'show("B inside the sourced file", getOption("tidymedia.timeout", "UNSET"))',
  'show("B parent.frame() at a source()d top level IS globalenv()", caller_is_globalenv())'
))
formB <- write_script("formB.R", c(
  preamble,
  'options(tidymedia.timeout = 99)',
  sprintf('source("%s")', inner),
  'show("B after source() returns", getOption("tidymedia.timeout", "UNSET"))'
))

# --- is there an undo SCHEDULED at an Rscript top level? -----------------------
#
# formA shows the limit still set at .Last and at a later finalizer. That is a
# statement about hook ORDERING, not about the absence of an undo: both versions
# do schedule one on globalenv() (3.x's global_defer(), 2.5.0's
# setup_handlers(), each via reg.finalizer(globalenv(), ..., onexit = TRUE)),
# and formA's observation points simply run first. Running the deferred handlers
# by hand is the only way to see whether the undo is there at all.

formA_undo <- write_script("formA-undo.R", c(
  preamble,
  'options(tidymedia.timeout = 99)',
  'local_timeout(30)',
  'show("A2 the limit in force at the top level", getOption("tidymedia.timeout", "UNSET"))',
  'withr::deferred_run(globalenv())',
  'show("A2 after deferred_run(globalenv()): was an undo scheduled?", getOption("tidymedia.timeout", "UNSET"))'
))

# --- WHERE did defer() register, at each of the two top-level forms? -----------
#
# `parent.frame() == globalenv()` is not the same fact as "defer() took the
# globalenv() branch withr 3.0.0 rewrote", and only the first is what formA and
# formB report. withr can accept globalenv() as the target and then redirect the
# handler elsewhere: inside a source(), 3.0.3 consults
# source_exit_frame_option() before global_defer(), and 2.5.0 runs
# exit_frame()/source_frame() before setup_handlers() is reached at all.
#
# deferred_run(globalenv()) discriminates the two without reaching into either
# version's internals, which differ. If the undo really is on globalenv(), it
# runs and the caller's 99 comes back; if it was redirected to source()'s frame,
# there is nothing on globalenv() to run and the limit stays at 30.
# formA-undo.R above is this same probe for the Rscript form.

innerBw <- write_script("formB-where-inner.R", c(
  'local_timeout(30)',
  'show("Bw inside the sourced file", getOption("tidymedia.timeout", "UNSET"))',
  'ran <- tryCatch({ withr::deferred_run(globalenv()); "ran" },',
  '                error = function(e) paste("error:", conditionMessage(e)))',
  'show("Bw deferred_run(globalenv()) there", ran)',
  'show("Bw after it: 99 = registered on globalenv, 30 = redirected", getOption("tidymedia.timeout", "UNSET"))'
))
formB_where <- write_script("formB-where.R", c(
  preamble,
  'options(tidymedia.timeout = 99)',
  sprintf('source("%s")', innerBw),
  'show("Bw after source() returns", getOption("tidymedia.timeout", "UNSET"))'
))

# --- form C: source(local = TRUE), the one 3.0.0 change at this seam -----------
#
# withr 3.0.0 made source() into a local environment need
# options(withr.hook_source = TRUE), where 2.5.0 redirected by default via
# exit_frame()/source_frame(). This is not one of AC2's two named forms -- the
# criterion is not widened here -- but it is the neighborhood the milestone's
# Scope flagged, so it is measured rather than disclaimed.

innerC <- write_script("formC-inner.R", c(
  'local_timeout(30)',
  'show("C inside the source(local = TRUE)d file", getOption("tidymedia.timeout", "UNSET"))',
  'show("C parent.frame() there is globalenv()?", caller_is_globalenv())'
))
formC <- write_script("formC.R", c(
  preamble,
  'options(tidymedia.timeout = 99)',
  'g <- function() {',
  sprintf('  source("%s", local = TRUE)', innerC),
  '  show("C back in g(), after source(local = TRUE) returned", getOption("tidymedia.timeout", "UNSET"))',
  '}',
  'g()',
  'show("C after g() returns", getOption("tidymedia.timeout", "UNSET"))'
))

# --- the withr:: calls the documentation compares local_timeout() to -----------
#
# local_timeout()'s @details tell the reader that withr::defer() and
# withr::local_options() lose their undo the same two ways, and that
# withr::with_options() + withr::local_options() nest the way with_timeout() +
# local_timeout() do. Those are claims about withr on each version, so they are
# run against withr directly -- the tidymedia values are ac4.R's job.

withrcmp <- write_script("withrcmp.R", c(
  preamble,
  '# 1 -- a frame writing its own on.exit() WITHOUT add = TRUE.',
  'options(tidymedia.timeout = 99)',
  'w1 <- function() { withr::defer(options(tidymedia.timeout = 99)); options(tidymedia.timeout = 30); on.exit(invisible(NULL)); invisible(NULL) }',
  'w1()',
  'show("w1 withr::defer() + clobbering on.exit()", getOption("tidymedia.timeout"))',
  'options(tidymedia.timeout = 99)',
  'w2 <- function() { withr::local_options(tidymedia.timeout = 30); on.exit(invisible(NULL)); invisible(NULL) }',
  'w2()',
  'show("w2 withr::local_options() + clobbering on.exit()", getOption("tidymedia.timeout"))',
  '# 2 -- a target environment that is not a live frame.',
  'options(tidymedia.timeout = 99)',
  'w3 <- function() { e <- new.env(); withr::defer(options(tidymedia.timeout = 99), envir = e); options(tidymedia.timeout = 30); invisible(NULL) }',
  'w3()',
  'show("w3 withr::defer() into a dead envir", getOption("tidymedia.timeout"))',
  'options(tidymedia.timeout = 99)',
  'w4 <- function() { e <- new.env(); withr::local_options(tidymedia.timeout = 30, .local_envir = e); invisible(NULL) }',
  'w4()',
  'show("w4 withr::local_options() into a dead envir", getOption("tidymedia.timeout"))',
  '# 3 -- local_options() written directly inside with_options()\'s code.',
  'options(tidymedia.timeout = 99)',
  'w5 <- function() {',
  '  withr::with_options(list(tidymedia.timeout = 30),',
  '                      { withr::local_options(tidymedia.timeout = 45); getOption("tidymedia.timeout") })',
  '  getOption("tidymedia.timeout")',
  '}',
  'show("w5 inside the frame once with_options() returned", w5())',
  'show("w5 what that frame leaves behind", getOption("tidymedia.timeout"))'
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
  cat("-- the two timeout-wrapper test files --\n");   run_under(lib, ver, suite)
  cat("-- AC2 form 1: top level of an Rscript file --\n"); run_under(lib, ver, formA)
  cat("-- was an undo scheduled at that top level? --\n"); run_under(lib, ver, formA_undo)
  cat("-- AC2 form 2: top level of a source()d file --\n"); run_under(lib, ver, formB)
  cat("-- where did defer() register in that form? --\n"); run_under(lib, ver, formB_where)
  cat("-- form C: source(local = TRUE) --\n");        run_under(lib, ver, formC)
  cat("-- AC4: the four documented claims --\n");      run_under(lib, ver, ac4)
  cat("-- the withr:: calls the docs compare against --\n"); run_under(lib, ver, withrcmp)
}
