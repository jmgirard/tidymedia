# timeout-valid-baseline.R ---------------------------------------------------
#
# Record what every member of `tm_timeout_domain()` returned, and how many
# processes it spawned, with `tidymedia.timeout` UNSET and with it set to a valid
# whole number -- at the pre-change ref. M094 changes only who is blamed for an
# INVALID limit, so this is the table that says it changed nothing else, and the
# repo held no such table before.
#
# The measurement itself is not duplicated here. `tm_spawn_trace()` lives in
# tests/testthat/helper-timeout-sweep.R, and this script runs THAT function
# against the old ref: a second copy of the reading is how the baseline and the
# assertion stop agreeing (M40). The helper reads only the installed namespace
# and the package's public entry points, so it runs unchanged against a checkout
# of any ref.
#
# Every reading is pinned so the recorded table is the package's behavior and not
# the machine's -- `tm_spawn_trace()` mocks the spawn wrapper and the three
# program locators, and the fixture directory and session temp dir are scrubbed
# out of each digest. There is no randomness and so no seed.
#
# Usage (from the package root):
#
#   Rscript data-raw/timeout-valid-baseline.R            # default ref, below
#   Rscript data-raw/timeout-valid-baseline.R <git-ref>
#
# It writes tests/testthat/fixtures/timeout-valid-baseline.rds. Re-run it only to
# re-derive the baseline from a different ref; the committed .rds is the artifact
# the suite reads.

# The last commit before M094's first code change: master at plan time.
default_ref <- "ae5ff1c"

timeout_valid_baseline <- function(ref = default_ref, root = ".") {
  root <- normalizePath(root, mustWork = TRUE)
  helpers <- file.path(root, "tests", "testthat")
  wt <- file.path(tempfile("timeout-baseline-"))
  dir.create(wt, recursive = TRUE)
  on.exit(unlink(wt, recursive = TRUE), add = TRUE)
  # A checkout of the ref rather than codec-guard-baseline.R's sys.source() of
  # one ref's R/*.R: `tm_spawn_trace()` mocks bindings inside the tidymedia
  # NAMESPACE, and a sourced environment is not a namespace. pkgload needs a
  # real package directory.
  #
  # `git archive | tar -x` rather than `git worktree add`, which the first
  # version used: a worktree is REGISTERED in the shared clone's .git, so a run
  # that died between add and remove left a stale registration behind, and two
  # runs (or a run beside an agent's own worktree) mutated state they did not
  # own (M094 review G8). An archive extract reads the object database and
  # writes only into this temp directory.
  status <- system2(
    "git", c("-C", shQuote(root), "archive", "--format=tar", shQuote(ref)),
    stdout = file.path(wt, "ref.tar")
  )
  if (!identical(as.integer(status), 0L)) {
    stop("git archive failed for ref ", ref)
  }
  utils::untar(file.path(wt, "ref.tar"), exdir = wt)
  unlink(file.path(wt, "ref.tar"))

  out <- file.path(tempfile("timeout-baseline-out-"), "table.rds")
  dir.create(dirname(out), recursive = TRUE)
  # A separate R process, because the old ref's package and this one cannot both
  # be loaded into one session.
  script <- c(
    "a <- commandArgs(trailingOnly = TRUE)",
    "suppressMessages(pkgload::load_all(a[[1]], quiet = TRUE))",
    "suppressMessages(library(testthat))",
    "source(file.path(a[[2]], 'helper-blame.R'))",
    "source(file.path(a[[2]], 'helper-timeout-sweep.R'))",
    "stopifnot(tm_spawn_interception_complete())",
    "dir <- file.path(tempfile('timeout-baseline-fix-'))",
    "dir.create(dir, recursive = TRUE)",
    "specs <- tm_timeout_call_specs(dir)",
    "options(tidymedia.hardware_encoders = NULL)",
    "res <- list()",
    "for (nm in tm_timeout_domain()) {",
    "  res[[nm]] <- list(",
    "    unset = tm_spawn_trace(nm, specs[[nm]], NULL, dir),",
    "    valid = tm_spawn_trace(nm, specs[[nm]], 30, dir)",
    "  )",
    "}",
    "saveRDS(res, a[[3]])"
  )
  script_path <- tempfile(fileext = ".R")
  writeLines(script, script_path)
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    shQuote(c(script_path, wt, helpers, out))
  )
  if (!identical(as.integer(status), 0L)) stop("baseline subprocess failed")
  readRDS(out)
}

timeout_valid_baseline_write <- function(ref = default_ref, root = ".") {
  table <- timeout_valid_baseline(ref, root)
  sha <- system2("git", c("-C", shQuote(root), "rev-parse", shQuote(ref)),
                 stdout = TRUE)
  # Provenance travels ON the artifact, so a reader of the .rds alone can say
  # where it came from and regenerate it.
  attr(table, "provenance") <- list(
    source = paste0("measured at ", sha, " (", ref, ")"),
    generator = "data-raw/timeout-valid-baseline.R",
    seed = "none: the reading is deterministic, see the header",
    recorded = "2026-08-30",
    r_version = R.version.string
  )
  dest <- file.path(root, "tests", "testthat", "fixtures",
                    "timeout-valid-baseline.rds")
  saveRDS(table, dest)
  message("wrote ", dest, " (", length(table), " members)")
  invisible(dest)
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  timeout_valid_baseline_write(if (length(args)) args[[1]] else default_ref)
}
