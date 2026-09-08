#!/usr/bin/env Rscript
# Does a run of the package's own checks write into the user's REAL
# configuration directories? (M117)
#
# tidymedia remembers a program location in a file under one of two user
# config directories -- `tools::R_user_dir("tidymedia", "config")` since
# 0.2.0, and `rappdirs::user_config_dir("tidymedia", "R")` before it. The
# suite is supposed to redirect both (tests/testthat/helper-program-config.R),
# but a site that bypasses the redirect leaves a file behind on whatever
# machine ran the suite -- a CRAN machine included. This script is the
# before/after comparison that catches such a site.
#
# It watches the two directories as a fresh R session with no environment
# overrides computes them, runs a command, and compares. A difference in
# either directory -- a file added, removed, or changed, or the directory
# appearing where it was absent -- is a leak.
#
# Usage:
#   Rscript tools/config_leak_check.R [options] -- <command> [args...]
#
# Options:
#   --plant=none|test-body|build   Plant a known-leaking write before the run
#                                  and remove it afterwards, to prove the
#                                  comparison can fail (see below).
#   --expect-difference            Invert the exit status: succeed only when a
#                                  difference IS reported. This is what the
#                                  planted runs assert.
#
# Exit status: 0 when the run's outcome matched what was expected, 1 when it
# did not. The command's own exit status is reported but never inherited --
# a red suite that leaked nothing is still a clean answer to THIS question.
#
# The two plants are the positive controls. They are two different plants
# because the leak has two possible origins and one comparison does not reach
# both: `test-body` writes from inside a `test_that()` body, which is what a
# `devtools::test()` run would leak through, and `build` writes from top-level
# package code, which runs during `R CMD check`'s install step and never
# inside a test at all. Each plant is written to disk before the run and
# removed afterwards, whether the run succeeds, fails, or is interrupted.

# The watched directories ----------------------------------------------------

# Computed in a SEPARATE R session with the redirect variables unset, so this
# script's own environment cannot make the watched paths agree with whatever
# the run happened to use. `R_USER_CONFIG_DIR` steers tools::R_user_dir() and
# rappdirs alike; `XDG_CONFIG_HOME` steers rappdirs on Linux.
tm_watched_dirs <- function() {
  script <- paste(
    'cat(tools::R_user_dir("tidymedia", "config"), "\n", sep = "")',
    'cat(rappdirs::user_config_dir("tidymedia", "R"), "\n", sep = "")',
    sep = "; "
  )
  out <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", shQuote(script)),
    stdout = TRUE,
    env = c("R_USER_CONFIG_DIR=", "XDG_CONFIG_HOME=")
  )
  out <- out[nzchar(out)]
  if (length(out) != 2L) {
    stop("could not compute the two config directories; got: ",
         paste(out, collapse = " | "))
  }
  # path.expand() because rappdirs answers with a leading `~` on macOS.
  c(current = path.expand(out[[1]]), legacy = path.expand(out[[2]]))
}

# State and comparison -------------------------------------------------------

# One directory's state: every file under it, by path relative to the
# directory, with its md5. An absent directory and an empty one are the same
# state -- `character(0)` -- because the criterion this serves treats them
# alike ("absent or empty").
tm_dir_state <- function(dir) {
  if (!dir.exists(dir)) return(character(0))
  files <- list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  files <- files[!dir.exists(file.path(dir, files))]
  if (length(files) == 0L) return(character(0))
  files <- sort(files)
  stats::setNames(unname(tools::md5sum(file.path(dir, files))), files)
}

tm_state <- function(dirs) lapply(dirs, tm_dir_state)

# What changed in one directory, as human-readable lines. Empty means no
# difference.
tm_dir_diff <- function(before, after) {
  names_all <- sort(union(names(before), names(after)))
  # Membership by `%in%`, never by a NULL from `[[`: these are named CHARACTER
  # vectors, and `x[["absent"]]` on one is an error, not NULL. The planted
  # controls are what caught that -- the first run in which a name was absent
  # from one side was the first run with anything to report.
  lines <- vapply(names_all, function(f) {
    if (!f %in% names(before)) return(paste0("  + added:   ", f))
    if (!f %in% names(after)) return(paste0("  - removed: ", f))
    if (!identical(before[[f]], after[[f]])) return(paste0("  ~ changed: ", f))
    ""
  }, character(1), USE.NAMES = FALSE)
  lines[nzchar(lines)]
}

tm_report <- function(dirs, before, after) {
  any_diff <- FALSE
  for (key in names(dirs)) {
    lines <- tm_dir_diff(before[[key]], after[[key]])
    cat(sprintf("%s config dir: %s\n", key, dirs[[key]]))
    cat(sprintf("  before: %d file(s); after: %d file(s)\n",
                length(before[[key]]), length(after[[key]])))
    if (length(lines) == 0L) {
      cat("  no difference\n")
    } else {
      any_diff <- TRUE
      cat(paste0(lines, collapse = "\n"), "\n", sep = "")
    }
  }
  any_diff
}

# The plants -----------------------------------------------------------------

# Both plants write the same probe file into BOTH watched directories, so a
# comparison that watches only one of them is caught too. The write goes
# through writeLines() rather than set_program(), because what is being proven
# is the comparison's reach, not any particular package function's behaviour.
tm_plant_body <- function() {
  c(
    '# PLANTED by tools/config_leak_check.R --plant. Removed when it exits.',
    'local({',
    '  dirs <- c(tools::R_user_dir("tidymedia", "config"),',
    '            path.expand(rappdirs::user_config_dir("tidymedia", "R")))',
    '  for (d in dirs) {',
    '    dir.create(d, recursive = TRUE, showWarnings = FALSE)',
    '    writeLines("planted", file.path(d, "tidymedia_leak_probe.txt"))',
    '  }',
    '})'
  )
}

tm_plant_files <- list(
  "test-body" = "tests/testthat/test-zzz-config-leak-probe.R",
  "build" = "R/zzz-config-leak-probe.R"
)

tm_write_plant <- function(kind) {
  path <- tm_plant_files[[kind]]
  if (file.exists(path)) stop("plant file already exists: ", path)
  body <- tm_plant_body()
  if (kind == "test-body") {
    body <- c(
      '# PLANTED by tools/config_leak_check.R --plant=test-body.',
      'test_that("planted leak probe writes into both real config dirs", {',
      paste0("  ", tm_plant_body()[-1]),
      '  expect_true(TRUE)',
      '})'
    )
  }
  writeLines(body, path)
  path
}

# The probe file the plants write, wherever they wrote it. Removed after a
# planted run so the machine is left as it was found; a directory the plant
# created is removed too, but only when it is empty.
tm_remove_probe <- function(dirs, existed_before) {
  for (key in names(dirs)) {
    d <- dirs[[key]]
    probe <- file.path(d, "tidymedia_leak_probe.txt")
    if (file.exists(probe)) unlink(probe)
    if (!existed_before[[key]] && dir.exists(d) &&
        length(list.files(d, all.files = TRUE, no.. = TRUE)) == 0L) {
      unlink(d, recursive = TRUE)
    }
  }
}

# Main -----------------------------------------------------------------------

# The run is wrapped in a function so `on.exit()` has a frame to attach to:
# at an Rscript's top level it has none, and the plant would survive the run.
tm_main <- function(args) {
  plant <- "none"
  expect_difference <- FALSE
  while (length(args) > 0L && grepl("^--", args[[1]])) {
    a <- args[[1]]
    args <- args[-1]
    if (a == "--") break
    if (grepl("^--plant=", a)) {
      plant <- sub("^--plant=", "", a)
      if (!plant %in% c("none", names(tm_plant_files))) {
        stop("unknown --plant value: ", plant)
      }
    } else if (a == "--expect-difference") {
      expect_difference <- TRUE
    } else {
      stop("unknown option: ", a)
    }
  }
  if (length(args) == 0L) stop("no command given")

  dirs <- tm_watched_dirs()
  existed_before <- vapply(dirs, dir.exists, logical(1))

  if (plant != "none") {
    planted <- tm_write_plant(plant)
    on.exit({
      unlink(planted)
      tm_remove_probe(dirs, existed_before)
    }, add = TRUE)
    cat("planted:", planted, "\n")
  }

  before <- tm_state(dirs)
  cat("running:", paste(args, collapse = " "), "\n\n")
  # shQuote() per argument: system2() pastes its `args` into a shell command
  # line, so an unquoted `-e cat("x")` would reach the shell as syntax.
  status <- system2(args[[1]], shQuote(args[-1]))
  cat("\ncommand exit status:", status, "\n\n")
  after <- tm_state(dirs)

  any_diff <- tm_report(dirs, before, after)

  ok <- if (expect_difference) any_diff else !any_diff
  cat("\n", if (ok) "PASS" else "FAIL", ": ",
      if (expect_difference) "a difference was expected" else "no difference was expected",
      " and ", if (any_diff) "one was reported" else "none was reported", "\n", sep = "")
  ok
}

quit(status = if (tm_main(commandArgs(trailingOnly = TRUE))) 0L else 1L)
