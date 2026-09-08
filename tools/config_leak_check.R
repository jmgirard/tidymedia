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
# did not. A command that RAN and failed does not change the verdict -- a red
# suite that leaked nothing is still a clean answer to THIS question. A command
# that never ran at all is a different thing, and does fail the verdict: the
# comparison of an unexecuted run is a false green, not an answer.
#
# The two plants are the positive controls. They are two different plants
# because the leak has two possible origins and one comparison does not reach
# both: `test-body` writes from inside a `test_that()` body, which is what a
# `devtools::test()` run would leak through, and `build` writes from top-level
# package code, which runs during `R CMD check`'s install step and never
# inside a test at all. Each plant is written to disk before the run and
# removed afterwards when the run succeeds, fails, or is interrupted with
# Ctrl-C. A SIGTERM or SIGKILL runs no cleanup, so a run killed that way leaves
# the plant file (`R/zzz-config-leak-probe.R` or
# `tests/testthat/test-zzz-config-leak-probe.R`) and a `tidymedia_leak_probe.txt`
# in each watched directory; delete those five paths by hand if it happens.

# Where it must be run -------------------------------------------------------

# The plants are written to paths relative to the working directory (`R/...`,
# `tests/testthat/...`), so a run from the wrong place would create and then
# delete files in an unrelated project. Refuse unless this is the package root.
tm_require_package_root <- function() {
  if (!file.exists("DESCRIPTION")) {
    stop("run this from the tidymedia package root: no DESCRIPTION here")
  }
  pkg <- read.dcf("DESCRIPTION", fields = "Package")[[1L]]
  if (is.na(pkg) || pkg != "tidymedia") {
    stop("run this from the tidymedia package root; DESCRIPTION names: ", pkg)
  }
  invisible(TRUE)
}

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
# directory, with its md5, size and mtime. An absent directory and an empty one
# are the same state -- `character(0)` -- because the criterion this serves
# treats them alike ("absent or empty").
#
# Size and mtime are recorded alongside the md5 for two reasons. A leak that
# rewrites an existing file with the SAME bytes -- a test that re-remembers the
# location already on the machine -- moves no md5, and on a populated machine
# that is the likeliest leak of all. And an unreadable file's md5 is NA on both
# sides, and `identical(NA, NA)` is TRUE, so md5 alone would call a changed but
# unreadable file unchanged; its mtime still moves.
tm_dir_state <- function(dir) {
  if (!dir.exists(dir)) return(character(0))
  files <- list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  files <- files[!dir.exists(file.path(dir, files))]
  if (length(files) == 0L) return(character(0))
  files <- sort(files)
  paths <- file.path(dir, files)
  md5 <- unname(tools::md5sum(paths))
  md5[is.na(md5)] <- "unreadable"
  info <- file.info(paths)
  stats::setNames(
    paste(md5, info$size, format(info$mtime, "%Y-%m-%dT%H:%M:%OS3")),
    files
  )
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
  body <- if (kind == "test-body") {
    c(
      '# PLANTED by tools/config_leak_check.R --plant=test-body.',
      'test_that("planted leak probe writes into both real config dirs", {',
      paste0("  ", tm_plant_body()[-1]),
      '  expect_true(TRUE)',
      '})'
    )
  } else {
    tm_plant_body()
  }
  writeLines(body, path)
  path
}

# The probe file the plants write, wherever they wrote it. Removed after a
# planted run so the machine is left as it was found.
#
# The plant does `dir.create(recursive = TRUE)`, which can make SEVERAL levels
# (`.../org.R-project.R/R/tidymedia` where none of that tree existed), so
# removing only the leaf would leave the machine holding directories this
# script created -- and would leave a later unplanted run starting from a state
# the harness itself built. Walk back up instead, deleting each level that is
# empty, and stop at the first one that existed before the run or is not.
tm_remove_probe <- function(dirs, existed_before) {
  for (key in names(dirs)) {
    d <- dirs[[key]]
    probe <- file.path(d, "tidymedia_leak_probe.txt")
    if (file.exists(probe)) unlink(probe)
    if (existed_before[[key]]) next
    repeat {
      parent <- dirname(d)
      if (parent == d) break
      if (!dir.exists(d)) break
      if (length(list.files(d, all.files = TRUE, no.. = TRUE)) > 0L) break
      if (d %in% tm_preexisting_dirs) break
      unlink(d, recursive = TRUE)
      d <- parent
    }
  }
}

# Every ancestor of the two watched directories that already existed when the
# run started. Recorded before the plant executes, so the walk above can tell a
# level it created from one it found. A global because `tm_remove_probe()` runs
# from `on.exit()`, after the frame that computed it may already be unwinding.
tm_preexisting_dirs <- character(0)

tm_ancestors <- function(dir) {
  out <- character(0)
  d <- dir
  repeat {
    parent <- dirname(d)
    if (parent == d) break
    out <- c(out, d)
    d <- parent
  }
  out
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
  tm_require_package_root()

  # The command must at least be findable before anything is planted: a typo'd
  # command name is the commonest way to get a comparison of a run that never
  # happened, and it is also what an option misplaced after `--` becomes.
  if (!nzchar(Sys.which(args[[1]]))) {
    stop("command not found on PATH: ", args[[1]])
  }

  dirs <- tm_watched_dirs()
  existed_before <- vapply(dirs, dir.exists, logical(1))
  tm_preexisting_dirs <<- unique(unlist(lapply(dirs, tm_ancestors)))
  tm_preexisting_dirs <<- tm_preexisting_dirs[dir.exists(tm_preexisting_dirs)]

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

  # 126 (found but not executable) and 127 (not found) are the shell's way of
  # saying the command never ran. A comparison across a run that never happened
  # reports "no difference" for the trivial reason, so it is a FAIL whatever
  # `--expect-difference` asked for -- the false green this gate exists to stop.
  ran <- !status %in% c(126L, 127L)
  ok <- ran && (if (expect_difference) any_diff else !any_diff)
  if (!ran) {
    cat("\nFAIL: the command did not run (exit status ", status,
        "); the comparison above is meaningless\n", sep = "")
    return(ok)
  }
  cat("\n", if (ok) "PASS" else "FAIL", ": ",
      if (expect_difference) "a difference was expected" else "no difference was expected",
      " and ", if (any_diff) "one was reported" else "none was reported", "\n", sep = "")
  ok
}

quit(status = if (tm_main(commandArgs(trailingOnly = TRUE))) 0L else 1L)
