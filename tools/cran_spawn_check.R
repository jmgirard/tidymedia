#!/usr/bin/env Rscript
# M118 -- does a suite run under CRAN's own conditions start FFmpeg, FFprobe or
# MediaInfo?
#
# The instrument is a set of stand-in programs named `ffmpeg`, `ffprobe` and
# `mediainfo`. Each appends one line to a log and then execs the real binary, so
# a run that goes through them behaves exactly as it would without them; the log
# is the only difference. What varies between modes is HOW the suite could reach
# a stand-in, which is the point: find_program() consults PATH first and a
# remembered location in the config directory only when PATH answers nothing
# (R/program_management.R:79-90), so a PATH-only instrument is blind to the
# config route and to test files that empty PATH themselves
# (tests/testthat/helper-program-config.R:43).
#
#   Rscript tools/cran_spawn_check.R --mode=<path|emptypath|config> \
#                                    [--not-cran] [--self-test-only] [--lib=DIR]
#
# The suite is run the way `R CMD check` runs it -- the package installed into a
# temporary library, then `test_check()` from `tests/`. NEITHER devtools::test()
# NOR testthat::test_local() can be used here: both force NOT_CRAN="true" inside
# the run (devtools:::r_env_vars() carries it; test_local() sets it too --
# measured 2026-09-08 by a probe test printing Sys.getenv("NOT_CRAN") as "true"
# with the variable unset in the calling process). Under either runner
# skip_on_cran() can never fire, so an empty spawn log would mean nothing.
#
#   path       stand-ins prepended to PATH, the real binaries still behind them.
#              The ordinary condition: bare-name resolution goes through them.
#   emptypath  PATH emptied for the whole run, nothing else changed. Nothing can
#              resolve, stand-ins included -- reported as UNINSTRUMENTED, because
#              an empty log here is the absence of a reachable program and not
#              evidence about the suite.
#   config     the three names made unresolvable on PATH, AND a remembered
#              absolute location pointing at each stand-in, written into a
#              temporary R_USER_CONFIG_DIR. find_program() consults the config
#              only when Sys.which() answers "", so the names have to go -- but
#              PATH itself must keep working, or the suite dies before reaching
#              any program at all (measured 2026-09-08: PATH="" exits 1 with an
#              empty log for the CONTROL too, which is a false green). Only the
#              directories that actually contain one of the three are dropped,
#              named in the output, rather than a fixed list of system paths.
#
# --not-cran sets NOT_CRAN=true, the control: the same run must produce a
# NON-EMPTY log, which is what shows the stand-ins can be seen at all. An empty
# log is only evidence when its control is non-empty.

args <- commandArgs(trailingOnly = TRUE)
has_flag <- function(x) any(args == x)
mode <- sub("^--mode=", "", grep("^--mode=", args, value = TRUE))
if (length(mode) != 1L || !mode %in% c("path", "emptypath", "config")) {
  stop("--mode= must be one of path, emptypath, config", call. = FALSE)
}
not_cran <- has_flag("--not-cran")

programs <- c("ffmpeg", "ffprobe", "mediainfo")

# The real binaries, resolved BEFORE any PATH surgery, and baked into each
# stand-in by absolute path -- so a stand-in still works from a run with no
# usable PATH, which the emptypath and config modes both create.
real <- vapply(programs, function(p) unname(Sys.which(p)), character(1))
missing <- programs[!nzchar(real)]
if (length(missing)) {
  stop(
    "not on PATH, so this check cannot be run here: ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )
}

root <- tempfile("cran-spawn-check-")
shim_dir <- file.path(root, "shims")
dir.create(shim_dir, recursive = TRUE)
log_file <- file.path(root, "spawns.log")
invisible(file.create(log_file))

for (p in programs) {
  path <- file.path(shim_dir, p)
  writeLines(
    c(
      "#!/bin/sh",
      # The program name and its argument list, so a line identifies WHICH
      # program a spawn asked for rather than only that one happened. Newlines
      # and carriage returns in the arguments are folded to spaces first: the
      # suite passes metadata values containing both, and without this one
      # spawn writes several lines (measured 2026-09-08 -- 1230 lines for 1228
      # spawns). The count is lines, so the error only ever inflates, but a
      # per-program tally splits across the break and reads wrong.
      # shQuote() on every interpolated path: a TMPDIR holding a space, a
      # quote or a `$` would otherwise yield a shim that logs to the wrong
      # place or not at all -- and since the `exec` below still runs, the
      # damage is a silent under-count rather than a visible failure.
      sprintf(
        'printf \'%%s\\t%%s\\n\' %s "$(printf \'%%s\' "$*" | tr \'\\n\\r\' \'  \')" >> %s',
        shQuote(p), shQuote(log_file)
      ),
      sprintf('exec %s "$@"', shQuote(real[[p]]))
    ),
    path
  )
  Sys.chmod(path, "0755")
}

# Prove the stand-ins log before trusting a run that says they did not. A
# generated instrument whose domain can silently empty is shown to run over a
# non-empty one first.
self_test <- function() {
  before <- length(readLines(log_file, warn = FALSE))
  for (p in programs) {
    system2(file.path(shim_dir, p), "-version", stdout = FALSE, stderr = FALSE)
  }
  after <- length(readLines(log_file, warn = FALSE))
  if (after - before != length(programs)) {
    stop(
      sprintf(
        "the stand-ins do not log: %d lines for %d direct calls",
        after - before, length(programs)
      ),
      call. = FALSE
    )
  }
  cat(sprintf("self-test: %d direct calls, %d lines logged\n",
              length(programs), after - before))
  # Start the measured run from an empty log.
  writeLines(character(0), log_file)
}
self_test()
if (has_flag("--self-test-only")) quit(status = 0L)

config_dir <- file.path(root, "config", "R", "tidymedia")
env <- c(NOT_CRAN = if (not_cran) "true" else NA_character_)
env[["TIDYMEDIA_SPAWN_LOG"]] <- log_file

instrumented <- TRUE
if (mode == "path") {
  env[["PATH"]] <- paste(shim_dir, Sys.getenv("PATH"), sep = .Platform$path.sep)
} else if (mode == "emptypath") {
  env[["PATH"]] <- ""
  instrumented <- FALSE
} else {
  # Drop exactly the directories holding one of the three, so every other tool
  # on PATH survives.
  drop <- unique(dirname(real))
  keep <- setdiff(strsplit(Sys.getenv("PATH"), .Platform$path.sep)[[1]], drop)
  env[["PATH"]] <- paste(keep, collapse = .Platform$path.sep)
  cat("dropped from PATH:", paste(drop, collapse = " "), "\n")
  dir.create(config_dir, recursive = TRUE)
  for (p in programs) {
    writeLines(file.path(shim_dir, p),
               file.path(config_dir, paste0(p, "_location.txt")))
  }
  env[["R_USER_CONFIG_DIR"]] <- file.path(root, "config")
  # The names must be gone from the PATH the run will see, or find_program()
  # never reaches the config file and this mode silently measures the PATH route
  # again.
  leftover <- withr::with_envvar(
    c(PATH = env[["PATH"]]),
    programs[nzchar(Sys.which(programs))]
  )
  if (length(leftover)) {
    stop(
      "still resolvable on the trimmed PATH, so the config route is not the ",
      "one under test: ", paste(leftover, collapse = ", "),
      call. = FALSE
    )
  }
}

cat(sprintf("mode=%s not_cran=%s instrumented=%s\n",
            mode, if (not_cran) "true" else "unset", instrumented))
cat(sprintf("log=%s\n", log_file))

# Install into a temporary library, so `test_check()` below loads the package
# the way a checked tarball's tests do.
lib <- sub("^--lib=", "", grep("^--lib=", args, value = TRUE))
if (length(lib) != 1L) {
  lib <- file.path(root, "lib")
  dir.create(lib, recursive = TRUE)
  cat("installing into", lib, "\n")
  inst <- system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", paste0("--library=", shQuote(lib)), "."),
    stdout = file.path(root, "install.out"), stderr = file.path(root, "install.err")
  )
  if (inst != 0L) {
    stop("R CMD INSTALL failed; see ", file.path(root, "install.err"), call. = FALSE)
  }
}
env[["R_LIBS"]] <- paste(lib, Sys.getenv("R_LIBS"), sep = .Platform$path.sep)

# The config mode needs a probe of its own. With the three names off PATH every
# skip_if_no_*() helper skips on binary-absence alone -- they ask Sys.which()
# directly -- so the suite never reaches a remembered location whatever NOT_CRAN
# says, and its CONTROL is empty for that reason rather than for a good one.
# What can be shown is that the route is LIVE: resolve and run one program the
# way the package does, in the run's own environment, and see the line appear.
# Without this the mode's zero is unfalsifiable.
if (mode == "config") {
  probe_status <- withr::with_envvar(env, {
    system2(
      file.path(R.home("bin"), "Rscript"),
      c("-e", shQuote(paste(
        'library(tidymedia);',
        'loc <- tidymedia:::find_ffmpeg();',
        'if (is.null(loc)) stop("find_ffmpeg() resolved nothing");',
        'invisible(tidymedia:::run_program(loc, "-version", program = "FFmpeg"))'
      ))),
      stdout = FALSE, stderr = FALSE
    )
  })
  probed <- length(readLines(log_file, warn = FALSE))
  if (probe_status != 0L || probed == 0L) {
    stop(
      sprintf(
        "the config route is not live here (probe exit %d, %d lines logged), ",
        probe_status, probed
      ),
      "so a zero from this mode would mean nothing",
      call. = FALSE
    )
  }
  cat(sprintf("config-route probe: live, %d line(s) logged\n", probed))
  writeLines(character(0), log_file)
}

started <- Sys.time()
status <- withr::with_envvar(env, {
  # A separate R process, so the suite's own environment is the one the env
  # vars above describe rather than this script's session. `test_check()` from
  # `tests/` is the call `tests/testthat.R` makes under R CMD check, and the one
  # runner of the three that leaves NOT_CRAN alone.
  system2(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote(paste(
      'setwd("tests");',
      'library(testthat); library(tidymedia);',
      'test_check("tidymedia", reporter = "summary")'
    ))),
    stdout = file.path(root, "suite.out"), stderr = file.path(root, "suite.err")
  )
})
elapsed <- round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1)

lines <- readLines(log_file, warn = FALSE)
cat(sprintf("suite exit status: %d, %s min\n", status, elapsed))
cat(sprintf("suite output: %s\n", file.path(root, "suite.out")))
cat(sprintf("suite errors: %s\n", file.path(root, "suite.err")))
cat(sprintf("lib: %s\n", lib))

# A spawn count is a RESULT only when the run that produced it finished. A
# suite that dies early logs few spawns or none for a reason that has nothing
# to do with skip_on_cran(), and printing that number in the same shape as a
# genuine zero is how a dead run reads as a clean one. The count is still
# printed -- it is diagnostic -- but under a label that is not the result
# label, and the script exits non-zero so a caller cannot miss it.
reportable <- instrumented && status == 0L
cat(sprintf("%s: %d\n",
            if (reportable) "SPAWNS LOGGED" else "spawns logged (NOT A RESULT)",
            length(lines)))
if (length(lines)) {
  counts <- table(sub("\t.*$", "", lines))
  for (nm in names(counts)) cat(sprintf("  %s: %d\n", nm, counts[[nm]]))
  cat("first lines:\n")
  cat(paste0("  ", utils::head(lines, 5L)), sep = "\n")
}
if (!instrumented) {
  cat("NOTE: this mode reaches no stand-in, so an empty log says only that\n")
  cat("      nothing was resolvable -- read it with the config mode's result.\n")
}
if (!reportable && instrumented) {
  cat(sprintf(
    "REFUSED: the suite exited %d, so the count above measures that failure\n",
    status
  ))
  cat("         and not the package. Read", file.path(root, "suite.err"), "\n")
  quit(status = 1L)
}
