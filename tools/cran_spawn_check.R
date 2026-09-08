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
#                                    [--not-cran] [--self-test-only]
#
#   path       stand-ins prepended to PATH, the real binaries still behind them.
#              The ordinary condition: bare-name resolution goes through them.
#   emptypath  PATH emptied for the whole run, nothing else changed. Nothing can
#              resolve, stand-ins included -- reported as UNINSTRUMENTED, because
#              an empty log here is the absence of a reachable program and not
#              evidence about the suite.
#   config     PATH emptied AND a remembered absolute location pointing at each
#              stand-in, written into a temporary R_USER_CONFIG_DIR. This is the
#              instrumented form of the same escape route: a spawn that resolves
#              through the config file lands in the log.
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
      # $$ and the argument list, so a line identifies WHICH program a spawn
      # asked for rather than only that one happened.
      sprintf('printf \'%%s\\t%%s\\n\' "%s" "$*" >> "%s"', p, log_file),
      sprintf('exec "%s" "$@"', real[[p]])
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
  env[["PATH"]] <- ""
  dir.create(config_dir, recursive = TRUE)
  for (p in programs) {
    writeLines(file.path(shim_dir, p),
               file.path(config_dir, paste0(p, "_location.txt")))
  }
  env[["R_USER_CONFIG_DIR"]] <- file.path(root, "config")
}

cat(sprintf("mode=%s not_cran=%s instrumented=%s\n",
            mode, if (not_cran) "true" else "unset", instrumented))
cat(sprintf("log=%s\n", log_file))

started <- Sys.time()
status <- withr::with_envvar(env, {
  # A separate R process, so the suite's own environment is the one the env
  # vars above describe rather than this script's session.
  system2(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote('devtools::test(reporter = "summary")')),
    stdout = file.path(root, "suite.out"), stderr = file.path(root, "suite.err")
  )
})
elapsed <- round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1)

lines <- readLines(log_file, warn = FALSE)
cat(sprintf("suite exit status: %d, %s min\n", status, elapsed))
cat(sprintf("suite output: %s\n", file.path(root, "suite.out")))
cat(sprintf("SPAWNS LOGGED: %d\n", length(lines)))
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
