#!/usr/bin/env Rscript
#
# What does `options(tidymedia.timeout = )` actually bound?
# =========================================================
#
# D055 recorded, as an aside to a floors measurement, that three test files
# wedge on a Linux/aarch64 runner: a blocked FFmpeg survives SIGTERM, and one
# isolated run took 191.8 s against a 2 s limit. This script measures that
# directly, on whatever machine it is run on, so the number can be quoted
# rather than transcribed.
#
# Run everything (each case in its own child R process, hard-capped):
#
#     Rscript data-raw/timeout-bound.R
#
# Run one case in THIS process (what the driver invokes):
#
#     Rscript data-raw/timeout-bound.R --case A1
#
# `data-raw/` is .Rbuildignore'd (`^data-raw$`), so nothing here ships.
#
# The grid answers three questions.
#
#   * Does the limit bound the WAIT or the PROCESS? Cases report the child's
#     liveness after the call returns, found by `pgrep -f` on a marker string
#     unique to the run.
#   * If the wait overruns, is R waiting on the PROCESS or on the output PIPE?
#     Every child-blocking case is run twice, once with `stdout = TRUE` (R
#     reads the child's pipe into a character vector) and once with
#     `stdout = ""` (R reads no pipe). A large overrun that disappears when
#     the pipe is not read is a pipe wait, not a process wait.
#   * Does the package's own surface differ from base R's? The package spawns
#     through two different calls -- `system2(stdout = TRUE, timeout = )` at
#     R/program_management.R:125, and `system(intern = TRUE, timeout = )` in
#     the Layer 0 hatches -- so both are measured, plus one case driven all
#     the way through `tidymedia::ffmpeg()` under `with_timeout()`.
#
# Every case prints a `key: value` block. The driver prints those blocks
# verbatim under a per-case header, then a summary table.

LIMIT <- 2L # the limit every case sets, in seconds
CAP <- 300L # hard cap the driver puts on each case, in seconds
BLOCK <- 600L # how long a child would block for if nothing stopped it

# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------

emit <- function(key, value) {
  cat(sprintf("%-22s %s\n", paste0(key, ":"), paste(format(value), collapse = " ")))
}

elapsed_since <- function(t0) {
  round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
}

# Is any process still alive whose command line contains `marker`?
#
# R runs `pgrep` through a shell, so the command line doing the searching
# contains the pattern too and a naive `pgrep -f <marker>` finds itself -- it
# reports a live child on every case, including cases where the child is
# provably dead. The pattern brackets one character of the marker: as an ERE it
# still matches the child's literal marker, and it no longer matches the
# command line carrying the pattern.
blind_pattern <- function(marker) {
  paste0(substr(marker, 1, 6), "[", substr(marker, 7, 7), "]",
         substr(marker, 8, nchar(marker)))
}

matching_pids <- function(marker) {
  out <- suppressWarnings(
    system2("pgrep", c("-f", shQuote(blind_pattern(marker))),
            stdout = TRUE, stderr = FALSE)
  )
  pids <- grep("^[0-9]+$", out, value = TRUE)
  setdiff(pids, as.character(Sys.getpid()))
}

# Report the surviving processes by pid AND command line, so a self-match or a
# stray helper is visible in the transcript rather than counted as the child.
still_alive <- function(marker) {
  pids <- matching_pids(marker)
  if (!length(pids)) return("no")
  info <- suppressWarnings(
    system2("ps", c("-o", "pid=,args=", "-p", paste(pids, collapse = ",")),
            stdout = TRUE, stderr = FALSE)
  )
  paste0("YES (", length(pids), ") | ", paste(trimws(info), collapse = " | "))
}

reap <- function(marker) {
  suppressWarnings(
    system2("pkill", c("-9", "-f", shQuote(blind_pattern(marker))),
            stdout = FALSE, stderr = FALSE)
  )
  Sys.sleep(0.5) # let the kernel reap before asking again
  invisible(NULL)
}

# The liveness probe's own control. Without it, "child alive after: no" is also
# what a broken probe says. This spawns a process that is certainly alive,
# checks the probe finds it, kills it, and checks the probe then does not --
# so a case's liveness verdict is only readable when both halves say ok.
probe_control <- function() {
  m <- sprintf("tmbctl%d", Sys.getpid())
  system2("sh", c("-c", shQuote(sprintf("echo %s; sleep 30", m))),
          wait = FALSE, stdout = FALSE, stderr = FALSE)
  Sys.sleep(1)
  live <- still_alive(m)
  reap(m)
  dead <- still_alive(m)
  emit("probe control", sprintf("finds-a-live-process=%s  clear-after-kill=%s",
                                if (identical(live, "no")) "FAIL" else "ok",
                                if (identical(dead, "no")) "ok" else "FAIL"))
}

# Report whatever came back from a spawn call: system()/system2() with
# `intern`/`stdout = TRUE` return a character vector carrying a `status`
# attribute when the command failed, and NULL/an integer otherwise.
describe_result <- function(x) {
  st <- attr(x, "status")
  emit("exit status", if (is.null(st)) "none (attribute absent)" else st)
  emit("result class", paste(class(x), collapse = "/"))
  emit("result length", length(x))
}

# ---------------------------------------------------------------------------
# the cases
# ---------------------------------------------------------------------------
#
# Each case is a function of the marker string. It must spawn something that
# blocks, under LIMIT, and return the spawn call's value.

# A signal-ignoring shell child: it traps INT and TERM and does nothing with
# them, so only SIGKILL can stop it. This is the pure form of "the limit did
# not terminate the program".
sigignore_cmd <- function(marker) {
  sprintf("trap '' INT TERM; echo %s; sleep %d", marker, BLOCK)
}

# An FFmpeg blocked reading a named pipe nothing ever writes to -- the shape
# `test-with-timeout.R`, `test-runtime-timeout.R` and `test-timeout-silence.R`
# build. The marker rides in the (never-created) output filename so `pgrep -f`
# can find the process.
fifo_setup <- function(marker) {
  dir <- file.path(tempdir(), marker)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  fifo <- file.path(dir, "in.mkv")
  system2("mkfifo", shQuote(fifo))
  list(fifo = fifo, out = file.path(dir, paste0(marker, ".mkv")))
}

CASES <- list(

  A1 = list(
    what = "signal-ignoring child, system2(stdout = TRUE), input = NULL",
    run = function(marker) {
      system2("sh", c("-c", shQuote(sigignore_cmd(marker))),
              stdout = TRUE, stderr = FALSE, input = NULL, timeout = LIMIT)
    }
  ),

  A2 = list(
    what = "signal-ignoring child, system2(stdout = \"\"), input = NULL",
    run = function(marker) {
      system2("sh", c("-c", shQuote(sigignore_cmd(marker))),
              stdout = "", stderr = FALSE, input = NULL, timeout = LIMIT)
    }
  ),

  A3 = list(
    what = "signal-ignoring child, system2(stdout = TRUE), input = \"\"",
    run = function(marker) {
      system2("sh", c("-c", shQuote(sigignore_cmd(marker))),
              stdout = TRUE, stderr = FALSE, input = "", timeout = LIMIT)
    }
  ),

  A4 = list(
    what = "signal-ignoring child, system(intern = TRUE) -- the Layer 0 call",
    run = function(marker) {
      system(sprintf("sh -c %s", shQuote(sigignore_cmd(marker))),
             intern = TRUE, input = "", timeout = LIMIT)
    }
  ),

  B1 = list(
    what = "FFmpeg blocked on a named pipe, system2(stdout = TRUE)",
    run = function(marker) {
      f <- fifo_setup(marker)
      system2("ffmpeg", shQuote(c("-y", "-i", f$fifo, "-c", "copy", f$out)),
              stdout = TRUE, stderr = FALSE, input = NULL, timeout = LIMIT)
    }
  ),

  B2 = list(
    what = "FFmpeg blocked on a named pipe, system2(stdout = \"\")",
    run = function(marker) {
      f <- fifo_setup(marker)
      system2("ffmpeg", shQuote(c("-y", "-i", f$fifo, "-c", "copy", f$out)),
              stdout = "", stderr = FALSE, input = NULL, timeout = LIMIT)
    }
  ),

  C1 = list(
    what = "the package's own path: with_timeout(ffmpeg(<blocked pipe>), LIMIT)",
    run = function(marker) {
      pkgload::load_all(pkg_root(), quiet = TRUE)
      f <- fifo_setup(marker)
      cmd <- sprintf("-y -i %s -c copy %s", shQuote(f$fifo), shQuote(f$out))
      tidymedia::with_timeout(tidymedia::ffmpeg(cmd), LIMIT)
    }
  )
)

# The package root, so C1 can load_all() it from wherever the script was run.
pkg_root <- function() {
  d <- normalizePath(".", mustWork = FALSE)
  while (!file.exists(file.path(d, "DESCRIPTION")) && dirname(d) != d) {
    d <- dirname(d)
  }
  if (!file.exists(file.path(d, "DESCRIPTION"))) stop("no package root above ", getwd())
  d
}

# ---------------------------------------------------------------------------
# one case, in this process
# ---------------------------------------------------------------------------

run_case <- function(name) {
  case <- CASES[[name]]
  if (is.null(case)) stop("unknown case: ", name)
  marker <- sprintf("tmbound%s%d", name, Sys.getpid())

  emit("case", name)
  emit("what", case$what)
  emit("limit set", sprintf("%d s", LIMIT))
  emit("child would block", sprintf("%d s", BLOCK))
  emit("marker", marker)
  probe_control()

  conds <- character()
  t0 <- Sys.time()
  res <- withCallingHandlers(
    tryCatch(case$run(marker), error = function(e) {
      conds <<- c(conds, sprintf("error<%s> %s",
                                 paste(class(e), collapse = "/"),
                                 conditionMessage(e)))
      structure("", class = "character")
    }),
    warning = function(w) {
      conds <<- c(conds, sprintf("warning<%s> %s",
                                 paste(class(w), collapse = "/"),
                                 conditionMessage(w)))
      invokeRestart("muffleWarning")
    },
    condition = function(cnd) {
      if (!inherits(cnd, c("warning", "error", "message"))) {
        conds <<- c(conds, sprintf("condition<%s> %s",
                                   paste(class(cnd), collapse = "/"),
                                   conditionMessage(cnd)))
      }
    }
  )
  secs <- elapsed_since(t0)

  emit("observed elapsed", sprintf("%.2f s", secs))
  emit("overrun", sprintf("%.2f s (%.1fx the limit)", secs - LIMIT, secs / LIMIT))
  describe_result(res)
  emit("conditions", if (length(conds)) length(conds) else "none")
  for (c1 in conds) emit("  condition", c1)
  emit("child alive after", still_alive(marker))
  reap(marker)
  emit("child alive after kill", still_alive(marker))
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# the driver
# ---------------------------------------------------------------------------

# Each case runs in its own R process so a case that never returns costs one
# case rather than the run. `timeout` is coreutils'; where it is absent (macOS
# without coreutils) the case runs uncapped and the header says so.
capped_argv <- function(self, name) {
  if (nzchar(Sys.which("timeout"))) {
    list(cmd = "timeout",
         args = c("-s", "KILL", as.character(CAP),
                  file.path(R.home("bin"), "Rscript"), self, "--case", name),
         capped = TRUE)
  } else {
    list(cmd = file.path(R.home("bin"), "Rscript"),
         args = c(self, "--case", name),
         capped = FALSE)
  }
}

# The case's own number, read back out of its log. A case that produced no
# `observed elapsed` line did not reach its own measurement, and the summary
# says that rather than substituting a number from somewhere else.
observed_elapsed <- function(out, name) {
  line <- grep("^observed elapsed:", out, value = TRUE)
  if (length(line) != 1L) return("(none)")
  m <- regmatches(line, regexec("([0-9.]+) s", line))[[1]]
  if (length(m) != 2L) return("(unparsed)")
  m[2]
}

this_file <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (length(f)) normalizePath(f[[1]]) else "data-raw/timeout-bound.R"
}

drive <- function() {
  self <- this_file()
  cat(strrep("=", 76), "\n", sep = "")
  cat("tidymedia -- what does the timeout bound?\n")
  cat(strrep("=", 76), "\n", sep = "")
  emit("script", self)
  emit("date", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  emit("R version", R.version.string)
  emit("platform", R.version$platform)
  emit("os", paste(Sys.info()[["sysname"]], Sys.info()[["release"]]))
  emit("ffmpeg", {
    v <- suppressWarnings(system2("ffmpeg", "-version", stdout = TRUE, stderr = FALSE))
    if (length(v)) v[[1]] else "not found"
  })
  emit("limit set (all cases)", sprintf("%d s", LIMIT))
  emit("per-case cap", if (nzchar(Sys.which("timeout")))
    sprintf("%d s (coreutils timeout -s KILL)", CAP) else "none (timeout(1) absent)")
  cat("\n")

  rows <- list()
  for (name in names(CASES)) {
    spec <- capped_argv(self, name)
    cat(strrep("-", 76), "\n", sep = "")
    cat(sprintf("case %s -- %s\n", name, CASES[[name]]$what))
    cat(strrep("-", 76), "\n", sep = "")
    # The child's output goes to a FILE, not a pipe.
    #
    # With `stdout = TRUE` R reads a pipe, and every descendant of the child
    # inherits the write end -- so when the cap SIGKILLs the child, R goes on
    # reading until the orphaned grandchild exits, and the cap bounds nothing.
    # That is this script's own subject matter, met in its own driver: the
    # first attempt sat for the full block on a case the cap had already
    # killed. Redirection to a file leaves R waiting on the child alone.
    log <- tempfile(paste0("tmbound-", name, "-"), fileext = ".log")
    t0 <- Sys.time()
    st <- suppressWarnings(
      system2(spec$cmd, spec$args, stdout = log, stderr = log)
    )
    wall <- elapsed_since(t0)
    out <- if (file.exists(log)) readLines(log, warn = FALSE) else character()
    cat(paste(out, collapse = "\n"), "\n", sep = "")
    emit("child R exit status", st)
    hit_cap <- spec$capped && !is.null(st) && identical(as.integer(st), 137L)
    emit("driver wall clock", sprintf("%.2f s", wall))
    emit("hit the cap", if (hit_cap) sprintf("YES -- killed at %d s", CAP) else "no")
    cat("\n")
    # THE SUMMARY REPORTS WHAT THE CASE MEASURED, NOT WHAT THE DRIVER WATCHED.
    # `wall` is this loop's own stopwatch around `system2`, and it carries an
    # R startup, the package load and the fixture build on top of the interval
    # the case is about -- about 2.2 s of it here. The number each case
    # measures is `observed elapsed`, timed around the call itself at the top
    # of `run_case()`, and that is the number quoted from these runs.
    rows[[name]] <- list(
      case = name,
      elapsed = if (hit_cap) sprintf(">%d", CAP) else observed_elapsed(out, name),
      wall = sprintf("%.2f", wall),
      capped = hit_cap
    )
  }

  cat(strrep("=", 76), "\n", sep = "")
  cat("summary -- limit set to ", LIMIT, " s in every case\n", sep = "")
  cat(strrep("=", 76), "\n", sep = "")
  cat(sprintf("%-5s %-12s %-12s %s\n", "case", "elapsed(s)", "driver(s)", "what"))
  for (r in rows) {
    cat(sprintf("%-5s %-12s %-12s %s\n", r$case, r$elapsed, r$wall, CASES[[r$case]]$what))
  }
  cat("\nelapsed(s) is the case's own `observed elapsed`, timed around the call.\n")
  cat("driver(s) is this script's stopwatch around the child process, which also\n")
  cat("carries an R startup and the fixture build -- it is not what the case measures.\n")
  cat("\nRead the pairs: A1 vs A2 and B1 vs B2 differ only in whether R reads\n")
  cat("the child's stdout pipe. If the overrun is present in the stdout = TRUE\n")
  cat("member and absent in the other, R was waiting on the pipe, not on the\n")
  cat("process.\n")
  invisible(NULL)
}

# ---------------------------------------------------------------------------

# Everything above this line is definitions. `TM_DEFS_ONLY` stops here, so
# data-raw/floor-probes.R can call them without starting a measurement. A
# signalled condition rather than a `return()`: `source()` evaluates top-level
# expressions one at a time, and there is no function here to return from.
if (nzchar(Sys.getenv("TM_DEFS_ONLY"))) {
  stop(structure(class = c("tm_defs_only", "error", "condition"),
                 list(message = "sourced for its definitions only", call = NULL)))
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 2L && args[[1]] == "--case") {
  run_case(args[[2]])
} else {
  drive()
}
