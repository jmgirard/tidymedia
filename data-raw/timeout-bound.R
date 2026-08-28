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

# Is any process still alive whose command line contains `marker`? `pgrep -f`
# would match this R process too if the marker appeared in its own arguments,
# so the marker is generated here and never passed on our own command line.
still_alive <- function(marker) {
  out <- suppressWarnings(
    system2("pgrep", c("-f", shQuote(marker)), stdout = TRUE, stderr = FALSE)
  )
  pids <- grep("^[0-9]+$", out, value = TRUE)
  pids <- setdiff(pids, as.character(Sys.getpid()))
  if (length(pids)) paste(pids, collapse = " ") else "no"
}

reap <- function(marker) {
  suppressWarnings(
    system2("pkill", c("-9", "-f", shQuote(marker)), stdout = FALSE, stderr = FALSE)
  )
  invisible(NULL)
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
    t0 <- Sys.time()
    out <- suppressWarnings(
      system2(spec$cmd, spec$args, stdout = TRUE, stderr = TRUE)
    )
    wall <- elapsed_since(t0)
    st <- attr(out, "status")
    cat(paste(out, collapse = "\n"), "\n", sep = "")
    hit_cap <- spec$capped && !is.null(st) && st == 137L
    emit("driver wall clock", sprintf("%.2f s", wall))
    emit("hit the cap", if (hit_cap) sprintf("YES -- killed at %d s", CAP) else "no")
    cat("\n")
    rows[[name]] <- list(
      case = name,
      elapsed = if (hit_cap) sprintf(">%d", CAP) else sprintf("%.2f", wall),
      capped = hit_cap
    )
  }

  cat(strrep("=", 76), "\n", sep = "")
  cat("summary -- limit set to ", LIMIT, " s in every case\n", sep = "")
  cat(strrep("=", 76), "\n", sep = "")
  cat(sprintf("%-5s %-12s %s\n", "case", "elapsed(s)", "what"))
  for (r in rows) {
    cat(sprintf("%-5s %-12s %s\n", r$case, r$elapsed, CASES[[r$case]]$what))
  }
  cat("\nRead the pairs: A1 vs A2 and B1 vs B2 differ only in whether R reads\n")
  cat("the child's stdout pipe. If the overrun is present in the stdout = TRUE\n")
  cat("member and absent in the other, R was waiting on the pipe, not on the\n")
  cat("process.\n")
  invisible(NULL)
}

# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 2L && args[[1]] == "--case") {
  run_case(args[[2]])
} else {
  drive()
}
