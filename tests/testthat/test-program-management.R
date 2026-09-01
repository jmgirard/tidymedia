# run_program() is the internal safe shell-out used by the metadata readers.
# It must pass arguments to the CLI verbatim (no shell interpolation) and abort
# cleanly when the program cannot be located.

test_that("run_program() passes arguments verbatim without shell interpolation", {
  # Uses printf as a stand-in CLI. POSIX-only: Windows shQuote/echo semantics
  # differ and the metadata binaries are exercised in their own gated tests.
  skip_on_os("windows")
  printf <- Sys.which("printf")
  skip_if_not(nzchar(printf), "printf not available")

  weird <- "a b'c$x`y;z"
  out <- run_program(printf, c("%s", weird))
  expect_equal(out, weird)
})

test_that("run_program() aborts when the program is missing", {
  expect_error(run_program(NULL, "x"), "locate")
  expect_error(run_program("", "x"), "locate")
})


# The remembered location lives under tools::R_user_dir() (M097) -------------

# An executable `Sys.which()` resolves under an EMPTY `PATH`, which is what
# set_program() demands of a location and what find_program() re-checks after
# reading one back. An absolute path resolves without a search, so the suite's
# own PATH never enters the evidence. On Windows `Sys.which()` needs an
# executable extension; `.bat` is the one that needs no compiler.
tm_stub_executable <- function(name = "stub", env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  if (.Platform$OS.type == "windows") {
    path <- file.path(dir, paste0(name, ".bat"))
    writeLines("@echo off", path)
  } else {
    path <- file.path(dir, paste0(name, ".sh"))
    writeLines("#!/bin/sh", path)
    Sys.chmod(path, "0755")
  }
  path
}

tm_program_vocabulary <- c("ffmpeg", "ffprobe", "ffplay", "mediainfo")

test_that("set_program() writes exactly at tools::R_user_dir(\"tidymedia\", \"config\")", {
  # AC1. Equality over the WHOLE set of files written, never containment:
  # rappdirs honors R_USER_CONFIG_DIR too, so "somewhere under the redirected
  # dir" is true of the pre-M097 function as well (measured at the plan gate:
  # `<dir>/tidymedia` vs `<dir>/R/tidymedia`).
  expect_identical(eval(formals(set_program)$program), tm_program_vocabulary)

  config <- withr::local_tempdir()
  withr::local_envvar(R_USER_CONFIG_DIR = config)
  stub <- tm_stub_executable()

  for (program in tm_program_vocabulary) set_program(program, stub)

  expected <- file.path(
    tools::R_user_dir("tidymedia", "config"),
    paste0(tm_program_vocabulary, "_location.txt")
  )
  written <- list.files(config, recursive = TRUE, full.names = TRUE)
  expect_setequal(normalizePath(written), normalizePath(expected))
  for (file in expected) expect_identical(readLines(file), stub)
})
