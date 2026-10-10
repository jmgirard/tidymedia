# Shared fixtures for the program-management family: a stub executable, the two
# redirected config directories, and the published program vocabulary. They
# live in a helper rather than in test-program-management.R because the M113
# tests of program_status() and unset_program() need the same three, and a test
# file's definitions are not visible to another test file.

# An executable at an absolute path, which is what set_program() demands of a
# location and what find_program() re-checks after reading one back. Both ask
# `tm_program_state()`, which reads a path from the filesystem, so the suite's
# own PATH never enters the evidence. On Windows a program needs an executable
# extension; `.bat` is the one that needs no compiler.
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

# find_program() under an empty PATH, so the Sys.which(program) branch cannot
# short-circuit and every answer comes from a config file. Since M138 the
# location read back is checked by `tm_program_state()`, which asks
# `Sys.which()` only about a bare name, so a `Sys.which()` mock that empties
# every path leaves a remembered absolute path answering as it did
# (`test-location-portability.R` holds that model).
#
# R_USER_CONFIG_DIR redirects tools::R_user_dir(); the legacy library is
# redirected by a mock of rappdirs::user_config_dir() that answers from a
# `legacy/` subtree of the same temporary root. One envvar cannot redirect
# both: rappdirs honors
# R_USER_CONFIG_DIR too, and on Windows the two libraries then collapse onto
# the same `<root>/R/tidymedia` (measured on the windows-latest CI leg at
# M097's review), which made the both-present state assert nothing and the
# third state delete the only file.
tm_redirect_config <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(R_USER_CONFIG_DIR = root, PATH = "", .local_envir = env)
  testthat::local_mocked_bindings(
    user_config_dir = function(appname, appauthor = appname, ...) {
      file.path(root, "legacy", appauthor, appname)
    },
    .package = "rappdirs",
    .env = env
  )
  dirs <- list(
    root = root,
    new = tm_config_dir(),
    legacy = tm_legacy_config_dir()
  )
  for (d in dirs[c("new", "legacy")]) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  dirs
}

tm_write_location <- function(dir, program, location) {
  writeLines(location, tm_config_file(program, dir))
}

# Run a call and hand back both its value and every warning it raised, muffled
# so the suite does not see them. expect_warning() would stop at the first, and
# what AC3 asks about is the whole set.
tm_collect_warnings <- function(expr) {
  warns <- list()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warns[[length(warns) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warns)
}

# Moved here from test-program-management.R at M138: a test file's definitions
# are not visible to another test file, and test-location-portability.R needs
# this seam too, for AC5's install-side arm.
# Run install_on_win() as if this were a Windows machine.
#
# M108 refuses a non-Windows host above every cost, so every test about what
# the installer does BELOW that gate needs the seam held at `windows` -- on a
# macOS or Linux developer machine, and on two of the three CI legs, it would
# otherwise reach none of the behaviour it is about. Held at the seam and
# nowhere else, so what the mock defeats is one function returning one word:
# the seam is asserted against the real host in `tm_os()`'s own tests, and the
# gate's real verdict per runner in `test-install-platform.R`, neither of which
# mocks anything.
tm_local_windows <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(tm_os = function(...) "windows", .env = env)
}
