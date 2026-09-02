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
  written <- list.files(config, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  expect_setequal(normalizePath(written), normalizePath(expected))
  for (file in expected) expect_identical(readLines(file), stub)
})

# find_program() under an empty PATH, so the Sys.which(program) branch cannot
# short-circuit and every answer comes from a config file. Never a Sys.which()
# mock: find_program() calls it AGAIN to validate what it read, so a mock that
# empties it makes all three states below return NULL. An absolute path to an
# executable still resolves under PATH = "" (measured at T4).
#
# R_USER_CONFIG_DIR redirects tools::R_user_dir(); the legacy library is
# redirected by a mock of rappdirs::user_config_dir() that records what the
# helper asked it for. One envvar cannot redirect both: rappdirs honors
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

test_that("both config directories are redirected, and differ", {
  # AC2's guard on its own instrument: neither library may reach the user's
  # real config dir, and the two must not collapse onto one path (which would
  # make the both-present state assert nothing).
  dirs <- tm_redirect_config()
  root <- normalizePath(dirs$root)
  # The legacy dir is pinned to what the helper ASKED rappdirs for, not to the
  # helper's own answer: a helper body that drifts (say, dropping the "R"
  # appauthor, the pre-M097 Windows layout) would otherwise write and read its
  # own wrong path and stay green. The mock's return value encodes the call.
  expect_identical(
    dirs$legacy, file.path(dirs$root, "legacy", "R", "tidymedia")
  )
  expect_identical(dirs$new, tools::R_user_dir("tidymedia", "config"))
  expect_false(identical(normalizePath(dirs$new), normalizePath(dirs$legacy)))
  expect_true(startsWith(normalizePath(dirs$new), root))
  expect_true(startsWith(normalizePath(dirs$legacy), root))
  expect_identical(Sys.which("ffmpeg"), c(ffmpeg = ""))
})

test_that("a location written before M097 is still returned after it", {
  # AC2, three file states per program. Distinct stubs at the two paths, so
  # the returned value says WHICH file was read, not merely that one was.
  dirs <- tm_redirect_config()
  old_stub <- tm_stub_executable("old")
  new_stub <- tm_stub_executable("new")
  expect_false(identical(old_stub, new_stub))

  for (program in tm_program_vocabulary) {
    # legacy alone
    tm_write_location(dirs$legacy, program, old_stub)
    expect_no_warning(expect_identical(find_program(program), old_stub))
    # both: the R_user_dir() one wins
    tm_write_location(dirs$new, program, new_stub)
    expect_no_warning(expect_identical(find_program(program), new_stub))
    # R_user_dir() alone
    unlink(tm_config_file(program, dirs$legacy))
    expect_no_warning(expect_identical(find_program(program), new_stub))
  }
})

test_that("the stale-location branch fires from the legacy path too", {
  # AC3. A legacy file naming a binary that no longer exists produces the
  # existing warning and a NULL location, as it did before the move.
  dirs <- tm_redirect_config()
  gone <- file.path(withr::local_tempdir(), "gone")
  expect_false(file.exists(gone))

  for (program in tm_program_vocabulary) {
    tm_write_location(dirs$legacy, program, gone)
    expect_warning(
      expect_null(find_program(program)),
      "no longer seems to exist"
    )
  }
})

test_that("with no file at either path, find_program() still warns and returns NULL", {
  # The pre-M097 nothing-configured branch survives the fallback unchanged.
  tm_redirect_config()
  expect_warning(expect_null(find_program("ffmpeg")), "Failed to find ffmpeg")
})


# install_on_win()'s default install dir (M098) ------------------------------

# `R_USER_DATA_DIR` is what tools::R_user_dir() reads, so pointing it at a
# temp dir keeps the test out of the user's real data directory. Returns the
# redirected root so a test can assert the full set of directories created
# under it.
tm_redirect_data <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(c(R_USER_DATA_DIR = root), .local_envir = env)
  root
}

test_that("install_on_win()'s own default install dir is R_user_dir()'s ffmpeg subdir", {
  # AC2. The function is called with no `install_dir`, so it is the function's
  # own default resolution that runs, not a helper called in its place. The
  # download is aimed at a `file://` URL that does not exist: no network is
  # touched, and `download.file()` fails only AFTER the default has resolved
  # and the directory has been created, which is the state being asserted.
  # The config directory is redirected too, so the registration this call never
  # reaches could not touch the developer's real one even if it did.
  config <- tm_redirect_config()
  root <- tm_redirect_data()
  expected <- file.path(tools::R_user_dir("tidymedia", "data"), "ffmpeg")
  # tools::R_user_dir() interposes an `R` component under the envvar's root.
  expect_identical(expected, file.path(root, "R", "tidymedia", "ffmpeg"))

  # `file:///` + a path with its leading separator removed is the absolute-path
  # form on both families: `file:///var/...` on Unix, `file:///C:/...` on
  # Windows. Built this way so the call fails because the archive is missing,
  # not because the URL is malformed.
  missing_archive <- paste0(
    "file:///",
    sub("^/", "", chartr("\\", "/", file.path(withr::local_tempdir(), "no-such-build.7z")))
  )
  suppressWarnings(
    expect_error(
      install_on_win(download_url = missing_archive),
      "cannot open URL"
    )
  )

  # The `ffmpeg` subdirectory is preserved, not dropped: the data directory
  # itself is not the extraction root. Pinned as the full set of directories
  # created beneath the redirected root, so a write anywhere else fails here.
  # Both sides normalized: `list.dirs()` and `file.path()` disagree on the
  # separator under a Windows `tempdir()`.
  expect_identical(
    normalizePath(setdiff(list.dirs(root), root)),
    normalizePath(c(
      file.path(root, "R"),
      file.path(root, "R", "tidymedia"),
      expected
    ))
  )

  # Nothing was registered: the three set_*() calls run only after extraction,
  # so a failed download must leave the config directory empty. This is what
  # fails if that ordering is ever inverted.
  expect_identical(
    list.files(config$root, recursive = TRUE, all.files = TRUE),
    character(0)
  )
})
