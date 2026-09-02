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
  # `confirm = FALSE`: this test is about where the default install directory
  # resolves to, and M101 put a consent gate above the resolution's first
  # write. The gate has its own tests below.
  suppressWarnings(
    expect_error(
      install_on_win(download_url = missing_archive, confirm = FALSE),
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


# tm_confirm() (M101) --------------------------------------------------------

test_that("tm_confirm() returns the reader's answer when someone can be asked", {
  # T1's ask branch. `rlang::local_interactive()` moves what tm_confirm()
  # gates on but not `base::interactive()`, which is what menu() itself
  # refuses on, so the mock is the only route into this branch.
  rlang::local_interactive()
  answers <- c(1L, 2L, 0L)
  seen <- list()
  testthat::local_mocked_bindings(
    menu = function(choices, graphics = FALSE, title = NULL) {
      seen[[length(seen) + 1L]] <<- list(choices = choices, title = title)
      answers[[length(seen)]]
    },
    .package = "utils"
  )

  expect_true(tm_confirm("proceed?"))
  expect_false(tm_confirm("proceed?"))
  # 0 is what menu() returns when the reader answers nothing: a decline, not
  # an approval.
  expect_false(tm_confirm("proceed?"))

  expect_identical(length(seen), 3L)
  for (call in seen) {
    expect_identical(call$choices, c("Yes", "No"))
    expect_identical(call$title, "proceed?")
  }
})

test_that("tm_confirm() refuses, with the caller's bullets, when no one can be asked", {
  # T1's refusal branch, and the seam property D080 states: the message names
  # the escape hatch its CALLER supplied, so the helper carries no argument
  # name of its own. A menu() mock that aborts proves the ask is never
  # reached rather than merely that the abort happened first.
  testthat::local_mocked_bindings(
    menu = function(...) stop("menu() must not be reached"),
    .package = "utils"
  )
  withr::local_options(rlang_interactive = FALSE)

  expect_error(
    tm_confirm("proceed?", "i" = "Pass {.code somehow = FALSE} to skip."),
    class = "tidymedia_confirmation_unavailable"
  )
  expect_error(tm_confirm("proceed?", "i" = "Pass somehow = FALSE."), "somehow = FALSE")
  # Nothing of the helper's own is asserted about a hatch it was not given.
  expect_error(tm_confirm("proceed?"), "non-interactive session")
})


# install_on_win() asks before it writes (M101) ------------------------------

# The two directories an install can touch, as AC1 states them: every file and
# every directory beneath each redirected root. A declined or refused call must
# leave both exactly as it found them.
tm_dir_snapshot <- function(...) {
  lapply(c(...), function(r) {
    list(
      files = list.files(r, recursive = TRUE, all.files = TRUE),
      dirs = list.dirs(r)
    )
  })
}

# Records every write install_on_win() would make instead of making it, so a
# test asserts WHICH calls happened rather than only that the function
# returned. `confirm` is a function of the prompt returning the answer to
# give; `NULL` leaves the real tm_confirm() in place.
tm_mock_install <- function(confirm = NULL, env = parent.frame()) {
  rec <- new.env(parent = emptyenv())
  rec$download <- list()
  rec$extract <- list()
  rec$set <- list()
  rec$prompts <- character(0)
  add <- function(slot, value) rec[[slot]] <- c(rec[[slot]], list(value))

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      # The destfile is a fresh tempfile() on every call, so it is held aside
      # rather than recorded: two runs of the same install must produce equal
      # records. The extract mock below checks the linkage instead.
      rec$destfile <- destfile
      add("download", list(url = url))
      writeLines("stub archive", destfile)
      0L
    },
    .package = "utils", .env = env
  )
  testthat::local_mocked_bindings(
    archive_extract = function(archive, dir, ...) {
      add("extract", list(
        dir = dir,
        from_download = identical(archive, rec$destfile)
      ))
      invisible(NULL)
    },
    .package = "archive", .env = env
  )
  testthat::local_mocked_bindings(
    set_program = function(program, location) {
      add("set", list(program = program, location = location))
      invisible(NULL)
    },
    .env = env
  )
  if (!is.null(confirm)) {
    testthat::local_mocked_bindings(
      tm_confirm = function(prompt, ...) {
        rec$prompts <- c(rec$prompts, prompt)
        confirm(prompt)
      },
      .env = env
    )
  }
  rec
}

test_that("install_on_win() refuses rather than assume consent when no one can be asked", {
  # AC1. Nothing is mocked below tm_confirm(): the refusal has to happen
  # before the first write, so the real download and extraction are simply
  # never reached, and the two snapshots are what says so.
  config <- tm_redirect_config()
  data_root <- tm_redirect_data()
  withr::local_options(rlang_interactive = FALSE)
  expect_false(rlang::is_interactive())

  before <- tm_dir_snapshot(data_root, config$root)
  expect_error(install_on_win(), class = "tidymedia_confirmation_unavailable")
  # The escape hatch is named by install_on_win(), not by the seam, so the
  # message says the argument this caller actually has.
  expect_error(install_on_win(), "confirm = FALSE")
  expect_identical(tm_dir_snapshot(data_root, config$root), before)
})

test_that("a declined install creates, downloads, extracts and registers nothing", {
  # AC2. The three mocks would record a call if one were made, so zero
  # records is evidence about the calls themselves and not only about the
  # directories they would have written to.
  config <- tm_redirect_config()
  data_root <- tm_redirect_data()
  rec <- tm_mock_install(confirm = function(prompt) FALSE)

  before <- tm_dir_snapshot(data_root, config$root)
  expect_false(install_on_win())
  expect_identical(rec$download, list())
  expect_identical(rec$extract, list())
  expect_identical(rec$set, list())
  expect_identical(tm_dir_snapshot(data_root, config$root), before)
})

test_that("an accepted install downloads, extracts and registers; confirm = FALSE does the same without asking", {
  # AC3 and AC5, in one block so AC5's "identical to AC3's record" is asserted
  # against the very record AC3 pinned, over the same URL and directory.
  tm_redirect_config()
  tm_redirect_data()
  u <- "https://example.invalid/ffmpeg-release-essentials.7z"
  d <- file.path(withr::local_tempdir(), "ffmpeg")

  accepted <- tm_mock_install(confirm = function(prompt) TRUE)
  expect_true(install_on_win(download_url = u, install_dir = d, confirm = TRUE))

  expect_identical(length(accepted$download), 1L)
  expect_identical(accepted$download[[1]]$url, u)
  expect_identical(length(accepted$extract), 1L)
  expect_identical(accepted$extract[[1]]$dir, d)
  # What is unpacked is what was just downloaded, not some other file.
  expect_true(accepted$extract[[1]]$from_download)
  expect_identical(
    vapply(accepted$set, function(x) x$program, character(1)),
    c("ffmpeg", "ffprobe", "ffplay")
  )
  expect_identical(
    vapply(accepted$set, function(x) x$location, character(1)),
    file.path(d, "bin", c("ffmpeg.exe", "ffprobe.exe", "ffplay.exe"))
  )

  # AC5: someone IS there to ask, and the call still must not ask. The mock
  # aborts rather than returning a value, so reaching it fails the test
  # instead of quietly changing what is asserted below.
  rlang::local_interactive()
  skipped <- tm_mock_install(
    confirm = function(prompt) stop("tm_confirm() must not be reached")
  )
  expect_true(install_on_win(download_url = u, install_dir = d, confirm = FALSE))
  expect_identical(skipped$prompts, character(0))
  expect_identical(skipped$download, accepted$download)
  expect_identical(skipped$extract, accepted$extract)
  expect_identical(skipped$set, accepted$set)
})

test_that("the prompt names the archive, the directory, and every location it overwrites", {
  # AC4. The prompt is read as the caller handed it over, before menu() ever
  # formats it, and at a width no wrapping can reach.
  config <- tm_redirect_config()
  data_root <- tm_redirect_data()
  withr::local_options(cli.width = 1000)
  rec <- tm_mock_install(confirm = function(prompt) TRUE)

  default_url <- "https://www.gyan.dev/ffmpeg/builds/ffmpeg-release-essentials.7z"
  default_dir <- file.path(tools::R_user_dir("tidymedia", "data"), "ffmpeg")
  named_url <- "https://example.invalid/build.7z"
  named_dir <- file.path(withr::local_tempdir(), "plain")
  # A directory whose name contains a space and a brace: cli interpolates
  # every bullet in the calling frame, so a raw `{...}` in a value would abort
  # the prompt or print a name that does not exist (M44).
  odd_dir <- file.path(withr::local_tempdir(), "an install {dir}")

  expect_true(install_on_win())
  expect_true(install_on_win(download_url = named_url, install_dir = named_dir))
  expect_true(install_on_win(download_url = named_url, install_dir = odd_dir))
  expect_identical(length(rec$prompts), 3L)

  # The set of files the prompt must name is read off the record of what the
  # call actually registered, never hand-listed here.
  registered <- unique(vapply(rec$set, function(x) x$program, character(1)))
  expect_identical(registered, c("ffmpeg", "ffprobe", "ffplay"))
  config_files <- vapply(registered, tm_config_file, character(1), USE.NAMES = FALSE)
  expect_true(all(startsWith(config_files, config$new)))

  expected <- list(
    c(default_url, default_dir, config_files),
    c(named_url, named_dir, config_files),
    c(named_url, odd_dir, config_files)
  )
  for (i in seq_along(expected)) {
    for (needle in expected[[i]]) {
      expect_true(
        grepl(needle, rec$prompts[[i]], fixed = TRUE),
        label = paste0("prompt ", i, " contains ", needle)
      )
    }
  }
})
