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
  # write. The gate has its own tests below. The URL is not the package's own
  # default, so M102 fetches no digest and says so before it refuses the
  # download; the refusal is asserted by class, not by base R's wording.
  # M103 made a refused install take its own directories back again, so the
  # chain this test reads is gone by the time the call returns. The removal is
  # held here rather than defeated: the seam records what it was handed and
  # leaves it in place, so the directories below are still the ones the call
  # actually made, and the recorded set says the removal aimed at exactly them
  # and nothing above.
  removed <- NULL
  testthat::local_mocked_bindings(
    tm_remove_created_dirs = function(dirs) {
      removed <<- dirs
      invisible(NULL)
    }
  )
  suppressWarnings(
    expect_error(
      suppressMessages(install_on_win(download_url = missing_archive, confirm = FALSE)),
      class = "tidymedia_download_unavailable"
    )
  )

  # The `ffmpeg` subdirectory is preserved, not dropped: the data directory
  # itself is not the extraction root. Pinned as the full set of directories
  # created beneath the redirected root, so a write anywhere else fails here.
  # Both sides normalized: `list.dirs()` and `file.path()` disagree on the
  # separator under a Windows `tempdir()`.
  chain <- c(file.path(root, "R"), file.path(root, "R", "tidymedia"), expected)
  expect_identical(
    normalizePath(setdiff(list.dirs(root), root)),
    normalizePath(chain)
  )
  # And the removal was handed that same chain, outermost first (M103 AC3).
  expect_identical(
    normalizePath(removed, winslash = "/", mustWork = FALSE),
    normalizePath(chain, winslash = "/", mustWork = FALSE)
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

test_that("tm_confirm() refuses the same way when menu() itself cannot ask", {
  # The two predicates can disagree: `rlang_interactive = TRUE` in a session
  # with no console leaves rlang::is_interactive() TRUE while menu() still
  # refuses. Before this was routed through the refusal, that reached the
  # caller as menu()'s own unclassed error with no escape hatch named.
  rlang::local_interactive()
  testthat::local_mocked_bindings(
    menu = function(...) stop("menu() cannot be used non-interactively"),
    .package = "utils"
  )

  expect_error(
    tm_confirm("proceed?", "i" = "Pass {.code somehow = FALSE} to skip."),
    class = "tidymedia_confirmation_unavailable"
  )
  expect_error(tm_confirm("proceed?", "i" = "Pass somehow = FALSE."), "somehow = FALSE")
})


# install_on_win() asks before it writes (M101) ------------------------------

# The two REDIRECTED ROOTS an install can touch, as M101 AC1 states them: every
# file and every directory beneath each. A declined or refused call must
# leave both exactly as it found them.
tm_roots_snapshot <- function(...) {
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
tm_mock_install <- function(confirm = NULL,
                            unpack = tm_install_registers,
                            sidecar = NULL,
                            archive = NULL,
                            download = NULL,
                            real_set = FALSE,
                            spoil = NULL,
                            env = parent.frame()) {
  rec <- new.env(parent = emptyenv())
  rec$download <- list()
  rec$sidecar <- list()
  rec$extract <- list()
  rec$set <- list()
  rec$prompts <- character(0)
  add <- function(slot, value) rec[[slot]] <- c(rec[[slot]], list(value))

  # What the download mock delivers. The body is fixed, so its digest can be
  # computed here and published by the sidecar mock below: a verifying run
  # passes because the two agree, not because verification was skipped. A test
  # wanting a mismatch publishes its own `sidecar` body, and a test wanting a
  # real extraction failure passes its own `archive` file.
  if (is.null(archive)) {
    archive <- withr::local_tempfile(.local_envir = env)
    writeLines("stub archive", archive)
  }
  rec$digest <- tm_archive_digest(archive)

  # `download` overrides the whole download.file() mock, which is how the two
  # shapes of a failed fetch are reached: signalling, and a non-zero status.
  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      # The digest and the archive are two calls to one function, told apart
      # here the way install_on_win() builds them: the digest's URL is the
      # archive's with `.sha256` appended.
      is_sidecar <- grepl("[.]sha256$", url)
      add(if (is_sidecar) "sidecar" else "download", list(url = url))
      if (!is.null(download)) return(download(url, destfile, is_sidecar))
      if (is_sidecar) {
        writeLines(if (is.null(sidecar)) rec$digest else sidecar, destfile)
        return(0L)
      }
      # The destfile is a fresh tempfile() on every call, so it is held aside
      # rather than recorded: two runs of the same install must produce equal
      # records. The extract mock below checks the linkage instead.
      rec$destfile <- destfile
      file.copy(archive, destfile, overwrite = TRUE)
      0L
    },
    .package = "utils", .env = env
  )
  # `unpack = NULL` leaves the REAL archive::archive_extract() in place, which
  # is how the corrupt-archive fixtures reach libarchive rather than a stub.
  if (!is.null(unpack)) {
    testthat::local_mocked_bindings(
      archive_extract = function(archive, dir, ...) {
        # tm_unpack() hands archive_extract() an open connection rather than
        # a path -- it owns the connection so the failing path can close it
        # (M102 AC3) -- so the linkage back to the download is read off the
        # connection's description.
        from <- if (inherits(archive, "connection")) {
          summary(archive)$description
        } else {
          archive
        }
        add("extract", list(dir = dir, from_download = identical(from, rec$destfile)))
        # What the extraction PRODUCES is what install_on_win() registers, so
        # the mock has to leave files behind: `unpack` is how a test says
        # which programs this build contained.
        bin <- file.path(dir, "bin")
        dir.create(bin, recursive = TRUE, showWarnings = FALSE)
        for (program in unpack) {
          exe <- file.path(bin, paste0(program, ".exe"))
          # `spoil` names, per program, which of the four unusable forms to
          # plant instead of a working stub (M104 AC4). Every form is still
          # LISTED in the return value below, because the list is what the
          # extraction reported and the check exists to disagree with it.
          form <- if (program %in% names(spoil)) spoil[[program]] else "good"
          switch(
            form,
            # Listed and never created: nothing is written at all.
            absent = NULL,
            # Created with no bytes -- the truncation a bit test cannot see.
            empty = file.create(exe),
            dir = dir.create(exe),
            # Bytes but no executable bit. POSIX only: Windows has no such
            # bit, which is why the check does not rest on it alone.
            noexec = {
              writeLines("stub program", exe)
              Sys.chmod(exe, "0644")
            },
            # A working stub is NON-EMPTY, because the registration check asks
            # for bytes: the zero-length stub this helper used to write would
            # now refuse every install it mocks.
            good = writeLines("stub program", exe),
            stop("unknown spoil form: ", form)
          )
          # The stub is made executable whatever `real_set` says. Two checks
          # ask Sys.which() about it: the REAL set_program(), reached only
          # under `real_set = TRUE`, and install_on_win()'s own registration
          # check, which runs on every path (M104). Before that second check
          # existed this bit was needed only for the first. The two forms
          # whose whole point is that they do not resolve are the exception.
          if (!form %in% c("absent", "dir", "noexec")) Sys.chmod(exe, "0755")
        }
        # What the install directory held the moment the extraction finished,
        # so a refusal below it can be held to leaving that state alone
        # (M104 AC2).
        rec$after_extract <- tm_dir_snapshot(dir)
        # The real archive_extract() returns the paths it wrote, relative to
        # `dir` and after strip_components -- measured 2026-09-02 against a
        # three-program control archive, which returned exactly these three
        # strings. install_on_win() registers off that list, so a mock that
        # returned nothing would make every AC4 assertion vacuous.
        invisible(file.path("bin", paste0(unpack, ".exe")))
      },
      .package = "archive", .env = env
    )
  }
  # `real_set = TRUE` leaves the real set_program() in place, so what the
  # install writes can be read back off disk rather than off the record.
  if (!real_set) {
    testthat::local_mocked_bindings(
      set_program = function(program, location) {
        add("set", list(program = program, location = location))
        invisible(NULL)
      },
      .env = env
    )
  }
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

  before <- tm_roots_snapshot(data_root, config$root)
  expect_error(install_on_win(), class = "tidymedia_confirmation_unavailable")
  # The escape hatch is named by install_on_win(), not by the seam, so the
  # message says the argument this caller actually has.
  expect_error(install_on_win(), "confirm = FALSE")
  # The refusal names what the call would have done, so a caller told to pass
  # `confirm = FALSE` has seen what that authorizes.
  withr::local_options(cli.width = 1000)
  u <- "https://example.invalid/build.7z"
  d <- file.path(withr::local_tempdir(), "an install {dir}")
  # Suppressed rather than asserted here: this source is caller-named, so the
  # unverified notice now fires above the confirmation, and the ordering it
  # belongs to is asserted with the prompt tests below.
  msg <- suppressMessages(tryCatch(
    install_on_win(download_url = u, install_dir = d),
    tidymedia_confirmation_unavailable = function(cnd) cli::ansi_strip(conditionMessage(cnd))
  ))
  for (needle in c(u, d, vapply(tm_install_registers, tm_config_file, character(1), USE.NAMES = FALSE))) {
    expect_true(grepl(needle, msg, fixed = TRUE), label = paste("refusal names", needle))
  }
  expect_identical(tm_roots_snapshot(data_root, config$root), before)
})

test_that("a declined install creates, downloads, extracts and registers nothing", {
  # AC2. The three mocks would record a call if one were made, so zero
  # records is evidence about the calls themselves and not only about the
  # directories they would have written to.
  config <- tm_redirect_config()
  data_root <- tm_redirect_data()
  rec <- tm_mock_install(confirm = function(prompt) FALSE)

  before <- tm_roots_snapshot(data_root, config$root)
  expect_false(install_on_win())
  expect_identical(rec$download, list())
  expect_identical(rec$extract, list())
  expect_identical(rec$set, list())
  expect_identical(tm_roots_snapshot(data_root, config$root), before)
})

test_that("an accepted install downloads, extracts and registers; confirm = FALSE does the same without asking", {
  # AC3 and AC5, in one block so AC5's "identical to AC3's record" is asserted
  # against the very record AC3 pinned, over the same URL and directory.
  tm_redirect_config()
  tm_redirect_data()
  u <- "https://example.invalid/ffmpeg-release-essentials.7z"
  d <- file.path(withr::local_tempdir(), "ffmpeg")

  accepted <- tm_mock_install(confirm = function(prompt) TRUE)
  # A caller-named source is unverified, so M102's message fires on every call
  # in this test. It is asserted where it belongs -- the prompt/verification
  # tests below -- and only muted here, where it is not what is being tested.
  expect_true(suppressMessages(
    install_on_win(download_url = u, install_dir = d, confirm = TRUE)
  ))

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
  expect_true(suppressMessages(
    install_on_win(download_url = u, install_dir = d, confirm = FALSE)
  ))
  expect_identical(skipped$prompts, character(0))
  expect_identical(skipped$download, accepted$download)
  expect_identical(skipped$extract, accepted$extract)
  expect_identical(skipped$set, accepted$set)
})

test_that("the prompt names the archive, the directory, and every location it overwrites", {
  # AC4. The prompt is read as the caller handed it over, before menu() ever
  # formats it. `cli.width` is pinned because AC4 names it, not because
  # anything here wraps: cli::format_inline() emits one line whatever the
  # width, so the option fences a formatter change rather than today's.
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
  # As above: the two caller-named sources are unverified, and that message is
  # asserted in the verification tests rather than here.
  expect_true(suppressMessages(
    install_on_win(download_url = named_url, install_dir = named_dir)
  ))
  expect_true(suppressMessages(
    install_on_win(download_url = named_url, install_dir = odd_dir)
  ))
  expect_identical(length(rec$prompts), 3L)

  # The set of files the prompt must name is read off the record of what the
  # call actually registered, never hand-listed here.
  registered <- unique(vapply(rec$set, function(x) x$program, character(1)))
  expect_identical(registered, c("ffmpeg", "ffprobe", "ffplay"))
  config_files <- vapply(registered, tm_config_file, character(1), USE.NAMES = FALSE)
  expect_true(all(startsWith(config_files, config$new)))
  # tm_config_file() is what builds the prompt's paths too, so pin the names
  # AC4 states rather than leave both sides free to move together (M097 F1).
  expect_identical(basename(config_files), paste0(registered, "_location.txt"))

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


# install_on_win() verifies before it registers (M102) ------------------------

# The digest of the stub archive tm_mock_install() delivers, written the three
# ways a source can publish one. Built from the record's own digest rather than
# hard-coded, so the fixture cannot drift from the file it describes.
tm_sidecar_bodies <- function(digest) {
  c(
    bare = digest,
    sha256sum = paste0(digest, "  ffmpeg-release-essentials.7z"),
    openssl = paste0("SHA256(ffmpeg-release-essentials.7z)= ", toupper(digest))
  )
}

test_that("the default source's published digest is fetched, parsed three ways, and checked", {
  # AC1. The URL is left at its default, so it is install_on_win()'s own
  # resolution that decides a sidecar is available -- not the test naming one.
  tm_redirect_config()
  tm_redirect_data()

  for (shape in names(tm_sidecar_bodies("x"))) {
    # A fresh install dir per iteration: the shapes are meant to be
    # independent runs, and a shared directory would let one iteration's
    # extraction stand in for the next one's.
    d <- file.path(withr::local_tempdir(), "ffmpeg")
    rec <- tm_mock_install(confirm = function(prompt) TRUE)
    body <- tm_sidecar_bodies(rec$digest)[[shape]]
    # Re-mock with the body this shape wants, now that the digest is known.
    rec <- tm_mock_install(confirm = function(prompt) TRUE, sidecar = body)
    expect_true(install_on_win(install_dir = d), label = shape)

    # The digest was fetched, from the archive's URL plus `.sha256`. That it
    # was fetched BEFORE the archive is not visible here -- the two records
    # are separate lists -- and is asserted where it IS visible: the
    # unreadable-digest test below finds `download` empty after the sidecar
    # fetch failed.
    expect_identical(length(rec$sidecar), 1L, label = shape)
    expect_identical(
      rec$sidecar[[1]]$url,
      paste0(rec$download[[1]]$url, ".sha256"),
      label = shape
    )
    # The matching-digest control reaches the extraction step, which is what
    # says the three shapes were understood rather than skipped.
    expect_identical(length(rec$extract), 1L, label = shape)
    expect_identical(rec$extract[[1]]$dir, d, label = shape)
  }
})

test_that("the consent prompt names the digest fetch, and only where one happens", {
  # AC1 and AC2 together: M101's property is that the prompt names every fetch
  # the call makes, so the sidecar line has to appear on the path that fetches
  # one and be absent on the two paths that do not.
  tm_redirect_config()
  tm_redirect_data()
  withr::local_options(cli.width = 1000)
  d <- file.path(withr::local_tempdir(), "ffmpeg")
  sidecar_url <- paste0(tm_default_download_url, ".sha256")

  rec <- tm_mock_install(confirm = function(prompt) TRUE)
  expect_true(install_on_win(install_dir = d))
  expect_true(grepl(sidecar_url, rec$prompts[[1]], fixed = TRUE))

  # Same source, but the caller brought a digest: no fetch, so no line.
  supplied <- tm_mock_install(confirm = function(prompt) TRUE)
  digest <- supplied$digest
  supplied <- tm_mock_install(confirm = function(prompt) TRUE)
  expect_true(install_on_win(install_dir = d, archive_checksum = digest))
  expect_identical(supplied$sidecar, list())
  expect_false(grepl(sidecar_url, supplied$prompts[[1]], fixed = TRUE))

  # A caller-named source has no sidecar to fetch either. It says so, and it
  # says so BEFORE the prompt: what a caller approves includes knowing that
  # nothing will check this download, which an after-the-fact notice does not
  # give them. The two events are recorded into one sequence, so the ordering
  # is asserted rather than inferred from where the message appears.
  order <- character(0)
  named <- tm_mock_install(confirm = function(prompt) {
    order <<- c(order, "prompt")
    TRUE
  })
  withCallingHandlers(
    expect_true(install_on_win(
      download_url = "https://example.invalid/build.7z", install_dir = d
    )),
    message = function(cnd) {
      if (grepl("will not be verified", conditionMessage(cnd), fixed = TRUE)) {
        order <<- c(order, "notice")
      }
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(order, c("notice", "prompt"))
  expect_identical(named$sidecar, list())
  expect_false(grepl(".sha256", named$prompts[[1]], fixed = TRUE))
})

test_that("an unreadable published digest aborts before anything is unpacked", {
  # AC1. Three routes to the same class: a body that is not a digest, a fetch
  # that signals, and a fetch that returns a non-zero status.
  config <- tm_redirect_config()
  tm_redirect_data()
  d <- file.path(withr::local_tempdir(), "ffmpeg")

  garbage <- tm_mock_install(confirm = function(prompt) TRUE,
                             sidecar = "<html>404 Not Found</html>")
  expect_error(
    install_on_win(install_dir = d),
    class = "tidymedia_checksum_unavailable"
  )
  expect_identical(garbage$extract, list())
  expect_identical(garbage$set, list())

  signalled <- tm_mock_install(
    confirm = function(prompt) TRUE,
    download = function(url, destfile, is_sidecar) {
      if (is_sidecar) stop("cannot open URL") else 0L
    }
  )
  expect_error(
    install_on_win(install_dir = d),
    class = "tidymedia_checksum_unavailable"
  )
  # The digest is fetched first, so the archive was never downloaded either.
  expect_identical(signalled$download, list())

  status <- tm_mock_install(
    confirm = function(prompt) TRUE,
    download = function(url, destfile, is_sidecar) if (is_sidecar) 1L else 0L
  )
  expect_error(
    install_on_win(install_dir = d),
    class = "tidymedia_checksum_unavailable"
  )
  expect_identical(status$download, list())
  expect_identical(
    list.files(config$root, recursive = TRUE, all.files = TRUE),
    character(0)
  )
})

test_that("a sidecar the fetch reported but did not deliver aborts classed", {
  # `download.file()` reporting status 0 is not a promise that a readable file
  # arrived. Without a guard on the read, this path leaves install_on_win()
  # through a bare "cannot open connection" -- unclassed, and invisible to the
  # AC6 census, which sees only `return()` and `cli_abort()` nodes.
  tm_redirect_config()
  tm_redirect_data()
  d <- file.path(withr::local_tempdir(), "ffmpeg")

  rec <- tm_mock_install(
    confirm = function(prompt) TRUE,
    download = function(url, destfile, is_sidecar) 0L
  )
  expect_error(
    install_on_win(install_dir = d),
    class = "tidymedia_checksum_unavailable"
  )
  expect_identical(rec$extract, list())
  expect_identical(rec$set, list())
})

test_that("a digest that does not match aborts, names both digests, and registers nothing", {
  # AC1. The config directory is compared before and after, so the claim is
  # about the remembered locations rather than about the return value.
  config <- tm_redirect_config()
  tm_redirect_data()
  d <- file.path(withr::local_tempdir(), "ffmpeg")
  wrong <- paste(rep("a", 64), collapse = "")

  rec <- tm_mock_install(confirm = function(prompt) TRUE, sidecar = wrong)
  before <- tm_roots_snapshot(config$root)
  msg <- tryCatch(
    install_on_win(install_dir = d),
    tidymedia_checksum_mismatch = function(cnd) cli::ansi_strip(conditionMessage(cnd))
  )
  # Both digests are named: the one that was published, and the one the
  # downloaded file actually has.
  expect_true(grepl(wrong, msg, fixed = TRUE))
  expect_true(grepl(rec$digest, msg, fixed = TRUE))
  expect_identical(rec$extract, list())
  expect_identical(rec$set, list())
  expect_identical(tm_roots_snapshot(config$root), before)
})

test_that("a caller-supplied digest is checked case-insensitively and fetches nothing", {
  # AC2. Both cases of the same digest are accepted, and neither one sends the
  # call to fetch a second copy of a digest it was already handed.
  tm_redirect_config()
  tm_redirect_data()
  d <- file.path(withr::local_tempdir(), "ffmpeg")

  probe <- tm_mock_install(confirm = function(prompt) TRUE)
  digest <- probe$digest

  for (value in c(digest, toupper(digest))) {
    d <- file.path(withr::local_tempdir(), "ffmpeg")
    rec <- tm_mock_install(confirm = function(prompt) TRUE)
    expect_true(install_on_win(install_dir = d, archive_checksum = value))
    expect_identical(rec$sidecar, list())
    expect_identical(length(rec$extract), 1L)
  }

  # And a supplied digest that is wrong still refuses, on this path as on the
  # published one -- the class is the same failure.
  wrong <- tm_mock_install(confirm = function(prompt) TRUE)
  expect_error(
    install_on_win(install_dir = d, archive_checksum = paste(rep("a", 64), collapse = "")),
    class = "tidymedia_checksum_mismatch"
  )
  expect_identical(wrong$set, list())
})

test_that("archive_checksum is refused at the front door, unclassed, before any fetch", {
  # AC2. The four rejected shapes, and the ordering: the download mock aborts
  # rather than returning, so a value that got past the check would fail here
  # instead of quietly passing the test.
  tm_redirect_config()
  tm_redirect_data()
  testthat::local_mocked_bindings(
    download.file = function(...) stop("download.file() must not be reached"),
    .package = "utils"
  )
  hex <- paste(rep("a", 64), collapse = "")
  bad <- list(
    short = substr(hex, 1, 63),
    non_hex = paste0(substr(hex, 1, 63), "z"),
    missing = NA_character_,
    two = c(hex, hex)
  )
  for (name in names(bad)) {
    cnd <- tryCatch(
      install_on_win(archive_checksum = bad[[name]], confirm = FALSE),
      error = function(cnd) cnd
    )
    expect_s3_class(cnd, "rlang_error")
    # The argument is named, so the caller is told which one to fix.
    expect_true(
      grepl("archive_checksum", cli::ansi_strip(conditionMessage(cnd)), fixed = TRUE),
      label = paste(name, "names the argument")
    )
    # And, like the package's other front-door checks, it is unclassed: the
    # tidymedia_* classes describe an install that went wrong, and here none
    # was attempted.
    expect_false(
      any(grepl("^tidymedia_", class(cnd))),
      label = paste(name, "carries no tidymedia_ class")
    )
  }
})

test_that("an archive libarchive cannot read aborts without libarchive's text", {
  # AC3. `unpack = NULL` leaves the real archive::archive_extract() in place,
  # so these two fixtures reach libarchive itself. They cover its two failure
  # routes: refusing to open at all, and failing to decompress an archive it
  # did open (data-raw/corrupt-archive-fixtures.R).
  tm_redirect_config()
  tm_redirect_data()

  for (fixture in c("not-an-archive.7z", "corrupt-payload.7z")) {
    d <- file.path(withr::local_tempdir(), "ffmpeg")
    rec <- tm_mock_install(
      confirm = function(prompt) TRUE,
      unpack = NULL,
      archive = testthat::test_path("fixtures", fixture)
    )
    open_before <- nrow(showConnections(all = FALSE))
    cnd <- tryCatch(
      install_on_win(install_dir = d, archive_checksum = rec$digest),
      error = function(cnd) cnd
    )
    expect_s3_class(cnd, "tidymedia_archive_unreadable")

    # The mechanism the surviving download was made of, asserted where every
    # platform can see it: archive_extract() opens its own connection to the
    # archive and, on a failure inside archive_read_data_block(), leaves it
    # open -- and Windows will not delete a file something still holds open.
    # The file.exists() line below is the criterion, but it can only go red on
    # Windows, so this one is what a developer running the suite anywhere else
    # would see if tm_unpack() stopped owning the connection.
    expect_identical(
      nrow(showConnections(all = FALSE)), open_before,
      label = paste(fixture, "leaves no connection open")
    )

    # The abort names the archive it could not read. Whether it also names the
    # install directory now depends on what the cleanup managed: M103 gave
    # this call its directories back, so where `d` is gone the message must
    # not point at it, and where the extraction left something undeletable
    # the message names both the directory and what stayed. AC7's own tests
    # below assert each branch; here the two are held together, so neither
    # can be satisfied by the message saying nothing at all.
    msg <- cli::ansi_strip(conditionMessage(cnd))
    expect_true(grepl(rec$destfile, msg, fixed = TRUE), label = fixture)
    expect_identical(dir.exists(d), grepl(d, msg, fixed = TRUE), label = fixture)

    # And libarchive's own text reaches the caller nowhere -- not in this
    # message, and not in any condition carried underneath it.
    walk <- cnd
    while (!is.null(walk)) {
      expect_false(
        grepl("archive_extract.cpp", cli::ansi_strip(conditionMessage(walk)), fixed = TRUE),
        label = paste(fixture, "chain is free of libarchive's source location")
      )
      walk <- walk$parent
    }
    # Nothing was registered, and the download did not survive the failure.
    expect_identical(rec$set, list())
    expect_false(file.exists(rec$destfile), label = fixture)
  }

  # The control: the same path, succeeding, also leaves no download behind and
  # no connection open.
  d <- file.path(withr::local_tempdir(), "ffmpeg")
  ok <- tm_mock_install(confirm = function(prompt) TRUE)
  open_before <- nrow(showConnections(all = FALSE))
  expect_true(install_on_win(install_dir = d, archive_checksum = ok$digest))
  expect_identical(nrow(showConnections(all = FALSE)), open_before)
  expect_false(file.exists(ok$destfile))
})

test_that("the real archive_extract() returns the paths the mock stands in for", {
  # The whole AC4 registration gate reads `tm_unpack()`'s file list, and every
  # test above reaches that list through a mock which hard-codes its shape.
  # This is the one test that asks libarchive itself, so a change in what
  # `archive_extract()` returns -- absolute paths, a `./` prefix, paths before
  # `strip_components` rather than after -- fails here instead of shipping and
  # aborting on every real Windows install.
  skip_if_not_installed("archive")
  # The upstream layout: one wrapper directory named for the build, holding
  # `bin/`. `strip_components = 1` is what removes the wrapper.
  root <- withr::local_tempdir()
  wrapper <- "ffmpeg-8.0-essentials_build"
  dir.create(file.path(root, wrapper, "bin"), recursive = TRUE)
  members <- file.path(wrapper, "bin", paste0(tm_install_registers, ".exe"))
  for (member in members) writeLines("stub", file.path(root, member))
  arch <- file.path(withr::local_tempdir(), "control.7z")
  withr::with_dir(root, archive::archive_write_files(arch, members))

  d <- withr::local_tempdir()
  produced <- tm_unpack(arch, d)
  # Relative to the directory, after strip_components -- which is exactly the
  # string tm_extracted_programs() matches against.
  expect_setequal(produced$files, file.path("bin", paste0(tm_install_registers, ".exe")))
  expect_setequal(
    tm_extracted_programs(produced$files, tm_install_registers),
    tm_install_registers
  )
  # A succeeding unpack removed nothing, so it has nothing to report (M103).
  expect_identical(produced$leftovers, character(0))
})

test_that("tm_extracted_programs() reads the path shapes libarchive can report", {
  # The normalization exists for Windows, where a backslash separator and a
  # capitalized name are both ordinary -- and where no test on this machine
  # would otherwise execute it.
  expect_setequal(
    tm_extracted_programs(
      c("bin\\ffmpeg.exe", "./bin/FFprobe.EXE", "bin/ffplay.exe"),
      tm_install_registers
    ),
    tm_install_registers
  )
  # And it stays a match on the install's own layout, not a name search: the
  # same three programs one directory deeper are not what the install reads.
  expect_identical(
    tm_extracted_programs(
      file.path("tools", "bin", paste0(tm_install_registers, ".exe")),
      tm_install_registers
    ),
    character(0)
  )
})

test_that("only the programs the extraction produced are registered", {
  # AC4. Four builds, told apart by what the extraction leaves behind.
  tm_redirect_config()
  tm_redirect_data()
  registered <- function(rec) vapply(rec$set, function(x) x$program, character(1))

  all_three <- tm_mock_install(confirm = function(prompt) TRUE)
  d <- file.path(withr::local_tempdir(), "ffmpeg")
  expect_true(install_on_win(install_dir = d, archive_checksum = all_three$digest))
  expect_identical(registered(all_three), c("ffmpeg", "ffprobe", "ffplay"))

  # ffplay is optional: its absence is said out loud and the install still
  # succeeds, but no location is remembered for it.
  d <- file.path(withr::local_tempdir(), "ffmpeg")
  no_ffplay <- tm_mock_install(
    confirm = function(prompt) TRUE, unpack = c("ffmpeg", "ffprobe")
  )
  expect_message(
    expect_true(install_on_win(install_dir = d, archive_checksum = no_ffplay$digest)),
    "ffplay"
  )
  expect_identical(registered(no_ffplay), c("ffmpeg", "ffprobe"))

  # Either required program missing refuses, names it, and writes nothing at
  # all -- not even the one that WAS unpacked.
  for (absent in c("ffmpeg", "ffprobe")) {
    d <- file.path(withr::local_tempdir(), "ffmpeg")
    rec <- tm_mock_install(
      confirm = function(prompt) TRUE,
      unpack = setdiff(tm_install_registers, absent)
    )
    cnd <- tryCatch(
      install_on_win(install_dir = d, archive_checksum = rec$digest),
      error = function(cnd) cnd
    )
    expect_s3_class(cnd, "tidymedia_program_not_extracted")
    expect_true(
      grepl(absent, cli::ansi_strip(conditionMessage(cnd)), fixed = TRUE),
      label = paste("names", absent)
    )
    expect_identical(rec$set, list())
  }
})

test_that("a re-install into a used directory counts only what THIS extraction produced", {
  # AC4, on the path the default `install_dir` puts every caller on: the same
  # directory, install after install, never cleared by the extraction. A
  # directory listing cannot tell this build's binaries from the last one's,
  # so a build missing a required program has to refuse even where the file
  # it did not produce is sitting there from before.
  tm_redirect_config()
  tm_redirect_data()
  d <- file.path(withr::local_tempdir(), "ffmpeg")
  bin <- file.path(d, "bin")
  dir.create(bin, recursive = TRUE)
  for (program in c("ffmpeg", "ffplay")) {
    file.create(file.path(bin, paste0(program, ".exe")))
  }

  rec <- tm_mock_install(confirm = function(prompt) TRUE, unpack = "ffprobe")
  cnd <- tryCatch(
    install_on_win(install_dir = d, archive_checksum = rec$digest),
    error = function(cnd) cnd
  )
  expect_s3_class(cnd, "tidymedia_program_not_extracted")
  expect_true(
    grepl("ffmpeg", cli::ansi_strip(conditionMessage(cnd)), fixed = TRUE)
  )
  expect_identical(rec$set, list())

  # And the optional half of the same reading: a build producing the two
  # required programs and no ffplay says so and remembers no ffplay location,
  # even with a previous run's ffplay.exe still in the directory.
  leftover <- tm_mock_install(
    confirm = function(prompt) TRUE, unpack = c("ffmpeg", "ffprobe")
  )
  expect_message(
    expect_true(install_on_win(install_dir = d, archive_checksum = leftover$digest)),
    "ffplay"
  )
  expect_identical(
    vapply(leftover$set, function(x) x$program, character(1)),
    c("ffmpeg", "ffprobe")
  )
})

test_that("an archive the fetch reported but did not deliver aborts classed", {
  # The archive-side counterpart of the sidecar guard above: status 0 with no
  # file left behind. Without it, tm_archive_digest() aborts with a bare
  # simpleError -- unclassed, and invisible to AC6's census, which sees only
  # `return()` and `cli_abort()` nodes (AC5).
  tm_redirect_config()
  tm_redirect_data()
  d <- file.path(withr::local_tempdir(), "ffmpeg")

  rec <- tm_mock_install(
    confirm = function(prompt) TRUE,
    download = function(url, destfile, is_sidecar) {
      # The sidecar arrives, so the run reaches the archive fetch and fails
      # there rather than one step earlier.
      if (is_sidecar) {
        writeLines(paste(rep("a", 64), collapse = ""), destfile)
      }
      0L
    }
  )
  cnd <- tryCatch(install_on_win(install_dir = d), error = function(cnd) cnd)
  expect_s3_class(cnd, "tidymedia_download_unavailable")
  expect_identical(rec$extract, list())
  expect_identical(rec$set, list())
})

test_that("what an install remembers is read back off disk, not off the record", {
  # AC4 again, with the REAL set_program() in place: the criterion's claim is
  # about config FILES, and every assertion above it reads the recorded calls
  # instead. This one asks the config directory what is actually there, so a
  # write reaching it by some route other than set_program() is visible too.
  config <- tm_redirect_config()
  tm_redirect_data()
  file_for <- function(p) tm_config_file(p)

  d <- file.path(withr::local_tempdir(), "ffmpeg")
  all_three <- tm_mock_install(confirm = function(prompt) TRUE, real_set = TRUE)
  expect_true(install_on_win(install_dir = d, archive_checksum = all_three$digest))
  for (program in tm_install_registers) {
    expect_true(file.exists(file_for(program)), label = paste(program, "written"))
    expect_identical(
      readLines(file_for(program)),
      tm_install_binary(d, program),
      label = paste(program, "contents")
    )
  }

  # ffplay optional: two files, and no third.
  config <- tm_redirect_config()
  file_for <- function(p) tm_config_file(p)
  d <- file.path(withr::local_tempdir(), "ffmpeg")
  no_ffplay <- tm_mock_install(
    confirm = function(prompt) TRUE, unpack = c("ffmpeg", "ffprobe"),
    real_set = TRUE
  )
  expect_message(
    expect_true(install_on_win(install_dir = d, archive_checksum = no_ffplay$digest)),
    "ffplay"
  )
  expect_true(file.exists(file_for("ffmpeg")))
  expect_true(file.exists(file_for("ffprobe")))
  expect_false(file.exists(file_for("ffplay")))

  # A required program missing writes nothing at all: the config directory is
  # byte-for-byte what it was, not merely missing the absent program's file.
  for (absent in tm_install_required) {
    config <- tm_redirect_config()
    d <- file.path(withr::local_tempdir(), "ffmpeg")
    rec <- tm_mock_install(
      confirm = function(prompt) TRUE,
      unpack = setdiff(tm_install_registers, absent),
      real_set = TRUE
    )
    before <- tm_roots_snapshot(config$root)
    expect_error(
      install_on_win(install_dir = d, archive_checksum = rec$digest),
      class = "tidymedia_program_not_extracted"
    )
    expect_identical(tm_roots_snapshot(config$root), before, label = absent)
  }
})

test_that("tm_archive_digest() computes SHA-256, pinned to a known answer", {
  # Every verifying test above compares tm_archive_digest() against itself:
  # the published digest and the downloaded file's digest both come from it,
  # so `algo = "sha256"` quietly becoming another 64-hex algorithm would leave
  # all of them green. This pins one file to the digest `shasum -a 256` gives.
  f <- withr::local_tempfile()
  writeBin(charToRaw("abc"), f)
  expect_identical(
    tm_archive_digest(f),
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  )

  # And a file that is not there is NULL, not a bare simpleError: the caller
  # turns that NULL into a classed refusal, which it cannot do for a condition
  # raised out from under it.
  expect_null(tm_archive_digest(file.path(tempdir(), "no-such-archive")))
})

test_that("an archive that vanishes between the fetch and the digest refuses classed", {
  # The second line of defense behind tm_fetch()'s existence test: whatever
  # the reason the digest cannot be computed, the refusal is AC5's classed
  # one naming the URL rather than whatever `digest` happened to raise.
  tm_redirect_config()
  tm_redirect_data()
  d <- file.path(withr::local_tempdir(), "ffmpeg")

  rec <- tm_mock_install(confirm = function(prompt) TRUE)
  testthat::local_mocked_bindings(tm_archive_digest = function(path) NULL)
  cnd <- tryCatch(install_on_win(install_dir = d), error = function(cnd) cnd)
  expect_s3_class(cnd, "tidymedia_download_unavailable")
  expect_identical(rec$extract, list())
  expect_identical(rec$set, list())
})

test_that("tm_unpack() reports failure where the archive cannot even be opened", {
  # The connection is opened by tm_unpack() itself (T9), so a path that is not
  # there fails before libarchive is reached -- and has to read as the same
  # refusal, since the caller only asks whether the unpack produced anything.
  # `file()` warns on its way to the error it raises; the warning is base R's
  # and says the same thing, so it is suppressed rather than asserted.
  out <- suppressWarnings(
    tm_unpack(file.path(tempdir(), "no-such-archive"), tempdir())
  )
  expect_null(out$files)
  # And nothing was extracted, so nothing is named as left behind -- the
  # cleanup is never reached on this path (M103).
  expect_identical(out$leftovers, character(0))
})

test_that("a download that does not deliver aborts, keeping the base condition underneath", {
  # AC5. Both shapes download.file()'s contract allows, asserted by class.
  tm_redirect_config()
  tm_redirect_data()
  d <- file.path(withr::local_tempdir(), "ffmpeg")
  u <- "https://example.invalid/build.7z"

  signalled <- tm_mock_install(
    confirm = function(prompt) TRUE,
    download = function(url, destfile, is_sidecar) stop("cannot open URL '", url, "'")
  )
  cnd <- tryCatch(
    suppressMessages(install_on_win(download_url = u, install_dir = d)),
    error = function(cnd) cnd
  )
  expect_s3_class(cnd, "tidymedia_download_unavailable")
  expect_true(grepl(u, cli::ansi_strip(conditionMessage(cnd)), fixed = TRUE))
  # The base condition is kept, so a caller who wants base R's wording can
  # still reach it -- it just is not what the top-level message says.
  expect_s3_class(cnd$parent, "condition")
  expect_true(grepl("cannot open URL", conditionMessage(cnd$parent), fixed = TRUE))
  expect_identical(signalled$set, list())

  status <- tm_mock_install(
    confirm = function(prompt) TRUE,
    download = function(url, destfile, is_sidecar) 1L
  )
  expect_error(
    suppressMessages(install_on_win(download_url = u, install_dir = d)),
    class = "tidymedia_download_unavailable"
  )
  expect_identical(status$set, list())
})


# install_on_win()'s exits are all accounted for (M102 AC6) -------------------

# Collect every `return()` and every `cli::cli_abort()` call in a function's
# own body. The DOMAIN is derived rather than listed: a new exit added to
# install_on_win() is collected the day it is written, so the census cannot go
# stale the way a hand-kept list of sites would.
#
# Two node types only, and deliberately: a call to a helper is neither, so the
# front-door checks and the aborts that originate inside tm_confirm() and
# set_program() are outside what this can see. AC6 names those rather than
# claiming them, and T2 sites every classed abort in the body itself so that
# what the walk cannot see is only what is named.
tm_collect_exits <- function(fn) {
  found <- list()
  walk <- function(node) {
    if (is.call(node)) {
      if (identical(tm_call_name(node), "return") ||
          identical(tm_call_name(node), "cli_abort")) {
        found[[length(found) + 1L]] <<- node
      }
    }
    if (is.call(node) || is.pairlist(node)) {
      parts <- as.list(node)
      # Two node kinds hold the empty symbol: a call with a blank argument
      # (`x[, 1]`), and the formals of an inline `function(x) ...`, whose
      # arguments without defaults are empty. Binding one to a variable makes
      # that variable missing, so the parts are reached by index and the empty
      # ones skipped before they are ever forced.
      for (i in seq_along(parts)) {
        if (identical(parts[[i]], quote(expr = ))) next
        walk(parts[[i]])
      }
    }
  }
  walk(body(fn))
  found
}

# The name a call calls, seeing through `pkg::fn`.
tm_call_name <- function(node) {
  head <- node[[1]]
  if (is.name(head)) return(as.character(head))
  if (is.call(head) && identical(as.character(head[[1]]), "::")) {
    return(as.character(head[[3]]))
  }
  NA_character_
}

# The classes a collected `cli_abort()` passes, as a character vector, or
# none. `class = c("tidymedia_x", "y")` reaches here as an unevaluated call to
# `c()` rather than as a vector, which a plain is.character() test would read
# as no class at all; a vector holding the class IS catchable by it, so the
# constant form is unwrapped instead.
tm_exit_classes <- function(node) {
  class <- node[["class"]]
  if (is.null(class)) return(character(0))
  if (is.call(class) && identical(tm_call_name(class), "c")) {
    parts <- as.list(class)[-1]
    if (!all(vapply(parts, is.character, logical(1)))) return(character(0))
    class <- unlist(parts)
  }
  if (is.character(class)) class else character(0)
}

# The property AC6 asserts of each collected exit: a `return()` hands back a
# literal TRUE or FALSE, and a `cli_abort()` passes a class beginning
# `tidymedia_`. A bare `return()` -- which hands back NULL -- is read as
# non-compliant rather than left to throw a subscript error out of the walk.
tm_exit_ok <- function(node) {
  if (identical(tm_call_name(node), "return")) {
    if (length(node) < 2L) return(FALSE)
    value <- node[[2]]
    return(is.logical(value) && length(value) == 1L && !is.na(value))
  }
  any(startsWith(tm_exit_classes(node), "tidymedia_"))
}

test_that("every exit install_on_win() takes in its own body is TRUE/FALSE or classed", {
  # AC6. The claim is exactly the collected set.
  exits <- tm_collect_exits(install_on_win)
  for (node in exits) {
    expect_true(tm_exit_ok(node), label = deparse1(node))
  }

  # The floor against a collector that silently under-reads the body: the set
  # holds the five abort sites AC1, AC3, AC4 and AC5 name. Without this, a
  # walk that found nothing would satisfy the loop above vacuously.
  classes <- unlist(lapply(exits, tm_exit_classes))
  expect_true(all(
    c(
      "tidymedia_checksum_unavailable",
      "tidymedia_checksum_mismatch",
      "tidymedia_download_unavailable",
      "tidymedia_archive_unreadable",
      "tidymedia_program_not_extracted"
    ) %in% classes
  ))
})

test_that("the exit census can fail", {
  # The collector and the property are only worth their green if they go red
  # on the two defects they exist to catch, so both are planted here rather
  # than left to a one-time check that no later reader can re-run.
  unclassed <- function() cli::cli_abort("something went wrong")
  non_literal <- function(x) return(x)
  compliant <- function(x) {
    if (x) return(FALSE)
    cli::cli_abort("no", class = "tidymedia_planted")
  }
  # Two shapes the property has to READ rather than break on: a bare
  # `return()`, whose value node does not exist, and a class passed as a
  # vector, which is catchable and so compliant. Neither shape is in
  # install_on_win() today, which is why they are asserted here.
  bare <- function() return()
  vector_class <- function() {
    cli::cli_abort("no", class = c("tidymedia_planted", "extra"))
  }

  ok <- function(fn) all(vapply(tm_collect_exits(fn), tm_exit_ok, logical(1)))
  expect_false(ok(unclassed))
  expect_false(ok(non_literal))
  expect_true(ok(compliant))
  expect_false(ok(bare))
  expect_true(ok(vector_class))
  expect_identical(
    tm_exit_classes(tm_collect_exits(vector_class)[[1]]),
    c("tidymedia_planted", "extra")
  )

  # And the collector reads a non-empty domain in each case, so "no exits
  # found" can never be what makes a function look compliant.
  expect_identical(length(tm_collect_exits(unclassed)), 1L)
  expect_identical(length(tm_collect_exits(non_literal)), 1L)
  expect_identical(length(tm_collect_exits(compliant)), 2L)
})


# Every refusal above the unpack leaves no directory behind (M103 AC3) -------

# The M102 census, narrowed to the exits that come BEFORE a named call.
#
# `tm_collect_exits()` walks the whole body and cannot say where in it a node
# sat, which is the one fact AC3's domain turns on: the rule is about the
# refusals that happen while the install directory holds nothing the call put
# there. The narrowing is positional rather than by name or class because the
# body's own order is what decides it -- the same abort class appears on both
# sides of some calls, and a class-keyed filter would silently take the wrong
# one.
#
# The filter runs over the body's top-level statements: `tm_unpack()` is
# called from one of them, and everything a `{` block holds before that
# statement is what precedes the call.
tm_collect_exits_before <- function(fn, callee) {
  statements <- as.list(body(fn))[-1]
  holds_callee <- vapply(
    statements,
    function(st) {
      hit <- FALSE
      walk <- function(node) {
        if (is.call(node)) {
          if (identical(tm_call_name(node), callee)) hit <<- TRUE
          for (part in as.list(node)) {
            if (identical(part, quote(expr = ))) next
            walk(part)
          }
        }
      }
      walk(st)
      hit
    },
    logical(1)
  )
  stopifnot(any(holds_callee))
  before <- statements[seq_len(which(holds_callee)[[1]] - 1L)]
  unlist(lapply(before, function(st) {
    tm_collect_exits(eval(call("function", NULL, st)))
  }), recursive = FALSE)
}

# A stable name for a collected exit: the condition class it raises, or the
# value it returns, plus its ordinal among the exits naming the same one.
#
# Position alone would do, but a key made of position alone renumbers every
# case below it the moment an exit is inserted above -- so a real omission and
# a harmless insertion would fail the same way. Keyed like this, a new exit
# raising a new class takes a key no case claims and the bijection names IT,
# while a new exit raising an EXISTING class pushes the count past the
# registry's and the bijection fails on the count.
tm_exit_keys <- function(nodes) {
  names <- vapply(nodes, function(node) {
    if (identical(tm_call_name(node), "return")) {
      return(paste0("return(", deparse1(node[[2]]), ")"))
    }
    classes <- tm_exit_classes(node)
    classes <- classes[startsWith(classes, "tidymedia_")]
    if (length(classes)) classes[[1]] else deparse1(node)
  }, character(1))
  vapply(
    seq_along(names),
    function(i) paste0(names[[i]], " #", sum(names[seq_len(i)] == names[[i]])),
    character(1)
  )
}

# One triggering case per exit AC3's domain holds, keyed by tm_exit_keys().
#
# Each case is a function of the install directory that drives install_on_win()
# to exactly that exit and returns nothing; the runner below supplies the
# directory and does the asserting, so a case says only how to reach its exit.
# `env` is the caller's frame, which is what the withr/testthat mocks below
# have to be scoped to.
#
# The two `return(FALSE)` exits are told apart by what makes them fire, which
# is also the only difference AC3 cares about: the first refuses above the
# first dir.create(), the second is dir.create() itself failing after it has
# already made the parents.
tm_ac3_cases <- function(env = parent.frame()) {
  list(
    "return(FALSE) #1" = function(dir) {
      tm_mock_install(confirm = function(prompt) FALSE, env = env)
      expect_false(install_on_win(install_dir = dir))
    },
    "return(FALSE) #2" = function(dir) {
      rec <- tm_mock_install(confirm = function(prompt) TRUE, env = env)
      # Captured BEFORE the mock replaces the binding, so the mock can still
      # make the parents the real recursive call would have made. Without
      # that, this case would assert the removal of directories nothing
      # created.
      real_dir_create <- base::dir.create
      testthat::local_mocked_bindings(
        dir.create = function(path, showWarnings = TRUE, recursive = FALSE,
                              mode = "0777") {
          real_dir_create(dirname(path), showWarnings = FALSE, recursive = TRUE)
          FALSE
        },
        .package = "base", .env = env
      )
      expect_false(install_on_win(install_dir = dir, archive_checksum = rec$digest))
    },
    "tidymedia_checksum_unavailable #1" = function(dir) {
      tm_mock_install(
        confirm = function(prompt) TRUE,
        download = function(url, destfile, is_sidecar) if (is_sidecar) 1L else 0L,
        env = env
      )
      expect_error(
        install_on_win(install_dir = dir),
        class = "tidymedia_checksum_unavailable"
      )
    },
    "tidymedia_checksum_unavailable #2" = function(dir) {
      tm_mock_install(
        confirm = function(prompt) TRUE, sidecar = "no digest here", env = env
      )
      expect_error(
        install_on_win(install_dir = dir),
        class = "tidymedia_checksum_unavailable"
      )
    },
    "tidymedia_download_unavailable #1" = function(dir) {
      rec <- tm_mock_install(
        confirm = function(prompt) TRUE,
        download = function(url, destfile, is_sidecar) 1L,
        env = env
      )
      expect_error(
        install_on_win(install_dir = dir, archive_checksum = rec$digest),
        class = "tidymedia_download_unavailable"
      )
    },
    "tidymedia_download_unavailable #2" = function(dir) {
      rec <- tm_mock_install(confirm = function(prompt) TRUE, env = env)
      testthat::local_mocked_bindings(
        tm_archive_digest = function(path) NULL, .env = env
      )
      expect_error(
        install_on_win(install_dir = dir, archive_checksum = rec$digest),
        class = "tidymedia_download_unavailable"
      )
    },
    "tidymedia_checksum_mismatch #1" = function(dir) {
      tm_mock_install(confirm = function(prompt) TRUE, env = env)
      expect_error(
        install_on_win(install_dir = dir, archive_checksum = strrep("a", 64)),
        class = "tidymedia_checksum_mismatch"
      )
    }
  )
}

# The five refusals the walk cannot see, because none of them is a `return()`
# or a `cli_abort()` in install_on_win()'s own body: four front-door checks
# that abort inside rlang or check_sha256(), and tm_confirm()'s refusal to
# assume consent where there is no one to ask.
#
# Each refuses ABOVE the call's first dir.create(), so each must create no
# directory at all -- which is why they take the install directory as a path
# to check rather than a path to pass. The three that cannot accept a
# directory argument at all take the default one instead.
tm_ac3_uncovered_cases <- function(env = parent.frame()) {
  list(
    # The four front-door checks abort inside rlang, which gives all four the
    # same `rlang_error` class -- so the message is the only instrument that
    # tells them apart, and each match names its own argument and its own
    # constraint. A classless `expect_error()` here would stay green with the
    # check it names deleted, because the call would then fail somewhere else
    # or not at all.
    "check_bool(confirm)" = function(dir) {
      expect_error(
        install_on_win(install_dir = dir, confirm = "yes"),
        "`confirm` must be `TRUE` or `FALSE`"
      )
    },
    "check_string(download_url)" = function(dir) {
      expect_error(
        install_on_win(install_dir = dir, download_url = 1),
        "`download_url` must be a single string"
      )
    },
    "check_string(install_dir)" = function(dir) {
      expect_error(
        install_on_win(install_dir = 1),
        "`install_dir` must be a single string"
      )
    },
    "check_sha256(archive_checksum)" = function(dir) {
      expect_error(
        install_on_win(install_dir = dir, archive_checksum = "nope"),
        "`archive_checksum` must be a SHA-256 digest"
      )
    },
    "tm_confirm() has no one to ask" = function(dir) {
      # The real tm_confirm() is left in place and the session is not
      # interactive, which is the state D080 refuses rather than assumes
      # consent for.
      tm_mock_install(confirm = NULL, env = env)
      expect_error(
        install_on_win(install_dir = dir),
        class = "tidymedia_confirmation_unavailable"
      )
    }
  )
}

# The exits that cannot be reached with the install directory already there:
# `dir.create()` only runs where it is not.
tm_ac3_needs_new_dir <- "return(FALSE) #2"

# Run one case in a frame of its own, so its mocks and temp files unwind
# before the next case starts. Building the registry HERE is what scopes them:
# the cases take `env` from this frame.
tm_ac3_run <- function(key, dir) {
  tm_redirect_config()
  data_root <- tm_redirect_data()
  cases <- c(
    tm_ac3_cases(env = environment()),
    tm_ac3_uncovered_cases(env = environment())
  )
  cases[[key]](dir)
  # What the case created under the DEFAULT install location, read here
  # rather than by the caller: the redirected root is this frame's temp
  # directory and is deleted the moment this function returns, so a caller
  # computing the default path for itself would be asking about a root no run
  # ever used. That is what made the assertion below unfalsifiable before
  # (M103 review pass 1).
  setdiff(list.dirs(data_root), data_root)
}

test_that("every exit above the unpack has a case, and every case has an exit", {
  # AC3's bijection. The domain is derived from the body, so an exit added
  # above the unpack with no case here fails this the day it is written; a
  # case naming an exit that no longer exists fails it the same way.
  exits <- tm_collect_exits_before(install_on_win, "tm_unpack")
  expect_setequal(tm_exit_keys(exits), names(tm_ac3_cases()))
  expect_identical(length(exits), length(tm_ac3_cases()))

  # The floor against a filter that silently under-reads: the narrowed set
  # still holds both `return(FALSE)` exits and the three abort classes that
  # can only fire above the unpack.
  expect_true(all(
    c("return(FALSE) #1", "return(FALSE) #2",
      "tidymedia_checksum_unavailable #1", "tidymedia_download_unavailable #1",
      "tidymedia_checksum_mismatch #1") %in% tm_exit_keys(exits)
  ))
  # And the floor against a filter that reads too far: the three exits BELOW
  # the unpack are the ones the narrowing exists to drop.
  all_classes <- unlist(lapply(tm_collect_exits(install_on_win), tm_exit_classes))
  expect_true("tidymedia_archive_unreadable" %in% all_classes)
  expect_false(
    "tidymedia_archive_unreadable" %in%
      unlist(lapply(exits, tm_exit_classes))
  )
  expect_false(
    "tidymedia_program_not_extracted" %in%
      unlist(lapply(exits, tm_exit_classes))
  )
  # The third, added by M104 and below the unpack for the same reason.
  expect_true("tidymedia_program_unusable" %in% all_classes)
  expect_false(
    "tidymedia_program_unusable" %in%
      unlist(lapply(exits, tm_exit_classes))
  )
})

test_that("a refusal above the unpack leaves no directory the call created", {
  # AC3, first half. The install directory is named two levels below a
  # directory that already exists, so `dir.create(recursive = TRUE)` has three
  # levels to make and the assertion is about the outermost of them: a removal
  # that stopped at `install_dir` would leave the other two behind and fail
  # here.
  for (key in names(tm_ac3_cases())) {
    root <- withr::local_tempdir()
    outermost <- file.path(root, "made")
    dir <- file.path(outermost, "by-the-call", "ffmpeg")
    tm_ac3_run(key, dir)
    expect_false(dir.exists(dir), label = key)
    expect_false(dir.exists(outermost), label = paste(key, "outermost"))
  }
})

test_that("a refusal above the unpack leaves an existing install directory as it was", {
  # AC3, second half. A directory the call did NOT create is never removed,
  # and neither is anything in it -- the same created-or-changed line the
  # unpack cleanup draws, drawn here.
  for (key in setdiff(names(tm_ac3_cases()), tm_ac3_needs_new_dir)) {
    root <- withr::local_tempdir()
    dir <- file.path(root, "ffmpeg")
    dir.create(file.path(dir, "keep"), recursive = TRUE)
    writeLines("the caller's own file", file.path(dir, "keep", "mine.txt"))
    before <- tm_dir_snapshot(dir)

    tm_ac3_run(key, dir)

    expect_true(dir.exists(dir), label = key)
    expect_identical(tm_dir_snapshot(dir), before, label = key)
  }
})

test_that("the refusals the census cannot see create no directory at all", {
  # AC3's named exceptions. None of the five is a `return()` or a
  # `cli_abort()` in install_on_win()'s own body, so no walk over that body
  # can find them; each is listed by hand and each refuses above the first
  # dir.create(). Both the directory the caller named and the default one are
  # checked, because two of the five refuse before either has been resolved.
  for (key in names(tm_ac3_uncovered_cases())) {
    root <- withr::local_tempdir()
    dir <- file.path(root, "made", "by-the-call", "ffmpeg")

    created_by_default <- tm_ac3_run(key, dir)

    expect_false(dir.exists(file.path(root, "made")), label = key)
    # And nothing under the default location either, which is where the two
    # cases that refuse before `install_dir` is resolved would create one.
    # It is NOT what carries `check_string(install_dir)`: that case passes
    # `install_dir = 1`, which is non-NULL, so `tm_install_dir()` is never
    # reached and this assertion could not go red for it whatever the call
    # did. That case is carried by its own message assertion inside
    # `tm_ac3_run()`, and the cell as a whole is falsifiable -- commenting out
    # `rlang::check_string(install_dir, allow_null = TRUE)` turns it red
    # (M103 review pass 2). That THIS assertion can go red is shown by the
    # M098 default-install-dir test above, where a call allowed past the
    # checks creates `R/tidymedia/ffmpeg` beneath exactly this root.
    expect_identical(created_by_default, character(0), label = paste(key, "default dir"))
  }
})

# The unpack refusal says what it left behind (M103 AC7) ---------------------

test_that("a failed unpack that leaves nothing says so, and takes its own directory back", {
  # AC7, the two states with no leftovers, over both libarchive failure
  # routes. `unpack = NULL` leaves the real archive::archive_extract() in
  # place, so what the message reports is what the cleanup actually did.
  tm_redirect_config()
  tm_redirect_data()

  for (fixture in c("not-an-archive.7z", "corrupt-payload.7z")) {
    # State one: the call created the install directory. It is removed again,
    # and the message names no directory -- naming one it had just deleted is
    # the failure this asserts against.
    root <- withr::local_tempdir()
    made <- file.path(root, "made", "ffmpeg")
    rec <- tm_mock_install(
      confirm = function(prompt) TRUE, unpack = NULL,
      archive = testthat::test_path("fixtures", fixture)
    )
    cnd <- tryCatch(
      install_on_win(install_dir = made, archive_checksum = rec$digest),
      error = function(cnd) cnd
    )
    expect_s3_class(cnd, "tidymedia_archive_unreadable")
    msg <- cli::ansi_strip(conditionMessage(cnd))
    if (dir.exists(made)) {
      # The extraction left something the platform will not delete. The
      # message then names the directory and every surviving entry under it,
      # so the caller can go and look -- the branch Windows takes.
      survivors <- list.files(
        made, recursive = TRUE, all.files = TRUE, include.dirs = TRUE,
        no.. = TRUE
      )
      expect_gt(length(survivors), 0)
      expect_true(grepl(made, msg, fixed = TRUE), label = fixture)
      expect_true(grepl(file.path(made, survivors[[1]]), msg, fixed = TRUE),
                  label = fixture)
      expect_match(msg, "could not", fixed = TRUE)
    } else {
      # Nothing survived, so the directory this call made is gone too, and
      # the message names no directory the caller could not go and look at.
      expect_false(dir.exists(file.path(root, "made")), label = fixture)
      expect_false(grepl(made, msg, fixed = TRUE), label = fixture)
      expect_match(msg, "removed it again", fixed = TRUE)
    }

    # State two: the directory was already there, holding a file of the
    # caller's. It stays, with its file, and the message says so.
    kept <- file.path(withr::local_tempdir(), "ffmpeg")
    dir.create(kept, recursive = TRUE)
    writeLines("the caller's own file", file.path(kept, "mine.txt"))
    before <- tm_dir_snapshot(kept)
    rec <- tm_mock_install(
      confirm = function(prompt) TRUE, unpack = NULL,
      archive = testthat::test_path("fixtures", fixture)
    )
    cnd <- tryCatch(
      install_on_win(install_dir = kept, archive_checksum = rec$digest),
      error = function(cnd) cnd
    )
    expect_s3_class(cnd, "tidymedia_archive_unreadable")
    msg <- cli::ansi_strip(conditionMessage(cnd))
    # The directory the call did not create is never removed, and the file in
    # it is untouched -- on every platform, whatever the cleanup managed.
    expect_true(dir.exists(kept), label = fixture)
    expect_true(file.exists(file.path(kept, "mine.txt")), label = fixture)
    expect_true(grepl(kept, msg, fixed = TRUE), label = fixture)
    if (identical(tm_dir_snapshot(kept), before)) {
      expect_match(msg, "Nothing was left behind", fixed = TRUE)
    } else {
      # Something the extraction wrote is still there. It is named, and the
      # caller's own file is still one of the entries above.
      expect_match(msg, "could not", fixed = TRUE)
    }
  }
})

test_that("a failed unpack names every entry it could not remove", {
  # AC7's mocked-failure cell. The seam is made to fail on every call, so what
  # survives is the whole of what the cleanup targeted: the created directory
  # -- which still keeps its child -- and the created file inside it. Both are
  # named, which is what "more than one entry" is here for: a message that
  # reported only the first would pass a one-entry cell.
  tm_redirect_config()
  tm_redirect_data()
  d <- file.path(withr::local_tempdir(), "ffmpeg")
  rec <- tm_mock_install(
    confirm = function(prompt) TRUE, unpack = NULL,
    archive = testthat::test_path("fixtures", "corrupt-payload.7z")
  )
  testthat::local_mocked_bindings(tm_unlink = function(path, recursive = FALSE) 1L)

  cnd <- tryCatch(
    install_on_win(install_dir = d, archive_checksum = rec$digest),
    error = function(cnd) cnd
  )
  expect_s3_class(cnd, "tidymedia_archive_unreadable")
  msg <- cli::ansi_strip(conditionMessage(cnd))

  entry <- gsub("/+", "/", sub(
    "^/+", "",
    archive::archive(testthat::test_path("fixtures", "corrupt-payload.7z"))$path[[1]]
  ))
  topmost <- strsplit(entry, "/", fixed = TRUE)[[1]][[1]]
  expect_gt(length(strsplit(entry, "/", fixed = TRUE)[[1]]), 1)
  # Both leftovers, by full path -- and the directory is the one that still
  # holds the file, so the two are different kinds of target, not one target
  # named twice.
  # Each is matched with its CLOSING quote, which is what makes the two
  # assertions independent: `{.file x}` renders as `'x'`, and the file's path
  # has the directory's as a literal prefix, so a bare substring match on the
  # directory would be satisfied by the file's line alone and could never fail
  # on the directory being dropped from the message (M103 review pass 1).
  flat <- gsub("\\s+", " ", msg)
  expect_true(grepl(paste0("'", file.path(d, entry), "'"), flat, fixed = TRUE))
  expect_true(grepl(paste0("'", file.path(d, topmost), "'"), flat, fixed = TRUE))
  expect_match(msg, "could not", fixed = TRUE)
  # And they really are still there. The seam is mocked to fail on every
  # call, so this cell reads the same on every platform.
  expect_true(file.exists(file.path(d, entry)))
  expect_true(dir.exists(file.path(d, topmost)))
})


test_that("a refusal with more leftovers than cli will print still names every one", {
  # AC7's "names every entry", at a count that catches the formatter rather
  # than the cleanup. cli truncates a vector in a message: measured on 25
  # paths, entries 19 through 23 come back as an ellipsis. A refusal that
  # named 20 of 25 undeletable entries would leave the caller hunting for the
  # rest, so the count is the instrument here and the removal seam is not --
  # `tm_remove_added()` is mocked to leave a fixed 25, which is the only way
  # to reach a count no fixture produces.
  tm_redirect_config()
  tm_redirect_data()
  d <- file.path(withr::local_tempdir(), "ffmpeg")
  rec <- tm_mock_install(
    confirm = function(prompt) TRUE, unpack = NULL,
    archive = testthat::test_path("fixtures", "not-an-archive.7z")
  )
  left <- sprintf("leftover-%02d.txt", 1:25)
  testthat::local_mocked_bindings(
    tm_remove_added = function(dir, before, after) {
      # The entries are put on disk as well as reported: a message naming
      # something that is not there is the other half of AC7, and the cell
      # should not be the one place that state is incoherent.
      for (rel in left) file.create(file.path(dir, rel))
      left
    }
  )

  cnd <- tryCatch(
    install_on_win(install_dir = d, archive_checksum = rec$digest),
    error = function(cnd) cnd
  )
  expect_s3_class(cnd, "tidymedia_archive_unreadable")
  flat <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(cnd)))

  for (rel in left) {
    expect_true(
      grepl(paste0("'", file.path(d, rel), "'"), flat, fixed = TRUE),
      label = paste("message names", rel)
    )
  }
})


test_that("a refusal that removed a file of the caller's says so", {
  # M103 review pass 3. D082 removes a pre-existing file the failed
  # extraction wrote over -- what it holds afterwards is nothing the caller
  # put there -- and that removal leaves no leftover, so the refusal used to
  # take the branch saying "the directory holds what it held when this call
  # started". Measured on this machine: a 22-byte file of the caller's at the
  # path the fixture writes was gone and the message said nothing had
  # changed. The message now names it.
  #
  # The file is placed at a path the fixture's own `archive::archive()`
  # listing says it writes, which is what makes the extraction write over it
  # rather than beside it; the cell above places one where it does not.
  skip_if_not(tm_unpack_deletes_open_files())
  tm_redirect_config()
  tm_redirect_data()
  fixture <- "corrupt-payload.7z"
  entry <- tm_fixture_entry(fixture)
  expect_false(is.null(entry))

  d <- file.path(withr::local_tempdir(), "ffmpeg")
  dir.create(file.path(d, dirname(entry)), recursive = TRUE)
  mine <- file.path(d, entry)
  writeLines("the caller's own bytes", mine)

  rec <- tm_mock_install(
    confirm = function(prompt) TRUE, unpack = NULL,
    archive = testthat::test_path("fixtures", fixture)
  )
  cnd <- tryCatch(
    install_on_win(install_dir = d, archive_checksum = rec$digest),
    error = function(cnd) cnd
  )
  expect_s3_class(cnd, "tidymedia_archive_unreadable")
  flat <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(cnd)))

  # The file really is gone -- the state the message has to describe.
  expect_false(file.exists(mine))
  expect_true(grepl(paste0("'", mine, "'"), flat, fixed = TRUE))
  expect_match(flat, "written over", fixed = TRUE)
  # And it does NOT claim the directory is as the call found it.
  expect_false(grepl("Nothing was left behind", flat, fixed = TRUE))
})

test_that("a refusal that could not remove the directory it created says so", {
  # M103 review pass 3. Both refusals used `!dir.exists(install_dir)` as the
  # proxy for "this call created it and took it back", so a created directory
  # the removal could NOT delete fell through to the sentence claiming the
  # directory holds what it held -- for a directory that did not exist before
  # the call. `tm_remove_created_dirs()` now reports what it left standing
  # and the message names it.
  tm_redirect_config()
  tm_redirect_data()
  d <- file.path(withr::local_tempdir(), "made", "ffmpeg")
  rec <- tm_mock_install(
    confirm = function(prompt) TRUE, unpack = NULL,
    archive = testthat::test_path("fixtures", "not-an-archive.7z")
  )
  # The seam that removes a created directory is made to fail, which is the
  # one thing this cell mocks: the fixture writes nothing, so there is no
  # leftover and the branch turns on the created directory alone.
  testthat::local_mocked_bindings(
    tm_unlink = function(path, recursive = FALSE) 1L
  )

  cnd <- tryCatch(
    install_on_win(install_dir = d, archive_checksum = rec$digest),
    error = function(cnd) cnd
  )
  expect_s3_class(cnd, "tidymedia_archive_unreadable")
  flat <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(cnd)))

  expect_true(dir.exists(d))
  expect_true(grepl(paste0("'", d, "'"), flat, fixed = TRUE))
  expect_match(flat, "could not remove", fixed = TRUE)
  expect_false(grepl("Nothing was left behind", flat, fixed = TRUE))
  expect_false(grepl("removed it again", flat, fixed = TRUE))
})

# An archive whose every entry is a single segment, so `strip_components = 1`
# strips all of them: the extraction succeeds and writes nothing.
tm_flat_archive <- function(envir = parent.frame()) {
  src <- withr::local_tempdir(.local_envir = envir)
  writeLines("payload", file.path(src, "payload.txt"))
  flat <- file.path(withr::local_tempdir(.local_envir = envir), "flat.7z")
  withr::with_dir(src, archive::archive_write_files(flat, "payload.txt"))
  flat
}

test_that("a successful unpack that produced no files takes back the directory it created", {
  # The boundary Scope, AC4 and D082 all draw: `tidymedia_program_not_extracted`
  # is the one refusal outside the leaves-it-as-found rule, because its
  # extraction SUCCEEDED and its message tells the caller the unpacked files
  # are still there. The carve-out is written over those files, so where the
  # extraction produced none it does not reach: there is nothing in the
  # directory to point the caller at, and a call that created that directory
  # has to give it back like any other refusal (M103 review pass 2).
  tm_redirect_config()
  tm_redirect_data()

  flat <- tm_flat_archive()
  made <- file.path(withr::local_tempdir(), "made", "ffmpeg")
  rec <- tm_mock_install(confirm = function(prompt) TRUE, unpack = NULL, archive = flat)

  cnd <- tryCatch(
    install_on_win(install_dir = made, archive_checksum = rec$digest),
    error = function(cnd) cnd
  )

  expect_s3_class(cnd, "tidymedia_program_not_extracted")
  expect_false(dir.exists(made))
  expect_false(dir.exists(dirname(made)))
  msg <- cli::ansi_strip(conditionMessage(cnd))
  expect_match(msg, "produced no files at all", fixed = TRUE)
  expect_match(msg, "removed the install directory it created", fixed = TRUE)
  # And it does not tell the caller to go and look in a directory that is gone.
  expect_false(grepl("still in that directory", msg, fixed = TRUE))
})

test_that("a successful unpack that produced no files leaves a directory it found alone", {
  # The other half: the same refusal, but the install directory was already
  # there. Nothing to give back, and nothing of the caller's to touch.
  tm_redirect_config()
  tm_redirect_data()

  flat <- tm_flat_archive()
  found <- file.path(withr::local_tempdir(), "found")
  dir.create(found)
  writeLines("the caller's own file", file.path(found, "keep.txt"))
  rec <- tm_mock_install(confirm = function(prompt) TRUE, unpack = NULL, archive = flat)

  cnd <- tryCatch(
    install_on_win(install_dir = found, archive_checksum = rec$digest),
    error = function(cnd) cnd
  )

  expect_s3_class(cnd, "tidymedia_program_not_extracted")
  expect_true(dir.exists(found))
  expect_identical(readLines(file.path(found, "keep.txt")), "the caller's own file")
  msg <- cli::ansi_strip(conditionMessage(cnd))
  expect_match(msg, "produced no files at all", fixed = TRUE)
  expect_match(msg, "holds what it held when this call started", fixed = TRUE)
})

test_that("a successful unpack that produced files but no required program keeps its directory", {
  # The carve-out itself, unchanged: an extraction that DID write files leaves
  # them where they are, and the message points the caller at them.
  tm_redirect_config()
  tm_redirect_data()

  src <- withr::local_tempdir()
  dir.create(file.path(src, "top", "bin"), recursive = TRUE)
  writeLines("not a program", file.path(src, "top", "bin", "readme.txt"))
  nested <- file.path(withr::local_tempdir(), "nested.7z")
  withr::with_dir(src, archive::archive_write_dir(nested, "top"))

  made <- file.path(withr::local_tempdir(), "made", "ffmpeg")
  rec <- tm_mock_install(confirm = function(prompt) TRUE, unpack = NULL, archive = nested)

  cnd <- tryCatch(
    install_on_win(install_dir = made, archive_checksum = rec$digest),
    error = function(cnd) cnd
  )

  expect_s3_class(cnd, "tidymedia_program_not_extracted")
  # The premise: this extraction really did write something.
  # `strip_components = 1` drops the leading `bin/`, so the file lands at the
  # root of the install directory -- what matters here is only that the
  # extraction wrote one.
  expect_true("readme.txt" %in% list.files(made, recursive = TRUE))
  expect_true(dir.exists(made))
  expect_match(
    cli::ansi_strip(conditionMessage(cnd)), "still in that directory",
    fixed = TRUE
  )
})


# Every program registers, or none does (M104) --------------------------------

# The four forms a produced path takes when the archive listed it and what is
# on disk cannot be used, as `tm_mock_install(spoil = )` plants them. Named
# once so the AC4 tests and the AC2/AC3 ones cannot drift apart about which
# form is which.
tm_unusable_forms <- c("absent", "empty", "dir", "noexec")

test_that("a produced program that cannot be used stops every registration", {
  # AC1. The config root is NOT empty when the call starts: `ffmpeg` already
  # has a remembered location pointing somewhere else, so an identical
  # snapshot afterwards says the install wrote nothing rather than saying the
  # directory was empty both times. The file's CONTENTS are read back too,
  # because the snapshot records names and not bytes -- re-registering
  # `ffmpeg` at the install directory's path would leave the name list alone.
  config <- tm_redirect_config()
  tm_redirect_data()
  kept <- tm_stub_executable("already remembered")
  tm_write_location(config$new, "ffmpeg", kept)

  d <- file.path(withr::local_tempdir(), "ffmpeg")
  rec <- tm_mock_install(
    confirm = function(prompt) TRUE, real_set = TRUE,
    spoil = c(ffprobe = "empty")
  )

  before <- tm_roots_snapshot(config$root)
  cnd <- tryCatch(
    install_on_win(install_dir = d, archive_checksum = rec$digest),
    error = function(cnd) cnd
  )
  expect_s3_class(cnd, "tidymedia_program_unusable")
  expect_identical(tm_roots_snapshot(config$root), before)
  expect_identical(readLines(tm_config_file("ffmpeg", config$new)), kept)
  # The premise the criterion rests on: `ffmpeg` passed the check, so the
  # unchanged config root is the check refusing to write rather than nothing
  # having been registrable in the first place.
  expect_true(file.size(tm_install_binary(d, "ffmpeg")) > 0)
})

test_that("the refusal names every failed program and leaves the install directory alone", {
  # AC2. Both required programs fail in one call, so the plural is exercised
  # by two entries rather than asserted of one.
  config <- tm_redirect_config()
  tm_redirect_data()
  withr::local_options(cli.width = 1000)
  d <- file.path(withr::local_tempdir(), "ffmpeg")
  rec <- tm_mock_install(
    confirm = function(prompt) TRUE, real_set = TRUE,
    spoil = c(ffmpeg = "empty", ffprobe = "empty")
  )

  before <- tm_roots_snapshot(config$root)
  cnd <- tryCatch(
    install_on_win(install_dir = d, archive_checksum = rec$digest),
    error = function(cnd) cnd
  )
  expect_s3_class(cnd, "tidymedia_program_unusable")
  expect_identical(blamed_verb(cnd), "install_on_win")

  msg <- cli::ansi_strip(conditionMessage(cnd))
  for (program in c("ffmpeg", "ffprobe")) {
    expect_match(msg, program, fixed = TRUE)
    expect_match(msg, tm_install_binary(d, program), fixed = TRUE)
  }
  # Not set_program()'s own unclassed abort, which is the failure this check
  # exists to reach first (M104 AC4).
  expect_no_match(msg, "Can't find an executable", fixed = TRUE)

  expect_identical(tm_roots_snapshot(config$root), before)
  # D082's boundary: this sits BELOW a successful extraction, so the unpacked
  # files stay exactly as the extraction left them.
  expect_identical(tm_dir_snapshot(d), rec$after_extract)
})

test_that("a produced ffplay that cannot be used leaves the install successful", {
  # AC3. The wording has to be the produced-but-unusable one, not the
  # archive-did-not-produce one, which would be false here: the archive DID
  # produce a path for `ffplay`.
  config <- tm_redirect_config()
  tm_redirect_data()
  withr::local_options(cli.width = 1000)
  d <- file.path(withr::local_tempdir(), "ffmpeg")
  rec <- tm_mock_install(
    confirm = function(prompt) TRUE, real_set = TRUE,
    spoil = c(ffplay = "empty")
  )

  msg <- NULL
  expect_true(withCallingHandlers(
    install_on_win(install_dir = d, archive_checksum = rec$digest),
    message = function(m) {
      msg <<- c(msg, cli::ansi_strip(conditionMessage(m)))
      invokeRestart("muffleMessage")
    }
  ))
  msg <- paste(msg, collapse = "")
  expect_match(msg, "ffplay", fixed = TRUE)
  expect_match(msg, tm_install_binary(d, "ffplay"), fixed = TRUE)
  expect_match(msg, "could not be used", fixed = TRUE)
  expect_no_match(msg, "did not produce", fixed = TRUE)

  expect_true(file.exists(tm_config_file("ffmpeg", config$new)))
  expect_true(file.exists(tm_config_file("ffprobe", config$new)))
  expect_false(file.exists(tm_config_file("ffplay", config$new)))
})

# AC4: each of the four planted forms is disposed the way AC2 states, at a
# required program. One test per form, so a form that stops being refused
# names itself rather than hiding inside a loop's first failure.
tm_expect_required_refusal <- function(form) {
  config <- tm_redirect_config()
  tm_redirect_data()
  withr::local_options(cli.width = 1000)
  d <- file.path(withr::local_tempdir(), "ffmpeg")
  spoil <- stats::setNames(form, "ffprobe")
  rec <- tm_mock_install(
    confirm = function(prompt) TRUE, real_set = TRUE, spoil = spoil
  )

  before <- tm_roots_snapshot(config$root)
  cnd <- tryCatch(
    install_on_win(install_dir = d, archive_checksum = rec$digest),
    error = function(cnd) cnd
  )
  expect_s3_class(cnd, "tidymedia_program_unusable")
  expect_identical(blamed_verb(cnd), "install_on_win")
  msg <- cli::ansi_strip(conditionMessage(cnd))
  expect_match(msg, "ffprobe", fixed = TRUE)
  expect_match(msg, tm_install_binary(d, "ffprobe"), fixed = TRUE)
  expect_no_match(msg, "Can't find an executable", fixed = TRUE)
  expect_identical(tm_roots_snapshot(config$root), before)
  expect_identical(tm_dir_snapshot(d), rec$after_extract)
  invisible(cnd)
}

test_that("a path the extraction listed and did not create is refused", {
  # AC4, form 1. The premise: nothing is at that path at all.
  cnd <- tm_expect_required_refusal("absent")
  expect_s3_class(cnd, "tidymedia_program_unusable")
})

test_that("a produced path created as an empty file is refused", {
  # AC4, form 2 -- the one the executable bit cannot see, which is why the
  # check asks for bytes as well as for a resolvable path.
  tm_expect_required_refusal("empty")
})

test_that("a produced path created as a directory is refused", {
  # AC4, form 3.
  tm_expect_required_refusal("dir")
})

test_that("a produced path with no executable bit is refused", {
  # AC4, form 4. POSIX only: Windows has no executable bit, so there is no
  # such state to plant there.
  skip_on_os("windows")
  tm_expect_required_refusal("noexec")
})

test_that("a directory planted at ffplay's path is informed about, not refused", {
  # AC4's second half: one of the four forms is also disposed at an optional
  # program the way AC3 states -- the install completes and says what it
  # skipped.
  config <- tm_redirect_config()
  tm_redirect_data()
  withr::local_options(cli.width = 1000)
  d <- file.path(withr::local_tempdir(), "ffmpeg")
  rec <- tm_mock_install(
    confirm = function(prompt) TRUE, real_set = TRUE, spoil = c(ffplay = "dir")
  )

  msg <- NULL
  expect_true(withCallingHandlers(
    install_on_win(install_dir = d, archive_checksum = rec$digest),
    message = function(m) {
      msg <<- c(msg, cli::ansi_strip(conditionMessage(m)))
      invokeRestart("muffleMessage")
    }
  ))
  msg <- paste(msg, collapse = "")
  expect_match(msg, "could not be used", fixed = TRUE)
  expect_no_match(msg, "did not produce", fixed = TRUE)
  expect_no_match(msg, "Can't find an executable", fixed = TRUE)
  expect_false(file.exists(tm_config_file("ffplay", config$new)))
  expect_true(file.exists(tm_config_file("ffmpeg", config$new)))
})

test_that("an absent optional program is still reported in one message", {
  # The archive omits `ffplay` entirely and nothing else fails. This is the
  # pre-M104 optional-program state, asserted here so the new branch beside it
  # cannot turn one message into two.
  #
  # It does NOT exercise both optional states at once, and nothing can:
  # `tm_install_registers` minus `tm_install_required` is exactly `ffplay`
  # (`R/program_management.R:306`, `:312`), so `absent_optional` and
  # `unusable_optional` can never both be non-empty in one call. The combining
  # branch in `install_on_win()` is written for a fourth registered program
  # that does not exist yet, and is unreachable until one does (M104 review
  # F4).
  tm_redirect_config()
  tm_redirect_data()
  withr::local_options(cli.width = 1000)
  d <- file.path(withr::local_tempdir(), "ffmpeg")
  rec <- tm_mock_install(
    confirm = function(prompt) TRUE, real_set = TRUE,
    unpack = c("ffmpeg", "ffprobe")
  )
  count <- 0L
  expect_true(withCallingHandlers(
    install_on_win(install_dir = d, archive_checksum = rec$digest),
    message = function(m) {
      count <<- count + 1L
      invokeRestart("muffleMessage")
    }
  ))
  expect_identical(count, 1L)
})

test_that("an install directory written with a tilde is not refused as unusable", {
  # M104 review F1. `file.info()` expands `~` and `Sys.which()` does not, so a
  # path built with a tilde was a non-empty file to one clause of the
  # registration check and absent to another: the check refused a build it had
  # just unpacked correctly, and blamed the archive for it. The fix expands
  # where the path is BUILT, so the check and the `set_program()` call after it
  # ask about one file -- expanding inside the check alone would move the same
  # failure into the loop, which is the partial registration M104 exists to
  # stop, so this test asserts the whole install succeeded rather than only
  # that the check passed.
  skip_on_os("windows")
  home <- withr::local_tempdir()
  withr::local_envvar(HOME = home)
  # The instrument, asserted rather than assumed: with no tilde redirection
  # this test would run against the real home directory and prove nothing.
  expect_identical(normalizePath(path.expand("~")), normalizePath(home))

  config <- tm_redirect_config()
  tm_redirect_data()
  rec <- tm_mock_install(confirm = function(prompt) TRUE, real_set = TRUE)

  expect_true(suppressMessages(
    install_on_win(install_dir = "~/ffmpeg", archive_checksum = rec$digest)
  ))
  for (program in c("ffmpeg", "ffprobe", "ffplay")) {
    file <- tm_config_file(program, config$new)
    expect_true(file.exists(file))
    # And what was remembered is the expanded path, so no later caller has to
    # expand it again to find the program.
    expect_false(grepl("~", readLines(file), fixed = TRUE))
  }
})

