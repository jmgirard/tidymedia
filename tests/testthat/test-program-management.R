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
tm_mock_install <- function(confirm = NULL,
                            unpack = tm_install_registers,
                            sidecar = NULL,
                            archive = NULL,
                            download = NULL,
                            real_set = FALSE,
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
        add("extract", list(
          dir = dir,
          from_download = identical(archive, rec$destfile)
        ))
        # What the extraction PRODUCES is what install_on_win() registers, so
        # the mock has to leave files behind: `unpack` is how a test says
        # which programs this build contained.
        bin <- file.path(dir, "bin")
        dir.create(bin, recursive = TRUE, showWarnings = FALSE)
        for (program in unpack) {
          exe <- file.path(bin, paste0(program, ".exe"))
          file.create(exe)
          # `real_set = TRUE` sends the run through the REAL set_program(),
          # which asks Sys.which() whether the path is executable -- so the
          # stub has to be, or AC4's on-disk claim could not be reached.
          if (real_set) Sys.chmod(exe, "0755")
        }
        invisible(NULL)
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

  before <- tm_dir_snapshot(data_root, config$root)
  expect_error(install_on_win(), class = "tidymedia_confirmation_unavailable")
  # The escape hatch is named by install_on_win(), not by the seam, so the
  # message says the argument this caller actually has.
  expect_error(install_on_win(), "confirm = FALSE")
  # The refusal names what the call would have done, so a caller told to pass
  # `confirm = FALSE` has seen what that authorizes.
  withr::local_options(cli.width = 1000)
  u <- "https://example.invalid/build.7z"
  d <- file.path(withr::local_tempdir(), "an install {dir}")
  msg <- tryCatch(
    install_on_win(download_url = u, install_dir = d),
    tidymedia_confirmation_unavailable = function(cnd) cli::ansi_strip(conditionMessage(cnd))
  )
  for (needle in c(u, d, vapply(tm_install_registers, tm_config_file, character(1), USE.NAMES = FALSE))) {
    expect_true(grepl(needle, msg, fixed = TRUE), label = paste("refusal names", needle))
  }
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
  d <- file.path(withr::local_tempdir(), "ffmpeg")

  for (shape in names(tm_sidecar_bodies("x"))) {
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

  # A caller-named source has no sidecar to fetch either.
  named <- tm_mock_install(confirm = function(prompt) TRUE)
  expect_message(
    expect_true(install_on_win(
      download_url = "https://example.invalid/build.7z", install_dir = d
    )),
    "will not be verified"
  )
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
  before <- tm_dir_snapshot(config$root)
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
  expect_identical(tm_dir_snapshot(config$root), before)
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
    cnd <- tryCatch(
      install_on_win(install_dir = d, archive_checksum = rec$digest),
      error = function(cnd) cnd
    )
    expect_s3_class(cnd, "tidymedia_archive_unreadable")

    # The abort names its own two facts: the archive, and where it was going.
    msg <- cli::ansi_strip(conditionMessage(cnd))
    expect_true(grepl(rec$destfile, msg, fixed = TRUE), label = fixture)
    expect_true(grepl(d, msg, fixed = TRUE), label = fixture)

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

  # The control: the same path, succeeding, also leaves no download behind.
  d <- file.path(withr::local_tempdir(), "ffmpeg")
  ok <- tm_mock_install(confirm = function(prompt) TRUE)
  expect_true(install_on_win(install_dir = d, archive_checksum = ok$digest))
  expect_false(file.exists(ok$destfile))
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
    before <- tm_dir_snapshot(config$root)
    expect_error(
      install_on_win(install_dir = d, archive_checksum = rec$digest),
      class = "tidymedia_program_not_extracted"
    )
    expect_identical(tm_dir_snapshot(config$root), before, label = absent)
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

# The property AC6 asserts of each collected exit: a `return()` hands back a
# literal TRUE or FALSE, and a `cli_abort()` passes a class beginning
# `tidymedia_`.
tm_exit_ok <- function(node) {
  if (identical(tm_call_name(node), "return")) {
    value <- node[[2]]
    return(is.logical(value) && length(value) == 1L && !is.na(value))
  }
  class <- node[["class"]]
  is.character(class) && length(class) == 1L && startsWith(class, "tidymedia_")
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
  classes <- vapply(
    exits,
    function(node) {
      class <- node[["class"]]
      if (is.character(class)) class else NA_character_
    },
    character(1)
  )
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

  ok <- function(fn) all(vapply(tm_collect_exits(fn), tm_exit_ok, logical(1)))
  expect_false(ok(unclassed))
  expect_false(ok(non_literal))
  expect_true(ok(compliant))

  # And the collector reads a non-empty domain in each case, so "no exits
  # found" can never be what makes a function look compliant.
  expect_identical(length(tm_collect_exits(unclassed)), 1L)
  expect_identical(length(tm_collect_exits(non_literal)), 1L)
  expect_identical(length(tm_collect_exits(compliant)), 2L)
})
