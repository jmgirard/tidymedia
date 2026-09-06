# program_status() reports all four programs in one call, and unset_program()
# removes a location set_program() remembered (M113). The fixtures they share
# with the rest of the family -- tm_redirect_config(), tm_stub_executable(),
# tm_program_vocabulary -- live in helper-program-config.R.


# program_status() ------------------------------------------------------------

test_that("program_status() reports one row per program, in the published vocabulary", {
  # AC1's shape half, and the only place the report's own program list is tied
  # to the one the set_*/unset_* guards enforce. A fifth program added to
  # set_program() alone would otherwise leave the report silently three-quarters
  # of an answer.
  tm_redirect_config()
  status <- program_status()

  expect_s3_class(status, "tbl_df")
  expect_identical(names(status), c("program", "location", "version"))
  expect_identical(status$program, tm_program_vocabulary)
  expect_identical(tm_programs(), tm_program_vocabulary)
  expect_type(status$location, "character")
  expect_type(status$version, "character")
})

test_that("program_status() finds every program and its version when all four are installed", {
  # AC1, the all-present state. Not gated on the config directories: the point
  # is what the machine's real PATH answers. Every version is asserted non-NA,
  # mediainfo included -- it answers `--version` in a shape with no "version"
  # token in it, so an NA there means the second parse arm stopped working.
  skip_if_no_ffmpeg()
  skip_if_no_ffprobe()
  skip_if_no_mediainfo()
  skip_if_not(nzchar(Sys.which("ffplay")), "ffplay binary not available")

  status <- expect_no_warning(program_status())

  expect_false(anyNA(status$location))
  for (i in seq_len(nrow(status))) {
    expect_identical(
      normalizePath(status$location[[i]]),
      normalizePath(unname(Sys.which(status$program[[i]]))),
      info = status$program[[i]]
    )
  }
  expect_false(anyNA(status$version))
})

test_that("program_status() returns NA for every program when none can be resolved", {
  # AC1, the none-resolvable state. An emptied PATH alone does not produce it:
  # find_program() then reads the current config dir and the legacy one, so
  # both must be redirected somewhere empty as well. tm_redirect_config() does
  # all three, and the assertion below on Sys.which() is what says the PATH
  # half took.
  dirs <- tm_redirect_config()
  expect_identical(Sys.which("ffmpeg"), c(ffmpeg = ""))
  expect_length(list.files(dirs$new), 0L)
  expect_length(list.files(dirs$legacy), 0L)

  status <- expect_no_warning(program_status())

  expect_identical(status$program, tm_program_vocabulary)
  expect_identical(status$location, rep(NA_character_, 4L))
  expect_identical(status$version, rep(NA_character_, 4L))
})

test_that("program_status() reports the other three when one program is hidden", {
  # AC2, one run per program: the report never aborts and never warns over a
  # program it cannot resolve. The three that ARE resolvable are remembered
  # locations pointing at a stub, so the run needs no binary and the hidden
  # program is hidden by the absence of a file rather than by a mock.
  for (hidden in tm_program_vocabulary) {
    dirs <- tm_redirect_config()
    stub <- tm_stub_executable()
    present <- setdiff(tm_program_vocabulary, hidden)
    for (program in present) tm_write_location(dirs$new, program, stub)

    status <- expect_no_warning(program_status())

    expect_identical(status$program, tm_program_vocabulary, info = hidden)
    expect_true(is.na(status$location[status$program == hidden]), info = hidden)
    expect_identical(
      status$location[status$program %in% present],
      rep(stub, length(present)),
      info = hidden
    )
    # The stub answers no version flag, so every version here is NA -- what
    # this case asserts is that an unreadable version is a value in a column
    # and not a condition.
    expect_identical(status$version, rep(NA_character_, 4L), info = hidden)
  }
})

test_that("a program that resolves but cannot be asked has a location and no version", {
  # AC1's "or NA" half on the version column alone, separated from the missing
  # -program case so the two are not read as the same fact. The location is
  # real; the probe fails.
  dirs <- tm_redirect_config()
  stub <- tm_stub_executable()
  for (program in tm_program_vocabulary) tm_write_location(dirs$new, program, stub)
  local_mocked_bindings(
    run_program = function(...) stop("no version flag here"),
    .package = "tidymedia"
  )

  status <- expect_no_warning(program_status())

  expect_identical(status$location, rep(stub, 4L))
  expect_identical(status$version, rep(NA_character_, 4L))
})

test_that("MediaInfo's version shape is parsed, and FFmpeg's is parsed as before", {
  # The parse arm the widening added, tested without a binary. MediaInfo's
  # `--version` names no "version" token; the FFmpeg line still wins on the
  # first arm, and a line naming neither is still NA.
  expect_identical(
    parse_version_line(c("MediaInfo Command line, ", "MediaInfoLib - v26.05")),
    "26.05"
  )
  expect_identical(
    parse_version_line("ffmpeg version 9.0.1 Copyright (c) 2003-2026"),
    "9.0.1"
  )
  expect_identical(parse_version_line("nothing to see"), NA_character_)
  expect_identical(parse_version_line(character(0)), NA_character_)
})

test_that("the version probe asks each binary the flag that binary answers", {
  # MediaInfo does not answer `-version`, so a probe that sent the FFmpeg flag
  # to all four would report NA for it on a machine where it is installed --
  # which is indistinguishable from the binary being absent. Recorded here
  # rather than inferred from a parsed string.
  asked <- list()
  local_mocked_bindings(
    run_program = function(location, args, program = "the program", ...) {
      asked[[program]] <<- args
      "x version 1.0"
    },
    .package = "tidymedia"
  )
  capture_version("/bin/stub", "FFmpeg")
  capture_version("/bin/stub", "MediaInfo", flag = "--version")

  expect_identical(asked$FFmpeg, "-version")
  expect_identical(asked$MediaInfo, "--version")
})

test_that("the manifest's version probe is unchanged by the widening", {
  # tool_versions() gained a `programs` argument; ffm_batch() still calls it
  # with none, and the manifest still reads exactly two names out of it.
  local_mocked_bindings(
    find_ffmpeg = function(...) "/usr/bin/ffmpeg",
    find_ffprobe = function(...) "/usr/bin/ffprobe",
    run_program = function(location, args, program = "the program", ...) {
      paste(tolower(program), "version 8.1.2 Copyright (c)")
    },
    .package = "tidymedia"
  )
  expect_identical(
    tool_versions(),
    list(ffmpeg = "8.1.2", ffprobe = "8.1.2")
  )
})


# unset_program() -------------------------------------------------------------

# AC3's three states, walked in order at one config location: never remembered,
# remembered, forgotten. `dir` is the directory the location is written to;
# `write` puts it there. The current directory is written through
# set_program(), which is the call whose effect unset_program() undoes; the
# legacy directory has no exported writer, so its file is written directly --
# which is also how it got there before 0.2.0.
tm_expect_forget_cycle <- function(dirs, dir, write, program) {
  files <- c(
    tm_config_file(program, dirs$new),
    tm_config_file(program, dirs$legacy)
  )
  stub <- tm_stub_executable()

  # State 1: never remembered. Under the emptied PATH this is find_program()'s
  # not-found answer, which is the answer the third state must return to.
  expect_false(any(file.exists(files)))
  before <- "not yet read"
  expect_warning(before <- find_program(program), "Failed to find")
  expect_null(before)

  # State 2: remembered.
  write(dir, program, stub)
  expect_true(file.exists(tm_config_file(program, dir)))
  expect_no_warning(expect_identical(find_program(program), stub))

  # State 3: forgotten. Neither file exists afterwards -- not merely the one
  # that was written -- and find_program() answers as it did in state 1,
  # warning and all.
  expect_true(unset_program(program))
  expect_false(any(file.exists(files)))
  after <- "not yet read"
  expect_warning(after <- find_program(program), "Failed to find")
  expect_null(after)
  expect_identical(after, before)
}

test_that("unset_program() forgets a location remembered in the current config dir", {
  # AC3, three states at the tools::R_user_dir() location, one walk per program.
  for (program in tm_program_vocabulary) {
    dirs <- tm_redirect_config()
    tm_expect_forget_cycle(
      dirs, dirs$new,
      function(dir, program, location) set_program(program, location, confirm = FALSE),
      program
    )
  }
})

test_that("unset_program() forgets a location remembered before 0.2.0", {
  # AC3, the same three states at the rappdirs location find_program() still
  # falls back to. Deleting only the current file would leave this one, and
  # find_program() would go straight on answering with it.
  for (program in tm_program_vocabulary) {
    dirs <- tm_redirect_config()
    tm_expect_forget_cycle(dirs, dirs$legacy, tm_write_location, program)
  }
})

test_that("unset_program() removes both files when a location is remembered at each", {
  # The state neither walk above reaches: current and legacy both hold a
  # location. One call clears both, so the fallback cannot resurrect the old
  # one.
  dirs <- tm_redirect_config()
  new_stub <- tm_stub_executable("new")
  old_stub <- tm_stub_executable("old")

  for (program in tm_program_vocabulary) {
    tm_write_location(dirs$new, program, new_stub)
    tm_write_location(dirs$legacy, program, old_stub)
    expect_identical(find_program(program), new_stub)

    expect_true(unset_program(program))

    expect_false(file.exists(tm_config_file(program, dirs$new)))
    expect_false(file.exists(tm_config_file(program, dirs$legacy)))
    expect_warning(expect_null(find_program(program)), "Failed to find")
  }
})

test_that("unset_program() returns TRUE invisibly and leaves the other programs alone", {
  # A forget is one program's, not the family's.
  dirs <- tm_redirect_config()
  stub <- tm_stub_executable()
  for (program in tm_program_vocabulary) tm_write_location(dirs$new, program, stub)

  expect_invisible(unset_program("ffmpeg"))

  expect_false(file.exists(tm_config_file("ffmpeg", dirs$new)))
  for (program in setdiff(tm_program_vocabulary, "ffmpeg")) {
    expect_identical(find_program(program), stub, info = program)
  }
})

test_that("unset_program() warns with a classed condition when nothing was remembered", {
  # AC4: the class vector and the message, for each of the four programs and at
  # a point where NEITHER config file exists -- the only state in which the
  # refusal can fire.
  dirs <- tm_redirect_config()

  for (program in tm_program_vocabulary) {
    expect_false(file.exists(tm_config_file(program, dirs$new)))
    expect_false(file.exists(tm_config_file(program, dirs$legacy)))

    condition <- tryCatch(unset_program(program), warning = function(w) w)
    expect_s3_class(condition, "tidymedia_no_remembered_location")
    expect_identical(
      class(condition),
      c("tidymedia_no_remembered_location", "rlang_warning", "warning",
        "condition")
    )
    expect_identical(condition$tm_program, program)
    expect_match(
      cli::ansi_strip(conditionMessage(condition)),
      paste0("No remembered location to forget for ", program),
      fixed = TRUE
    )
    # The refusal is a warning, not an abort: the call returns, and it returns
    # FALSE.
    expect_false(suppressWarnings(unset_program(program)))
  }
})

test_that("the nothing-remembered warning blames the function the caller typed", {
  # `call` is threaded through tm_unset_program() for this, the way M112 threads
  # it through tm_set_program(): the condition names unset_program(), not the
  # helper underneath it.
  tm_redirect_config()
  condition <- tryCatch(unset_program("ffmpeg"), warning = function(w) w)
  expect_identical(rlang::call_name(conditionCall(condition)), "unset_program")
})

test_that("unset_program() refuses a program outside the published vocabulary", {
  tm_redirect_config()
  expect_error(unset_program("vlc"), class = "rlang_error")
  expect_identical(
    eval(formals(unset_program)$program),
    tm_program_vocabulary
  )
})

test_that("unset_program() aborts, naming the file, when a removal does not take", {
  # The failure branch, fired through the tm_unlink() seam rather than by
  # making the filesystem fail: a seam that removes nothing leaves the file
  # exactly where a permission failure would. The abort is what says the call
  # never claims a forget it did not perform.
  dirs <- tm_redirect_config()
  stub <- tm_stub_executable()
  tm_write_location(dirs$new, "ffmpeg", stub)
  local_mocked_bindings(
    tm_unlink = function(path, recursive = FALSE) 1L,
    .package = "tidymedia"
  )

  condition <- tryCatch(unset_program("ffmpeg"), error = function(e) e)
  expect_s3_class(condition, "tidymedia_location_not_removed")
  expect_identical(condition$tm_program, "ffmpeg")
  expect_identical(condition$tm_files, tm_config_file("ffmpeg", dirs$new))
  expect_true(file.exists(tm_config_file("ffmpeg", dirs$new)))
})

test_that("unset_program() drops the memoized FFmpeg capabilities", {
  # A forgotten location can change which binary find_program() resolves to, so
  # what was memoized about the old one goes with it -- the reason set_program()
  # drops the memo when it points at a different binary (M67/D044).
  dirs <- tm_redirect_config()
  stub <- tm_stub_executable()
  tm_write_location(dirs$new, "ffmpeg", stub)
  dropped <- 0L
  local_mocked_bindings(
    forget_ffmpeg_capabilities = function() dropped <<- dropped + 1L,
    .package = "tidymedia"
  )

  expect_true(unset_program("ffmpeg"))
  expect_identical(dropped, 1L)
})
