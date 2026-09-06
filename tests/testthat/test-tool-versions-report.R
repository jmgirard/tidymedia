# What tool_versions() says when a version probe is killed, and what it refuses
# when its two arguments do not line up (M116). The warning is raised from two
# callers -- ffm_batch(manifest = TRUE) and program_status() -- so its wording
# belongs to neither.


# The timed-out probe's wording ------------------------------------------------

# Every version probe killed by the limit, with the four programs resolvable so
# the probes are actually run. `run_program` is the seam: capture_version()
# turns anything else into a silent NA, and abort_timeout() is what the limit
# raises.
tm_local_all_probes_timeout <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    run_program = function(location, args, program = "the program", ...) {
      abort_timeout(program, 2)
    },
    .package = "tidymedia",
    .env = env
  )
}

test_that("the timeout warning names each program the way the report's column does", {
  # AC4. program_status() prints "ffmpeg" in its `program` column; a warning
  # explaining the NA beside it said "FFmpeg", so a reader had to match two
  # spellings of one program to connect the two.
  dirs <- tm_redirect_config()
  stub <- tm_stub_executable()
  for (program in tm_program_vocabulary) tm_write_location(dirs$new, program, stub)
  tm_local_all_probes_timeout()

  got <- tm_collect_warnings(program_status())

  timeouts <- Filter(
    function(w) inherits(w, "tidymedia_probe_timeout"),
    got$warnings
  )
  expect_length(timeouts, 1L)
  message <- cli::ansi_strip(conditionMessage(timeouts[[1]]))
  for (program in tm_program_vocabulary) {
    expect_match(message, program, fixed = TRUE, info = program)
  }
  # The display labels, which are what the warning used to print.
  for (label in c("FFmpeg", "FFprobe", "FFplay", "MediaInfo")) {
    expect_no_match(message, label, fixed = TRUE, info = label)
  }
  expect_identical(got$value$version, rep(NA_character_, 4L))
})

test_that("the timeout warning reads the same from the manifest caller", {
  # The other caller, pinned so the wording cannot be tuned for one of them.
  # Both messages are compared whole rather than by keyword: what AC4 asks is
  # that they are the same sentence, not that each contains some phrase.
  testthat::local_mocked_bindings(
    find_ffmpeg = function(...) "/usr/bin/ffmpeg",
    find_ffprobe = function(...) "/usr/bin/ffprobe",
    .package = "tidymedia"
  )
  tm_local_all_probes_timeout()

  from_manifest <- tm_collect_warnings(tool_versions())
  from_status <- tm_collect_warnings(tool_versions(c("ffmpeg", "ffprobe"),
                                                   list("/usr/bin/ffmpeg",
                                                        "/usr/bin/ffprobe")))

  expect_length(from_manifest$warnings, 1L)
  expect_identical(
    cli::ansi_strip(conditionMessage(from_manifest$warnings[[1]])),
    cli::ansi_strip(conditionMessage(from_status$warnings[[1]]))
  )
  expect_identical(from_manifest$value,
                   list(ffmpeg = NA_character_, ffprobe = NA_character_))
})

test_that("the timeout warning says what NA means without naming a caller", {
  # The sentence itself. It has to explain the NA to a reader of a returned
  # tibble as well as to a reader of a manifest, so it names neither.
  testthat::local_mocked_bindings(
    find_ffmpeg = function(...) "/usr/bin/ffmpeg",
    find_ffprobe = function(...) "/usr/bin/ffprobe",
    .package = "tidymedia"
  )
  tm_local_all_probes_timeout()

  got <- tm_collect_warnings(tool_versions())
  message <- cli::ansi_strip(conditionMessage(got$warnings[[1]]))

  expect_match(message, "The version probe timed out after 2 seconds", fixed = TRUE)
  expect_match(message, "Those versions are recorded as NA", fixed = TRUE)
  expect_match(message, "options(tidymedia.timeout = )", fixed = TRUE)
  expect_no_match(message, "manifest", fixed = TRUE)
})

test_that("the timeout wording guard reddens on the sentence it retired", {
  # Mutation probe, the shape test-timeout-silence.R uses for the doc guards:
  # every assertion above is a substring test, and a substring test is how a
  # caller-specific sentence shipped green in the first place. The retired
  # wording is restored into a stand-in message and the same assertions are
  # checked to fail on it.
  mutant <- paste(
    "The version probe timed out after 2 seconds.",
    "FFmpeg and FFprobe.",
    "The manifest records NA for those versions; raise or remove",
    "options(tidymedia.timeout = )."
  )
  expect_match(mutant, "manifest", fixed = TRUE)
  expect_match(mutant, "FFmpeg", fixed = TRUE)
  expect_no_match(mutant, "Those versions are recorded as NA", fixed = TRUE)
  expect_no_match(mutant, "ffmpeg", fixed = TRUE)
})

test_that("one killed probe names only itself, in the column's spelling", {
  testthat::local_mocked_bindings(
    find_ffmpeg = function(...) "/usr/bin/ffmpeg",
    find_ffprobe = function(...) "/usr/bin/ffprobe",
    run_program = function(location, args, program = "the program", ...) {
      if (program == "FFprobe") abort_timeout(program, 2)
      "ffmpeg version 8.1.2 Copyright (c)"
    },
    .package = "tidymedia"
  )

  got <- tm_collect_warnings(tool_versions())
  message <- cli::ansi_strip(conditionMessage(got$warnings[[1]]))

  expect_match(message, "ffprobe", fixed = TRUE)
  expect_no_match(message, "ffmpeg", fixed = TRUE)
  expect_identical(got$value, list(ffmpeg = "8.1.2", ffprobe = NA_character_))
})


# locations must line up with programs -----------------------------------------

test_that("tool_versions() refuses a locations list of the wrong length", {
  # AC6. Map() pairs the two positionally: a length-1 list recycles silently
  # onto every program, and any other mismatch warns from base R rather than
  # refusing -- either way the probes come back labelled with program names
  # that are not theirs.
  testthat::local_mocked_bindings(
    run_program = function(location, args, program = "the program", ...) {
      paste(tolower(program), "version 8.1.2 Copyright (c)")
    },
    .package = "tidymedia"
  )
  programs <- tm_program_vocabulary

  cases <- list(
    recycled = list("/usr/bin/ffmpeg"),
    short = list("/a", "/b", "/c"),
    empty = list()
  )
  for (case in names(cases)) {
    condition <- tryCatch(
      tool_versions(programs, cases[[case]]),
      condition = function(c) c
    )
    expect_s3_class(condition, "tidymedia_locations_mismatch")
    expect_identical(condition$tm_n_programs, 4L, info = case)
    expect_identical(condition$tm_n_locations, length(cases[[case]]), info = case)
  }
})

test_that("the length refusal is reported from the frame the caller named", {
  # `call` is already threaded for the timeout refusal above it; the new abort
  # reports from the same frame rather than from tool_versions(), which no
  # caller types. A named wrapper stands in for the real caller, so the
  # condition's call is something a reader could have typed.
  tm_caller_stub <- function() {
    tool_versions(c("ffmpeg", "ffprobe"), list("/a"),
                  call = rlang::current_env())
  }

  condition <- tryCatch(tm_caller_stub(), error = function(e) e)

  expect_s3_class(condition, "tidymedia_locations_mismatch")
  expect_identical(blamed_verb(condition), "tm_caller_stub")
})

test_that("the default locations path is untouched by the length check", {
  # AC6's other half: the check is on the non-NULL arm only, so the call
  # ffm_batch() makes -- which passes no locations at all -- still resolves its
  # own and answers as before.
  testthat::local_mocked_bindings(
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

test_that("ffm_batch(manifest = TRUE) still records both tool versions", {
  # The end-to-end caller, so "the default path is untouched" does not rest on
  # a unit test of the default path alone.
  skip_if_no_ffmpeg()
  a <- make_test_video()
  out <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(input = a, output = out)

  res <- ffm_batch(jobs, function(input, output, ...) {
    ffm_files(input, output) |> ffm_scale(32, 32) |> ffm_codec(video = "libx264")
  }, manifest = TRUE)
  man <- ffm_manifest(res)

  expect_true(all(c("ffmpeg_version", "ffprobe_version") %in% names(man)))
  expect_false(anyNA(man$ffmpeg_version))
})
