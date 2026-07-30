# M44: the dropped-audio-track warning, and D024's licence for the FFprobe probe
# behind it. The probe runs only on a `run = TRUE` call whose caller named no
# `audio_stream`, and its outcome may change nothing but whether this warning is
# signalled -- so the tests come in three groups: what the message says, that
# nothing else observable moves, and that every failure path is silent.


# What the message says (AC1) ----------------------------------------------

test_that("probe_audio()'s index and audio_stream disagree on the same file", {
  # The oracle behind AC1's wording: the message must carry BOTH readings,
  # because a reader who takes probe_audio()'s index for an audio_stream value
  # lands one track off on any file with video before its audio.
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  expect_equal(as.integer(probe_audio(infile = infile)$index), 1:3)
})

test_that("extract_audio() warns once, naming the count, the argument and the offset", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  out <- withr::local_tempfile(fileext = ".mka")
  w <- tryCatch(extract_audio(infile, out), warning = function(w) w)
  expect_s3_class(w, "tidymedia_dropped_audio")
  msg <- cli::ansi_strip(conditionMessage(w))
  expect_match(msg, "3 audio tracks")
  expect_match(msg, "drops 2")
  expect_match(msg, "audio_stream")
  expect_match(msg, "probe_audio")
  expect_match(msg, "1, 2, 3", fixed = TRUE)
  expect_match(msg, "0, 1, 2", fixed = TRUE)
})

test_that("convert_audio() warns on the same input", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  out <- withr::local_tempfile(fileext = ".mp3")
  expect_warning(convert_audio(infile, out), class = "tidymedia_dropped_audio")
})

test_that("naming a track silences the warning", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  expect_no_warning(
    extract_audio(infile, withr::local_tempfile(fileext = ".mka"), audio_stream = 1)
  )
  expect_no_warning(
    convert_audio(infile, withr::local_tempfile(fileext = ".mp3"), audio_stream = 2)
  )
})

test_that("a single-track input warns about nothing", {
  skip_if_no_ffprobe()
  infile <- make_test_video()
  expect_no_warning(
    extract_audio(infile, withr::local_tempfile(fileext = ".m4a"))
  )
})


# Nothing else observable moves (AC2, AC9) ---------------------------------

test_that("run = FALSE invokes no binary on any of the four verbs", {
  # The strong form of AC2, and machine-independent: any shell-out at all fails
  # the test, rather than relying on a bare PATH mask that a user config file
  # could still resolve around (find_program() falls back to one).
  infile <- make_input("mkv")
  jobs <- tibble::tibble(input = infile, output = "a.mka")
  local_mocked_bindings(
    run_program = function(...) stop("a binary was invoked under run = FALSE")
  )
  expect_no_error(extract_audio(infile, "a.mka", run = FALSE))
  expect_no_error(convert_audio(infile, "a.mp3", run = FALSE))
  expect_no_error(extract_audio_batch(jobs, run = FALSE))
  expect_no_error(convert_audio_batch(
    tibble::tibble(input = infile, output = "a.mp3"), run = FALSE
  ))
})

test_that("run = FALSE compiles cleanly with ffmpeg and ffprobe masked off PATH", {
  # AC2 as the plan words it (M30's trick). Weaker than the mocked test above --
  # it is kept because it exercises the real locator rather than a stand-in.
  withr::local_envvar(PATH = "")
  skip_if(nzchar(Sys.which("ffmpeg")) || nzchar(Sys.which("ffprobe")),
          "PATH could not be masked on this platform")
  infile <- make_input("mkv")
  expect_no_warning({
    extract_audio(infile, "a.mka", run = FALSE)
    convert_audio(infile, "a.mp3", run = FALSE)
    extract_audio_batch(tibble::tibble(input = infile, output = "a.mka"),
                        run = FALSE)
    convert_audio_batch(tibble::tibble(input = infile, output = "a.mp3"),
                        run = FALSE)
  })
})

test_that("ffm_batch()'s signature is untouched by the diagnostic", {
  # D024/RR02 Q3: the probe belongs in the Layer-2 verb, never as a hook in the
  # generic runner. This pins that the engine contract did not move.
  expect_identical(
    names(formals(ffm_batch)),
    c("jobs", ".f", "...", "run", "parallel", "verify", "progress", "manifest",
      "checksums")
  )
})


# Every failure path is silent (AC3) ---------------------------------------

test_that("an absent ffprobe skips the check without error or warning", {
  # Non-vacuous: count_audio_streams() resolves the binary through
  # find_ffprobe(), which is the binding mocked here. PATH masking cannot
  # produce this machine -- it takes ffmpeg away too.
  skip_if_no_ffmpeg()
  infile <- make_multitrack_video()
  out <- withr::local_tempfile(fileext = ".mka")
  local_mocked_bindings(find_ffprobe = function() NULL)
  expect_no_warning(expect_no_error(extract_audio(infile, out)))
})

test_that("count_audio_streams() answers NA rather than signalling", {
  skip_if_no_ffprobe()
  # An unreadable input: ffprobe exits non-zero, which arrives as a `status`
  # attribute rather than an R condition and must not read as a count.
  expect_no_warning(
    expect_identical(count_audio_streams(withr::local_tempfile(fileext = ".mp4")),
                     NA_integer_)
  )
  local_mocked_bindings(find_ffprobe = function() NULL)
  expect_no_warning(expect_identical(count_audio_streams("anything.mp4"),
                                     NA_integer_))
})

test_that("an unanswered count warns about nothing", {
  expect_no_warning(warn_dropped_audio("x.mkv", NA_integer_))
  expect_no_warning(warn_dropped_audio("x.mkv", 1L))
})


# The batch form (AC4) ------------------------------------------------------

test_that("only rows that named no track are probed", {
  probed <- character(0)
  local_mocked_bindings(
    count_audio_streams = function(file) {
      probed <<- c(probed, file)
      3L
    }
  )
  cols <- c("x", "y")

  # Every row names a track: no probe at all.
  jobs <- tibble::tibble(input = c("a.mkv", "b.mkv"), output = cols,
                         audio_stream = c(0, 1))
  expect_no_warning(warn_dropped_audio_batch(jobs))
  expect_identical(probed, character(0))

  # A scalar argument covers every row: likewise no probe.
  jobs <- tibble::tibble(input = c("a.mkv", "b.mkv"), output = cols)
  expect_no_warning(warn_dropped_audio_batch(jobs, audio_stream = 1))
  expect_identical(probed, character(0))

  # An NA cell is the column form of the NULL sentinel (D023) -- that row named
  # NO track, so it is probed and the row that did name one is not.
  jobs <- tibble::tibble(input = c("a.mkv", "b.mkv"), output = cols,
                         audio_stream = c(NA, 1))
  expect_warning(warn_dropped_audio_batch(jobs),
                 class = "tidymedia_dropped_audio")
  expect_identical(probed, "a.mkv")
})

test_that("a repeated input is probed once", {
  probed <- character(0)
  local_mocked_bindings(
    count_audio_streams = function(file) {
      probed <<- c(probed, file)
      3L
    }
  )
  jobs <- tibble::tibble(input = c("a.mkv", "a.mkv", "b.mkv"),
                         output = c("x", "y", "z"))
  expect_warning(warn_dropped_audio_batch(jobs),
                 class = "tidymedia_dropped_audio")
  expect_identical(probed, c("a.mkv", "b.mkv"))
})

test_that("the batch warns once, naming every affected row and no other", {
  local_mocked_bindings(
    count_audio_streams = function(file) if (file == "many.mkv") 3L else 1L
  )
  jobs <- tibble::tibble(input = c("many.mkv", "one.mkv", "many.mkv"),
                         output = c("x", "y", "z"))
  w <- tryCatch(warn_dropped_audio_batch(jobs), warning = function(w) w)
  expect_s3_class(w, "tidymedia_dropped_audio")
  msg <- cli::ansi_strip(conditionMessage(w))
  expect_match(msg, "4 audio tracks from 2 inputs")
  expect_match(msg, "Row 1")
  expect_match(msg, "Row 3")
  expect_false(grepl("Row 2", msg))
})

test_that("the batch verbs warn end to end", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  jobs <- tibble::tibble(
    input = c(infile, infile),
    output = c(withr::local_tempfile(fileext = ".mka"),
               withr::local_tempfile(fileext = ".mka"))
  )
  expect_warning(extract_audio_batch(jobs), class = "tidymedia_dropped_audio")
})
