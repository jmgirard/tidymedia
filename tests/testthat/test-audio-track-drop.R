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
  #
  # It RECORDS the invocation rather than raising from the mock. A mock that
  # stop()s proves nothing here: count_audio_streams() wraps run_program() in a
  # tryCatch() -- deliberately, so a broken probe stays silent -- which swallows
  # the mock's error and lets a probe on the compile path pass unseen. Measured:
  # with the stop() spelling, deleting the `run` gate left this test green.
  infile <- make_input("mkv")
  called <- 0L
  local_mocked_bindings(
    run_program = function(...) {
      called <<- called + 1L
      character(0)
    }
  )
  extract_audio(infile, "a.mka", run = FALSE)
  convert_audio(infile, "a.mp3", run = FALSE)
  extract_audio_batch(tibble::tibble(input = infile, output = "a.mka"),
                      run = FALSE)
  convert_audio_batch(tibble::tibble(input = infile, output = "a.mp3"),
                      run = FALSE)
  expect_identical(called, 0L)
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

test_that("a locator that throws is as silent as one that returns NULL", {
  # find_ffprobe() has TWO failure channels, not one. The state that showed it
  # was an empty or multi-line user config, which made find_program() abort
  # rather than warn until M116 guarded it; without a tryCatch around the
  # locator itself that abort escaped and took the verb with it (M44 review
  # F2), which the NULL-returning mock cannot detect. The mock below stands in
  # for the error channel directly, so this test is unaffected by that repair
  # and still says what happens if anything below find_ffprobe() throws.
  local_mocked_bindings(
    find_ffprobe = function() stop("argument is of length zero")
  )
  expect_no_warning(
    expect_identical(count_audio_streams("anything.mp4"), NA_integer_)
  )
})

test_that("a locator returning character(0) does not throw on the guard", {
  local_mocked_bindings(find_ffprobe = function() character(0))
  expect_no_warning(
    expect_identical(count_audio_streams("anything.mp4"), NA_integer_)
  )
})


# The message survives hostile input (AC1, AC3) ----------------------------

test_that("a file path containing braces neither aborts nor misreports", {
  # cli_warn() glue-interpolates every bullet, so an unescaped path is executed
  # rather than printed: `my{video}.mkv` aborted the verb outright and `{n}.mkv`
  # -- naming a local of warn_dropped_audio() -- silently printed a filename
  # that does not exist. Either one gives the probe an effect beyond its
  # diagnostic, which is what D024 licenses it on not having (M44 review F1).
  hostile <- c("my{video}.mkv", "{n}.mkv", "{inputs}.mkv", "a}b{c.mkv")
  for (path in hostile) {
    w <- tryCatch(warn_dropped_audio(path, 3L), warning = function(w) w)
    expect_s3_class(w, "tidymedia_dropped_audio")
    expect_match(cli::ansi_strip(conditionMessage(w)), path, fixed = TRUE)
  }
  # And through the batch builder, whose bullets go through basename().
  jobs <- tibble::tibble(input = c("{n}.mkv", "plain.mkv"),
                         output = c("x", "y"))
  local_mocked_bindings(count_audio_streams = function(file) 3L)
  w <- tryCatch(warn_dropped_audio_batch(jobs), warning = function(w) w)
  expect_s3_class(w, "tidymedia_dropped_audio")
  expect_match(cli::ansi_strip(conditionMessage(w)), "{n}.mkv", fixed = TRUE)
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


# M075: the loudness verbs join the family (AC1-AC5) -----------------------

# catch_drop() -- collect every dropped-audio warning an expression signals --
# lives in helper-audio-track-drop.R, shared with test-check-tracks-seam.R.

# AC5's shape, at its one site: a wrong value must abort with `match` in its
# message AND signal no drop warning on the way there. Both halves matter --
# an abort that warned first still passes expect_error() alone.
expect_refuses_before_warning <- function(expr, match) {
  res <- catch_drop(expr)
  testthat::expect_s3_class(res$value, "error")
  testthat::expect_match(cli::ansi_strip(conditionMessage(res$value)), match)
  testthat::expect_length(res$warnings, 0L)
}

# Both two_pass values, because AC1 quantifies over every run = TRUE call and
# the verb probes from two different sites. Round 1 of review found the
# two-pass path warning TWICE -- a count the single-value test could not see.
test_that("normalize_audio() warns once at both two_pass values, naming the count, the argument and the offset", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  for (tp in c(FALSE, TRUE)) {
    out <- withr::local_tempfile(fileext = ".mkv")
    res <- catch_drop(normalize_audio(infile, out, two_pass = tp))
    expect_length(res$warnings, 1L)
    msg <- cli::ansi_strip(conditionMessage(res$warnings[[1]]))
    expect_match(msg, "3 audio tracks")
    expect_match(msg, "drops 2")
    expect_match(msg, "audio_stream")
    expect_match(msg, "probe_audio")
    expect_match(msg, "1, 2, 3", fixed = TRUE)
    expect_match(msg, "0, 1, 2", fixed = TRUE)
  }
})

test_that("normalize_audio_batch() warns once, naming every affected row", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  jobs <- tibble::tibble(
    input = c(infile, infile),
    output = c(withr::local_tempfile(fileext = ".mkv"),
               withr::local_tempfile(fileext = ".mkv"))
  )
  res <- catch_drop(normalize_audio_batch(jobs))
  expect_length(res$warnings, 1L)
  msg <- cli::ansi_strip(conditionMessage(res$warnings[[1]]))
  expect_match(msg, "Row 1")
  expect_match(msg, "Row 2")
})

# AC3: the five silent cases ------------------------------------------------

test_that("naming a track silences normalize_audio()", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  expect_no_warning(
    normalize_audio(infile, withr::local_tempfile(fileext = ".mkv"),
                    audio_stream = 1)
  )
})

test_that("the audio_stream argument silences normalize_audio_batch()", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  jobs <- tibble::tibble(
    input = c(infile, infile),
    output = c(withr::local_tempfile(fileext = ".mkv"),
               withr::local_tempfile(fileext = ".mkv"))
  )
  expect_no_warning(normalize_audio_batch(jobs, audio_stream = 1))
})

test_that("an audio_stream cell on every row silences normalize_audio_batch()", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  jobs <- tibble::tibble(
    input = c(infile, infile),
    output = c(withr::local_tempfile(fileext = ".mkv"),
               withr::local_tempfile(fileext = ".mkv")),
    audio_stream = c(1, 2)
  )
  expect_no_warning(normalize_audio_batch(jobs))
})

test_that("run = FALSE is silent on both verbs at both two_pass values", {
  # run = FALSE still runs the two-pass ANALYSIS on both verbs -- what it gates
  # is the correction command -- so this needs the binary, not just a compile.
  skip_if_no_ffmpeg()
  infile <- make_multitrack_video()
  jobs <- tibble::tibble(input = infile, output = "out.mkv")
  for (tp in c(FALSE, TRUE)) {
    expect_no_warning(
      normalize_audio(infile, "out.mkv", two_pass = tp, run = FALSE)
    )
    expect_no_warning(normalize_audio_batch(jobs, two_pass = tp, run = FALSE))
  }
})

test_that("a single-track input warns about nothing on the loudness verbs", {
  skip_if_no_ffprobe()
  infile <- make_test_video()
  expect_no_warning(
    normalize_audio(infile, withr::local_tempfile(fileext = ".m4a"))
  )
})

# AC4: the warning lands before the two-pass analysis ------------------------

test_that("normalize_audio() warns before the analysis pass runs", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  # M44's lesson: a stop()ing mock only proves ordering where the call site is
  # not wrapped in tryCatch(error = ), which this one is not -- so the error
  # propagating is itself part of the assertion.
  local_mocked_bindings(
    run_loudnorm_analysis = function(...) stop("analysis pass reached")
  )
  res <- catch_drop(
    normalize_audio(infile, withr::local_tempfile(fileext = ".mkv"),
                    two_pass = TRUE)
  )
  expect_length(res$warnings, 1L)
  expect_s3_class(res$value, "error")
  expect_match(conditionMessage(res$value), "analysis pass reached")
})

test_that("normalize_audio_batch() warns before Phase 1 runs", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  local_mocked_bindings(
    run_loudnorm_analysis_batch = function(...) stop("Phase 1 reached")
  )
  jobs <- tibble::tibble(
    input = c(infile, infile),
    output = c(withr::local_tempfile(fileext = ".mkv"),
               withr::local_tempfile(fileext = ".mkv"))
  )
  res <- catch_drop(normalize_audio_batch(jobs, two_pass = TRUE))
  expect_length(res$warnings, 1L)
  expect_s3_class(res$value, "error")
  expect_match(conditionMessage(res$value), "Phase 1 reached")
})

# AC5: a wrong value still refuses before anything warns ---------------------

test_that("normalize_audio(target_loudness = 999) refuses before it probes", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  expect_refuses_before_warning(
    normalize_audio(infile, withr::local_tempfile(fileext = ".mkv"),
                    target_loudness = 999),
    "target_loudness"
  )
})

test_that("normalize_audio(target_loudness = 999, two_pass = TRUE) refuses before it probes", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  expect_refuses_before_warning(
    normalize_audio(infile, withr::local_tempfile(fileext = ".mkv"),
                    target_loudness = 999, two_pass = TRUE),
    "target_loudness"
  )
})

test_that("normalize_audio(audio_codec = 'copy') refuses before it probes", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  expect_refuses_before_warning(
    normalize_audio(infile, withr::local_tempfile(fileext = ".mkv"),
                    audio_codec = "copy"),
    "audio_codec"
  )
})

test_that("normalize_audio(audio_codec = 'copy', two_pass = TRUE) refuses before it probes", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  expect_refuses_before_warning(
    normalize_audio(infile, withr::local_tempfile(fileext = ".mkv"),
                    audio_codec = "copy", two_pass = TRUE),
    "audio_codec"
  )
})

test_that("a 999 target_loudness cell refuses before normalize_audio_batch() probes", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  jobs <- tibble::tibble(input = infile, output = "out.mkv",
                         target_loudness = 999)
  expect_refuses_before_warning(normalize_audio_batch(jobs), "target_loudness")
})

test_that("a 999 target_loudness cell refuses before two-pass batch probes", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  jobs <- tibble::tibble(input = infile, output = "out.mkv",
                         target_loudness = 999)
  expect_refuses_before_warning(
    normalize_audio_batch(jobs, two_pass = TRUE), "target_loudness"
  )
})

test_that("a 'copy' audio_codec cell refuses before normalize_audio_batch() probes", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  jobs <- tibble::tibble(input = infile, output = "out.mkv",
                         audio_codec = "copy")
  expect_refuses_before_warning(normalize_audio_batch(jobs), "audio_codec")
})

test_that("a 'copy' audio_codec cell refuses before two-pass batch probes", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  jobs <- tibble::tibble(input = infile, output = "out.mkv",
                         audio_codec = "copy")
  expect_refuses_before_warning(
    normalize_audio_batch(jobs, two_pass = TRUE), "audio_codec"
  )
})
