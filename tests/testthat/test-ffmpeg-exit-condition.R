# M085: a non-zero FFmpeg exit is a condition a caller can catch by class, and
# the exit number rides on the condition as `tm_status` rather than being read
# back out of the formatted message.

test_that("a non-zero exit from ffm_run() is catchable by class alone", {
  skip_if_no_ffmpeg()
  infile <- make_test_video()
  out <- withr::local_tempfile(fileext = ".mp3")
  # Copying AAC into an MP3 container is a guaranteed non-zero exit on every
  # FFmpeg build; leaving the codec unset would simply re-encode and succeed.
  p <- ffm_codec(ffm_map(ffm_files(infile, out), "0:a"), audio = "copy")
  cnd <- tryCatch(ffm_run(p), tidymedia_ffmpeg_exit = function(e) e)

  # The class vector exactly: a parent or sibling would promise handlers this
  # milestone did not ship (M085-D2).
  expect_identical(
    class(cnd),
    c("tidymedia_ffmpeg_exit", "rlang_error", "error", "condition")
  )
  expect_true(is.integer(cnd$tm_status))
  expect_length(cnd$tm_status, 1L)
  expect_false(identical(cnd$tm_status, 0L))

  # Oracle: the same command, spawned the way run_program() spawns it
  # (R/program_management.R), writing to a FRESH output path so the first run's
  # leftovers cannot change the status this returns.
  oracle_out <- withr::local_tempfile(fileext = ".mp3")
  q <- ffm_codec(ffm_map(ffm_files(infile, oracle_out), "0:a"), audio = "copy")
  quote_type <- if (.Platform$OS.type == "windows") "cmd" else "sh"
  observed <- suppressWarnings(system2(
    find_ffmpeg(), shQuote(ffm_args(q), type = quote_type),
    stdout = TRUE, stderr = "", input = "", timeout = 0
  ))
  expect_identical(cnd$tm_status, attr(observed, "status"))
})

test_that("the loudnorm analysis pass raises the same class and field", {
  skip_if_no_ffmpeg()
  # A file FFmpeg cannot demux at all: the analysis pass exits non-zero before
  # it can print a measurement block.
  bad <- withr::local_tempfile(fileext = ".mp4")
  writeLines("this is not a media file", bad)
  cnd <- tryCatch(run_loudnorm_analysis(bad),
                  tidymedia_ffmpeg_exit = function(e) e)

  expect_s3_class(cnd, "tidymedia_ffmpeg_exit")
  expect_true(is.integer(cnd$tm_status))
  expect_length(cnd$tm_status, 1L)
  expect_false(identical(cnd$tm_status, 0L))
  # The prose is the one this abort has always carried.
  expect_match(
    cli::ansi_strip(conditionMessage(cnd)),
    "The `loudnorm` analysis pass failed (FFmpeg exited with status",
    fixed = TRUE
  )
})

test_that("ffmpeg_exit_status() reads the class and the field, nothing else", {
  # No message at all: the status comes from the field, so there is nothing to
  # parse and nothing to strip.
  expect_identical(
    ffmpeg_exit_status(rlang::error_cnd("tidymedia_ffmpeg_exit", tm_status = 3L)),
    3L
  )
  # Classed but fieldless.
  expect_identical(
    ffmpeg_exit_status(rlang::error_cnd("tidymedia_ffmpeg_exit")),
    NA_integer_
  )
  # An unresolvable binary: run_program()'s own abort, caught from the call
  # rather than constructed, because it carries no class to construct.
  no_binary <- tryCatch(run_program(NULL, "-version", program = "FFmpeg"),
                        error = function(e) e)
  expect_identical(ffmpeg_exit_status(no_binary), NA_integer_)
  # A timeout, and the multi-track diagnostic: both are tidymedia conditions
  # and neither is a non-zero exit.
  timed_out <- tryCatch(abort_timeout("FFmpeg", 5), error = function(e) e)
  expect_s3_class(timed_out, "tidymedia_timeout")
  expect_identical(ffmpeg_exit_status(timed_out), NA_integer_)
  expect_identical(
    ffmpeg_exit_status(rlang::error_cnd(
      "tidymedia_multitrack_separation",
      message = "Can't write out.mp3: FFmpeg exited with status 3."
    )),
    NA_integer_
  )
  # An unclassed condition whose message carries the phrase. The old parse
  # answered 3 here; reading the class answers NA, which is the intended change
  # (AC4) and unobservable outside the package.
  expect_identical(
    ffmpeg_exit_status(simpleError("FFmpeg exited with status 3.")),
    NA_integer_
  )
})

test_that("a missing FFmpeg binary still falls open past the enrichment", {
  # The enrichment's fail-open (D024): no status means "not the failure this
  # diagnostic is about", so the original condition is re-raised untouched.
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  audio <- withr::local_tempfile(fileext = ".mp3")
  video <- withr::local_tempfile(fileext = ".mp4")
  local_mocked_bindings(find_ffmpeg = function() NULL)
  cnd <- tryCatch(separate_audio_video(infile, audio, video),
                  error = function(e) e)
  expect_false(inherits(cnd, "tidymedia_multitrack_separation"))
  expect_match(cli::ansi_strip(conditionMessage(cnd)), "Could not locate FFmpeg")
})
