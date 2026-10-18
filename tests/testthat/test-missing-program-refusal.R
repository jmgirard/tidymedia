# The direct-command functions and the two capability listings refuse a
# missing program with the message run_program() gives (M142). Before M142,
# with no program found, ffmpeg() returned character(0), ffprobe() and
# mediainfo() stopped with "non-empty character argument expected", and
# ffmpeg_codecs() and ffmpeg_encoders() stopped with "argument of length 0".
# find_program() returns NULL in every case where it finds no program, so the
# mock stands in for all of them. No binary is needed.

missing_program_refusal <- function(expr) {
  # `.package`, as the suite's other helpers write it: with `.env = topenv()`
  # the mock outlived this helper and hid FFmpeg from every later test file.
  testthat::local_mocked_bindings(find_program = function(...) NULL,
                                  .package = "tidymedia")
  cnd <- tryCatch(expr, error = function(e) e)
  expect_s3_class(cnd, "rlang_error")
  cnd
}

test_that("ffmpeg() refuses a missing FFmpeg and names itself (M142)", {
  cnd <- missing_program_refusal(ffmpeg("-version"))
  expect_identical(cli::ansi_strip(conditionMessage(cnd)),
                   "Could not locate FFmpeg.")
  expect_identical(cnd$call[[1]], quote(ffmpeg))
})

test_that("ffprobe() refuses a missing FFprobe and names itself (M142)", {
  cnd <- missing_program_refusal(ffprobe("-version"))
  expect_identical(cli::ansi_strip(conditionMessage(cnd)),
                   "Could not locate FFprobe.")
  expect_identical(cnd$call[[1]], quote(ffprobe))
})

test_that("mediainfo() refuses a missing MediaInfo and names itself (M142)", {
  cnd <- missing_program_refusal(mediainfo("--Version"))
  expect_identical(cli::ansi_strip(conditionMessage(cnd)),
                   "Could not locate MediaInfo.")
  expect_identical(cnd$call[[1]], quote(mediainfo))
})

test_that("ffmpeg_codecs() refuses a missing FFmpeg and names itself (M142)", {
  cnd <- missing_program_refusal(ffmpeg_codecs())
  expect_identical(cli::ansi_strip(conditionMessage(cnd)),
                   "Could not locate FFmpeg.")
  expect_identical(cnd$call[[1]], quote(ffmpeg_codecs))
})

test_that("ffmpeg_encoders() refuses a missing FFmpeg and names itself (M142)", {
  cnd <- missing_program_refusal(ffmpeg_encoders())
  expect_identical(cli::ansi_strip(conditionMessage(cnd)),
                   "Could not locate FFmpeg.")
  expect_identical(cnd$call[[1]], quote(ffmpeg_encoders))
})
