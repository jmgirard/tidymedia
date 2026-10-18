# The direct-command functions and the two capability listings refuse a
# missing program with the message run_program() gives (M142). Before M142,
# with no program found, ffmpeg() returned character(0), ffprobe() and
# mediainfo() stopped with "non-empty character argument expected", and
# ffmpeg_codecs() and ffmpeg_encoders() stopped with "argument of length 0".
# find_program() returns NULL in every case where it finds no program, so the
# mock stands in for all of them. No binary is needed.

missing_program_refusal <- function(expr) {
  # `.package`, as the suite's other helpers write it. With `.env = topenv()`
  # the mock was still in place after this helper returned (measured
  # 2026-10-18), and the later test files could not find FFmpeg.
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

test_that("the lookup's warning reaches the caller with its class (M142)", {
  # Every find_program() path that returns NULL warns first. Before M142 the
  # lookup ran inside guard_timeout(), which held its warnings: ffmpeg() gave
  # them again as plain warnings, and ffprobe() and mediainfo() lost them when
  # system() then stopped.
  testthat::local_mocked_bindings(
    find_program = function(program, ...) {
      cli::cli_warn("M142 stub: {program} is gone.",
                    class = "tidymedia_location_gone")
      NULL
    },
    .package = "tidymedia"
  )
  calls <- list(ffmpeg = quote(ffmpeg("-version")),
                ffprobe = quote(ffprobe("-version")),
                mediainfo = quote(mediainfo("--Version")))
  for (nm in names(calls)) {
    seen <- character()
    cnd <- tryCatch(
      withCallingHandlers(
        eval(calls[[nm]]),
        warning = function(w) {
          seen <<- c(seen, class(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) e
    )
    expect_true("tidymedia_location_gone" %in% seen, info = nm)
    expect_match(conditionMessage(cnd), "Could not locate", fixed = TRUE,
                 info = nm)
  }
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
