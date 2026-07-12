# Tests for anonymize_video(): cover fixed rectangular regions of a video with
# opaque filled boxes. Command construction is tested purely (run = FALSE);
# execution is gated on the ffmpeg binary.

# Command construction ----------------------------------------------------

test_that("anonymize_video() draws one filled box per region", {
  f <- make_input()
  regions <- data.frame(x = 10, y = 20, width = 120, height = 90)
  cmd <- anonymize_video(f, "anon.mp4", regions, run = FALSE)
  expect_match(cmd, "drawbox=x=10:y=20:w=120:h=90:c=black:t=fill", fixed = TRUE)
  expect_match(cmd, f, fixed = TRUE)
  expect_match(cmd, '"anon.mp4"', fixed = TRUE)
})

test_that("anonymize_video() chains N regions into a single -vf (no filter_complex)", {
  f <- make_input()
  regions <- data.frame(
    x = c(10, 200), y = c(10, 150),
    width = c(120, 80), height = c(90, 60)
  )
  cmd <- anonymize_video(f, "anon.mp4", regions, run = FALSE)
  # Two boxes, both present, both filled.
  expect_match(cmd, "drawbox=x=10:y=10:w=120:h=90:c=black:t=fill", fixed = TRUE)
  expect_match(cmd, "drawbox=x=200:y=150:w=80:h=60:c=black:t=fill", fixed = TRUE)
  # A single-input sequential chain: -vf, never -filter_complex (IP2).
  expect_match(cmd, "-vf ", fixed = TRUE)
  expect_no_match(cmd, "-filter_complex", fixed = TRUE)
})

test_that("anonymize_video() re-encodes reproducibly and copies audio", {
  f <- make_input()
  regions <- data.frame(x = 0, y = 0, width = 50, height = 50)
  cmd <- anonymize_video(f, "anon.mp4", regions, run = FALSE)
  # Even-dimension safeguard (M12), default codec/pixfmt, audio stream-copy.
  expect_match(cmd, "crop=w=floor(in_w/2)*2:h=floor(in_h/2)*2", fixed = TRUE)
  expect_match(cmd, "-codec:v libx264 -codec:a copy", fixed = TRUE)
  expect_match(cmd, "-pix_fmt yuv420p", fixed = TRUE)
  # The same input and regions compile byte-identically.
  expect_identical(cmd, anonymize_video(f, "anon.mp4", regions, run = FALSE))
})

test_that("anonymize_video() honors color argument, per-row color, and expressions", {
  f <- make_input()
  # Default is opaque black; the color argument sets the default fill.
  cmd_arg <- anonymize_video(
    f, "anon.mp4",
    data.frame(x = 0, y = 0, width = 10, height = 10),
    color = "red", run = FALSE
  )
  expect_match(cmd_arg, "c=red:t=fill", fixed = TRUE)

  # A per-row color column overrides the argument for that row only.
  regions <- data.frame(
    x = c(0, 5), y = c(0, 5),
    width = c(10, 20), height = c(10, 20),
    color = c("white", "green"), stringsAsFactors = FALSE
  )
  cmd_col <- anonymize_video(f, "anon.mp4", regions, color = "red", run = FALSE)
  expect_match(cmd_col, "w=10:h=10:c=white:t=fill", fixed = TRUE)
  expect_match(cmd_col, "w=20:h=20:c=green:t=fill", fixed = TRUE)
  expect_no_match(cmd_col, "c=red", fixed = TRUE)

  # FFmpeg expressions pass through unquoted for size and position.
  cmd_expr <- anonymize_video(
    f, "anon.mp4",
    data.frame(x = "in_w/4", y = 0, width = "in_w/2", height = "in_h",
               stringsAsFactors = FALSE),
    run = FALSE
  )
  expect_match(cmd_expr, "drawbox=x=in_w/4:y=0:w=in_w/2:h=in_h", fixed = TRUE)
})

test_that("anonymize_video() accepts integer coordinate columns", {
  f <- make_input()
  # tibble() keeps 1:2 as integers; check_dim() rejects bare integers, so the
  # pipeline must coerce them to double (regression guard).
  regions <- tibble::tibble(
    x = 1:2, y = c(0L, 5L), width = c(10L, 20L), height = c(10L, 20L)
  )
  expect_no_error(anonymize_video(f, "anon.mp4", regions, run = FALSE))
})

# Validation --------------------------------------------------------------

test_that("anonymize_video() rejects a malformed regions table", {
  f <- make_input()
  expect_error(anonymize_video(f, "anon.mp4", list(x = 1), run = FALSE),
               "must be a data frame")
  expect_error(
    anonymize_video(f, "anon.mp4", data.frame(x = numeric(0), y = numeric(0),
                                              width = numeric(0),
                                              height = numeric(0)),
                    run = FALSE),
    "at least one row"
  )
  expect_error(
    anonymize_video(f, "anon.mp4", data.frame(x = 0, y = 0, width = 10),
                    run = FALSE),
    "missing"
  )
  expect_error(
    anonymize_video(f, "anon.mp4",
                    data.frame(x = c(0, NA), y = 0, width = 10, height = 10),
                    run = FALSE),
    "must not contain"
  )
  # Non-positive size is caught per-region by ffm_drawbox()'s check_dim().
  expect_error(
    anonymize_video(f, "anon.mp4",
                    data.frame(x = 0, y = 0, width = 0, height = 10),
                    run = FALSE),
    "single FFmpeg expression or number"
  )
})

# Execution ---------------------------------------------------------------

test_that("anonymize_video() runs end-to-end and writes a valid video", {
  skip_if_no_ffmpeg()
  src <- make_test_video()
  out <- withr::local_tempfile(fileext = ".mp4")
  regions <- data.frame(x = 8, y = 8, width = 24, height = 24)
  anonymize_video(src, out, regions)
  expect_true(file.exists(out))
  # The 64x64 (even) source encodes unchanged in size, proving the box was
  # drawn and the file re-encoded successfully.
  expect_gt(file.info(out)$size, 0)
  expect_equal(probe_duration(out), 2, tolerance = 0.2)
})
