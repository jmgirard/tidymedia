# MediaInfo-backed helpers. All are gated on the mediainfo binary and therefore
# skip on machines/CI images without it.

test_that("mediainfo_parameter() returns a scalar value", {
  skip_if_no_mediainfo()
  infile <- make_test_video()
  val <- mediainfo_parameter(infile, "General", "Format")
  expect_length(val, 1)
})

test_that("get_duration() returns a numeric duration in the requested unit", {
  skip_if_no_mediainfo()
  infile <- make_test_video()
  ms <- get_duration(infile, unit = "ms")
  sec <- get_duration(infile, unit = "sec")
  expect_type(as.numeric(ms), "double")
  expect_equal(as.numeric(sec), as.numeric(ms) / 1000, tolerance = 1e-6)
})

test_that("get_framerate()/get_width()/get_height() query the video stream", {
  skip_if_no_mediainfo()
  infile <- make_test_video()
  expect_equal(as.numeric(get_width(infile)), 64)
  expect_equal(as.numeric(get_height(infile)), 64)
  expect_gt(as.numeric(get_framerate(infile)), 0)
})

test_that("mediainfo_query() returns a one-row tibble", {
  skip_if_no_mediainfo()
  infile <- make_test_video()
  out <- mediainfo_query(infile, "General", c("Format", "Duration"))
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
})

test_that("mediainfo_query() rejects mismatched names length", {
  # Validation aborts before any binary is invoked, so this needs no mediainfo.
  infile <- withr::local_tempfile(fileext = ".mp4")
  file.create(infile)
  expect_error(
    mediainfo_query(infile, "General", c("Format", "Duration"), names = "one")
  )
})
