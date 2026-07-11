# Tests for verify_media() and its pure comparison core. The core
# (compare_expectations) is binary-free and CI-safe; verify_media() probes a
# real file and is binary-gated at the bottom.

# compare_expectations() — pure core (CI-safe) ----------------------------------

test_that("compare_expectations() returns the tidy report shape", {
  res <- compare_expectations(
    expected = list(duration = 5, video_codec = "h264"),
    actual   = list(duration = 5, video_codec = "h264")
  )
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("check", "expected", "actual", "pass"))
  expect_equal(res$check, c("duration", "video_codec"))
  expect_type(res$pass, "logical")
})

test_that("compare_expectations() passes exact numeric and string matches", {
  res <- compare_expectations(
    expected = list(width = 320L, video_codec = "h264"),
    actual   = list(width = 320L, video_codec = "h264")
  )
  expect_true(all(res$pass))
})

test_that("compare_expectations() honors the numeric tolerance (inclusive)", {
  # exact edge: diff == tolerance passes (0.5 is exactly representable)
  expect_true(
    compare_expectations(list(d = 2), list(d = 2.5), tolerance = 0.5)$pass
  )
  # just over the edge fails
  expect_false(
    compare_expectations(list(d = 2), list(d = 3), tolerance = 0.5)$pass
  )
})

test_that("compare_expectations() default tolerance keeps integers exact", {
  expect_true(compare_expectations(list(w = 320), list(w = 320))$pass)
  expect_false(compare_expectations(list(w = 320), list(w = 321))$pass)
})

test_that("compare_expectations() compares strings exactly, not fuzzily", {
  expect_false(compare_expectations(list(c = "h264"), list(c = "hevc"))$pass)
  expect_true(compare_expectations(list(c = "h264"), list(c = "h264"))$pass)
})

test_that("compare_expectations() marks a missing (NA) actual as failing", {
  res <- compare_expectations(
    expected = list(audio_codec = "aac"),
    actual   = list(audio_codec = NA)
  )
  expect_false(res$pass)
  expect_true(is.na(res$actual))
})

test_that("compare_expectations() coerces a character actual for numeric checks", {
  # probe values can arrive as strings; a numeric expectation still compares
  res <- compare_expectations(list(width = 320), list(width = "320"))
  expect_true(res$pass)
})

test_that("compare_expectations() renders expected/actual as character columns", {
  res <- compare_expectations(list(width = 320), list(width = 321))
  expect_type(res$expected, "character")
  expect_type(res$actual, "character")
  expect_equal(res$expected, "320")
  expect_equal(res$actual, "321")
})

# verify_media() — validation (CI-safe) -----------------------------------------

test_that("verify_media() errors when no properties are checked", {
  v <- system.file("extdata", "sample.mp4", package = "tidymedia")
  skip_if(!nzchar(v))
  expect_error(verify_media(v), "at least one property")
})

test_that("verify_media() errors on a missing file", {
  expect_error(
    verify_media(tempfile(fileext = ".mp4"), width = 320),
    "does not exist"
  )
})

# verify_media() — probing (binary-gated) ---------------------------------------

test_that("verify_media() returns a passing tibble for a matching spec", {
  skip_if_no_ffprobe()
  v <- system.file("extdata", "sample.mp4", package = "tidymedia")
  res <- verify_media(
    v, duration = 1, width = 320, height = 240,
    video_codec = "h264", audio_codec = "aac"
  )
  expect_named(res, c("file", "check", "expected", "actual", "pass"))
  expect_equal(nrow(res), 5)
  expect_true(all(res$pass))
})

test_that("verify_media() flags a mismatch and reports the actual value", {
  skip_if_no_ffprobe()
  v <- system.file("extdata", "sample.mp4", package = "tidymedia")
  res <- verify_media(v, width = 999)
  expect_false(res$pass[res$check == "width"])
  expect_equal(res$actual[res$check == "width"], "320")
})

test_that("verify_media() resolves ... extras against the probe columns", {
  skip_if_no_ffprobe()
  v <- system.file("extdata", "sample.mp4", package = "tidymedia")
  # pix_fmt lives on the video stream; format-level names on the container
  res <- verify_media(v, pix_fmt = "yuv420p")
  expect_true(res$pass[res$check == "pix_fmt"])
})

test_that("verify_media() marks a checked-but-absent stream as failing", {
  skip_if_no_ffmpeg()
  src <- make_test_video()
  vo <- withr::local_tempfile(fileext = ".mp4")
  ffm(src, vo) |> ffm_drop("audio") |> ffm_run()
  res <- verify_media(vo, audio_codec = "aac")
  expect_false(res$pass[res$check == "audio_codec"])
  expect_true(is.na(res$actual[res$check == "audio_codec"]))
})
