# M135: the `quality` argument -- the encoder's own rate-control value, passed
# through unchanged. quality_flags() is the one table keyed on the exact
# resolved encoder name; check_quality() refuses a value that is not one finite
# number, one outside the row's range, and one whose encoder the table lacks.
# No cross-encoder scale exists, so the number means what the encoder says.

test_that("quality_flags() holds the seven encoders with their flag and range", {
  tbl <- quality_flags()
  expect_setequal(names(tbl), c("encoder", "flag", "min", "max"))
  expect_identical(
    tbl$encoder,
    c("libx264", "libx265", "h264_nvenc", "hevc_nvenc", "av1_nvenc",
      "h264_videotoolbox", "hevc_videotoolbox")
  )
  expect_identical(tbl$flag[tbl$encoder %in% c("libx264", "libx265")],
                   c("-crf", "-crf"))
  expect_identical(tbl$flag[grepl("_nvenc$", tbl$encoder)],
                   c("-cq", "-cq", "-cq"))
  expect_identical(tbl$flag[grepl("_videotoolbox$", tbl$encoder)],
                   c("-q:v", "-q:v"))
  expect_identical(tbl$min, c(0, 0, 0, 0, 0, 1, 1))
  expect_identical(tbl$max, c(51, 51, 51, 51, 51, 100, 100))
  # Every hardware encoder the backend table can produce has a row, so no
  # (family, backend) pair a verb resolves to is silently unkeyed.
  hw <- unlist(lapply(names(hardware_backend_families()), function(b) {
    paste0(hardware_backend_families()[[b]], "_", b)
  }))
  expect_true(all(hw %in% tbl$encoder))
})

# A frame to blame, standing in for the verb the caller typed.
quality_caller <- function(quality, encoder) {
  check_quality(quality, encoder, call = rlang::current_env())
}

test_that("check_quality() accepts NULL and an in-range number for every row", {
  tbl <- quality_flags()
  for (i in seq_len(nrow(tbl))) {
    expect_null(quality_caller(NULL, tbl$encoder[i]))
    expect_null(quality_caller(tbl$min[i], tbl$encoder[i]))
    expect_null(quality_caller(tbl$max[i], tbl$encoder[i]))
    expect_null(quality_caller((tbl$min[i] + tbl$max[i]) / 2, tbl$encoder[i]))
  }
})

test_that("check_quality() refuses a value that is not one finite number", {
  for (bad in list("23", c(20, 23), NA_real_, Inf, -Inf, NaN, TRUE, list(23))) {
    expect_error(quality_caller(bad, "libx264"), class = "rlang_error")
    expect_error(quality_caller(bad, "libx264"), "quality")
  }
  # The refusal names the caller's frame, never check_quality().
  err <- rlang::catch_cnd(quality_caller("23", "libx264"))
  expect_identical(rlang::call_name(err$call), "quality_caller")
})

test_that("check_quality() refuses a value outside the row's range", {
  tbl <- quality_flags()
  for (i in seq_len(nrow(tbl))) {
    below <- tbl$min[i] - 1
    above <- tbl$max[i] + 1
    for (bad in c(below, above)) {
      err <- rlang::catch_cnd(quality_caller(bad, tbl$encoder[i]))
      expect_s3_class(err, "rlang_error")
      msg <- conditionMessage(err)
      expect_match(msg, tbl$encoder[i], fixed = TRUE)
      expect_match(msg, tbl$flag[i], fixed = TRUE)
      expect_match(msg, as.character(tbl$min[i]), fixed = TRUE)
      expect_match(msg, as.character(tbl$max[i]), fixed = TRUE)
      expect_identical(rlang::call_name(err$call), "quality_caller")
    }
  }
})

test_that("check_quality() refuses an encoder the table lacks, naming it", {
  # An alias, a software encoder outside the table, and a hardware encoder no
  # backend row produces -- each refused by name, with `quality` set.
  for (enc in c("h264", "libvpx-vp9", "libaom-av1", "h264_qsv", "prores_ks")) {
    err <- rlang::catch_cnd(quality_caller(23, enc))
    expect_s3_class(err, "rlang_error")
    expect_match(conditionMessage(err), enc, fixed = TRUE)
    expect_match(conditionMessage(err), "quality", fixed = TRUE)
    expect_identical(rlang::call_name(err$call), "quality_caller")
    # NULL is never refused, whatever the encoder.
    expect_null(quality_caller(NULL, enc))
  }
})

test_that("check_quality() refuses `quality` with no encoder to key on", {
  # NULL encoder: `video_codec = NULL` under `hardware = "none"` leaves the
  # container's default encoder in charge, and no table row can be read.
  err <- rlang::catch_cnd(quality_caller(23, NULL))
  expect_s3_class(err, "rlang_error")
  expect_match(conditionMessage(err), "video_codec", fixed = TRUE)
  expect_identical(rlang::call_name(err$call), "quality_caller")
  expect_null(quality_caller(NULL, NULL))
  # "copy" stream-copies, so no encoder runs and no value can apply.
  err <- rlang::catch_cnd(quality_caller(23, "copy"))
  expect_s3_class(err, "rlang_error")
  expect_match(conditionMessage(err), "copy", fixed = TRUE)
  expect_identical(rlang::call_name(err$call), "quality_caller")
})
