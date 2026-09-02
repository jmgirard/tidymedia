# The AC1 sweep for the four candidates D014's pre-0.2.0 window held open,
# decided by D077: (a) `audio` -> `audio_input` on the four fan-in verbs;
# (b) `check_tracks =` and (c) `timeout =` declined, so no verb carries either;
# (d) the hardware helpers and their option take backend-neutral names. Each
# procedure is scoped to the names its candidate puts under test and enumerates
# its domain from the namespace rather than recalling it.

tm_exports <- function() sort(getNamespaceExports("tidymedia"))

# Exported functions whose formals include `name`.
verbs_with_formal <- function(name) {
  ns <- asNamespace("tidymedia")
  hits <- Filter(function(fn) {
    obj <- get(fn, envir = ns)
    is.function(obj) && name %in% names(formals(obj))
  }, tm_exports())
  sort(hits)
}

test_that("(a) `audio_input` is the input index on exactly the four fan-in verbs", {
  expect_identical(
    verbs_with_formal("audio_input"),
    c("compare_videos", "compare_videos_batch",
      "picture_in_picture", "picture_in_picture_batch")
  )
  # The bare name survives only at Layer 1, where it is a codec string and a
  # logical, never an index (D077).
  expect_identical(verbs_with_formal("audio"), c("ffm_codec", "ffm_copy"))
})

test_that("(a) `audio_stream` keeps its verb set", {
  # D032's count, re-confirmed by D077: the track index is unchanged.
  expect_length(verbs_with_formal("audio_stream"), 18L)
})

test_that("(b) and (c) add no formal: no verb takes `check_tracks` or `timeout`", {
  expect_identical(verbs_with_formal("check_tracks"), character(0))
  expect_identical(verbs_with_formal("timeout"), character(0))
})

test_that("(d) the hardware-encoder exports carry backend-neutral names", {
  hits <- grep("nvenc|cuda|gpu|videotoolbox|qsv|vaapi|amf|hardware",
               tm_exports(), ignore.case = TRUE, value = TRUE)
  expect_identical(sort(hits), c("hardware_encoder", "has_hardware_encoder"))
})

test_that("(d) hardware_encoder() maps a family to its encoder name", {
  expect_identical(hardware_encoder("h264", "nvenc"), "h264_nvenc")
  expect_identical(hardware_encoder("hevc", "nvenc"), "hevc_nvenc")
  expect_identical(hardware_encoder("av1", "nvenc"), "av1_nvenc")
  expect_error(hardware_encoder("vp9", "nvenc"), "must be one of")
})

test_that("(d) has_hardware_encoder() answers from `tidymedia.hardware_encoders`", {
  # The option seam is read before FFmpeg is asked (D044), so no binary is
  # needed to exercise it.
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
  expect_true(has_hardware_encoder("h264", "nvenc"))
  expect_false(has_hardware_encoder("hevc", "nvenc"))
  withr::local_options(tidymedia.hardware_encoders = character(0))
  expect_false(has_hardware_encoder("h264", "nvenc"))
})

test_that("(d) the old option name is no longer read", {
  # Only the retired name is set, so a read of it would answer TRUE.
  withr::local_options(tidymedia.hardware_encoders = character(0),
                       tidymedia.nvenc_encoders = "h264_nvenc")
  expect_false(has_hardware_encoder("h264", "nvenc"))
})

test_that("(a) a leftover `audio` jobs column is unread, so its rows fall back to the default", {
  # The NEWS entry's claim: no shim reads the old column name, and the batch
  # verb does not refuse it; both rows compile as `audio_input = NULL` would.
  f1 <- make_input(); f2 <- make_input()
  jobs <- tibble::tibble(
    inputs = list(c(f1, f2), c(f1, f2)),
    output = c("a.mp4", "b.mp4"),
    audio = c(1, 1)
  )
  res <- compare_videos_batch(jobs, run = FALSE)
  expect_no_match(res$command[[1]], ":a", fixed = TRUE)
  expect_no_match(res$command[[2]], ":a", fixed = TRUE)
  # The same table under the shipped column name carries the audio.
  jobs$audio_input <- jobs$audio
  jobs$audio <- NULL
  expect_match(compare_videos_batch(jobs, run = FALSE)$command[[1]],
               "-map \"1:a\"", fixed = TRUE)
})
