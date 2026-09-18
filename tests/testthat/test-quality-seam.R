# M135: the `quality` seam -- emit_video_codec() checks the value against the
# encoder the call WOULD use before the availability probe, emits the flag
# through ffm_output_options() after the resolver answers, and drops it with a
# message when the resolver fell back. Tested at the seam with a bare pipeline
# object, because the claims are about the seam's own ordering; the eight
# verbs' compiled commands are the grid in test-quality-grid.R.

# The encoder pool is set both ways through the option seam (LESSONS M094): a
# green suite on a machine with no GPU says nothing about the arm that runs
# when the encoder is present.
local_quality_pool <- function(pool, env = parent.frame()) {
  withr::local_options(
    tidymedia.hardware_encoders = pool,
    tidymedia.check_tracks = FALSE,
    .local_envir = env
  )
}

all_hw_encoders <- function() {
  c("h264_nvenc", "hevc_nvenc", "av1_nvenc",
    "h264_videotoolbox", "hevc_videotoolbox")
}

seam_pipeline <- function(dir) {
  vid <- file.path(dir, "in.mp4")
  if (!file.exists(vid)) file.create(vid)
  ffm_files(vid, file.path(dir, "out.mp4"))
}

# A frame to blame, standing in for the verb.
seam_caller <- function(p, video_codec, hardware = "none", fallback = FALSE,
                        quality = NULL) {
  emit_video_codec(p, video_codec, hardware, fallback, quality,
                   call = rlang::current_env())
}

# The flag must follow `-codec:v <encoder>` in the compiled bytes.
expect_flag_after_codec <- function(cmd, encoder, flag, value) {
  codec_at <- regexpr(paste0("-codec:v ", encoder), cmd, fixed = TRUE)
  flag_at <- regexpr(paste(flag, value), cmd, fixed = TRUE)
  expect_gt(codec_at, 0)
  expect_gt(flag_at, codec_at)
}

expect_no_quality_flag <- function(cmd) {
  expect_false(grepl("-crf", cmd, fixed = TRUE))
  expect_false(grepl("-cq", cmd, fixed = TRUE))
  expect_false(grepl("-q:v", cmd, fixed = TRUE))
}

test_that("every table row emits its flag after the codec, and NULL emits none", {
  dir <- withr::local_tempdir()
  local_quality_pool(all_hw_encoders())
  # (video_codec, hardware) pairs that resolve to each row's encoder.
  pairs <- list(
    libx264 = c("libx264", "none"),
    libx265 = c("libx265", "none"),
    h264_nvenc = c("libx264", "nvenc"),
    hevc_nvenc = c("libx265", "nvenc"),
    av1_nvenc = c("libaom-av1", "nvenc"),
    h264_videotoolbox = c("libx264", "videotoolbox"),
    hevc_videotoolbox = c("libx265", "videotoolbox")
  )
  tbl <- quality_flags()
  expect_setequal(names(pairs), tbl$encoder)
  for (enc in tbl$encoder) {
    row <- tbl[tbl$encoder == enc, ]
    value <- row$min + 1
    p <- seam_pipeline(dir)
    cmd <- ffm_compile(seam_caller(p, pairs[[enc]][1], pairs[[enc]][2],
                                   quality = value))
    expect_flag_after_codec(cmd, enc, row$flag, value)
    cmd0 <- ffm_compile(seam_caller(p, pairs[[enc]][1], pairs[[enc]][2],
                                    quality = NULL))
    expect_true(grepl(paste0("-codec:v ", enc), cmd0, fixed = TRUE))
    expect_no_quality_flag(cmd0)
  }
})

test_that("a decimal value passes through unchanged", {
  dir <- withr::local_tempdir()
  p <- seam_pipeline(dir)
  cmd <- ffm_compile(seam_caller(p, "libx264", quality = 22.5))
  expect_flag_after_codec(cmd, "libx264", "-crf", "22.5")
})

test_that("the sentinel under a backend takes the H.264 row", {
  dir <- withr::local_tempdir()
  local_quality_pool(all_hw_encoders())
  p <- seam_pipeline(dir)
  cmd <- ffm_compile(seam_caller(p, NULL, "nvenc", quality = 30))
  expect_flag_after_codec(cmd, "h264_nvenc", "-cq", 30)
  cmd <- ffm_compile(seam_caller(p, NULL, "videotoolbox", quality = 30))
  expect_flag_after_codec(cmd, "h264_videotoolbox", "-q:v", 30)
})

test_that("a wrong value is refused before the availability probe", {
  dir <- withr::local_tempdir()
  p <- seam_pipeline(dir)
  count <- local_encoder_probe_counter(names = all_hw_encoders())
  # Shape, range, and an encoder the table lacks -- each with zero probes.
  err <- rlang::catch_cnd(seam_caller(p, "libx264", "nvenc", quality = "23"))
  expect_s3_class(err, "rlang_error")
  expect_identical(rlang::call_name(err$call), "seam_caller")
  err <- rlang::catch_cnd(seam_caller(p, "libx264", "nvenc", quality = 52))
  expect_s3_class(err, "rlang_error")
  expect_match(conditionMessage(err), "h264_nvenc", fixed = TRUE)
  expect_identical(rlang::call_name(err$call), "seam_caller")
  err <- rlang::catch_cnd(seam_caller(p, "libvpx-vp9", "none", quality = 23))
  expect_s3_class(err, "rlang_error")
  expect_match(conditionMessage(err), "libvpx-vp9", fixed = TRUE)
  expect_identical(rlang::call_name(err$call), "seam_caller")
  expect_identical(count(), 0L)
  # The same range refusal under fallback = TRUE: the value is checked against
  # the encoder the call asked for, never against what the machine has.
  err <- rlang::catch_cnd(seam_caller(p, "libx264", "videotoolbox",
                                      fallback = TRUE, quality = 0))
  expect_s3_class(err, "rlang_error")
  expect_match(conditionMessage(err), "h264_videotoolbox", fixed = TRUE)
  expect_identical(count(), 0L)
  # And a good value under a backend probes exactly once.
  seam_caller(p, "libx264", "nvenc", quality = 23)
  expect_identical(count(), 1L)
})

test_that("a fallback drops the value and the message says so", {
  dir <- withr::local_tempdir()
  local_quality_pool(character())
  p <- seam_pipeline(dir)
  for (hw in c("nvenc", "videotoolbox")) {
    enc <- tm_hardware_encoder("h264", hw, call = rlang::current_env())
    msgs <- character()
    cmd <- withCallingHandlers(
      ffm_compile(seam_caller(p, "libx264", hw, fallback = TRUE, quality = 20)),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    expect_length(msgs, 1L)
    expect_match(msgs, "is not available", fixed = TRUE)
    expect_match(msgs, "`quality` = 20 is dropped", fixed = TRUE)
    expect_match(msgs, enc, fixed = TRUE)
    expect_true(grepl("-codec:v libx264", cmd, fixed = TRUE))
    expect_no_quality_flag(cmd)
  }
  # The sentinel's fallback says the same.
  msgs <- character()
  cmd <- withCallingHandlers(
    ffm_compile(seam_caller(p, NULL, "nvenc", fallback = TRUE, quality = 20)),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_match(msgs, "`quality` = 20 is dropped", fixed = TRUE)
  expect_false(grepl("-codec:v", cmd, fixed = TRUE))
  expect_no_quality_flag(cmd)
})

test_that("a fallback with quality = NULL keeps its old one-line message", {
  dir <- withr::local_tempdir()
  local_quality_pool(character())
  p <- seam_pipeline(dir)
  msgs <- character()
  withCallingHandlers(
    seam_caller(p, "libx264", "nvenc", fallback = TRUE, quality = NULL),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_length(msgs, 1L)
  expect_false(grepl("quality", msgs, fixed = TRUE))
})

test_that("resolve_hw_encoder() keeps its one-string contract", {
  local_quality_pool(all_hw_encoders())
  expect_identical(resolve_hw_encoder("libx264", "nvenc"), "h264_nvenc")
  expect_identical(resolve_hw_encoder("libx264", "none"), "libx264")
  expect_null(resolve_hw_encoder(NULL, "none"))
  info <- resolve_hw_encoder_info("libx264", "nvenc")
  expect_identical(info, list(encoder = "h264_nvenc", fell_back = FALSE))
  local_quality_pool(character())
  info <- suppressMessages(resolve_hw_encoder_info("libx264", "nvenc", TRUE))
  expect_identical(info, list(encoder = "libx264", fell_back = TRUE))
})
