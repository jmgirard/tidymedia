# nvenc hardware-encoding helpers, resolver, and verb toggles (M31).
#
# Availability is simulated with the `tidymedia.nvenc_encoders` option seam that
# has_nvenc() consults, so every compile test here is binary-free (no GPU). The
# one real GPU encode is guarded by skip_if_no_nvenc().

# nvenc_encoder() -------------------------------------------------------------

test_that("nvenc_encoder() maps each family to its encoder name", {
  expect_equal(nvenc_encoder("h264"), "h264_nvenc")
  expect_equal(nvenc_encoder("hevc"), "hevc_nvenc")
  expect_equal(nvenc_encoder("av1"), "av1_nvenc")
  expect_equal(nvenc_encoder(), "h264_nvenc") # default is the first choice
})

test_that("nvenc_encoder() rejects an unknown family", {
  expect_error(nvenc_encoder("vp9"), class = "rlang_error")
})

# has_nvenc() -----------------------------------------------------------------

test_that("has_nvenc() reads the option-seam pool when set", {
  withr::local_options(tidymedia.nvenc_encoders = c("h264_nvenc", "av1_nvenc"))
  expect_true(has_nvenc("h264"))
  expect_false(has_nvenc("hevc"))
  expect_true(has_nvenc("av1"))
})

test_that("has_nvenc() returns a length-one logical against real FFmpeg", {
  skip_if_no_ffmpeg()
  out <- has_nvenc("h264")
  expect_type(out, "logical")
  expect_length(out, 1)
  expect_false(is.na(out))
})

# codec_family() / resolve_hw_encoder() ---------------------------------------

test_that("codec_family() infers the family from common codec names", {
  expect_equal(codec_family("libx264"), "h264")
  expect_equal(codec_family("h264"), "h264")
  expect_equal(codec_family("libx265"), "hevc")
  expect_equal(codec_family("hevc"), "hevc")
  expect_equal(codec_family("libaom-av1"), "av1")
  expect_error(codec_family("prores"), "No nvenc encoder")
})

test_that("resolve_hw_encoder() leaves the codec untouched for hardware none", {
  expect_equal(resolve_hw_encoder("libx264", "none"), "libx264")
  expect_equal(resolve_hw_encoder("libx265", "none"), "libx265")
})

test_that("resolve_hw_encoder() returns the nvenc encoder when available", {
  withr::local_options(tidymedia.nvenc_encoders = c("h264_nvenc", "hevc_nvenc"))
  expect_equal(resolve_hw_encoder("libx264", "nvenc"), "h264_nvenc")
  expect_equal(resolve_hw_encoder("libx265", "nvenc"), "hevc_nvenc")
})

test_that("resolve_hw_encoder() aborts when nvenc is unavailable", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  expect_error(resolve_hw_encoder("libx264", "nvenc"), "not available")
})

test_that("resolve_hw_encoder() falls back to software with a message", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  expect_message(
    out <- resolve_hw_encoder("libx264", "nvenc", fallback = TRUE),
    "falling back"
  )
  expect_equal(out, "libx264")
})

# standardize_video() ----------------------------------------------------------

test_that("standardize_video(hardware = 'nvenc') compiles to the nvenc encoder", {
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  f <- make_input()
  cmd <- standardize_video(f, "out.mp4", hardware = "nvenc", run = FALSE)
  expect_match(cmd, "-codec:v h264_nvenc", fixed = TRUE)
  expect_no_match(cmd, "libx264", fixed = TRUE)
})

test_that("standardize_video() default is software and free of nvenc", {
  f <- make_input()
  cmd <- standardize_video(f, "out.mp4", run = FALSE)
  expect_match(cmd, "-codec:v libx264", fixed = TRUE)
  expect_no_match(cmd, "nvenc", fixed = TRUE)
})

test_that("standardize_video(hardware = 'nvenc') aborts when unavailable", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  f <- make_input()
  expect_error(
    standardize_video(f, "out.mp4", hardware = "nvenc", run = FALSE),
    "not available"
  )
})

test_that("standardize_video() fallback re-encodes with the software codec", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  f <- make_input()
  expect_message(
    cmd <- standardize_video(
      f, "out.mp4",
      hardware = "nvenc", fallback = TRUE, run = FALSE
    ),
    "falling back"
  )
  expect_match(cmd, "-codec:v libx264", fixed = TRUE)
})

test_that("standardize_video() rejects an unknown hardware value", {
  f <- make_input()
  expect_error(
    standardize_video(f, "out.mp4", hardware = "gpu", run = FALSE),
    class = "rlang_error"
  )
})

# format_for_web() -------------------------------------------------------------

test_that("format_for_web(hardware = 'nvenc') compiles to h264_nvenc", {
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  f <- make_input()
  cmd <- format_for_web(f, "out.mp4", hardware = "nvenc", run = FALSE)
  expect_match(cmd, "-codec:v h264_nvenc -codec:a aac", fixed = TRUE)
  expect_match(cmd, "-movflags +faststart", fixed = TRUE)
})

test_that("format_for_web() fallback re-encodes with libx264", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  f <- make_input()
  expect_message(
    cmd <- format_for_web(
      f, "out.mp4",
      hardware = "nvenc", fallback = TRUE, run = FALSE
    ),
    "falling back"
  )
  expect_match(cmd, "-codec:v libx264 -codec:a aac", fixed = TRUE)
})

# batch siblings ---------------------------------------------------------------

test_that("standardize_video_batch(hardware = 'nvenc') applies nvenc per row", {
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"))
  res <- standardize_video_batch(jobs, hardware = "nvenc", run = FALSE)
  expect_true(all(grepl("-codec:v h264_nvenc", res$command, fixed = TRUE)))
})

test_that("format_for_web_batch(hardware = 'nvenc') applies nvenc per row", {
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"))
  res <- format_for_web_batch(jobs, hardware = "nvenc", run = FALSE)
  expect_true(all(grepl("-codec:v h264_nvenc", res$command, fixed = TRUE)))
})

# real GPU encode --------------------------------------------------------------

test_that("standardize_video(hardware = 'nvenc') writes a playable file", {
  skip_if_no_nvenc()
  skip_if_no_mediainfo()
  infile <- make_test_video()
  outfile <- withr::local_tempfile(fileext = ".mp4")
  standardize_video(infile, outfile, width = 64, height = 48, hardware = "nvenc")
  expect_true(file.exists(outfile))
  expect_gt(file.size(outfile), 0)
  expect_equal(get_width(outfile), 64)
})
