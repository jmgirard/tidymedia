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

# The NULL sentinel (M34/D016): "leave the codec alone". codec_family() errors
# on NULL, so the sentinel is resolved in its own branch before that call.

test_that("resolve_hw_encoder() passes the NULL sentinel through for hardware none", {
  expect_null(resolve_hw_encoder(NULL, "none"))
})

test_that("resolve_hw_encoder() resolves the NULL sentinel to the h264 family", {
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  expect_equal(resolve_hw_encoder(NULL, "nvenc"), "h264_nvenc")
})

test_that("resolve_hw_encoder() aborts on the NULL sentinel when nvenc is unavailable", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  expect_error(resolve_hw_encoder(NULL, "nvenc"), "not available")
})

test_that("resolve_hw_encoder() falls back from the NULL sentinel to the container default", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  expect_message(
    out <- resolve_hw_encoder(NULL, "nvenc", fallback = TRUE),
    "container"
  )
  # Never a silently injected libx264 -- the fallback keeps the sentinel.
  expect_null(out)
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

# anonymize_video() ------------------------------------------------------------

test_that("anonymize_video(hardware = 'nvenc') compiles to the nvenc encoder", {
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  f <- make_input()
  regions <- data.frame(x = 10, y = 10, width = 20, height = 20)
  cmd <- anonymize_video(f, "out.mp4", regions, hardware = "nvenc", run = FALSE)
  expect_match(cmd, "-codec:v h264_nvenc", fixed = TRUE)
  expect_no_match(cmd, "libx264", fixed = TRUE)
})

test_that("anonymize_video() default is software and free of nvenc", {
  f <- make_input()
  regions <- data.frame(x = 10, y = 10, width = 20, height = 20)
  cmd <- anonymize_video(f, "out.mp4", regions, run = FALSE)
  expect_match(cmd, "-codec:v libx264", fixed = TRUE)
  expect_no_match(cmd, "nvenc", fixed = TRUE)
})

test_that("anonymize_video(hardware = 'nvenc') respects the video_codec family", {
  withr::local_options(tidymedia.nvenc_encoders = c("h264_nvenc", "hevc_nvenc"))
  f <- make_input()
  regions <- data.frame(x = 10, y = 10, width = 20, height = 20)
  cmd <- anonymize_video(f, "out.mp4", regions, video_codec = "libx265",
                         hardware = "nvenc", run = FALSE)
  expect_match(cmd, "-codec:v hevc_nvenc", fixed = TRUE)
})

test_that("anonymize_video(hardware = 'nvenc') aborts for a non-nvenc codec family", {
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  f <- make_input()
  regions <- data.frame(x = 10, y = 10, width = 20, height = 20)
  expect_error(
    anonymize_video(f, "out.mp4", regions, video_codec = "prores",
                    hardware = "nvenc", run = FALSE),
    "No nvenc encoder"
  )
})

test_that("anonymize_video(hardware = 'nvenc') aborts when unavailable", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  f <- make_input()
  regions <- data.frame(x = 10, y = 10, width = 20, height = 20)
  expect_error(
    anonymize_video(f, "out.mp4", regions, hardware = "nvenc", run = FALSE),
    "not available"
  )
})

test_that("anonymize_video() fallback re-encodes with the software codec", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  f <- make_input()
  regions <- data.frame(x = 10, y = 10, width = 20, height = 20)
  expect_message(
    cmd <- anonymize_video(f, "out.mp4", regions, hardware = "nvenc",
                           fallback = TRUE, run = FALSE),
    "falling back"
  )
  expect_match(cmd, "-codec:v libx264", fixed = TRUE)
})

test_that("anonymize_video() rejects an unknown hardware value", {
  f <- make_input()
  regions <- data.frame(x = 10, y = 10, width = 20, height = 20)
  expect_error(
    anonymize_video(f, "out.mp4", regions, hardware = "gpu", run = FALSE),
    class = "rlang_error"
  )
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

test_that("anonymize_video_batch(hardware = 'nvenc') applies nvenc per row", {
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  f <- make_input()
  jobs <- tibble::tibble(
    input   = c(f, f),
    output  = c("a.mp4", "b.mp4"),
    regions = list(
      data.frame(x = 10, y = 10, width = 20, height = 20),
      data.frame(x = 30, y = 30, width = 20, height = 20)
    )
  )
  res <- anonymize_video_batch(jobs, hardware = "nvenc", run = FALSE)
  expect_true(all(grepl("-codec:v h264_nvenc", res$command, fixed = TRUE)))
})

test_that("anonymize_video_batch() ignores a per-row hardware column (batch-wide)", {
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  f <- make_input()
  jobs <- tibble::tibble(
    input    = c(f, f),
    output   = c("a.mp4", "b.mp4"),
    hardware = c("nvenc", "nvenc"),
    regions  = list(
      data.frame(x = 10, y = 10, width = 20, height = 20),
      data.frame(x = 30, y = 30, width = 20, height = 20)
    )
  )
  # hardware is batch-wide, not a per-row column: the jobs column is ignored,
  # so with the scalar default (hardware = "none") every row stays software.
  res <- anonymize_video_batch(jobs, run = FALSE)
  expect_true(all(grepl("-codec:v libx264", res$command, fixed = TRUE)))
  expect_false(any(grepl("nvenc", res$command, fixed = TRUE)))
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

test_that("anonymize_video(hardware = 'nvenc') writes a non-empty file", {
  skip_if_no_nvenc()
  infile <- make_test_video()
  outfile <- withr::local_tempfile(fileext = ".mp4")
  regions <- data.frame(x = 0, y = 0, width = 16, height = 16)
  anonymize_video(infile, outfile, regions, hardware = "nvenc")
  expect_true(file.exists(outfile))
  expect_gt(file.size(outfile), 0)
})

test_that("separate_audio_video(hardware = 'nvenc') writes both outputs (M38)", {
  # skip_if_no_nvenc() probes a real 1-frame encode rather than trusting the
  # encoder list, which CI populates with no GPU behind it (M31 lesson).
  skip_if_no_nvenc()
  skip_if_no_ffprobe()
  infile <- make_test_video()
  dir <- withr::local_tempdir()
  audiofile <- file.path(dir, "a.m4a")
  videofile <- file.path(dir, "v.mp4")
  # video_codec = NULL, not the "copy" default: a stream copy runs no encoder,
  # so the sentinel is what actually hands this stream to the GPU.
  separate_audio_video(infile, audiofile, videofile, video_codec = NULL,
                       hardware = "nvenc")
  expect_true(file.exists(audiofile))
  expect_gt(file.size(videofile), 0)
  # The video really went through nvenc (h264), and the audio kept its default
  # copy -- the two streams stayed independent end to end.
  expect_equal(probe_video(infile = videofile)$codec_name, "h264")
  expect_equal(probe_audio(infile = audiofile)$codec_name,
               probe_audio(infile = infile)$codec_name)
})
