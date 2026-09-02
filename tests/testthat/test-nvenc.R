# nvenc hardware-encoding helpers, resolver, and verb toggles (M31).
#
# Availability is simulated with the `tidymedia.hardware_encoders` option seam that
# has_hardware_encoder() consults, so every compile test here is binary-free (no GPU). The
# one real GPU encode is guarded by skip_if_no_nvenc().

# hardware_encoder() -------------------------------------------------------------

test_that("hardware_encoder() maps each family to its encoder name", {
  expect_equal(hardware_encoder("h264", "nvenc"), "h264_nvenc")
  expect_equal(hardware_encoder("hevc", "nvenc"), "hevc_nvenc")
  expect_equal(hardware_encoder("av1", "nvenc"), "av1_nvenc")
  expect_equal(hardware_encoder("h264", "videotoolbox"), "h264_videotoolbox")
  expect_equal(hardware_encoder("hevc", "videotoolbox"), "hevc_videotoolbox")
})

test_that("hardware_encoder() rejects an unknown family", {
  expect_error(hardware_encoder("vp9", "nvenc"), class = "rlang_error")
})

test_that("hardware_encoder() requires a backend and refuses the off position", {
  # `hardware` has no default: every candidate default is one member of the set
  # it ranges over, and a member as a silent default is the same defect the
  # backend-neutral NAME removed, moved into a value (D079).
  expect_error(hardware_encoder("h264"), class = "rlang_error")
  expect_error(hardware_encoder("h264", "none"), "must be one of")
})

test_that("hardware_encoder() refuses a family its backend has no encoder for", {
  # The (family, backend) refusal, sited in the mapper and nowhere else. Each
  # abort names the backend the caller asked for and the family, and neither
  # names the other backend.
  vt <- rlang::catch_cnd(hardware_encoder("av1", "videotoolbox"))
  expect_s3_class(vt, "rlang_error")
  expect_match(conditionMessage(vt), "videotoolbox", fixed = TRUE)
  expect_match(conditionMessage(vt), "av1", fixed = TRUE)
  expect_no_match(conditionMessage(vt), "nvenc", fixed = TRUE)

  nv <- rlang::catch_cnd(hardware_encoder("prores", "nvenc"))
  expect_s3_class(nv, "rlang_error")
  expect_match(conditionMessage(nv), "nvenc", fixed = TRUE)
  expect_match(conditionMessage(nv), "prores", fixed = TRUE)
  expect_no_match(conditionMessage(nv), "videotoolbox", fixed = TRUE)
})

# has_hardware_encoder() -----------------------------------------------------------------

test_that("has_hardware_encoder() reads the option-seam pool when set", {
  withr::local_options(tidymedia.hardware_encoders = c("h264_nvenc", "av1_nvenc"))
  expect_true(has_hardware_encoder("h264", "nvenc"))
  expect_false(has_hardware_encoder("hevc", "nvenc"))
  expect_true(has_hardware_encoder("av1", "nvenc"))
})

test_that("has_hardware_encoder() returns a length-one logical against real FFmpeg", {
  skip_if_no_ffmpeg()
  out <- has_hardware_encoder("h264", "nvenc")
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
  # Backend-free: prores IS a family, and whether a backend encodes it is the
  # table's question, refused in hardware_encoder(). The abort here fires only
  # when no family matches at all, and names no backend.
  expect_equal(codec_family("prores"), "prores")
  cnd <- rlang::catch_cnd(codec_family("vp9"))
  expect_s3_class(cnd, "rlang_error")
  expect_match(conditionMessage(cnd), "No hardware encoder family")
  expect_no_match(conditionMessage(cnd), "nvenc", fixed = TRUE)
  expect_no_match(conditionMessage(cnd), "videotoolbox", fixed = TRUE)
})

test_that("resolve_hw_encoder() leaves the codec untouched for hardware none", {
  expect_equal(resolve_hw_encoder("libx264", "none"), "libx264")
  expect_equal(resolve_hw_encoder("libx265", "none"), "libx265")
})

test_that("resolve_hw_encoder() returns the nvenc encoder when available", {
  withr::local_options(tidymedia.hardware_encoders = c("h264_nvenc", "hevc_nvenc"))
  expect_equal(resolve_hw_encoder("libx264", "nvenc"), "h264_nvenc")
  expect_equal(resolve_hw_encoder("libx265", "nvenc"), "hevc_nvenc")
})

test_that("resolve_hw_encoder() aborts when nvenc is unavailable", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
  expect_error(resolve_hw_encoder("libx264", "nvenc"), "not available")
})

test_that("resolve_hw_encoder() falls back to software with a message", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
  expect_message(
    out <- resolve_hw_encoder("libx264", "nvenc", fallback = TRUE),
    "falling back"
  )
  expect_equal(out, "libx264")
})

# An nvenc-unavailable abort must name the VERB the user called, never the
# internal *_pipeline() helper (M41's blame convention; M54). resolve_hw_encoder()
# aborts with call = call, so a call site that omits `call =` blames itself.
# crop_video()/anonymize_video() thread it and are the discriminating controls:
# if this test ever passed for the wrong reason, they would fail too.
#
# Each case asserts WHICH failure it caught. Every verb here validates its own
# arguments with `call = call` too, so a malformed input aborts blaming the same
# front door and satisfies the expectation for a reason that has nothing to do
# with encoder resolution -- which is how this block's `regions` control used to
# pass while pinning nothing.

test_that("an nvenc-unavailable abort names the verb, not its pipeline helper", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
  infile <- withr::local_tempfile(fileext = ".mp4")
  file.create(infile)

  blamed <- function(expr) {
    err <- rlang::catch_cnd(expr, classes = "error")
    expect_match(
      paste(conditionMessage(err), collapse = " "),
      "h264_nvenc\" is not available"
    )
    deparse(conditionCall(err))[[1]]
  }

  expect_match(
    blamed(standardize_video(infile, "o.mp4", hardware = "nvenc", run = FALSE)),
    "^standardize_video\\("
  )
  expect_match(
    blamed(format_for_web(infile, "o.mp4", hardware = "nvenc", run = FALSE)),
    "^format_for_web\\("
  )
  # Controls: these two already passed `call =` before M54.
  expect_match(
    blamed(crop_video(infile, "o.mp4",
      width = 10, height = 10,
      hardware = "nvenc", run = FALSE
    )),
    "^crop_video\\("
  )
  expect_match(
    blamed(anonymize_video(infile, "o.mp4",
      regions = data.frame(x = 1, y = 1, width = 2, height = 2),
      hardware = "nvenc", run = FALSE
    )),
    "^anonymize_video\\("
  )
})

test_that("a fan-out call blames the verb, not the fan-out", {
  # Every verb taking `hardware` blames ITSELF for an unavailable encoder.
  # Threading `call =` into a pipeline reaches only a verb that calls that
  # pipeline DIRECTLY: a fan-out routes through ffm_batch() -> purrr::pmap(),
  # so caller_env() lands on the anonymous closure (LESSONS M47/M48-F1), and
  # until M57 these three read "purrr::pmap(jobs, .f, ...)". M57 gave each
  # fan-out verb a front-door guard (D035), so they now name themselves like
  # the scalar control below. The wider sweep over all nine fan-out verbs, and
  # the preconditions the guards mirror, live in test-nvenc-front-door.R.
  #
  # Every case asserts WHICH failure it caught, not merely that one happened: a
  # malformed jobs table aborts at the schema check, before any fan-out, and
  # reads as correct blame attribution if the message goes unchecked. That is
  # exactly how this test's previous control passed while pinning nothing.
  withr::local_options(tidymedia.hardware_encoders = character(0))
  infile <- withr::local_tempfile(fileext = ".mp4")
  file.create(infile)

  blamed <- function(expr) {
    err <- rlang::catch_cnd(expr, classes = "error")
    expect_match(
      paste(conditionMessage(err), collapse = " "),
      "h264_nvenc\" is not available"
    )
    deparse(conditionCall(err))[[1]]
  }

  expect_match(
    blamed(standardize_video_batch(
      tibble::tibble(input = infile, output = "o.mp4"),
      hardware = "nvenc", run = FALSE
    )),
    "^standardize_video_batch\\("
  )
  expect_match(
    blamed(segment_video(infile, 0, 5, "o.mp4",
      hardware = "nvenc", run = FALSE
    )),
    "^segment_video\\("
  )
  expect_match(
    blamed(picture_in_picture_batch(
      tibble::tibble(main = infile, overlay = infile, output = "o.mp4"),
      hardware = "nvenc", run = FALSE
    )),
    "^picture_in_picture_batch\\("
  )
  expect_match(
    blamed(standardize_video(infile, "o.mp4",
      hardware = "nvenc", run = FALSE
    )),
    "^standardize_video\\("
  )
})

# The NULL sentinel (M34/D016): "leave the codec alone". codec_family() errors
# on NULL, so the sentinel is resolved in its own branch before that call.

test_that("resolve_hw_encoder() passes the NULL sentinel through for hardware none", {
  expect_null(resolve_hw_encoder(NULL, "none"))
})

test_that("resolve_hw_encoder() resolves the NULL sentinel to the h264 family", {
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
  expect_equal(resolve_hw_encoder(NULL, "nvenc"), "h264_nvenc")
})

test_that("resolve_hw_encoder() aborts on the NULL sentinel when nvenc is unavailable", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
  expect_error(resolve_hw_encoder(NULL, "nvenc"), "not available")
})

test_that("resolve_hw_encoder() falls back from the NULL sentinel to the container default", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
  expect_message(
    out <- resolve_hw_encoder(NULL, "nvenc", fallback = TRUE),
    "container"
  )
  # Never a silently injected libx264 -- the fallback keeps the sentinel.
  expect_null(out)
})

# standardize_video() ----------------------------------------------------------

test_that("standardize_video(hardware = 'nvenc') compiles to the nvenc encoder", {
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
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
  withr::local_options(tidymedia.hardware_encoders = character(0))
  f <- make_input()
  expect_error(
    standardize_video(f, "out.mp4", hardware = "nvenc", run = FALSE),
    "not available"
  )
})

test_that("standardize_video() fallback re-encodes with the software codec", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
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
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
  f <- make_input()
  cmd <- format_for_web(f, "out.mp4", hardware = "nvenc", run = FALSE)
  expect_match(cmd, "-codec:v h264_nvenc -codec:a aac", fixed = TRUE)
  expect_match(cmd, "-movflags +faststart", fixed = TRUE)
})

test_that("format_for_web() fallback re-encodes with libx264", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
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
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
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
  withr::local_options(tidymedia.hardware_encoders = c("h264_nvenc", "hevc_nvenc"))
  f <- make_input()
  regions <- data.frame(x = 10, y = 10, width = 20, height = 20)
  cmd <- anonymize_video(f, "out.mp4", regions, video_codec = "libx265",
                         hardware = "nvenc", run = FALSE)
  expect_match(cmd, "-codec:v hevc_nvenc", fixed = TRUE)
})

test_that("anonymize_video(hardware = 'nvenc') aborts for a family nvenc lacks", {
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
  f <- make_input()
  regions <- data.frame(x = 10, y = 10, width = 20, height = 20)
  expect_error(
    anonymize_video(f, "out.mp4", regions, video_codec = "prores",
                    hardware = "nvenc", run = FALSE),
    'nvenc has no "prores" encoder'
  )
})

test_that("anonymize_video(hardware = 'nvenc') aborts when unavailable", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
  f <- make_input()
  regions <- data.frame(x = 10, y = 10, width = 20, height = 20)
  expect_error(
    anonymize_video(f, "out.mp4", regions, hardware = "nvenc", run = FALSE),
    "not available"
  )
})

test_that("anonymize_video() fallback re-encodes with the software codec", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
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
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"))
  res <- standardize_video_batch(jobs, hardware = "nvenc", run = FALSE)
  expect_true(all(grepl("-codec:v h264_nvenc", res$command, fixed = TRUE)))
})

test_that("format_for_web_batch(hardware = 'nvenc') applies nvenc per row", {
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"))
  res <- format_for_web_batch(jobs, hardware = "nvenc", run = FALSE)
  expect_true(all(grepl("-codec:v h264_nvenc", res$command, fixed = TRUE)))
})

test_that("anonymize_video_batch(hardware = 'nvenc') applies nvenc per row", {
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
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
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
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
  # MP3-in-MP4, not make_test_video(): the audio assertion below has to tell a
  # stream copy from a re-encode, and an AAC-in-MP4 source cannot, because the
  # container's own default encoder is also AAC (M35 lesson).
  infile <- make_mp3_audio_video()
  dir <- withr::local_tempdir()
  audiofile <- file.path(dir, "a.mp3")
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
  expect_equal(probe_audio(infile = audiofile)$codec_name, "mp3")
})
