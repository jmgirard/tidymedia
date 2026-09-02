# The backend vocabulary: two backends, one per-backend codec-family table, and
# one availability probe reached by two routes.
#
# Everything below the executing test at the bottom is binary-free: availability
# is simulated, so a runner with neither an NVIDIA GPU nor a Mac still decides
# every compile, refusal and fallback assertion here.

# A software codec name per family, so the table can be iterated without
# hand-writing a call per (family, backend) cell.
hw_software_codec <- function(family) {
  c(h264 = "libx264", hevc = "libx265", av1 = "libaom-av1",
    prores = "prores")[[family]]
}

# AC2 -- the table decides what is emitted --------------------------------------

test_that("each backend compiles its own encoder for every family it declares", {
  table <- tidymedia:::hardware_backend_families()
  # Iterated, never hand-listed: a family added to a table without a builder
  # case fails here rather than shipping.
  for (backend in names(table)) {
    for (family in table[[backend]]) {
      encoder <- paste0(family, "_", backend)
      withr::local_options(tidymedia.hardware_encoders = encoder)
      f <- make_input()
      cmd <- standardize_video(f, "out.mp4",
                               video_codec = hw_software_codec(family),
                               hardware = backend, run = FALSE)
      expect_match(cmd, paste0("-codec:v ", encoder), fixed = TRUE,
                   info = encoder)
      expect_no_match(cmd, hw_software_codec(family), fixed = TRUE,
                      info = encoder)
    }
  }
})

test_that("both backends declare the two families they share", {
  # The iteration above is only as strong as the table it reads, so the two
  # families both backends cover are asserted by name as well.
  table <- tidymedia:::hardware_backend_families()
  expect_true(all(c("h264", "hevc") %in% table$nvenc))
  expect_true(all(c("h264", "hevc") %in% table$videotoolbox))
})

test_that("a family outside a backend's table is refused at the verb", {
  # Each refusal names the backend the caller asked for and the family, and
  # neither names the other backend.
  withr::local_options(
    tidymedia.hardware_encoders = c("h264_nvenc", "hevc_nvenc", "av1_nvenc",
                                    "h264_videotoolbox", "hevc_videotoolbox")
  )
  f <- make_input()

  vt <- rlang::catch_cnd(
    standardize_video(f, "out.mp4", video_codec = "libaom-av1",
                      hardware = "videotoolbox", run = FALSE)
  )
  expect_s3_class(vt, "rlang_error")
  expect_match(conditionMessage(vt), "videotoolbox", fixed = TRUE)
  expect_match(conditionMessage(vt), "av1", fixed = TRUE)
  expect_no_match(conditionMessage(vt), "nvenc", fixed = TRUE)

  nv <- rlang::catch_cnd(
    standardize_video(f, "out.mp4", video_codec = "prores",
                      hardware = "nvenc", run = FALSE)
  )
  expect_s3_class(nv, "rlang_error")
  expect_match(conditionMessage(nv), "nvenc", fixed = TRUE)
  expect_match(conditionMessage(nv), "prores", fixed = TRUE)
  expect_no_match(conditionMessage(nv), "videotoolbox", fixed = TRUE)
})

# AC3 -- the two routes to the probe's answer -----------------------------------
#
# The option seam is read before the session memo, so a test that mocks the memo
# without unsetting the option measures the option (D044). The two routes are
# asserted separately for that reason, and because the option is the seam
# carried into `parallel = TRUE` workers.

# One (backend available, backend missing) pair, against whatever pool the
# calling test has already put in place.
hw_expect_pool_decides <- function(available, missing) {
  f <- make_input()
  cmd <- standardize_video(f, "out.mp4", hardware = available, run = FALSE)
  expect_match(cmd, paste0("-codec:v h264_", available), fixed = TRUE,
               info = available)

  cnd <- rlang::catch_cnd(
    standardize_video(f, "out.mp4", hardware = missing, run = FALSE)
  )
  expect_s3_class(cnd, "rlang_error")
  expect_match(conditionMessage(cnd), paste0("h264_", missing), fixed = TRUE,
               info = missing)
}

test_that("the memo route decides which backend proceeds and which aborts", {
  # The option MUST be unset, or the mock below is inert: the option seam is
  # read first and the memo is only the fall-through.
  withr::local_options(tidymedia.hardware_encoders = NULL)
  forget_ffmpeg_capabilities()
  withr::defer(forget_ffmpeg_capabilities())

  local_mocked_bindings(cached_encoder_names = function() "h264_videotoolbox",
                        .package = "tidymedia")
  hw_expect_pool_decides(available = "videotoolbox", missing = "nvenc")
})

test_that("the reverse memo pool inverts both answers", {
  withr::local_options(tidymedia.hardware_encoders = NULL)
  forget_ffmpeg_capabilities()
  withr::defer(forget_ffmpeg_capabilities())

  local_mocked_bindings(cached_encoder_names = function() "h264_nvenc",
                        .package = "tidymedia")
  hw_expect_pool_decides(available = "nvenc", missing = "videotoolbox")
})

test_that("the option route decides the same outcomes with no mock", {
  withr::local_options(tidymedia.hardware_encoders = "h264_videotoolbox")
  hw_expect_pool_decides(available = "videotoolbox", missing = "nvenc")
})

test_that("the reverse option pool inverts both answers", {
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
  hw_expect_pool_decides(available = "nvenc", missing = "videotoolbox")
})

test_that("fallback re-encodes in software and names the backend it left", {
  f <- make_input()
  for (backend in tidymedia:::hardware_backends()) {
    withr::local_options(tidymedia.hardware_encoders = character(0))
    msg <- rlang::catch_cnd(
      standardize_video(f, "out.mp4", hardware = backend, fallback = TRUE,
                        run = FALSE),
      classes = "message"
    )
    expect_s3_class(msg, "message")
    expect_match(conditionMessage(msg), backend, fixed = TRUE, info = backend)
    expect_match(conditionMessage(msg), "libx264", fixed = TRUE, info = backend)

    cmd <- withCallingHandlers(
      standardize_video(f, "out.mp4", hardware = backend, fallback = TRUE,
                        run = FALSE),
      message = function(m) invokeRestart("muffleMessage")
    )
    expect_match(cmd, "-codec:v libx264", fixed = TRUE, info = backend)
  }
})

test_that("a missing backend in a _batch call is blamed on the verb", {
  # The front-door gate is membership in the backend set, never a test against
  # one backend's name: under the old gate a videotoolbox batch call returned
  # early and the abort surfaced from purrr::pmap() instead of the verb.
  withr::local_options(tidymedia.hardware_encoders = character(0))
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "out.mp4")
  for (backend in tidymedia:::hardware_backends()) {
    cnd <- rlang::catch_cnd(
      standardize_video_batch(jobs, hardware = backend, run = FALSE)
    )
    expect_s3_class(cnd, "rlang_error")
    expect_identical(deparse(conditionCall(cnd))[[1]],
                     "standardize_video_batch(jobs, hardware = backend, run = FALSE)",
                     info = backend)
  }
})

# AC5 -- the exported helper answers for either backend --------------------------

test_that("has_hardware_encoder() answers per backend under either pool", {
  withr::local_options(tidymedia.hardware_encoders = "h264_videotoolbox")
  expect_true(has_hardware_encoder("h264", "videotoolbox"))
  expect_false(has_hardware_encoder("h264", "nvenc"))

  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
  expect_true(has_hardware_encoder("h264", "nvenc"))
  expect_false(has_hardware_encoder("h264", "videotoolbox"))
})

# AC4 -- the videotoolbox path executed, not only compiled -----------------------

test_that("videotoolbox encodes a real file", {
  skip_if_no_videotoolbox()
  infile <- withr::local_tempfile(fileext = ".mp4")
  outfile <- withr::local_tempfile(fileext = ".mp4")
  # A synthetic input, so the test carries no fixture and no provenance debt.
  system2("ffmpeg",
          c("-hide_banner", "-loglevel", "error", "-y", "-f", "lavfi",
            "-i", "testsrc=s=320x240:d=1", "-c:v", "libx264",
            shQuote(infile)),
          stdout = FALSE, stderr = FALSE)
  skip_if_not(file.exists(infile) && file.size(infile) > 0,
              message = "could not build a test input")

  standardize_video(infile, outfile, width = 160, height = 120,
                    hardware = "videotoolbox")

  expect_true(file.exists(outfile))
  expect_gt(file.size(outfile), 0)
  expect_equal(get_width(outfile), 160)
})
