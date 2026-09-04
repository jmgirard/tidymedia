# M106: what `video_codec = NULL` does under a hardware backend.
#
# NULL is M34/D016's "leave the codec alone" sentinel: no -codec:v is emitted
# and the output keeps its container's default encoder. resolve_hw_encoder()
# gives it its own branch under a backend -- it assumes the h264 family rather
# than inferring one from a codec name, and under `fallback = TRUE` it returns
# the sentinel rather than silently substituting libx264, which would change
# the codec behind the caller's back.
#
# Nothing in the suite asserted any of that. The probe grid pinned
# `video_codec` to "libx264" on every cell (M106 widened it), and the sentinel's
# own tests never turned `hardware` on. All three of these behaviours were
# therefore free to change without a red test.
#
# `standardize_video()` is the entry point rather than emit_video_codec(): the
# claim is about the compiled bytes a caller gets, and the seam half compiles
# no command of its own.

# The encoder pool is mocked, so no cell depends on what this machine's FFmpeg
# build lists; `check_tracks` is off so no cell spawns a binary either. The
# option seam is unset because hardware_encoder_available() reads it above
# cached_encoder_names() and would otherwise answer before the mock does.
local_sentinel_pool <- function(pool, env = parent.frame()) {
  withr::local_options(
    tidymedia.hardware_encoders = NULL,
    tidymedia.check_tracks = FALSE,
    .local_envir = env
  )
  local_mocked_bindings(cached_encoder_names = function() pool, .env = env)
}

sentinel_paths <- function(dir) {
  vid <- file.path(dir, "in.mp4")
  if (!file.exists(vid)) file.create(vid)
  list(infile = vid, outfile = file.path(dir, "out.mp4"))
}

test_that("the sentinel falls back to the container default, not to a codec", {
  dir <- withr::local_tempdir()
  p <- sentinel_paths(dir)
  local_sentinel_pool(character())

  expect_message(
    cmd <- standardize_video(p$infile, p$outfile, video_codec = NULL,
                             hardware = "nvenc", fallback = TRUE, run = FALSE),
    "falling back to the output container's default video encoder",
    fixed = TRUE
  )
  # No -codec:v at all: the fallback keeps the sentinel rather than injecting
  # libx264, which is the branch resolve_hw_encoder() takes only for NULL.
  expect_false(grepl("-codec:v", cmd, fixed = TRUE))
  # And the command is otherwise a real one, so the expectation above is not
  # passing on an empty string.
  expect_true(grepl("-movflags +faststart", cmd, fixed = TRUE))
})

test_that("the sentinel is refused by name when the encoder is missing", {
  dir <- withr::local_tempdir()
  p <- sentinel_paths(dir)
  local_sentinel_pool(character())

  # h264_nvenc, from the h264 family the sentinel branch assumes -- the refusal
  # names an encoder the caller never spelled, which is the thing worth pinning.
  expect_error(
    standardize_video(p$infile, p$outfile, video_codec = NULL,
                      hardware = "nvenc", fallback = FALSE, run = FALSE),
    "h264_nvenc",
    fixed = TRUE
  )
  expect_error(
    standardize_video(p$infile, p$outfile, video_codec = NULL,
                      hardware = "nvenc", fallback = FALSE, run = FALSE),
    "is not available",
    fixed = TRUE
  )
})

test_that("the sentinel takes the hardware encoder when the build lists it", {
  dir <- withr::local_tempdir()
  p <- sentinel_paths(dir)
  local_sentinel_pool(c("h264_nvenc", "hevc_nvenc", "av1_nvenc"))

  cmd <- standardize_video(p$infile, p$outfile, video_codec = NULL,
                           hardware = "nvenc", fallback = FALSE, run = FALSE)
  expect_true(grepl("-codec:v h264_nvenc", cmd, fixed = TRUE))
})

test_that("the pool is what decides, and hardware = \"none\" ignores it", {
  # The discrimination check for the three tests above: the same call with the
  # same sentinel emits no -codec:v when no backend is asked for, whichever
  # pool is mocked. A suite where the pool were not being read would pass the
  # tests above and this one identically.
  dir <- withr::local_tempdir()
  p <- sentinel_paths(dir)
  local_sentinel_pool(c("h264_nvenc", "hevc_nvenc", "av1_nvenc"))

  cmd <- standardize_video(p$infile, p$outfile, video_codec = NULL,
                           hardware = "none", run = FALSE)
  expect_false(grepl("-codec:v", cmd, fixed = TRUE))
})
