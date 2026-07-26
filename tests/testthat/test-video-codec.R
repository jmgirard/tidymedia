# video_codec / hardware / fallback on the four codec-less re-encode verbs and
# their _batch siblings (M34, D016).
#
# `video_codec = NULL` is a "leave the codec alone" sentinel: no -codec:v is
# emitted, so the output keeps its container's default encoder and the compiled
# command is byte-identical to the pre-M34 one. nvenc availability is simulated
# with the `tidymedia.nvenc_encoders` option seam that has_nvenc() consults, so
# every compile test here is binary-free (no GPU); the execution tests are
# guarded by skip_if_no_nvenc().

# crop_video() ----------------------------------------------------------------

test_that("crop_video() default compiles the pre-M34 command byte-for-byte", {
  f <- make_input()
  cmd <- crop_video(f, "out.mp4", width = 100, height = 50, x = 0, y = 0,
                    run = FALSE)
  # The literal below is the command master compiled before M34 existed.
  expect_equal(
    as.character(cmd),
    paste0('-y -i "', f, '" -vf "crop=w=100:h=50:x=0:y=0" -map 0 "out.mp4"')
  )
  expect_no_match(as.character(cmd), "-codec:v", fixed = TRUE)
})

test_that("crop_video(video_codec = ) sets the codec", {
  f <- make_input()
  cmd <- crop_video(f, "out.mp4", width = 100, height = 50,
                    video_codec = "libx265", run = FALSE)
  expect_match(as.character(cmd), "-codec:v libx265", fixed = TRUE)
})

test_that("crop_video() rejects a non-token video_codec", {
  f <- make_input()
  expect_error(
    crop_video(f, "out.mp4", width = 100, height = 50,
               video_codec = "libx264 -evil", run = FALSE)
  )
  # Same rejection under nvenc, where family inference runs first.
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  expect_error(
    crop_video(f, "out.mp4", width = 100, height = 50,
               video_codec = "libx264 -evil", hardware = "nvenc", run = FALSE)
  )
})

test_that("crop_video(hardware = 'nvenc') resolves the sentinel to h264_nvenc", {
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  f <- make_input()
  cmd <- crop_video(f, "out.mp4", width = 100, height = 50,
                    hardware = "nvenc", run = FALSE)
  expect_match(as.character(cmd), "-codec:v h264_nvenc", fixed = TRUE)
})

test_that("crop_video(hardware = 'nvenc') follows an explicit codec's family", {
  withr::local_options(tidymedia.nvenc_encoders = "hevc_nvenc")
  f <- make_input()
  cmd <- crop_video(f, "out.mp4", width = 100, height = 50,
                    video_codec = "libx265", hardware = "nvenc", run = FALSE)
  expect_match(as.character(cmd), "-codec:v hevc_nvenc", fixed = TRUE)
})

test_that("crop_video(hardware = 'nvenc') aborts when nvenc is unavailable", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  f <- make_input()
  expect_error(
    crop_video(f, "out.mp4", width = 100, height = 50, hardware = "nvenc",
               run = FALSE),
    "not available"
  )
})

test_that("crop_video() fallback from the sentinel emits no codec at all", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  f <- make_input()
  expect_message(
    cmd <- crop_video(f, "out.mp4", width = 100, height = 50,
                      hardware = "nvenc", fallback = TRUE, run = FALSE),
    "container"
  )
  expect_no_match(as.character(cmd), "-codec:v", fixed = TRUE)
})

test_that("crop_video() fallback with an explicit codec keeps that codec", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  f <- make_input()
  expect_message(
    cmd <- crop_video(f, "out.mp4", width = 100, height = 50,
                      video_codec = "libx264", hardware = "nvenc",
                      fallback = TRUE, run = FALSE),
    "falling back"
  )
  expect_match(as.character(cmd), "-codec:v libx264", fixed = TRUE)
})
