# M106: the codec seam's two halves cannot be called apart.
#
# M095 split apply_video_codec() into check_video_codec() and
# emit_video_codec() so a pipeline could check the caller's token where the
# combined call used to sit and emit the codec after its last
# machine-independent check. The split left the emit half checking nothing: it
# went straight to resolve_hw_encoder(), which under a hardware backend asks
# this FFmpeg build for its encoder list. A pipeline reaching the emit half
# without a check above it would therefore answer "that encoder is not
# available" about a token that was never a codec name -- the machine deciding
# which error the caller sees, which is the whole thing M095 removed.
#
# These tests call emit_video_codec() ALONE, with no check_video_codec() above
# it: the property under test is a property of the emit half's own body, so a
# test that reached it through a pipeline would be measuring the pipeline's
# check instead.

# The two mocked encoder pools are the probe grid's own
# (`nvenc_order_pools`, data-raw/nvenc-probe-order-baseline.R): nvenc names, or
# nothing. Neither lists a videotoolbox encoder, which is what makes the
# videotoolbox arm the stronger half of the cross -- there the availability
# abort is what the caller WOULD get in both pools if the token check did not
# run first.
seam_pools <- function() {
  list(present = c("h264_nvenc", "hevc_nvenc", "av1_nvenc"),
       absent = character())
}

# What each of the five wrong forms is refused WITH, recorded from a run rather
# than described: four are rlang::check_string()'s type refusal and one is
# check_token()'s shape refusal, and all five name `video_codec`. Every one of
# them is a complaint about the caller's own argument; none of them mentions an
# encoder or a build.
seam_expected <- function() {
  c(number     = "must be a single string, not the number 123",
    token      = "must be a single clean token",
    missing    = "must be a single string, not `NA`",
    length_two = "must be a single string, not a double vector",
    list       = "must be a single string, not a list")
}

seam_pipeline <- function(dir) {
  vid <- file.path(dir, "in.mp4")
  if (!file.exists(vid)) file.create(vid)
  ffm(vid, file.path(dir, "out.mp4"))
}

test_that("a wrong codec token is refused before the build is asked anything", {
  dir <- withr::local_tempdir()
  p <- seam_pipeline(dir)
  forms <- tm_nvenc_wrong_forms()
  expected <- seam_expected()

  # The forms are read from the shared table, so a sixth form added there joins
  # this sweep rather than being silently skipped -- and a form with no recorded
  # message here fails the sweep instead of passing it vacuously.
  expect_setequal(names(forms), names(expected))
  expect_length(forms, 5L)

  # The option seam returns above cached_encoder_names(), so leaving it set
  # would make every cell below measure the option and never the mock.
  withr::local_options(tidymedia.hardware_encoders = NULL)

  for (pool in names(seam_pools())) {
    for (hw in c("nvenc", "videotoolbox")) {
      for (form in names(forms)) {
        probes <- 0L
        local_mocked_bindings(
          cached_encoder_names = function() {
            probes <<- probes + 1L
            seam_pools()[[pool]]
          }
        )
        label <- paste(pool, hw, form, sep = "/")
        expect_error(
          emit_video_codec(p, forms[[form]], hw, fallback = FALSE),
          regexp = expected[[form]],
          fixed = TRUE,
          info = label
        )
        # The claim is not only that the call is refused -- it is that the
        # build was never consulted. A probe count above zero means the
        # resolver ran, whatever the message said.
        expect_identical(probes, 0L, info = label)
      }
    }
  }
})

test_that("the same cells reach the build once the token is a real one", {
  # The discrimination check for the sweep above. Its zero probe counts are
  # worth nothing unless this mock can be reached at all, and its "not
  # available" absence is worth nothing unless that abort is what a valid token
  # gets in these very cells. Both are shown here.
  dir <- withr::local_tempdir()
  p <- seam_pipeline(dir)
  withr::local_options(tidymedia.hardware_encoders = NULL)

  for (hw in c("nvenc", "videotoolbox")) {
    probes <- 0L
    local_mocked_bindings(
      cached_encoder_names = function() {
        probes <<- probes + 1L
        character()
      }
    )
    expect_error(
      emit_video_codec(p, "libx264", hw, fallback = FALSE),
      regexp = "is not available",
      fixed = TRUE,
      info = hw
    )
    expect_gt(probes, 0L)
  }

  # And with the encoder listed, the same call compiles the hardware encoder in
  # -- so the pool the sweep above holds is a pool this seam actually reads.
  probes <- 0L
  local_mocked_bindings(
    cached_encoder_names = function() {
      probes <<- probes + 1L
      c("h264_nvenc", "hevc_nvenc", "av1_nvenc")
    }
  )
  out <- emit_video_codec(p, "libx264", "nvenc", fallback = FALSE)
  expect_true(any(grepl("h264_nvenc", as.character(ffm_compile(out)), fixed = TRUE)))
  expect_gt(probes, 0L)
})
