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

# The mocked encoder pools are the shared derived ones
# (`tm_hardware_encoder_pools()`, helper-timeout-sweep.R), which the probe grid
# reads too (M107). Three levels rather than the two M106 wrote out by hand:
#
#   nvenc         the nvenc row's encoders -- against `hardware = "videotoolbox"`
#                 this is the STRONGER half of the cross, since there the
#                 availability abort is what the caller would get if the token
#                 check did not run first
#   videotoolbox  the videotoolbox row's encoders, the level M106 lacked
#                 entirely: with only nvenc names mocked, no cell of the cross
#                 ever asked what a videotoolbox call does against a build that
#                 HAS its encoders
#   absent        no encoder at all
seam_pools <- function() {
  tm_hardware_encoder_pools()
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
  ffm_files(vid, file.path(dir, "out.mp4"))
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

  # And the pools are read from the shared derived helper, so a family added to
  # either backend's row reaches this cross too. Three levels, and the cross
  # below runs both backends against each, so the nvenc-pool-under-videotoolbox
  # cell M106 relied on is still here.
  expect_setequal(names(seam_pools()),
                  c(hardware_backends(), "absent"))
  expect_length(seam_pools(), length(hardware_backends()) + 1L)

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

test_that("a clean token naming no codec is refused before the build too", {
  # The case the five forms above cannot reach. Each of them is refused by a
  # type or shape check, so none gets as far as `codec_family()` -- and the
  # emit half's claim is not only that a MALFORMED token outranks the build,
  # it is that everything the caller can be told about their own token does.
  # `"notacodec"` passes `check_token()` and is then refused for naming no
  # family, with the build still unasked.
  dir <- withr::local_tempdir()
  p <- seam_pipeline(dir)
  withr::local_options(tidymedia.hardware_encoders = NULL)
  codec <- tm_nvenc_unmappable_codec()

  for (pool in names(seam_pools())) {
    for (hw in hardware_backends()) {
      probes <- 0L
      local_mocked_bindings(
        cached_encoder_names = function() {
          probes <<- probes + 1L
          seam_pools()[[pool]]
        }
      )
      label <- paste(pool, hw, sep = "/")
      cnd <- tryCatch(emit_video_codec(p, codec, hw, fallback = FALSE),
                      error = function(e) e)
      expect_s3_class(cnd, "rlang_error")
      # Which refusal, not merely that there was one: the sentence names the
      # caller's own token and no encoder or build.
      msg <- cli::ansi_strip(conditionMessage(cnd))
      expect_match(msg, "No hardware encoder family maps to that codec",
                   fixed = TRUE, info = label)
      expect_false(grepl("is not available", msg, fixed = TRUE), info = label)
      expect_identical(probes, 0L, info = label)
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

  for (hw in hardware_backends()) {
    probes <- 0L
    local_mocked_bindings(
      cached_encoder_names = function() {
        probes <<- probes + 1L
        seam_pools()$absent
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

  # And with each backend's OWN encoders listed, the same call compiles that
  # backend's encoder in -- so every pool level the sweeps above hold is a pool
  # this seam actually reads, and the control runs in every arm rather than only
  # the nvenc one (M107).
  for (hw in hardware_backends()) {
    probes <- 0L
    local_mocked_bindings(
      cached_encoder_names = function() {
        probes <<- probes + 1L
        seam_pools()[[hw]]
      }
    )
    out <- emit_video_codec(p, "libx264", hw, fallback = FALSE)
    expect_true(
      any(grepl(paste0("h264_", hw), as.character(ffm_compile(out)),
                fixed = TRUE)),
      info = hw
    )
    expect_gt(probes, 0L)
  }
})
