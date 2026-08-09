# The `scale` range rule and the three loudness target bounds each exist as ONE
# internal binding, read by both the Layer-1 builder and the Layer-2 front door
# (M65 AC1). Each test reads the binding from the namespace and probes the
# value either side of it AT BOTH LAYERS, asserting each layer's accept/refuse
# boundary sits at the binding's value -- so a layer that restated the number
# goes red the moment the binding moves, where comparing two literals would
# compare equal forever.
#
# Nothing here needs FFmpeg: every probe compiles at `run = FALSE` or aborts
# before a command is built (normalize_audio() stays on its single-pass path).

range_wording <- function(arg) {
  paste0("`", arg, "` must be ")
}

test_that("the overlay scale range is one binding read by both layers", {
  rng <- get("overlay_scale_range", envir = asNamespace("tidymedia"))
  input <- make_input()
  eps <- 1e-3

  builder <- function(scale) {
    ffm(c(input, input), "o.mp4") |> ffm_overlay(scale = scale)
  }
  verb <- function(scale) {
    picture_in_picture(input, input, "o.mp4", scale = scale, run = FALSE)
  }
  for (layer in list(builder, verb)) {
    # Exclusive lower bound: the bound itself refuses, a nudge above accepts.
    expect_error(layer(rng[[1]]), range_wording("scale"))
    expect_no_error(layer(rng[[1]] + eps))
    # Inclusive upper bound: the bound itself accepts, a nudge above refuses.
    expect_no_error(layer(rng[[2]]))
    expect_error(layer(rng[[2]] + eps), range_wording("scale"))
  }
})

test_that("the three loudness ranges are one binding each, read by both layers", {
  ns <- asNamespace("tidymedia")
  input <- make_input()

  for (arg in c("target_loudness", "true_peak", "loudness_range")) {
    rng <- get(paste0("loudnorm_range_", arg), envir = ns)
    builder <- function(value) {
      args <- list(ffm(input, "o.wav"))
      args[[arg]] <- value
      do.call(ffm_loudnorm, args)
    }
    verb <- function(value) {
      args <- list(infile = input, outfile = "o.wav", run = FALSE)
      args[[arg]] <- value
      do.call(normalize_audio, args)
    }
    batch_verb <- function(value) {
      args <- list(jobs = tibble::tibble(input = input, output = "o.wav"),
                   run = FALSE)
      args[[arg]] <- value
      do.call(normalize_audio_batch, args)
    }
    for (layer in list(builder, verb, batch_verb)) {
      # Both bounds inclusive: each bound accepts, a step past it refuses.
      expect_error(layer(rng[[1]] - 1), range_wording(arg))
      expect_no_error(layer(rng[[1]]))
      expect_no_error(layer(rng[[2]]))
      expect_error(layer(rng[[2]] + 1), range_wording(arg))
    }
  }
})
