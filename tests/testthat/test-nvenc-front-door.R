# The nvenc availability guard at the front door of every fan-out verb (M57).
#
# A verb that fans out through ffm_batch() -> purrr::pmap() cannot get useful
# blame from threading `call` into its pipeline: the pipeline's caller_env()
# lands on the anonymous closure, so an unavailable nvenc encoder was reported
# as "Error in `purrr::pmap(jobs, .f, ...)`" (LESSONS M47/M48-F1). D035
# licenses re-running the availability check at each such verb's front door.
#
# Availability is simulated with the `tidymedia.nvenc_encoders` option seam, so
# these tests need no GPU and no nvenc-capable FFmpeg build.

# --- AC2: one abort site -----------------------------------------------------
#
# The front door and the pipeline must reach the same worded abort. Comparing
# the two functions directly is the sharpest form of that check: a copy of the
# wording in resolve_hw_encoder() would pass every verb-level test in this file
# right up until someone edited one of the two.

test_that("the front-door guard and resolve_hw_encoder() word the abort identically", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  front <- tryCatch(
    tidymedia:::check_nvenc_available("libx264", "nvenc", FALSE),
    condition = function(cnd) cnd
  )
  pipeline <- tryCatch(
    tidymedia:::resolve_hw_encoder("libx264", "nvenc", FALSE),
    condition = function(cnd) cnd
  )
  expect_s3_class(front, "rlang_error")
  expect_s3_class(pipeline, "rlang_error")
  expect_identical(conditionMessage(front), conditionMessage(pipeline))
  expect_match(conditionMessage(front), "h264_nvenc", fixed = TRUE)
})

# --- the fan-out family -----------------------------------------------------
#
# The nine verbs that take `hardware` AND fan out through ffm_batch(). The list
# is fixed rather than derived, so a verb that gains `hardware` later fails the
# completeness test below instead of silently dropping out of the sweep.
nvenc_fanout_verbs <- function() {
  c("segment_video", "anonymize_video_batch", "segment_video_batch",
    "standardize_video_batch", "crop_video_batch", "format_for_web_batch",
    "separate_audio_video_batch", "compare_videos_batch",
    "picture_in_picture_batch")
}

# The seven that take `hardware` and call their pipelines DIRECTLY, where `call`
# already resolves to the verb, so M57 adds them no guard (M47 F8). Named here
# only so the two lists together can be checked against the package's exports:
# a new hardware-bearing verb has to be classified, not quietly omitted.
nvenc_direct_verbs <- function() {
  c("separate_audio_video", "crop_video", "format_for_web", "standardize_video",
    "anonymize_video", "compare_videos", "picture_in_picture")
}

# Call templates. Eight come from helper-codec-family.R, which already models
# each verb's real column names -- picture_in_picture_batch takes main/overlay
# columns (D015) and compare_videos_batch an `inputs` list-column, and getting
# either wrong records a schema error in place of the abort under test (M54).
# format_for_web_batch has no codec argument at all (its recipe fixes both
# codecs by identity), so it is not in that helper and gets a template here.
nvenc_fanout_call <- function(verb, input, out) {
  if (identical(verb, "format_for_web_batch")) {
    return(list(jobs = tibble::tibble(input = input, output = out)))
  }
  args <- codec_family_call(verb, input, out)
  # separate_audio_video*'s video_codec defaults to "copy", which codec_family()
  # refuses outright -- a DIFFERENT abort, and one that would satisfy a blame
  # assertion while measuring nothing about availability (M54). Name an
  # h264-family codec so the cell measures what it claims to.
  if (identical(verb, "separate_audio_video_batch")) {
    args$video_codec <- "libx264"
  }
  args
}

# Which function the condition blames, rendered from the call's head alone: the
# full call deparses a whole jobs tibble inline.
nvenc_blamed <- function(cnd) {
  cl <- conditionCall(cnd)
  if (is.null(cl)) return(NA_character_)
  paste(deparse(cl[[1]]), collapse = "")
}

nvenc_fanout_catch <- function(verb, input, out = "out.mp4", parallel = FALSE,
                               extra = list()) {
  f <- get(verb, envir = asNamespace("tidymedia"))
  args <- nvenc_fanout_call(verb, input, out)
  args$hardware <- "nvenc"
  args$fallback <- FALSE
  args$run <- FALSE
  if ("parallel" %in% names(formals(f))) args$parallel <- parallel
  args <- utils::modifyList(args, extra)
  # Catch the ERROR, never any condition: at parallel = TRUE the sequential-plan
  # warning arrives first and a `condition =` handler would return that instead
  # (measured while recording the master readings).
  withCallingHandlers(
    tryCatch({
      do.call(verb, args, envir = asNamespace("tidymedia"))
      NULL
    }, error = function(e) e),
    warning = function(w) invokeRestart("muffleWarning")
  )
}

test_that("the two lists together are every hardware-bearing exported verb", {
  exported <- getNamespaceExports("tidymedia")
  hw <- Filter(function(nm) {
    # `inherits = TRUE` (the default): a re-export such as `.data` is listed in
    # NAMESPACE but bound in the imports env, and an inherits = FALSE lookup
    # errors on it outright (M55).
    obj <- get(nm, envir = asNamespace("tidymedia"))
    is.function(obj) && "hardware" %in% names(formals(obj))
  }, exported)
  expect_setequal(hw, c(nvenc_fanout_verbs(), nvenc_direct_verbs()))
})

# --- AC1: the abort names the verb ------------------------------------------

test_that("an unavailable nvenc encoder blames the fan-out verb, not purrr::pmap()", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  input <- make_input()
  for (verb in nvenc_fanout_verbs()) {
    cnd <- nvenc_fanout_catch(verb, input)
    expect_s3_class(cnd, "rlang_error")
    # Match the MESSAGE first: a jobs-schema error blames the same front door
    # and would satisfy a bare conditionCall() assertion with the guard
    # deleted -- three M54 review rounds were paid for exactly that (M54).
    expect_match(conditionMessage(cnd), "nvenc encoder .* is not available",
                 info = verb)
    expect_identical(nvenc_blamed(cnd), verb, info = verb)
  }
})

test_that("the abort names the verb at parallel = TRUE too", {
  skip_if_not_installed("furrr")
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  old <- future::plan(future::sequential)
  withr::defer(future::plan(old))
  input <- make_input()
  cnd <- nvenc_fanout_catch("standardize_video_batch", input, parallel = TRUE)
  expect_s3_class(cnd, "rlang_error")
  expect_match(conditionMessage(cnd), "nvenc encoder .* is not available")
  expect_identical(nvenc_blamed(cnd), "standardize_video_batch")
})

test_that("resolve_hw_encoder() reaches the abort by calling the shared guard", {
  # Read the function object, never the source tree: a test that opens R/ under
  # the package root SKIPS under R CMD check, which runs against an installed
  # package with no R/ dir, and so looks healthy while never running (M51).
  src <- deparse(body(tidymedia:::resolve_hw_encoder))
  expect_true(any(grepl("check_nvenc_available", src, fixed = TRUE)))
  # cli_inform() stays -- that is the fallback message, not the abort.
  expect_false(any(grepl("cli_abort", src, fixed = TRUE)))
})
