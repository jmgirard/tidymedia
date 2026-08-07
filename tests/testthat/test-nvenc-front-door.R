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

test_that("resolve_hw_encoder() reaches the abort by calling the shared guard", {
  # Read the function object, never the source tree: a test that opens R/ under
  # the package root SKIPS under R CMD check, which runs against an installed
  # package with no R/ dir, and so looks healthy while never running (M51).
  src <- deparse(body(tidymedia:::resolve_hw_encoder))
  expect_true(any(grepl("check_nvenc_available", src, fixed = TRUE)))
  # cli_inform() stays -- that is the fallback message, not the abort.
  expect_false(any(grepl("cli_abort", src, fixed = TRUE)))
})
