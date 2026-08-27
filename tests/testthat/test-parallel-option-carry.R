# The carrier itself, in one process ------------------------------------------
#
# These run everywhere: they are about what carry_options() does to an option
# list, which needs no worker. The parallel cases below need a plan and a fake
# binary and skip accordingly.

test_that("carry_options() runs .f under the values captured in the parent", {
  withr::local_options(
    tidymedia.timeout = 42, tidymedia.nvenc_encoders = "h264_nvenc"
  )
  seen <- carry_options(function() {
    list(
      timeout = getOption("tidymedia.timeout"),
      encoders = getOption("tidymedia.nvenc_encoders")
    )
  })
  # Change the parent AFTER the wrap: what the wrapper installs must be what
  # was captured, not what happens to be set when the mapped call runs.
  options(tidymedia.timeout = 7, tidymedia.nvenc_encoders = "hevc_nvenc")
  expect_equal(seen(), list(timeout = 42, encoders = "h264_nvenc"))
})

test_that("carry_options() restores the prior values when .f returns", {
  withr::local_options(
    tidymedia.timeout = 42, tidymedia.nvenc_encoders = "h264_nvenc"
  )
  wrapped <- carry_options(function() NULL)
  withr::local_options(
    tidymedia.timeout = 7, tidymedia.nvenc_encoders = "hevc_nvenc"
  )
  wrapped()
  expect_equal(getOption("tidymedia.timeout"), 7)
  expect_equal(getOption("tidymedia.nvenc_encoders"), "hevc_nvenc")
})

test_that("carry_options() restores the prior values when .f aborts", {
  withr::local_options(
    tidymedia.timeout = 42, tidymedia.nvenc_encoders = "h264_nvenc"
  )
  wrapped <- carry_options(function() {
    cli::cli_abort("mapped call failed", class = "tm_test_failure")
  })
  withr::local_options(
    tidymedia.timeout = 7, tidymedia.nvenc_encoders = "hevc_nvenc"
  )
  expect_error(wrapped(), class = "tm_test_failure")
  expect_equal(getOption("tidymedia.timeout"), 7)
  expect_equal(getOption("tidymedia.nvenc_encoders"), "hevc_nvenc")
})

test_that("an option unset in the parent is unset for the mapped call", {
  withr::local_options(tidymedia.nvenc_encoders = NULL)
  wrapped <- carry_options(function() {
    # getOption()'s default only applies when the name is absent from the
    # option list, so this distinguishes "unset" from "set to NULL".
    getOption("tidymedia.nvenc_encoders", default = "absent")
  })
  withr::local_options(tidymedia.nvenc_encoders = "hevc_nvenc")
  expect_equal(wrapped(), "absent")
  expect_equal(getOption("tidymedia.nvenc_encoders"), "hevc_nvenc")
})

test_that("an unset timeout is carried as the no-limit sentinel", {
  withr::local_options(tidymedia.timeout = NULL)
  wrapped <- carry_options(function() getOption("tidymedia.timeout"))
  expect_equal(wrapped(), 0)
})

test_that("a bad limit is refused when the carrier is built, before .f runs", {
  ran <- FALSE
  for (bad in list(0.5, -1, NA, "2")) {
    withr::local_options(tidymedia.timeout = bad)
    expect_error(
      carry_options(function() ran <<- TRUE),
      class = "rlang_error"
    )
  }
  expect_false(ran)
})
