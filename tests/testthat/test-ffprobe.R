# convert_fractions() is pure. probe_* functions are gated on the ffprobe
# binary.

test_that("convert_fractions() evaluates string fractions", {
  expect_equal(convert_fractions(c("30000 / 1001", "25/1")), c(29.97003, 25),
               tolerance = 1e-4)
})

test_that("probe_all() returns container and streams tibbles", {
  skip_if_no_ffprobe()
  infile <- make_test_video()
  p <- probe_all(infile)
  expect_named(p, c("container", "streams"))
  expect_s3_class(p$container, "tbl_df")
  expect_s3_class(p$streams, "tbl_df")
  expect_gt(nrow(p$streams), 0)
})
