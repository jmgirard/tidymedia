# convert_fractions() and format_probe() are pure. probe_* functions are gated
# on the ffprobe binary.

test_that("convert_fractions() evaluates string fractions", {
  expect_equal(convert_fractions(c("30000 / 1001", "25/1")), c(29.97003, 25),
               tolerance = 1e-4)
})

test_that("convert_fractions() passes NA through and rejects junk", {
  expect_equal(convert_fractions(c("1/2", NA)), c(0.5, NA_real_))
  expect_error(convert_fractions("not a fraction"))
  expect_error(convert_fractions(1:3))
})

test_that("format_probe() splits only on the first '='", {
  out <- format_probe(c("key=a=b=c", "n=2"))
  expect_equal(out$key, "a=b=c")
  expect_equal(out$n, "2")
})

test_that("probe_all() returns file-keyed container and streams tibbles", {
  skip_if_no_ffprobe()
  infile <- make_test_video()
  p <- probe_all(infile)
  expect_named(p, c("container", "streams"))
  expect_s3_class(p$container, "tbl_df")
  expect_s3_class(p$streams, "tbl_df")
  expect_identical(names(p$container)[[1]], "file")
  expect_identical(names(p$streams)[[1]], "file")
  expect_true(all(p$container$file == infile))
  expect_gt(nrow(p$streams), 0)
})

test_that("probe_all() types numeric columns by default and not when typed = FALSE", {
  skip_if_no_ffprobe()
  infile <- make_test_video()
  pt <- probe_all(infile, typed = TRUE)
  vid <- dplyr::filter(pt$streams, .data$codec_type == "video")
  expect_type(vid$width, "integer")
  expect_equal(vid$width, 64L)

  pf <- probe_all(infile, typed = FALSE)
  expect_type(pf$streams$width, "character")
})

test_that("probe_all() preserves hex identifiers under typing", {
  skip_if_no_ffprobe()
  infile <- make_test_video()
  vid <- probe_video(infile = infile)
  # codec_tag is 0x-prefixed; must stay character, not become a decimal.
  expect_type(vid$codec_tag, "character")
  expect_match(vid$codec_tag, "^0x")
})

test_that("probe_all() vectorizes over multiple files", {
  skip_if_no_ffprobe()
  infile <- make_test_video()
  p <- probe_all(c(infile, infile))
  expect_equal(nrow(p$container), 2)
  expect_gt(nrow(p$streams), 2)
})

test_that("probe_all() is resilient to an unprobeable file", {
  skip_if_no_ffprobe()
  infile <- make_test_video()
  missing <- file.path(tempdir(), "tm-does-not-exist-xyz.mp4")
  expect_warning(p <- probe_all(c(infile, missing)))
  expect_true(missing %in% p$container$file)
  # The good file still carries real data.
  expect_true(any(!is.na(p$container$format_name)))
})

test_that("probe_video()/probe_audio() stay resilient on a wholly-unreadable input", {
  skip_if_no_ffprobe()
  missing <- file.path(tempdir(), "tm-does-not-exist-xyz.mp4")
  # streams carries only a `file` column (no codec_type); must not abort.
  expect_warning(v <- probe_video(infile = missing))
  expect_s3_class(v, "tbl_df")
  expect_equal(nrow(v), 0)
  expect_warning(a <- probe_audio(infile = missing))
  expect_equal(nrow(a), 0)
})

test_that("probe_all() aborts on malformed arguments", {
  expect_error(probe_all(123))
  expect_error(probe_all(character(0)))
})

test_that("probe_container(infile =) returns a non-NULL tibble (regression)", {
  skip_if_no_ffprobe()
  infile <- make_test_video()
  expect_s3_class(probe_container(infile = infile), "tbl_df")
  expect_s3_class(probe_streams(infile = infile), "tbl_df")
  expect_true(all(probe_video(infile = infile)$codec_type == "video"))
  expect_true(all(probe_audio(infile = infile)$codec_type == "audio"))
})

test_that("probe_*() require exactly one of probe/infile", {
  expect_error(probe_container())
  skip_if_no_ffprobe()
  p <- probe_all(make_test_video())
  expect_error(probe_container(probe = p, infile = "x"))
  expect_s3_class(probe_container(probe = p), "tbl_df")
})
