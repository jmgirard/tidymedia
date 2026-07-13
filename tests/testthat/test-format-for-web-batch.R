# Tests for format_for_web_batch(): a table-driven sibling of format_for_web()
# that re-encodes many videos to a web-friendly form from one jobs tibble.
# Command construction is tested purely (run = FALSE); execution and verify
# forwarding are gated on the ffmpeg binary.

test_that("format_for_web_batch() returns one re-encode command per job", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(input = c(f1, f2), output = c("a.mp4", "b.mp4"))
  res <- format_for_web_batch(jobs, run = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_match(res$command[[1]], "-codec:v libx264 -codec:a aac", fixed = TRUE)
  expect_match(res$command[[1]], "-pix_fmt yuv420p", fixed = TRUE)
  expect_match(res$command[[1]], "+faststart", fixed = TRUE)
})

test_that("format_for_web_batch() command is byte-identical to the scalar verb", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "out.mp4")
  res <- format_for_web_batch(jobs, run = FALSE)
  scalar <- format_for_web(f, "out.mp4", run = FALSE)
  expect_identical(res$command[[1]], scalar)
})

# Output auto-naming --------------------------------------------------------

test_that("format_for_web_batch() auto-names outputs as <base>_web.mp4", {
  f1 <- make_input()
  f2 <- make_input(ext = "mkv")
  b1 <- tools::file_path_sans_ext(f1)
  b2 <- tools::file_path_sans_ext(f2)
  jobs <- tibble::tibble(input = c(f1, f2))
  res <- format_for_web_batch(jobs, run = FALSE)
  # The web re-encode always writes H.264/mp4, so the derived name is .mp4
  # regardless of the input extension.
  expect_equal(res$output, c(paste0(b1, "_web.mp4"), paste0(b2, "_web.mp4")))
})

# Return schema -------------------------------------------------------------

test_that("format_for_web_batch() adds only a command column under run = FALSE", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.mp4")
  res <- format_for_web_batch(jobs, run = FALSE)
  expect_identical(setdiff(names(res), names(jobs)), "command")
})

# Front-door validation -----------------------------------------------------

test_that("format_for_web_batch() rejects a non-data-frame jobs", {
  expect_error(format_for_web_batch(list(input = "a"), run = FALSE), "data frame")
})

test_that("format_for_web_batch() rejects an empty jobs table", {
  expect_error(format_for_web_batch(tibble::tibble(input = character()), run = FALSE),
               "at least one row")
})

test_that("format_for_web_batch() names the missing input column", {
  expect_error(format_for_web_batch(tibble::tibble(output = "a.mp4"), run = FALSE),
               "input")
})

test_that("format_for_web_batch() rejects an NA input", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, NA), output = c("a.mp4", "b.mp4"))
  expect_error(format_for_web_batch(jobs, run = FALSE), "input")
})

test_that("format_for_web_batch() rejects a duplicated resolved output (M26)", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f))  # same derived _web.mp4 twice
  expect_error(format_for_web_batch(jobs, run = FALSE), "same output path")
})

test_that("format_for_web_batch() rejects a duplicated explicit output (M26)", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(input = c(f1, f2), output = c("same.mp4", "same.mp4"))
  expect_error(format_for_web_batch(jobs, run = FALSE), "same output path")
})

# Execution + ffm_batch forwarding (binary-gated) ---------------------------

test_that("format_for_web_batch() writes H.264 outputs (binary-gated)", {
  v1 <- make_test_video()
  v2 <- make_test_video()
  out1 <- withr::local_tempfile(fileext = ".mp4")
  out2 <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(input = c(v1, v2), output = c(out1, out2))
  res <- format_for_web_batch(jobs, verify = list(video_codec = "h264"))
  expect_true(all(res$success))
  expect_true(all(res$verified))
})
