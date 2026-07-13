# Tests for strip_metadata_batch(): a table-driven sibling of strip_metadata()
# that de-identifies many input files from one jobs tibble. Command construction
# is tested purely (run = FALSE); execution and the ffm_batch forwarding paths
# (verify) are gated on the ffmpeg binary.

test_that("strip_metadata_batch() returns one strip command per job", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(
    input  = c(f1, f2),
    output = c("a.mp4", "b.mp4")
  )
  res <- strip_metadata_batch(jobs, run = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_true("command" %in% names(res))
  # Each row scrubs its own input into its own output.
  expect_match(res$command[[1]], f1, fixed = TRUE)
  expect_match(res$command[[1]], '"a.mp4"', fixed = TRUE)
  expect_match(res$command[[1]], "-map_metadata -1", fixed = TRUE)
  expect_match(res$command[[2]], f2, fixed = TRUE)
  expect_match(res$command[[2]], '"b.mp4"', fixed = TRUE)
})

test_that("strip_metadata_batch() command is byte-identical to the scalar verb", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "out.mp4")
  res <- strip_metadata_batch(jobs, run = FALSE)
  scalar <- strip_metadata(f, "out.mp4", run = FALSE)
  # Compile-parity via the shared strip_metadata_pipeline() (M13).
  expect_identical(res$command[[1]], scalar)
})

# Optional `output`: auto-naming + collision ------------------------------

test_that("strip_metadata_batch() auto-names outputs when the column is absent", {
  f1 <- make_input()
  f2 <- make_input()
  b1 <- tools::file_path_sans_ext(f1)
  b2 <- tools::file_path_sans_ext(f2)
  jobs <- tibble::tibble(input = c(f1, f2))
  res <- strip_metadata_batch(jobs, run = FALSE)
  expect_equal(res$output, c(paste0(b1, "_stripped.mp4"),
                             paste0(b2, "_stripped.mp4")))
  expect_match(res$command[[1]], paste0('"', b1, '_stripped.mp4"'), fixed = TRUE)
})

test_that("strip_metadata_batch() keeps the input extension when auto-naming", {
  f <- make_input(ext = "mkv")
  base <- tools::file_path_sans_ext(f)
  res <- strip_metadata_batch(tibble::tibble(input = f), run = FALSE)
  expect_equal(res$output, paste0(base, "_stripped.mkv"))
})

test_that("strip_metadata_batch() aborts on a duplicated input with no output column", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f))  # same derived output twice
  expect_error(strip_metadata_batch(jobs, run = FALSE), "same output path")
})

test_that("strip_metadata_batch() aborts on a duplicated explicit output (M26)", {
  f1 <- make_input()
  f2 <- make_input()
  # Distinct inputs, but the user pointed two rows at the same output file.
  jobs <- tibble::tibble(input = c(f1, f2), output = c("same.mp4", "same.mp4"))
  expect_error(strip_metadata_batch(jobs, run = FALSE), "same output path")
})

test_that("strip_metadata_batch() allows a duplicated input with distinct outputs", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mkv"))
  res <- strip_metadata_batch(jobs, run = FALSE)
  expect_equal(res$output, c("a.mp4", "b.mkv"))
})

# Front-door validation ---------------------------------------------------

test_that("strip_metadata_batch() rejects a non-data-frame jobs", {
  expect_error(strip_metadata_batch(list(input = "a"), run = FALSE), "data frame")
})

test_that("strip_metadata_batch() rejects an empty jobs table", {
  jobs <- tibble::tibble(input = character())
  expect_error(strip_metadata_batch(jobs, run = FALSE), "at least one row")
})

test_that("strip_metadata_batch() names the missing input column", {
  jobs <- tibble::tibble(output = "a.mp4")
  expect_error(strip_metadata_batch(jobs, run = FALSE), "input")
})

test_that("strip_metadata_batch() rejects an NA input", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, NA), output = c("a.mp4", "b.mp4"))
  expect_error(strip_metadata_batch(jobs, run = FALSE), "input")
})

test_that("strip_metadata_batch() rejects an NA in an explicit output column", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp4", NA))
  expect_error(strip_metadata_batch(jobs, run = FALSE), "output")
})

# Execution + ffm_batch forwarding (binary-gated) -------------------------

test_that("strip_metadata_batch() writes de-identified outputs (binary-gated)", {
  a <- make_tagged_video()
  b <- make_tagged_video()
  out_a <- withr::local_tempfile(fileext = ".mp4")
  out_b <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(input = c(a, b), output = c(out_a, out_b))
  res <- strip_metadata_batch(jobs)
  expect_true(all(res$success))
  expect_true(all(file.exists(res$output)))
  # The identifying tags are gone from every output.
  for (out in res$output) {
    tags <- probe_format_tags(out)
    expect_false(any(grepl("^title=", tags)))
    expect_false(any(grepl("^creation_time=", tags)))
    expect_false(any(grepl("^encoder=", tags)))
  }
})

test_that("strip_metadata_batch() forwards verify (binary-gated)", {
  a <- make_tagged_video()
  out_a <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(input = a, output = out_a)
  # `verify` is a named ffm_batch argument reached only via `...`.
  res <- strip_metadata_batch(jobs, verify = list(video_codec = "h264"))
  expect_true(all(res$success))
  expect_true("verified" %in% names(res))
  expect_true(all(res$verified))
})
