# Tests for concatenate_videos_batch(): the fan-in batch sibling of
# concatenate_videos(). Each row carries an `inputs` list-column cell (a vector
# of source paths) and one `output`; N rows -> N single-output concat commands.
# Command construction is tested purely (run = FALSE); execution + ffm_batch
# forwarding are binary-gated.

# The concat demuxer references a per-invocation temp list-file, so its path
# differs between any two calls; scrub it before comparing commands for parity.
scrub_concat_list <- function(cmd) gsub("ffm-concat[^\"]*\\.txt", "<list>", cmd)

# AC1: thin fan-in — one concat command per row from the inputs list-column ----

test_that("concatenate_videos_batch() compiles one concat command per row", {
  f1 <- make_input(); f2 <- make_input(); f3 <- make_input()
  jobs <- tibble::tibble(
    inputs = list(c(f1, f2), c(f2, f3)),
    output = c("j1.mp4", "j2.mp4")
  )
  res <- concatenate_videos_batch(jobs, run = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_true(all(c("inputs", "output", "command") %in% names(res)))
  # Each row is a concat-demuxer command writing its own output.
  expect_match(res$command[[1]], "-f concat -safe 0", fixed = TRUE)
  expect_match(res$command[[1]], '-codec:v copy -codec:a copy -map "0" "j1.mp4"', fixed = TRUE)
  expect_match(res$command[[2]], '"j2.mp4"', fixed = TRUE)
})

# AC5: parity — batch command equals the scalar verb's (list path scrubbed) ----

test_that("concatenate_videos_batch() glues nothing: command equals the scalar verb's", {
  f1 <- make_input(); f2 <- make_input()
  batch <- concatenate_videos_batch(
    tibble::tibble(inputs = list(c(f1, f2)), output = "o.mp4"), run = FALSE
  )
  scalar <- concatenate_videos(c(f1, f2), "o.mp4", run = FALSE)
  expect_equal(scrub_concat_list(batch$command[[1]]),
               scrub_concat_list(unname(scalar)))
})

test_that("concatenate_videos_batch() warns on a row with mixed extensions", {
  f1 <- make_input("mp4"); f2 <- make_input("mkv")
  expect_warning(
    concatenate_videos_batch(
      tibble::tibble(inputs = list(c(f1, f2)), output = "o.mp4"), run = FALSE
    ),
    "same extension"
  )
})

# AC4: jobs-table guards ------------------------------------------------------

test_that("concatenate_videos_batch() rejects a non-data-frame jobs", {
  expect_error(concatenate_videos_batch(list(inputs = "a"), run = FALSE), "data frame")
})

test_that("concatenate_videos_batch() rejects an empty jobs table", {
  jobs <- tibble::tibble(inputs = list(), output = character())
  expect_error(concatenate_videos_batch(jobs, run = FALSE), "at least one row")
})

test_that("concatenate_videos_batch() names a missing inputs column", {
  jobs <- tibble::tibble(output = "o.mp4")
  expect_error(concatenate_videos_batch(jobs, run = FALSE), "inputs")
})

test_that("concatenate_videos_batch() names a missing output column", {
  f <- make_input()
  jobs <- tibble::tibble(inputs = list(c(f, f)))
  expect_error(concatenate_videos_batch(jobs, run = FALSE), "output")
})

test_that("concatenate_videos_batch() rejects a non-list inputs column", {
  jobs <- tibble::tibble(inputs = c("a", "b"), output = c("o.mp4", "p.mp4"))
  expect_error(concatenate_videos_batch(jobs, run = FALSE), "list-column")
})

test_that("concatenate_videos_batch() rejects NA inside an inputs cell", {
  f <- make_input()
  jobs <- tibble::tibble(inputs = list(c(f, NA_character_)), output = "o.mp4")
  expect_error(concatenate_videos_batch(jobs, run = FALSE), "no")
})

test_that("concatenate_videos_batch() reports MULTIPLE invalid rows without a cli crash", {
  # Two invalid rows: the error message pluralizes off a scalar count, never the
  # numeric row-index vector (M18 lesson — a 1-row test hides the crash).
  f <- make_input()
  jobs <- tibble::tibble(
    inputs = list(c(f, NA_character_), NA_character_),
    output = c("a.mp4", "b.mp4")
  )
  expect_error(
    concatenate_videos_batch(jobs, run = FALSE),
    "Found 2 invalid"
  )
})

test_that("concatenate_videos_batch() rejects duplicate output paths", {
  f <- make_input()
  jobs <- tibble::tibble(inputs = list(c(f, f), c(f, f)), output = c("same.mp4", "same.mp4"))
  expect_error(concatenate_videos_batch(jobs, run = FALSE), "same output path")
})

# Execution + ffm_batch forwarding (binary-gated) ----------------------------

test_that("concatenate_videos_batch() joins clips end to end (binary-gated)", {
  skip_if_no_ffmpeg()
  src <- make_test_video()
  dir <- withr::local_tempdir()
  jobs <- tibble::tibble(
    inputs = list(c(src, src), c(src, src)),
    output = file.path(dir, c("j1.mp4", "j2.mp4"))
  )
  res <- concatenate_videos_batch(jobs)
  expect_equal(nrow(res), 2)
  expect_true(all(res$success))
  expect_true(all(file.exists(res$output)))
  expect_true(all(file.size(res$output) > 0))
})
