# Tests for compare_videos_batch(): the fan-in batch sibling of compare_videos().
# Each row carries an `inputs` list-column (>= 2 paths) and one `output`, plus
# optional per-row `direction`/`resize`/`audio` override columns that fall back
# to the scalar argument. Command construction is tested purely (run = FALSE);
# execution + ffm_batch forwarding are binary-gated.

# AC2: thin fan-in — one stack command per row ---------------------------------

test_that("compare_videos_batch() compiles one comparison command per row", {
  f1 <- make_input(); f2 <- make_input()
  jobs <- tibble::tibble(
    inputs = list(c(f1, f2), c(f1, f2)),
    output = c("c1.mp4", "c2.mp4")
  )
  res <- compare_videos_batch(jobs, run = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_true(all(c("inputs", "output", "command") %in% names(res)))
  expect_match(res$command[[1]], "hstack", fixed = TRUE)
  expect_match(res$command[[1]], '"c1.mp4"', fixed = TRUE)
})

test_that("compare_videos_batch() direction column overrides the arg per row", {
  f1 <- make_input(); f2 <- make_input()
  jobs <- tibble::tibble(
    inputs    = list(c(f1, f2), c(f1, f2)),
    output    = c("h.mp4", "v.mp4"),
    direction = c("horizontal", "vertical")
  )
  # Arg says horizontal, but the column wins per row.
  res <- compare_videos_batch(jobs, direction = "horizontal", run = FALSE)
  expect_match(res$command[[1]], "hstack", fixed = TRUE)
  expect_match(res$command[[2]], "vstack", fixed = TRUE)
})

test_that("compare_videos_batch() resize column overrides the arg per row", {
  f1 <- make_input(); f2 <- make_input(); f3 <- make_input()
  jobs <- tibble::tibble(
    inputs = list(c(f1, f2), c(f1, f2, f3)),
    output = c("r.mp4", "n.mp4"),
    resize = c(TRUE, FALSE)
  )
  res <- compare_videos_batch(jobs, run = FALSE)
  expect_match(res$command[[1]], "scale2ref", fixed = TRUE)     # resized
  expect_no_match(res$command[[2]], "scale2ref", fixed = TRUE)  # not resized (3 inputs)
})

test_that("compare_videos_batch() audio column carries one input's audio; NA drops it", {
  f1 <- make_input(); f2 <- make_input()
  jobs <- tibble::tibble(
    inputs = list(c(f1, f2), c(f1, f2)),
    output = c("a.mp4", "s.mp4"),
    audio  = c(1, NA)
  )
  res <- compare_videos_batch(jobs, run = FALSE)
  expect_match(res$command[[1]], "-map \"1:a\"", fixed = TRUE)      # keep input 1's audio
  expect_no_match(res$command[[2]], ":a", fixed = TRUE)         # NA -> drop audio
})

# AC5: parity — batch command equals the scalar verb's -------------------------

test_that("compare_videos_batch() glues nothing: command equals the scalar verb's", {
  f1 <- make_input(); f2 <- make_input()
  batch <- compare_videos_batch(
    tibble::tibble(inputs = list(c(f1, f2)), output = "o.mp4"), run = FALSE
  )
  scalar <- compare_videos(c(f1, f2), "o.mp4", run = FALSE)
  expect_equal(batch$command[[1]], unname(scalar))
})

# AC4: jobs-table guards -------------------------------------------------------

test_that("compare_videos_batch() rejects an empty jobs table", {
  jobs <- tibble::tibble(inputs = list(), output = character())
  expect_error(compare_videos_batch(jobs, run = FALSE), "at least one row")
})

test_that("compare_videos_batch() names a missing inputs column", {
  expect_error(compare_videos_batch(tibble::tibble(output = "o.mp4"), run = FALSE), "inputs")
})

test_that("compare_videos_batch() requires two or more inputs per row", {
  f <- make_input()
  jobs <- tibble::tibble(inputs = list(c(f)), output = "o.mp4")
  expect_error(compare_videos_batch(jobs, run = FALSE), "or more")
})

test_that("compare_videos_batch() reports MULTIPLE under-filled rows without a cli crash", {
  # Two rows each with a single input (min is 2): the message must pluralize off
  # a scalar count, not the numeric row-index vector (M18 lesson).
  f <- make_input()
  jobs <- tibble::tibble(inputs = list(c(f), c(f)), output = c("a.mp4", "b.mp4"))
  expect_error(compare_videos_batch(jobs, run = FALSE), "Found 2 invalid")
})

test_that("compare_videos_batch() rejects a non-logical resize column", {
  f1 <- make_input(); f2 <- make_input()
  jobs <- tibble::tibble(inputs = list(c(f1, f2)), output = "o.mp4", resize = "yes")
  expect_error(compare_videos_batch(jobs, run = FALSE), "resize")
})

test_that("compare_videos_batch() rejects duplicate output paths", {
  f1 <- make_input(); f2 <- make_input()
  jobs <- tibble::tibble(inputs = list(c(f1, f2), c(f1, f2)), output = c("x.mp4", "x.mp4"))
  expect_error(compare_videos_batch(jobs, run = FALSE), "same output path")
})

test_that("compare_videos_batch() rejects resize = TRUE with a non-pair row", {
  f1 <- make_input(); f2 <- make_input(); f3 <- make_input()
  jobs <- tibble::tibble(inputs = list(c(f1, f2, f3)), output = "o.mp4")
  # default resize = TRUE, but the row has three inputs
  expect_error(compare_videos_batch(jobs, run = FALSE), "exactly two inputs")
})

# Execution + ffm_batch forwarding (binary-gated) ------------------------------

test_that("compare_videos_batch() writes comparison videos end to end (binary-gated)", {
  skip_if_no_ffmpeg()
  src <- make_test_video()
  dir <- withr::local_tempdir()
  jobs <- tibble::tibble(
    inputs = list(c(src, src), c(src, src)),
    output = file.path(dir, c("c1.mp4", "c2.mp4"))
  )
  res <- compare_videos_batch(jobs)
  expect_true(all(res$success))
  expect_true(all(file.exists(res$output)))
  expect_true(all(file.size(res$output) > 0))
})
