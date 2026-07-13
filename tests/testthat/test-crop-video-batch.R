# Tests for crop_video_batch(): a table-driven sibling of crop_video() that
# crops many videos from one jobs tibble. Command construction is tested purely
# (run = FALSE); execution and verify forwarding are gated on the ffmpeg binary.

test_that("crop_video_batch() returns one crop command per job", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(input = c(f1, f2), output = c("a.mp4", "b.mp4"),
                         width = c(100, 80), height = c(50, 40))
  res <- crop_video_batch(jobs, run = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_match(res$command[[1]], "crop=w=100:h=50", fixed = TRUE)
  expect_match(res$command[[2]], "crop=w=80:h=40", fixed = TRUE)
})

test_that("crop_video_batch() command is byte-identical to the scalar verb", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "out.mp4", width = 100, height = 50,
                         x = 0, y = 0)
  res <- crop_video_batch(jobs, run = FALSE)
  scalar <- crop_video(f, "out.mp4", width = 100, height = 50, x = 0, y = 0,
                       run = FALSE)
  expect_identical(res$command[[1]], scalar)
})

# Per-row override columns + argument fallback ------------------------------

test_that("crop_video_batch() takes width/height as arguments applied to all rows", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(input = c(f1, f2), output = c("a.mp4", "b.mp4"))
  res <- crop_video_batch(jobs, width = 120, height = 90, run = FALSE)
  expect_match(res$command[[1]], "crop=w=120:h=90", fixed = TRUE)
  expect_match(res$command[[2]], "crop=w=120:h=90", fixed = TRUE)
})

test_that("crop_video_batch() honors per-row x/y columns over the argument", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.mp4", width = 100, height = 50,
                         x = 10, y = 20)
  res <- crop_video_batch(jobs, run = FALSE)
  expect_match(res$command[[1]], "crop=w=100:h=50:x=10:y=20", fixed = TRUE)
})

# Output auto-naming --------------------------------------------------------

test_that("crop_video_batch() auto-names outputs when the column is absent", {
  f1 <- make_input()
  f2 <- make_input(ext = "mkv")
  b1 <- tools::file_path_sans_ext(f1)
  b2 <- tools::file_path_sans_ext(f2)
  jobs <- tibble::tibble(input = c(f1, f2), width = c(100, 100), height = c(50, 50))
  res <- crop_video_batch(jobs, run = FALSE)
  # Crop keeps the source container.
  expect_equal(res$output, c(paste0(b1, "_cropped.mp4"), paste0(b2, "_cropped.mkv")))
})

# Return schema -------------------------------------------------------------

test_that("crop_video_batch() adds only a command column under run = FALSE", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.mp4", width = 100, height = 50)
  res <- crop_video_batch(jobs, run = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_identical(setdiff(names(res), names(jobs)), "command")
  # Same added-column schema as another batch verb (M19 parity).
  strip <- strip_metadata_batch(tibble::tibble(input = f, output = "a.mp4"),
                                run = FALSE)
  expect_identical(setdiff(names(res), names(jobs)),
                   setdiff(names(strip), c("input", "output")))
})

# Front-door validation -----------------------------------------------------

test_that("crop_video_batch() rejects a non-data-frame jobs", {
  expect_error(crop_video_batch(list(input = "a"), width = 1, height = 1, run = FALSE),
               "data frame")
})

test_that("crop_video_batch() rejects an empty jobs table", {
  expect_error(
    crop_video_batch(tibble::tibble(input = character()), width = 1, height = 1,
                     run = FALSE),
    "at least one row"
  )
})

test_that("crop_video_batch() names the missing input column", {
  expect_error(crop_video_batch(tibble::tibble(output = "a.mp4"), width = 1,
                                height = 1, run = FALSE), "input")
})

test_that("crop_video_batch() rejects an NA input", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, NA), output = c("a.mp4", "b.mp4"),
                         width = c(1, 1), height = c(1, 1))
  expect_error(crop_video_batch(jobs, run = FALSE), "input")
})

test_that("crop_video_batch() requires width and height (argument or column)", {
  f <- make_input()
  # No width/height argument and no columns.
  expect_error(crop_video_batch(tibble::tibble(input = f, output = "a.mp4"),
                                height = 50, run = FALSE), "width")
  expect_error(crop_video_batch(tibble::tibble(input = f, output = "a.mp4"),
                                width = 50, run = FALSE), "height")
})

test_that("crop_video_batch() rejects an NA in a dimension column", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"),
                         width = c(100, NA), height = c(50, 50))
  expect_error(crop_video_batch(jobs, run = FALSE), "width")
})

test_that("crop_video_batch() rejects a duplicated resolved output (M26)", {
  f <- make_input()
  # Same input twice, no output column -> same derived path.
  jobs <- tibble::tibble(input = c(f, f), width = c(100, 80), height = c(50, 40))
  expect_error(crop_video_batch(jobs, run = FALSE), "same output path")
})

# Execution + ffm_batch forwarding (binary-gated) ---------------------------

test_that("crop_video_batch() writes cropped outputs (binary-gated)", {
  v1 <- make_test_video()  # 64x64
  v2 <- make_test_video()
  out1 <- withr::local_tempfile(fileext = ".mp4")
  out2 <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(input = c(v1, v2), output = c(out1, out2),
                         width = c(32, 32), height = c(32, 32), x = c(0, 0), y = c(0, 0))
  res <- crop_video_batch(jobs, verify = list(width = 32, height = 32))
  expect_true(all(res$success))
  expect_true(all(res$verified))
})
