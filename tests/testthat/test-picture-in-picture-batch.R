# Tests for picture_in_picture_batch(): the fan-in batch sibling of
# picture_in_picture(). Its two inputs have distinct roles, so jobs carries fixed
# `main`/`overlay` columns (not a list-column; D015) plus `output`, with optional
# per-row `position`/`scale`/`margin`/`audio_input` override columns falling back to the
# arguments. Command construction is tested purely (run = FALSE); execution +
# ffm_batch forwarding are binary-gated.

# AC3: thin fan-in — one overlay command per row -------------------------------

test_that("picture_in_picture_batch() compiles one overlay command per row", {
  m1 <- make_input(); o1 <- make_input(); m2 <- make_input(); o2 <- make_input()
  jobs <- tibble::tibble(
    main    = c(m1, m2),
    overlay = c(o1, o2),
    output  = c("p1.mp4", "p2.mp4")
  )
  res <- picture_in_picture_batch(jobs, run = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_true(all(c("main", "overlay", "output", "command") %in% names(res)))
  expect_match(res$command[[1]], "overlay=", fixed = TRUE)
  expect_match(res$command[[1]], '"p1.mp4"', fixed = TRUE)
})

test_that("picture_in_picture_batch() position/scale/margin columns override the args per row", {
  m1 <- make_input(); o1 <- make_input(); m2 <- make_input(); o2 <- make_input()
  jobs <- tibble::tibble(
    main     = c(m1, m2),
    overlay  = c(o1, o2),
    output   = c("tl.mp4", "ctr.mp4"),
    position = c("topleft", "center"),
    scale    = c(0.25, 0.5),
    margin   = c(8, 0)
  )
  res <- picture_in_picture_batch(jobs, run = FALSE)
  # Row 1: topleft => x=8, y=8; scale 0.25.
  expect_match(res$command[[1]], "overlay=x=8:y=8", fixed = TRUE)
  expect_match(res$command[[1]], "main_w*0.25", fixed = TRUE)
  # Row 2: center => centered expressions; scale 0.5.
  expect_match(res$command[[2]], "(main_w-overlay_w)/2", fixed = TRUE)
  expect_match(res$command[[2]], "main_w*0.5", fixed = TRUE)
})

test_that("picture_in_picture_batch() audio column carries an input's audio; NA drops it", {
  m1 <- make_input(); o1 <- make_input(); m2 <- make_input(); o2 <- make_input()
  jobs <- tibble::tibble(
    main = c(m1, m2), overlay = c(o1, o2),
    output = c("a.mp4", "s.mp4"), audio_input = c(1, NA)
  )
  res <- picture_in_picture_batch(jobs, run = FALSE)
  expect_match(res$command[[1]], "-map \"1:a\"", fixed = TRUE)
  # The overlay always maps its video output ([vout]); only the audio map is
  # conditional, so NA drops the ":a" map, not the "[vout]" one.
  expect_no_match(res$command[[2]], ":a", fixed = TRUE)
})

# AC5: parity — batch command equals the scalar verb's -------------------------

test_that("picture_in_picture_batch() glues nothing: command equals the scalar verb's", {
  m <- make_input(); o <- make_input()
  batch <- picture_in_picture_batch(
    tibble::tibble(main = m, overlay = o, output = "o.mp4"), run = FALSE
  )
  scalar <- picture_in_picture(m, o, "o.mp4", run = FALSE)
  expect_equal(batch$command[[1]], unname(scalar))
})

# AC4: jobs-table guards -------------------------------------------------------

test_that("picture_in_picture_batch() rejects a non-data-frame jobs", {
  expect_error(picture_in_picture_batch(list(main = "a"), run = FALSE), "data frame")
})

test_that("picture_in_picture_batch() rejects an empty jobs table", {
  jobs <- tibble::tibble(main = character(), overlay = character(), output = character())
  expect_error(picture_in_picture_batch(jobs, run = FALSE), "at least one row")
})

test_that("picture_in_picture_batch() names a missing main/overlay/output column", {
  m <- make_input()
  expect_error(
    picture_in_picture_batch(tibble::tibble(main = m, output = "o.mp4"), run = FALSE),
    "overlay"
  )
})

test_that("picture_in_picture_batch() rejects NA in main or overlay", {
  m <- make_input()
  bad <- tibble::tibble(main = m, overlay = NA_character_, output = "o.mp4")
  expect_error(picture_in_picture_batch(bad, run = FALSE), "overlay")
})

test_that("picture_in_picture_batch() rejects a scale column containing NA", {
  m <- make_input(); o <- make_input()
  jobs <- tibble::tibble(main = m, overlay = o, output = "o.mp4", scale = NA_real_)
  expect_error(picture_in_picture_batch(jobs, run = FALSE), "scale")
})

test_that("picture_in_picture_batch() enforces the scalar's margin contract per row", {
  # A `margin` column bypasses the scalar arg's check; a negative or fractional
  # value must still abort here, exactly as the scalar picture_in_picture() does.
  m <- make_input(); o <- make_input()
  neg  <- tibble::tibble(main = m, overlay = o, output = "o.mp4", margin = -8)
  frac <- tibble::tibble(main = m, overlay = o, output = "o.mp4", margin = 16.5)
  expect_error(picture_in_picture_batch(neg, run = FALSE), "margin")
  expect_error(picture_in_picture_batch(frac, run = FALSE), "margin")
})

test_that("picture_in_picture_batch() accepts an all-NA audio column as 'drop audio'", {
  # `audio_input = NA` is logical, not numeric; the roxygen documents it as "drop
  # audio", so it must be accepted (parity with compare_videos_batch).
  m <- make_input(); o <- make_input()
  jobs <- tibble::tibble(main = m, overlay = o, output = "o.mp4", audio_input = NA)
  res <- picture_in_picture_batch(jobs, run = FALSE)
  expect_no_match(res$command[[1]], ":a", fixed = TRUE)   # audio dropped
})

test_that("picture_in_picture_batch() rejects duplicate output paths", {
  m <- make_input(); o <- make_input()
  jobs <- tibble::tibble(main = c(m, m), overlay = c(o, o), output = c("x.mp4", "x.mp4"))
  expect_error(picture_in_picture_batch(jobs, run = FALSE), "same output path")
})

# Execution + ffm_batch forwarding (binary-gated) ------------------------------

test_that("picture_in_picture_batch() writes inset videos end to end (binary-gated)", {
  skip_if_no_ffmpeg()
  src <- make_test_video()
  dir <- withr::local_tempdir()
  jobs <- tibble::tibble(
    main = c(src, src), overlay = c(src, src),
    output = file.path(dir, c("p1.mp4", "p2.mp4"))
  )
  res <- picture_in_picture_batch(jobs)
  expect_true(all(res$success))
  expect_true(all(file.exists(res$output)))
  expect_true(all(file.size(res$output) > 0))
})
