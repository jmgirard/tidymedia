# Tests for standardize_video_batch(): a table-driven sibling of standardize_video()
# that re-encodes many input files to a reproducible format from one jobs
# tibble. Command construction is tested purely (run = FALSE); execution and the
# ffm_batch forwarding paths (verify/manifest) are gated on the ffmpeg binary.

test_that("standardize_video_batch() returns one command per job across multiple inputs", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(
    input  = c(f1, f1, f2),
    output = c("a.mp4", "b.mp4", "c.mp4")
  )
  res <- standardize_video_batch(jobs, run = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 3)
  expect_true("command" %in% names(res))
  # Each row uses its own input and output.
  expect_match(res$command[[1]], f1, fixed = TRUE)
  expect_match(res$command[[1]], '"a.mp4"', fixed = TRUE)
  expect_match(res$command[[3]], f2, fixed = TRUE)
  expect_match(res$command[[3]], '"c.mp4"', fixed = TRUE)
})

test_that("standardize_video_batch() command is byte-identical to the scalar verb", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "out.mp4", width = 640)
  res <- standardize_video_batch(jobs, run = FALSE)
  scalar <- standardize_video(f, "out.mp4", width = 640, run = FALSE)
  # Compile-parity: the shared standardize_pipeline() makes the batch row's
  # command identical to the scalar call with the same parameters.
  expect_identical(res$command[[1]], scalar)
})

test_that("standardize_video_batch() default knobs match the scalar defaults", {
  f <- make_input()
  res <- standardize_video_batch(tibble::tibble(input = f, output = "out.mp4"),
                            run = FALSE)
  scalar <- standardize_video(f, "out.mp4", run = FALSE)
  expect_identical(res$command[[1]], scalar)
  # Sanity-check the default composition rides through: even-dimension
  # safeguard (no scale), libx264 / yuv420p, audio copy, +faststart.
  expect_match(res$command[[1]], "crop=w=floor(in_w/2)*2", fixed = TRUE)
  expect_match(res$command[[1]], "-codec:v libx264 -codec:a copy", fixed = TRUE)
  expect_match(res$command[[1]], "-pix_fmt yuv420p", fixed = TRUE)
  expect_match(res$command[[1]], "-movflags +faststart", fixed = TRUE)
})

# Per-row override columns ------------------------------------------------

test_that("standardize_video_batch() honors per-row knob columns", {
  f <- make_input()
  jobs <- tibble::tibble(
    input        = c(f, f, f),
    output       = c("a.mp4", "b.mp4", "c.mp4"),
    width        = c(640, 320, 1280),
    fps          = c(24, 25, 30),
    vcodec       = c("libx264", "libx265", "libx264"),
    pixel_format = c("yuv420p", "yuv422p", "yuv444p")
  )
  res <- standardize_video_batch(jobs, run = FALSE)
  expect_match(res$command[[1]], "scale=w=640:h=-2", fixed = TRUE)
  expect_match(res$command[[1]], "fps=24", fixed = TRUE)
  expect_match(res$command[[2]], "scale=w=320:h=-2", fixed = TRUE)
  expect_match(res$command[[2]], "-codec:v libx265", fixed = TRUE)
  expect_match(res$command[[2]], "-pix_fmt yuv422p", fixed = TRUE)
  expect_match(res$command[[3]], "scale=w=1280:h=-2", fixed = TRUE)
  expect_match(res$command[[3]], "fps=30", fixed = TRUE)
})

test_that("standardize_video_batch() knob column overrides the scalar arg per row", {
  f <- make_input()
  # A fps column is present for both rows; the scalar fps = 60 is the default
  # only for rows lacking the column (none here), so the column wins.
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"), fps = c(24, 30)
  )
  res <- standardize_video_batch(jobs, fps = 60, run = FALSE)
  expect_match(res$command[[1]], "fps=24", fixed = TRUE)
  expect_match(res$command[[2]], "fps=30", fixed = TRUE)
  expect_no_match(res$command[[1]], "fps=60", fixed = TRUE)
})

test_that("standardize_video_batch() scalar arg applies to every row without a column", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"))
  res <- standardize_video_batch(jobs, width = 512, vcodec = "libx265", run = FALSE)
  for (cmd in res$command) {
    expect_match(cmd, "scale=w=512:h=-2", fixed = TRUE)
    expect_match(cmd, "-codec:v libx265", fixed = TRUE)
  }
})

# Optional `output`: auto-naming + collision ------------------------------

test_that("standardize_video_batch() auto-names outputs when the column is absent", {
  f1 <- make_input()
  f2 <- make_input()
  b1 <- tools::file_path_sans_ext(f1)
  b2 <- tools::file_path_sans_ext(f2)
  jobs <- tibble::tibble(input = c(f1, f2))
  res <- standardize_video_batch(jobs, run = FALSE)
  # One-input -> one-output: <base>_standardized.<input-ext>, carried on the
  # returned tibble and used in the command.
  expect_equal(res$output, c(paste0(b1, "_standardized.mp4"),
                             paste0(b2, "_standardized.mp4")))
  expect_match(res$command[[1]], paste0('"', b1, '_standardized.mp4"'),
               fixed = TRUE)
})

test_that("standardize_video_batch() keeps the input extension when auto-naming", {
  f <- make_input(ext = "mkv")
  base <- tools::file_path_sans_ext(f)
  res <- standardize_video_batch(tibble::tibble(input = f), run = FALSE)
  expect_equal(res$output, paste0(base, "_standardized.mkv"))
})

test_that("standardize_video_batch() aborts on a duplicated input with no output column", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f))  # same input twice, would collide
  expect_error(standardize_video_batch(jobs, run = FALSE), "[Dd]uplicated")
})

test_that("standardize_video_batch() allows a duplicated input with an explicit output column", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"))
  res <- standardize_video_batch(jobs, run = FALSE)
  expect_equal(res$output, c("a.mp4", "b.mp4"))
})

test_that("standardize_video_batch() uses an explicit output column unchanged", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("keep_a.mp4", "keep_b.mkv"))
  res <- standardize_video_batch(jobs, run = FALSE)
  expect_equal(res$output, c("keep_a.mp4", "keep_b.mkv"))
})

# Front-door validation ---------------------------------------------------

test_that("standardize_video_batch() rejects a non-data-frame jobs", {
  expect_error(standardize_video_batch(list(input = "a"), run = FALSE), "data frame")
})

test_that("standardize_video_batch() rejects an empty jobs table", {
  jobs <- tibble::tibble(input = character())
  expect_error(standardize_video_batch(jobs, run = FALSE), "at least one row")
})

test_that("standardize_video_batch() names the missing input column", {
  jobs <- tibble::tibble(output = "a.mp4")
  expect_error(standardize_video_batch(jobs, run = FALSE), "input")
})

test_that("standardize_video_batch() rejects a wrong-type knob column", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.mp4", width = TRUE)
  expect_error(standardize_video_batch(jobs, run = FALSE), "width")
})

test_that("standardize_video_batch() rejects an NA in a dimension column at the front door", {
  f <- make_input()
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"), fps = c(24, NA)
  )
  expect_error(standardize_video_batch(jobs, run = FALSE), "fps")
})

test_that("standardize_video_batch() rejects a non-character vcodec column", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.mp4", vcodec = 5)
  expect_error(standardize_video_batch(jobs, run = FALSE), "vcodec")
})

# Execution + ffm_batch forwarding (binary-gated) -------------------------

test_that("standardize_video_batch() writes standardized outputs (binary-gated)", {
  skip_if_no_mediainfo()
  a <- make_test_video()
  b <- make_test_video()
  out_a <- withr::local_tempfile(fileext = ".mp4")
  out_b <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(
    input  = c(a, b),
    output = c(out_a, out_b),
    width  = c(48, 32),
    height = c(32, 32),
    fps    = c(5, 10)
  )
  res <- standardize_video_batch(jobs)
  expect_true(all(res$success))
  expect_true(all(file.exists(res$output)))
  # Each row encodes to its own requested spec.
  expect_equal(get_width(out_a), 48)
  expect_equal(get_framerate(out_a), 5)
  expect_equal(get_width(out_b), 32)
  expect_equal(get_framerate(out_b), 10)
})

test_that("standardize_video_batch() forwards verify (binary-gated)", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  b <- make_test_video()
  out_a <- withr::local_tempfile(fileext = ".mp4")
  out_b <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(
    input  = c(a, b),
    output = c(out_a, out_b),
    width  = c(64, 64),
    height = c(64, 64)
  )
  # `verify` is a named ffm_batch argument reached only via `...`.
  res <- standardize_video_batch(jobs, verify = list(width = 64, height = 64))
  expect_true(all(res$success))
  expect_true("verified" %in% names(res))
  expect_true(all(res$verified))
})

test_that("standardize_video_batch() forwards manifest, read by ffm_manifest (binary-gated)", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  b <- make_test_video()
  out_a <- withr::local_tempfile(fileext = ".mp4")
  out_b <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(input = c(a, b), output = c(out_a, out_b))
  res <- standardize_video_batch(jobs, manifest = TRUE)
  man <- ffm_manifest(res)
  expect_equal(nrow(man), 2)
  expect_true(all(man$output_size > 0))
})
