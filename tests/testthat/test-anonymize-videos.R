# Tests for anonymize_videos(): a table-driven sibling of anonymize_video() that
# box-fills fixed regions of many input videos from one jobs tibble. Command
# construction is tested purely (run = FALSE); execution and the ffm_batch
# forwarding paths (verify/manifest) are gated on the ffmpeg binary.

# Command construction ----------------------------------------------------

test_that("anonymize_videos() returns one command per job with its own regions", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(
    input   = c(f1, f2),
    output  = c("a.mp4", "b.mp4"),
    regions = list(
      data.frame(x = 10, y = 20, width = 120, height = 90),
      data.frame(x = 200, y = 150, width = 80, height = 60)
    )
  )
  res <- anonymize_videos(jobs, run = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_true("command" %in% names(res))
  # Each row uses its own input, output, and regions.
  expect_match(res$command[[1]], f1, fixed = TRUE)
  expect_match(res$command[[1]], '"a.mp4"', fixed = TRUE)
  expect_match(res$command[[1]], "drawbox=x=10:y=20:w=120:h=90:c=black:t=fill",
               fixed = TRUE)
  expect_match(res$command[[2]], f2, fixed = TRUE)
  expect_match(res$command[[2]], '"b.mp4"', fixed = TRUE)
  expect_match(res$command[[2]], "drawbox=x=200:y=150:w=80:h=60:c=black:t=fill",
               fixed = TRUE)
})

test_that("anonymize_videos() draws every box for a multi-region row", {
  f <- make_input()
  jobs <- tibble::tibble(
    input   = f,
    output  = "a.mp4",
    regions = list(data.frame(
      x = c(10, 200), y = c(10, 150),
      width = c(120, 80), height = c(90, 60)
    ))
  )
  res <- anonymize_videos(jobs, run = FALSE)
  expect_match(res$command[[1]], "drawbox=x=10:y=10:w=120:h=90:c=black:t=fill",
               fixed = TRUE)
  expect_match(res$command[[1]], "drawbox=x=200:y=150:w=80:h=60:c=black:t=fill",
               fixed = TRUE)
  # A single-input sequential chain: -vf, never -filter_complex (IP2).
  expect_match(res$command[[1]], "-vf ", fixed = TRUE)
  expect_no_match(res$command[[1]], "-filter_complex", fixed = TRUE)
})

test_that("anonymize_videos() command is byte-identical to the scalar verb", {
  f <- make_input()
  regions <- data.frame(x = 10, y = 20, width = 120, height = 90)
  jobs <- tibble::tibble(input = f, output = "out.mp4", regions = list(regions))
  res <- anonymize_videos(jobs, run = FALSE)
  scalar <- anonymize_video(f, "out.mp4", regions, run = FALSE)
  # Compile-parity: the shared anonymize_pipeline() makes the batch row's
  # command identical to the scalar call with the same parameters.
  expect_identical(res$command[[1]], scalar)
})

# Per-row override columns ------------------------------------------------

test_that("anonymize_videos() honors per-row knob columns", {
  f <- make_input()
  jobs <- tibble::tibble(
    input        = c(f, f),
    output       = c("a.mp4", "b.mp4"),
    regions      = list(
      data.frame(x = 0, y = 0, width = 10, height = 10),
      data.frame(x = 0, y = 0, width = 10, height = 10)
    ),
    color        = c("red", "white"),
    vcodec       = c("libx264", "libx265"),
    pixel_format = c("yuv420p", "yuv422p")
  )
  res <- anonymize_videos(jobs, run = FALSE)
  expect_match(res$command[[1]], "c=red:t=fill", fixed = TRUE)
  expect_match(res$command[[1]], "-codec:v libx264", fixed = TRUE)
  expect_match(res$command[[1]], "-pix_fmt yuv420p", fixed = TRUE)
  expect_match(res$command[[2]], "c=white:t=fill", fixed = TRUE)
  expect_match(res$command[[2]], "-codec:v libx265", fixed = TRUE)
  expect_match(res$command[[2]], "-pix_fmt yuv422p", fixed = TRUE)
})

test_that("anonymize_videos() knob column overrides the scalar arg per row", {
  f <- make_input()
  # A color column is present for both rows; the scalar color = "green" is the
  # default only for rows lacking the column (none here), so the column wins.
  jobs <- tibble::tibble(
    input   = c(f, f), output = c("a.mp4", "b.mp4"),
    regions = list(
      data.frame(x = 0, y = 0, width = 10, height = 10),
      data.frame(x = 0, y = 0, width = 10, height = 10)
    ),
    color   = c("red", "white")
  )
  res <- anonymize_videos(jobs, color = "green", run = FALSE)
  expect_match(res$command[[1]], "c=red:t=fill", fixed = TRUE)
  expect_match(res$command[[2]], "c=white:t=fill", fixed = TRUE)
  expect_no_match(res$command[[1]], "c=green", fixed = TRUE)
})

test_that("anonymize_videos() scalar arg applies to every row without a column", {
  f <- make_input()
  jobs <- tibble::tibble(
    input   = c(f, f), output = c("a.mp4", "b.mp4"),
    regions = list(
      data.frame(x = 0, y = 0, width = 10, height = 10),
      data.frame(x = 0, y = 0, width = 10, height = 10)
    )
  )
  res <- anonymize_videos(jobs, color = "red", vcodec = "libx265", run = FALSE)
  for (cmd in res$command) {
    expect_match(cmd, "c=red:t=fill", fixed = TRUE)
    expect_match(cmd, "-codec:v libx265", fixed = TRUE)
  }
})

test_that("anonymize_videos() honors a per-box color column inside a regions cell", {
  f <- make_input()
  jobs <- tibble::tibble(
    input   = f, output = "a.mp4",
    regions = list(data.frame(
      x = c(0, 5), y = c(0, 5), width = c(10, 20), height = c(10, 20),
      color = c("white", "green"), stringsAsFactors = FALSE
    ))
  )
  res <- anonymize_videos(jobs, color = "red", run = FALSE)
  expect_match(res$command[[1]], "w=10:h=10:c=white:t=fill", fixed = TRUE)
  expect_match(res$command[[1]], "w=20:h=20:c=green:t=fill", fixed = TRUE)
  expect_no_match(res$command[[1]], "c=red", fixed = TRUE)
})

# Optional `output`: auto-naming + collision ------------------------------

test_that("anonymize_videos() auto-names outputs when the column is absent", {
  f1 <- make_input()
  f2 <- make_input()
  b1 <- tools::file_path_sans_ext(f1)
  b2 <- tools::file_path_sans_ext(f2)
  jobs <- tibble::tibble(
    input   = c(f1, f2),
    regions = list(
      data.frame(x = 0, y = 0, width = 10, height = 10),
      data.frame(x = 0, y = 0, width = 10, height = 10)
    )
  )
  res <- anonymize_videos(jobs, run = FALSE)
  # One-input -> one-output: <base>_anonymized.<input-ext>, carried on the
  # returned tibble and used in the command.
  expect_equal(res$output, c(paste0(b1, "_anonymized.mp4"),
                             paste0(b2, "_anonymized.mp4")))
  expect_match(res$command[[1]], paste0('"', b1, '_anonymized.mp4"'),
               fixed = TRUE)
})

test_that("anonymize_videos() keeps the input extension when auto-naming", {
  f <- make_input(ext = "mkv")
  base <- tools::file_path_sans_ext(f)
  jobs <- tibble::tibble(
    input   = f,
    regions = list(data.frame(x = 0, y = 0, width = 10, height = 10))
  )
  res <- anonymize_videos(jobs, run = FALSE)
  expect_equal(res$output, paste0(base, "_anonymized.mkv"))
})

test_that("anonymize_videos() aborts on a duplicated input with no output column", {
  f <- make_input()
  jobs <- tibble::tibble(
    input   = c(f, f),  # same input twice, would collide
    regions = list(
      data.frame(x = 0, y = 0, width = 10, height = 10),
      data.frame(x = 0, y = 0, width = 10, height = 10)
    )
  )
  expect_error(anonymize_videos(jobs, run = FALSE), "[Dd]uplicated")
})

test_that("anonymize_videos() allows a duplicated input with an explicit output", {
  f <- make_input()
  jobs <- tibble::tibble(
    input   = c(f, f), output = c("a.mp4", "b.mp4"),
    regions = list(
      data.frame(x = 0, y = 0, width = 10, height = 10),
      data.frame(x = 0, y = 0, width = 10, height = 10)
    )
  )
  res <- anonymize_videos(jobs, run = FALSE)
  expect_equal(res$output, c("a.mp4", "b.mp4"))
})

# Front-door validation ---------------------------------------------------

test_that("anonymize_videos() rejects a non-data-frame jobs", {
  expect_error(anonymize_videos(list(input = "a"), run = FALSE), "data frame")
})

test_that("anonymize_videos() rejects an empty jobs table", {
  jobs <- tibble::tibble(input = character(), regions = list())
  expect_error(anonymize_videos(jobs, run = FALSE), "at least one row")
})

test_that("anonymize_videos() names the missing input column", {
  jobs <- tibble::tibble(
    output = "a.mp4",
    regions = list(data.frame(x = 0, y = 0, width = 10, height = 10))
  )
  expect_error(anonymize_videos(jobs, run = FALSE), "input")
})

test_that("anonymize_videos() names the missing regions column", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.mp4")
  expect_error(anonymize_videos(jobs, run = FALSE), "regions")
})

test_that("anonymize_videos() rejects a flat (non-list) regions column", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.mp4", regions = 5)
  expect_error(anonymize_videos(jobs, run = FALSE), "list-column")
})

test_that("anonymize_videos() rejects a non-character knob column", {
  f <- make_input()
  jobs <- tibble::tibble(
    input = f, output = "a.mp4",
    regions = list(data.frame(x = 0, y = 0, width = 10, height = 10)),
    color = 5
  )
  expect_error(anonymize_videos(jobs, run = FALSE), "color")
})

# Inherited per-region validation (reported by row index) -----------------

test_that("anonymize_videos() inherits per-region validation, reported by row", {
  f <- make_input()
  # Row 2's regions cell is missing the `height` column; check_regions() (inside
  # anonymize_pipeline) aborts, and purrr annotates the failing row index. The
  # front door does not re-implement the region checks (M13 lesson).
  jobs <- tibble::tibble(
    input   = c(f, f), output = c("a.mp4", "b.mp4"),
    regions = list(
      data.frame(x = 0, y = 0, width = 10, height = 10),
      data.frame(x = 0, y = 0, width = 10)
    )
  )
  expect_error(anonymize_videos(jobs, run = FALSE), "missing")
  expect_error(anonymize_videos(jobs, run = FALSE), "index: 2")
})

test_that("anonymize_videos() inherits per-region size checks from ffm_drawbox", {
  f <- make_input()
  # A non-positive size is caught per-region by ffm_drawbox()'s check_dim(),
  # inherited through anonymize_pipeline() — not re-checked at the front door.
  jobs <- tibble::tibble(
    input   = f, output = "a.mp4",
    regions = list(data.frame(x = 0, y = 0, width = 0, height = 10))
  )
  expect_error(anonymize_videos(jobs, run = FALSE),
               "single FFmpeg expression or number")
})

# Return-schema parity ----------------------------------------------------

test_that("anonymize_videos() return schema matches a direct ffm_batch call", {
  f <- make_input()
  regions <- data.frame(x = 0, y = 0, width = 10, height = 10)
  jobs <- tibble::tibble(input = f, output = "a.mp4", regions = list(regions))
  res <- anonymize_videos(jobs, run = FALSE)
  # anonymize_videos() is a thin wrapper over ffm_batch(), so its dry-run schema
  # is exactly ffm_batch()'s: the jobs columns (regions list-column preserved)
  # plus a character `command` column, no `success` under run = FALSE.
  ref <- ffm_batch(jobs, run = FALSE, .f = function(input, output, regions, ...) {
    anonymize_pipeline(input, output, regions, "black", "libx264", "yuv420p")
  })
  expect_identical(names(res), names(ref))
  expect_identical(vapply(res, class, character(1)),
                   vapply(ref, class, character(1)))
  expect_true(is.list(res$regions))
  expect_type(res$command, "character")
  expect_false("success" %in% names(res))
})

# Execution + ffm_batch forwarding (binary-gated) -------------------------

test_that("anonymize_videos() writes anonymized outputs (binary-gated)", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  b <- make_test_video()
  out_a <- withr::local_tempfile(fileext = ".mp4")
  out_b <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(
    input   = c(a, b),
    output  = c(out_a, out_b),
    regions = list(
      data.frame(x = 8, y = 8, width = 24, height = 24),
      data.frame(x = 4, y = 4, width = 16, height = 16)
    )
  )
  res <- anonymize_videos(jobs)
  expect_true(all(res$success))
  expect_true(all(file.exists(res$output)))
  # The 64x64 (even) sources encode to non-empty, valid videos of unchanged
  # duration, proving the boxes were drawn and each file re-encoded.
  expect_gt(file.info(out_a)$size, 0)
  expect_gt(file.info(out_b)$size, 0)
  expect_equal(probe_duration(out_a), 2, tolerance = 0.2)
  expect_equal(probe_duration(out_b), 2, tolerance = 0.2)
})

test_that("anonymize_videos() forwards verify (binary-gated)", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  b <- make_test_video()
  out_a <- withr::local_tempfile(fileext = ".mp4")
  out_b <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(
    input   = c(a, b),
    output  = c(out_a, out_b),
    regions = list(
      data.frame(x = 8, y = 8, width = 24, height = 24),
      data.frame(x = 8, y = 8, width = 24, height = 24)
    )
  )
  # `verify` is a named ffm_batch argument reached only via `...`.
  res <- anonymize_videos(jobs, verify = list(width = 64, height = 64))
  expect_true(all(res$success))
  expect_true("verified" %in% names(res))
  expect_true(all(res$verified))
})

test_that("anonymize_videos() forwards manifest, read by ffm_manifest (binary-gated)", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  b <- make_test_video()
  out_a <- withr::local_tempfile(fileext = ".mp4")
  out_b <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(
    input   = c(a, b),
    output  = c(out_a, out_b),
    regions = list(
      data.frame(x = 8, y = 8, width = 24, height = 24),
      data.frame(x = 8, y = 8, width = 24, height = 24)
    )
  )
  res <- anonymize_videos(jobs, manifest = TRUE)
  man <- ffm_manifest(res)
  expect_equal(nrow(man), 2)
  expect_true(all(man$output_size > 0))
})
