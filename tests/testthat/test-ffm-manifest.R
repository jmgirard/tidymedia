# Tests for the batch provenance manifest. Version parsing and manifest
# assembly (including checksums) are binary-free and CI-safe; capturing real
# tool versions and an end-to-end batch manifest are binary-gated.

# parse_version_line() — pure --------------------------------------------------

test_that("parse_version_line() extracts the version token", {
  line <- "ffmpeg version 8.1.2 Copyright (c) 2000-2026 the FFmpeg developers"
  expect_equal(parse_version_line(line), "8.1.2")
})

test_that("parse_version_line() returns NA for empty or missing input", {
  expect_true(is.na(parse_version_line(NULL)))
  expect_true(is.na(parse_version_line(character(0))))
})

# build_manifest() — assembly + checksums (CI-safe) ----------------------------

test_that("build_manifest() assembles one row per job with sizes and versions", {
  out1 <- withr::local_tempfile(fileext = ".mp4")
  out2 <- withr::local_tempfile(fileext = ".mp4")
  writeBin(as.raw(rep(1, 10)), out1)
  writeBin(as.raw(rep(1, 20)), out2)
  pipelines <- list(ffm_dry("in1.mp4", out1), ffm_dry("in2.mp4", out2))
  man <- build_manifest(
    pipelines,
    commands = c("cmd1", "cmd2"),
    versions = list(ffmpeg = "8.1.2", ffprobe = "8.1.2"),
    checksums = FALSE
  )
  expect_s3_class(man, "tbl_df")
  expect_equal(nrow(man), 2)
  expect_equal(man$command, c("cmd1", "cmd2"))
  expect_equal(man$output_size, c(10, 20))
  expect_equal(man$ffmpeg_version, c("8.1.2", "8.1.2"))
  expect_true(all(c("input", "output", "timestamp") %in% names(man)))
  expect_false("output_md5" %in% names(man))
})

test_that("build_manifest() adds md5 columns when checksums = TRUE", {
  outp <- withr::local_tempfile(fileext = ".mp4")
  writeBin(as.raw(rep(2, 8)), outp)
  pipelines <- list(ffm_dry(outp, outp))
  man <- build_manifest(
    pipelines, commands = "cmd",
    versions = list(ffmpeg = "x", ffprobe = "y"), checksums = TRUE
  )
  expect_true(all(c("input_md5", "output_md5") %in% names(man)))
  expect_equal(man$output_md5[[1]], unname(tools::md5sum(outp)))
})

test_that("build_manifest() records NA size for a missing output", {
  pipelines <- list(ffm_dry("in.mp4", tempfile(fileext = ".mp4")))
  man <- build_manifest(
    pipelines, commands = "cmd",
    versions = list(ffmpeg = NA_character_, ffprobe = NA_character_),
    checksums = FALSE
  )
  expect_true(is.na(man$output_size[[1]]))
})

# ffm_manifest() — retrieval + CSV (CI-safe) -----------------------------------

test_that("ffm_manifest() errors when no manifest is attached", {
  x <- tibble::tibble(command = "c")
  expect_error(ffm_manifest(x), "No provenance manifest")
})

test_that("ffm_manifest() returns the attached manifest and writes CSV", {
  x <- tibble::tibble(command = "c")
  attr(x, "manifest") <- tibble::tibble(command = "c", output_size = 42)
  expect_identical(ffm_manifest(x), attr(x, "manifest"))
  csv <- withr::local_tempfile(fileext = ".csv")
  invisible(ffm_manifest(x, path = csv))
  expect_true(file.exists(csv))
  back <- utils::read.csv(csv)
  expect_equal(back$output_size, 42)
})

# tool_versions() + end-to-end batch (binary-gated) ----------------------------

test_that("tool_versions() captures a real ffmpeg version string", {
  skip_if_no_ffmpeg()
  v <- tool_versions()
  expect_false(is.na(v$ffmpeg))
  expect_match(v$ffmpeg, "^[0-9]")
})

test_that("ffm_batch(manifest = TRUE) attaches a manifest ffm_manifest() reads", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  b <- make_test_video()
  out_a <- withr::local_tempfile(fileext = ".mp4")
  out_b <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(input = c(a, b), output = c(out_a, out_b))
  res <- ffm_batch(jobs, function(input, output, ...) {
    ffm_files(input, output) |> ffm_scale(32, 32) |> ffm_codec(video = "libx264")
  }, manifest = TRUE, checksums = TRUE)
  man <- ffm_manifest(res)
  expect_equal(nrow(man), 2)
  expect_true(all(c(
    "command", "ffmpeg_version", "ffprobe_version", "timestamp",
    "output_size", "input_md5", "output_md5"
  ) %in% names(man)))
  expect_false(anyNA(man$ffmpeg_version))
  expect_true(all(man$output_size > 0))
})

test_that("ffm_batch() attaches no manifest by default", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  out_a <- withr::local_tempfile(fileext = ".mp4")
  jobs <- tibble::tibble(input = a, output = out_a)
  res <- ffm_batch(jobs, function(input, output, ...) {
    ffm_files(input, output) |> ffm_scale(32, 32) |> ffm_codec(video = "libx264")
  })
  expect_null(attr(res, "manifest"))
})
