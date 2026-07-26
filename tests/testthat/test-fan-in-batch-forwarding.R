# AC6 for the fan-in batch verbs: options passed through `...` reach ffm_batch,
# and a multi-input job's provenance manifest records its inputs joined with ";".
# Binary-gated: needs ffmpeg to run and ffprobe for verification.

test_that("concatenate_videos_batch() forwards verify + records a multi-input manifest", {
  skip_if_no_ffmpeg()
  skip_if_no_ffprobe()
  src <- make_test_video()                 # 64x64 test clip with audio
  dir <- withr::local_tempdir()
  jobs <- tibble::tibble(
    inputs = list(c(src, src)),            # two inputs -> one output
    output = file.path(dir, "joined.mp4")
  )
  res <- concatenate_videos_batch(
    jobs, verify = list(width = 64), manifest = TRUE, checksums = TRUE
  )

  # success + verified columns populate (verify forwarded through `...`).
  expect_true(all(res$success))
  expect_true("verified" %in% names(res))
  expect_true(all(res$verified))

  # The manifest joins the row's two inputs into one ";"-separated cell.
  man <- ffm_manifest(res)
  expect_s3_class(man, "tbl_df")
  expect_equal(nrow(man), 1)
  expect_match(man$input[[1]], ";", fixed = TRUE)
  expect_length(strsplit(man$input[[1]], ";")[[1]], 2)
  # checksums: two input md5s, likewise ";"-joined.
  expect_true("input_md5" %in% names(man))
  expect_match(man$input_md5[[1]], ";", fixed = TRUE)
})

test_that("compare_videos_batch() forwards verify (binary-gated)", {
  skip_if_no_ffmpeg()
  skip_if_no_ffprobe()
  src <- make_test_video()                 # 64x64
  dir <- withr::local_tempdir()
  jobs <- tibble::tibble(
    inputs = list(c(src, src)),
    output = file.path(dir, "cmp.mp4")
  )
  # A horizontal stack of two 64-wide clips is 128 wide.
  res <- compare_videos_batch(jobs, verify = list(width = 128))
  expect_true(all(res$success))
  expect_true(all(res$verified))
})
