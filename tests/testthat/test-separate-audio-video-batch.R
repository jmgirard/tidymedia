# Tests for separate_audio_video_batch(): the fan-out batch sibling of
# separate_audio_video(). Each input row reshapes into TWO single-output rows
# (one audio, one video), so N input rows return 2N rows. Command construction is
# tested purely (run = FALSE); execution + ffm_batch forwarding are binary-gated.

# AC1: thin fan-out — N inputs -> 2N single-output rows --------------------

test_that("separate_audio_video_batch() reshapes N inputs into 2N single-output rows", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(
    input     = c(f1, f2),
    audiofile = c("a1.aac", "a2.aac"),
    videofile = c("v1.mp4", "v2.mp4")
  )
  res <- separate_audio_video_batch(jobs, run = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 4)                       # 2 inputs -> 4 rows
  expect_equal(res$stream, rep(c("audio", "video"), 2))
  expect_equal(res$output, c("a1.aac", "v1.mp4", "a2.aac", "v2.mp4"))
  # Audio rows map 0:a into audiofile; video rows map 0:v into videofile.
  expect_match(res$command[[1]], '-map 0:a "a1.aac"', fixed = TRUE)
  expect_match(res$command[[2]], '-map 0:v "v1.mp4"', fixed = TRUE)
  expect_match(res$command[[3]], '-map 0:a "a2.aac"', fixed = TRUE)
  expect_match(res$command[[4]], '-map 0:v "v2.mp4"', fixed = TRUE)
  # Each row carries its own input file.
  expect_match(res$command[[1]], f1, fixed = TRUE)
  expect_match(res$command[[3]], f2, fixed = TRUE)
})

test_that("separate_audio_video_batch() glues nothing: commands equal the scalar verb's", {
  # Thin Layer-2 fan-out (IP1/D007): the batch commands must be byte-identical to
  # separate_audio_video()'s two commands for the same job.
  f <- make_input()
  batch <- separate_audio_video_batch(
    tibble::tibble(input = f, audiofile = "a.aac", videofile = "v.mp4"),
    run = FALSE
  )
  scalar <- separate_audio_video(f, "a.aac", "v.mp4", run = FALSE)
  expect_equal(batch$command[[1]], unname(scalar[["audio"]]))
  expect_equal(batch$command[[2]], unname(scalar[["video"]]))
})

# AC2: jobs-table guards + reencode override ------------------------------

test_that("separate_audio_video_batch() rejects a non-data-frame jobs", {
  expect_error(separate_audio_video_batch(list(input = "a"), run = FALSE), "data frame")
})

test_that("separate_audio_video_batch() rejects an empty jobs table", {
  jobs <- tibble::tibble(input = character(), audiofile = character(),
                         videofile = character())
  expect_error(separate_audio_video_batch(jobs, run = FALSE), "at least one row")
})

test_that("separate_audio_video_batch() names a missing input column", {
  jobs <- tibble::tibble(audiofile = "a.aac", videofile = "v.mp4")
  expect_error(separate_audio_video_batch(jobs, run = FALSE), "input")
})

test_that("separate_audio_video_batch() names a missing output column", {
  f <- make_input()
  # audiofile present, videofile absent.
  jobs <- tibble::tibble(input = f, audiofile = "a.aac")
  expect_error(separate_audio_video_batch(jobs, run = FALSE), "Missing column")
})

test_that("separate_audio_video_batch() rejects NA in input", {
  jobs <- tibble::tibble(input = NA_character_, audiofile = "a.aac", videofile = "v.mp4")
  expect_error(separate_audio_video_batch(jobs, run = FALSE), "input")
})

test_that("separate_audio_video_batch() rejects NA in audiofile or videofile", {
  f <- make_input()
  bad_a <- tibble::tibble(input = f, audiofile = NA_character_, videofile = "v.mp4")
  expect_error(separate_audio_video_batch(bad_a, run = FALSE), "audiofile")
  bad_v <- tibble::tibble(input = f, audiofile = "a.aac", videofile = NA_character_)
  expect_error(separate_audio_video_batch(bad_v, run = FALSE), "videofile")
})

test_that("separate_audio_video_batch() default reencode = FALSE stream-copies each stream", {
  f <- make_input()
  res <- separate_audio_video_batch(
    tibble::tibble(input = f, audiofile = "a.aac", videofile = "v.mp4"),
    run = FALSE
  )
  expect_match(res$command[[1]], "-codec:a copy", fixed = TRUE)   # audio row
  expect_match(res$command[[2]], "-codec:v copy", fixed = TRUE)   # video row
})

test_that("separate_audio_video_batch(reencode = TRUE) omits the stream copy", {
  f <- make_input()
  res <- separate_audio_video_batch(
    tibble::tibble(input = f, audiofile = "a.aac", videofile = "v.mp4"),
    reencode = TRUE, run = FALSE
  )
  expect_no_match(res$command[[1]], "-codec:a copy", fixed = TRUE)
  expect_no_match(res$command[[2]], "-codec:v copy", fixed = TRUE)
})

test_that("separate_audio_video_batch() reencode column overrides the arg per row", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(
    input     = c(f1, f2),
    audiofile = c("a1.aac", "a2.aac"),
    videofile = c("v1.mp4", "v2.mp4"),
    reencode  = c(FALSE, TRUE)
  )
  # Arg says TRUE, but the column wins per input (and both of that input's rows).
  res <- separate_audio_video_batch(jobs, reencode = TRUE, run = FALSE)
  # Input 1 (reencode = FALSE) -> copy on its audio and video rows (1, 2)...
  expect_match(res$command[[1]], "-codec:a copy", fixed = TRUE)
  expect_match(res$command[[2]], "-codec:v copy", fixed = TRUE)
  # ...input 2 (reencode = TRUE) -> no copy on its rows (3, 4).
  expect_no_match(res$command[[3]], "-codec:a copy", fixed = TRUE)
  expect_no_match(res$command[[4]], "-codec:v copy", fixed = TRUE)
})

test_that("separate_audio_video_batch() rejects a non-logical reencode column", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, audiofile = "a.aac", videofile = "v.mp4",
                         reencode = "yes")
  expect_error(separate_audio_video_batch(jobs, run = FALSE), "reencode")
})

test_that("separate_audio_video_batch() rejects an NA in the reencode column", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(
    input = c(f1, f2), audiofile = c("a1.aac", "a2.aac"),
    videofile = c("v1.mp4", "v2.mp4"), reencode = c(TRUE, NA)
  )
  expect_error(separate_audio_video_batch(jobs, run = FALSE), "reencode")
})

# AC3: pooled duplicate-path guard across both output columns --------------

test_that("separate_audio_video_batch() rejects a cross-column output collision", {
  f1 <- make_input()
  f2 <- make_input()
  # audiofile of row 1 collides with videofile of row 2.
  jobs <- tibble::tibble(
    input     = c(f1, f2),
    audiofile = c("clash.dat", "a2.aac"),
    videofile = c("v1.mp4", "clash.dat")
  )
  expect_error(separate_audio_video_batch(jobs, run = FALSE), "same output path")
})

test_that("separate_audio_video_batch() rejects within-row audiofile == videofile", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, audiofile = "same.x", videofile = "same.x")
  expect_error(separate_audio_video_batch(jobs, run = FALSE), "same output path")
})

# AC4: return-schema parity with segment_video_batch + stream marker -------

test_that("separate_audio_video_batch() return schema matches the other batch verbs plus a stream marker", {
  f <- make_input()
  sep <- separate_audio_video_batch(
    tibble::tibble(input = f, audiofile = "a.aac", videofile = "v.mp4"),
    run = FALSE
  )
  seg <- segment_video_batch(
    tibble::tibble(input = f, output = "o.mp4", start = 0, end = 1),
    run = FALSE
  )
  # Same core batch columns as segment_video_batch (single output + command)...
  expect_true(all(c("input", "output", "command") %in% names(sep)))
  expect_equal(class(sep), class(seg))
  expect_type(sep$output, "character")
  expect_type(sep$command, "character")
  # ...plus the fan-out stream marker distinguishing each row's stream.
  expect_true("stream" %in% names(sep))
  expect_setequal(sep$stream, c("audio", "video"))
})

# Execution + ffm_batch forwarding (binary-gated) -------------------------

test_that("separate_audio_video_batch() writes both streams end to end (binary-gated)", {
  skip_if_no_ffmpeg()
  src <- make_test_video()      # carries both a video and an audio track
  dir <- withr::local_tempdir()
  jobs <- tibble::tibble(
    input     = c(src, src),
    audiofile = file.path(dir, c("a1.aac", "a2.aac")),
    videofile = file.path(dir, c("v1.mp4", "v2.mp4"))
  )
  res <- separate_audio_video_batch(jobs)   # default reencode = FALSE (copy)
  expect_equal(nrow(res), 4)                # 2 inputs -> audio + video rows each
  expect_true(all(res$success))
  expect_true(all(file.exists(res$output)))
  expect_true(all(file.size(res$output) > 0))
})
