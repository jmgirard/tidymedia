# Tests for the ffmpeg-backed task functions. Command-construction paths that
# expose a run = FALSE switch are tested purely; everything else is gated on the
# ffmpeg binary being available.

test_that("segment_video() returns a job tibble with one accurate-cut command per segment", {
  f <- make_input()
  res <- segment_video(f, c(0, 5), c(5, 10), outfiles = c("a.mp4", "b.mp4"), run = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  # Default reencode = TRUE: accurate output-seek (-ss/-to after -i), no copy.
  expect_match(res$command[[1]], '-ss 0 -to 5 "a.mp4"', fixed = TRUE)
  expect_no_match(res$command[[1]], "-codec:v copy", fixed = TRUE)
})

test_that("segment_video(reencode = FALSE) uses the fast copy path", {
  f <- make_input()
  res <- segment_video(f, 0, 5, outfiles = "clip.mp4", reencode = FALSE, run = FALSE)
  expect_match(res$command[[1]], "-ss 0 -to 5 -i", fixed = TRUE)
  expect_match(res$command[[1]], "-codec:v copy -codec:a copy", fixed = TRUE)
  expect_match(res$command[[1]], "-avoid_negative_ts make_zero", fixed = TRUE)
})

test_that("segment_video() defaults outfiles from the input name", {
  f <- make_input()
  res <- segment_video(f, c(0), c(5), run = FALSE)
  expect_match(res$output[[1]], "_1", fixed = TRUE)
})

test_that("segment_video() rejects mismatched timestamp lengths", {
  f <- make_input()
  expect_error(segment_video(f, c(0, 5), c(5)), "same length")
})

test_that("get_codecs() returns a tidy tibble", {
  skip_if_no_ffmpeg()
  cc <- get_codecs()
  expect_s3_class(cc, "tbl_df")
  expect_setequal(
    names(cc),
    c("name", "details", "type", "decoding", "encoding",
      "intraframe", "lossy", "lossless")
  )
  expect_s3_class(cc$type, "factor")
  expect_type(cc$decoding, "logical")
  expect_gt(nrow(cc), 0)
})

test_that("get_codecs() sort_by_type toggles ordering", {
  skip_if_no_ffmpeg()
  by_name <- get_codecs(sort_by_type = FALSE)
  expect_false(is.unsorted(by_name$name))
})

test_that("get_encoders() returns a tidy tibble", {
  skip_if_no_ffmpeg()
  ee <- get_encoders()
  expect_s3_class(ee, "tbl_df")
  expect_setequal(
    names(ee),
    c("name", "details", "type", "frame_mt", "slice_mt",
      "experimental", "horiz_band", "direct_render")
  )
  expect_gt(nrow(ee), 0)
})

test_that("extract_audio() writes an audio file", {
  infile <- make_test_video()
  outfile <- withr::local_tempfile(fileext = ".aac")
  extract_audio(infile, outfile)
  expect_true(file.exists(outfile))
  expect_gt(file.size(outfile), 0)
})

test_that("segment_video() writes the segments when run", {
  infile <- make_test_video()
  outfiles <- withr::local_tempfile(fileext = ".mp4", pattern = c("a", "b"))
  segment_video(infile, c(0, 1), c(1, 2), outfiles = outfiles)
  expect_true(all(file.exists(outfiles)))
})

test_that("extract_frame() writes an image", {
  infile <- make_test_video()
  outfile <- withr::local_tempfile(fileext = ".png")
  extract_frame(infile, outfile, timestamp = 0.5)
  expect_true(file.exists(outfile))
  expect_gt(file.size(outfile), 0)
})

test_that("extract_frame() requires exactly one of timestamp/frame", {
  infile <- make_test_video()
  outfile <- withr::local_tempfile(fileext = ".png")
  expect_error(extract_frame(infile, outfile))
})

# Migrated single-output verbs: pure command tests (run = FALSE, no binary) ----

test_that("extract_audio() compiles to a copy + drop-video command", {
  f <- make_input()
  cmd <- extract_audio(f, "out.aac", run = FALSE)
  expect_match(cmd, "-codec:a copy -vn", fixed = TRUE)
  expect_match(cmd, '"out.aac"', fixed = TRUE)
})

test_that("extract_audio(acodec=) sets the audio codec", {
  f <- make_input()
  cmd <- extract_audio(f, "out.m4a", acodec = "aac", run = FALSE)
  expect_match(cmd, "-codec:a aac -vn", fixed = TRUE)
})

test_that("audio_as_mp3() compiles to -q:a 0 -map a", {
  f <- make_input()
  cmd <- audio_as_mp3(f, "out.mp3", run = FALSE)
  expect_match(cmd, "-q:a 0 -map a", fixed = TRUE)
})

test_that("crop_video() compiles to a crop filter mapping all streams", {
  f <- make_input()
  cmd <- crop_video(f, "out.mp4", width = 100, height = 50, x = 0, y = 0, run = FALSE)
  expect_match(cmd, '-vf "crop=w=100:h=50:x=0:y=0" -map 0', fixed = TRUE)
})

test_that("format_for_web() compiles to the web-friendly re-encode", {
  f <- make_input()
  cmd <- format_for_web(f, "out.mp4", run = FALSE)
  expect_match(cmd, "-codec:v libx264 -codec:a aac", fixed = TRUE)
  expect_match(cmd, "-pix_fmt yuv420p", fixed = TRUE)
  expect_match(cmd, "-movflags +faststart", fixed = TRUE)
  expect_match(cmd, "crop=w=floor(in_w/2)*2", fixed = TRUE)
})

test_that("extract_frame() compiles to a fast input-seek single-frame grab", {
  f <- make_input()
  cmd <- extract_frame(f, "out.png", timestamp = 1.5, run = FALSE)
  expect_match(cmd, "-ss 1.5 -i", fixed = TRUE)
  expect_match(cmd, "-frames:v 1", fixed = TRUE)
})

test_that("separate_audio_video() emits two single-output mapped commands", {
  f <- make_input()
  cmds <- separate_audio_video(f, "a.aac", "v.mp4", run = FALSE)
  expect_named(cmds, c("audio", "video"))
  expect_match(cmds[["audio"]], '-map 0:a "a.aac"', fixed = TRUE)
  expect_match(cmds[["video"]], '-map 0:v "v.mp4"', fixed = TRUE)
})

test_that("concatenate_videos() compiles to the concat demuxer", {
  f1 <- make_input()
  f2 <- make_input()
  cmd <- concatenate_videos(c(f1, f2), "out.mp4", run = FALSE)
  expect_match(cmd, "-f concat -safe 0 -i ", fixed = TRUE)
  expect_match(cmd, "-codec:v copy -codec:a copy -map 0", fixed = TRUE)
})

test_that("concatenate_videos() warns on mixed extensions", {
  f1 <- make_input("mp4")
  f2 <- make_input("mkv")
  expect_warning(
    concatenate_videos(c(f1, f2), "out.mp4", run = FALSE),
    "same extension"
  )
})

test_that("concatenate_videos() joins inputs end to end (binary-gated)", {
  skip_if_no_ffmpeg()
  skip_if_no_ffprobe()
  a <- make_test_video()
  b <- make_test_video()
  out <- withr::local_tempfile(fileext = ".mp4")
  concatenate_videos(c(a, b), out)
  expect_true(file.exists(out))
  expect_gt(probe_duration(out), 3.5)
})

test_that("task verbs reject a missing input file (no binary needed)", {
  missing <- withr::local_tempfile(fileext = ".mp4")  # not created
  expect_error(extract_audio(missing, "out.aac"))
  expect_error(audio_as_mp3(missing, "out.mp3"))
  expect_error(separate_audio_video(missing, "a.aac", "v.mp4"))
  expect_error(crop_video(missing, "out.mp4", 10, 10, 0, 0))
  expect_error(format_for_web(missing, "out.mp4"))
})
