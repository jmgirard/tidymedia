# Tests for the ffmpeg-backed task functions. Command-construction paths that
# expose a run = FALSE switch are tested purely; everything else is gated on the
# ffmpeg binary being available.

test_that("segment_video() builds the expected command (run = FALSE)", {
  expect_equal(
    segment_video("in.mp4", c(0, 5), c(5, 10), run = FALSE),
    paste(
      '-y -i "in.mp4" -vcodec copy -acodec copy',
      '-ss 0 -to 5 -sn "in_1.mp4" -ss 5 -to 10 -sn "in_2.mp4"'
    )
  )
})

test_that("segment_video() honours explicit outfiles", {
  cmd <- segment_video(
    "in.mp4", c(0), c(5),
    outfiles = "clip.mp4", run = FALSE
  )
  expect_match(cmd, '-sn "clip.mp4"', fixed = TRUE)
})

test_that("segment_video() rejects mismatched timestamp lengths", {
  expect_error(segment_video("in.mp4", c(0, 5), c(5), run = FALSE))
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

test_that("task verbs reject a missing input file (no binary needed)", {
  missing <- withr::local_tempfile(fileext = ".mp4")  # not created
  expect_error(extract_audio(missing, "out.aac"))
  expect_error(audio_as_mp3(missing, "out.mp3"))
  expect_error(separate_audio_video(missing, "a.aac", "v.mp4"))
  expect_error(crop_video(missing, "out.mp4", 10, 10, 0, 0))
  expect_error(format_for_web(missing, "out.mp4"))
})
