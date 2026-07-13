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

test_that("ffmpeg_codecs() returns a tidy tibble", {
  skip_if_no_ffmpeg()
  cc <- ffmpeg_codecs()
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

test_that("ffmpeg_codecs() sort_by_type toggles ordering", {
  skip_if_no_ffmpeg()
  by_name <- ffmpeg_codecs(sort_by_type = FALSE)
  expect_false(is.unsorted(by_name$name))
})

test_that("ffmpeg_encoders() returns a tidy tibble", {
  skip_if_no_ffmpeg()
  ee <- ffmpeg_encoders()
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

test_that("extract_audio(audio_codec=) sets the audio codec", {
  f <- make_input()
  cmd <- extract_audio(f, "out.m4a", audio_codec = "aac", run = FALSE)
  expect_match(cmd, "-codec:a aac -vn", fixed = TRUE)
})

test_that("convert_audio() default (format = NULL) compiles to -q:a 0 -map a", {
  f <- make_input()
  cmd <- convert_audio(f, "out.mp3", run = FALSE)
  expect_match(cmd, "-q:a 0 -map a", fixed = TRUE)
})

test_that("convert_audio(format=) pins -codec:a and drops -q:a", {
  f <- make_input()
  cmd <- convert_audio(f, "out.m4a", format = "aac", run = FALSE)
  expect_match(cmd, "-codec:a aac", fixed = TRUE)
  expect_match(cmd, "-map a", fixed = TRUE)
  expect_no_match(cmd, "-q:a", fixed = TRUE)
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

test_that("standardize_video() compiles the full standardization command", {
  f <- make_input()
  cmd <- standardize_video(
    f, "out.mp4",
    width = 1280, height = 720, fps = 30,
    video_codec = "libx264", pixel_format = "yuv420p",
    run = FALSE
  )
  expect_equal(
    cmd,
    sprintf(
      paste(
        '-y -i "%s" -vf "scale=w=1280:h=720,fps=30"',
        "-codec:v libx264 -codec:a copy -pix_fmt yuv420p -movflags +faststart",
        '"out.mp4"'
      ),
      f
    )
  )
})

test_that("standardize_video() preserves aspect when only one dimension is given", {
  f <- make_input()
  w_only <- standardize_video(f, "out.mp4", width = 640, run = FALSE)
  expect_match(w_only, '-vf "scale=w=640:h=-2', fixed = TRUE)
  h_only <- standardize_video(f, "out.mp4", height = 480, run = FALSE)
  expect_match(h_only, '-vf "scale=w=-2:h=480', fixed = TRUE)
})

test_that("standardize_video() forces exact dimensions when both are given", {
  f <- make_input()
  cmd <- standardize_video(f, "out.mp4", width = 640, height = 480, run = FALSE)
  expect_match(cmd, "scale=w=640:h=480", fixed = TRUE)
})

test_that("standardize_video() applies the even-dimension safeguard, not scale, when no dimension is given", {
  f <- make_input()
  cmd <- standardize_video(f, "out.mp4", fps = 25, run = FALSE)
  expect_no_match(cmd, "scale=", fixed = TRUE)
  expect_match(cmd, '-vf "crop=w=floor(in_w/2)*2:h=floor(in_h/2)*2', fixed = TRUE)
  expect_match(cmd, ',fps=25"', fixed = TRUE)
})

test_that("standardize_video() defaults are deterministic and documented", {
  f <- make_input()
  cmd1 <- standardize_video(f, "out.mp4", run = FALSE)
  cmd2 <- standardize_video(f, "out.mp4", run = FALSE)
  expect_identical(cmd1, cmd2)
  expect_equal(
    cmd1,
    sprintf(
      paste(
        '-y -i "%s"',
        '-vf "crop=w=floor(in_w/2)*2:h=floor(in_h/2)*2:x=(in_w-out_w)/2:y=(in_h-out_h)/2"',
        "-codec:v libx264 -codec:a copy -pix_fmt yuv420p -movflags +faststart",
        '"out.mp4"'
      ),
      f
    )
  )
})

test_that("standardize_video() validates dimensions, fps, and the input file", {
  f <- make_input()
  expect_error(standardize_video(f, "out.mp4", width = 0, run = FALSE))
  expect_error(standardize_video(f, "out.mp4", height = -10, run = FALSE))
  expect_error(standardize_video(f, "out.mp4", fps = 0, run = FALSE))
  missing <- file.path(tempdir(), "tidymedia-does-not-exist.mp4")
  expect_error(standardize_video(missing, "out.mp4", run = FALSE))
})

test_that("standardize_video() writes an output with the requested fps and width", {
  skip_if_no_mediainfo()
  infile <- make_test_video()
  outfile <- withr::local_tempfile(fileext = ".mp4")
  standardize_video(infile, outfile, width = 48, height = 32, fps = 5)
  expect_true(file.exists(outfile))
  expect_equal(get_width(outfile), 48)
  expect_equal(get_frame_rate(outfile), 5)
})

test_that("standardize_video() default path encodes an odd-dimensioned source", {
  # Regression: without the even-dimension safeguard the default libx264 /
  # yuv420p re-encode rejects odd dimensions and writes a 0-byte file.
  skip_if_no_ffmpeg()
  infile <- withr::local_tempfile(fileext = ".mp4")
  ffmpeg(sprintf(
    "-y -f lavfi -i testsrc=duration=1:size=65x49:rate=10 -pix_fmt yuv444p \"%s\"",
    infile
  ))
  skip_if_not(file.exists(infile), "odd-dimensioned test video could not be generated")
  outfile <- withr::local_tempfile(fileext = ".mp4")
  standardize_video(infile, outfile)
  expect_true(file.exists(outfile))
  expect_gt(file.size(outfile), 0)
})

test_that("standardize_video() stream-copies audio unchanged", {
  # Regression: a bare video re-encode transcodes audio to the container
  # default; -c:a copy must preserve the source audio codec.
  skip_if_no_ffprobe()
  infile <- withr::local_tempfile(fileext = ".mp4")
  ffmpeg(sprintf(paste(
    "-y -f lavfi -i testsrc=duration=1:size=64x64:rate=10",
    "-f lavfi -i sine=frequency=440:duration=1",
    "-c:a libmp3lame -shortest -pix_fmt yuv420p \"%s\""
  ), infile))
  skip_if_not(file.exists(infile), "test video could not be generated")
  outfile <- withr::local_tempfile(fileext = ".mp4")
  standardize_video(infile, outfile)
  acodec <- function(path) ffprobe(sprintf(
    "-v error -select_streams a:0 -show_entries stream=codec_name -of csv=p=0 \"%s\"",
    path
  ))
  expect_equal(acodec(outfile), acodec(infile))
})

test_that("extract_frame() compiles to a fast input-seek single-frame grab", {
  f <- make_input()
  cmd <- extract_frame(f, "out.png", timestamp = 1.5, run = FALSE)
  expect_match(cmd, "-ss 1.5 -i", fixed = TRUE)
  expect_match(cmd, "-frames:v 1", fixed = TRUE)
})

# sample_frames() (fixed-rate frame sampling) --------------------------------

test_that("sample_frames() compiles to an fps filter into an image2 pattern", {
  f <- make_input()
  d <- withr::local_tempdir()
  cmd <- sample_frames(f, d, fps = 2, run = FALSE)
  expect_match(cmd, '-vf "fps=2"', fixed = TRUE)
  # Output is a zero-padded printf pattern under outdir, stem from the input.
  expect_match(cmd, "_%06d.png", fixed = TRUE)
  base <- tools::file_path_sans_ext(basename(f))
  expect_match(cmd, file.path(d, paste0(base, "_%06d.png")), fixed = TRUE)
})

test_that("sample_frames() maps interval to the reciprocal frame rate", {
  f <- make_input()
  d <- withr::local_tempdir()
  # interval = 0.5 s between frames -> fps = 1/0.5 = 2.
  expect_identical(
    sample_frames(f, d, interval = 0.5, run = FALSE),
    sample_frames(f, d, fps = 2, run = FALSE)
  )
  expect_match(sample_frames(f, d, interval = 4, run = FALSE),
               '-vf "fps=0.25"', fixed = TRUE)
})

test_that("sample_frames() requires exactly one of fps/interval", {
  f <- make_input()
  d <- withr::local_tempdir()
  expect_error(sample_frames(f, d, run = FALSE), "exactly one")
  expect_error(sample_frames(f, d, fps = 2, interval = 1, run = FALSE),
               "exactly one")
})

test_that("sample_frames() accepts a bare-integer rate (M20 coercion)", {
  f <- make_input()
  d <- withr::local_tempdir()
  # 2L must not trip ffm_fps()'s check_dim() integer rejection.
  expect_match(sample_frames(f, d, fps = 2L, run = FALSE), '-vf "fps=2"',
               fixed = TRUE)
})

test_that("sample_frames() honors format and prefix", {
  f <- make_input()
  d <- withr::local_tempdir()
  cmd <- sample_frames(f, d, fps = 1, format = "jpg", prefix = "shot",
                       run = FALSE)
  expect_match(cmd, file.path(d, "shot_%06d.jpg"), fixed = TRUE)
})

test_that("sample_frames() rejects a non-image format", {
  f <- make_input()
  d <- withr::local_tempdir()
  expect_error(sample_frames(f, d, fps = 1, format = "mp4", run = FALSE),
               "image format")
})

test_that("sample_frames() rejects a non-positive rate", {
  f <- make_input()
  d <- withr::local_tempdir()
  expect_error(sample_frames(f, d, fps = 0, run = FALSE), "positive")
  expect_error(sample_frames(f, d, interval = -1, run = FALSE), "positive")
})

test_that("sample_frames() rejects a missing input file", {
  d <- withr::local_tempdir()
  missing <- withr::local_tempfile(fileext = ".mp4")  # not created
  expect_error(sample_frames(missing, d, fps = 1), "exist")
})

test_that("sample_frames() aborts on an uncreatable output directory", {
  f <- make_input()
  # A path *under* an existing file cannot be created as a directory.
  blocker <- make_input()
  expect_error(sample_frames(f, file.path(blocker, "sub"), fps = 1,
                             run = FALSE),
               "directory")
})

test_that("sample_frames() writes a numbered sequence at the requested rate", {
  skip_if_no_ffmpeg()
  v <- make_test_video()  # 2 s at 10 fps
  d <- withr::local_tempdir()
  sample_frames(v, d, fps = 2)
  files <- sort(list.files(d, pattern = "\\.png$"))
  # 2 fps over 2 s -> ~4 frames (allow the boundary +/- 1).
  expect_gte(length(files), 3)
  expect_lte(length(files), 5)
  # Zero-padded, sequential numbering starting at 1.
  expect_match(files[[1]], "_000001\\.png$")
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
  expect_error(convert_audio(missing, "out.mp3"))
  expect_error(separate_audio_video(missing, "a.aac", "v.mp4"))
  expect_error(crop_video(missing, "out.mp4", 10, 10, 0, 0))
  expect_error(format_for_web(missing, "out.mp4"))
})

# compare_videos() (Layer-2 on hstack/vstack) ----------------------------------

test_that("compare_videos() defaults to a horizontal resized stack", {
  f1 <- make_input()
  f2 <- make_input()
  cmd <- compare_videos(c(f1, f2), "out.mp4", run = FALSE)
  expect_match(cmd, "scale2ref", fixed = TRUE)
  expect_match(cmd, 'hstack,setsar=1[vout]" -map "[vout]"', fixed = TRUE)
})

test_that("compare_videos(direction = 'vertical') stacks with vstack", {
  f1 <- make_input()
  f2 <- make_input()
  cmd <- compare_videos(c(f1, f2), "out.mp4", direction = "vertical",
                        run = FALSE)
  expect_match(cmd, 'vstack,setsar=1[vout]" -map "[vout]"', fixed = TRUE)
})

test_that("compare_videos(resize = FALSE) accepts more than two inputs", {
  f1 <- make_input()
  f2 <- make_input()
  f3 <- make_input()
  cmd <- compare_videos(c(f1, f2, f3), "out.mp4", resize = FALSE, run = FALSE)
  expect_match(cmd, "hstack=inputs=3:shortest=0[vout]", fixed = TRUE)
})

test_that("compare_videos(audio = ) carries that input's audio via -map", {
  f1 <- make_input()
  f2 <- make_input()
  cmd <- compare_videos(c(f1, f2), "out.mp4", audio = 1, run = FALSE)
  expect_match(cmd, '-map "[vout]" -map 1:a', fixed = TRUE)
})

test_that("compare_videos() drops audio by default (no extra -map)", {
  f1 <- make_input()
  f2 <- make_input()
  cmd <- compare_videos(c(f1, f2), "out.mp4", run = FALSE)
  expect_no_match(cmd, ":a", fixed = TRUE)
})

test_that("compare_videos() validates inputs, resize arity, and audio index", {
  f1 <- make_input()
  f2 <- make_input()
  expect_error(compare_videos(f1, "out.mp4", run = FALSE), "two")
  expect_error(
    compare_videos(c(f1, f2, f1), "out.mp4", run = FALSE),
    "exactly two"
  )
  expect_error(
    compare_videos(c(f1, f2), "out.mp4", audio = 5, run = FALSE)
  )
})

test_that("compare_videos() runs through ffmpeg (binary-gated)", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  b <- make_test_video()
  out <- withr::local_tempfile(fileext = ".mp4")
  compare_videos(c(a, b), out, audio = 0)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

# picture_in_picture() (Layer-2 on overlay) ------------------------------------

test_that("picture_in_picture() insets a scaled overlay top-right by default", {
  m <- make_input()
  o <- make_input()
  cmd <- picture_in_picture(m, o, "out.mp4", run = FALSE)
  expect_match(cmd, "scale2ref=w='main_w*0.25'", fixed = TRUE)
  expect_match(cmd, "overlay=x=main_w-overlay_w-16:y=16", fixed = TRUE)
})

test_that("picture_in_picture(position = ) places the inset per corner/center", {
  m <- make_input()
  o <- make_input()
  expect_match(
    picture_in_picture(m, o, "out.mp4", position = "topleft", run = FALSE),
    "overlay=x=16:y=16", fixed = TRUE
  )
  expect_match(
    picture_in_picture(m, o, "out.mp4", position = "bottomright", run = FALSE),
    "overlay=x=main_w-overlay_w-16:y=main_h-overlay_h-16", fixed = TRUE
  )
  expect_match(
    picture_in_picture(m, o, "out.mp4", position = "center", run = FALSE),
    "overlay=x=(main_w-overlay_w)/2:y=(main_h-overlay_h)/2", fixed = TRUE
  )
})

test_that("picture_in_picture() honours scale and margin", {
  m <- make_input()
  o <- make_input()
  cmd <- picture_in_picture(m, o, "out.mp4", scale = 0.4, margin = 8,
                            run = FALSE)
  expect_match(cmd, "scale2ref=w='main_w*0.4'", fixed = TRUE)
  expect_match(cmd, "overlay=x=main_w-overlay_w-8:y=8", fixed = TRUE)
})

test_that("picture_in_picture(audio = ) carries that input's audio", {
  m <- make_input()
  o <- make_input()
  cmd <- picture_in_picture(m, o, "out.mp4", audio = 0, run = FALSE)
  expect_match(cmd, '-map "[vout]" -map 0:a', fixed = TRUE)
})

test_that("picture_in_picture() validates scale, margin, and audio", {
  m <- make_input()
  o <- make_input()
  expect_error(picture_in_picture(m, o, "out.mp4", scale = 2, run = FALSE))
  expect_error(picture_in_picture(m, o, "out.mp4", audio = 2, run = FALSE))
  missing <- withr::local_tempfile(fileext = ".mp4")
  expect_error(picture_in_picture(missing, o, "out.mp4", run = FALSE))
})

test_that("picture_in_picture() runs through ffmpeg (binary-gated)", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  b <- make_test_video()
  out <- withr::local_tempfile(fileext = ".mp4")
  picture_in_picture(a, b, out, audio = 0)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

# M06: separate_audio_video() stream-copy default (D-M06-4) ----------------------

test_that("separate_audio_video() stream-copies by default", {
  f <- make_input()
  cmds <- separate_audio_video(f, "a.aac", "v.mp4", run = FALSE)
  expect_match(cmds[["audio"]], "-codec:a copy", fixed = TRUE)
  expect_match(cmds[["video"]], "-codec:v copy", fixed = TRUE)
})

test_that("separate_audio_video(reencode = TRUE) omits the stream copy", {
  f <- make_input()
  cmds <- separate_audio_video(f, "a.aac", "v.mp4", reencode = TRUE,
                               run = FALSE)
  expect_no_match(cmds[["audio"]], "copy", fixed = TRUE)
  expect_no_match(cmds[["video"]], "copy", fixed = TRUE)
})

test_that("separate_audio_video() copy outputs exist and are nonempty", {
  skip_if_no_ffmpeg()
  src <- make_test_video()
  dir <- withr::local_tempdir()
  audiofile <- file.path(dir, "a.aac")
  videofile <- file.path(dir, "v.mp4")
  separate_audio_video(src, audiofile, videofile)
  expect_true(file.exists(audiofile))
  expect_gt(file.size(audiofile), 0)
  expect_true(file.exists(videofile))
  expect_gt(file.size(videofile), 0)
})
