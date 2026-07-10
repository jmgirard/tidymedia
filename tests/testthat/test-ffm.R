# Pure tests for the Layer 1 ffm_* builder and ffm_compile(). These need no
# binaries: they assert the compiled command *string*. The M02 rework replaced
# the old string-fragment model with a structured one, so these assert the
# *correct* command (the KNOWN-BUG characterizations from M01 are gone).
#
# End-to-end execution tests (binary-gated) live at the bottom of the file.

# Object construction ----------------------------------------------------------

test_that("ffm_files() constructs a tidymedia_ffm object", {
  f <- make_input()
  p <- ffm_files(f, "out.mp4")
  expect_s3_class(p, "tidymedia_ffm")
  expect_equal(p$input, f)
  expect_equal(p$output, "out.mp4")
  expect_true(p$overwrite)
  expect_false(p$complex)
})

test_that("new_ffm() no longer carries the dead trim_start/trim_end fields", {
  f <- make_input()
  p <- ffm_files(f, "out.mp4")
  expect_false("trim_start" %in% names(p))
  expect_false("trim_end" %in% names(p))
  expect_false("drop_streams" %in% names(p))
})

test_that("ffm is an alias for ffm_files", {
  expect_identical(ffm, ffm_files)
})

test_that("ffm_files() rejects unreadable input", {
  expect_error(ffm_files(tempfile(fileext = ".mp4"), "out.mp4"))
})

# ffm_compile() minimal / global options ---------------------------------------

test_that("ffm_compile() renders a minimal pipeline with -y before -i", {
  f <- make_input()
  p <- ffm_files(f, "out.mp4")
  expect_equal(ffm_compile(p), sprintf('-y -i "%s" "out.mp4"', f))
})

test_that("overwrite = FALSE emits -n as a global option before -i", {
  f <- make_input()
  p <- ffm_files(f, "out.mp4", overwrite = FALSE)
  expect_equal(ffm_compile(p), sprintf('-n -i "%s" "out.mp4"', f))
})

# ffm_trim() -------------------------------------------------------------------

test_that("ffm_trim() adds a trim + setpts filter via -vf (tds units)", {
  f <- make_input()
  p <- ffm_trim(ffm_files(f, "out.mp4"), start = 1, end = 2)
  expect_equal(
    ffm_compile(p),
    sprintf(
      '-y -i "%s" -vf "trim=start=1:end=2,setpts=PTS-STARTPTS" "out.mp4"',
      f
    )
  )
})

test_that("ffm_trim() honours pts units and drops NULL bounds", {
  f <- make_input()
  p <- ffm_trim(ffm_files(f, "out.mp4"), start = 10, units = "pts")
  expect_match(p$filter_video[[1]], "trim=start_pts=10", fixed = TRUE)
  expect_false(grepl("end", p$filter_video[[1]], fixed = TRUE))
})

test_that("ffm_trim(setpts = FALSE) omits the setpts filter (M02 fix)", {
  f <- make_input()
  p <- ffm_trim(ffm_files(f, "out.mp4"), start = 1, end = 2, setpts = FALSE)
  expect_false(any(p$filter_video == "setpts=PTS-STARTPTS"))
  expect_no_match(ffm_compile(p), "setpts")
})

test_that("ffm_trim(setpts = TRUE) keeps the setpts filter", {
  f <- make_input()
  p <- ffm_trim(ffm_files(f, "out.mp4"), start = 1, end = 2, setpts = TRUE)
  expect_true(any(p$filter_video == "setpts=PTS-STARTPTS"))
})

# ffm_drop() -------------------------------------------------------------------

test_that("ffm_drop() stores stream names and compiles them as output options", {
  f <- make_input()
  p <- ffm_drop(ffm_files(f, "out.mp4"), c("audio", "subtitles"))
  expect_equal(p$drop, c("audio", "subtitles"))
  # M02 fix: drop flags are output options *after* -i, not prepended before it.
  expect_equal(ffm_compile(p), sprintf('-y -i "%s" -an -sn "out.mp4"', f))
})

# ffm_crop() / ffm_scale() -----------------------------------------------------

test_that("ffm_crop() adds a crop filter with default x/y expressions", {
  f <- make_input()
  p <- ffm_crop(ffm_files(f, "out.mp4"), width = 100, height = 50)
  expect_equal(
    p$filter_video,
    "crop=w=100:h=50:x=(in_w-out_w)/2:y=(in_h-out_h)/2"
  )
})

test_that("ffm_scale() adds a scale filter", {
  f <- make_input()
  p <- ffm_scale(ffm_files(f, "out.mp4"), 640, 480)
  expect_equal(p$filter_video, "scale=w=640:h=480")
})

test_that("ffm_crop() rejects a non-positive size", {
  f <- make_input()
  p <- ffm_files(f, "out.mp4")
  expect_error(ffm_crop(p, width = 0, height = 10))
  expect_error(ffm_crop(p, width = -5, height = 10))
})

test_that("ffm_crop() accepts FFmpeg expressions as strings", {
  f <- make_input()
  p <- ffm_crop(ffm_files(f, "out.mp4"), width = "in_w/2", height = "in_h")
  expect_match(p$filter_video, "crop=w=in_w/2:h=in_h", fixed = TRUE)
})

# ffm_codec() / ffm_copy() -----------------------------------------------------

test_that("ffm_codec() stores bare codec names, video rendered first", {
  f <- make_input()
  p <- ffm_codec(ffm_files(f, "out.mp4"), audio = "aac", video = "libx264")
  expect_equal(p$codec_audio, "aac")
  expect_equal(p$codec_video, "libx264")
  expect_match(ffm_compile(p), "-codec:v libx264 -codec:a aac ", fixed = TRUE)
})

test_that("ffm_copy() copies codecs and maps all streams", {
  f <- make_input()
  p <- ffm_copy(ffm_files(f, "out.mp4"))
  expect_equal(p$codec_audio, "copy")
  expect_equal(p$codec_video, "copy")
  expect_equal(p$map, "0")
  expect_equal(
    ffm_compile(p),
    sprintf('-y -i "%s" -codec:v copy -codec:a copy -map 0 "out.mp4"', f)
  )
})

# ffm_map() --------------------------------------------------------------------

test_that("ffm_map() sets the stream mapping", {
  f <- make_input()
  p <- ffm_map(ffm_files(f, "out.mp4"), "0:v")
  expect_equal(p$map, "0:v")
  expect_match(ffm_compile(p), "-map 0:v ", fixed = TRUE)
})

# ffm_pixel_format() -----------------------------------------------------------

test_that("ffm_pixel_format() sets the pixel format with correct spacing", {
  f <- make_input()
  p <- ffm_pixel_format(ffm_files(f, "out.mp4"), "yuv420p")
  expect_equal(p$pixel_format, "yuv420p")
  # M02 fix: token no longer abuts the output filename.
  expect_equal(ffm_compile(p), sprintf('-y -i "%s" -pix_fmt yuv420p "out.mp4"', f))
})

# ffm_drawbox() ----------------------------------------------------------------

test_that("ffm_drawbox() adds a drawbox filter", {
  f <- make_input()
  p <- ffm_drawbox(ffm_files(f, "out.mp4"), color = "red")
  expect_equal(
    p$filter_video,
    "drawbox=x=0:y=0:w=in_w:h=in_h:c=red:t=fill"
  )
})

# ffm_hstack() (multi-input, filter_complex path) ------------------------------

test_that("ffm_hstack() flags the pipeline complex and stores a label-free token", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_hstack(ffm_files(c(f1, f2), "out.mp4"))
  expect_true(p$complex)
  expect_equal(p$filter_video, "hstack=inputs=2:shortest=0")
})

test_that("ffm_hstack() compiles to -filter_complex with labels and auto -map", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_hstack(ffm_files(c(f1, f2), "out.mp4"))
  expect_equal(
    ffm_compile(p),
    sprintf(
      paste0(
        '-y -i "%s" -i "%s" ',
        '-filter_complex "[0:v][1:v]hstack=inputs=2:shortest=0[vout]" ',
        '-map "[vout]" "out.mp4"'
      ),
      f1, f2
    )
  )
})

test_that("ffm_hstack() requires more than one input", {
  f <- make_input()
  expect_error(ffm_hstack(ffm_files(f, "out.mp4")))
})

test_that("ffm_hstack() refuses to follow a single-input video filter", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_scale(ffm_files(c(f1, f2), "out.mp4"), 640, 480)
  expect_error(ffm_hstack(p), "before other video filters")
})

test_that("ffm_hstack(resize = TRUE) emits a single-line self-labelled graph", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_hstack(ffm_files(c(f1, f2), "out.mp4"), resize = TRUE)
  expect_true(p$complex)
  # No embedded newlines may leak into the filtergraph token.
  expect_no_match(p$filter_video, "\n", fixed = TRUE)
  cmd <- ffm_compile(p)
  expect_match(cmd, "scale2ref", fixed = TRUE)
  expect_match(cmd, 'hstack,setsar=1[vout]" -map "[vout]"', fixed = TRUE)
})

# Filter emission (no invalid -filter_complex:v anywhere) ----------------------

test_that("single-input filter chains compile to -vf, never -filter_complex:v", {
  f <- make_input()
  p <- ffm_crop(ffm_scale(ffm_files(f, "out.mp4"), 640, 480), width = 100, height = 50)
  cmd <- ffm_compile(p)
  expect_match(cmd, "-vf ", fixed = TRUE)
  expect_no_match(cmd, "-filter_complex", fixed = TRUE)
})

test_that("no compiled command emits the invalid -filter_complex:v/:a syntax", {
  f1 <- make_input()
  f2 <- make_input()
  simple <- ffm_compile(ffm_scale(ffm_files(f1, "out.mp4"), 640, 480))
  complex <- ffm_compile(ffm_hstack(ffm_files(c(f1, f2), "out.mp4")))
  expect_no_match(simple, "-filter_complex:", fixed = TRUE)
  expect_no_match(complex, "-filter_complex:", fixed = TRUE)
})

# copy + filter guard ----------------------------------------------------------

test_that("ffm_compile() aborts on a video codec copy + video filter", {
  f <- make_input()
  p <- ffm_scale(ffm_copy(ffm_files(f, "out.mp4")), 640, 480)
  expect_error(ffm_compile(p), "copy")
})

test_that("audio filters compile to -af and hit the audio copy guard", {
  # No public verb writes filter_audio yet (audio verbs are a later milestone),
  # so poke the field directly to exercise the -af branch and audio guard.
  f <- make_input()
  p <- ffm_files(f, "out.mp4")
  p$filter_audio <- "volume=2.0"
  expect_match(ffm_compile(p), '-af "volume=2.0"', fixed = TRUE)

  p2 <- ffm_codec(ffm_files(f, "out.mp4"), audio = "copy")
  p2$filter_audio <- "volume=2.0"
  expect_error(ffm_compile(p2), "audio filter")
})

# Validation / print -----------------------------------------------------------

test_that("ffm verbs reject a non-pipeline object", {
  expect_error(ffm_crop(list(), width = 10, height = 10), "ffm pipeline")
  expect_error(ffm_compile("not a pipeline"), "ffm pipeline")
})

test_that("print.tidymedia_ffm() shows the compiled command", {
  f <- make_input()
  p <- ffm_files(f, "out.mp4")
  expect_output(print(p), "tidymedia ffmpeg pipeline", fixed = TRUE)
  expect_output(print(p), ffm_compile(p), fixed = TRUE)
})

# Full-command snapshots (paths scrubbed to <inN> for determinism) -------------

test_that("compiled commands match snapshots", {
  f1 <- make_input()
  f2 <- make_input()
  expect_snapshot({
    writeLines(compile_scrubbed(ffm_files(f1, "out.mp4")))
    writeLines(compile_scrubbed(ffm_files(f1, "out.mp4", overwrite = FALSE)))
    writeLines(compile_scrubbed(ffm_trim(ffm_files(f1, "out.mp4"), start = 1, end = 2)))
    writeLines(compile_scrubbed(ffm_drop(ffm_files(f1, "out.mp4"), c("audio", "subtitles"))))
    writeLines(compile_scrubbed(ffm_pixel_format(ffm_files(f1, "out.mp4"), "yuv420p")))
    writeLines(compile_scrubbed(ffm_codec(ffm_files(f1, "out.mp4"), audio = "aac", video = "libx264")))
    writeLines(compile_scrubbed(ffm_copy(ffm_files(f1, "out.mp4"))))
    writeLines(compile_scrubbed(ffm_crop(ffm_scale(ffm_files(f1, "out.mp4"), 640, 480), width = 100, height = 50)))
    writeLines(compile_scrubbed(ffm_drawbox(ffm_files(f1, "out.mp4"), color = "red")))
    writeLines(compile_scrubbed(ffm_hstack(ffm_files(c(f1, f2), "out.mp4"))))
    writeLines(compile_scrubbed(ffm_crop(ffm_hstack(ffm_files(c(f1, f2), "out.mp4")), width = 100, height = 50)))
    writeLines(compile_scrubbed(ffm_hstack(ffm_files(c(f1, f2), "out.mp4"), resize = TRUE)))
  })
})

# End-to-end execution (binary-gated) ------------------------------------------

test_that("a trim + crop pipeline runs through ffmpeg and writes output", {
  skip_if_no_ffmpeg()
  input <- make_test_video()
  out <- withr::local_tempfile(fileext = ".mp4")
  p <- ffm_crop(
    ffm_trim(ffm_files(input, out), start = 0, end = 1),
    width = 32, height = 32
  )
  ffm_run(p)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

test_that("an hstack pipeline runs through ffmpeg and writes output", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  b <- make_test_video()
  out <- withr::local_tempfile(fileext = ".mp4")
  p <- ffm_hstack(ffm_files(c(a, b), out))
  ffm_run(p)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

test_that("an hstack(resize = TRUE) pipeline runs through ffmpeg", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  b <- make_test_video()
  out <- withr::local_tempfile(fileext = ".mp4")
  p <- ffm_hstack(ffm_files(c(a, b), out), resize = TRUE)
  ffm_run(p)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})
