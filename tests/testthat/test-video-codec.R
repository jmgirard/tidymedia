# video_codec / hardware / fallback on the four codec-less re-encode verbs and
# their _batch siblings (M34, D016).
#
# `video_codec = NULL` is a "leave the codec alone" sentinel: no -codec:v is
# emitted, so the output keeps its container's default video encoder.
#
# M35 later changed the AUDIO default on these same verbs, which splits the pins
# below in two. crop_video's and segment_video's full default commands gained
# -codec:a copy, so their pins here assert the video half and the full literals
# moved to test-audio-codec.R. The composite pins are untouched: those verbs map
# no audio by default, so they emit no -codec:a either way and stay
# byte-identical to pre-M34. nvenc availability is simulated
# with the `tidymedia.nvenc_encoders` option seam that has_nvenc() consults, so
# every compile test here is binary-free (no GPU); the execution tests are
# guarded by skip_if_no_nvenc().

# crop_video() ----------------------------------------------------------------

test_that("crop_video() with the default video_codec emits no -codec:v", {
  f <- make_input()
  cmd <- crop_video(f, "out.mp4", width = 100, height = 50, x = 0, y = 0,
                    run = FALSE)
  # The video half of the command is still exactly what master compiled before
  # M34 existed. The full default literal is no longer pinned here because M35
  # changed it (the default audio_codec adds -codec:a copy); it is pinned
  # byte-for-byte in test-audio-codec.R instead.
  expect_match(as.character(cmd), '-vf "crop=w=100:h=50:x=0:y=0"', fixed = TRUE)
  expect_match(as.character(cmd), '-map 0 "out.mp4"', fixed = TRUE)
  expect_no_match(as.character(cmd), "-codec:v", fixed = TRUE)
})

test_that("crop_video(video_codec = ) sets the codec", {
  f <- make_input()
  cmd <- crop_video(f, "out.mp4", width = 100, height = 50,
                    video_codec = "libx265", run = FALSE)
  expect_match(as.character(cmd), "-codec:v libx265", fixed = TRUE)
})

test_that("crop_video() rejects a non-token video_codec", {
  f <- make_input()
  expect_error(
    crop_video(f, "out.mp4", width = 100, height = 50,
               video_codec = "libx264 -evil", run = FALSE)
  )
  # Same rejection under nvenc, where family inference runs first.
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  expect_error(
    crop_video(f, "out.mp4", width = 100, height = 50,
               video_codec = "libx264 -evil", hardware = "nvenc", run = FALSE)
  )
})

test_that("crop_video(hardware = 'nvenc') resolves the sentinel to h264_nvenc", {
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  f <- make_input()
  cmd <- crop_video(f, "out.mp4", width = 100, height = 50,
                    hardware = "nvenc", run = FALSE)
  expect_match(as.character(cmd), "-codec:v h264_nvenc", fixed = TRUE)
})

test_that("crop_video(hardware = 'nvenc') follows an explicit codec's family", {
  withr::local_options(tidymedia.nvenc_encoders = "hevc_nvenc")
  f <- make_input()
  cmd <- crop_video(f, "out.mp4", width = 100, height = 50,
                    video_codec = "libx265", hardware = "nvenc", run = FALSE)
  expect_match(as.character(cmd), "-codec:v hevc_nvenc", fixed = TRUE)
})

test_that("crop_video(hardware = 'nvenc') aborts when nvenc is unavailable", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  f <- make_input()
  expect_error(
    crop_video(f, "out.mp4", width = 100, height = 50, hardware = "nvenc",
               run = FALSE),
    "not available"
  )
})

test_that("crop_video() fallback from the sentinel emits no codec at all", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  f <- make_input()
  expect_message(
    cmd <- crop_video(f, "out.mp4", width = 100, height = 50,
                      hardware = "nvenc", fallback = TRUE, run = FALSE),
    "container"
  )
  expect_no_match(as.character(cmd), "-codec:v", fixed = TRUE)
})

test_that("crop_video() fallback with an explicit codec keeps that codec", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  f <- make_input()
  expect_message(
    cmd <- crop_video(f, "out.mp4", width = 100, height = 50,
                      video_codec = "libx264", hardware = "nvenc",
                      fallback = TRUE, run = FALSE),
    "falling back"
  )
  expect_match(as.character(cmd), "-codec:v libx264", fixed = TRUE)
})

# compare_videos() ------------------------------------------------------------

test_that("compare_videos() default compiles the pre-M34 command byte-for-byte", {
  f1 <- make_input()
  f2 <- make_input()
  cmd <- compare_videos(c(f1, f2), "out.mp4", run = FALSE)
  # The literal below is the command master compiled before M34 existed.
  expect_equal(
    as.character(cmd),
    paste0(
      '-y -i "', f1, '" -i "', f2, '" -filter_complex ',
      '"[0:v][1:v]scale2ref=\'oh*mdar\':\'if(lt(main_h,ih),ih,main_h)\'[0s][1s];',
      '[1s][0s]scale2ref=\'oh*mdar\':\'if(lt(main_h,ih),ih,main_h)\'[1s][0s];',
      '[0s][1s]hstack,setsar=1[vout]" -map "[vout]" "out.mp4"'
    )
  )
  expect_no_match(as.character(cmd), "-codec:v", fixed = TRUE)
})

test_that("compare_videos(video_codec = ) rides alongside the filtergraph", {
  f1 <- make_input()
  f2 <- make_input()
  cmd <- as.character(
    compare_videos(c(f1, f2), "out.mp4", video_codec = "libx265", run = FALSE)
  )
  expect_match(cmd, "-filter_complex", fixed = TRUE)
  expect_match(cmd, "[vout]", fixed = TRUE)
  expect_match(cmd, '-map "[vout]"', fixed = TRUE)
  expect_match(cmd, "-codec:v libx265", fixed = TRUE)
})

test_that("compare_videos(hardware = 'nvenc') resolves per family", {
  f1 <- make_input()
  f2 <- make_input()
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  expect_match(
    as.character(compare_videos(c(f1, f2), "out.mp4", hardware = "nvenc",
                                run = FALSE)),
    "-codec:v h264_nvenc", fixed = TRUE
  )
  withr::local_options(tidymedia.nvenc_encoders = "hevc_nvenc")
  expect_match(
    as.character(compare_videos(c(f1, f2), "out.mp4", video_codec = "libx265",
                                hardware = "nvenc", run = FALSE)),
    "-codec:v hevc_nvenc", fixed = TRUE
  )
})

test_that("compare_videos() honors the nvenc abort and fallback branches", {
  f1 <- make_input()
  f2 <- make_input()
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  expect_error(
    compare_videos(c(f1, f2), "out.mp4", hardware = "nvenc", run = FALSE),
    "not available"
  )
  expect_message(
    cmd <- compare_videos(c(f1, f2), "out.mp4", hardware = "nvenc",
                          fallback = TRUE, run = FALSE),
    "container"
  )
  expect_no_match(as.character(cmd), "-codec:v", fixed = TRUE)
})

# segment_video() -------------------------------------------------------------

test_that("segment_video() with the default video_codec emits no -codec:v", {
  f <- make_input()
  out <- segment_video(f, 0, 1, "seg.mp4", run = FALSE)
  # The seek half of the command is still exactly what master compiled before
  # M34 existed. The full default literal is no longer pinned here because M35
  # changed it (the default audio_codec adds -codec:a copy); it is pinned
  # byte-for-byte in test-audio-codec.R instead.
  expect_match(as.character(out$command), '-ss 0 -to 1 "seg.mp4"', fixed = TRUE)
  expect_no_match(as.character(out$command), "-codec:v", fixed = TRUE)
})

test_that("segment_video(video_codec = ) sets the codec on every segment", {
  f <- make_input()
  out <- segment_video(f, c(0, 1), c(1, 2), c("a.mp4", "b.mp4"),
                       video_codec = "libx265", run = FALSE)
  expect_true(all(grepl("-codec:v libx265", out$command, fixed = TRUE)))
})

test_that("segment_video(hardware = 'nvenc') resolves per family", {
  f <- make_input()
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  expect_match(
    as.character(segment_video(f, 0, 1, "seg.mp4", hardware = "nvenc",
                               run = FALSE)$command),
    "-codec:v h264_nvenc", fixed = TRUE
  )
  withr::local_options(tidymedia.nvenc_encoders = "hevc_nvenc")
  expect_match(
    as.character(segment_video(f, 0, 1, "seg.mp4", video_codec = "libx265",
                               hardware = "nvenc", run = FALSE)$command),
    "-codec:v hevc_nvenc", fixed = TRUE
  )
})

test_that("segment_video() aborts when a codec meets a stream copy", {
  f <- make_input()
  expect_error(
    segment_video(f, 0, 1, "seg.mp4", reencode = FALSE,
                  video_codec = "libx264", run = FALSE),
    "re-encoding cut"
  )
  expect_error(
    segment_video(f, 0, 1, "seg.mp4", reencode = FALSE, hardware = "nvenc",
                  run = FALSE),
    "re-encoding cut"
  )
})

test_that("segment_video(reencode = FALSE) keeps its pre-M34 stream copy", {
  f <- make_input()
  out <- segment_video(f, 0, 1, "seg.mp4", reencode = FALSE, run = FALSE)
  # The literal below is the command master compiled before M34 existed: the
  # copy path already names `-codec:v copy`, which the guard leaves untouched.
  expect_equal(
    as.character(out$command),
    paste0('-y -ss 0 -to 1 -i "', f, '" -codec:v copy -codec:a copy ',
           '-avoid_negative_ts make_zero -map 0 "seg.mp4"')
  )
})

# picture_in_picture() --------------------------------------------------------

test_that("picture_in_picture() default compiles the pre-M34 command byte-for-byte", {
  f1 <- make_input()
  f2 <- make_input()
  cmd <- picture_in_picture(f1, f2, "pip.mp4", run = FALSE)
  # The literal below is the command master compiled before M34 existed.
  expect_equal(
    as.character(cmd),
    paste0(
      '-y -i "', f1, '" -i "', f2, '" -filter_complex ',
      '"[1:v][0:v]scale2ref=w=\'main_w*0.25\':h=\'main_w*0.25*ih/iw\'[pip][bg];',
      '[bg][pip]overlay=x=main_w-overlay_w-16:y=16:shortest=0[vout]" ',
      '-map "[vout]" "pip.mp4"'
    )
  )
  expect_no_match(as.character(cmd), "-codec:v", fixed = TRUE)
})

test_that("picture_in_picture(video_codec = ) rides alongside the filtergraph", {
  f1 <- make_input()
  f2 <- make_input()
  cmd <- as.character(
    picture_in_picture(f1, f2, "pip.mp4", video_codec = "libx265", run = FALSE)
  )
  expect_match(cmd, "-filter_complex", fixed = TRUE)
  expect_match(cmd, "[vout]", fixed = TRUE)
  expect_match(cmd, '-map "[vout]"', fixed = TRUE)
  expect_match(cmd, "-codec:v libx265", fixed = TRUE)
})

test_that("picture_in_picture(hardware = 'nvenc') resolves per family", {
  f1 <- make_input()
  f2 <- make_input()
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  expect_match(
    as.character(picture_in_picture(f1, f2, "pip.mp4", hardware = "nvenc",
                                    run = FALSE)),
    "-codec:v h264_nvenc", fixed = TRUE
  )
  withr::local_options(tidymedia.nvenc_encoders = "hevc_nvenc")
  expect_match(
    as.character(picture_in_picture(f1, f2, "pip.mp4", video_codec = "libx265",
                                    hardware = "nvenc", run = FALSE)),
    "-codec:v hevc_nvenc", fixed = TRUE
  )
})

test_that("picture_in_picture() honors the nvenc abort and fallback branches", {
  f1 <- make_input()
  f2 <- make_input()
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  expect_error(
    picture_in_picture(f1, f2, "pip.mp4", hardware = "nvenc", run = FALSE),
    "not available"
  )
  expect_message(
    cmd <- picture_in_picture(f1, f2, "pip.mp4", hardware = "nvenc",
                              fallback = TRUE, run = FALSE),
    "container"
  )
  expect_no_match(as.character(cmd), "-codec:v", fixed = TRUE)
})

# _batch siblings: video_codec as a per-row column ----------------------------

test_that("crop_video_batch() takes a per-row video_codec column, NA = unset", {
  f <- make_input()
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"),
    video_codec = c("libx265", NA_character_)
  )
  out <- crop_video_batch(jobs, width = 100, height = 50, run = FALSE)
  expect_match(as.character(out$command[[1]]), "-codec:v libx265", fixed = TRUE)
  expect_no_match(as.character(out$command[[2]]), "-codec:v", fixed = TRUE)
})

test_that("crop_video_batch() accepts an all-NA (logical) video_codec column", {
  f <- make_input()
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"), video_codec = NA
  )
  out <- crop_video_batch(jobs, width = 100, height = 50, run = FALSE)
  expect_false(any(grepl("-codec:v", out$command, fixed = TRUE)))
})

test_that("crop_video_batch() rejects a numeric video_codec column up front", {
  f <- make_input()
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"), video_codec = c(1, 2)
  )
  expect_error(
    crop_video_batch(jobs, width = 100, height = 50, run = FALSE),
    "must be character"
  )
  # An ALL-NA numeric column is still numeric, so it is rejected too: only the
  # all-NA *logical* column is the accepted spelling of "leave every row unset".
  jobs$video_codec <- c(NA_real_, NA_real_)
  expect_error(
    crop_video_batch(jobs, width = 100, height = 50, run = FALSE),
    "must be character"
  )
  # A non-NA logical column is not a codec either.
  jobs$video_codec <- c(TRUE, FALSE)
  expect_error(
    crop_video_batch(jobs, width = 100, height = 50, run = FALSE),
    "must be character"
  )
})

test_that("crop_video_batch() honors hardware only as a formal, not a column", {
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  f <- make_input()
  # A `hardware` column is an ordinary ignored column: it must not touch the
  # compiled commands (hardware is a machine property, D016).
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"),
    hardware = c("nvenc", "nvenc")
  )
  out <- crop_video_batch(jobs, width = 100, height = 50, run = FALSE)
  expect_false(any(grepl("nvenc", out$command, fixed = TRUE)))

  out <- crop_video_batch(jobs, width = 100, height = 50, hardware = "nvenc",
                          run = FALSE)
  expect_true(all(grepl("-codec:v h264_nvenc", out$command, fixed = TRUE)))
})

test_that("segment_video_batch() takes a per-row video_codec column", {
  f <- make_input()
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"),
    start = c(0, 1), end = c(1, 2),
    video_codec = c("libx265", NA_character_)
  )
  out <- segment_video_batch(jobs, run = FALSE)
  expect_match(as.character(out$command[[1]]), "-codec:v libx265", fixed = TRUE)
  expect_no_match(as.character(out$command[[2]]), "-codec:v", fixed = TRUE)
})

test_that("segment_video_batch() aborts on a stream-copy row that names a codec", {
  f <- make_input()
  # Per-row `reencode` column meeting a batch-wide video_codec.
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"),
    start = c(0, 1), end = c(1, 2), reencode = c(TRUE, FALSE)
  )
  expect_error(
    segment_video_batch(jobs, video_codec = "libx264", run = FALSE),
    "re-encoding cut"
  )
  # Per-row `reencode` column meeting a per-row video_codec column.
  jobs$video_codec <- c(NA_character_, "libx264")
  expect_error(segment_video_batch(jobs, run = FALSE), "re-encoding cut")
  # The same table with the codec on the re-encoding row alone is fine.
  jobs$video_codec <- c("libx264", NA_character_)
  expect_no_error(segment_video_batch(jobs, run = FALSE))
})

test_that("compare_videos_batch() takes a per-row video_codec column", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(
    inputs = list(c(f1, f2), c(f1, f2)), output = c("a.mp4", "b.mp4"),
    video_codec = c("libx265", NA_character_)
  )
  out <- compare_videos_batch(jobs, run = FALSE)
  expect_match(as.character(out$command[[1]]), "-codec:v libx265", fixed = TRUE)
  expect_no_match(as.character(out$command[[2]]), "-codec:v", fixed = TRUE)
})

test_that("compare_videos_batch() rejects a numeric video_codec column", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(
    inputs = list(c(f1, f2)), output = "a.mp4", video_codec = 1
  )
  expect_error(compare_videos_batch(jobs, run = FALSE), "must be character")
})

test_that("picture_in_picture_batch() takes a per-row video_codec column", {
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(
    main = c(f1, f1), overlay = c(f2, f2), output = c("a.mp4", "b.mp4"),
    video_codec = c("libx265", NA_character_)
  )
  out <- picture_in_picture_batch(jobs, run = FALSE)
  expect_match(as.character(out$command[[1]]), "-codec:v libx265", fixed = TRUE)
  expect_no_match(as.character(out$command[[2]]), "-codec:v", fixed = TRUE)
})

test_that("picture_in_picture_batch() applies hardware batch-wide", {
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  f1 <- make_input()
  f2 <- make_input()
  jobs <- tibble::tibble(main = f1, overlay = f2, output = "a.mp4")
  out <- picture_in_picture_batch(jobs, hardware = "nvenc", run = FALSE)
  expect_match(as.character(out$command[[1]]), "-codec:v h264_nvenc",
               fixed = TRUE)
})

test_that("the four _batch siblings default to no codec at all", {
  f1 <- make_input()
  f2 <- make_input()
  crop <- crop_video_batch(
    tibble::tibble(input = f1, output = "a.mp4"), width = 100, height = 50,
    run = FALSE
  )
  seg <- segment_video_batch(
    tibble::tibble(input = f1, output = "b.mp4", start = 0, end = 1),
    run = FALSE
  )
  cmp <- compare_videos_batch(
    tibble::tibble(inputs = list(c(f1, f2)), output = "c.mp4"), run = FALSE
  )
  pip <- picture_in_picture_batch(
    tibble::tibble(main = f1, overlay = f2, output = "d.mp4"), run = FALSE
  )
  for (cmd in c(crop$command, seg$command, cmp$command, pip$command)) {
    expect_no_match(as.character(cmd), "-codec:v", fixed = TRUE)
  }
})

# real encodes ----------------------------------------------------------------

test_that("crop_video(video_codec = ) writes a playable file", {
  skip_if_no_ffprobe()
  infile <- make_test_video()
  outfile <- withr::local_tempfile(fileext = ".mp4")
  crop_video(infile, outfile, width = 32, height = 24, video_codec = "libx265")
  expect_true(file.exists(outfile))
  expect_gt(file.size(outfile), 0)
  expect_equal(get_width(outfile), 32)
})

test_that("picture_in_picture(video_codec = ) writes a playable file", {
  skip_if_no_ffprobe()
  infile <- make_test_video()
  outfile <- withr::local_tempfile(fileext = ".mp4")
  picture_in_picture(infile, infile, outfile, video_codec = "libx264")
  expect_true(file.exists(outfile))
  expect_gt(file.size(outfile), 0)
  expect_equal(get_width(outfile), 64)
})

test_that("crop_video(hardware = 'nvenc') writes a non-empty file", {
  skip_if_no_nvenc()
  infile <- make_test_video()
  outfile <- withr::local_tempfile(fileext = ".mp4")
  crop_video(infile, outfile, width = 32, height = 24, hardware = "nvenc")
  expect_true(file.exists(outfile))
  expect_gt(file.size(outfile), 0)
})

test_that("compare_videos(hardware = 'nvenc') writes a non-empty file", {
  skip_if_no_nvenc()
  infile <- make_test_video()
  outfile <- withr::local_tempfile(fileext = ".mp4")
  compare_videos(c(infile, infile), outfile, hardware = "nvenc")
  expect_true(file.exists(outfile))
  expect_gt(file.size(outfile), 0)
})

# scope guards ----------------------------------------------------------------

test_that("M34 adds no pixel_format argument to any of the eight verbs", {
  # AC10: pixel_format is deliberately deferred on these verbs (D016).
  verbs <- c("crop_video", "segment_video", "compare_videos",
             "picture_in_picture", "crop_video_batch", "segment_video_batch",
             "compare_videos_batch", "picture_in_picture_batch")
  for (verb in verbs) {
    expect_false("pixel_format" %in% names(formals(get(verb))), label = verb)
  }
})

test_that("all eight verbs carry the D014 argument spellings", {
  # AC1: exact spellings, no vcodec/codec alias.
  verbs <- c("crop_video", "segment_video", "compare_videos",
             "picture_in_picture", "crop_video_batch", "segment_video_batch",
             "compare_videos_batch", "picture_in_picture_batch")
  for (verb in verbs) {
    fo <- formals(get(verb))
    expect_true("video_codec" %in% names(fo), label = verb)
    expect_null(fo$video_codec)
    expect_equal(eval(fo$hardware), c("none", "nvenc"))
    expect_false(fo$fallback)
    expect_false(any(c("vcodec", "codec") %in% names(fo)), label = verb)
  }
})
