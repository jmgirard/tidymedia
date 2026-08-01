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

# ffm_seek() -------------------------------------------------------------------

test_that("ffm_seek() defaults to accurate: -ss/-to are output options after -i", {
  f <- make_input()
  p <- ffm_seek(ffm_files(f, "out.mp4"), start = 3, end = 7)
  expect_equal(p$seek_start, "3")
  expect_equal(p$seek_end, "7")
  expect_true(p$seek_reencode)
  # Output seeking: -ss/-to appear after -i, no -avoid_negative_ts.
  expect_equal(
    ffm_compile(p),
    sprintf('-y -i "%s" -ss 3 -to 7 "out.mp4"', f)
  )
})

test_that("ffm_seek(reencode = FALSE) input-seeks before -i with avoid_negative_ts", {
  f <- make_input()
  p <- ffm_seek(ffm_files(f, "out.mp4"), start = 3, end = 7, reencode = FALSE)
  expect_false(p$seek_reencode)
  expect_equal(
    ffm_compile(p),
    sprintf(
      '-y -ss 3 -to 7 -i "%s" -avoid_negative_ts make_zero "out.mp4"', f
    )
  )
})

test_that("ffm_seek() accepts an open start or end and tds strings", {
  f <- make_input()
  p1 <- ffm_seek(ffm_files(f, "out.mp4"), end = "00:00:05")
  expect_equal(p1$seek_end, "00:00:05")
  expect_no_match(ffm_compile(p1), "-ss ", fixed = TRUE)
  p2 <- ffm_seek(ffm_files(f, "out.mp4"), start = 2)
  expect_no_match(ffm_compile(p2), "-to ", fixed = TRUE)
})

test_that("ffm_seek() requires at least one bound", {
  f <- make_input()
  expect_error(ffm_seek(ffm_files(f, "out.mp4")), "at least one")
})

test_that("an accurate seek on a copied video stream is refused at compile", {
  f <- make_input()
  p <- ffm_seek(ffm_copy(ffm_files(f, "out.mp4")), start = 1, end = 2)
  expect_error(ffm_compile(p), "frame-accurate")
})

test_that("a fast copy seek coexists with codec copy (the intended fast path)", {
  f <- make_input()
  p <- ffm_copy(ffm_seek(ffm_files(f, "out.mp4"), start = 1, end = 2, reencode = FALSE))
  cmd <- ffm_compile(p)
  expect_match(cmd, "-ss 1 -to 2 -i", fixed = TRUE)
  expect_match(cmd, "-codec:v copy -codec:a copy", fixed = TRUE)
})

# ffm_output_options() ---------------------------------------------------------

test_that("ffm_output_options() appends raw output options in order", {
  f <- make_input()
  p <- ffm_output_options(ffm_files(f, "out.mp4"), "-q:v 1", "-frames:v 1")
  expect_equal(p$output_opts, c("-q:v 1", "-frames:v 1"))
  expect_equal(
    ffm_compile(p),
    sprintf('-y -i "%s" -q:v 1 -frames:v 1 "out.mp4"', f)
  )
})

test_that("ffm_output_options() rejects an empty call", {
  f <- make_input()
  expect_error(ffm_output_options(ffm_files(f, "out.mp4")), "at least one")
})

# ffm_concat() -----------------------------------------------------------------

test_that("ffm_concat() flags concat, copies codecs, and maps all streams", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_concat(ffm_files(c(f1, f2), "out.mp4"))
  expect_true(p$concat)
  expect_true(file.exists(p$concat_list))
  expect_equal(p$codec_video, "copy")
  expect_equal(p$codec_audio, "copy")
  expect_equal(p$map, "0")
})

test_that("ffm_concat() compiles to the concat demuxer input form", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_concat(ffm_files(c(f1, f2), "out.mp4"))
  cmd <- ffm_compile(p)
  expect_match(cmd, "-f concat -safe 0 -i ", fixed = TRUE)
  expect_no_match(cmd, sprintf('-i "%s"', f1), fixed = TRUE)
  expect_match(cmd, "-codec:v copy -codec:a copy -map \"0\"", fixed = TRUE)
})

test_that("ffm_concat() writes a list file naming each input", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_concat(ffm_files(c(f1, f2), "out.mp4"))
  lines <- readLines(p$concat_list)
  expect_equal(lines, c(sprintf("file '%s'", f1), sprintf("file '%s'", f2)))
})

test_that("ffm_concat() requires more than one input", {
  f <- make_input()
  expect_error(ffm_concat(ffm_files(f, "out.mp4")), "more than one")
})

test_that("ffm_concat() refuses to follow a filter", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_scale(ffm_files(c(f1, f2), "out.mp4"), 640, 480)
  expect_error(ffm_concat(p), "before other filters")
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

# ffm_fps() --------------------------------------------------------------------

test_that("ffm_fps() appends an fps filter compiling into -vf", {
  f <- make_input()
  p <- ffm_fps(ffm_files(f, "out.mp4"), 30)
  expect_equal(p$filter_video, "fps=30")
  expect_match(ffm_compile(p), '-vf "fps=30"', fixed = TRUE)
})

test_that("ffm_fps() appends after existing video filters", {
  f <- make_input()
  p <- ffm_fps(ffm_scale(ffm_files(f, "out.mp4"), 640, 480), 24)
  expect_equal(p$filter_video, c("scale=w=640:h=480", "fps=24"))
})

test_that("ffm_fps() accepts an FFmpeg framerate expression as a string", {
  f <- make_input()
  p <- ffm_fps(ffm_files(f, "out.mp4"), "30000/1001")
  expect_equal(p$filter_video, "fps=30000/1001")
})

# ffm_loudnorm() ---------------------------------------------------------------

test_that("ffm_loudnorm() appends a loudnorm filter compiling into -af", {
  f <- make_input()
  p <- ffm_loudnorm(ffm_files(f, "out.mp4"))
  expect_equal(p$filter_audio, "loudnorm=I=-23:TP=-1:LRA=7")
  expect_match(
    ffm_compile(p), '-af "loudnorm=I=-23:TP=-1:LRA=7"', fixed = TRUE
  )
})

test_that("ffm_loudnorm() accepts custom EBU R128 targets", {
  f <- make_input()
  p <- ffm_loudnorm(
    ffm_files(f, "out.mp4"),
    target_loudness = -16, true_peak = -1.5, loudness_range = 11
  )
  expect_equal(p$filter_audio, "loudnorm=I=-16:TP=-1.5:LRA=11")
})

test_that("ffm_loudnorm() coexists with a video filter (both -vf and -af)", {
  f <- make_input()
  p <- ffm_loudnorm(ffm_scale(ffm_files(f, "out.mp4"), 640, 480))
  cmd <- ffm_compile(p)
  expect_match(cmd, '-vf "scale=w=640:h=480"', fixed = TRUE)
  expect_match(cmd, '-af "loudnorm=I=-23:TP=-1:LRA=7"', fixed = TRUE)
})

test_that("ffm_loudnorm() appends after an existing audio filter", {
  f <- make_input()
  p <- ffm_loudnorm(ffm_loudnorm(ffm_files(f, "out.mp4")), target_loudness = -16)
  expect_equal(
    p$filter_audio,
    c("loudnorm=I=-23:TP=-1:LRA=7", "loudnorm=I=-16:TP=-1:LRA=7")
  )
})

test_that("ffm_loudnorm() rejects out-of-range or non-numeric targets", {
  f <- make_input()
  p <- ffm_files(f, "out.mp4")
  expect_error(ffm_loudnorm(p, target_loudness = -3))   # above -5
  expect_error(ffm_loudnorm(p, true_peak = 1))          # above 0
  expect_error(ffm_loudnorm(p, loudness_range = 0))      # below 1
  expect_error(ffm_loudnorm(p, target_loudness = "x"))   # not a number
})

# ffm_loudnorm() two-pass params (M16) ------------------------------------

test_that("ffm_loudnorm() appends print_format for the analysis pass", {
  f <- make_input()
  p <- ffm_loudnorm(ffm_files(f, "out.mp4"), print_format = "json")
  expect_equal(p$filter_audio, "loudnorm=I=-23:TP=-1:LRA=7:print_format=json")
})

test_that("ffm_loudnorm() builds the correction filter from measured values", {
  f <- make_input()
  p <- ffm_loudnorm(
    ffm_files(f, "out.mp4"),
    measured_i = -27.61, measured_tp = -9.32, measured_lra = 5.90,
    measured_thresh = -38.06, offset = 0.30, linear = TRUE
  )
  expect_equal(
    p$filter_audio,
    paste0("loudnorm=I=-23:TP=-1:LRA=7:measured_I=-27.61:measured_TP=-9.32:",
           "measured_LRA=5.9:measured_thresh=-38.06:offset=0.3:linear=true")
  )
})

test_that("ffm_loudnorm() omits linear=false and never emits it by default", {
  f <- make_input()
  # Default single-pass call must stay byte-for-byte (no linear/measured keys).
  p <- ffm_loudnorm(ffm_files(f, "out.mp4"))
  expect_no_match(p$filter_audio, "linear", fixed = TRUE)
  expect_no_match(p$filter_audio, "measured", fixed = TRUE)
})

test_that("ffm_loudnorm() requires the measured set to be supplied together", {
  f <- make_input()
  p <- ffm_files(f, "out.mp4")
  # Partial measured set is a usage error (all five or none).
  expect_error(ffm_loudnorm(p, measured_i = -27.61), "together")
  expect_error(
    ffm_loudnorm(p, measured_i = -27.61, measured_tp = -9.32,
                 measured_lra = 5.9, measured_thresh = -38.06),  # offset missing
    "together"
  )
})

test_that("ffm_loudnorm() validates the new params", {
  f <- make_input()
  p <- ffm_files(f, "out.mp4")
  expect_error(ffm_loudnorm(p, print_format = "xml"))       # not a known format
  expect_error(ffm_loudnorm(p, linear = "yes"))             # not a bool
  expect_error(                                             # non-finite measured
    ffm_loudnorm(p, measured_i = Inf, measured_tp = -9.32, measured_lra = 5.9,
                 measured_thresh = -38.06, offset = 0.3)
  )
})

test_that("ffm_fps() rejects a non-positive or non-numeric/non-string fps", {
  f <- make_input()
  p <- ffm_files(f, "out.mp4")
  expect_error(ffm_fps(p, 0))
  expect_error(ffm_fps(p, -5))
  expect_error(ffm_fps(p, c(24, 30)))
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
    sprintf('-y -i "%s" -codec:v copy -codec:a copy -map "0" "out.mp4"', f)
  )
})

# ffm_map() --------------------------------------------------------------------

test_that("ffm_map() sets the stream mapping", {
  f <- make_input()
  p <- ffm_map(ffm_files(f, "out.mp4"), "0:v")
  expect_equal(p$map, "0:v")
  expect_match(ffm_compile(p), "-map \"0:v\" ", fixed = TRUE)
})

test_that("ffm_map() accepts a character vector and emits one -map per element", {
  f <- make_input()
  p <- ffm_map(ffm_files(f, "out.mkv"), c("0:v", "0:a:1", "0:a:2"))
  expect_equal(p$map, c("0:v", "0:a:1", "0:a:2"))
  args <- tidymedia:::ffm_args(p)
  expect_identical(sum(args == "-map"), 3L)
  expect_identical(args[which(args == "-map") + 1L], c("0:v", "0:a:1", "0:a:2"))
})

test_that("ffm_map() appends on chaining rather than overwriting", {
  # M43 changed this: the builder used to overwrite `object$map`, so a second
  # call silently discarded the first. Appending is what lets a caller keep the
  # video and then name one audio track.
  f <- make_input()
  p <- ffm_map(ffm_map(ffm_files(f, "out.mkv"), "0:v"), "0:a:1")
  expect_equal(p$map, c("0:v", "0:a:1"))
  expect_match(ffm_compile(p), "-map \"0:v\" -map \"0:a:1\"", fixed = TRUE)
})

test_that("ffm_map(replace = TRUE) narrows an existing map instead of adding to it", {
  # The escape hatch appending needs: ffm_copy() already mapped "0" (every
  # stream), and appending "0:a:1" to that would duplicate the output stream
  # rather than narrow it -- the exact failure the convert_audio map fix removed.
  f <- make_input()
  p <- ffm_map(ffm_copy(ffm_files(f, "out.mkv")), "0:a:1", replace = TRUE)
  expect_equal(p$map, "0:a:1")
  args <- tidymedia:::ffm_args(p)
  expect_identical(sum(args == "-map"), 1L)
  expect_identical(args[which(args == "-map") + 1L], "0:a:1")
})

test_that("ffm_map() rejects a non-character, empty, or NA mapping", {
  f <- make_input()
  p <- ffm_files(f, "out.mp4")
  expect_error(ffm_map(p, 0), "character vector")
  expect_error(ffm_map(p, character(0)), "character vector")
  expect_error(ffm_map(p, c("0:v", NA)), "character vector")
  expect_error(ffm_map(p, "0:v", replace = NA), "`replace`")
})

# M48/D027: ffm_copy() assigns its map ----------------------------------------
#
# These sit BELOW the two ffm_map() contract tests above on purpose. Those two
# are the evidence that this fix landed in ffm_copy() and not in ffm_map(), so
# they must pass unmodified -- and RR03's binding criterion identifies them by
# line number, which inserting above them would falsify.

test_that("ffm_copy() applied twice compiles one -map \"0\", not two", {
  # Before M48 this compiled `-map 0 -map 0` and duplicated every output stream
  # (a 1v/1a input gave a 4-stream output), because ffm_copy() set its map
  # through ffm_map(), which appends since M43.
  f <- make_input()
  p <- ffm_copy(ffm_copy(ffm_files(f, "out.mkv")))
  expect_equal(p$map, "0")
  args <- tidymedia:::ffm_args(p)
  expect_identical(sum(args == "-map"), 1L)
  expect_identical(args[which(args == "-map") + 1L], "0")
})

test_that("ffm_concat() |> ffm_copy() compiles one -map \"0\", not two", {
  # ffm_concat() calls ffm_copy() internally, so this composition doubled too.
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_copy(ffm_concat(ffm_files(c(f1, f2), "out.mp4")))
  expect_equal(p$map, "0")
  args <- tidymedia:::ffm_args(p)
  expect_identical(sum(args == "-map"), 1L)
  expect_identical(args[which(args == "-map") + 1L], "0")
})

test_that("ffm_copy() refuses to discard a different stated mapping", {
  # Assignment without this guard would silently drop the caller's selection --
  # the flaw D023 removed from ffm_map(), reintroduced for one verb.
  f <- make_input()
  p <- ffm_map(ffm_files(f, "out.mkv"), "0:v")
  expect_error(ffm_copy(p), class = "tidymedia_copy_map_conflict")
  # The message has to be actionable on both escape routes, and must describe
  # the PIPELINE rather than presume the caller typed ffm_copy(): ffm_concat()
  # reaches this from a frame the user never called, where `streams = FALSE` is
  # not an argument they can pass.
  expect_error(ffm_copy(p), "streams = FALSE", fixed = TRUE)
  expect_error(ffm_copy(p), "replace = TRUE", fixed = TRUE)
  expect_error(ffm_copy(p), "already sets a stream mapping", fixed = TRUE)
})

test_that("the conflict guard reaches the pipeline through ffm_concat() too", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_map(ffm_files(c(f1, f2), "out.mp4"), "0:v")
  expect_error(ffm_concat(p), class = "tidymedia_copy_map_conflict")
})

test_that("the identical-to-\"0\" carve-out is literal", {
  # It is what keeps the doubled ffm_copy() a silent no-op; a map already
  # doubled by hand is a different vector and is a conflict.
  f <- make_input()
  expect_error(
    ffm_copy(ffm_map(ffm_files(f, "out.mkv"), c("0", "0"))),
    class = "tidymedia_copy_map_conflict"
  )
})

test_that("streams = FALSE keeps an existing mapping and still copies codecs", {
  f <- make_input()
  p <- ffm_copy(ffm_map(ffm_files(f, "out.mkv"), "0:v"), streams = FALSE)
  expect_equal(p$map, "0:v")
  args <- tidymedia:::ffm_args(p)
  expect_identical(sum(args == "-map"), 1L)
  expect_identical(args[which(args == "-map") + 1L], "0:v")
  expect_match(ffm_compile(p), "-codec:v copy -codec:a copy", fixed = TRUE)
})

test_that("strip_metadata() compiles its pre-M48 command unchanged", {
  # The untouched in-package ffm_copy() caller: it calls ffm_copy() on an empty
  # map, so assignment and appending agree and its command must not move. A
  # sprintf() template rather than a fixed literal because the input path is a
  # per-test tempfile (the deviation recorded against RR03's BC6).
  f <- make_input()
  expect_identical(
    strip_metadata(f, "out.mp4", run = FALSE),
    sprintf(
      paste0('-y -i "%s" -codec:v copy -codec:a copy -map_metadata -1 ',
             '-map_chapters -1 -fflags +bitexact -map "0" "%s"'),
      f, "out.mp4"
    )
  )
})

test_that("a doubled ffm_copy() remux writes the input's stream count", {
  # The execution half: the compile-level fix is only worth having because the
  # duplicated maps duplicated real output streams. Five in, five out.
  skip_if_no_ffmpeg()
  infile <- make_multitrack_subtitle_video()
  out <- withr::local_tempfile(fileext = ".mkv")
  ffm_run(ffm_copy(ffm_copy(ffm_files(infile, out))))
  expect_identical(length(stream_types(out)), length(stream_types(infile)))
  expect_identical(length(stream_types(out)), 5L)
})

test_that("every in-package pipeline emits the maps its verb's contract says", {
  # THE RULE, as of M49. A COMPILED COMMAND's -map count is exactly one of two
  # numbers, and which one is a property of what that command's output is:
  #
  #   2 -- a command whose output carries both video and audio, and which
  #        states both halves of its selection: `-map 0:v?` plus either
  #        `-map 0:a?` or `-map 0:a:<n>` (D026). No in-package command compiles
  #        `0:a:0?`; D030 deleted the one that did.
  #   1 -- a command whose output is ONE stream (the extraction verbs, each
  #        side of the separation fan-out, the two-pass analysis pass, which
  #        writes to `-f null` and maps audio alone), or one that copies every
  #        stream with the all-streams specifier `0` (strip_metadata,
  #        concatenate_videos).
  #
  # The rows are one per COMPILED COMMAND, not one per verb (M49): a verb whose
  # branches compile different commands gets a row each, named verb-plus-branch,
  # so normalize_audio()'s analysis and correction commands are separate rows.
  # Keying on the verb hid the analysis pass entirely -- the one command in the
  # package whose map has to AGREE with another command's rather than merely be
  # well-formed.
  #
  # No row is 0 any more, but that is a fact about the commands LISTED here, not
  # about the package: extract_frame() and sample_frames() still compile no map
  # at all, and are absent from this table rather than pinned at zero, because
  # their output is an image and D026's stream-selection question does not
  # arise for them. The table is a hardcoded list, so it cannot fail on a verb
  # nobody added to it -- adding the row is what catches a new verb, and this
  # comment is the standing reminder to do it.
  #
  # The count is the assertion because ffm_map() appends (D023): a pipeline
  # that gains a map without meaning to duplicates the output stream rather
  # than overwriting it, and no containment assertion can see that. The
  # invariant was a "<= 1" BOUND until M47; a bound stopped saying anything
  # once verbs began emitting two, so it is an exact count per command, and a
  # wrong count fails in either direction.
  f <- make_input()
  f2 <- make_input()
  maps <- function(cmd) {
    vapply(cmd, function(x) {
      sum(gregexpr("-map ", x, fixed = TRUE)[[1]] > 0)
    }, integer(1), USE.NAMES = FALSE)
  }
  regions <- data.frame(x = 0, y = 0, width = 10, height = 10)
  expected <- c(
    "extract_audio" = 1L,
    "convert_audio" = 1L,
    "strip_metadata" = 1L,
    "concatenate_videos" = 1L,
    # The fan-out: one map on each of the two commands.
    "separate_audio_video(audio)" = 1L,
    "separate_audio_video(video)" = 1L,
    # The pass-through family. Both segment_video() branches are here: the
    # re-encode path, which emitted NO map before M48, and the copy path, where
    # the selection replaces ffm_copy()'s `-map 0` rather than appending to it.
    "crop_video" = 2L,
    "segment_video(reencode = TRUE)" = 2L,
    "segment_video(reencode = FALSE)" = 2L,
    "standardize_video" = 2L,
    "anonymize_video" = 2L,
    "format_for_web" = 2L,
    # normalize_audio()'s two commands. Both map one audio stream and no video
    # (D030): this verb's output IS an audio stream, so it sits with the
    # extraction verbs rather than the pass-through family, and its analysis
    # command writes to `-f null`.
    "normalize_audio(correction)" = 1L,
    "normalize_audio(analysis)" = 1L
  )
  cmds <- c(
    extract_audio(f, "out.aac", run = FALSE),
    convert_audio(f, "out.mp3", run = FALSE),
    strip_metadata(f, "out.mp4", run = FALSE),
    concatenate_videos(c(f, f2), "out.mp4", run = FALSE),
    unlist(separate_audio_video(f, "out.m4a", "out.mp4", run = FALSE)),
    crop_video(f, "out.mp4", 32, 32, run = FALSE),
    segment_video(f, 0, 1, outfiles = "seg.mp4", run = FALSE)$command,
    segment_video(f, 0, 1, outfiles = "seg.mp4", reencode = FALSE,
                  run = FALSE)$command,
    standardize_video(f, "out.mp4", run = FALSE),
    anonymize_video(f, "out.mp4", regions, run = FALSE),
    format_for_web(f, "out.mp4", run = FALSE),
    normalize_audio(f, "out.mp4", run = FALSE),
    # The analysis pipeline is compiled directly rather than reached through
    # normalize_audio(two_pass = TRUE): D013 makes that pass RUN before `run` is
    # consulted, so no verb call yields this command without executing FFmpeg
    # (D024), and this test must stay binary-free.
    ffm_compile(loudnorm_analysis_pipeline(f))
  )
  expect_identical(setNames(maps(cmds), names(expected)), expected)
})

# M50: the executed argument vector, pinned ------------------------------------

# The same commands the map-count table above enumerates, rebuilt as PIPELINE
# OBJECTS rather than compiled strings. The verbs return only the compiled
# string under `run = FALSE`, and `ffm_args()` -- the vector that actually
# reaches FFmpeg -- needs the object, so each row calls the internal builder its
# verb calls, with that verb's documented defaults. The parity test below is
# what stops this rebuild drifting from the verbs it stands in for.
enumerated_pipelines <- function(f, f2) {
  regions <- data.frame(x = 0, y = 0, width = 10, height = 10)
  list(
    "extract_audio" = extract_audio_pipeline(f, "out.aac", "copy"),
    "convert_audio" = convert_audio_pipeline(f, "out.mp3", NULL),
    "strip_metadata" = strip_metadata_pipeline(f, "out.mp4"),
    "concatenate_videos" = concatenate_pipeline(c(f, f2), "out.mp4"),
    "separate_audio_video(audio)" = separate_stream_pipeline(
      f, "out.m4a", "audio", "copy", "none", FALSE, NULL
    ),
    "separate_audio_video(video)" = separate_stream_pipeline(
      f, "out.mp4", "video", "copy", "none", FALSE
    ),
    "crop_video" = crop_video_pipeline(f, "out.mp4", 32, 32),
    "segment_video(reencode = TRUE)" = segment_pipeline(
      f, "seg.mp4", 0, 1, TRUE, NULL, "copy", "none", FALSE, NULL
    ),
    "segment_video(reencode = FALSE)" = segment_pipeline(
      f, "seg.mp4", 0, 1, FALSE, NULL, "copy", "none", FALSE, NULL
    ),
    "standardize_video" = standardize_pipeline(
      f, "out.mp4", NULL, NULL, NULL, "libx264", "copy", "yuv420p"
    ),
    "anonymize_video" = anonymize_pipeline(
      f, "out.mp4", regions, "black", "libx264", "copy", "yuv420p"
    ),
    "format_for_web" = format_for_web_pipeline(f, "out.mp4"),
    "normalize_audio(correction)" = normalize_audio_pipeline(f, "out.mp4"),
    "normalize_audio(analysis)" = loudnorm_analysis_pipeline(f)
  )
}

# ffm_concat() writes a fresh demuxer list file per call, under a name carrying
# a random suffix, so two compilations of the same concat pipeline differ in
# that one token and in nothing else. Both the parity test and the snapshot
# below compare across compilations, so both blank it out.
stable_paths <- function(x, f = NULL, f2 = NULL) {
  # Bounded by the enclosing quote rather than by whitespace: a temp path may
  # contain a space (routinely so under a Windows profile directory), and a
  # whitespace-bounded pattern would fail to scrub it on that platform alone.
  x <- gsub("[^\"|]*ffm-concat[^\"|]*\\.txt", "<concat-list>", x)
  if (!is.null(f)) x <- sub(f, "<input>", x, fixed = TRUE)
  if (!is.null(f2)) x <- sub(f2, "<input2>", x, fixed = TRUE)
  x
}

test_that("the rebuilt pipelines compile the commands their verbs compile", {
  # Guards the rebuild above. Both sides read the live code, so a future change
  # to a verb's map moves them together; what this catches is the rebuild
  # standing for a command the verb no longer compiles.
  f <- make_input()
  f2 <- make_input()
  regions <- data.frame(x = 0, y = 0, width = 10, height = 10)
  rebuilt <- vapply(enumerated_pipelines(f, f2), ffm_compile, character(1))
  from_verbs <- c(
    extract_audio(f, "out.aac", run = FALSE),
    convert_audio(f, "out.mp3", run = FALSE),
    strip_metadata(f, "out.mp4", run = FALSE),
    concatenate_videos(c(f, f2), "out.mp4", run = FALSE),
    unlist(separate_audio_video(f, "out.m4a", "out.mp4", run = FALSE)),
    crop_video(f, "out.mp4", 32, 32, run = FALSE),
    segment_video(f, 0, 1, outfiles = "seg.mp4", run = FALSE)$command,
    segment_video(f, 0, 1, outfiles = "seg.mp4", reencode = FALSE,
                  run = FALSE)$command,
    standardize_video(f, "out.mp4", run = FALSE),
    anonymize_video(f, "out.mp4", regions, run = FALSE),
    format_for_web(f, "out.mp4", run = FALSE),
    normalize_audio(f, "out.mp4", run = FALSE),
    # Compiled directly for the reason the map-count table gives: no verb call
    # yields the analysis command without running FFmpeg (D013/D024).
    ffm_compile(loudnorm_analysis_pipeline(f))
  )
  expect_identical(stable_paths(unname(rebuilt)), stable_paths(unname(from_verbs)))
})

test_that("the executed argument vector is pinned for every enumerated pipeline", {
  # AC3/T1, recorded BEFORE the map-quoting change and not touched by it: the
  # quoting M50 adds lives in each group's `display` only, so this snapshot is
  # the evidence that nothing reached the vector `ffm_run()` executes. A diff
  # here is the change escaping to FFmpeg. Committed before the source change so
  # the reference is fixed rather than co-authored with what it checks (M44).
  f <- make_input()
  f2 <- make_input()
  rendered <- vapply(enumerated_pipelines(f, f2), function(p) {
    args <- stable_paths(tidymedia:::ffm_args(p), f, f2)
    # " | " between elements: the point of the vector is where one argument
    # ends and the next begins, which a space-joined string would hide.
    paste(args, collapse = " | ")
  }, character(1))
  expect_snapshot(cat(paste0(names(rendered), ": ", rendered), sep = "\n"))
})

# M50: every map specifier is quoted in the DISPLAY string ---------------------

# The specifier token following each `-map` in a compiled command. The trailing
# space in the pattern is load-bearing: `-map_metadata` and `-map_chapters` are
# different options and carry no specifier to quote.
map_specifiers <- function(cmd) {
  hits <- regmatches(cmd, gregexpr('-map ("[^"]*"|[^ ]+)', cmd))
  sub("^-map ", "", unlist(hits, use.names = FALSE))
}

test_that("a single-input pipeline quotes its map specifier", {
  # The simple branch (R/ffm.R). M47's `?` is what forced this: the compiled
  # string is the artifact the vignette tells a reader to inspect, log, and
  # paste, and an unquoted `-map 0:v?` pasted into zsh fails with
  # `no matches found`.
  f <- make_input()
  p <- ffm_map(ffm_files(f, "out.mkv"), c("0:v?", "0:a?"))
  expect_equal(
    ffm_compile(p),
    sprintf('-y -i "%s" -map "0:v?" -map "0:a?" "out.mkv"', f)
  )
  expect_identical(map_specifiers(ffm_compile(p)), c('"0:v?"', '"0:a?"'))
})

test_that("the complex branch quotes a user map beside the automatic one", {
  # D-M06-1 puts an explicit ffm_map() alongside the auto `[vout]` map. The
  # auto one has been quoted since M06; the user's rode bare, so a fix touching
  # only the simple branch would have left this branch inconsistent with it.
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_map(ffm_hstack(ffm_files(c(f1, f2), "out.mp4")), "0:a?")
  expect_equal(
    ffm_compile(p),
    sprintf(
      paste0(
        '-y -i "%s" -i "%s" ',
        '-filter_complex "[0:v][1:v]hstack=inputs=2:shortest=0[vout]" ',
        '-map "[vout]" -map "0:a?" "out.mp4"'
      ),
      f1, f2
    )
  )
  expect_identical(map_specifiers(ffm_compile(p)), c('"[vout]"', '"0:a?"'))
})

test_that("no in-package pipeline compiles a bare map specifier", {
  # AC2, over the same fourteen commands the map-count table enumerates. That
  # table says how many maps each command emits; this says every one of them
  # survives a paste into a shell.
  f <- make_input()
  f2 <- make_input()
  specifiers <- lapply(enumerated_pipelines(f, f2), function(p) {
    map_specifiers(ffm_compile(p))
  })
  # Two separate claims, so each names its own offender. Folding "emits no map"
  # into the quoting predicate would report a map-less pipeline as compiling a
  # BARE specifier, which is the wrong diagnosis: every row here emits at least
  # one map today, but `extract_frame()` and `sample_frames()` emit none and are
  # legitimate future rows.
  expect_identical(names(specifiers)[lengths(specifiers) == 0L], character(0))
  bare <- specifiers[vapply(specifiers, function(s) {
    !all(startsWith(s, '"') & endsWith(s, '"'))
  }, logical(1))]
  expect_identical(names(bare), character(0))
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

# ffm_vstack() (multi-input, filter_complex path) ------------------------------

test_that("ffm_vstack() flags the pipeline complex and stores a label-free token", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_vstack(ffm_files(c(f1, f2), "out.mp4"))
  expect_true(p$complex)
  expect_equal(p$filter_video, "vstack=inputs=2:shortest=0")
})

test_that("ffm_vstack() compiles to -filter_complex with labels and auto -map", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_vstack(ffm_files(c(f1, f2), "out.mp4"))
  expect_equal(
    ffm_compile(p),
    sprintf(
      paste0(
        '-y -i "%s" -i "%s" ',
        '-filter_complex "[0:v][1:v]vstack=inputs=2:shortest=0[vout]" ',
        '-map "[vout]" "out.mp4"'
      ),
      f1, f2
    )
  )
})

test_that("ffm_vstack() carries the input count into the token", {
  f1 <- make_input()
  f2 <- make_input()
  f3 <- make_input()
  p <- ffm_vstack(ffm_files(c(f1, f2, f3), "out.mp4"))
  expect_equal(p$filter_video, "vstack=inputs=3:shortest=0")
})

test_that("ffm_vstack() requires more than one input", {
  f <- make_input()
  expect_error(ffm_vstack(ffm_files(f, "out.mp4")), "more than one")
})

test_that("ffm_vstack() refuses to follow a single-input video filter", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_scale(ffm_files(c(f1, f2), "out.mp4"), 640, 480)
  expect_error(ffm_vstack(p), "before other video filters")
})

test_that("ffm_vstack(resize = TRUE) emits a single-line self-labelled graph", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_vstack(ffm_files(c(f1, f2), "out.mp4"), resize = TRUE)
  expect_true(p$complex)
  expect_no_match(p$filter_video, "\n", fixed = TRUE)
  cmd <- ffm_compile(p)
  expect_match(cmd, "scale2ref", fixed = TRUE)
  expect_match(cmd, 'vstack,setsar=1[vout]" -map "[vout]"', fixed = TRUE)
})

# ffm_overlay() (multi-input, filter_complex path) -----------------------------

test_that("ffm_overlay() flags complex and stores a label-free token", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_overlay(ffm_files(c(f1, f2), "out.mp4"))
  expect_true(p$complex)
  expect_equal(p$filter_video, "overlay=x=0:y=0:shortest=0")
})

test_that("ffm_overlay() compiles to -filter_complex with labels and auto -map", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_overlay(ffm_files(c(f1, f2), "out.mp4"),
                   x = "main_w-overlay_w-16", y = 16)
  expect_equal(
    ffm_compile(p),
    sprintf(
      paste0(
        '-y -i "%s" -i "%s" ',
        '-filter_complex "[0:v][1:v]overlay=x=main_w-overlay_w-16:y=16:shortest=0[vout]" ',
        '-map "[vout]" "out.mp4"'
      ),
      f1, f2
    )
  )
})

test_that("ffm_overlay() requires exactly two inputs", {
  f <- make_input()
  expect_error(ffm_overlay(ffm_files(f, "out.mp4")), "exactly two")
  expect_error(
    ffm_overlay(ffm_files(c(f, f, f), "out.mp4")),
    "exactly two"
  )
})

test_that("ffm_overlay() refuses to follow a single-input video filter", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_scale(ffm_files(c(f1, f2), "out.mp4"), 640, 480)
  expect_error(ffm_overlay(p), "before other video filters")
})

test_that("ffm_overlay(shortest = TRUE) sets the shortest flag in the token", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_overlay(ffm_files(c(f1, f2), "out.mp4"), shortest = TRUE)
  expect_equal(p$filter_video, "overlay=x=0:y=0:shortest=1")
})

test_that("ffm_overlay(scale = ) emits a self-labelled scale2ref graph", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_overlay(ffm_files(c(f1, f2), "out.mp4"),
                   x = "main_w-overlay_w-16", y = 16, scale = 0.25)
  expect_true(p$complex)
  expect_no_match(p$filter_video, "\n", fixed = TRUE)
  cmd <- ffm_compile(p)
  expect_match(cmd, "[1:v][0:v]scale2ref=w='main_w*0.25'", fixed = TRUE)
  expect_match(
    cmd,
    'overlay=x=main_w-overlay_w-16:y=16:shortest=0[vout]" -map "[vout]"',
    fixed = TRUE
  )
})

test_that("ffm_overlay() rejects an out-of-range scale", {
  f1 <- make_input()
  f2 <- make_input()
  expect_error(
    ffm_overlay(ffm_files(c(f1, f2), "out.mp4"), scale = 0), "at most 1"
  )
  expect_error(
    ffm_overlay(ffm_files(c(f1, f2), "out.mp4"), scale = 2), "at most 1"
  )
})

test_that("ffm_overlay() args parity with ffm_compile()", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_overlay(ffm_files(c(f1, f2), "out.mp4"))
  expect_identical(
    tidymedia:::ffm_args(p),
    c("-y", "-i", f1, "-i", f2,
      "-filter_complex", "[0:v][1:v]overlay=x=0:y=0:shortest=0[vout]",
      "-map", "[vout]", "out.mp4")
  )
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
    writeLines(compile_scrubbed(ffm_vstack(ffm_files(c(f1, f2), "out.mp4"))))
    writeLines(compile_scrubbed(ffm_vstack(ffm_files(c(f1, f2), "out.mp4"), resize = TRUE)))
    writeLines(compile_scrubbed(ffm_overlay(ffm_files(c(f1, f2), "out.mp4"), x = "main_w-overlay_w-16", y = 16)))
    writeLines(compile_scrubbed(ffm_overlay(ffm_files(c(f1, f2), "out.mp4"), x = "main_w-overlay_w-16", y = 16, scale = 0.25)))
    writeLines(compile_scrubbed(ffm_seek(ffm_files(f1, "out.mp4"), start = 3, end = 7)))
    writeLines(compile_scrubbed(ffm_copy(ffm_seek(ffm_files(f1, "out.mp4"), start = 3, end = 7, reencode = FALSE))))
    writeLines(compile_scrubbed(ffm_output_options(ffm_files(f1, "out.mp4"), "-q:v 1", "-frames:v 1")))
    writeLines(compile_scrubbed(ffm_concat(ffm_files(c(f1, f2), "out.mp4"))))
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

test_that("a vstack pipeline runs through ffmpeg and writes output", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  b <- make_test_video()
  out <- withr::local_tempfile(fileext = ".mp4")
  p <- ffm_vstack(ffm_files(c(a, b), out))
  ffm_run(p)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

test_that("a vstack(resize = TRUE) pipeline runs through ffmpeg", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  b <- make_test_video()
  out <- withr::local_tempfile(fileext = ".mp4")
  p <- ffm_vstack(ffm_files(c(a, b), out), resize = TRUE)
  ffm_run(p)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

test_that("an overlay pipeline runs through ffmpeg and writes output", {
  skip_if_no_ffmpeg()
  a <- make_test_video()
  b <- make_test_video()
  out <- withr::local_tempfile(fileext = ".mp4")
  p <- ffm_overlay(ffm_files(c(a, b), out), x = 8, y = 8)
  ffm_run(p)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

test_that("ffm_seek() default (accurate) produces a frame-exact duration", {
  skip_if_no_ffmpeg()
  skip_if_no_ffprobe()
  # Keyframes every 2s; request 3.0->7.0 (both non-keyframe). Accurate mode must
  # land on 4.0s, unlike a keyframe-snapped copy (M03 D-M03-5 evidence).
  input <- make_keyframed_video()
  out <- withr::local_tempfile(fileext = ".mp4")
  ffm_run(ffm_seek(ffm_files(input, out), start = 3, end = 7))
  expect_true(file.exists(out))
  expect_equal(probe_duration(out), 4.0, tolerance = 0.15)
})

test_that("ffm_seek(reencode = FALSE) copy cut is clean and starts at zero", {
  skip_if_no_ffmpeg()
  skip_if_no_ffprobe()
  input <- make_keyframed_video()
  out <- withr::local_tempfile(fileext = ".mp4")
  p <- ffm_copy(ffm_seek(ffm_files(input, out), start = 3, end = 7, reencode = FALSE))
  ffm_run(p)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
  # The old broken output-seek-copy path shifted the start pts; the input-seek +
  # avoid_negative_ts path must start the segment at (or very near) zero.
  start_pts <- as.numeric(ffprobe(sprintf(
    paste0('-v error -select_streams v -show_entries frame=pts_time',
           ' -of csv=p=0 -read_intervals "%%+#1" "%s"'), out
  ))[[1]])
  expect_lt(start_pts, 0.1)
  # Copy cuts snap to keyframes (every 2s here), so the duration is approximate
  # rather than exactly 4.0s -- but it must stay near the request, not run away
  # (e.g. a version that mis-read input -to could copy to the end at ~10s).
  dur <- probe_duration(out)
  expect_gt(dur, 3.5)
  expect_lt(dur, 6.5)
})

test_that("ffm_concat() joins inputs end to end through ffmpeg", {
  skip_if_no_ffmpeg()
  skip_if_no_ffprobe()
  a <- make_test_video()
  b <- make_test_video()
  out <- withr::local_tempfile(fileext = ".mp4")
  ffm_run(ffm_concat(ffm_files(c(a, b), out)))
  expect_true(file.exists(out))
  # Roughly the sum of the two ~2s inputs.
  expect_gt(probe_duration(out), 3.5)
})

# M06: ffm_args() shared assembly (pure) ----------------------------------------

test_that("ffm_args() yields one element per CLI argument, unquoted", {
  f <- make_input()
  p <- ffm_files(f, "out file.mp4") |>
    ffm_trim(start = 1, end = 2) |>
    ffm_codec(video = "libx264")
  expect_identical(
    tidymedia:::ffm_args(p),
    c("-y", "-i", f, "-vf", "trim=start=1:end=2,setpts=PTS-STARTPTS",
      "-codec:v", "libx264", "out file.mp4")
  )
})

test_that("ffm_args() matches ffm_compile() ordering for a complex pipeline", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_hstack(ffm_files(c(f1, f2), "out.mp4"))
  expect_identical(
    tidymedia:::ffm_args(p),
    c("-y", "-i", f1, "-i", f2,
      "-filter_complex", "[0:v][1:v]hstack=inputs=2:shortest=0[vout]",
      "-map", "[vout]", "out.mp4")
  )
})

test_that("ffm_args() renders both seek modes", {
  f <- make_input()
  fast <- ffm_seek(ffm_files(f, "out.mp4"), start = 3, end = 7,
                   reencode = FALSE)
  expect_identical(
    tidymedia:::ffm_args(fast),
    c("-y", "-ss", "3", "-to", "7", "-i", f,
      "-avoid_negative_ts", "make_zero", "out.mp4")
  )
  slow <- ffm_seek(ffm_files(f, "out.mp4"), start = 3, end = 7)
  expect_identical(
    tidymedia:::ffm_args(slow),
    c("-y", "-i", f, "-ss", "3", "-to", "7", "out.mp4")
  )
})

test_that("ffm_args() splits raw output option groups into tokens", {
  f <- make_input()
  p <- ffm_output_options(ffm_files(f, "out.png"), "-frames:v 1", "-q:v 2")
  expect_identical(
    tidymedia:::ffm_args(p),
    c("-y", "-i", f, "-frames:v", "1", "-q:v", "2", "out.png")
  )
  # ...while the display string keeps each group verbatim
  expect_match(ffm_compile(p), "-frames:v 1 -q:v 2", fixed = TRUE)
})

test_that("ffm_args() elements carry no shell quoting", {
  f <- make_input()
  p <- ffm_scale(ffm_files(f, "out dir/out.mp4"), 640, 480)
  expect_false(any(grepl('"', tidymedia:::ffm_args(p), fixed = TRUE)))
})

test_that("ffm_args() and ffm_compile() agree on the concat demuxer", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_copy(ffm_concat(ffm_files(c(f1, f2), "out.mp4")))
  args <- tidymedia:::ffm_args(p)
  expect_identical(
    args[1:5],
    c("-y", "-f", "concat", "-safe", "0")
  )
  expect_identical(args[[6]], "-i")
  expect_match(ffm_compile(p), '-f concat -safe 0 -i "', fixed = TRUE)
})

# M06: explicit ffm_map() in complex mode (D-M06-1) ------------------------------

test_that("complex mode combines the auto [vout] map with an explicit map", {
  f1 <- make_input()
  f2 <- make_input()
  p <- ffm_map(ffm_hstack(ffm_files(c(f1, f2), "out.mp4")), "0:a")
  expect_match(ffm_compile(p), '-map "[vout]" -map "0:a"', fixed = TRUE)
  args <- tidymedia:::ffm_args(p)
  expect_identical(sum(args == "-map"), 2L)
  expect_identical(args[which(args == "-map") + 1L], c("[vout]", "0:a"))
})

# M06: cheap token validation (D-M06-3) ------------------------------------------

test_that("ffm_codec() rejects unclean codec tokens", {
  f <- make_input()
  p <- ffm_files(f, "out.mp4")
  expect_error(ffm_codec(p, video = "libx264 -evil"), "single clean token")
  expect_error(ffm_codec(p, audio = "aac; rm -rf ~"), "single clean token")
  expect_no_error(ffm_codec(p, video = "libx264", audio = "copy"))
})

test_that("ffm_pixel_format() rejects unclean format tokens", {
  f <- make_input()
  p <- ffm_files(f, "out.mp4")
  expect_error(ffm_pixel_format(p, "yuv 420p"), "single clean token")
  expect_error(ffm_pixel_format(p, 'yuv420p"'), "single clean token")
  expect_no_error(ffm_pixel_format(p, "yuv420p10le"))
})

# M06: safe execution with hostile paths (binary-gated) --------------------------

test_that("ffm_run() handles paths with spaces, quotes, $, and backticks", {
  skip_if_no_ffmpeg()
  src <- make_test_video()
  dir <- withr::local_tempdir()
  hostile_in <- file.path(dir, "in put's $HOME `x`.mp4")
  expect_true(file.copy(src, hostile_in))
  hostile_out <- file.path(dir, "out put's $HOME `y`.mp4")
  res <- ffm_files(hostile_in, hostile_out) |>
    ffm_scale(width = 32, height = 32) |>
    ffm_codec(video = "libx264") |>
    ffm_run()
  expect_null(attr(res, "status"))
  expect_true(file.exists(hostile_out))
  expect_gt(file.size(hostile_out), 0)
})

# M06 review fixes (Opus F1, F3, F4) ---------------------------------------------

test_that("ffm_run() aborts loudly when FFmpeg fails", {
  skip_if_no_ffmpeg()
  f <- make_input() # empty file, not decodable
  out <- withr::local_tempfile(fileext = ".mp4")
  p <- ffm_codec(ffm_files(f, out), video = "libx264")
  expect_error(ffm_run(p), "exited with status")
})

test_that("ffm_output_options() rejects quoted option groups", {
  f <- make_input()
  p <- ffm_files(f, "out.mp4")
  expect_error(
    ffm_output_options(p, '-metadata title="My Title"'),
    "quote characters"
  )
  expect_error(ffm_output_options(p, "-metadata title='x y'"), "quote characters")
})

test_that("check_token() rejects tokens with a leading dash", {
  f <- make_input()
  p <- ffm_files(f, "out.mp4")
  expect_error(ffm_codec(p, video = "-vn"), "single clean token")
  expect_error(ffm_pixel_format(p, "-f"), "single clean token")
  expect_no_error(ffm_codec(p, video = "libvpx-vp9"))
})
