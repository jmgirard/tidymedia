# audio_codec on the four re-encode verbs and their _batch siblings (M35, D017).
#
# Unlike video_codec's NULL sentinel (M34/D016), audio_codec defaults to "copy":
# these verbs never need to touch the audio, so re-encoding it to the local
# build's container default was pure loss. The compiled commands therefore DO
# change — every default gains -codec:a copy — and the tests below pin the new
# literals so the change stays exactly that one token. NULL is retained as the
# "emit no -codec:a" escape hatch. All compile tests here are binary-free.

# crop_video() ----------------------------------------------------------------

test_that("crop_video() stream-copies audio by default", {
  f <- make_input()
  cmd <- crop_video(f, "out.mp4", width = 100, height = 50, x = 0, y = 0,
                    run = FALSE)
  # The pre-M35 literal, plus -codec:a copy and nothing else.
  expect_equal(
    as.character(cmd),
    paste0('-y -i "', f, '" -vf "crop=w=100:h=50:x=0:y=0" ',
           '-codec:a copy -map 0 "out.mp4"')
  )
  expect_no_match(as.character(cmd), "-codec:v", fixed = TRUE)
})

test_that("crop_video(audio_codec = ) names the audio encoder", {
  f <- make_input()
  cmd <- crop_video(f, "out.mp4", width = 100, height = 50,
                    audio_codec = "aac", run = FALSE)
  expect_match(as.character(cmd), "-codec:a aac", fixed = TRUE)
  expect_no_match(as.character(cmd), "-codec:a copy", fixed = TRUE)
})

test_that("crop_video(audio_codec = NULL) emits no -codec:a", {
  f <- make_input()
  cmd <- crop_video(f, "out.mp4", width = 100, height = 50,
                    audio_codec = NULL, run = FALSE)
  expect_no_match(as.character(cmd), "-codec:a", fixed = TRUE)
  # The rest of the command is untouched by the escape hatch.
  expect_equal(
    as.character(cmd),
    paste0('-y -i "', f, '" -vf "crop=w=100:h=50:x=',
           '(in_w-out_w)/2:y=(in_h-out_h)/2" -map 0 "out.mp4"')
  )
})

test_that("crop_video() rejects a non-token audio_codec", {
  f <- make_input()
  expect_error(
    crop_video(f, "out.mp4", width = 100, height = 50,
               audio_codec = "aac -evil", run = FALSE),
    "clean token"
  )
  expect_error(
    crop_video(f, "out.mp4", width = 100, height = 50,
               audio_codec = 1, run = FALSE),
    class = "rlang_error"
  )
})

test_that("crop_video() sets both codecs when both are named", {
  f <- make_input()
  cmd <- crop_video(f, "out.mp4", width = 100, height = 50,
                    video_codec = "libx265", audio_codec = "aac", run = FALSE)
  expect_match(as.character(cmd), "-codec:v libx265", fixed = TRUE)
  expect_match(as.character(cmd), "-codec:a aac", fixed = TRUE)
})

# segment_video() --------------------------------------------------------------

test_that("segment_video() stream-copies audio on the re-encode path", {
  f <- make_input()
  out <- segment_video(f, start = 0, end = 1, outfiles = "seg.mp4",
                       run = FALSE)
  # The pre-M35 literal, plus -codec:a copy and nothing else.
  expect_equal(
    as.character(out$command),
    paste0('-y -i "', f, '" -codec:a copy -ss 0 -to 1 "seg.mp4"')
  )
  expect_no_match(as.character(out$command), "-codec:v", fixed = TRUE)
})

test_that("segment_video(audio_codec = ) names the audio encoder", {
  f <- make_input()
  out <- segment_video(f, start = 0, end = 1, outfiles = "seg.mp4",
                       audio_codec = "aac", run = FALSE)
  expect_match(as.character(out$command), "-codec:a aac", fixed = TRUE)
})

test_that("segment_video(audio_codec = NULL) emits no -codec:a", {
  f <- make_input()
  out <- segment_video(f, start = 0, end = 1, outfiles = "seg.mp4",
                       audio_codec = NULL, run = FALSE)
  expect_no_match(as.character(out$command), "-codec:a", fixed = TRUE)
})

test_that("segment_video(reencode = FALSE) still stream-copies audio", {
  f <- make_input()
  # The copy path already sets -codec:a copy via ffm_copy(); the default
  # audio_codec agrees with it, so this combination is legal.
  out <- segment_video(f, start = 0, end = 1, outfiles = "seg.mp4",
                       reencode = FALSE, run = FALSE)
  expect_match(as.character(out$command), "-codec:a copy", fixed = TRUE)
  expect_match(as.character(out$command), "-codec:v copy", fixed = TRUE)
})

test_that("segment_video() rejects a non-copy audio_codec on the copy path", {
  f <- make_input()
  # A stream copy runs no encoder, so naming one (or unsetting the codec, which
  # ffm_copy() would then overwrite) is not meaningful (D017, mirroring D016).
  expect_error(
    segment_video(f, start = 0, end = 1, outfiles = "seg.mp4",
                  reencode = FALSE, audio_codec = "aac", run = FALSE),
    "re-encoding cut"
  )
  expect_error(
    segment_video(f, start = 0, end = 1, outfiles = "seg.mp4",
                  reencode = FALSE, audio_codec = NULL, run = FALSE),
    "re-encoding cut"
  )
})

test_that("segment_video_batch() checks audio_codec per row against reencode", {
  f <- make_input()
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"), start = c(0, 1),
    end = c(1, 2), reencode = c(TRUE, FALSE)
  )
  # Batch-wide audio_codec = "aac" is fine on the re-encoding row and a conflict
  # on the stream-copy row, so the mixed table must abort.
  expect_error(
    segment_video_batch(jobs, audio_codec = "aac", run = FALSE),
    "re-encoding cut"
  )
  # The same table with the default audio_codec compiles both rows.
  out <- segment_video_batch(jobs, run = FALSE)
  expect_equal(nrow(out), 2)
  expect_true(all(grepl("-codec:a copy", out$command, fixed = TRUE)))
})

test_that("segment_video_batch() takes a per-row audio_codec column", {
  f <- make_input()
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"), start = c(0, 1),
    end = c(1, 2), audio_codec = c("aac", NA_character_)
  )
  out <- segment_video_batch(jobs, run = FALSE)
  expect_match(as.character(out$command[[1]]), "-codec:a aac", fixed = TRUE)
  expect_no_match(as.character(out$command[[2]]), "-codec:a", fixed = TRUE)
})

# compare_videos() -------------------------------------------------------------

test_that("compare_videos() emits no -codec:a when no audio is mapped", {
  f1 <- make_input()
  f2 <- make_input()
  # Default audio = NULL drops audio entirely, so there is no stream for a
  # -codec:a to apply to and the pre-M35 command is unchanged.
  cmd <- compare_videos(c(f1, f2), "out.mp4", run = FALSE)
  expect_no_match(as.character(cmd), "-codec:a", fixed = TRUE)
})

test_that("compare_videos(audio = ) stream-copies the carried track", {
  f1 <- make_input()
  f2 <- make_input()
  cmd <- compare_videos(c(f1, f2), "out.mp4", audio = 0, run = FALSE)
  expect_match(as.character(cmd), "-codec:a copy", fixed = TRUE)
  expect_match(as.character(cmd), "-map 0:a", fixed = TRUE)
})

test_that("compare_videos() carries a named audio codec into the complex path", {
  f1 <- make_input()
  f2 <- make_input()
  cmd <- compare_videos(c(f1, f2), "out.mp4", audio = 1,
                        video_codec = "libx264", audio_codec = "aac",
                        run = FALSE)
  # One command carrying the filtergraph, its [vout] label and map, and both
  # codecs (D009/IP3: the codecs ride alongside the graph, never inside it).
  cmd <- as.character(cmd)
  expect_match(cmd, "-filter_complex", fixed = TRUE)
  expect_match(cmd, "[vout]", fixed = TRUE)
  expect_match(cmd, '-map "[vout]"', fixed = TRUE)
  expect_match(cmd, "-map 1:a", fixed = TRUE)
  expect_match(cmd, "-codec:v libx264", fixed = TRUE)
  expect_match(cmd, "-codec:a aac", fixed = TRUE)
})

test_that("compare_videos() rejects an audio codec with no audio mapped", {
  f1 <- make_input()
  f2 <- make_input()
  expect_error(
    compare_videos(c(f1, f2), "out.mp4", audio_codec = "aac", run = FALSE),
    "no audio"
  )
  # NULL is the "leave it unset" escape hatch, not a request to encode, so it
  # stays legal with no audio mapped.
  expect_no_error(
    compare_videos(c(f1, f2), "out.mp4", audio_codec = NULL, run = FALSE)
  )
})

# picture_in_picture() ---------------------------------------------------------

test_that("picture_in_picture() emits no -codec:a when no audio is mapped", {
  f1 <- make_input()
  f2 <- make_input()
  cmd <- picture_in_picture(f1, f2, "out.mp4", run = FALSE)
  expect_no_match(as.character(cmd), "-codec:a", fixed = TRUE)
})

test_that("picture_in_picture(audio = ) stream-copies the carried track", {
  f1 <- make_input()
  f2 <- make_input()
  cmd <- picture_in_picture(f1, f2, "out.mp4", audio = 0, run = FALSE)
  expect_match(as.character(cmd), "-codec:a copy", fixed = TRUE)
  expect_match(as.character(cmd), "-map 0:a", fixed = TRUE)
})

test_that("picture_in_picture() carries a named audio codec into the complex path", {
  f1 <- make_input()
  f2 <- make_input()
  cmd <- as.character(
    picture_in_picture(f1, f2, "out.mp4", audio = 1, video_codec = "libx264",
                       audio_codec = "aac", run = FALSE)
  )
  expect_match(cmd, "-filter_complex", fixed = TRUE)
  expect_match(cmd, '-map "[vout]"', fixed = TRUE)
  expect_match(cmd, "-map 1:a", fixed = TRUE)
  expect_match(cmd, "-codec:v libx264", fixed = TRUE)
  expect_match(cmd, "-codec:a aac", fixed = TRUE)
})

test_that("picture_in_picture() rejects an audio codec with no audio mapped", {
  f1 <- make_input()
  f2 <- make_input()
  expect_error(
    picture_in_picture(f1, f2, "out.mp4", audio_codec = "aac", run = FALSE),
    "no audio"
  )
  expect_no_error(
    picture_in_picture(f1, f2, "out.mp4", audio_codec = NULL, run = FALSE)
  )
})

# _batch siblings: the per-row audio_codec column -----------------------------

test_that("crop_video_batch() takes a per-row audio_codec column", {
  f <- make_input()
  jobs <- tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"),
    audio_codec = c("aac", NA_character_)
  )
  out <- crop_video_batch(jobs, width = 100, height = 50, run = FALSE)
  expect_match(as.character(out$command[[1]]), "-codec:a aac", fixed = TRUE)
  # NA is the column's way of writing the argument's NULL: no -codec:a at all.
  expect_no_match(as.character(out$command[[2]]), "-codec:a", fixed = TRUE)
})

test_that("crop_video_batch() defaults every row to a stream copy", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"))
  out <- crop_video_batch(jobs, width = 100, height = 50, run = FALSE)
  expect_true(all(grepl("-codec:a copy", out$command, fixed = TRUE)))
})

test_that("the batch siblings accept an all-NA (logical) audio_codec column", {
  f <- make_input()
  # R types an all-NA column logical, so an is.character-only guard would
  # wrongly reject the documented "every row unset" table (M34 lesson).
  crop <- crop_video_batch(
    tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"),
                   audio_codec = NA),
    width = 100, height = 50, run = FALSE
  )
  expect_false(any(grepl("-codec:a", crop$command, fixed = TRUE)))

  cmp <- compare_videos_batch(
    tibble::tibble(inputs = list(c(f, f), c(f, f)),
                   output = c("a.mp4", "b.mp4"), audio_codec = NA),
    audio = 0, run = FALSE
  )
  expect_false(any(grepl("-codec:a", cmp$command, fixed = TRUE)))

  pip <- picture_in_picture_batch(
    tibble::tibble(main = c(f, f), overlay = c(f, f),
                   output = c("a.mp4", "b.mp4"), audio_codec = NA),
    audio = 0, run = FALSE
  )
  expect_false(any(grepl("-codec:a", pip$command, fixed = TRUE)))
})

test_that("the batch siblings reject a wrongly typed audio_codec column", {
  f <- make_input()
  expect_error(
    crop_video_batch(
      tibble::tibble(input = f, output = "a.mp4", audio_codec = 1),
      width = 100, height = 50, run = FALSE
    ),
    "audio_codec"
  )
  # An all-NA numeric column is not the logical one R produces, so it is still
  # rejected rather than read as "all default".
  expect_error(
    crop_video_batch(
      tibble::tibble(input = f, output = "a.mp4", audio_codec = NA_real_),
      width = 100, height = 50, run = FALSE
    ),
    "audio_codec"
  )
  expect_error(
    compare_videos_batch(
      tibble::tibble(inputs = list(c(f, f)), output = "a.mp4",
                     audio_codec = 1),
      run = FALSE
    ),
    "audio_codec"
  )
  expect_error(
    picture_in_picture_batch(
      tibble::tibble(main = f, overlay = f, output = "a.mp4",
                     audio_codec = 1),
      run = FALSE
    ),
    "audio_codec"
  )
})

test_that("the composite batch siblings take a per-row audio_codec column", {
  f <- make_input()
  cmp <- compare_videos_batch(
    tibble::tibble(inputs = list(c(f, f), c(f, f)),
                   output = c("a.mp4", "b.mp4"),
                   audio_codec = c("aac", NA_character_)),
    audio = 0, run = FALSE
  )
  expect_match(as.character(cmp$command[[1]]), "-codec:a aac", fixed = TRUE)
  expect_no_match(as.character(cmp$command[[2]]), "-codec:a", fixed = TRUE)

  pip <- picture_in_picture_batch(
    tibble::tibble(main = c(f, f), overlay = c(f, f),
                   output = c("a.mp4", "b.mp4"),
                   audio_codec = c("aac", NA_character_)),
    audio = 1, run = FALSE
  )
  expect_match(as.character(pip$command[[1]]), "-codec:a aac", fixed = TRUE)
  expect_no_match(as.character(pip$command[[2]]), "-codec:a", fixed = TRUE)
})

# The batch `audio` (stream index) column guards -------------------------------
# Not about audio_codec, but the same all-NA typing trap on the neighbouring
# column: both composite batch verbs now guard it up front and identically.

test_that("both composite batch verbs accept an all-NA audio column", {
  f <- make_input()
  # R types `audio = NA` logical while the column must otherwise be numeric; the
  # roxygen documents NA as "drop audio", so it has to be accepted (M34 lesson).
  cmp <- compare_videos_batch(
    tibble::tibble(inputs = list(c(f, f)), output = "a.mp4", audio = NA),
    run = FALSE
  )
  expect_no_match(as.character(cmp$command[[1]]), ":a", fixed = TRUE)

  pip <- picture_in_picture_batch(
    tibble::tibble(main = f, overlay = f, output = "a.mp4", audio = NA),
    run = FALSE
  )
  expect_no_match(as.character(pip$command[[1]]), ":a", fixed = TRUE)

  # An all-NA *numeric* column is a well-typed "drop audio everywhere" too.
  pip2 <- picture_in_picture_batch(
    tibble::tibble(main = f, overlay = f, output = "a.mp4",
                   audio = NA_real_),
    run = FALSE
  )
  expect_no_match(as.character(pip2$command[[1]]), ":a", fixed = TRUE)
})

test_that("both composite batch verbs reject a wrongly typed audio column", {
  f <- make_input()
  # compare_videos_batch had no up-front guard at all before M35, so a bad
  # column only failed later, per row.
  expect_error(
    compare_videos_batch(
      tibble::tibble(inputs = list(c(f, f)), output = "a.mp4", audio = "0"),
      run = FALSE
    ),
    "audio"
  )
  expect_error(
    picture_in_picture_batch(
      tibble::tibble(main = f, overlay = f, output = "a.mp4", audio = "0"),
      run = FALSE
    ),
    "audio"
  )
  # An all-NA character column is not the logical one R produces, so the
  # tightened guard rejects it rather than reading it as "drop audio".
  expect_error(
    picture_in_picture_batch(
      tibble::tibble(main = f, overlay = f, output = "a.mp4",
                     audio = NA_character_),
      run = FALSE
    ),
    "audio"
  )
  # A real logical column is not an all-NA one either.
  expect_error(
    picture_in_picture_batch(
      tibble::tibble(main = c(f, f), overlay = c(f, f),
                     output = c("a.mp4", "b.mp4"), audio = c(TRUE, FALSE)),
      run = FALSE
    ),
    "audio"
  )
})

# Execution: the copied audio really does survive untouched -------------------

test_that("crop_video() leaves the audio stream's codec unchanged", {
  skip_if_no_ffmpeg()
  skip_if_no_ffprobe()
  # MP3 audio in an MP4 makes the difference observable: FFmpeg's default audio
  # encoder for MP4 is AAC, so only a genuine stream copy can keep mp3.
  input <- make_mp3_audio_video()
  expect_equal(probe_audio(infile = input)$codec_name, "mp3")

  copied <- withr::local_tempfile(fileext = ".mp4")
  crop_video(input, copied, width = 32, height = 32)
  expect_equal(
    probe_audio(infile = copied)$codec_name,
    probe_audio(infile = input)$codec_name
  )

  # And the escape hatch really does hand the audio back to the container,
  # which is the pre-M35 behavior this milestone stopped defaulting to.
  unset <- withr::local_tempfile(fileext = ".mp4")
  crop_video(input, unset, width = 32, height = 32, audio_codec = NULL)
  expect_equal(probe_audio(infile = unset)$codec_name, "aac")
})

# Argument spelling across all eight verbs -------------------------------------

test_that("all eight verbs carry the D014 audio_codec spelling", {
  # AC1: exact spelling, "copy" default, no acodec/audio alias.
  verbs <- c("crop_video", "segment_video", "compare_videos",
             "picture_in_picture", "crop_video_batch", "segment_video_batch",
             "compare_videos_batch", "picture_in_picture_batch")
  for (verb in verbs) {
    fo <- formals(get(verb))
    expect_true("audio_codec" %in% names(fo), label = verb)
    expect_equal(fo$audio_codec, "copy", label = verb)
    expect_false("acodec" %in% names(fo), label = verb)
    # audio_codec sits beside video_codec, not appended after `run`.
    expect_lt(
      which(names(fo) == "audio_codec"),
      which(names(fo) == "hardware"),
      label = verb
    )
  }
})

test_that("audio_codec is not added to the fixed-recipe or audio verbs", {
  # D016/D017's boundary rule: a fixed-recipe verb keeps its codecs hidden.
  # format_for_web hard-codes AAC by identity, so it gains nothing here.
  expect_false("audio_codec" %in% names(formals(format_for_web)))
  expect_false("audio_codec" %in% names(formals(format_for_web_batch)))
  # extract_audio already had the argument before M35; it is untouched.
  expect_equal(formals(extract_audio)$audio_codec, "copy")
})
