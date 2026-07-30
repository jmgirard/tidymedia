# M48: `audio_stream` on crop_video() / segment_video() (+ _batch), under the
# same rule M47 gave standardize_video() and anonymize_video() (D026).
#
# These two verbs did NOT have M47's defect: they emit `-map 0` and so already
# carried every audio track. What they carried was too much and too little at
# once -- `-map 0` also carries subtitles and data, which fails outright into
# .mp4 on a subtitle-bearing input (exit 8, no default mp4 subtitle encoder),
# and it offered no way to name one track. Both now compile
# `-map 0:v? -map 0:a?` when no track is named and `-map 0:v? -map 0:a:<n>`
# when one is.
#
# The `?` on the unselected specifiers is load-bearing: a bare `-map 0:a`
# aborts FFmpeg on a silent video and a bare `-map 0:v` aborts on an audio-only
# file, both exit 234, where `-map 0` handled either. The named specifier keeps
# no `?`, so naming a track the input lacks stays an FFmpeg error (D023/D026).
#
# On the `reencode = FALSE` branch the map has to REPLACE ffm_copy()'s `-map 0`
# rather than append beside it, because ffm_map() appends (D023). That is
# ffm_map(replace = TRUE)'s first in-package caller.

# The pre-M48 commands, recorded from master at 0b9985a and committed here as
# templates taking the map arguments. Templates rather than fixed strings
# because the input path is a per-test tempfile; templates rather than a
# comparison against "what master returns today" because that stops being
# checkable the moment this branch merges (M47's lesson, same shape).
#
#   crop:            -y -i "<f>" -vf "crop=..." -codec:a copy -map 0 "out.mp4"
#   segment(TRUE):   -y -i "<f>" -codec:a copy -ss 0 -to 1 "seg.mp4"
#   segment(FALSE):  -y -ss 0 -to 1 -i "<f>" -codec:v copy -codec:a copy \
#                       -avoid_negative_ts make_zero -map 0 "seg.mp4"
#
# Note segment(reencode = TRUE) carried NO map at all, so its `maps` default is
# the empty string: the re-encode branch is the one place in this milestone
# where a map appears where none stood before.

crop_command <- function(infile, maps = "-map 0 ", outfile = "out.mp4") {
  paste0(
    '-y -i "', infile, '"',
    ' -vf "crop=w=32:h=32:x=(in_w-out_w)/2:y=(in_h-out_h)/2"',
    " -codec:a copy ", maps, '"', outfile, '"'
  )
}

segment_reencode_command <- function(infile, maps = "", outfile = "seg.mp4") {
  paste0(
    '-y -i "', infile, '" -codec:a copy -ss 0 -to 1 ', maps, '"', outfile, '"'
  )
}

segment_copy_command <- function(infile, maps = "-map 0 ", outfile = "seg.mp4") {
  paste0(
    '-y -ss 0 -to 1 -i "', infile, '"',
    " -codec:v copy -codec:a copy -avoid_negative_ts make_zero ",
    maps, '"', outfile, '"'
  )
}

# Count -map arguments. A containment assertion cannot see a duplicate, and
# ffm_map() appends since M43, so the count is the discriminator (M43/M45/M47).
map_count <- function(cmd) {
  vapply(cmd, function(x) {
    sum(gregexpr("-map ", x, fixed = TRUE)[[1]] > 0)
  }, integer(1), USE.NAMES = FALSE)
}

crop_of <- function(f, ...) crop_video(f, "out.mp4", 32, 32, ..., run = FALSE)

segment_of <- function(f, ...) {
  segment_video(f, 0, 1, outfiles = "seg.mp4", ..., run = FALSE)$command
}


# AC1: the unset selector ------------------------------------------------------

test_that("an unset audio_stream compiles every video and every audio stream", {
  f <- make_input()
  expect_identical(crop_of(f), crop_command(f, "-map 0:v? -map 0:a? "))
  expect_identical(
    segment_of(f),
    segment_reencode_command(f, "-map 0:v? -map 0:a? ")
  )
  expect_identical(
    segment_of(f, reencode = FALSE),
    segment_copy_command(f, "-map 0:v? -map 0:a? ")
  )
})

test_that("an explicit NULL audio_stream compiles what the absent argument does", {
  f <- make_input()
  expect_identical(crop_of(f, audio_stream = NULL), crop_of(f))
  expect_identical(
    segment_of(f, audio_stream = NULL),
    segment_of(f)
  )
  expect_identical(
    segment_of(f, reencode = FALSE, audio_stream = NULL),
    segment_of(f, reencode = FALSE)
  )
})

test_that("each verb compiles exactly two -map arguments, never more", {
  # The count, not containment: appending is what M43 made possible and what a
  # `grepl("-map 0:a?")` assertion cannot see going wrong.
  f <- make_input()
  expect_identical(map_count(crop_of(f)), 2L)
  expect_identical(map_count(segment_of(f)), 2L)
  expect_identical(map_count(segment_of(f, reencode = FALSE)), 2L)
})


# AC2: a named track -----------------------------------------------------------

test_that("audio_stream narrows the audio map and leaves the video map alone", {
  f <- make_input()
  expect_identical(
    crop_of(f, audio_stream = 2),
    crop_command(f, "-map 0:v? -map 0:a:2 ")
  )
  expect_identical(
    segment_of(f, audio_stream = 2),
    segment_reencode_command(f, "-map 0:v? -map 0:a:2 ")
  )
  expect_identical(
    segment_of(f, reencode = FALSE, audio_stream = 2),
    segment_copy_command(f, "-map 0:v? -map 0:a:2 ")
  )
})

test_that("the named specifier carries no `?`, so a missing track stays an error", {
  # D026's third bullet: `0:a:9` on a 3-track input must reach FFmpeg as an
  # error rather than compile to a silently audio-less output.
  f <- make_input()
  expect_false(grepl("0:a:2?", crop_of(f, audio_stream = 2), fixed = TRUE))
  expect_false(
    grepl("0:a:2?", segment_of(f, reencode = FALSE, audio_stream = 2),
          fixed = TRUE)
  )
})

test_that("the copy branch narrows ffm_copy()'s map rather than appending to it", {
  # ffm_copy() sets `-map 0`; appending the selection beside it would compile
  # THREE maps and duplicate every stream. The discriminator is that no bare
  # `-map 0` survives -- exactly what ffm_map(replace = TRUE) buys.
  f <- make_input()
  for (cmd in c(segment_of(f, reencode = FALSE),
                segment_of(f, reencode = FALSE, audio_stream = 2))) {
    expect_identical(map_count(cmd), 2L)
    expect_false(grepl("-map 0 ", cmd, fixed = TRUE))
  }
})

test_that("audio_stream = 0 is a selection, not the unset sentinel", {
  # The two compile DIFFERENTLY on these verbs, unlike on the extraction verbs
  # where NULL resolves to the first track: here NULL keeps every track (D026).
  f <- make_input()
  expect_identical(crop_of(f, audio_stream = 0),
                   crop_command(f, "-map 0:v? -map 0:a:0 "))
  expect_false(identical(crop_of(f, audio_stream = 0), crop_of(f)))
})


# AC5 / AC6: the batch siblings ------------------------------------------------

crop_jobs <- function(f, ...) {
  tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"), ...)
}

segment_jobs <- function(f, ...) {
  tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"),
    start = c(0, 1), end = c(1, 2), ...
  )
}

test_that("the batch argument reaches every row", {
  f <- make_input()
  out <- crop_video_batch(crop_jobs(f), width = 32, height = 32,
                          audio_stream = 2, run = FALSE)
  expect_true(all(grepl("-map 0:v? -map 0:a:2", out$command, fixed = TRUE)))
  out <- segment_video_batch(segment_jobs(f), audio_stream = 2, run = FALSE)
  expect_true(all(grepl("-map 0:v? -map 0:a:2", out$command, fixed = TRUE)))
  # And on the copy branch, where the selection has to REPLACE ffm_copy()'s map
  # rather than sit beside it.
  out <- segment_video_batch(segment_jobs(f), reencode = FALSE,
                             audio_stream = 2, run = FALSE)
  expect_true(all(grepl("-map 0:v? -map 0:a:2", out$command, fixed = TRUE)))
  expect_identical(map_count(out$command), c(2L, 2L))
})

test_that("an audio_stream column overrides the argument per row", {
  f <- make_input()
  out <- crop_video_batch(crop_jobs(f, audio_stream = c(1, NA)),
                          width = 32, height = 32, audio_stream = 2,
                          run = FALSE)
  # NA is the column form of NULL, so row 2 keeps EVERY track -- it does not
  # fall back to the argument, which is what an ABSENT column means (D023/D026).
  expect_match(out$command[[1]], "-map 0:v? -map 0:a:1", fixed = TRUE)
  expect_match(out$command[[2]], "-map 0:v? -map 0:a?", fixed = TRUE)

  out <- segment_video_batch(segment_jobs(f, audio_stream = c(1, NA)),
                             audio_stream = 2, run = FALSE)
  expect_match(out$command[[1]], "-map 0:v? -map 0:a:1", fixed = TRUE)
  expect_match(out$command[[2]], "-map 0:v? -map 0:a?", fixed = TRUE)
})

test_that("a one-row batch call compiles byte-identically to the scalar call", {
  f <- make_input()
  for (sel in list(NULL, 2)) {
    expect_identical(
      crop_video_batch(tibble::tibble(input = f, output = "out.mp4"),
                       width = 32, height = 32, audio_stream = sel,
                       run = FALSE)$command,
      crop_video(f, "out.mp4", 32, 32, audio_stream = sel, run = FALSE)
    )
    expect_identical(
      segment_video_batch(
        tibble::tibble(input = f, output = "seg.mp4", start = 0, end = 1),
        audio_stream = sel, run = FALSE
      )$command,
      segment_video(f, 0, 1, outfiles = "seg.mp4", audio_stream = sel,
                    run = FALSE)$command
    )
  }
})

test_that("segment_video()'s own fan-out carries the argument to every segment", {
  # This verb builds its OWN jobs tibble from one input, so the argument has to
  # reach the closure rather than a column.
  f <- make_input()
  out <- segment_video(f, c(0, 1, 2), c(1, 2, 3),
                       outfiles = c("a.mp4", "b.mp4", "c.mp4"),
                       audio_stream = 2, run = FALSE)
  expect_identical(nrow(out), 3L)
  expect_true(all(grepl("-map 0:v? -map 0:a:2", out$command, fixed = TRUE)))
})

test_that("a wrongly typed audio_stream column aborts before any row runs", {
  f <- make_input()
  for (bad in list(c("0", "1"), c(TRUE, FALSE))) {
    err <- expect_error(
      crop_video_batch(crop_jobs(f, audio_stream = bad), width = 32,
                       height = 32, run = FALSE)
    )
    expect_match(conditionMessage(err), "audio_stream")
    expect_match(conditionMessage(err), "keep every audio track")
    expect_identical(rlang::call_name(conditionCall(err)), "crop_video_batch")

    err <- expect_error(
      segment_video_batch(segment_jobs(f, audio_stream = bad), run = FALSE)
    )
    expect_match(conditionMessage(err), "audio_stream")
    expect_match(conditionMessage(err), "keep every audio track")
    expect_identical(rlang::call_name(conditionCall(err)),
                     "segment_video_batch")
  }
})

test_that("a scalar audio_stream = NA aborts rather than compiling the default", {
  # The column path resolves NA to the NULL sentinel, so without the batch
  # verbs' own front-door check this would quietly keep every track (M37/M41).
  f <- make_input()
  expect_error(
    crop_video_batch(crop_jobs(f), width = 32, height = 32,
                     audio_stream = NA, run = FALSE),
    "audio_stream"
  )
  expect_error(
    segment_video_batch(segment_jobs(f), audio_stream = NA, run = FALSE),
    "audio_stream"
  )
})


# AC7 --------------------------------------------------------------------------

test_that("run = FALSE runs no binary at the default hardware", {
  f <- make_input()
  # Count invocations rather than stop()ing in the mock: these call sites sit
  # under tryCatch() in places, which swallows a raising mock and leaves the
  # test green with the gate it exists to pin deleted (M44).
  n <- 0L
  local_mocked_bindings(
    run_program = function(...) {
      n <<- n + 1L
      list(status = 0L, stdout = character(), stderr = character())
    },
    find_ffmpeg = function(...) {
      n <<- n + 1L
      "ffmpeg"
    },
    find_ffprobe = function(...) {
      n <<- n + 1L
      "ffprobe"
    }
  )
  crop_video(f, "out.mp4", 32, 32, run = FALSE)
  crop_video(f, "out.mp4", 32, 32, audio_stream = 2, run = FALSE)
  segment_video(f, 0, 1, outfiles = "seg.mp4", audio_stream = 2, run = FALSE)
  segment_video(f, 0, 1, outfiles = "seg.mp4", reencode = FALSE,
                audio_stream = 2, run = FALSE)
  crop_video_batch(crop_jobs(f), width = 32, height = 32, audio_stream = 2,
                   run = FALSE)
  segment_video_batch(segment_jobs(f), audio_stream = 2, run = FALSE)
  expect_identical(n, 0L)
  # Prove the mock is actually in scope rather than silently inert: one
  # run = TRUE call must trip the counter. Without this, `n == 0` is equally
  # consistent with "no binary ran" and "the mock never bound" (M39's
  # discriminate-the-test rule, M44's counting-mock rule).
  crop_video_batch(crop_jobs(f), width = 32, height = 32, run = TRUE)
  expect_gt(n, 0L)
  # Deliberately NOT extended to hardware = "nvenc": resolve_hw_encoder()
  # reaches ffmpeg("-encoders") before `run` is consulted, so that path DOES
  # shell out under run = FALSE on these verbs too. Carried as a ROADMAP
  # candidate row since M47; it falsifies D024's "sole exception" sentence and
  # is not M48's to fix.
})


# AC3: execution on a 3-audio-track, 1-subtitle .mkv ---------------------------

test_that("crop_video(audio_stream = ) writes exactly the named track", {
  skip_if_no_ffmpeg()
  infile <- make_multitrack_subtitle_video()
  out <- withr::local_tempfile(fileext = ".mkv")
  crop_video(infile, out, width = 32, height = 32, audio_stream = 2)
  expect_identical(stream_types(out), c("video", "audio"))
  # The language tag, not the position: it names the track independently of
  # stream order, and track 2 is `fra` in this fixture.
  expect_identical(audio_languages(out), "fra")
})

test_that("crop_video() with no selection keeps every audio track and no subtitle", {
  skip_if_no_ffmpeg()
  infile <- make_multitrack_subtitle_video()
  out <- withr::local_tempfile(fileext = ".mkv")
  crop_video(infile, out, width = 32, height = 32)
  # Every audio track survives -- `-map 0` did that too, and D026 keeps it --
  # but the subtitle no longer does. That is the deliberate change.
  expect_identical(stream_types(out),
                   c("video", "audio", "audio", "audio"))
  expect_identical(audio_languages(out), c("eng", "spa", "fra"))
})

test_that("crop_video() into .mp4 now succeeds on a subtitle-bearing input", {
  # On master this call FAILED: `-map 0` carried the subtitle into .mp4 and
  # FFmpeg exited 8 with no default mp4 subtitle encoder. Not carrying
  # subtitles is what fixes it.
  skip_if_no_ffmpeg()
  infile <- make_multitrack_subtitle_video()
  out <- withr::local_tempfile(fileext = ".mp4")
  expect_no_error(crop_video(infile, out, width = 32, height = 32))
  expect_true(file.exists(out) && file.size(out) > 0)
  expect_identical(stream_types(out), c("video", "audio", "audio", "audio"))
})

test_that("segment_video() carries the named track on both branches", {
  skip_if_no_ffmpeg()
  infile <- make_multitrack_subtitle_video()
  for (reencode in c(TRUE, FALSE)) {
    out <- withr::local_tempfile(fileext = ".mkv")
    segment_video(infile, 0, 1, outfiles = out, reencode = reencode,
                  audio_stream = 2)
    expect_identical(stream_types(out), c("video", "audio"))
    expect_identical(audio_languages(out), "fra")
  }
})

test_that("a named track the input lacks is an FFmpeg error, not an R one", {
  # D026's third bullet, end to end: the named specifier carries no `?`, so
  # `0:a:9` on a 3-track input must fail rather than write a silent file.
  skip_if_no_ffmpeg()
  infile <- make_multitrack_subtitle_video()
  out <- withr::local_tempfile(fileext = ".mkv")
  expect_error(crop_video(infile, out, width = 32, height = 32,
                          audio_stream = 9))
})
