# M49: `audio_stream` on format_for_web() (+ _batch), under D026's rule
# unchanged -- the unselected case keeps EVERY audio track.
#
# This verb emitted NO `-map` before M49, so FFmpeg's implicit selection chose
# for it: one stream of each type, preferring whichever audio track carries the
# container's DEFAULT disposition. Measured on-branch before any source edit
# (ffmpeg 8.1.2, macOS; a 3-audio-track .mkv, eng/spa/fra, DEFAULT moved to
# track 2): the output carried only `fra` -- the THIRD track, chosen by a rule
# the caller never wrote and could not see.
#
# normalize_audio() takes a DIFFERENT rule for the unselected case and lives in
# test-audio-stream-normalize.R; D028 records the split.
#
# The pre-M49 command, recorded from master at a4fc322 and committed here as a
# template taking the map argument -- a template rather than a comparison
# against "what master returns today", which stops being checkable the moment
# this branch merges (M47's lesson, M48's shape). It carried no map at all, so
# the `maps` default is the empty string.
#
#   -y -i "<f>" -vf "crop=..." -codec:v libx264 -codec:a aac -pix_fmt yuv420p \
#      -movflags +faststart "out.mp4"

web_command <- function(infile, maps = "", outfile = "out.mp4") {
  paste0(
    '-y -i "', infile, '"',
    ' -vf "crop=w=floor(in_w/2)*2:h=floor(in_h/2)*2:x=(in_w-out_w)/2:y=(in_h-out_h)/2"',
    " -codec:v libx264 -codec:a aac -pix_fmt yuv420p -movflags +faststart ",
    maps, '"', outfile, '"'
  )
}

# Count -map arguments. A containment assertion cannot see a duplicate, and
# ffm_map() appends since M43, so the count is the discriminator (M43/M45/M47).
web_map_count <- function(cmd) {
  vapply(cmd, function(x) {
    sum(gregexpr("-map ", x, fixed = TRUE)[[1]] > 0)
  }, integer(1), USE.NAMES = FALSE)
}

web_of <- function(f, ...) format_for_web(f, "out.mp4", ..., run = FALSE)

web_jobs <- function(f, ...) {
  tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"), ...)
}


# AC1: the scalar verb ---------------------------------------------------------

test_that("format_for_web() with no selection compiles every video and audio stream", {
  f <- make_input()
  expect_identical(web_of(f), web_command(f, "-map \"0:v?\" -map \"0:a?\" "))
  expect_identical(web_of(f, audio_stream = NULL), web_of(f))
  expect_identical(web_map_count(web_of(f)), 2L)
})

test_that("format_for_web(audio_stream = ) narrows the audio map only", {
  f <- make_input()
  expect_identical(web_of(f, audio_stream = 2),
                   web_command(f, "-map \"0:v?\" -map \"0:a:2\" "))
  expect_identical(web_map_count(web_of(f, audio_stream = 2)), 2L)
  # D026's third bullet: the named specifier carries no `?`, so naming a track
  # the input lacks stays an FFmpeg error rather than a silently audio-less
  # output.
  expect_false(grepl("0:a:2?", web_of(f, audio_stream = 2), fixed = TRUE))
})

test_that("format_for_web(audio_stream = 0) is a selection, not the unset sentinel", {
  f <- make_input()
  expect_identical(web_of(f, audio_stream = 0),
                   web_command(f, "-map \"0:v?\" -map \"0:a:0\" "))
  expect_false(identical(web_of(f, audio_stream = 0), web_of(f)))
})


# AC1: the batch sibling -------------------------------------------------------

test_that("the format_for_web_batch() argument reaches every row", {
  f <- make_input()
  out <- format_for_web_batch(web_jobs(f), audio_stream = 2, run = FALSE)
  expect_true(all(grepl("-map \"0:v?\" -map \"0:a:2\"", out$command, fixed = TRUE)))
  expect_identical(web_map_count(out$command), c(2L, 2L))
})

test_that("a format_for_web_batch() audio_stream column overrides the argument per row", {
  f <- make_input()
  out <- format_for_web_batch(web_jobs(f, audio_stream = c(1, NA)),
                              audio_stream = 2, run = FALSE)
  # NA is the column form of NULL, so row 2 keeps EVERY track -- it does not
  # fall back to the argument, which is what an ABSENT column means (D023/D026).
  expect_match(out$command[[1]], "-map \"0:v?\" -map \"0:a:1\"", fixed = TRUE)
  expect_match(out$command[[2]], "-map \"0:v?\" -map \"0:a?\"", fixed = TRUE)
})

test_that("a one-row format_for_web_batch() call matches the scalar call byte for byte", {
  f <- make_input()
  for (sel in list(NULL, 2)) {
    expect_identical(
      format_for_web_batch(tibble::tibble(input = f, output = "out.mp4"),
                           audio_stream = sel, run = FALSE)$command,
      format_for_web(f, "out.mp4", audio_stream = sel, run = FALSE)
    )
  }
})

test_that("a wrongly typed format_for_web_batch() audio_stream column aborts up front", {
  f <- make_input()
  for (bad in list(c("0", "1"), c(TRUE, FALSE))) {
    err <- expect_error(
      format_for_web_batch(web_jobs(f, audio_stream = bad), run = FALSE)
    )
    expect_match(conditionMessage(err), "audio_stream")
    expect_match(conditionMessage(err), "keep every audio track")
    expect_identical(rlang::call_name(conditionCall(err)),
                     "format_for_web_batch")
  }
})

test_that("a scalar format_for_web_batch(audio_stream = NA) aborts rather than compiling the default", {
  # The column path resolves NA to the NULL sentinel, so without the batch
  # verb's own front-door check this would quietly keep every track (M37/M41).
  f <- make_input()
  expect_error(
    format_for_web_batch(web_jobs(f), audio_stream = NA, run = FALSE),
    "audio_stream"
  )
})


# AC7 / D024: run = FALSE stays binary-free ------------------------------------

test_that("format_for_web()'s new argument runs no binary under run = FALSE", {
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
  format_for_web(f, "out.mp4", audio_stream = 2, run = FALSE)
  format_for_web_batch(web_jobs(f), audio_stream = 2, run = FALSE)
  expect_identical(n, 0L)
  # Prove the mock is in scope rather than silently inert: one run = TRUE call
  # must trip the counter. Without this, `n == 0` is equally consistent with
  # "no binary ran" and "the mock never bound" (M39/M44).
  format_for_web_batch(web_jobs(f), run = TRUE)
  expect_gt(n, 0L)
})


# AC4: execution on a 3-audio-track .mkv with DEFAULT moved off track 0 --------

test_that("format_for_web() with no selection carries every audio track", {
  # T1's baseline: on master this wrote ONE track, `fra`, because that is where
  # this fixture's DEFAULT disposition sits.
  skip_if_no_ffmpeg()
  infile <- make_multitrack_video(default_track = 2)
  out <- withr::local_tempfile(fileext = ".mp4")
  format_for_web(infile, out)
  expect_identical(audio_languages(out), c("eng", "spa", "fra"))
})

test_that("format_for_web(audio_stream = ) writes exactly the named track", {
  skip_if_no_ffmpeg()
  infile <- make_multitrack_video(default_track = 2)
  out <- withr::local_tempfile(fileext = ".mp4")
  format_for_web(infile, out, audio_stream = 1)
  expect_identical(audio_languages(out), "spa")
})
