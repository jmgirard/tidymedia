# M45: `audio_stream` on separate_audio_video() / _batch, and the enriched abort
# a multi-track input gets when FFmpeg refuses the audio output.
#
# The `NULL` default of this verb's `audio_stream` means EVERY audio track --
# `-map 0:a`, what the verb has compiled since it shipped -- not the first track
# the extraction verbs' `NULL` selects (D023). So the baseline below is a pin
# against silently narrowing the Matroska callers who receive all their tracks
# today, and it is the pre-change form, recorded from commit b548902 (the last
# commit before this milestone; `git diff b548902 HEAD -- R/ffmpeg.R` touched no
# separation code, so the strings the working tree produced before T1 are that
# commit's). Verbatim, on inst/extdata/sample.mp4:
#
#   audio: -y -i "<...>/sample.mp4" -codec:a copy -map 0:a "audio.aac"
#   video: -y -i "<...>/sample.mp4" -codec:v copy -map 0:v "video.mp4"
#
# The tests pin that form as a template so they do not depend on a temp path.


# AC1: what each spelling compiles ------------------------------------------

baseline_pair <- function(infile, audiofile, videofile) {
  c(
    audio = sprintf('-y -i "%s" -codec:a copy -map 0:a "%s"', infile, audiofile),
    video = sprintf('-y -i "%s" -codec:v copy -map 0:v "%s"', infile, videofile)
  )
}

test_that("the default compiles the pre-change every-track pair", {
  infile <- make_input("mkv")
  expect_identical(
    separate_audio_video(infile, "audio.aac", "video.mp4", run = FALSE),
    baseline_pair(infile, "audio.aac", "video.mp4")
  )
})

test_that("an explicit NULL compiles the same pair as the absent argument", {
  infile <- make_input("mkv")
  expect_identical(
    separate_audio_video(infile, "a.aac", "v.mp4", audio_stream = NULL,
                         run = FALSE),
    baseline_pair(infile, "a.aac", "v.mp4")
  )
})

test_that("audio_stream narrows the audio map and leaves the video map alone", {
  infile <- make_input("mkv")
  out <- separate_audio_video(infile, "a.aac", "v.mp4", audio_stream = 1,
                              run = FALSE)
  expect_match(out[["audio"]], "-map 0:a:1", fixed = TRUE)
  expect_match(out[["video"]], "-map 0:v", fixed = TRUE)
  # The every-track form must be GONE from the audio command, not merely joined
  # by the narrow one: `-map 0:a -map 0:a:1` would carry every track and pass a
  # containment-only assertion (M43 made ffm_map() append).
  expect_false(grepl("-map 0:a ", out[["audio"]], fixed = TRUE))
  expect_identical(out[["video"]], baseline_pair(infile, "a.aac", "v.mp4")[["video"]])
})

test_that("audio_stream = 1L compiles the identical pair to audio_stream = 1", {
  infile <- make_input("mkv")
  expect_identical(
    separate_audio_video(infile, "a.aac", "v.mp4", audio_stream = 1L, run = FALSE),
    separate_audio_video(infile, "a.aac", "v.mp4", audio_stream = 1, run = FALSE)
  )
})

test_that("audio_stream = 0 selects the first track rather than every track", {
  # The discriminator between this verb's NULL and an explicit 0: on the
  # extraction verbs they compile the same map, here they must not.
  infile <- make_input("mkv")
  narrow <- separate_audio_video(infile, "a.aac", "v.mp4", audio_stream = 0,
                                 run = FALSE)
  expect_match(narrow[["audio"]], "-map 0:a:0", fixed = TRUE)
  expect_false(identical(narrow, baseline_pair(infile, "a.aac", "v.mp4")))
})

test_that("a non-whole or out-of-range audio_stream is rejected by name", {
  infile <- make_input("mkv")
  expect_error(
    separate_audio_video(infile, "a.aac", "v.mp4", audio_stream = 1.5,
                         run = FALSE),
    "audio_stream"
  )
  expect_error(
    separate_audio_video(infile, "a.aac", "v.mp4", audio_stream = -1,
                         run = FALSE),
    "audio_stream"
  )
  expect_error(
    separate_audio_video(infile, "a.aac", "v.mp4", audio_stream = NA,
                         run = FALSE),
    "audio_stream"
  )
})

# AC2: the enriched abort on the executing path -----------------------------

test_that("ffm_run() still words its non-zero exit as the enrichment reads it", {
  # The coupling pin. run_separation_audio() tells a non-zero EXIT apart from
  # every other failure (a missing binary, an unreadable path) by parsing
  # ffm_run()'s own message, so a reword there would silently retire the
  # enrichment. This fails loudly instead.
  skip_if_no_ffmpeg()
  infile <- make_test_video()
  out <- withr::local_tempfile(fileext = ".mp3")
  # Copying AAC into an MP3 container is a guaranteed non-zero exit; leaving the
  # codec unset would simply re-encode and succeed.
  p <- ffm_codec(ffm_map(ffm_files(infile, out), "0:a"), audio = "copy")
  cnd <- tryCatch(ffm_run(p), error = function(e) e)
  expect_match(cli::ansi_strip(conditionMessage(cnd)), "exited with status -?[0-9]+")
  expect_false(is.na(ffmpeg_exit_status(cnd)))
})

test_that("a multi-track input into a single-stream container names the way out", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  audio <- withr::local_tempfile(fileext = ".aac")
  video <- withr::local_tempfile(fileext = ".mp4")
  cnd <- tryCatch(separate_audio_video(infile, audio, video),
                  error = function(e) e)
  expect_s3_class(cnd, "tidymedia_multitrack_separation")
  msg <- cli::ansi_strip(conditionMessage(cnd))
  expect_match(msg, "exited with status -?[0-9]+")  # AC2: carries the status
  expect_match(msg, "3 audio tracks")         # AC2: states the count
  expect_match(msg, "audio_stream")           # AC2: names the way to take one
  expect_match(msg, ".mka", fixed = TRUE)     # AC2: names a container for several
})

test_that("naming a track falls through to ffm_run()'s own abort", {
  # AC2's narrowing: with `0:a:1` mapped, exactly one stream was carried, so the
  # failure is the AAC-into-MP3 copy and "take one track" would be false advice.
  skip_if_no_ffmpeg()
  infile <- make_multitrack_video()
  audio <- withr::local_tempfile(fileext = ".mp3")
  video <- withr::local_tempfile(fileext = ".mp4")
  cnd <- tryCatch(
    separate_audio_video(infile, audio, video, audio_stream = 1),
    error = function(e) e
  )
  expect_s3_class(cnd, "error")
  expect_false(inherits(cnd, "tidymedia_multitrack_separation"))
  expect_match(cli::ansi_strip(conditionMessage(cnd)), "exited with status")
})

test_that("a single-track input falls through to ffm_run()'s own abort", {
  skip_if_no_ffprobe()
  infile <- make_test_video()
  audio <- withr::local_tempfile(fileext = ".mp3")
  video <- withr::local_tempfile(fileext = ".mp4")
  cnd <- tryCatch(separate_audio_video(infile, audio, video),
                  error = function(e) e)
  expect_s3_class(cnd, "error")
  expect_false(inherits(cnd, "tidymedia_multitrack_separation"))
})

test_that("an unavailable ffprobe falls through to ffm_run()'s own abort", {
  # D024's fail-open consequence: "could not check" must look exactly like
  # "nothing to add", never like a second failure mode.
  skip_if_no_ffmpeg()
  infile <- make_multitrack_video()
  audio <- withr::local_tempfile(fileext = ".aac")
  video <- withr::local_tempfile(fileext = ".mp4")
  local_mocked_bindings(find_ffprobe = function() NULL)
  cnd <- tryCatch(separate_audio_video(infile, audio, video),
                  error = function(e) e)
  expect_false(inherits(cnd, "tidymedia_multitrack_separation"))
  expect_match(cli::ansi_strip(conditionMessage(cnd)), "exited with status")
})

test_that("a successful multi-track separation into .mka raises nothing", {
  # The other side of the enrichment: Matroska holds all three tracks, which is
  # exactly why this verb's NULL default was left meaning every track.
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  audio <- withr::local_tempfile(fileext = ".mka")
  video <- withr::local_tempfile(fileext = ".mkv")
  expect_no_error(separate_audio_video(infile, audio, video))
  expect_equal(count_audio_streams(audio), 3L)
})

test_that("a brace-bearing output path does not execute in the abort", {
  # M44 review F1: cli glue-interpolates every bullet in the calling frame, so
  # user data must go through a cli field. `{n}` names a local of the message
  # builder, which would print a filename that does not exist; a stray `{` aborts
  # with "could not evaluate cli expression" instead of the diagnostic.
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  dir <- withr::local_tempdir()
  audio <- file.path(dir, "my{n}.aac")
  video <- file.path(dir, "v.mp4")
  cnd <- tryCatch(separate_audio_video(infile, audio, video),
                  error = function(e) e)
  expect_s3_class(cnd, "tidymedia_multitrack_separation")
  expect_match(cli::ansi_strip(conditionMessage(cnd)), "my{n}.aac", fixed = TRUE)
})

# AC4: the batch sibling -----------------------------------------------------

sep_jobs <- function(inputs, tag = "") {
  tibble::tibble(
    input = inputs,
    audiofile = sprintf("a%s%d.aac", tag, seq_along(inputs)),
    videofile = sprintf("v%s%d.mp4", tag, seq_along(inputs))
  )
}

test_that("the batch argument reaches every audio row and no video row", {
  # A NON-default value, because asserting only the default passes even when the
  # argument never reaches the fan-out (M39's lesson).
  infile <- make_input("mkv")
  out <- separate_audio_video_batch(sep_jobs(c(infile, infile)),
                                    audio_stream = 2, run = FALSE)
  audio <- out$command[out$stream == "audio"]
  video <- out$command[out$stream == "video"]
  expect_true(all(grepl("-map 0:a:2", audio, fixed = TRUE)))
  expect_false(any(grepl("0:a", video, fixed = TRUE)))
  expect_true(all(grepl("-map 0:v", video, fixed = TRUE)))
})

test_that("the batch default leaves every row on every audio track", {
  infile <- make_input("mkv")
  out <- separate_audio_video_batch(sep_jobs(c(infile, infile)), run = FALSE)
  audio <- out$command[out$stream == "audio"]
  expect_true(all(grepl("-map 0:a ", audio, fixed = TRUE)))
  expect_false(any(grepl("0:a:", audio, fixed = TRUE)))
})

test_that("an audio_stream column overrides the argument per row", {
  infile <- make_input("mkv")
  jobs <- sep_jobs(c(infile, infile, infile))
  jobs$audio_stream <- c(0, 2, NA)
  out <- separate_audio_video_batch(jobs, audio_stream = 1, run = FALSE)
  audio <- out$command[out$stream == "audio"]
  expect_match(audio[[1]], "-map 0:a:0", fixed = TRUE)
  expect_match(audio[[2]], "-map 0:a:2", fixed = TRUE)
  # The NA cell is the column form of the NULL sentinel: every track for that
  # row, overriding the argument rather than deferring to it (D023's rule,
  # applied to this verb's NULL meaning).
  expect_match(audio[[3]], "-map 0:a ", fixed = TRUE)
  expect_false(grepl("0:a:", audio[[3]], fixed = TRUE))
  expect_false(any(grepl("0:a", out$command[out$stream == "video"], fixed = TRUE)))
})

test_that("an all-NA audio_stream column is accepted and keeps every track", {
  # R types an all-NA column logical, which an is.numeric-only guard rejects
  # (M34's lesson).
  infile <- make_input("mkv")
  jobs <- sep_jobs(c(infile, infile))
  jobs$audio_stream <- NA
  out <- separate_audio_video_batch(jobs, run = FALSE)
  expect_true(all(grepl("-map 0:a ", out$command[out$stream == "audio"],
                        fixed = TRUE)))
})

test_that("a non-numeric audio_stream column and an NA argument are rejected", {
  infile <- make_input("mkv")
  jobs <- sep_jobs(c(infile, infile))
  jobs$audio_stream <- c("0", "1")
  expect_error(separate_audio_video_batch(jobs, run = FALSE),
               "keep every audio track")
  # The argument's front-door check: NA resolves to the NULL sentinel in the
  # reshape, so without it this would silently keep every track (M37/M41).
  expect_error(
    separate_audio_video_batch(sep_jobs(infile), audio_stream = NA, run = FALSE),
    "audio_stream"
  )
})

test_that("the result carries the resolved audio_stream, and only when asked", {
  # The return schema is a contract (M19): supplying the argument or the column
  # adds a column, and supplying neither must leave the pre-change shape.
  infile <- make_input("mkv")
  plain <- separate_audio_video_batch(sep_jobs(infile), run = FALSE)
  expect_false("audio_stream" %in% names(plain))
  named <- separate_audio_video_batch(sep_jobs(c(infile, infile)),
                                      audio_stream = 1, run = FALSE)
  expect_identical(named$audio_stream, c(1, NA, 1, NA))
})

test_that("a failed audio row records success = FALSE and warns once", {
  skip_if_no_ffprobe()
  multi <- make_multitrack_video()
  single <- make_test_video()
  dir <- withr::local_tempdir()
  jobs <- tibble::tibble(
    input     = c(multi, multi, single),
    audiofile = file.path(dir, c("bad.aac", "named.aac", "ok.aac")),
    videofile = file.path(dir, c("v1.mkv", "v2.mkv", "v3.mkv")),
    # Row 2 names a track, so its audio output succeeds; row 3 is single-track.
    audio_stream = c(NA, 1, NA)
  )
  w <- tryCatch(separate_audio_video_batch(jobs), warning = function(w) w)
  expect_s3_class(w, "tidymedia_multitrack_separation")
  msg <- cli::ansi_strip(conditionMessage(w))
  expect_match(msg, "Input row 1")
  expect_false(grepl("Input row 2", msg))
  expect_false(grepl("Input row 3", msg))
  expect_match(msg, "3 audio tracks")
  expect_match(msg, "audio_stream")
  expect_match(msg, ".mka", fixed = TRUE)

  res <- suppressWarnings(separate_audio_video_batch(jobs))
  expect_false(res$success[[1]])          # row 1's audio: refused
  expect_true(res$success[[3]])           # row 2's audio: one track named
  expect_true(res$success[[5]])           # row 3's audio: single-track input
})

test_that("a batch where every row names a track probes nothing", {
  skip_if_no_ffmpeg()
  multi <- make_multitrack_video()
  dir <- withr::local_tempdir()
  jobs <- tibble::tibble(
    input     = c(multi, multi),
    audiofile = file.path(dir, c("a1.aac", "a2.aac")),
    videofile = file.path(dir, c("v1.mkv", "v2.mkv")),
    audio_stream = c(0, 1)
  )
  probed <- 0L
  local_mocked_bindings(
    count_audio_streams = function(file) {
      probed <<- probed + 1L
      3L
    }
  )
  expect_no_warning(separate_audio_video_batch(jobs))
  expect_identical(probed, 0L)
})


# AC3: nothing runs a binary under run = FALSE -------------------------------

test_that("run = FALSE invokes no binary on either separation verb", {
  # Counts invocations rather than raising from the mock: run_separation_audio()
  # and count_audio_streams() both wrap their calls in tryCatch(), which would
  # swallow a stop() and let a probe on the compile path pass unseen (M44's
  # lesson). find_ffmpeg()/find_ffprobe() are mocked too, since PATH masking
  # alone leaves find_program()'s stored-config fallback in play.
  infile <- make_input("mkv")
  called <- 0L
  local_mocked_bindings(
    run_program = function(...) {
      called <<- called + 1L
      character(0)
    },
    find_ffmpeg = function() {
      called <<- called + 1L
      "ffmpeg"
    },
    find_ffprobe = function() {
      called <<- called + 1L
      "ffprobe"
    }
  )
  separate_audio_video(infile, "a.aac", "v.mp4", run = FALSE)
  separate_audio_video(infile, "a.aac", "v.mp4", audio_stream = 1, run = FALSE)
  separate_audio_video_batch(sep_jobs(infile), run = FALSE)
  separate_audio_video_batch(sep_jobs(infile), audio_stream = 1, run = FALSE)
  expect_identical(called, 0L)
})

test_that("run = FALSE compiles with the locators stubbed to abort", {
  # AC3's evidence as worded. Weaker than the counting test above and kept beside
  # it rather than instead of it: nothing in a test can see whether the call site
  # wraps its call in a tryCatch() that would swallow this stop(), which is how
  # M44's equivalent test stayed green with the gate it pinned deleted. The
  # counting test is the one that cannot go vacuous.
  infile <- make_input("mkv")
  local_mocked_bindings(
    find_ffmpeg = function() stop("no binary may be located on this path"),
    find_ffprobe = function() stop("no binary may be located on this path")
  )
  expect_no_error({
    separate_audio_video(infile, "a.aac", "v.mp4", run = FALSE)
    separate_audio_video(infile, "a.aac", "v.mp4", audio_stream = 1, run = FALSE)
    separate_audio_video_batch(sep_jobs(infile), run = FALSE)
    separate_audio_video_batch(sep_jobs(infile), audio_stream = 1, run = FALSE)
  })
})

test_that("every documented call of both verbs compiles with no binary", {
  # AC3 over the roxygen @examples themselves: an ungated example that shells out
  # breaks the CI-absent build, which is where this class of defect surfaces
  # (M30's lesson).
  sample <- system.file("extdata", "sample.mp4", package = "tidymedia")
  called <- 0L
  local_mocked_bindings(
    run_program = function(...) {
      called <<- called + 1L
      character(0)
    },
    find_ffmpeg = function() {
      called <<- called + 1L
      "ffmpeg"
    },
    find_ffprobe = function() {
      called <<- called + 1L
      "ffprobe"
    }
  )
  expect_no_error({
    separate_audio_video(sample, "audio.aac", "video.mp4", run = FALSE)
    separate_audio_video(sample, "audio.mp3", "video.mp4",
                         audio_codec = "libmp3lame", run = FALSE)
    separate_audio_video(sample, "audio.aac", "video.mp4",
                         audio_stream = 1, run = FALSE)
    separate_audio_video_batch(
      tibble::tibble(
        input     = c(sample, sample),
        audiofile = c("a1.aac", "a2.aac"),
        videofile = c("v1.mp4", "v2.mp4")
      ),
      run = FALSE
    )
  })
  expect_identical(called, 0L)
})


test_that("the extraction verbs' NULL still means the first track", {
  # The other half of the split this milestone records: parameterizing
  # audio_stream_map()'s NULL resolution must not have moved D023's callers.
  infile <- make_input("mkv")
  expect_match(extract_audio(infile, "a.aac", run = FALSE), "-map 0:a:0",
               fixed = TRUE)
  expect_match(convert_audio(infile, "a.mp3", run = FALSE), "-map 0:a:0",
               fixed = TRUE)
})
