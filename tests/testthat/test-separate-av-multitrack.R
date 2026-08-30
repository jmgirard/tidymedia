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
# The tests pin that form as a template so they do not depend on a temp path --
# with the map specifier quoted, `-map "0:a"` and `-map "0:v"`, since M50/D031.
# The transcript above is left in its pre-M50 spelling because it records what
# b548902 produced; the SELECTION it pins is what this file is about, and that
# is unchanged.
#
# PORTABILITY, learned the hard way at review (M27's lesson on a new surface):
# WHICH container refuses several audio streams is FFmpeg-version dependent. The
# adts muxer on ffmpeg 8.1.2 rejects a multi-stream `.aac` ("adts muxer does not
# support more than one stream of type audio"); ffmpeg 6.1.1, which ubuntu-latest
# ships, writes that file happily and exits 0, so seven tests triggering the
# enrichment through `.aac` saw no condition at all and went red on CI while
# passing on macOS. The enrichment tests below therefore trigger the failure with
# an AAC-to-MP3 STREAM COPY, which no FFmpeg build can do whatever its muxer
# limits, and the container-refusal occasion gets its own test that probes this
# FFmpeg first and skips when it does not refuse.


# AC1: what each spelling compiles ------------------------------------------

baseline_pair <- function(infile, audiofile, videofile) {
  c(
    audio = sprintf('-y -i "%s" -codec:a copy -map "0:a" "%s"', infile, audiofile),
    video = sprintf('-y -i "%s" -codec:v copy -map "0:v" "%s"', infile, videofile)
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
  expect_match(out[["audio"]], "-map \"0:a:1\"", fixed = TRUE)
  expect_match(out[["video"]], "-map \"0:v\"", fixed = TRUE)
  # The every-track form must be GONE from the audio command, not merely joined
  # by the narrow one: `-map 0:a -map 0:a:1` would carry every track and pass a
  # containment-only assertion (M43 made ffm_map() append).
  expect_false(grepl("-map \"0:a\" ", out[["audio"]], fixed = TRUE))
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
  expect_match(narrow[["audio"]], "-map \"0:a:0\"", fixed = TRUE)
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

# Skip unless THIS FFmpeg's single-stream audio muxer actually refuses several
# audio streams. Assert the environment's own property before trusting a result
# that depends on it (M43's fixture lesson), rather than assuming the local
# build's behavior is universal.
skip_unless_adts_refuses_multistream <- function(infile) {
  skip_if_no_ffmpeg()
  out <- withr::local_tempfile(fileext = ".aac")
  st <- suppressWarnings(tryCatch(
    system2("ffmpeg",
            c("-y", "-loglevel", "error", "-i", infile, "-map", "0:a",
              "-c:a", "copy", out),
            stdout = FALSE, stderr = FALSE),
    error = function(e) 1L
  ))
  testthat::skip_if_not(
    !identical(as.integer(st), 0L),
    message = "this FFmpeg writes several audio streams to .aac without refusing"
  )
}

test_that("a failed audio command on a multi-track input names the way out", {
  # Triggered by an AAC-to-MP3 stream copy, which fails on every FFmpeg build --
  # NOT by the .aac stream-count refusal, which only ffmpeg >= 8 performs. AC2
  # requires each clause to hold on *any* non-zero exit, so the trigger being an
  # invalid copy rather than a muxer refusal is the criterion's own case.
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  audio <- withr::local_tempfile(fileext = ".mp3")
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

test_that("the container-refusal occasion is covered where FFmpeg refuses", {
  # The real-world case the milestone exists for: the audio codec is fine and the
  # container simply will not hold three streams. Only ffmpeg >= 8's adts muxer
  # refuses, so this skips on older builds rather than pretending to be portable.
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  skip_unless_adts_refuses_multistream(infile)
  audio <- withr::local_tempfile(fileext = ".aac")
  video <- withr::local_tempfile(fileext = ".mp4")
  cnd <- tryCatch(separate_audio_video(infile, audio, video),
                  error = function(e) e)
  expect_s3_class(cnd, "tidymedia_multitrack_separation")
  expect_match(cli::ansi_strip(conditionMessage(cnd)), "3 audio tracks")
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
  # .mp3 rather than .aac: the trigger must be a failure on every FFmpeg build,
  # or on ffmpeg 6.x this test asserts a fall-through from a run that succeeded.
  audio <- withr::local_tempfile(fileext = ".mp3")
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
  audio <- file.path(dir, "my{n}.mp3")
  video <- file.path(dir, "v.mp4")
  cnd <- tryCatch(separate_audio_video(infile, audio, video),
                  error = function(e) e)
  expect_s3_class(cnd, "tidymedia_multitrack_separation")
  expect_match(cli::ansi_strip(conditionMessage(cnd)), "my{n}.mp3", fixed = TRUE)
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
  expect_true(all(grepl("-map \"0:a:2\"", audio, fixed = TRUE)))
  expect_false(any(grepl("0:a", video, fixed = TRUE)))
  expect_true(all(grepl("-map \"0:v\"", video, fixed = TRUE)))
})

test_that("the batch default leaves every row on every audio track", {
  infile <- make_input("mkv")
  out <- separate_audio_video_batch(sep_jobs(c(infile, infile)), run = FALSE)
  audio <- out$command[out$stream == "audio"]
  expect_true(all(grepl("-map \"0:a\" ", audio, fixed = TRUE)))
  expect_false(any(grepl("0:a:", audio, fixed = TRUE)))
})

test_that("an audio_stream column overrides the argument per row", {
  infile <- make_input("mkv")
  jobs <- sep_jobs(c(infile, infile, infile))
  jobs$audio_stream <- c(0, 2, NA)
  out <- separate_audio_video_batch(jobs, audio_stream = 1, run = FALSE)
  audio <- out$command[out$stream == "audio"]
  expect_match(audio[[1]], "-map \"0:a:0\"", fixed = TRUE)
  expect_match(audio[[2]], "-map \"0:a:2\"", fixed = TRUE)
  # The NA cell is the column form of the NULL sentinel: every track for that
  # row, overriding the argument rather than deferring to it (D023's rule,
  # applied to this verb's NULL meaning).
  expect_match(audio[[3]], "-map \"0:a\" ", fixed = TRUE)
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
  expect_true(all(grepl("-map \"0:a\" ", out$command[out$stream == "audio"],
                        fixed = TRUE)))
})

test_that("a non-numeric audio_stream column and an NA argument are rejected", {
  infile <- make_input("mkv")
  jobs <- sep_jobs(c(infile, infile))
  jobs$audio_stream <- c("0", "1")
  err <- tryCatch(separate_audio_video_batch(jobs, run = FALSE),
                  error = function(e) cli::ansi_strip(conditionMessage(e)))
  expect_match(err, "keep every audio track")
  # And the inherited default must be ABSENT, not merely outvoted: dropping the
  # `na_means =` argument would fall back to "drop audio", which is false here,
  # and a presence-only assertion would stay green (M40's lesson).
  expect_no_match(err, "drop audio")
  # The argument's front-door check: NA resolves to the NULL sentinel in the
  # reshape, so without it this would silently keep every track (M37/M41).
  expect_error(
    separate_audio_video_batch(sep_jobs(infile), audio_stream = NA, run = FALSE),
    "audio_stream"
  )
})

test_that("a bad audio_stream CELL is blamed by the caller's row number", {
  # The reshape turns N input rows into 2N, so the per-row check inside the
  # pipeline reports an index of the RESHAPED table -- "In index: 3" for a
  # two-row jobs table, a row the caller cannot find, and it names Layer-1's
  # pmap (M45 review F4; M32 + M41).
  infile <- make_input("mkv")
  jobs <- sep_jobs(c(infile, infile))
  jobs$audio_stream <- c(0, -1)
  err <- tryCatch(separate_audio_video_batch(jobs, run = FALSE),
                  error = function(e) cli::ansi_strip(conditionMessage(e)))
  expect_match(err, "row 2", fixed = TRUE)
  expect_no_match(err, "index", fixed = TRUE)
  expect_no_match(err, "pmap", fixed = TRUE)
  # Two bad cells must not crash the message: a plural governed by a vector
  # throws `length(object) == 1` (M18), so this needs 2+ items to be a real test.
  jobs$audio_stream <- c(-1, 1.5)
  err2 <- tryCatch(separate_audio_video_batch(jobs, run = FALSE),
                   error = function(e) cli::ansi_strip(conditionMessage(e)))
  expect_match(err2, "row 1 and 2", fixed = TRUE)
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
    # Row 1's .mp3 fails in every build (AAC cannot be copied into MP3); rows 2
    # and 3 write one AAC track to .aac, which succeeds in every build.
    audiofile = file.path(dir, c("bad.mp3", "named.aac", "ok.aac")),
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

test_that("the batch warning names two failed rows without crashing", {
  # A one-row warning cannot see M18's plural crash, and cannot show that the
  # aggregation names EVERY affected row rather than the first.
  skip_if_no_ffprobe()
  multi <- make_multitrack_video()
  single <- make_test_video()
  dir <- withr::local_tempdir()
  jobs <- tibble::tibble(
    input     = c(multi, single, multi),
    audiofile = file.path(dir, c("f1.mp3", "ok.aac", "f3.mp3")),
    videofile = file.path(dir, c("v1.mkv", "v2.mkv", "v3.mkv"))
  )
  w <- tryCatch(separate_audio_video_batch(jobs), warning = function(w) w)
  expect_s3_class(w, "tidymedia_multitrack_separation")
  msg <- cli::ansi_strip(conditionMessage(w))
  expect_match(msg, "2 audio outputs failed")
  expect_match(msg, "Input row 1")
  expect_match(msg, "Input row 3")
  expect_false(grepl("Input row 2", msg))
})

test_that("a brace-bearing path does not execute in the batch warning either", {
  # The scalar abort and the batch warning escape braces independently, so the
  # scalar's test does not cover this one (M44 review F1).
  skip_if_no_ffprobe()
  multi <- make_multitrack_video()
  dir <- withr::local_tempdir()
  jobs <- tibble::tibble(
    input     = multi,
    audiofile = file.path(dir, "my{n}.mp3"),
    videofile = file.path(dir, "v.mkv")
  )
  w <- tryCatch(separate_audio_video_batch(jobs), warning = function(w) w)
  expect_s3_class(w, "tidymedia_multitrack_separation")
  expect_match(cli::ansi_strip(conditionMessage(w)), "my{n}.mp3", fixed = TRUE)
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
  expect_match(extract_audio(infile, "a.aac", run = FALSE), "-map \"0:a:0\"",
               fixed = TRUE)
  expect_match(convert_audio(infile, "a.mp3", run = FALSE), "-map \"0:a:0\"",
               fixed = TRUE)
})


# M088: the video command runs even after the audio command fails ------------

# What `master` raised on these two paths, recorded on 2026-08-29 before any
# R/ change on this branch, with ffmpeg 9.0.1 on macOS (arm64):
#
#   enriched multi-track branch:
#     class     tidymedia_multitrack_separation, tidymedia_ffmpeg_exit,
#               rlang_error, error, condition
#     tm_status 234
#   n <= 1 fall-open branch:
#     class     tidymedia_ffmpeg_exit, rlang_error, error, condition
#     tm_status 234
#
# The class vectors are pinned literally below; the STATUS is not, because 234
# is this FFmpeg build's number for an AAC-into-MP3 stream copy and another
# build may answer differently. What the criterion needs is that `tm_status`
# still carries the exit status the run reported, so the tests read the number
# out of the rendered message and require the field to equal it -- a fact
# stated independently of the field being checked.
sep_status_in_message <- function(cnd) {
  msg <- cli::ansi_strip(conditionMessage(cnd))
  hit <- regmatches(msg, regexpr("exited with status (-?[0-9]+)", msg))
  expect_length(hit, 1L)
  as.integer(sub("exited with status ", "", hit, fixed = TRUE))
}

# The audio failure every one of these tests is built on: an AAC-into-MP3 stream
# copy, which no FFmpeg build can perform (the .aac stream-count refusal is
# version-dependent, per this file's PORTABILITY note). The VIDEO command beside
# it is the default `-codec:v copy` into .mp4, which succeeds on the same inputs.
sep_fresh_video <- function(ext = ".mp4", env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ext, .local_envir = env)
  # withr::local_tempfile() only reserves the name, but the criterion is about a
  # path that did not exist BEFORE the call, so assert that rather than assume it.
  expect_false(file.exists(path))
  path
}

test_that("a failed audio command still leaves the video file behind", {
  # AC1, on the enriched multi-track branch.
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  audio <- withr::local_tempfile(fileext = ".mp3")
  video <- sep_fresh_video()
  cnd <- tryCatch(separate_audio_video(infile, audio, video),
                  error = function(e) e)
  expect_s3_class(cnd, "error")            # AC1: the call still aborts
  expect_true(file.exists(video))          # AC1: the video was written anyway
  expect_gt(file.size(video), 0)
  expect_equal(nrow(probe_video(infile = video)), 1L)
})

test_that("the enriched abort keeps its class vector and status", {
  # AC2, branch one: the enriched multi-track diagnostic.
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  audio <- withr::local_tempfile(fileext = ".mp3")
  video <- sep_fresh_video()
  cnd <- tryCatch(separate_audio_video(infile, audio, video),
                  error = function(e) e)
  expect_identical(
    class(cnd),
    c("tidymedia_multitrack_separation", "tidymedia_ffmpeg_exit",
      "rlang_error", "error", "condition")
  )
  expect_identical(cnd$tm_status, sep_status_in_message(cnd))
})

test_that("the fall-open re-raise keeps its class vector and status", {
  # AC2, branch two: a single-track input takes the `n <= 1L` fall-open, which
  # re-raises ffm_run()'s own condition -- so the multi-track class must be
  # ABSENT here, and the video must still be written.
  skip_if_no_ffprobe()
  infile <- make_test_video()
  audio <- withr::local_tempfile(fileext = ".mp3")
  video <- sep_fresh_video()
  cnd <- tryCatch(separate_audio_video(infile, audio, video),
                  error = function(e) e)
  expect_identical(
    class(cnd),
    c("tidymedia_ffmpeg_exit", "rlang_error", "error", "condition")
  )
  expect_identical(cnd$tm_status, sep_status_in_message(cnd))
  expect_true(file.exists(video))          # AC1 on this branch too
})

test_that("the abort names the video file it wrote", {
  # AC4, both branches: the added line is what tells the caller the video half
  # survived. Asserted on the rendered message, since that is what a caller
  # reads; basename() alone, because the temp path is not stable.
  skip_if_no_ffprobe()
  # Named, so a failure says WHICH branch failed: the two go through different
  # halves of run_separation_audio() and only the label separates them here.
  cases <- list("enriched multi-track" = make_multitrack_video(),
                "n <= 1 fall-open" = make_test_video())
  for (branch in names(cases)) {
    infile <- cases[[branch]]
    audio <- withr::local_tempfile(fileext = ".mp3")
    video <- sep_fresh_video()
    cnd <- tryCatch(separate_audio_video(infile, audio, video),
                    error = function(e) e)
    msg <- cli::ansi_strip(conditionMessage(cnd))
    expect_match(msg, "The video output was written to", fixed = TRUE,
                 info = branch)
    expect_match(msg, basename(video), fixed = TRUE, info = branch)
    # The audio half's own output: this failure is a stream copy, so FFmpeg
    # opened the file before giving up and Layer 1 removed what it wrote (D046).
    expect_false(file.exists(audio), info = branch)
  }
})

test_that("a brace-bearing video path is not interpolated into the abort", {
  # M44's lesson on the new line: the path goes through a cli field and is
  # formatted once, so `{n}` reaches the reader as text rather than naming a
  # local of the message builder (or aborting the abort).
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  dir <- withr::local_tempdir()
  audio <- withr::local_tempfile(fileext = ".mp3")
  video <- file.path(dir, "v{n}.mp4")
  cnd <- tryCatch(separate_audio_video(infile, audio, video),
                  error = function(e) e)
  expect_s3_class(cnd, "tidymedia_multitrack_separation")
  expect_match(cli::ansi_strip(conditionMessage(cnd)), "v{n}.mp4", fixed = TRUE)
  expect_true(file.exists(video))
})

test_that("when both commands fail the audio failure is what aborts", {
  # AC3 and AC4's silent half. An unknown video encoder is a token the builder
  # accepts and FFmpeg rejects, so the video command fails at run time -- after
  # the audio command has already failed.
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  audio <- withr::local_tempfile(fileext = ".mp3")
  video <- sep_fresh_video()
  cnd <- tryCatch(
    separate_audio_video(infile, audio, video, video_codec = "nosuchcodec"),
    error = function(e) e
  )
  # AC3: the AUDIO command's condition, not the video one's -- the video run
  # fails on an unknown encoder and never reaches a multi-track diagnostic.
  expect_identical(
    class(cnd),
    c("tidymedia_multitrack_separation", "tidymedia_ffmpeg_exit",
      "rlang_error", "error", "condition")
  )
  expect_identical(cnd$tm_status, sep_status_in_message(cnd))
  expect_false(file.exists(video))         # AC3: nothing left behind
  expect_no_match(cli::ansi_strip(conditionMessage(cnd)),
                  "The video output was written to", fixed = TRUE)
})


# What a failed command leaves at ITS OWN output path is a per-run fact, not a
# per-path one: D046 removes what a run WROTE, and a file the run never wrote to
# is left as it was. The docs say so as of M088's first defect return, and these
# two tests are what hold that wording honest -- every other test in this block
# uses a fresh path, where the distinction cannot show.

test_that("a pre-existing audiofile the failed command never wrote is kept", {
  # An unknown audio encoder fails before FFmpeg opens the output, so the file
  # already sitting at `audiofile` survives byte-for-byte -- and the error says
  # so rather than claiming the path is empty.
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  audio <- withr::local_tempfile(fileext = ".mp3")
  video <- sep_fresh_video()
  writeLines("not audio", audio)
  before <- unname(tools::md5sum(audio))
  cnd <- tryCatch(
    separate_audio_video(infile, audio, video, audio_codec = "nosuchcodec"),
    error = function(e) e
  )
  expect_s3_class(cnd, "tidymedia_ffmpeg_exit")
  expect_true(file.exists(audio))
  expect_identical(unname(tools::md5sum(audio)), before)
  expect_match(cli::ansi_strip(conditionMessage(cnd)),
               "was left as it was", fixed = TRUE)
  expect_true(file.exists(video))          # the video half still ran
})

test_that("a pre-existing videofile survives the both-fail path", {
  # The both-fail path with an unknown VIDEO encoder: that run never writes, so
  # a file already at `videofile` is still there afterwards. AC3's own promise
  # is about what the run created or changed, and is unaffected.
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  audio <- withr::local_tempfile(fileext = ".mp3")
  video <- withr::local_tempfile(fileext = ".mp4")
  writeLines("not video", video)
  before <- unname(tools::md5sum(video))
  cnd <- tryCatch(
    separate_audio_video(infile, audio, video, video_codec = "nosuchcodec"),
    error = function(e) e
  )
  expect_s3_class(cnd, "tidymedia_multitrack_separation")
  expect_true(file.exists(video))
  expect_identical(unname(tools::md5sum(video)), before)
  # No bullet: the video command failed, whatever was already at that path.
  expect_no_match(cli::ansi_strip(conditionMessage(cnd)),
                  "The video output was written to", fixed = TRUE)
})


test_that("a both-fail run that destroyed a pre-existing videofile claims no line", {
  # The case the pre/post comparison alone cannot decide, and the one the
  # sibling above does NOT reach: an unknown encoder is refused before the
  # output is opened, so nothing at `videofile` moves. Here the video command
  # opens the file, writes to it, and only then fails, so Layer 1 removes what
  # that run wrote (D046) -- and the caller's own file goes with it. The two
  # snapshots therefore differ while nothing survives to be named, which is the
  # one shape a changed-snapshot gate reads as "written".
  skip_if_no_ffprobe()
  infile <- make_test_video()
  audio <- withr::local_tempfile(fileext = ".mp3")
  # .wav cannot carry h264, and the video command is a stream copy, so the
  # muxer accepts the file and rejects the stream.
  video <- withr::local_tempfile(fileext = ".wav")
  writeLines("a file the caller already had", video)
  cnd <- tryCatch(separate_audio_video(infile, audio, video, audio_stream = 0),
                  error = function(e) e)
  # The case under test is the both-fail one, and the pre-existing file is
  # gone -- without these the silence below could be any other outcome.
  expect_s3_class(cnd, "tidymedia_ffmpeg_exit")
  expect_s3_class(cnd$tm_video_error, "tidymedia_ffmpeg_exit")
  expect_false(file.exists(video))
  expect_no_match(cli::ansi_strip(conditionMessage(cnd)),
                  "The video output was written to", fixed = TRUE)
})


# M090: the both-fail path stops throwing away what it knows -----------------
#
# D068: the video run's condition is attached to the raised audio condition at
# `tm_video_error` rather than discarded. Nothing a human reads changes, so
# these tests assert the field's identity on one side and the message's silence
# about the video failure on the other.
#
# The two conditions are told apart by what each one's message names: the video
# run fails on `video_codec = "nosuchcodec"`, so ffm_run()'s "The failing command
# was:" bullet carries that token and `videofile`, while the audio failure -- an
# AAC-into-MP3 stream copy -- names neither on either of its branches.

test_that("the both-fail path carries the video run's own condition", {
  # AC1, both audio branches. The class vector and `tm_status` the audio run
  # raised are unchanged by the attachment, and the rendered message still names
  # no video failure -- D068 supersedes what D065 DID with the object, not what
  # D065 said the reader should see.
  skip_if_no_ffprobe()
  cases <- list("enriched multi-track" = make_multitrack_video(),
                "n <= 1 fall-open" = make_test_video())
  expected <- list(
    "enriched multi-track" = c("tidymedia_multitrack_separation",
                               "tidymedia_ffmpeg_exit", "rlang_error", "error",
                               "condition"),
    "n <= 1 fall-open" = c("tidymedia_ffmpeg_exit", "rlang_error", "error",
                           "condition")
  )
  for (branch in names(cases)) {
    audio <- withr::local_tempfile(fileext = ".mp3")
    video <- sep_fresh_video()
    cnd <- tryCatch(
      separate_audio_video(cases[[branch]], audio, video,
                           video_codec = "nosuchcodec"),
      error = function(e) e
    )
    expect_identical(class(cnd), expected[[branch]], info = branch)
    expect_identical(cnd$tm_status, sep_status_in_message(cnd), info = branch)

    # The field holds the VIDEO run's condition: its message names the encoder
    # only that command was given, and the output only that command wrote to.
    vcnd <- cnd$tm_video_error
    expect_s3_class(vcnd, "tidymedia_ffmpeg_exit")
    vmsg <- cli::ansi_strip(conditionMessage(vcnd))
    expect_match(vmsg, "nosuchcodec", fixed = TRUE, info = branch)
    expect_match(vmsg, basename(video), fixed = TRUE, info = branch)

    # ... and the message the caller reads names no video failure at all.
    msg <- cli::ansi_strip(conditionMessage(cnd))
    expect_no_match(msg, "nosuchcodec", fixed = TRUE, info = branch)
    expect_no_match(msg, basename(video), fixed = TRUE, info = branch)
  }
})

test_that("a succeeded video command leaves tm_video_error NULL", {
  # AC1's other half: the field's ABSENCE is what says the video half survived,
  # the way the video-written bullet does in the rendered message. Both branches
  # again, since either can reach the caller with the video command intact.
  skip_if_no_ffprobe()
  cases <- list("enriched multi-track" = make_multitrack_video(),
                "n <= 1 fall-open" = make_test_video())
  for (branch in names(cases)) {
    audio <- withr::local_tempfile(fileext = ".mp3")
    video <- sep_fresh_video()
    cnd <- tryCatch(separate_audio_video(cases[[branch]], audio, video),
                    error = function(e) e)
    # The control: this is the same audio failure, and the video half ran and
    # wrote -- so a NULL field here is the video command's success, not a
    # missing attachment.
    expect_true(file.exists(video), info = branch)
    expect_match(cli::ansi_strip(conditionMessage(cnd)),
                 "The video output was written to", fixed = TRUE, info = branch)
    expect_null(cnd$tm_video_error, info = branch)
  }
})


# The `wrote` gate ----------------------------------------------------------
#
# AC2: the video-written line answers "did this run write videofile?", and a
# zero exit does not answer it. FFmpeg refuses an unknown encoder, filter or
# option value BEFORE opening the output, and the mirror case is a command that
# returns 0 having left a pre-existing file untouched -- D046's own distinction,
# read here at Layer 2 rather than in the removal path.
#
# These three cases cannot be staged with a real FFmpeg: the point is a video
# command that SUCCEEDS without writing, which no real invocation does. So
# `ffm_run()` is mocked and dispatches on `object$output` -- the audio pipeline
# gets the failure, the video pipeline gets the behavior each case is named
# after. `audio_stream` is named so run_separation_audio() returns ffm_run()'s
# condition directly instead of probing the input's track count.

# `.env` is the SCOPE the mock is undone at, not the namespace it is installed
# in -- testthat installs it in the package under test on its own. Passing
# `asNamespace("tidymedia")` here scopes the undo to an environment that never
# exits, so the mocked `ffm_run()` outlives the test and every later file runs
# against it: the timeout-silence sweep then reads a namespace whose `ffm_run()`
# reaches no spawn, and 60 tests across six files fail (seen 2026-08-30). The
# scope wanted is the calling test_that() block, which is `parent.frame()` from
# inside this helper.
sep_mock_runs <- function(audiofile, videofile, on_video,
                          scope = parent.frame()) {
  testthat::local_mocked_bindings(
    ffm_run = function(object, verify = NULL) {
      if (identical(object$output, audiofile)) {
        rlang::abort("Mocked audio failure.", class = "tidymedia_ffmpeg_exit")
      }
      # Anything else must be the video pipeline; say so rather than silently
      # treating an unexpected output as the video half.
      expect_identical(object$output, videofile)
      on_video()
      invisible(NULL)
    },
    .env = scope
  )
}

# One input file the three cases share: check_file_readable() reads it before
# either pipeline is built, and the mock never opens it.
sep_mock_infile <- function(env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".mp4", .local_envir = env)
  writeLines("not really a video", path)
  path
}

sep_mock_abort <- function(infile, audiofile, videofile) {
  cnd <- tryCatch(
    separate_audio_video(infile, audiofile, videofile, audio_stream = 0),
    error = function(e) e
  )
  # The failure under test is the mocked AUDIO one, not a check firing earlier:
  # without this the three cases below could all pass on the wrong condition.
  expect_s3_class(cnd, "tidymedia_ffmpeg_exit")
  expect_match(conditionMessage(cnd), "Mocked audio failure.", fixed = TRUE)
  cli::ansi_strip(conditionMessage(cnd))
}

test_that("a video command that succeeds without writing gets no line", {
  # Case one: a fresh path the video run never creates. A zero exit alone would
  # claim the file was written and name a path with nothing at it.
  infile <- sep_mock_infile()
  audio <- withr::local_tempfile(fileext = ".mp3")
  video <- sep_fresh_video()
  sep_mock_runs(audio, video, on_video = function() NULL)
  msg <- sep_mock_abort(infile, audio, video)
  expect_false(file.exists(video))
  expect_no_match(msg, "The video output was written to", fixed = TRUE)
})

test_that("a pre-existing videofile the run never touched gets no line", {
  # Case two: the same zero exit over a path that already holds a file. This is
  # the case existence cannot decide -- file.exists() is TRUE both before and
  # after -- and the one the pre/post comparison is for.
  infile <- sep_mock_infile()
  audio <- withr::local_tempfile(fileext = ".mp3")
  video <- withr::local_tempfile(fileext = ".mp4")
  writeLines("a file the caller already had", video)
  before <- unname(tools::md5sum(video))
  sep_mock_runs(audio, video, on_video = function() NULL)
  msg <- sep_mock_abort(infile, audio, video)
  expect_identical(unname(tools::md5sum(video)), before)
  expect_no_match(msg, "The video output was written to", fixed = TRUE)
})

test_that("a video run that rewrites an existing videofile gets the line", {
  # Case three, the passing control: the same starting state as case two and
  # the same zero exit, differing only in that this run WROTE. The line has to
  # be here, or the gate is refusing every pre-existing path rather than
  # distinguishing rewritten from untouched.
  infile <- sep_mock_infile()
  audio <- withr::local_tempfile(fileext = ".mp3")
  video <- withr::local_tempfile(fileext = ".mp4")
  writeLines("a file the caller already had", video)
  before <- unname(tools::md5sum(video))
  sep_mock_runs(audio, video, on_video = function() {
    # A different length as well as different content: where a filesystem's
    # timestamp resolution hides the mtime move, the size still shows it.
    writeLines("the video this run wrote, longer than what it replaced", video)
  })
  msg <- sep_mock_abort(infile, audio, video)
  expect_false(identical(unname(tools::md5sum(video)), before))
  expect_match(msg, "The video output was written to", fixed = TRUE)
  expect_match(msg, basename(video), fixed = TRUE)
})


test_that("a timed-out audio half still lets the video command run", {
  # D066's behavior, and the claim the changelog makes about it: a reached
  # wall-clock limit is held like any other audio failure, so the video command
  # is spawned afterwards -- on its own fresh budget, which is what makes such
  # a call able to wait up to two limits. The second spawn HAPPENING is the
  # half a test can pin without spending two real limits of wall clock; the
  # budget being fresh is `with_timeout()`'s documented per-spawned-program
  # scope, tested where that scope lives.
  infile <- sep_mock_infile()
  audio <- withr::local_tempfile(fileext = ".mp3")
  video <- withr::local_tempfile(fileext = ".mp4")
  writeLines("a file the caller already had", video)
  video_ran <- FALSE
  testthat::local_mocked_bindings(
    ffm_run = function(object, verify = NULL) {
      if (identical(object$output, audio)) {
        abort_timeout("FFmpeg", 2)
      }
      expect_identical(object$output, video)
      video_ran <<- TRUE
      invisible(NULL)
    }
  )
  cnd <- tryCatch(
    separate_audio_video(infile, audio, video, audio_stream = 0),
    error = function(e) e
  )
  # The failure raised is the TIMEOUT, unchanged in class and limit by the video
  # half having had its turn.
  expect_s3_class(cnd, "tidymedia_timeout")
  expect_identical(cnd$tm_limit, 2)
  expect_true(video_ran)
})

test_that("the mocked ffm_run() did not outlive the three cases above", {
  # The sentinel for the scope bug the helper's comment records. A leaked mock
  # is silent HERE and loud somewhere else -- it surfaced as 60 failures across
  # six unrelated files, because the timeout-silence sweep reads the live
  # namespace and a mocked `ffm_run()` reaches no spawn. Asserted on the body,
  # which is what that sweep reads, rather than on a call.
  expect_true("try_fetch" %in% all.names(body(ffm_run)))
})

test_that("a bare condition reaches stop() unchanged and without the note", {
  # AC3. `abort_after_video()` is internal and this branch is unreachable from
  # the verb -- every cause that raises a condition without `body` (a missing
  # FFmpeg binary, say) stops the video command too, so `wrote` is FALSE there.
  # Called directly for that reason: the guard is a floor for a shape this
  # function has not met, and a floor no test stands on is a floor no one knows
  # the shape of.
  bare <- simpleError("bare and shapeless")
  video_cnd <- rlang::catch_cnd(rlang::abort("the video half also failed",
                                             class = "tidymedia_ffmpeg_exit"))
  out <- tryCatch(
    abort_after_video(bare, "/nowhere/v.mp4", wrote = TRUE,
                      video_error = video_cnd),
    error = function(e) e
  )
  # Unchanged: same class vector, same rendered message, no bullet anywhere.
  expect_identical(class(out), c("simpleError", "error", "condition"))
  expect_identical(conditionMessage(out), "bare and shapeless")
  expect_null(out$body)
  expect_no_match(conditionMessage(out), "The video output was written to",
                  fixed = TRUE)
  # The video failure still travels, though: D068 attaches the field on any
  # condition shape, and only the note needs `body`.
  expect_identical(out$tm_video_error, video_cnd)
})

test_that("an rlang condition through the same call does get the note", {
  # The discriminating control for the test above: identical arguments but for
  # the condition's shape, so a green pair means the guard is reading the shape
  # rather than the note having been dropped for everyone.
  rich <- rlang::catch_cnd(rlang::abort("the audio half failed",
                                        class = "tidymedia_ffmpeg_exit"))
  out <- tryCatch(
    abort_after_video(rich, "/nowhere/v.mp4", wrote = TRUE, video_error = NULL),
    error = function(e) e
  )
  expect_match(cli::ansi_strip(conditionMessage(out)),
               "The video output was written to", fixed = TRUE)
  expect_null(out$tm_video_error)
})
