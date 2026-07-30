# M47: the pass-through verbs state their audio selection instead of inheriting
# FFmpeg's implicit stream choice.
#
# standardize_video() and anonymize_video() emitted no -map at all before this
# milestone, so FFmpeg's implicit selection applied: one stream of each type,
# preferring the audio track carrying the container's DEFAULT disposition.
# Measured on a 3-audio-track .mkv with DEFAULT moved to track 1 (ffmpeg 8.1.2,
# macOS): one audio stream out, and it was `spa`, the SECOND track. Which track
# survived was therefore a property of the input's flags and of the FFmpeg
# build, which is the invisible variation D023 exists to remove.
#
# Both verbs now compile `-map 0:v? -map 0:a?` when no track is named and
# `-map 0:v? -map 0:a:<n>` when one is. A uniform `-map 0` was rejected at plan
# time because it fails outright into .mp4 on a subtitle-bearing input (exit 8,
# no default mp4 subtitle encoder).
#
# The `?` on the unselected specifiers is what keeps a stream-less input
# working: a bare `-map 0:a` aborts FFmpeg on a silent video and a bare
# `-map 0:v` aborts on an audio-only file, both exit 234, where master emitted
# no map and exited 0. The named specifier keeps no `?` so that naming a track
# the input lacks stays an error (D023). See the "tolerates" tests at the foot
# of this file.

regions_1 <- function() {
  data.frame(x = 0, y = 0, width = 10, height = 10)
}

# The pre-M47 commands, recorded from master at f3c3054 and committed here as
# literals. They are written as templates taking the maps because the claim
# under test is "these two -map arguments appeared and nothing else moved", and
# a comparison against "what master returns today" stops being checkable the
# moment this branch merges and master becomes this code.
standardize_command <- function(infile, maps = "") {
  paste0(
    '-y -i "', infile, '"',
    ' -vf "crop=w=floor(in_w/2)*2:h=floor(in_h/2)*2:x=(in_w-out_w)/2:y=(in_h-out_h)/2"',
    " -codec:v libx264 -codec:a copy -pix_fmt yuv420p -movflags +faststart ",
    maps, '"out.mp4"'
  )
}

anonymize_command <- function(infile, maps = "") {
  paste0(
    '-y -i "', infile, '"',
    ' -vf "crop=w=floor(in_w/2)*2:h=floor(in_h/2)*2:x=(in_w-out_w)/2:y=(in_h-out_h)/2',
    ',drawbox=x=0:y=0:w=10:h=10:c=black:t=fill"',
    " -codec:v libx264 -codec:a copy -pix_fmt yuv420p ",
    maps, '"out.mp4"'
  )
}

# Count -map arguments. A containment assertion cannot see a duplicate, and
# ffm_map() appends since M43, so the count is the discriminator (M43/M45).
map_count <- function(cmd) {
  vapply(cmd, function(x) {
    sum(gregexpr("-map ", x, fixed = TRUE)[[1]] > 0)
  }, integer(1), USE.NAMES = FALSE)
}

# AC1 -------------------------------------------------------------------------

test_that("an unset audio_stream compiles every video and every audio stream", {
  f <- make_input()
  expect_identical(
    standardize_video(f, "out.mp4", run = FALSE),
    standardize_command(f, "-map 0:v? -map 0:a? ")
  )
  expect_identical(
    anonymize_video(f, "out.mp4", regions = regions_1(), run = FALSE),
    anonymize_command(f, "-map 0:v? -map 0:a? ")
  )
})

test_that("an explicit NULL audio_stream compiles what leaving it unset does", {
  f <- make_input()
  expect_identical(
    standardize_video(f, "out.mp4", audio_stream = NULL, run = FALSE),
    standardize_video(f, "out.mp4", run = FALSE)
  )
  expect_identical(
    anonymize_video(f, "out.mp4", regions = regions_1(), audio_stream = NULL,
                    run = FALSE),
    anonymize_video(f, "out.mp4", regions = regions_1(), run = FALSE)
  )
})

# AC2 -------------------------------------------------------------------------

test_that("a named audio_stream compiles that track and no other", {
  f <- make_input()
  expect_identical(
    standardize_video(f, "out.mp4", audio_stream = 2, run = FALSE),
    standardize_command(f, "-map 0:v? -map 0:a:2 ")
  )
  expect_identical(
    anonymize_video(f, "out.mp4", regions = regions_1(), audio_stream = 2,
                    run = FALSE),
    anonymize_command(f, "-map 0:v? -map 0:a:2 ")
  )
})

test_that("both verbs compile exactly two maps either way", {
  f <- make_input()
  cmds <- c(
    standardize_video(f, "out.mp4", run = FALSE),
    standardize_video(f, "out.mp4", audio_stream = 2, run = FALSE),
    anonymize_video(f, "out.mp4", regions = regions_1(), run = FALSE),
    anonymize_video(f, "out.mp4", regions = regions_1(), audio_stream = 2,
                    run = FALSE)
  )
  expect_identical(map_count(cmds), rep(2L, 4))
  # A named call must not ALSO carry the every-track map -- ffm_map() appends,
  # so `-map 0:a? -map 0:a:2` would satisfy a containment check for either one
  # on its own (M45's absence-assertion trick).
  expect_false(grepl("-map 0:a? ", cmds[[2]], fixed = TRUE))
  expect_false(grepl("-map 0:a? ", cmds[[4]], fixed = TRUE))
})

test_that("audio_stream = 0 names the first track rather than emitting 0:a", {
  f <- make_input()
  expect_identical(
    standardize_video(f, "out.mp4", audio_stream = 0, run = FALSE),
    standardize_command(f, "-map 0:v? -map 0:a:0 ")
  )
})

# AC3 -------------------------------------------------------------------------

test_that("a bad audio_stream aborts naming the argument and blaming the verb", {
  f <- make_input()
  bad <- list("1", 1.5, -1, NA, NA_integer_, c(0, 1))
  for (value in bad) {
    err <- expect_error(
      standardize_video(f, "out.mp4", audio_stream = value, run = FALSE)
    )
    expect_match(conditionMessage(err), "audio_stream")
    expect_identical(rlang::call_name(conditionCall(err)), "standardize_video")

    err <- expect_error(
      anonymize_video(f, "out.mp4", regions = regions_1(),
                      audio_stream = value, run = FALSE)
    )
    expect_match(conditionMessage(err), "audio_stream")
    expect_identical(rlang::call_name(conditionCall(err)), "anonymize_video")
  }
})

# AC7 -------------------------------------------------------------------------

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
  standardize_video(f, "out.mp4", run = FALSE)
  standardize_video(f, "out.mp4", audio_stream = 2, run = FALSE)
  anonymize_video(f, "out.mp4", regions = regions_1(), run = FALSE)
  anonymize_video(f, "out.mp4", regions = regions_1(), audio_stream = 2,
                  run = FALSE)
  expect_identical(n, 0L)
  # Deliberately NOT extended to hardware = "nvenc": resolve_hw_encoder()
  # reaches ffmpeg("-encoders") before `run` is consulted, so that path DOES
  # shell out under run = FALSE. Found by this milestone's criteria audit,
  # reproduced under this same mock, and carried as a ROADMAP candidate row --
  # it falsifies D024's "sole exception" sentence and is not M47's to fix.
})

# AC4 / AC5: the batch siblings --------------------------------------------

std_jobs <- function(f, ...) {
  tibble::tibble(input = c(f, f), output = c("a.mp4", "b.mp4"), ...)
}

anon_jobs <- function(f, ...) {
  tibble::tibble(
    input = c(f, f), output = c("a.mp4", "b.mp4"),
    regions = list(regions_1(), regions_1()), ...
  )
}

test_that("the batch argument reaches every row", {
  f <- make_input()
  out <- standardize_video_batch(std_jobs(f), audio_stream = 2, run = FALSE)
  expect_true(all(grepl("-map 0:v? -map 0:a:2", out$command, fixed = TRUE)))
  out <- anonymize_video_batch(anon_jobs(f), audio_stream = 2, run = FALSE)
  expect_true(all(grepl("-map 0:v? -map 0:a:2", out$command, fixed = TRUE)))
})

test_that("an audio_stream column overrides the argument per row", {
  f <- make_input()
  # Row 2's NA is the column form of the NULL sentinel: it keeps that row on
  # every audio track, overriding the argument rather than deferring to it.
  out <- standardize_video_batch(
    std_jobs(f, audio_stream = c(1, NA)), audio_stream = 2, run = FALSE
  )
  expect_match(out$command[[1]], "-map 0:a:1", fixed = TRUE)
  expect_match(out$command[[2]], "-map 0:a?", fixed = TRUE)
  expect_false(grepl("-map 0:a:", out$command[[2]], fixed = TRUE))
})

test_that("a one-row batch compiles what the scalar verb compiles", {
  f <- make_input()
  out <- standardize_video_batch(
    tibble::tibble(input = f, output = "out.mp4"), audio_stream = 2,
    run = FALSE
  )
  expect_identical(
    as.character(out$command[[1]]),
    as.character(standardize_video(f, "out.mp4", audio_stream = 2,
                                   run = FALSE))
  )
  out <- anonymize_video_batch(
    tibble::tibble(input = f, output = "out.mp4",
                   regions = list(regions_1())),
    audio_stream = 2, run = FALSE
  )
  expect_identical(
    as.character(out$command[[1]]),
    as.character(anonymize_video(f, "out.mp4", regions = regions_1(),
                                 audio_stream = 2, run = FALSE))
  )
})

test_that("a wrongly typed audio_stream column aborts before any row runs", {
  f <- make_input()
  for (bad in list(c("1", "2"), c(TRUE, FALSE))) {
    err <- expect_error(
      standardize_video_batch(std_jobs(f, audio_stream = bad), run = FALSE)
    )
    expect_match(conditionMessage(err), "audio_stream")
    # The hint has to be true on THIS family. Borrowing the extraction verbs'
    # wording would tell a caller NA keeps the first track, where here it keeps
    # them all (M40's stale-hint lesson, which is about a caller being ADDED).
    expect_match(conditionMessage(err), "keep every audio track")
    expect_no_match(conditionMessage(err), "first audio track")
    expect_no_match(conditionMessage(err), "drop audio")
  }
  err <- expect_error(
    anonymize_video_batch(anon_jobs(f, audio_stream = c("1", "2")),
                          run = FALSE)
  )
  expect_match(conditionMessage(err), "keep every audio track")
})

test_that("an all-NA audio_stream column is accepted, being logical", {
  f <- make_input()
  # R types c(NA, NA) as logical, so an is.numeric-only guard would wrongly
  # reject the column spelling of "leave every row unselected" (M34).
  out <- standardize_video_batch(
    std_jobs(f, audio_stream = c(NA, NA)), run = FALSE
  )
  expect_true(all(grepl("-map 0:a?", out$command, fixed = TRUE)))
})

test_that("a scalar NA audio_stream aborts rather than compiling every track", {
  f <- make_input()
  # Load-bearing here in a way it is not on the scalar verb: the column path
  # reads NA as the NULL sentinel, so without the front-door guard this would
  # quietly compile 0:a? instead of erroring (M37/M41).
  expect_error(
    standardize_video_batch(std_jobs(f), audio_stream = NA, run = FALSE),
    "audio_stream"
  )
})

# AC6: the stream-less inputs an explicit map can break --------------------

# Stating a selection means naming stream types the input may not have, and
# FFmpeg treats an unmatched `-map` as fatal rather than empty. These two are
# the regression guards for the `?` suffix: with a bare `0:a`/`0:v` both abort
# at exit 234, where master -- emitting no map at all -- exits 0.

test_that("a video-only input still standardizes", {
  skip_if_no_ffmpeg()
  infile <- make_silent_video()
  outfile <- withr::local_tempfile(fileext = ".mp4")
  expect_no_error(standardize_video(infile, outfile))
  expect_true(file.exists(outfile))
  expect_identical(stream_types(outfile), "video")
})

test_that("an audio-only input still standardizes", {
  skip_if_no_ffmpeg()
  infile <- make_silent_audio()
  outfile <- withr::local_tempfile(fileext = ".mp4")
  expect_no_error(standardize_video(infile, outfile))
  expect_true(file.exists(outfile))
  expect_identical(stream_types(outfile), "audio")
})

test_that("a video-only input still anonymizes", {
  skip_if_no_ffmpeg()
  infile <- make_silent_video()
  outfile <- withr::local_tempfile(fileext = ".mp4")
  expect_no_error(anonymize_video(infile, outfile, regions = regions_1()))
  expect_identical(stream_types(outfile), "video")
})

# AC6: which track actually comes out -------------------------------------

test_that("an unset audio_stream now carries every track, not FFmpeg's pick", {
  skip_if_no_ffmpeg()
  skip_if_no_ffprobe()
  # DEFAULT on track 1 so "the heuristic" and "track 0" cannot coincide: on a
  # fixture leaving DEFAULT at 0 this test passes with audio_stream ignored
  # entirely, which is what the criteria audit caught in the drafted version.
  infile <- make_multitrack_video(default_track = 1)
  outfile <- withr::local_tempfile(fileext = ".mkv")
  standardize_video(infile, outfile)
  expect_identical(stream_types(outfile), c("video", "audio", "audio", "audio"))
})

test_that("a named audio_stream takes that track and no other", {
  skip_if_no_ffmpeg()
  skip_if_no_ffprobe()
  infile <- make_multitrack_video(default_track = 1)
  outfile <- withr::local_tempfile(fileext = ".mkv")
  # Track 2 (`fra`) is neither the first track nor the DEFAULT one, so no
  # implicit selection can produce it -- only the map can.
  standardize_video(infile, outfile, audio_stream = 2)
  expect_identical(stream_types(outfile), c("video", "audio"))
  expect_identical(audio_languages(outfile), "fra")
})

test_that("naming a track the input lacks stays an FFmpeg error", {
  skip_if_no_ffmpeg()
  infile <- make_multitrack_video()
  outfile <- withr::local_tempfile(fileext = ".mkv")
  # The `?` that makes an unselected map tolerant is deliberately absent here:
  # every @param audio_stream in the package promises this is an FFmpeg error
  # rather than an R one (D023), and a silent audio-less output would be worse.
  expect_error(standardize_video(infile, outfile, audio_stream = 9))
})
