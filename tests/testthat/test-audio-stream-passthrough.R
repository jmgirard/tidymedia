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
# Both verbs now compile `-map 0:v -map 0:a` when no track is named and
# `-map 0:v -map 0:a:<n>` when one is. A uniform `-map 0` was rejected at plan
# time because it fails outright into .mp4 on a subtitle-bearing input (exit 8,
# no default mp4 subtitle encoder).

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
    standardize_command(f, "-map 0:v -map 0:a ")
  )
  expect_identical(
    anonymize_video(f, "out.mp4", regions = regions_1(), run = FALSE),
    anonymize_command(f, "-map 0:v -map 0:a ")
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
    standardize_command(f, "-map 0:v -map 0:a:2 ")
  )
  expect_identical(
    anonymize_video(f, "out.mp4", regions = regions_1(), audio_stream = 2,
                    run = FALSE),
    anonymize_command(f, "-map 0:v -map 0:a:2 ")
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
  # 0:a and 0:a:2 differ by a suffix, so a containment check for the first
  # would pass on the second. Pin the absence with the trailing space (M45).
  expect_false(grepl("-map 0:a ", cmds[[2]], fixed = TRUE))
})

test_that("audio_stream = 0 names the first track rather than emitting 0:a", {
  f <- make_input()
  expect_identical(
    standardize_video(f, "out.mp4", audio_stream = 0, run = FALSE),
    standardize_command(f, "-map 0:v -map 0:a:0 ")
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
