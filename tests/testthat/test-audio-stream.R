# Tests for `audio_stream` on the two extraction verbs and their _batch
# siblings (M43): which audio track a multi-track input yields, and the
# front-door / per-row guards on the selector.

# Compiled commands (pure, no binary) -----------------------------------------

test_that("both verbs compile an explicit audio-stream map by default", {
  f <- make_input()
  expect_match(extract_audio(f, "out.aac", run = FALSE),
               "-map 0:a:0", fixed = TRUE)
  expect_match(convert_audio(f, "out.mp3", run = FALSE),
               "-map 0:a:0", fixed = TRUE)
})

test_that("audio_stream selects the named track on both verbs", {
  f <- make_input()
  expect_match(extract_audio(f, "out.aac", audio_stream = 1, run = FALSE),
               "-map 0:a:1", fixed = TRUE)
  expect_match(convert_audio(f, "out.mp3", audio_stream = 2, run = FALSE),
               "-map 0:a:2", fixed = TRUE)
})

test_that("audio_stream = NULL and audio_stream = 0 compile the same command", {
  # NULL is the argument's "no selection" sentinel, not a third behavior: it is
  # what lets a batch NA cell say "leave this row on the default".
  f <- make_input()
  expect_identical(
    extract_audio(f, "out.aac", run = FALSE),
    extract_audio(f, "out.aac", audio_stream = 0, run = FALSE)
  )
  expect_identical(
    convert_audio(f, "out.mp3", run = FALSE),
    convert_audio(f, "out.mp3", audio_stream = 0, run = FALSE)
  )
})

test_that("convert_audio() commands are unchanged when audio_stream is absent", {
  # The hotfix pinned `-map 0:a:0`; M43 must compile it from the selector's
  # default rather than regress to `-map a` or drop the map.
  f <- make_input()
  expect_identical(
    convert_audio(f, "out.mp3", run = FALSE),
    paste0('-y -i "', f, '" -q:a 0 -map 0:a:0 "out.mp3"')
  )
  expect_identical(
    convert_audio(f, "out.m4a", audio_codec = "aac", run = FALSE),
    paste0('-y -i "', f, '" -codec:a aac -map 0:a:0 "out.m4a"')
  )
})

test_that("audio_stream compiles exactly one -map on either verb", {
  f <- make_input()
  count <- function(cmd) sum(gregexpr("-map ", cmd, fixed = TRUE)[[1]] > 0)
  expect_identical(count(extract_audio(f, "o.aac", audio_stream = 1,
                                       run = FALSE)), 1L)
  expect_identical(count(convert_audio(f, "o.mp3", audio_stream = 1,
                                       run = FALSE)), 1L)
})

# Front-door guards -----------------------------------------------------------

test_that("both verbs reject a non-whole, negative, or non-numeric audio_stream", {
  f <- make_input()
  for (verb in list(extract_audio, convert_audio)) {
    expect_error(verb(f, "out.mp3", audio_stream = 1.5, run = FALSE),
                 "whole number")
    expect_error(verb(f, "out.mp3", audio_stream = -1, run = FALSE),
                 "audio_stream")
    expect_error(verb(f, "out.mp3", audio_stream = "1", run = FALSE),
                 "audio_stream")
    expect_error(verb(f, "out.mp3", audio_stream = NA, run = FALSE),
                 "audio_stream")
    expect_error(verb(f, "out.mp3", audio_stream = c(0, 1), run = FALSE),
                 "audio_stream")
  }
})

test_that("a bad audio_stream blames the verb the caller wrote", {
  # This pins the CONTRACT -- the error names the verb, not an internal helper
  # (M41) -- and deliberately not the front-door guard, which cannot be pinned:
  # deleting it leaves this and every other test green, because
  # audio_stream_map()'s `call` already resolves to the verb's frame. Naming it
  # after the guard would be a test that claims coverage it does not have (M42).
  f <- make_input()
  expect_error(extract_audio(f, "out.aac", audio_stream = -1, run = FALSE),
               class = "rlang_error")
  err <- tryCatch(extract_audio(f, "out.aac", audio_stream = -1, run = FALSE),
                  error = function(e) e)
  expect_identical(rlang::call_name(conditionCall(err)), "extract_audio")
  err <- tryCatch(convert_audio(f, "out.mp3", audio_stream = -1, run = FALSE),
                  error = function(e) e)
  expect_identical(rlang::call_name(conditionCall(err)), "convert_audio")
})

# Batch: argument, override column, and guards --------------------------------

test_that("the batch verbs carry a batch-wide audio_stream argument", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.aac", "b.aac"))
  res <- extract_audio_batch(jobs, audio_stream = 1, run = FALSE)
  expect_true(all(grepl("-map 0:a:1", res$command, fixed = TRUE)))

  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp3", "b.mp3"))
  res <- convert_audio_batch(jobs, audio_stream = 2, run = FALSE)
  expect_true(all(grepl("-map 0:a:2", res$command, fixed = TRUE)))
})

test_that("an audio_stream column overrides the argument per row", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.aac", "b.aac"),
                         audio_stream = c(2, 1))
  res <- extract_audio_batch(jobs, audio_stream = 0, run = FALSE)
  expect_match(res$command[[1]], "-map 0:a:2", fixed = TRUE)
  expect_match(res$command[[2]], "-map 0:a:1", fixed = TRUE)

  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp3", "b.mp3"),
                         audio_stream = c(2, 1))
  res <- convert_audio_batch(jobs, audio_stream = 0, run = FALSE)
  expect_match(res$command[[1]], "-map 0:a:2", fixed = TRUE)
  expect_match(res$command[[2]], "-map 0:a:1", fixed = TRUE)
})

test_that("an NA cell keeps that row on the track-0 default, overriding the argument", {
  # The family's sentinel meaning: NA is the column form of NULL, which is "no
  # selection" -- NOT "fall back to the argument", which is what an ABSENT
  # column means.
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.aac", "b.aac"),
                         audio_stream = c(NA, 1))
  res <- extract_audio_batch(jobs, audio_stream = 2, run = FALSE)
  expect_match(res$command[[1]], "-map 0:a:0", fixed = TRUE)
  expect_match(res$command[[2]], "-map 0:a:1", fixed = TRUE)

  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp3", "b.mp3"),
                         audio_stream = c(NA, 1))
  res <- convert_audio_batch(jobs, audio_stream = 2, run = FALSE)
  expect_match(res$command[[1]], "-map 0:a:0", fixed = TRUE)
  expect_match(res$command[[2]], "-map 0:a:1", fixed = TRUE)
})

test_that("an all-NA audio_stream column is legal on both batch verbs", {
  # R types an all-NA column logical, so a numeric-only guard would wrongly
  # reject the one spelling that means "leave every row alone" (M34).
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.aac", "b.aac"),
                         audio_stream = c(NA, NA))
  res <- extract_audio_batch(jobs, run = FALSE)
  expect_true(all(grepl("-map 0:a:0", res$command, fixed = TRUE)))

  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp3", "b.mp3"),
                         audio_stream = c(NA, NA))
  res <- convert_audio_batch(jobs, run = FALSE)
  expect_true(all(grepl("-map 0:a:0", res$command, fixed = TRUE)))
})

test_that("a non-numeric audio_stream column is rejected up front", {
  # Both boundaries of the M34 guard: a character column (rejected) and a
  # mixed logical column, which is NOT the all-NA spelling (also rejected).
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.aac", audio_stream = "1")
  expect_error(extract_audio_batch(jobs, run = FALSE), "must be numeric")
  jobs <- tibble::tibble(input = f, output = "a.aac",
                         audio_stream = TRUE)
  expect_error(extract_audio_batch(jobs, run = FALSE), "must be numeric")
  jobs <- tibble::tibble(input = f, output = "a.mp3", audio_stream = "1")
  expect_error(convert_audio_batch(jobs, run = FALSE), "must be numeric")
})

test_that("the audio_stream column guard's hint names what NA does here", {
  # Inherited wording would say "drop audio", which is what NA means on the
  # composite verbs sharing this guard and is false here (M40 lesson).
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.aac", audio_stream = "1")
  err <- tryCatch(extract_audio_batch(jobs, run = FALSE),
                  error = function(e) conditionMessage(e))
  expect_match(err, "first audio track", fixed = TRUE)
  expect_no_match(err, "drop audio", fixed = TRUE)
})

test_that("the batch verbs re-validate each audio_stream cell's value", {
  # The column path skips the scalar argument's range check, so a negative or
  # fractional cell must still be refused (M32).
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.aac", "b.aac"),
                         audio_stream = c(0, -1))
  expect_error(extract_audio_batch(jobs, run = FALSE), "audio_stream")
  jobs <- tibble::tibble(input = c(f, f), output = c("a.mp3", "b.mp3"),
                         audio_stream = c(0, 1.5))
  expect_error(convert_audio_batch(jobs, run = FALSE), "whole number")
})

test_that("the batch verbs reject a bad audio_stream argument at the front door", {
  # A scalar NA would otherwise resolve through the same NA-means-default path
  # a cell takes and quietly compile track 0 (the M37/M41 shape).
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.aac")
  expect_error(extract_audio_batch(jobs, audio_stream = NA, run = FALSE),
               "audio_stream")
  expect_error(extract_audio_batch(jobs, audio_stream = -1, run = FALSE),
               "audio_stream")
  jobs <- tibble::tibble(input = f, output = "a.mp3")
  expect_error(convert_audio_batch(jobs, audio_stream = NA, run = FALSE),
               "audio_stream")
  expect_error(convert_audio_batch(jobs, audio_stream = 1.5, run = FALSE),
               "whole number")
})

test_that("batch and scalar compile byte-identical commands for the same selector", {
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "a.aac", audio_stream = 1)
  expect_identical(
    extract_audio_batch(jobs, run = FALSE)$command[[1]],
    extract_audio(f, "a.aac", audio_stream = 1, run = FALSE)
  )
  jobs <- tibble::tibble(input = f, output = "a.mp3", audio_stream = 1)
  expect_identical(
    convert_audio_batch(jobs, run = FALSE)$command[[1]],
    convert_audio(f, "a.mp3", audio_stream = 1, run = FALSE)
  )
})

# Execution: which track actually comes out (binary-gated) ---------------------

# Read the language tag of the single audio stream in `path`. Matroska and MP4
# both carry it; .aac and .mp3 do not, which is why the assertions below write
# .m4a rather than the extension the verb's examples use.
audio_language <- function(path) {
  skip_if_no_ffprobe()
  out <- ffprobe(sprintf(
    '-v error -select_streams a -show_entries stream_tags=language -of csv=p=0 "%s"',
    path
  ))
  trimws(out[[1]])
}

test_that("extract_audio(audio_stream = 1) writes the second track (spa)", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  out <- withr::local_tempfile(fileext = ".m4a")
  extract_audio(infile, out, audio_stream = 1)
  expect_true(file.exists(out) && file.size(out) > 0)
  expect_identical(audio_language(out), "spa")
})

test_that("extract_audio() defaults to the first track (eng)", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  out <- withr::local_tempfile(fileext = ".m4a")
  extract_audio(infile, out)
  expect_identical(audio_language(out), "eng")
})

test_that("extract_audio() writes exactly one audio stream from a 3-track input", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  out <- withr::local_tempfile(fileext = ".mka")
  extract_audio(infile, out, audio_stream = 2)
  streams <- ffprobe(sprintf(
    '-v error -select_streams a -show_entries stream=index -of csv=p=0 "%s"', out
  ))
  expect_length(streams, 1L)
  expect_identical(audio_language(out), "fra")
})

test_that("extract_audio() ignores the container's DEFAULT disposition", {
  # The behavior change the explicit map buys, measured rather than asserted.
  # With the DEFAULT flag moved to the second track, the old recipe (no -map)
  # extracted `spa` because FFmpeg's default-stream selection prefers the
  # flagged track; the explicit map takes `eng` regardless of the flag. Remux
  # with -map 0 or the remux itself drops the extra tracks (default selection
  # applies there too) and the fixture silently stops discriminating.
  skip_if_no_ffprobe()
  plain <- make_multitrack_video()
  flagged <- withr::local_tempfile(fileext = ".mkv")
  ffmpeg(sprintf(
    '-y -i "%s" -map 0 -c copy -disposition:a:0 0 -disposition:a:1 default "%s"',
    plain, flagged
  ))
  skip_if_not(file.exists(flagged), "disposition fixture could not be generated")
  dispositions <- ffprobe(sprintf(
    '-v error -select_streams a -show_entries stream_disposition=default -of csv=p=0 "%s"',
    flagged
  ))
  # The fixture only discriminates if the flag really moved off track 0.
  skip_if_not(identical(trimws(dispositions), c("0", "1", "0")),
              "DEFAULT disposition did not move to the second track")

  out <- withr::local_tempfile(fileext = ".m4a")
  extract_audio(flagged, out)
  expect_identical(audio_language(out), "eng")

  # And the flagged track is still reachable, by name rather than by flag.
  out2 <- withr::local_tempfile(fileext = ".m4a")
  extract_audio(flagged, out2, audio_stream = 1)
  expect_identical(audio_language(out2), "spa")
})

test_that("extract_audio() takes audio alone from a subtitle-bearing input", {
  # The explicit map's second, documented consequence. With no -map, FFmpeg
  # carried one stream of EACH type, so a subtitle reached any container that
  # accepts one and `-vn` removed only the video; naming the audio stream takes
  # audio alone. The output container has to accept subtitles for this to
  # discriminate at all -- .m4a or .mka would pass either way.
  skip_if_no_ffprobe()
  srt <- withr::local_tempfile(fileext = ".srt")
  writeLines(c("1", "00:00:00,000 --> 00:00:01,000", "hello", ""), srt)
  infile <- withr::local_tempfile(fileext = ".mkv")
  ffmpeg(paste(
    "-y -f lavfi -i testsrc=duration=2:size=64x64:rate=10",
    "-f lavfi -i sine=frequency=440:duration=2",
    sprintf('-i "%s"', srt),
    "-map 0:v -map 1:a -map 2:s -c:v libx264 -c:a aac -c:s srt",
    sprintf('-shortest -pix_fmt yuv420p "%s"', infile)
  ))
  skip_if_not(file.exists(infile), "subtitle fixture could not be generated")
  types <- function(p) {
    trimws(ffprobe(sprintf(
      '-v error -show_entries stream=codec_type -of csv=p=0 "%s"', p
    )))
  }
  # The fixture only discriminates if the source really carries a subtitle.
  skip_if_not("subtitle" %in% types(infile), "fixture carries no subtitle")

  out <- withr::local_tempfile(fileext = ".mkv")
  extract_audio(infile, out)
  expect_identical(types(out), "audio")
})

test_that("convert_audio(audio_stream = 1) converts the second track (spa)", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  out <- withr::local_tempfile(fileext = ".m4a")
  convert_audio(infile, out, audio_codec = "aac", audio_stream = 1)
  expect_true(file.exists(out) && file.size(out) > 0)
  expect_identical(audio_language(out), "spa")
})

test_that("an audio_stream column selects per row when the batch verbs run", {
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  o1 <- withr::local_tempfile(fileext = ".m4a")
  o2 <- withr::local_tempfile(fileext = ".m4a")
  jobs <- tibble::tibble(input = c(infile, infile), output = c(o1, o2),
                         audio_stream = c(2, NA))
  # The argument names track 1; row 1's cell overrides it and row 2's NA falls
  # back to the track-0 default rather than to the argument.
  extract_audio_batch(jobs, audio_stream = 1)
  expect_identical(audio_language(o1), "fra")
  expect_identical(audio_language(o2), "eng")
})
