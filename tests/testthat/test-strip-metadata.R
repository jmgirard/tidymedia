# Tests for strip_metadata(): the de-identification metadata scrub. The
# command-construction path (run = FALSE) is tested purely; the guard that
# establishes which tags actually clear and that stream-copy preserves A/V is
# gated on the ffmpeg/ffprobe binaries.

test_that("strip_metadata() compiles to a bit-exact stream copy that drops metadata", {
  f <- make_input()
  cmd <- strip_metadata(f, "clean.mp4", run = FALSE)
  # Lossless carry-through of every stream.
  expect_match(cmd, "-codec:v copy -codec:a copy", fixed = TRUE)
  expect_match(cmd, "-map 0", fixed = TRUE)
  # The scrub itself: global tags, chapters, and FFmpeg's own re-added tags.
  expect_match(cmd, "-map_metadata -1", fixed = TRUE)
  expect_match(cmd, "-map_chapters -1", fixed = TRUE)
  expect_match(cmd, "-fflags +bitexact", fixed = TRUE)
  expect_match(cmd, '"clean.mp4"', fixed = TRUE)
  # Binary-free (IP1/D002): this whole test has no skip_if_no_ffmpeg and never
  # shells out, so it passes on a machine with no ffmpeg by construction.
})

test_that("strip_metadata() clears identifying tags, chapters, and FFmpeg's own tags", {
  infile <- make_tagged_video()
  # Sanity: the fixture really carries the tags we expect to remove, both at the
  # container level and on a stream.
  before <- probe_format_tags(infile)
  expect_true(any(grepl("^title=", before)))
  expect_true(any(grepl("^creation_time=", before)))
  before_streams <- probe_stream_tags(infile)
  expect_true(any(grepl("CAM-OPERATOR-JANE", before_streams)))

  outfile <- withr::local_tempfile(fileext = ".mp4")
  strip_metadata(infile, outfile)

  after <- probe_format_tags(outfile)
  # The identifying global tags are gone...
  identifying <- c("title", "comment", "location", "location-eng",
                   "creation_time")
  for (key in identifying) {
    expect_false(any(grepl(paste0("^", key, "="), after)),
                 label = sprintf("format tag %s cleared", key))
  }
  # ...and -fflags +bitexact stops FFmpeg re-stamping its own creation_time /
  # encoder tag (the de-id + reproducibility guard).
  expect_false(any(grepl("^encoder=", after)))
  expect_false(any(grepl("^creation_time=", after)))

  # AC2 also covers *stream*-level tags: the per-stream identifying tag and the
  # per-stream encoder tag are gone; only inert housekeeping (handler_name,
  # language) may remain. Guards against a stream-tag leak passing green.
  after_streams <- probe_stream_tags(outfile)
  for (key in c("name", "title", "creation_time", "encoder", "comment")) {
    expect_false(any(grepl(paste0("^", key, "="), after_streams)),
                 label = sprintf("stream tag %s cleared", key))
  }
  expect_false(any(grepl("CAM-OPERATOR-JANE", after_streams)))

  # No chapters survive the scrub.
  chapters <- ffprobe(sprintf(
    '-v error -show_entries chapter=id -of csv=p=0 "%s"', outfile
  ))
  expect_length(chapters[nzchar(chapters)], 0)
})

test_that("strip_metadata() preserves the A/V streams and rotation (stream copy)", {
  infile <- make_tagged_video()
  outfile <- withr::local_tempfile(fileext = ".mp4")
  strip_metadata(infile, outfile)

  # Same streams, same codecs, in the same order.
  codecs <- function(f) ffprobe(sprintf(
    '-v error -show_entries stream=codec_name -of csv=p=0 "%s"', f
  ))
  expect_equal(codecs(outfile), codecs(infile))
  # Duration is unchanged (lossless copy), within a small remux tolerance.
  expect_equal(probe_duration(outfile), probe_duration(infile), tolerance = 0.05)
  # The rotation display matrix (side data, not a metadata tag) survives.
  expect_equal(probe_rotation(outfile), 90)
})
