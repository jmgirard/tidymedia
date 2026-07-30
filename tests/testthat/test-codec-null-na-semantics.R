# What `NULL` and a column `NA` mean across the codec family (M42, D022).
#
# The rule: `video_codec` / `audio_codec` = NULL emits no `-codec:v` /
# `-codec:a` at all, deferring to the output container's default encoder, and a
# jobs-table column `NA` is the column form of that same NULL. The one departure
# is recorded here rather than skipped -- convert_audio's NULL and column NA
# select `-q:a 0` (D021, reaffirmed by D022).
#
# Every "the flag is absent" assertion is paired with a NON-VACUITY assertion
# that the same call with a real encoder DOES show the flag. Without it, "no
# -codec:v in this command" passes on a verb that emits one under no setting at
# all, and the table measures nothing -- the failure mode
# data-raw/codec-guard-baseline.R's codec_guard_vacuous() exists to catch on the
# probe side.

# Set `arg` to `value` in a call template. Single-bracket assignment of
# list(value) so a NULL is STORED rather than deleting the element, which would
# silently turn every NULL case back into the default case.
codec_with <- function(args, arg, value) {
  args[arg] <- list(value)
  args
}

# Compile a verb to one string. A fan-out verb emits several commands and a
# `_batch` verb returns a tibble carrying them in a column; both are collapsed,
# because every assertion below asks whether a flag appears anywhere in what the
# call compiles.
codec_compiled <- function(verb, args) {
  f <- get(verb, envir = asNamespace("tidymedia"))
  args$run <- FALSE
  if ("parallel" %in% names(formals(f))) args$parallel <- FALSE
  out <- do.call(verb, args, envir = asNamespace("tidymedia"))
  if (is.data.frame(out)) out <- out$command
  paste(as.character(out), collapse = " ||| ")
}

test_that("the four standardize/anonymize verbs agree on video_codec = NULL (D022)", {
  input <- make_input()
  regions <- data.frame(x = 0, y = 0, width = 32, height = 32)

  # Before M42 these four disagreed three ways: standardize_video and its batch
  # sibling compiled NULL, anonymize_video aborted at anonymize_pipeline()'s
  # unconditional check_token(), and anonymize_video_batch aborted INSIDE
  # purrr::pmap() carrying `In index: 1` (measured at M42 T1).
  calls <- list(
    standardize_video       = list(infile = input, outfile = "o.mp4"),
    standardize_video_batch = list(jobs = tibble::tibble(input = input,
                                                         output = "o.mp4")),
    anonymize_video         = list(infile = input, outfile = "o.mp4",
                                   regions = regions),
    anonymize_video_batch   = list(jobs = tibble::tibble(
                                     input = input, output = "o.mp4",
                                     regions = list(regions)))
  )

  for (verb in names(calls)) {
    args <- calls[[verb]]

    # Non-vacuity: this verb emits -codec:v at all, so the absence asserted
    # below is the sentinel's doing and not the verb's silence.
    expect_match(
      codec_compiled(verb, codec_with(args, "video_codec", "libx265")),
      "-codec:v libx265", fixed = TRUE,
      label = paste(verb, "emits -codec:v for a named encoder")
    )

    # The rule: NULL drops the flag, on all four.
    expect_no_match(
      codec_compiled(verb, codec_with(args, "video_codec", NULL)),
      "-codec:v", fixed = TRUE,
      label = paste(verb, "drops -codec:v under NULL")
    )
  }
})

test_that("the NULL sentinel changes nothing else about the compiled command", {
  # The two verbs ship a documented standard profile, so `NULL` must remove the
  # encoder and nothing else: dropping the codec is not licence to drop the
  # pixel format, the faststart flag, or the audio stream copy alongside it.
  input <- make_input()
  regions <- data.frame(x = 0, y = 0, width = 32, height = 32)

  std <- codec_compiled("standardize_video",
                        list(infile = input, outfile = "o.mp4",
                             video_codec = NULL))
  expect_match(std, "-pix_fmt yuv420p", fixed = TRUE)
  expect_match(std, "-movflags +faststart", fixed = TRUE)
  expect_match(std, "-codec:a copy", fixed = TRUE)

  anon <- codec_compiled("anonymize_video",
                         list(infile = input, outfile = "o.mp4",
                              regions = regions, video_codec = NULL))
  expect_match(anon, "-pix_fmt yuv420p", fixed = TRUE)
  expect_match(anon, "-codec:a copy", fixed = TRUE)
  expect_match(anon, "drawbox", fixed = TRUE)
})

test_that("a NULL video_codec still resolves to the nvenc H.264 family (D016)", {
  # D016 puts the sentinel INSIDE resolve_hw_encoder(), before family inference:
  # under hardware = "nvenc" a NULL assumes H.264 rather than emitting nothing.
  # anonymize_video reached that branch only for non-NULL values before M42,
  # because check_token() refused NULL first, so this pins the branch it now
  # reaches. Availability is simulated through the option seam has_nvenc()
  # consults, so this stays a binary-free compile test (test-video-codec.R).
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  input <- make_input()
  regions <- data.frame(x = 0, y = 0, width = 32, height = 32)
  expect_match(
    codec_compiled("anonymize_video",
                   list(infile = input, outfile = "o.mp4", regions = regions,
                        video_codec = NULL, hardware = "nvenc")),
    "-codec:v h264_nvenc", fixed = TRUE
  )
})
