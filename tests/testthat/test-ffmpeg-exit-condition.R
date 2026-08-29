# M085: a non-zero FFmpeg exit is a condition a caller can catch by class, and
# the exit number rides on the condition as `tm_status` rather than being read
# back out of the formatted message.

test_that("a non-zero exit from ffm_run() is catchable by class alone", {
  skip_if_no_ffmpeg()
  infile <- make_test_video()
  out <- withr::local_tempfile(fileext = ".mp3")
  # Copying AAC into an MP3 container is a guaranteed non-zero exit on every
  # FFmpeg build; leaving the codec unset would simply re-encode and succeed.
  p <- ffm_codec(ffm_map(ffm_files(infile, out), "0:a"), audio = "copy")
  cnd <- tryCatch(ffm_run(p), tidymedia_ffmpeg_exit = function(e) e)

  # The class vector exactly: a parent or sibling would promise handlers this
  # milestone did not ship (M085-D2).
  expect_identical(
    class(cnd),
    c("tidymedia_ffmpeg_exit", "rlang_error", "error", "condition")
  )
  expect_true(is.integer(cnd$tm_status))
  expect_length(cnd$tm_status, 1L)
  expect_false(identical(cnd$tm_status, 0L))

  # Oracle: the same command, spawned the way run_program() spawns it
  # (R/program_management.R), writing to a FRESH output path so the first run's
  # leftovers cannot change the status this returns.
  # run_program() passes resolve_timeout(), which is 0 only while the option is
  # unset; pinning it makes the oracle's `timeout = 0` the same call in any
  # session (M085 review F7).
  withr::local_options(tidymedia.timeout = 0)
  oracle_out <- withr::local_tempfile(fileext = ".mp3")
  q <- ffm_codec(ffm_map(ffm_files(infile, oracle_out), "0:a"), audio = "copy")
  quote_type <- if (.Platform$OS.type == "windows") "cmd" else "sh"
  observed <- suppressWarnings(system2(
    find_ffmpeg(), shQuote(ffm_args(q), type = quote_type),
    stdout = TRUE, stderr = "", input = "", timeout = 0
  ))
  expect_identical(cnd$tm_status, attr(observed, "status"))
})

test_that("the loudnorm analysis pass raises the same class and field", {
  skip_if_no_ffmpeg()
  # A file FFmpeg cannot demux at all: the analysis pass exits non-zero before
  # it can print a measurement block.
  bad <- withr::local_tempfile(fileext = ".mp4")
  writeLines("this is not a media file", bad)
  cnd <- tryCatch(run_loudnorm_analysis(bad),
                  tidymedia_ffmpeg_exit = function(e) e)

  # Exactly as at the ffm_run() site: one flat class, no parent or sibling
  # (M085-D2), enforced at both sites rather than one (M085 review F5).
  expect_identical(
    class(cnd),
    c("tidymedia_ffmpeg_exit", "rlang_error", "error", "condition")
  )
  expect_true(is.integer(cnd$tm_status))
  expect_length(cnd$tm_status, 1L)
  expect_false(identical(cnd$tm_status, 0L))
  # The prose is the one this abort has always carried.
  expect_match(
    cli::ansi_strip(conditionMessage(cnd)),
    "The `loudnorm` analysis pass failed (FFmpeg exited with status",
    fixed = TRUE
  )
})

test_that("ffmpeg_exit_status() reads the class and the field, nothing else", {
  # No message at all: the status comes from the field, so there is nothing to
  # parse and nothing to strip.
  expect_identical(
    ffmpeg_exit_status(rlang::error_cnd("tidymedia_ffmpeg_exit", tm_status = 3L)),
    3L
  )
  # Classed but fieldless.
  expect_identical(
    ffmpeg_exit_status(rlang::error_cnd("tidymedia_ffmpeg_exit")),
    NA_integer_
  )
  # An unresolvable binary: run_program()'s own abort, caught from the call
  # rather than constructed, because it carries no class to construct.
  no_binary <- tryCatch(run_program(NULL, "-version", program = "FFmpeg"),
                        error = function(e) e)
  expect_identical(ffmpeg_exit_status(no_binary), NA_integer_)
  # A timeout, and the multi-track diagnostic: both are tidymedia conditions
  # and neither is a non-zero exit.
  timed_out <- tryCatch(abort_timeout("FFmpeg", 5), error = function(e) e)
  expect_s3_class(timed_out, "tidymedia_timeout")
  expect_identical(ffmpeg_exit_status(timed_out), NA_integer_)
  expect_identical(
    ffmpeg_exit_status(rlang::error_cnd(
      "tidymedia_multitrack_separation",
      message = "Can't write out.mp3: FFmpeg exited with status 3."
    )),
    NA_integer_
  )
  # A foreign condition that DOES carry a `tm_status` field: the only case that
  # falsifies the class guard, since every case above also lacks the field and
  # so would pass on the is.null() guard alone (M085 review F2).
  expect_identical(
    ffmpeg_exit_status(rlang::error_cnd("tidymedia_timeout", tm_status = 3L)),
    NA_integer_
  )
  # An unclassed condition whose message carries the phrase. The old parse
  # answered 3 here; reading the class answers NA, which is the intended change
  # (AC4) and unobservable outside the package.
  expect_identical(
    ffmpeg_exit_status(simpleError("FFmpeg exited with status 3.")),
    NA_integer_
  )
})

# M086 AC1: the enriched multi-track abort is itself catchable by exit class ---

# The three phrases the enrichment -- and nothing else in the package -- puts in
# a message. AC2 asserts their ABSENCE on the near-miss cases, so they are named
# as literal strings rather than by referent: "the track count" is a bare integer
# that ffm_run()'s own message can carry by accident, which is how a negative
# test on it would pass for the wrong reason.
enrichment_phrases <- c("audio tracks", "audio_stream", ".mka")

expect_no_enrichment <- function(cnd) {
  expect_false(inherits(cnd, "tidymedia_multitrack_separation"))
  msg <- cli::ansi_strip(conditionMessage(cnd))
  for (phrase in enrichment_phrases) {
    expect_false(grepl(phrase, msg, fixed = TRUE),
                 label = paste0("message carries ", phrase))
  }
}

# Skip unless THIS FFmpeg's single-stream audio muxer actually refuses several
# audio streams; only ffmpeg >= 8's adts muxer does. Asserting the environment's
# own property beats assuming the local build's behavior is universal (M43).
adts_refuses_multistream <- function(infile) {
  out <- withr::local_tempfile(fileext = ".aac")
  st <- suppressWarnings(tryCatch(
    system2("ffmpeg",
            c("-y", "-loglevel", "error", "-i", infile, "-map", "0:a",
              "-c:a", "copy", out),
            stdout = FALSE, stderr = FALSE),
    error = function(e) 1L
  ))
  !identical(as.integer(st), 0L)
}

test_that("the multi-track separation abort is catchable by exit class", {
  # AC1. Varied over the output container and over the FAILURE CAUSE, because
  # the criterion promises `tm_status` tracks the parent's status rather than a
  # constant: a container refusal and a missing output directory exit with
  # different numbers on the same build.
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  dir <- withr::local_tempdir()
  video <- file.path(dir, "v.mp4")

  cases <- list(
    # AAC copied into MP3 or WAV: refused by every FFmpeg build.
    list(name = ".mp3", out = file.path(dir, "a.mp3")),
    list(name = ".wav", out = file.path(dir, "a.wav")),
    # A directory that does not exist: a different exit number, same enrichment.
    list(name = "missing directory", out = file.path(dir, "nope", "a.mp3"))
  )
  if (adts_refuses_multistream(infile)) {
    cases <- c(cases, list(list(name = ".aac", out = file.path(dir, "a.aac"))))
  }

  statuses <- integer()
  for (case in cases) {
    # (i) an exit-status handler catches it -- the whole point of the milestone.
    cnd <- tryCatch(separate_audio_video(infile, case$out, video),
                    tidymedia_ffmpeg_exit = function(e) e)
    expect_s3_class(cnd, "tidymedia_ffmpeg_exit")
    # (ii) the enriched class is still there, and still first.
    expect_identical(
      class(cnd),
      c("tidymedia_multitrack_separation", "tidymedia_ffmpeg_exit",
        "rlang_error", "error", "condition")
    )
    # (iii) the diagnostic still renders the count and both ways out.
    msg <- cli::ansi_strip(conditionMessage(cnd))
    expect_match(msg, "3 audio tracks", info = case$name)
    expect_match(msg, "audio_stream", info = case$name)
    expect_match(msg, ".mka", fixed = TRUE, info = case$name)
    # (iv) `tm_status` is the parent's status, not a re-parse of the message.
    expect_true(is.integer(cnd$tm_status))
    expect_length(cnd$tm_status, 1L)
    expect_identical(cnd$tm_status, cnd$parent$tm_status)
    expect_false(identical(cnd$tm_status, 0L))
    statuses <- c(statuses, cnd$tm_status)
  }
  # The status varies with the cause: a field pinned to one number would pass
  # every per-case check above and fail here.
  expect_gt(length(unique(statuses)), 1L)
})

# M086 AC2: the four near misses, none of which gets the enrichment ----------

test_that("an unresolvable FFmpeg falls open to run_program()'s own abort", {
  # AC2 (a): no status means "not the failure this diagnostic is about", so the
  # original condition is re-raised untouched (D024's fail-open consequence).
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  dir <- withr::local_tempdir()
  local_mocked_bindings(find_ffmpeg = function() NULL)
  cnd <- tryCatch(
    separate_audio_video(infile, file.path(dir, "a.mp3"), file.path(dir, "v.mp4")),
    error = function(e) e
  )
  expect_no_enrichment(cnd)
  expect_false(inherits(cnd, "tidymedia_ffmpeg_exit"))
  expect_null(cnd$tm_status)
  expect_false(any(grepl("^tidymedia_", class(cnd))))
  # Unchanged in class and message: the same abort run_program() raises when it
  # is handed no location, captured independently of the call above.
  direct <- tryCatch(run_program(NULL, "-version", program = "FFmpeg"),
                     error = function(e) e)
  expect_identical(class(cnd), class(direct))
  expect_identical(cli::ansi_strip(conditionMessage(cnd)),
                   cli::ansi_strip(conditionMessage(direct)))
})

test_that("an unanswerable track count falls open to ffm_run()'s abort", {
  # AC2 (b): FFprobe cannot say how many tracks there are, so there is nothing
  # to add and the exit condition passes through as it is.
  skip_if_no_ffmpeg()
  infile <- make_multitrack_video()
  dir <- withr::local_tempdir()
  local_mocked_bindings(find_ffprobe = function() NULL)
  cnd <- tryCatch(
    separate_audio_video(infile, file.path(dir, "a.mp3"), file.path(dir, "v.mp4")),
    error = function(e) e
  )
  expect_no_enrichment(cnd)
  # It IS a non-zero exit, and re-raising it unchanged means it keeps the class
  # ffm_run() gave it -- the clause the milestone's first AC2 wording denied.
  expect_identical(
    class(cnd),
    c("tidymedia_ffmpeg_exit", "rlang_error", "error", "condition")
  )
  expect_true(is.integer(cnd$tm_status))
  expect_false(identical(cnd$tm_status, 0L))
})

test_that("a single-track input falls open to ffm_run()'s abort", {
  # AC2 (c): one track mapped, so a track count answers nothing.
  skip_if_no_ffprobe()
  infile <- make_test_video()
  dir <- withr::local_tempdir()
  cnd <- tryCatch(
    separate_audio_video(infile, file.path(dir, "a.mp3"), file.path(dir, "v.mp4")),
    error = function(e) e
  )
  expect_no_enrichment(cnd)
  expect_identical(
    class(cnd),
    c("tidymedia_ffmpeg_exit", "rlang_error", "error", "condition")
  )
  expect_true(is.integer(cnd$tm_status))
  expect_false(identical(cnd$tm_status, 0L))
})

test_that("a reached timeout falls open with its own class intact", {
  # AC2 (d): forced at the spawn site so the condition travels a real
  # separate_audio_video() call rather than being constructed and asserted on.
  skip_if_no_ffmpeg()
  infile <- make_multitrack_video()
  dir <- withr::local_tempdir()
  res <- tm_force_timeout(
    "separate_audio_video",
    list(infile = infile,
         audiofile = file.path(dir, "a.mp3"),
         videofile = file.path(dir, "v.mp4"))
  )
  expect_no_enrichment(res$error)
  expect_s3_class(res$error, "tidymedia_timeout")
  expect_false(inherits(res$error, "tidymedia_ffmpeg_exit"))
  expect_null(res$error$tm_status)
})

# M086 AC3/AC5: the docs' claims, run rather than read -----------------------

test_that("the handler ?separate_audio_video shows fires on that path", {
  # AC3. The recipe in the *When the audio output fails* section, executed
  # verbatim against a call that really fails: a documented handler that has
  # never been run is a claim, not a guarantee.
  skip_if_no_ffprobe()
  infile <- make_multitrack_video()
  dir <- withr::local_tempdir()
  status <- tryCatch(
    separate_audio_video(infile, file.path(dir, "audio.mp3"),
                         file.path(dir, "video.mp4")),
    tidymedia_ffmpeg_exit = function(cnd) cnd$tm_status
  )
  expect_true(is.integer(status))
  expect_length(status, 1L)
  expect_false(identical(status, 0L))
})

test_that("the exit-class docs name both classes and both changed paths", {
  # AC3/AC5. Read from whichever Rd shape this run has (source tree under
  # devtools::test(), the installed Rd database under R CMD check), so the
  # guard runs in the check the release gate uses rather than skipping there.
  rd <- rd_sources()
  skip_if(is.null(rd), "no Rd source available")
  pick <- function(topic) {
    hit <- rd[grepl(topic, names(rd), fixed = TRUE)]
    skip_if(length(hit) == 0, paste("no Rd for", topic))
    paste(hit, collapse = "\n")
  }

  sep <- pick("separate_audio_video.Rd")
  expect_match(sep, "tidymedia_ffmpeg_exit", fixed = TRUE)
  expect_match(sep, "tidymedia_multitrack_separation", fixed = TRUE)
  expect_match(sep, "tm_status", fixed = TRUE)

  run <- pick("ffm_run.Rd")
  expect_match(run, "separate_audio_video", fixed = TRUE)
  expect_match(run, "tidymedia_loudnorm_analysis", fixed = TRUE)
  expect_match(run, "tm_row_status", fixed = TRUE)

  pkg <- pick("tidymedia-package.Rd")
  expect_match(pkg, "separate_audio_video", fixed = TRUE)
  expect_match(pkg, "tidymedia_loudnorm_analysis", fixed = TRUE)

  batch <- pick("normalize_audio_batch.Rd")
  expect_match(batch, "tidymedia_loudnorm_analysis", fixed = TRUE)
  expect_match(batch, "tm_rows", fixed = TRUE)

  # NEWS carries the same two names, and no longer says the separation path
  # deliberately withholds the exit class -- which is what it did say.
  news <- if (file.exists("../../NEWS.md")) {
    "../../NEWS.md"
  } else {
    system.file("NEWS.md", package = "tidymedia")
  }
  skip_if(!nzchar(news) || !file.exists(news), "no NEWS.md available")
  txt <- paste(readLines(news, warn = FALSE), collapse = "\n")
  expect_match(txt, "tidymedia_loudnorm_analysis", fixed = TRUE)
  expect_match(txt, "tm_row_status", fixed = TRUE)
  expect_no_match(txt, "Two paths deliberately do not signal it", fixed = TRUE)
})
