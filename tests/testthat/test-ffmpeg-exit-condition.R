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

  # The exit class and its field are the ones the ffm_run() site raises, and
  # they arrive alongside the event class M087 gave this site: the analysis
  # pass yielded no usable measurement, and FFmpeg exited non-zero saying so.
  expect_identical(
    class(cnd),
    c("tidymedia_loudnorm_no_measurement", "tidymedia_ffmpeg_exit",
      "rlang_error", "error", "condition")
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
  # Two tidymedia conditions carrying neither the exit class nor the field: a
  # timeout, and a bare `tidymedia_multitrack_separation` whose MESSAGE names a
  # status. The shipped multi-track diagnostic carries both since M086, so the
  # second probe is a hand-built condition rather than that error -- what it
  # tests is that the class guard, not the message text, decides.
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
  expect_match(run, "tidymedia_loudnorm_no_measurement", fixed = TRUE)
  expect_match(run, "tm_row_status", fixed = TRUE)

  pkg <- pick("tidymedia-package.Rd")
  expect_match(pkg, "separate_audio_video", fixed = TRUE)
  expect_match(pkg, "tidymedia_loudnorm_no_measurement", fixed = TRUE)

  batch <- pick("normalize_audio_batch.Rd")
  expect_match(batch, "tidymedia_loudnorm_no_measurement", fixed = TRUE)
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
  expect_match(txt, "tidymedia_loudnorm_no_measurement", fixed = TRUE)
  expect_match(txt, "tm_row_status", fixed = TRUE)
  expect_no_match(txt, "Two paths deliberately do not signal it", fixed = TRUE)
})

# M087 AC1-AC3: one event, one class name, at every severity and both forms ---

# A file that EXISTS and is readable but is not media. `nonexistent.wav` never
# reaches FFmpeg -- check_file_readable() refuses it first (R/ffmpeg.R:2240) and
# aborts unclassed -- so a probe built on a missing path demonstrates a
# different site than the one under test (RR05 B3).
make_unreadable_media <- function(env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".wav", .local_envir = env)
  writeLines("this is text, not a RIFF header", path)
  path
}

test_that("the scalar analysis abort names the event and the exit alike", {
  # AC1, scalar non-zero-exit site (R/loudnorm_two_pass.R:151). Both facts are
  # true here -- no usable measurement, and a known non-zero exit -- so both
  # classes ride, context first, and `tm_status` comes with the exit class as it
  # does everywhere that class appears.
  skip_if_no_ffmpeg()
  bad <- make_unreadable_media()
  out <- withr::local_tempfile(fileext = ".m4a")
  cnd <- tryCatch(
    normalize_audio(bad, out, two_pass = TRUE, run = FALSE),
    tidymedia_loudnorm_no_measurement = function(e) e
  )
  expect_identical(
    class(cnd),
    c("tidymedia_loudnorm_no_measurement", "tidymedia_ffmpeg_exit",
      "rlang_error", "error", "condition")
  )
  expect_true(is.integer(cnd$tm_status))
  expect_length(cnd$tm_status, 1L)
  expect_false(identical(cnd$tm_status, 0L))
})

test_that("the scalar unparseable abort answers to the event class alone", {
  # AC1, scalar zero-exit site (R/loudnorm_two_pass.R:112). FFmpeg ran and
  # exited zero but printed no finite measurement block, so the event holds and
  # the exit class does not: there is no non-zero status to assert. Driven with
  # a recorded analysis output rather than a real spawn, because a real FFmpeg
  # that exits zero always prints the block -- the mock is what makes the
  # zero-exit half of the event reachable at all.
  local_mocked_bindings(
    run_program = function(...) "ffmpeg printed nothing parseable",
    .package = "tidymedia"
  )
  f <- make_input("wav")
  out <- withr::local_tempfile(fileext = ".m4a")
  cnd <- tryCatch(
    normalize_audio(f, out, two_pass = TRUE, run = FALSE),
    tidymedia_loudnorm_no_measurement = function(e) e
  )
  expect_identical(
    class(cnd),
    c("tidymedia_loudnorm_no_measurement", "rlang_error", "error", "condition")
  )
  expect_false(inherits(cnd, "tidymedia_ffmpeg_exit"))
  expect_null(cnd$tm_status)
  # The site, not merely the class: this is the parse abort, not the run abort.
  expect_match(cli::ansi_strip(conditionMessage(cnd)),
               "Could not parse", fixed = TRUE)
})

test_that("a silent input still aborts under its own name, not the event class", {
  # The boundary that earns the name. A silent input WAS measured -- at -inf --
  # so "no measurement" would be false of it, and the abort three lines above
  # the parse abort must not answer to the shared class (RR05 B2). It is
  # unclassed today; what this locks is that M087 did not sweep it in.
  local_mocked_bindings(
    run_program = function(...) c('  "input_i" : "-inf",',
                                  '  "input_tp" : "-inf",',
                                  '  "input_lra" : "0.00",',
                                  '  "input_thresh" : "-inf",',
                                  '  "target_offset" : "0.00"'),
    .package = "tidymedia"
  )
  f <- make_input("wav")
  out <- withr::local_tempfile(fileext = ".m4a")
  cnd <- tryCatch(normalize_audio(f, out, two_pass = TRUE, run = FALSE),
                  error = function(e) e)
  expect_match(cli::ansi_strip(conditionMessage(cnd)),
               "appears to be silent", fixed = TRUE)
  expect_false(inherits(cnd, "tidymedia_loudnorm_no_measurement"))
  expect_identical(class(cnd), c("rlang_error", "error", "condition"))
})

test_that("the batch analysis abort answers to the same event class", {
  # AC1/AC2, batch site (R/loudnorm_two_pass.R:253). The same name the scalar
  # form raises, so a handler written from either topic fires on both; and NOT
  # the exit class, which would be false for any row that exited zero.
  skip_if_no_ffmpeg()
  bad <- make_unreadable_media()
  dir <- withr::local_tempdir()
  jobs <- tibble::tibble(input = bad, output = file.path(dir, "a.m4a"))
  cnd <- tryCatch(
    normalize_audio_batch(jobs, two_pass = TRUE, run = FALSE),
    tidymedia_loudnorm_no_measurement = function(e) e
  )
  expect_identical(
    class(cnd),
    c("tidymedia_loudnorm_no_measurement", "rlang_error", "error", "condition")
  )
  expect_false(inherits(cnd, "tidymedia_ffmpeg_exit"))
  expect_null(cnd$tm_status)
  expect_identical(cnd$tm_rows, 1L)
  expect_true(is.integer(cnd$tm_row_status))
})

test_that("the batch separation warning carries the event class and no exit", {
  # AC3, batch warning site (R/ffmpeg.R:742). One event, one name at both
  # severities -- the error site at R/ffmpeg.R:681 adds the exit class for a
  # second fact it can evidence, and this site cannot: ffm_batch() reduces a row
  # to whether it succeeded and discards the condition (D007), so no exit number
  # and no `tm_status` exist here.
  skip_if_no_ffprobe()
  multi <- make_multitrack_video()
  dir <- withr::local_tempdir()
  jobs <- tibble::tibble(
    input     = multi,
    audiofile = file.path(dir, "bad.mp3"),
    videofile = file.path(dir, "v.mkv")
  )
  w <- tryCatch(separate_audio_video_batch(jobs), warning = function(w) w)
  expect_identical(
    class(w),
    c("tidymedia_multitrack_separation", "rlang_warning", "warning",
      "condition")
  )
  expect_false(inherits(w, "tidymedia_ffmpeg_exit"))
  expect_null(w$tm_status)
})

test_that("the two batch topics state why their diagnostic has no exit status", {
  # AC2/AC3, the documented halves. Each reason is a claim about the code's
  # behaviour, so it is locked here rather than left to prose drift: the
  # loudnorm batch abort has no single status because a batch mixes causes, and
  # the separation batch warning has none because the runner records whether a
  # row succeeded, not how FFmpeg exited.
  rd <- rd_sources()
  skip_if(is.null(rd), "no Rd source available")
  pick <- function(topic) {
    hit <- rd[grepl(topic, names(rd), fixed = TRUE)]
    skip_if(length(hit) == 0, paste("no Rd for", topic))
    paste(hit, collapse = "\n")
  }

  nb <- pick("normalize_audio_batch.Rd")
  expect_match(nb, "carries no single exit status", fixed = TRUE)
  expect_match(nb, "rows that exited zero", fixed = TRUE)

  sb <- pick("separate_audio_video_batch.Rd")
  expect_match(sb, "carries no exit status", fixed = TRUE)
  expect_match(sb, "success", fixed = TRUE)
  expect_match(sb, "not \\emph{how} FFmpeg exited", fixed = TRUE)

  # The silence asymmetry, stated from both sides (RR05 B2).
  expect_match(pick("normalize_audio.Rd"), "does not abort on a silent row",
               fixed = TRUE)
  expect_match(nb, "aborts on a silent", fixed = TRUE)
})

test_that("every topic names the classes its site actually raises", {
  # AC4. The pairing is checked from the OBSERVED side: each site is executed,
  # its `tidymedia_*` classes are read off the condition object, and every one
  # of them must appear in each topic the milestone pairs with that site. A
  # topic that names a class by hand and drifts from the code fails here.
  skip_if_no_ffmpeg()
  skip_if_no_ffprobe()
  rd <- rd_sources()
  skip_if(is.null(rd), "no Rd source available")
  pick <- function(topic) {
    hit <- rd[grepl(topic, names(rd), fixed = TRUE)]
    skip_if(length(hit) == 0, paste("no Rd for", topic))
    paste(hit, collapse = "\n")
  }
  tm_classes <- function(cnd) grep("^tidymedia_", class(cnd), value = TRUE)

  dir <- withr::local_tempdir()
  bad <- make_unreadable_media()
  multi <- make_multitrack_video()

  observed <- list(
    # R/loudnorm_two_pass.R:151 -- scalar analysis, non-zero exit.
    scalar_exit = tryCatch(
      normalize_audio(bad, file.path(dir, "a.m4a"), two_pass = TRUE,
                      run = FALSE),
      condition = function(e) e),
    # R/loudnorm_two_pass.R:253 -- batch analysis.
    batch_loudnorm = tryCatch(
      normalize_audio_batch(
        tibble::tibble(input = bad, output = file.path(dir, "c.m4a")),
        two_pass = TRUE, run = FALSE),
      condition = function(e) e),
    # R/ffmpeg.R:681 -- scalar multi-track separation error.
    scalar_sep = tryCatch(
      separate_audio_video(multi, file.path(dir, "s.mp3"),
                           file.path(dir, "s.mkv")),
      condition = function(e) e),
    # R/ffmpeg.R:742 -- batch multi-track separation warning.
    batch_sep = tryCatch(
      separate_audio_video_batch(tibble::tibble(
        input = multi,
        audiofile = file.path(dir, "b.mp3"),
        videofile = file.path(dir, "b.mkv"))),
      warning = function(w) w)
  )
  # R/loudnorm_two_pass.R:112 -- scalar analysis, zero exit, unparseable. Its
  # own block, because the mock must not be live for the four real spawns.
  observed$scalar_unparseable <- local({
    local_mocked_bindings(
      run_program = function(...) "ffmpeg printed nothing parseable",
      .package = "tidymedia")
    tryCatch(
      normalize_audio(make_input("wav"), file.path(dir, "d.m4a"),
                      two_pass = TRUE, run = FALSE),
      condition = function(e) e)
  })

  pairing <- list(
    scalar_exit        = c("normalize_audio.Rd", "ffm_run.Rd",
                           "tidymedia-package.Rd"),
    scalar_unparseable = c("normalize_audio.Rd", "ffm_run.Rd",
                           "tidymedia-package.Rd"),
    batch_loudnorm     = c("normalize_audio_batch.Rd", "tidymedia-package.Rd"),
    scalar_sep         = c("separate_audio_video.Rd", "ffm_run.Rd",
                           "tidymedia-package.Rd"),
    batch_sep          = c("separate_audio_video_batch.Rd")
  )

  for (site in names(pairing)) {
    classes <- tm_classes(observed[[site]])
    # The site really signalled, and really carries package classes: an empty
    # vector here would let every assertion below pass vacuously.
    expect_gt(length(classes), 0L)
    for (topic in pairing[[site]]) {
      txt <- pick(topic)
      for (cls in classes) {
        expect_match(txt, cls, fixed = TRUE,
                     info = paste(site, "->", topic, "::", cls))
      }
    }
  }

  # AC1's sweep: the retired name survives nowhere outside the tracking files.
  # Assembled rather than written out, so this file does not match itself.
  retired <- paste0("tidymedia_loudnorm_", "analysis")
  # `git grep` searches from its own working directory downward, and testthat
  # runs with the wd set to tests/testthat -- so the sweep must be aimed at the
  # package root explicitly or it reaches only this directory and can never see
  # the name come back in R/, man/ or NEWS.md (M087 re-review F3).
  root <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  git_at_root <- function(...) suppressWarnings(system2(
    "git", c("-C", root, ...), stdout = TRUE, stderr = FALSE))
  # `root` must be the checkout's OWN top level, not merely somewhere inside a
  # work tree: under `R CMD check` the tests run from a copy at
  # <pkg>.Rcheck/tests/testthat, whose root is the untracked <pkg>.Rcheck dir --
  # inside the workspace repo on CI, so `--is-inside-work-tree` says true while
  # `git grep` sees no tracked file there and the sweep would fail vacuously.
  top <- suppressWarnings(tryCatch(
    git_at_root("rev-parse", "--show-toplevel"), error = function(e) character(0)))
  in_source_checkout <- nzchar(Sys.which("git")) &&
    length(top) == 1L && nzchar(top) &&
    identical(normalizePath(top, mustWork = FALSE), root)
  skip_if(!in_source_checkout, "not in the package's own git checkout")
  # The sweep must be shown to run over a non-empty domain, and over the domain
  # it CLAIMS: the current name lives in R/, man/ and NEWS.md as well as here,
  # so requiring a hit outside tests/ proves the grep reaches past its own wd.
  control <- git_at_root(
    "grep", "-l", "tidymedia_loudnorm_no_measurement", "--", ":!cairn/")
  expect_gt(length(control), 0L)
  expect_true(any(!startsWith(as.character(control), "tests/")))
  hits <- git_at_root("grep", "-l", retired, "--", ":!cairn/")
  expect_identical(as.character(hits), character(0))
})
