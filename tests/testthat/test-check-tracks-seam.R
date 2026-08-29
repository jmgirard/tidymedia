# M082: `tidymedia.check_tracks`, the seam that switches off D024's
# dropped-audio-track probe and the FFprobe call it costs per distinct input.


# resolve_check_tracks() (AC1) ---------------------------------------------

test_that("an unset option answers TRUE", {
  withr::local_options(list(tidymedia.check_tracks = NULL))
  expect_true(resolve_check_tracks())
})

test_that("the two legal values come back unchanged", {
  withr::local_options(list(tidymedia.check_tracks = TRUE))
  expect_true(resolve_check_tracks())
  withr::local_options(list(tidymedia.check_tracks = FALSE))
  expect_false(resolve_check_tracks())
})

test_that("a value that is not one logical is refused, naming the option", {
  # Not isTRUE(): that reads every value below as FALSE and would silently
  # REMOVE the check from a session that asked to keep it. Each case asserts
  # which refusal it got -- the option's name plus the kind of value named --
  # so one message cannot stand in for another.
  cases <- list(
    list(value = "yes",         says = 'not the string'),
    list(value = NA,            says = 'not `NA`'),
    list(value = c(TRUE, TRUE), says = 'not a logical vector'),
    list(value = 1,             says = 'not the number 1')
  )
  for (case in cases) {
    withr::local_options(list(tidymedia.check_tracks = case$value))
    msg <- tryCatch(resolve_check_tracks(), error = function(e) {
      cli::ansi_strip(conditionMessage(e))
    })
    expect_match(msg, "`tidymedia.check_tracks` must be `TRUE` or `FALSE`",
                 fixed = TRUE)
    expect_match(gsub("\n", " ", msg), case$says, fixed = TRUE)
  }
})

test_that("the refusal blames the caller's frame, not the resolver", {
  # `call` is threaded so a verb's abort says the verb, the way
  # resolve_timeout()'s does. Without it every message here would read
  # "in resolve_check_tracks()".
  withr::local_options(list(tidymedia.check_tracks = "yes"))
  outer <- function() resolve_check_tracks()
  cnd <- tryCatch(outer(), error = function(e) e)
  expect_identical(rlang::call_name(conditionCall(cnd)), "outer")
})


# The seam at every probe site (AC1, AC2) ----------------------------------

# One entry per site the AC1 grep returns: extract_audio(), convert_audio(),
# normalize_audio()'s two mutually exclusive two_pass branches, and the shared
# batch site reached through each of the three `_batch` verbs. Named, so a
# failure says which site rather than which index.
drop_call_sites <- function(infile, dir) {
  p <- function(stem, ext) file.path(dir, paste0(stem, ext))
  jobs <- function(stem, ext) {
    tibble::tibble(input = c(infile, infile),
                   output = c(p(paste0(stem, "1"), ext),
                              p(paste0(stem, "2"), ext)))
  }
  list(
    "extract_audio()" = function() extract_audio(infile, p("ea", ".mka")),
    "convert_audio()" = function() convert_audio(infile, p("ca", ".mp3")),
    "normalize_audio(two_pass = FALSE)" =
      function() normalize_audio(infile, p("n1", ".mkv"), two_pass = FALSE),
    "normalize_audio(two_pass = TRUE)" =
      function() normalize_audio(infile, p("n2", ".mkv"), two_pass = TRUE),
    "extract_audio_batch()" =
      function() extract_audio_batch(jobs("eab", ".mka")),
    "convert_audio_batch()" =
      function() convert_audio_batch(jobs("cab", ".mp3")),
    "normalize_audio_batch()" =
      function() normalize_audio_batch(jobs("nab", ".mkv"))
  )
}

# Run `site` with a counting stand-in for count_audio_streams_all(), returning
# the call count and every dropped-audio warning. A COUNTER, never a stop()ing
# mock: count_audio_streams() wraps its run in tryCatch() by design, so a mock
# that threw would be swallowed and let a probe on a gated path pass unseen
# (M44's lesson, and the reason M44's own run = FALSE test is written this way).
# The stand-in answers three tracks per input, so the warning still fires
# without an FFprobe call of its own.
count_probes <- function(site) {
  calls <- 0L
  local_mocked_bindings(
    count_audio_streams_all = function(files, ...) {
      calls <<- calls + 1L
      rep(3L, length(files))
    }
  )
  res <- catch_drop(site())
  list(calls = calls, warnings = res$warnings, value = res$value)
}

test_that("the seam switched off runs no probe and warns at no site", {
  skip_if_no_ffmpeg()
  infile <- make_multitrack_video()
  dir <- withr::local_tempdir()
  withr::local_options(list(tidymedia.check_tracks = FALSE))
  for (nm in names(drop_call_sites(infile, dir))) {
    res <- count_probes(drop_call_sites(infile, dir)[[nm]])
    expect_identical(res$calls, 0L, info = nm)
    expect_length(res$warnings, 0L)
  }
})

test_that("the default probes and warns exactly once at every site", {
  # The passing control for the test above, and the unchanged-default criterion
  # at the same time: with the option unset every site probes and signals ONE
  # warning -- including normalize_audio() at both two_pass values, whose two
  # sites are mutually exclusive and warned twice before M075 separated them.
  skip_if_no_ffmpeg()
  infile <- make_multitrack_video()
  dir <- withr::local_tempdir()
  withr::local_options(list(tidymedia.check_tracks = NULL))
  for (nm in names(drop_call_sites(infile, dir))) {
    res <- count_probes(drop_call_sites(infile, dir)[[nm]])
    expect_gte(res$calls, 1L)
    expect_length(res$warnings, 1L)
    expect_s3_class(res$warnings[[1]], "tidymedia_dropped_audio")
  }
})

test_that("a malformed option aborts at every site, naming the option", {
  # No binary needed: every site consults the seam before it probes or runs, so
  # an empty file that merely exists carries this test on a machine with no
  # FFmpeg at all.
  infile <- make_input("mkv")
  dir <- withr::local_tempdir()
  withr::local_options(list(tidymedia.check_tracks = "yes"))
  for (nm in names(drop_call_sites(infile, dir))) {
    res <- catch_drop(drop_call_sites(infile, dir)[[nm]]())
    expect_s3_class(res$value, "error")
    expect_match(cli::ansi_strip(conditionMessage(res$value)),
                 "`tidymedia.check_tracks` must be `TRUE` or `FALSE`",
                 fixed = TRUE)
    expect_length(res$warnings, 0L)
  }
})

test_that("a call that names a track, or does not run, reads no option", {
  # The seam is the LAST conjunct at the scalar sites and sits below the batch
  # form's rows check, so a caller who declined the probe on the OTHER two
  # grounds is not exposed to a stale option in a startup file. Without that
  # ordering each expression below aborts instead.
  infile <- make_input("mkv")
  jobs <- tibble::tibble(input = infile, output = "out.mka")
  withr::local_options(list(tidymedia.check_tracks = "yes"))
  expect_no_error(extract_audio(infile, "out.mka", run = FALSE))
  expect_no_error(extract_audio(infile, "out.mka", audio_stream = 0,
                                run = FALSE))
  expect_no_error(extract_audio_batch(jobs, run = FALSE))
  expect_no_error(
    warn_dropped_audio_batch(
      tibble::tibble(input = infile, output = "x", audio_stream = 0)
    )
  )
})
