# The six argument contradictions, refused at the front door of every verb that
# fans out (M58).
#
# A contradiction here is a disagreement between two values the verb already
# holds -- a stream copy that also names a GPU encoder, an audio encoder with no
# audio mapped. Nothing is probed to decide one, which is what separates these
# from M57's encoder-availability guard: the same wrong call is a contradiction
# on every machine.
#
# Each used to abort inside a `*_pipeline()` function. On a verb that fans out
# through ffm_batch() -> purrr::pmap() the pipeline's `call` resolves to an
# anonymous closure, so the user was shown
# "Error in `purrr::pmap(jobs, .f, ...)` / In index: 1" -- a dependency's name
# and an internal index in place of the verb they called (LESSONS M47/M48-F1).
# The abort now lives in a shared checker that the pipeline AND the fan-out
# verb's front door both call.
#
# Nothing here needs FFmpeg: every probe runs at `run = FALSE`. The encoder seam
# is held EMPTY wherever a call is expected to abort, so a message mentioning
# availability is a failure rather than a coincidence; the one test that needs a
# call to SUCCEED under `hardware = "nvenc"` holds it full and says why.

blamed_verb <- function(cnd) {
  cl <- conditionCall(cnd)
  if (is.null(cl)) return(NA_character_)
  paste(deparse(cl[[1]]), collapse = "")
}

catch_call <- function(verb, args) {
  # Only DEFAULT `run`, never override it: one case below passes a malformed
  # `run` on purpose, and clobbering it would leave that case asserting the
  # absence of an error the call no longer had.
  if (is.null(args$run)) args$run <- FALSE
  tryCatch(
    do.call(verb, args, envir = asNamespace("tidymedia")),
    error = function(e) e
  )
}

# --- AC1: one site per headline ---------------------------------------------
#
# The five distinct headlines the six conditions carry. Conditions 4 and 6 share
# one, which is why five checkers cover six conditions: their headline and their
# "x" line are identical and only the way out differs, so a sixth checker would
# be a second site spelling the same words -- the drift M40 paid for by copying
# a shared guard's wording rather than parameterizing it.
contradiction_headlines <- function() {
  c(
    hardware_needs_encode = "{.arg hardware} needs a re-encoding {.arg video_codec}.",
    codec_needs_reencode = "{.arg video_codec} and {.arg hardware} need a re-encoding cut.",
    audio_codec_needs_reencode = "{.arg audio_codec} needs a re-encoding cut.",
    audio_codec_needs_audio = "{.arg audio_codec} needs an audio stream to encode.",
    resize_needs_two_inputs = "{.arg resize} currently supports exactly two inputs."
  )
}

test_that("each contradiction headline is written at exactly one site", {
  src <- unlist(lapply(list.files("../../R", pattern = "\\.R$",
                                  full.names = TRUE),
                       readLines, warn = FALSE))
  skip_if(length(src) == 0, "package sources not readable from the test dir")
  for (nm in names(contradiction_headlines())) {
    headline <- contradiction_headlines()[[nm]]
    hits <- sum(grepl(headline, src, fixed = TRUE))
    expect_identical(hits, 1L, info = nm)
  }
})

test_that("the shared checker carries a different way out for each of its verbs", {
  # The parameterization AC1 rests on: one headline, two hints. If the two
  # hints were ever collapsed this passes only by breaking one verb's advice.
  input <- make_input()
  compare <- catch_call("compare_videos_batch", list(
    jobs = tibble::tibble(inputs = list(c(input, input)), output = "o.mp4"),
    audio_codec = "aac"))
  pip <- catch_call("picture_in_picture_batch", list(
    jobs = tibble::tibble(main = input, overlay = input, output = "o.mp4"),
    audio_codec = "aac"))
  expect_match(conditionMessage(compare), "0-based index of the input")
  expect_match(conditionMessage(pip), "for the main video's audio")
  expect_false(identical(conditionMessage(compare), conditionMessage(pip)))
})

# --- AC2: one blame test per (condition, verb) pair --------------------------
#
# The eight pairs the milestone enumerates. `args` is a call that violates the
# named condition and nothing else, and `own` is a fragment of that
# condition's message -- asserted so a pair that starts failing for an unrelated
# reason (a schema error, a missing column) records that instead of passing on a
# bare abort.
contradiction_pairs <- function(input) {
  list(
    list(id = "1/separate_audio_video_batch",
         verb = "separate_audio_video_batch",
         own = "`hardware` needs a re-encoding",
         args = list(jobs = tibble::tibble(input = input, audiofile = "a.aac",
                                           videofile = "v.mp4"),
                     video_codec = "copy", hardware = "nvenc")),
    list(id = "2/segment_video", verb = "segment_video",
         own = "need a re-encoding cut",
         args = list(infile = input, start = 0, end = 1, outfiles = "o.mp4",
                     reencode = FALSE, video_codec = "libx264")),
    list(id = "2/segment_video_batch", verb = "segment_video_batch",
         own = "need a re-encoding cut",
         args = list(jobs = tibble::tibble(input = input, output = "o.mp4",
                                           start = 0, end = 1),
                     reencode = FALSE, video_codec = "libx264")),
    list(id = "3/segment_video", verb = "segment_video",
         own = "`audio_codec` needs a re-encoding cut",
         args = list(infile = input, start = 0, end = 1, outfiles = "o.mp4",
                     reencode = FALSE, audio_codec = "aac")),
    list(id = "3/segment_video_batch", verb = "segment_video_batch",
         own = "`audio_codec` needs a re-encoding cut",
         args = list(jobs = tibble::tibble(input = input, output = "o.mp4",
                                           start = 0, end = 1),
                     reencode = FALSE, audio_codec = "aac")),
    list(id = "4/compare_videos_batch", verb = "compare_videos_batch",
         own = "needs an audio stream to encode",
         args = list(jobs = tibble::tibble(inputs = list(c(input, input)),
                                           output = "o.mp4"),
                     audio_codec = "aac")),
    list(id = "5/compare_videos_batch", verb = "compare_videos_batch",
         own = "exactly two inputs",
         args = list(jobs = tibble::tibble(
           inputs = list(c(input, input, input)), output = "o.mp4"),
           resize = TRUE)),
    list(id = "6/picture_in_picture_batch", verb = "picture_in_picture_batch",
         own = "needs an audio stream to encode",
         args = list(jobs = tibble::tibble(main = input, overlay = input,
                                           output = "o.mp4"),
                     audio_codec = "aac"))
  )
}

test_that("every (condition, verb) pair blames the verb the user called", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  input <- make_input()
  for (pair in contradiction_pairs(input)) {
    cnd <- catch_call(pair$verb, pair$args)
    expect_s3_class(cnd, "rlang_error")
    expect_match(conditionMessage(cnd), pair$own, info = pair$id)
    expect_identical(blamed_verb(cnd), pair$verb, info = pair$id)
    expect_no_match(conditionMessage(cnd), "purrr::pmap", fixed = TRUE,
                    info = pair$id)
    expect_no_match(conditionMessage(cnd), "In index:", fixed = TRUE,
                    info = pair$id)
    # The blame lives in conditionCall(), which is what the console prints as
    # "Error in `<verb>()`"; deparsing the whole call would also catch the
    # dependency's name leaking there.
    expect_no_match(paste(deparse(conditionCall(cnd)), collapse = ""),
                    "pmap", fixed = TRUE, info = pair$id)
  }
})

# --- AC4: the guards sweep rows ---------------------------------------------
#
# Every value a condition reads can arrive as a `jobs` column on at least one of
# these verbs, so the guard has to answer per row. Each case pairs a table with
# ONE violating row against a table of the same shape with none: the first must
# be refused naming the verb, the second must compile. Asserting only the first
# would pass for a guard that refuses every table.
contradiction_columns <- function(input) {
  two <- function(...) tibble::tibble(...)
  list(
    # `ok_rows` is 4, not 2: this verb reshapes each input row into an audio row
    # and a video row before the fan-out (D003/D007), so its clean table
    # compiles two commands per job.
    list(id = "1/video_codec column", verb = "separate_audio_video_batch",
         own = "`hardware` needs a re-encoding", ok_rows = 4L,
         base = list(hardware = "nvenc"),
         jobs_bad = two(input = c(input, input),
                        audiofile = c("a1.aac", "a2.aac"),
                        videofile = c("v1.mp4", "v2.mp4"),
                        video_codec = c("libx264", "copy")),
         jobs_ok = two(input = c(input, input),
                       audiofile = c("a1.aac", "a2.aac"),
                       videofile = c("v1.mp4", "v2.mp4"),
                       video_codec = c("libx264", "libx264"))),
    list(id = "2/reencode column", verb = "segment_video_batch",
         own = "need a re-encoding cut",
         base = list(video_codec = "libx264"),
         jobs_bad = two(input = c(input, input), output = c("a.mp4", "b.mp4"),
                        start = c(0, 0), end = c(1, 1),
                        reencode = c(TRUE, FALSE)),
         jobs_ok = two(input = c(input, input), output = c("a.mp4", "b.mp4"),
                       start = c(0, 0), end = c(1, 1),
                       reencode = c(TRUE, TRUE))),
    list(id = "2/video_codec column", verb = "segment_video_batch",
         own = "need a re-encoding cut",
         base = list(reencode = FALSE),
         # NA is the column form of the NULL sentinel (D022), so the clean table
         # is the one naming no encoder at all -- the reading the front door
         # must share with the pipeline or it would refuse a call that compiles.
         jobs_bad = two(input = c(input, input), output = c("a.mp4", "b.mp4"),
                        start = c(0, 0), end = c(1, 1),
                        video_codec = c(NA, "libx264")),
         jobs_ok = two(input = c(input, input), output = c("a.mp4", "b.mp4"),
                       start = c(0, 0), end = c(1, 1),
                       video_codec = c(NA, NA))),
    list(id = "3/audio_codec column", verb = "segment_video_batch",
         own = "`audio_codec` needs a re-encoding cut",
         base = list(reencode = FALSE),
         jobs_bad = two(input = c(input, input), output = c("a.mp4", "b.mp4"),
                        start = c(0, 0), end = c(1, 1),
                        audio_codec = c("copy", "aac")),
         jobs_ok = two(input = c(input, input), output = c("a.mp4", "b.mp4"),
                       start = c(0, 0), end = c(1, 1),
                       audio_codec = c("copy", "copy"))),
    list(id = "4/audio column", verb = "compare_videos_batch",
         own = "needs an audio stream to encode",
         base = list(audio_codec = "aac"),
         # An NA `audio` cell drops that row's audio, which is exactly what
         # leaves its named encoder with nothing to act on.
         jobs_bad = two(inputs = list(c(input, input), c(input, input)),
                        output = c("a.mp4", "b.mp4"), audio = c(0, NA)),
         jobs_ok = two(inputs = list(c(input, input), c(input, input)),
                       output = c("a.mp4", "b.mp4"), audio = c(0, 0))),
    list(id = "4/audio_codec column", verb = "compare_videos_batch",
         own = "needs an audio stream to encode",
         base = list(),
         jobs_bad = two(inputs = list(c(input, input), c(input, input)),
                        output = c("a.mp4", "b.mp4"),
                        audio_codec = c(NA, "aac")),
         jobs_ok = two(inputs = list(c(input, input), c(input, input)),
                       output = c("a.mp4", "b.mp4"),
                       audio_codec = c(NA, NA))),
    list(id = "5/resize column", verb = "compare_videos_batch",
         own = "exactly two inputs",
         base = list(),
         jobs_bad = two(inputs = list(c(input, input, input),
                                      c(input, input, input)),
                        output = c("a.mp4", "b.mp4"),
                        resize = c(FALSE, TRUE)),
         jobs_ok = two(inputs = list(c(input, input, input),
                                     c(input, input, input)),
                       output = c("a.mp4", "b.mp4"),
                       resize = c(FALSE, FALSE))),
    list(id = "5/per-row input count", verb = "compare_videos_batch",
         own = "exactly two inputs",
         base = list(resize = TRUE),
         # The count is per row by construction on a fan-in verb: one row of
         # three inputs is enough, and a table of two-input rows must compile.
         jobs_bad = two(inputs = list(c(input, input),
                                      c(input, input, input)),
                        output = c("a.mp4", "b.mp4")),
         jobs_ok = two(inputs = list(c(input, input), c(input, input)),
                       output = c("a.mp4", "b.mp4"))),
    list(id = "6/audio column", verb = "picture_in_picture_batch",
         own = "needs an audio stream to encode",
         base = list(audio_codec = "aac"),
         jobs_bad = two(main = c(input, input), overlay = c(input, input),
                        output = c("a.mp4", "b.mp4"), audio = c(0, NA)),
         jobs_ok = two(main = c(input, input), overlay = c(input, input),
                       output = c("a.mp4", "b.mp4"), audio = c(0, 0)))
  )
}

test_that("one violating row is refused and a clean column compiles", {
  # The one test here that holds the seam FULL rather than empty. Case 1's
  # clean table names `hardware = "nvenc"` on every row, so its baseline can
  # only compile on a machine that has the encoder -- and a baseline that
  # aborts would satisfy the comparison while measuring nothing. The violating
  # half is unaffected: a contradiction is settled without the encoder list, and
  # the empty-seam half of that same table is pinned in
  # test-nvenc-front-door.R's mixed-copy-column test.
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  input <- make_input()
  for (case in contradiction_columns(input)) {
    bad <- catch_call(case$verb, c(case$base, list(jobs = case$jobs_bad)))
    expect_s3_class(bad, "rlang_error")
    expect_match(conditionMessage(bad), case$own, info = case$id)
    expect_identical(blamed_verb(bad), case$verb, info = case$id)

    ok <- catch_call(case$verb, c(case$base, list(jobs = case$jobs_ok)))
    expect_false(inherits(ok, "condition"), info = case$id)
    expected_rows <- if (is.null(case$ok_rows)) 2L else case$ok_rows
    expect_identical(nrow(ok), expected_rows, info = case$id)
    expect_true(all(nzchar(ok$command)), info = case$id)
  }
})

# --- the precedence the front door reassigns ---------------------------------
#
# A guard moved ahead of the fan-out reports ahead of everything the fan-out
# raised, not only the checks the milestone set out to precede. That is M41's
# known cost, and the rule this repo applies to it is that it be tested rather
# than assumed away (D035's second condition). The four cases below are the
# ones the changelog names, so the entry has a test that fails without the
# behavior it asserts.
#
# Each case is wrong in TWO ways: the contradiction, plus a second error that
# reported first on the previous version. The control is the same call with the
# contradiction removed, asserted to still raise the second error -- without it
# a case would pass for a call that had only ever had one error, and the
# precedence claim would rest on nothing.

test_that("a contradiction reports before errors raised inside the fan-out", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  input <- make_input()
  two <- function(...) tibble::tibble(...)
  cases <- list(
    list(id = "per-row audio index", verb = "compare_videos_batch",
         second = "must be a whole number",
         args = list(jobs = two(inputs = list(c(input, input), c(input, input)),
                                output = c("a.mp4", "b.mp4"),
                                audio = c(7, NA)),
                     audio_codec = "aac"),
         # Row 2's NA is what drops the audio and makes the encoder a
         # contradiction; giving it an index leaves only the range error.
         control = list(jobs = two(inputs = list(c(input, input),
                                                 c(input, input)),
                                   output = c("a.mp4", "b.mp4"),
                                   audio = c(7, 0)),
                        audio_codec = "aac")),
    list(id = "direction vocabulary", verb = "compare_videos_batch",
         second = "must be one of",
         args = list(jobs = two(inputs = list(rep(input, 3)), output = "o.mp4",
                                direction = "diagonal"),
                     resize = TRUE),
         control = list(jobs = two(inputs = list(rep(input, 3)),
                                   output = "o.mp4", direction = "diagonal"),
                        resize = FALSE)),
    list(id = "per-row margin range", verb = "picture_in_picture_batch",
         second = "must be a whole number",
         args = list(jobs = two(main = input, overlay = input,
                                output = "o.mp4", margin = -3),
                     audio_codec = "aac"),
         control = list(jobs = two(main = input, overlay = input,
                                   output = "o.mp4", margin = -3),
                        audio_codec = "copy")),
    list(id = "ffm_batch's own run check", verb = "segment_video_batch",
         second = "`run` must be",
         args = list(jobs = two(input = input, output = "o.mp4",
                                start = 0, end = 1),
                     reencode = FALSE, video_codec = "libx264", run = "yes"),
         control = list(jobs = two(input = input, output = "o.mp4",
                                   start = 0, end = 1),
                        reencode = TRUE, video_codec = "libx264",
                        run = "yes"))
  )
  for (case in cases) {
    # The control first: it establishes that the second error is live on this
    # call at all, so the assertion below records a precedence and not a typo.
    ctl <- tryCatch(do.call(case$verb, case$control,
                            envir = asNamespace("tidymedia")),
                    error = function(e) e)
    expect_s3_class(ctl, "rlang_error")
    expect_match(conditionMessage(ctl), case$second, info = case$id)

    cnd <- catch_call(case$verb, case$args)
    expect_s3_class(cnd, "rlang_error")
    expect_no_match(conditionMessage(cnd), case$second, info = case$id)
    expect_identical(blamed_verb(cnd), case$verb, info = case$id)
  }
})

# --- AC6: the scalar siblings keep the pipeline's abort ----------------------
#
# Four of the six conditions belong to verbs whose scalar sibling calls its
# pipeline directly, so `call` already resolves to the verb and M58 adds no
# front-door guard there (the M47 F8 reading). What has to hold is that the
# extraction did not cost them their blame -- and, for condition 5, that the
# `call = call` M58 added actually reaches the user, since before it the abort
# displayed `compare_videos_pipeline()`.

test_that("the scalar siblings still blame themselves", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  input <- make_input()
  cases <- list(
    list(id = "1/separate_audio_video", verb = "separate_audio_video",
         own = "`hardware` needs a re-encoding",
         args = list(infile = input, audiofile = "a.aac", videofile = "v.mp4",
                     video_codec = "copy", hardware = "nvenc")),
    list(id = "4/compare_videos", verb = "compare_videos",
         own = "needs an audio stream to encode",
         args = list(infiles = c(input, input), outfile = "o.mp4",
                     audio_codec = "aac")),
    list(id = "5/compare_videos", verb = "compare_videos",
         own = "exactly two inputs",
         args = list(infiles = c(input, input, input), outfile = "o.mp4",
                     resize = TRUE)),
    list(id = "6/picture_in_picture", verb = "picture_in_picture",
         own = "needs an audio stream to encode",
         args = list(main = input, overlay = input, outfile = "o.mp4",
                     audio_codec = "aac"))
  )
  for (case in cases) {
    cnd <- catch_call(case$verb, case$args)
    expect_s3_class(cnd, "rlang_error")
    expect_match(conditionMessage(cnd), case$own, info = case$id)
    expect_identical(blamed_verb(cnd), case$verb, info = case$id)
    expect_no_match(blamed_verb(cnd), "_pipeline", fixed = TRUE, info = case$id)
  }
})
