# A value error and a contradiction resolve the same way in both forms (M61).
#
# M58 put six argument contradictions at these verbs' front doors and D036 gave
# them precedence: where a verb carries both a contradiction and a value error,
# the contradiction reports, because a contradiction is decided identically on
# every machine. M59 then swept the per-row VALUE checks to the front door,
# below that contradiction sweep -- so a value error arriving in a `jobs` column
# obeyed D036, while the same value passed as a scalar ARGUMENT was caught by a
# guard at the top of the verb and reported instead. D038 recorded that
# disagreement as a disclosed gap. This suite is the gap closed: the four guards
# now sit below the contradiction sweep, so both forms answer alike.
#
# Every case here is wrong in TWO ways, and every case has a CONTROL: the same
# call with the value in range, asserted to still raise the other error. Without
# the control a case would pass for a call that only ever had one error, and the
# ordering claim would rest on nothing.
#
# Nothing here needs FFmpeg: every probe runs at `run = FALSE`. The encoder seam
# is held EMPTY wherever a call is expected to abort, so a message mentioning
# availability is a failure rather than a coincidence.
#
# blamed_verb() / catch_call() come from helper-blame.R.

expect_reports <- function(case) {
  # The control FIRST: it establishes that `wins` is live on this call at all.
  ctl <- catch_call(case$verb, case$control)
  expect_s3_class(ctl, "rlang_error")
  expect_match(conditionMessage(ctl), case$wins, info = paste(case$id, "control"))

  cnd <- catch_call(case$verb, case$args)
  expect_s3_class(cnd, "rlang_error")
  expect_match(conditionMessage(cnd), case$wins, info = case$id)
  expect_no_match(conditionMessage(cnd), case$other, info = case$id)
  expect_identical(blamed_verb(cnd), case$verb, info = case$id)
}

# --- AC1: the contradiction reports, in BOTH forms ---------------------------

ordering_cases <- function(input) {
  two <- function(...) tibble::tibble(...)
  # The two contradiction wordings these verbs carry, and the two value-check
  # wordings the four guards raise.
  no_audio <- "needs an audio stream to encode"
  resize2 <- "supports exactly two inputs"
  vocab <- "must be one of"
  range <- "must be a whole number"

  cmp1 <- function(...) two(inputs = list(c(input, input)), output = "o.mp4", ...)
  cmp2 <- function(...) two(inputs = list(c(input, input), c(input, input)),
                            output = c("a.mp4", "b.mp4"), ...)
  # Three inputs, so `resize = TRUE` contradicts the input count while `audio`
  # stays free to be in or out of range independently of it. This is the only
  # contradiction on compare_videos_batch() that an `audio` case can cross:
  # supplying `audio` at all is what removes the audio_codec one.
  cmp1x <- function(...) two(inputs = list(rep(input, 3)), output = "o.mp4", ...)
  cmp2x <- function(...) two(inputs = list(rep(input, 3), rep(input, 3)),
                             output = c("a.mp4", "b.mp4"), ...)
  pip1 <- function(...) two(main = input, overlay = input, output = "o.mp4", ...)
  pip2 <- function(...) two(main = c(input, input), overlay = c(input, input),
                            output = c("a.mp4", "b.mp4"), ...)

  list(
    list(id = "direction/argument", verb = "compare_videos_batch",
         wins = no_audio, other = vocab,
         args = list(jobs = cmp1(), direction = "sideways",
                     audio_codec = "aac"),
         control = list(jobs = cmp1(), direction = "vertical",
                        audio_codec = "aac")),
    list(id = "direction/column", verb = "compare_videos_batch",
         wins = no_audio, other = vocab,
         args = list(jobs = cmp2(direction = c("sideways", "sideways")),
                     audio_codec = "aac"),
         control = list(jobs = cmp2(direction = c("vertical", "vertical")),
                        audio_codec = "aac")),

    list(id = "position/argument", verb = "picture_in_picture_batch",
         wins = no_audio, other = vocab,
         args = list(jobs = pip1(), position = "middleish",
                     audio_codec = "aac"),
         control = list(jobs = pip1(), position = "center",
                        audio_codec = "aac")),
    list(id = "position/column", verb = "picture_in_picture_batch",
         wins = no_audio, other = vocab,
         args = list(jobs = pip2(position = c("middleish", "middleish")),
                     audio_codec = "aac"),
         control = list(jobs = pip2(position = c("center", "center")),
                        audio_codec = "aac")),

    list(id = "margin/argument", verb = "picture_in_picture_batch",
         wins = no_audio, other = range,
         args = list(jobs = pip1(), margin = -3, audio_codec = "aac"),
         control = list(jobs = pip1(), margin = 16, audio_codec = "aac")),
    list(id = "margin/column", verb = "picture_in_picture_batch",
         wins = no_audio, other = range,
         args = list(jobs = pip2(margin = c(-3, -3)), audio_codec = "aac"),
         control = list(jobs = pip2(margin = c(16, 16)),
                        audio_codec = "aac")),

    # `audio` on compare_videos_batch() is probed at BOTH bounds, because the
    # two used to be checked in two places: the argument's lower bound at the
    # top of the verb, above the contradiction sweep, and its upper bound in the
    # per-row sweep below it. That asymmetry is what D038 recorded as "for
    # `audio` even by which bound was crossed", and probing one bound would miss
    # the half that moves.
    list(id = "audio-low/argument", verb = "compare_videos_batch",
         wins = resize2, other = range,
         args = list(jobs = cmp1x(), audio = -1, resize = TRUE),
         control = list(jobs = cmp1x(), audio = 0, resize = TRUE)),
    list(id = "audio-low/column", verb = "compare_videos_batch",
         wins = resize2, other = range,
         args = list(jobs = cmp2x(audio = c(-1, -1)), resize = TRUE),
         control = list(jobs = cmp2x(audio = c(0, 0)), resize = TRUE)),
    list(id = "audio-high/argument", verb = "compare_videos_batch",
         wins = resize2, other = range,
         args = list(jobs = cmp1x(), audio = 7, resize = TRUE),
         control = list(jobs = cmp1x(), audio = 0, resize = TRUE)),
    list(id = "audio-high/column", verb = "compare_videos_batch",
         wins = resize2, other = range,
         args = list(jobs = cmp2x(audio = c(7, 7)), resize = TRUE),
         control = list(jobs = cmp2x(audio = c(0, 0)), resize = TRUE)),

    # picture_in_picture_batch()'s `audio` reaches its verb's only
    # contradiction in the column form ONLY -- see the test below for the
    # argument form, which cannot. Row 1 drops audio (`NA`) and so contradicts
    # the encoder; row 2 carries an index past the two fixed roles.
    list(id = "audio/column", verb = "picture_in_picture_batch",
         wins = no_audio, other = range,
         args = list(jobs = pip2(audio = c(NA, 9)), audio_codec = "aac"),
         control = list(jobs = pip2(audio = c(NA, 0)), audio_codec = "aac"))
  )
}

test_that("a contradiction reports before a value error, in both forms", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  input <- make_input()
  for (case in ordering_cases(input)) expect_reports(case)
})

test_that("pip's `audio` argument has no contradiction to be ordered against", {
  # The one cell of the grid that does not exist, asserted rather than left as
  # a silent gap. picture_in_picture_batch() carries exactly one contradiction
  # -- an `audio_codec` naming an encoder with no audio carried -- and an
  # `audio` ARGUMENT applies to every row, so supplying one at all is what
  # removes the contradiction. There is therefore no call that is wrong in both
  # this value and this contradiction, and the value is what reports.
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  input <- make_input()
  jobs <- tibble::tibble(main = input, overlay = input, output = "o.mp4")

  cnd <- catch_call("picture_in_picture_batch",
                    list(jobs = jobs, audio = 9, audio_codec = "aac"))
  expect_s3_class(cnd, "rlang_error")
  expect_match(conditionMessage(cnd), "must be a whole number")
  expect_no_match(conditionMessage(cnd), "needs an audio stream to encode")
  expect_identical(blamed_verb(cnd), "picture_in_picture_batch")

  # And the reason, measured rather than asserted from the code: the same call
  # with the index IN range compiles, so the `audio_codec` had no contradiction
  # to raise in the first place.
  ok <- catch_call("picture_in_picture_batch",
                   list(jobs = jobs, audio = 1, audio_codec = "aac"))
  expect_false(inherits(ok, "condition"))
  expect_identical(nrow(ok), 1L)
})

# --- AC3: the availability check still reports LAST ---------------------------

test_that("a value check still reports before nvenc availability, in both forms", {
  # M59's AC5(b), re-run over the moved guards: moving a check DOWNWARD could
  # have put it behind the availability probe, which is the failure this pins.
  # The seam is held EMPTY so the control's availability abort is real rather
  # than assumed, which is what makes the claim machine-independent (M54/D035).
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  input <- make_input()
  two <- function(...) tibble::tibble(...)
  gpu <- list(hardware = "nvenc", video_codec = "libx264")
  cmp <- function(...) two(inputs = list(c(input, input)), output = "o.mp4", ...)
  pip <- function(...) two(main = input, overlay = input, output = "o.mp4", ...)

  cases <- list(
    list(id = "direction/argument", verb = "compare_videos_batch",
         wins = "must be one of", other = "nvenc",
         args = c(list(jobs = cmp(), direction = "sideways"), gpu),
         control = c(list(jobs = cmp(), direction = "vertical"), gpu)),
    list(id = "direction/column", verb = "compare_videos_batch",
         wins = "must be one of", other = "nvenc",
         args = c(list(jobs = cmp(direction = "sideways")), gpu),
         control = c(list(jobs = cmp(direction = "vertical")), gpu)),
    list(id = "audio/argument", verb = "compare_videos_batch",
         wins = "must be a whole number", other = "nvenc",
         args = c(list(jobs = cmp(), audio = 7), gpu),
         control = c(list(jobs = cmp(), audio = 0), gpu)),
    list(id = "audio/column", verb = "compare_videos_batch",
         wins = "must be a whole number", other = "nvenc",
         args = c(list(jobs = cmp(audio = 7)), gpu),
         control = c(list(jobs = cmp(audio = 0)), gpu)),
    list(id = "position/argument", verb = "picture_in_picture_batch",
         wins = "must be one of", other = "nvenc",
         args = c(list(jobs = pip(), position = "middleish"), gpu),
         control = c(list(jobs = pip(), position = "center"), gpu)),
    list(id = "position/column", verb = "picture_in_picture_batch",
         wins = "must be one of", other = "nvenc",
         args = c(list(jobs = pip(position = "middleish")), gpu),
         control = c(list(jobs = pip(position = "center")), gpu)),
    list(id = "margin/argument", verb = "picture_in_picture_batch",
         wins = "must be a whole number", other = "nvenc",
         args = c(list(jobs = pip(), margin = -3), gpu),
         control = c(list(jobs = pip(), margin = 16), gpu)),
    list(id = "margin/column", verb = "picture_in_picture_batch",
         wins = "must be a whole number", other = "nvenc",
         args = c(list(jobs = pip(margin = -3)), gpu),
         control = c(list(jobs = pip(margin = 16)), gpu)),
    # The new site: before this milestone pip's `audio` COLUMN had no
    # front-door guard, so this cell reported the encoder.
    list(id = "pip audio/argument", verb = "picture_in_picture_batch",
         wins = "must be a whole number", other = "nvenc",
         args = c(list(jobs = pip(), audio = 9), gpu),
         control = c(list(jobs = pip(), audio = 1), gpu)),
    list(id = "pip audio/column", verb = "picture_in_picture_batch",
         wins = "must be a whole number", other = "nvenc",
         args = c(list(jobs = pip(audio = 9)), gpu),
         control = c(list(jobs = pip(audio = 1)), gpu))
  )
  for (case in cases) {
    # `wins` and `other` are the other way round here: the VALUE wins and the
    # availability error is what must not appear, so the control must raise
    # `other` rather than `wins`.
    ctl <- catch_call(case$verb, case$control)
    expect_s3_class(ctl, "rlang_error")
    expect_match(conditionMessage(ctl), case$other, info = paste(case$id, "control"))

    cnd <- catch_call(case$verb, case$args)
    expect_s3_class(cnd, "rlang_error")
    expect_match(conditionMessage(cnd), case$wins, info = case$id)
    expect_no_match(conditionMessage(cnd), case$other, info = case$id)
    expect_identical(blamed_verb(cnd), case$verb, info = case$id)
  }
})

# --- AC4: pip's per-row `audio` index is checked at the front door -------------

test_that("picture_in_picture_batch's `audio` index is refused at its front door", {
  # Before this milestone the index was re-checked only inside the fan-out
  # closure, where the abort's `call` resolves to an anonymous function: the
  # user was shown "Error in `purrr::pmap(jobs, .f, ...)` / In index: 1", and
  # the message named the closure's local `aud` rather than the argument (M59
  # review F7). Both `parallel` settings, for parity with M59's suite -- and
  # because at `parallel = TRUE` the leaked name would be `furrr::future_pmap`,
  # which no other assertion here would see.
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  input <- make_input()
  two <- function(...) tibble::tibble(...)
  cases <- list(
    list(id = "argument", jobs = two(main = input, overlay = input,
                                     output = "o.mp4"), extra = list(audio = 9)),
    list(id = "column", jobs = two(main = input, overlay = input,
                                   output = "o.mp4", audio = 9),
         extra = list()),
    list(id = "mixed column", jobs = two(main = c(input, input),
                                         overlay = c(input, input),
                                         output = c("a.mp4", "b.mp4"),
                                         audio = c(0, 9)),
         extra = list())
  )
  for (parallel in c(FALSE, TRUE)) {
    for (case in cases) {
      id <- paste0(case$id, " parallel=", parallel)
      cnd <- catch_call("picture_in_picture_batch",
                        c(list(jobs = case$jobs, parallel = parallel),
                          case$extra))
      expect_s3_class(cnd, "rlang_error")
      expect_match(conditionMessage(cnd), "`audio` must be a whole number",
                   info = id)
      expect_identical(blamed_verb(cnd), "picture_in_picture_batch", info = id)
      # `pmap` covers purrr::pmap and furrr::future_pmap alike; `aud` is the
      # retired closure local, matched with word boundaries so `audio` does not
      # satisfy it.
      expect_no_match(conditionMessage(cnd), "pmap", fixed = TRUE, info = id)
      expect_no_match(conditionMessage(cnd), "In index:", fixed = TRUE, info = id)
      expect_no_match(conditionMessage(cnd), "\\baud\\b", info = id)
      deparsed <- paste(deparse(conditionCall(cnd)), collapse = "")
      expect_no_match(deparsed, "pmap", fixed = TRUE, info = id)
    }
  }
})

test_that("a clean `audio` column still compiles every row", {
  # The other half of the mixed form: a guard that refuses every table would
  # pass every assertion above.
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  input <- make_input()
  jobs <- tibble::tibble(main = c(input, input), overlay = c(input, input),
                         output = c("a.mp4", "b.mp4"), audio = c(0, 1))
  out <- picture_in_picture_batch(jobs, run = FALSE)
  expect_identical(nrow(out), 2L)
  expect_true(all(nzchar(out$command)))
})
