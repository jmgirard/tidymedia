# A value error and a contradiction resolve the same way in both forms (M61).
#
# M58 put six argument contradictions at these verbs' front doors and D036 gave
# them precedence: where a verb carries both a contradiction and a value error,
# the contradiction reports, because a contradiction is decided identically on
# every machine. M59 then swept the per-row VALUE checks to the front door,
# below that contradiction sweep -- so a value error arriving in a `jobs` column
# obeyed D036, while the same value passed as a scalar ARGUMENT was caught by a
# guard at the top of the verb and reported instead. D038 recorded that
# disagreement without defending it; D039 removes it. This suite is what D039
# rests on: the four guards now sit below the contradiction sweep, so both forms
# answer alike.
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
  # contradiction an IN-RANGE or out-of-range `audio` can cross: supplying such
  # a value is what removes the audio_codec one. An NA-ish `audio` is the
  # exception, and the `audio-na/argument` case below crosses exactly that.
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
    # compare_videos_batch() carries TWO contradictions, and a guard is crossed
    # with each rather than with whichever came to hand. The `resize` one is
    # live on three inputs, independently of `direction`.
    list(id = "direction-resize/argument", verb = "compare_videos_batch",
         wins = resize2, other = vocab,
         args = list(jobs = cmp1x(), direction = "sideways", resize = TRUE),
         control = list(jobs = cmp1x(), direction = "vertical",
                        resize = TRUE)),
    list(id = "direction-resize/column", verb = "compare_videos_batch",
         wins = resize2, other = vocab,
         args = list(jobs = cmp2x(direction = c("sideways", "sideways")),
                     resize = TRUE),
         control = list(jobs = cmp2x(direction = c("vertical", "vertical")),
                        resize = TRUE)),

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
    # contradiction in the column form because rows may disagree: row 1 drops
    # audio (`NA`) and so contradicts the encoder, row 2 carries an index past
    # the two fixed roles.
    list(id = "audio/column", verb = "picture_in_picture_batch",
         wins = no_audio, other = range,
         args = list(jobs = pip2(audio = c(NA, 9)), audio_codec = "aac"),
         control = list(jobs = pip2(audio = c(NA, 0)), audio_codec = "aac")),

    # And it reaches the same contradiction in the ARGUMENT form at exactly one
    # value. An in-range index removes the contradiction, and so does 9 -- but
    # `NA` does not: batch_stream_cell() resolves it to `NULL`, which drops the
    # audio the encoder needs while still being a value the argument guard
    # refuses. An earlier draft of this milestone recorded this cell as one
    # that could not exist; M61's review measured otherwise.
    #
    # The CONTROL is `audio = NULL`, not an in-range index, and it has to be:
    # an in-range index is what REMOVES the contradiction, so it would prove
    # nothing. `NULL` is the value `NA` resolves to, and the argument guard
    # accepts it, leaving the contradiction as the only live error.
    list(id = "audio/argument", verb = "picture_in_picture_batch",
         wins = no_audio, other = range,
         args = list(jobs = pip1(), audio = NA, audio_codec = "aac"),
         control = list(jobs = pip1(), audio = NULL, audio_codec = "aac")),
    list(id = "audio-na/argument", verb = "compare_videos_batch",
         wins = no_audio, other = range,
         args = list(jobs = cmp1(), audio = NA, audio_codec = "aac"),
         control = list(jobs = cmp1(), audio = NULL, audio_codec = "aac")),
    # `NaN` too, and it is not a curiosity: batch_stream_cell() tests is.na(),
    # and is.na(NaN) is TRUE, so the reachable set is every length-1 NA-ish
    # value. Naming only `NA` was this milestone's SECOND over-generalization
    # of the same shape -- reasoning from one value to a universal -- caught a
    # round after the first.
    list(id = "audio-nan/argument", verb = "picture_in_picture_batch",
         wins = no_audio, other = range,
         args = list(jobs = pip1(), audio = NaN, audio_codec = "aac"),
         control = list(jobs = pip1(), audio = NULL, audio_codec = "aac")),

    # The SCALAR verbs. Neither has a vocabulary guard of its own -- the shared
    # *_pipeline() is the only one -- so moving that check below the pipeline's
    # contradiction checkers moved these verbs' answer too. The milestone's
    # scope carves their front doors out only "beyond their shared pipeline",
    # so this is intended; what it was not, until M61's review, was covered.
    # There is no column form: these verbs take arguments and no `jobs` table.
    list(id = "direction/scalar-verb", verb = "compare_videos",
         wins = no_audio, other = vocab,
         args = list(infiles = c(input, input), outfile = "o.mp4",
                     direction = "sideways", audio_codec = "aac"),
         control = list(infiles = c(input, input), outfile = "o.mp4",
                        direction = "vertical", audio_codec = "aac")),
    list(id = "direction-resize/scalar-verb", verb = "compare_videos",
         wins = resize2, other = vocab,
         args = list(infiles = rep(input, 3), outfile = "o.mp4",
                     direction = "sideways", resize = TRUE),
         control = list(infiles = rep(input, 3), outfile = "o.mp4",
                        direction = "vertical", resize = TRUE)),
    list(id = "position/scalar-verb", verb = "picture_in_picture",
         wins = no_audio, other = vocab,
         args = list(main = input, overlay = input, outfile = "o.mp4",
                     position = "middleish", audio_codec = "aac"),
         control = list(main = input, overlay = input, outfile = "o.mp4",
                        position = "center", audio_codec = "aac"))
  )
}

test_that("a contradiction reports before a value error, in both forms", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  input <- make_input()
  for (case in ordering_cases(input)) expect_reports(case)
})

test_that("only `NA` reaches pip's contradiction through the `audio` argument", {
  # The reachability condition AC1 names, pinned as its own test rather than
  # left implicit in the case list. `audio` is the one guard here whose value
  # decides whether the contradiction exists at all, so the three outcomes are
  # asserted together: `NA` resolves to `NULL` and reaches it, an out-of-range
  # index does NOT (it carries audio, so the encoder has something to encode),
  # and an in-range index compiles.
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  input <- make_input()
  jobs <- tibble::tibble(main = input, overlay = input, output = "o.mp4")

  na_cell <- catch_call("picture_in_picture_batch",
                        list(jobs = jobs, audio = NA, audio_codec = "aac"))
  expect_s3_class(na_cell, "rlang_error")
  expect_match(conditionMessage(na_cell), "needs an audio stream to encode")
  expect_identical(blamed_verb(na_cell), "picture_in_picture_batch")

  # Out of range, and therefore NOT a cell of the contradiction pairing: the
  # index carries audio, so there is no contradiction and the value reports.
  # This is the fact an earlier draft over-generalized into "supplying `audio`
  # at all removes the contradiction", which is false at `NA`.
  out_of_range <- catch_call("picture_in_picture_batch",
                             list(jobs = jobs, audio = 9, audio_codec = "aac"))
  expect_s3_class(out_of_range, "rlang_error")
  expect_match(conditionMessage(out_of_range), "must be a whole number")
  expect_no_match(conditionMessage(out_of_range),
                  "needs an audio stream to encode")

  # In range: compiles, which is what makes the line above a statement about
  # the contradiction rather than about the guard.
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

# --- AC6: the sentence the docs and the changelog pin -------------------------

# The claim these verbs make to the user, in one wording, in three places. It is
# pinned rather than paraphrased because it is quantified twice -- over the four
# values and over the two forms -- and a rewrite that widens either quantifier
# would claim behavior no cell measures. The enumeration test below is what
# stops the sentence outrunning the evidence.
ordering_sentence <- paste(
  "A value error and a contradiction resolve the same way whether the value",
  "arrived as an argument or in a jobs column; the contradiction reports first."
)

# Both sources wrap the sentence across lines and mark up `jobs` differently --
# \code{jobs} in the Rd, `jobs` in the changelog -- so the comparison is made on
# normalized text. Markup, not wording, is what is normalized away: a changed
# word still fails.
normalize_prose <- function(txt) {
  txt <- gsub("\\\\code\\{([^}]*)\\}", "\\1", txt)
  txt <- gsub("`", "", txt, fixed = TRUE)
  txt <- gsub("[[:space:]]+", " ", txt)
  txt
}

ordering_topics <- c("compare_videos_batch", "picture_in_picture_batch")

test_that("both `_batch` verbs' help states the ordering sentence", {
  rd <- rd_sources()
  skip_if(is.null(rd), "no Rd source available")
  carrying <- sub("\\.Rd$", "", names(rd)[
    grepl(ordering_sentence, normalize_prose(rd), fixed = TRUE)])
  # Both directions at once: every topic that should carry it does, and no
  # other topic claims it. A verb whose front door was never reordered must not
  # tell its user that it was.
  expect_identical(sort(carrying), sort(ordering_topics))
})

test_that("the changelog states the ordering sentence", {
  news <- if (file.exists("../../NEWS.md")) "../../NEWS.md" else
    system.file("NEWS.md", package = "tidymedia")
  skip_if(!nzchar(news) || !file.exists(news), "no NEWS.md available")
  txt <- normalize_prose(paste(readLines(news, warn = FALSE), collapse = "\n"))
  expect_match(txt, ordering_sentence, fixed = TRUE)
})

test_that("each term the sentence quantifies over has a cell", {
  # The sentence says "a value error" and "either form". This is the test that
  # keeps those two quantifiers honest: every (verb, value) pair must have an
  # ordering case in each form it can reach -- so widening the sentence later
  # without widening the evidence fails here rather than at a reader.
  ids <- vapply(ordering_cases(make_input()), function(case) {
    paste(case$verb, case$id)
  }, character(1))
  # "compare_videos_batch audio-low/argument" -> verb + value + form, with the
  # bound suffix dropped: both bounds of one value are the same term.
  key <- sub("-(low|high|na|nan|resize)", "", sub("/.*$", "", ids))
  form <- sub("^.*/", "", ids)
  present <- lapply(split(form, key), function(x) sort(unique(x)))

  expect_setequal(names(present), c(
    "compare_videos_batch direction", "compare_videos_batch audio",
    "picture_in_picture_batch position", "picture_in_picture_batch margin",
    "picture_in_picture_batch audio",
    "compare_videos direction", "picture_in_picture position"
  ))
  both <- c("argument", "column")
  # Every `_batch` pair carries both forms. `audio` reaches its contradiction
  # in the argument form only at `NA`, which is why the case list carries an
  # `audio-na` id -- the key strips the bound suffix, so both bounds and the
  # `NA` cell count as the one term the sentence quantifies over.
  expect_identical(present[["compare_videos_batch direction"]], both)
  expect_identical(present[["compare_videos_batch audio"]], both)
  expect_identical(present[["picture_in_picture_batch position"]], both)
  expect_identical(present[["picture_in_picture_batch margin"]], both)
  expect_identical(present[["picture_in_picture_batch audio"]], both)
  # The two scalar verbs have an argument form and no other: they take no
  # `jobs` table, so "either form" has only one member for them.
  expect_identical(present[["compare_videos direction"]], "scalar-verb")
  expect_identical(present[["picture_in_picture position"]], "scalar-verb")
})
