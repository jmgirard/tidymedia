# The six per-row VALUE checks, refused at the front door of every verb that
# fans out (M59).
#
# Where M58's six were contradictions between two values a verb already held,
# these six are range, shape and vocabulary checks on ONE held value: a
# non-positive crop dimension, a negative inset margin, a malformed regions
# table, an audio index past the row's own input count, and the two enumerated
# vocabularies.
#
# Each used to abort inside a `*_pipeline()` function, a shared helper it
# reaches, or the fan-out closure itself. On a verb that fans out through
# ffm_batch() -> purrr::pmap() the abort's `call` resolves to an anonymous
# closure, so the user was shown "Error in `purrr::pmap(jobs, .f, ...)` /
# In index: 1" -- and at `parallel = TRUE`, `furrr::future_pmap(...)` instead
# (LESSONS M47/M48-F1). The abort now fires at the front door, before anything
# is built.
#
# Nothing here needs FFmpeg: every probe runs at `run = FALSE`. The encoder seam
# is held EMPTY wherever a call is expected to abort, so a message mentioning
# availability is a failure rather than a coincidence.

blamed_verb <- function(cnd) {
  cl <- conditionCall(cnd)
  if (is.null(cl)) return(NA_character_)
  paste(deparse(cl[[1]]), collapse = "")
}

catch_call <- function(verb, args) {
  if (is.null(args$run)) args$run <- FALSE
  tryCatch(
    do.call(verb, args, envir = asNamespace("tidymedia")),
    error = function(e) e
  )
}

# --- AC1: one blame test per (site, verb) pair, at both `parallel` settings ---
#
# `args` is a call that violates the named check and nothing else; `own` is a
# fragment of that check's own message, asserted so a pair that starts failing
# for an unrelated reason (a schema error, a missing column) records that
# instead of passing on a bare abort.
#
# Both `parallel` settings, but be honest about what that buys NOW. Before this
# milestone the two fan-out backends named themselves differently in the leaked
# message -- `purrr::pmap` sequentially, `furrr::future_pmap` in parallel -- so
# testing one setting could leave the other's name reaching the user. Now that
# every one of these checks aborts BEFORE `ffm_batch()` is reached, neither
# backend is engaged and the two iterations run the same code (M59 review F12).
# The loop is kept as a regression pin rather than as doubled evidence: it is
# what fails if a future change lets one of these values reach the fan-out
# again, and at `parallel = TRUE` the name it would leak is the one no test
# below this line would otherwise see.

value_check_pairs <- function(input) {
  two <- function(...) tibble::tibble(...)
  list(
    list(id = "1/crop_video_batch/width", verb = "crop_video_batch",
         own = "must be a single FFmpeg expression or number",
         args = list(jobs = two(input = input, output = "o.mp4"),
                     width = 0, height = 120)),
    list(id = "1/crop_video_batch/height column",
         verb = "crop_video_batch",
         own = "must be a single FFmpeg expression or number",
         args = list(jobs = two(input = input, output = "o.mp4", height = -1),
                     width = 160)),
    list(id = "2/picture_in_picture_batch/margin",
         verb = "picture_in_picture_batch",
         own = "must be a whole number",
         args = list(jobs = two(main = input, overlay = input,
                                output = "o.mp4", margin = -3))),
    list(id = "3/anonymize_video_batch/regions",
         verb = "anonymize_video_batch",
         own = "missing 1 required column",
         args = list(jobs = two(input = input, output = "o.mp4",
                                regions = list(
                                  data.frame(x = 0, y = 0, width = 10))))),
    list(id = "4/compare_videos_batch/audio index",
         verb = "compare_videos_batch",
         own = "must be a whole number",
         args = list(jobs = two(inputs = list(c(input, input)),
                                output = "o.mp4", audio = 7))),
    list(id = "5/compare_videos_batch/direction",
         verb = "compare_videos_batch",
         own = "must be one of",
         args = list(jobs = two(inputs = list(c(input, input)),
                                output = "o.mp4", direction = "sideways"))),
    list(id = "6/picture_in_picture_batch/position",
         verb = "picture_in_picture_batch",
         own = "must be one of",
         args = list(jobs = two(main = input, overlay = input,
                                output = "o.mp4", position = "middleish")))
  )
}

test_that("every (value check, verb) pair blames the verb the user called", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  input <- make_input()
  for (parallel in c(FALSE, TRUE)) {
    for (pair in value_check_pairs(input)) {
      id <- paste0(pair$id, " parallel=", parallel)
      cnd <- catch_call(pair$verb, c(pair$args, list(parallel = parallel)))
      expect_s3_class(cnd, "rlang_error")
      expect_match(conditionMessage(cnd), pair$own, info = id)
      expect_identical(blamed_verb(cnd), pair$verb, info = id)
      # `pmap` covers purrr::pmap and furrr::future_pmap alike.
      expect_no_match(conditionMessage(cnd), "pmap", fixed = TRUE, info = id)
      expect_no_match(conditionMessage(cnd), "In index:", fixed = TRUE,
                      info = id)
      expect_no_match(conditionMessage(cnd), "_pipeline(", fixed = TRUE,
                      info = id)
      # The blame lives in conditionCall(), which is what the console prints as
      # "Error in `<verb>()`"; deparsing the whole call also catches a
      # dependency's or an internal's name leaking there.
      deparsed <- paste(deparse(conditionCall(cnd)), collapse = "")
      expect_no_match(deparsed, "pmap", fixed = TRUE, info = id)
      expect_no_match(deparsed, "_pipeline(", fixed = TRUE, info = id)
    }
  }
})

# --- AC2: one vocabulary literal, one refusal site ---------------------------
#
# Sites 5 and 6 could each have been swept by writing its list out again at the
# front door. Each list is instead written once and every signature defaults to
# it, so this fails on a fourth copy the way M58's headline test fails on a
# second copy of a message.

#
# Read from the loaded NAMESPACE, never from `../../R`. The first version of
# this test globbed the source directory, which exists under `devtools::test()`
# and does not under `R CMD check` — there the tests run in
# `<pkg>.Rcheck/tests/testthat/` and `../../R` holds only the lazy-load
# database, so the whole test skipped in exactly the run the AC6 gate uses
# (M59 review F10). It also grepped literal source text, so a fourth copy
# written with single quotes, different spacing, or wrapped across lines would
# have left it green (F11). Deparsing normalizes all three.

vocabularies <- function() {
  list(
    list(arg = "direction", accessor = "stack_directions",
         values = c("horizontal", "vertical")),
    list(arg = "position", accessor = "pip_positions",
         values = c("topright", "topleft", "bottomright", "bottomleft",
                    "center"))
  )
}

namespace_functions <- function() {
  ns <- asNamespace("tidymedia")
  Filter(is.function,
         mget(ls(ns, all.names = TRUE), envir = ns, ifnotfound = list(NULL)))
}

test_that("each enumerated vocabulary is written in exactly one function body", {
  fns <- namespace_functions()
  expect_gt(length(fns), 0)
  for (v in vocabularies()) {
    # width.cutoff on BOTH sides, or the five-element vocabulary deparses onto
    # two lines here and one line in the body, and the comparison fails on the
    # rejoin rather than on the invariant.
    literal <- paste(deparse(v$values, width.cutoff = 500L), collapse = "")
    holders <- names(Filter(function(f) {
      grepl(literal,
            paste(deparse(body(f), width.cutoff = 500L), collapse = " "),
            fixed = TRUE)
    }, fns))
    # Exactly one function's body spells the vocabulary out, and it is the
    # accessor. Naming it rather than only counting means a test that passes
    # because the literal moved somewhere else still fails.
    expect_identical(holders, v$accessor, info = v$arg)
    expect_identical(do.call(v$accessor, list(), envir = asNamespace("tidymedia")),
                     v$values, info = v$arg)
  }
})

test_that("every signature taking a vocabulary agrees with the accessor", {
  # The copies that remain are deliberate, so the invariant is AGREEMENT rather
  # than uniqueness. The four EXPORTED signatures spell their vocabulary out, so
  # `?compare_videos` shows the values and `formals()` returns something a
  # caller can read and evaluate (M59 review N8); the two internal pipelines
  # default to the accessor. What must never drift is the values themselves, so
  # every default is EVALUATED and compared to the accessor's answer -- which is
  # the actual M40 failure mode, and a check no spelling-based test can make.
  #
  # Three signatures per vocabulary (scalar verb, _batch sibling, shared
  # pipeline); the count is asserted so a NEW signature taking one of these
  # arguments cannot join without this test noticing.
  ns <- asNamespace("tidymedia")
  fns <- namespace_functions()
  exported <- getNamespaceExports("tidymedia")
  for (v in vocabularies()) {
    takers <- Filter(function(f) v$arg %in% names(formals(f)), fns)
    expect_identical(length(takers), 3L, info = v$arg)
    for (nm in names(takers)) {
      default <- formals(takers[[nm]])[[v$arg]]
      expect_identical(eval(default, envir = ns), v$values,
                       info = paste(v$arg, nm))
      if (nm %in% exported) {
        # Self-contained: it must evaluate with no access to this package, or
        # the Usage line names something the reader cannot resolve and
        # `formals()` hands back an unevaluatable call (N8).
        expect_identical(eval(default, envir = baseenv()), v$values,
                         info = paste(v$arg, nm, "exported"))
      }
    }
  }
})

# --- AC2: the shared vocabulary checker keeps arg_match()'s whole contract ---
#
# M59's review (F1/F2) caught check_vocab_arg() reaching past rlang::arg_match()
# to arg_match0(), which takes a STRING: on any longer value arg_match0()'s own
# length guard fired first and aborted with ITS call, ignoring `error_call`. A
# user passing `position = c("center", "topleft")` was shown
# "`arg` must be a string or have the same length as `values`" blamed on
# `rlang::arg_match0(value, values, arg_nm = arg, error_call = call)` -- a
# WORSE blame than the master this milestone set out to improve on.
#
# Each case below states master's answer, so the assertion is parity with what
# these verbs did before M59 rather than a fresh opinion about what they should
# do. The two vocabularies are covered separately because the defect only
# surfaced on `position`: a two-element value happens to match the two-element
# `direction` vocabulary's length, so it slipped past arg_match0()'s length
# guard and failed later, with the frame intact but the wrong argument named.

test_that("a multi-element vocabulary value is refused as rlang::arg_match() refuses it", {
  input <- make_input()
  cases <- list(
    # A value that is not a permutation: refused, naming the ARGUMENT and the
    # verb, never the checker or its formals.
    list(id = "position 2-of-5, scalar verb", verb = "picture_in_picture",
         own = "`position` must be one of", blame = "picture_in_picture",
         args = list(main = input, overlay = input, outfile = "o.mp4",
                     position = c("center", "topleft"))),
    list(id = "position 2-of-5, batch verb", verb = "picture_in_picture_batch",
         own = "`position` must be one of", blame = "picture_in_picture_batch",
         args = list(jobs = tibble::tibble(main = input, overlay = input,
                                           output = "o.mp4"),
                     position = c("center", "topleft"))),
    list(id = "direction 2 non-permutation, scalar verb", verb = "compare_videos",
         own = "`direction` must be one of", blame = "compare_videos",
         args = list(infiles = c(input, input), outfile = "o.mp4",
                     direction = c("sideways", "up"))),
    list(id = "direction 2 non-permutation, batch verb",
         verb = "compare_videos_batch",
         own = "`direction` must be one of", blame = "compare_videos_batch",
         args = list(jobs = tibble::tibble(inputs = list(c(input, input)),
                                           output = "o.mp4"),
                     direction = c("sideways", "up"))),
    # Zero length is arg_match()'s own third branch, and its message differs
    # from the out-of-vocabulary one; pinned so a hand-rolled replacement that
    # collapses the branches is caught.
    list(id = "direction zero-length", verb = "compare_videos",
         own = "must be length 1", blame = "compare_videos",
         args = list(infiles = c(input, input), outfile = "o.mp4",
                     direction = character(0)))
  )
  for (case in cases) {
    cnd <- catch_call(case$verb, case$args)
    expect_s3_class(cnd, "rlang_error")
    expect_match(conditionMessage(cnd), case$own, fixed = TRUE, info = case$id)
    expect_identical(blamed_verb(cnd), case$blame, info = case$id)
    # The checker and rlang's string-only entry point are both internals;
    # neither may reach the user, in the message or in the blamed call.
    expect_no_match(conditionMessage(cnd), "arg_match", fixed = TRUE,
                    info = case$id)
    expect_no_match(blamed_verb(cnd), "arg_match", fixed = TRUE, info = case$id)
    expect_no_match(conditionMessage(cnd), "check_vocab_arg", fixed = TRUE,
                    info = case$id)
  }
})

test_that("a reordered vocabulary default still selects its own first element", {
  # arg_match()'s permutation branch: passing the vocabulary in another order is
  # how a caller re-defaults it, and the FIRST element of what was passed wins
  # (not the first of the vocabulary). Compiling rather than aborting is the
  # property; the vertical stack proves which element was taken.
  input <- make_input()
  out <- compare_videos(c(input, input), "o.mp4",
                        direction = c("vertical", "horizontal"), run = FALSE)
  expect_true(any(grepl("vstack", out, fixed = TRUE)))
  expect_false(any(grepl("hstack", out, fixed = TRUE)))
})

# --- AC2: the scalar siblings still blame themselves -------------------------
#
# Every one of the six checks is also reachable from a scalar verb, and four of
# them (sites 1, 3, 5, 6) reach it through the very call the front door now
# duplicates -- check_dim(), check_regions(), and the two pipelines'
# check_vocab_arg(). Deleting that shared call is the mutation AC2 asks for, and
# this is the test it has to turn red.

test_that("the scalar siblings refuse the same values and blame themselves", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  input <- make_input()
  cases <- list(
    # `blame` is the scalar verb everywhere but site 1, where check_dim() is
    # reached through ffm_crop() and so names the BUILDER. That leak predates
    # M59 and is left standing by it: closing it means threading `call` through
    # an exported Layer-1 builder, which M59-D1 rejected. Recorded here rather
    # than glossed, so the test says what the user actually sees.
    list(id = "1/crop_video", verb = "crop_video", blame = "ffm_crop",
         own = "must be a single FFmpeg expression or number",
         args = list(infile = input, outfile = "o.mp4", width = 0,
                     height = 120)),
    list(id = "2/picture_in_picture", verb = "picture_in_picture",
         own = "must be a whole number",
         args = list(main = input, overlay = input, outfile = "o.mp4",
                     margin = -3)),
    list(id = "3/anonymize_video", verb = "anonymize_video",
         own = "missing 1 required column",
         args = list(infile = input, outfile = "o.mp4",
                     regions = data.frame(x = 0, y = 0, width = 10))),
    list(id = "4/compare_videos", verb = "compare_videos",
         own = "must be a whole number",
         args = list(infiles = c(input, input), outfile = "o.mp4", audio = 7)),
    list(id = "5/compare_videos", verb = "compare_videos",
         own = "must be one of",
         args = list(infiles = c(input, input), outfile = "o.mp4",
                     direction = "sideways")),
    list(id = "6/picture_in_picture", verb = "picture_in_picture",
         own = "must be one of",
         args = list(main = input, overlay = input, outfile = "o.mp4",
                     position = "middleish"))
  )
  for (case in cases) {
    cnd <- catch_call(case$verb, case$args)
    expect_s3_class(cnd, "rlang_error")
    expect_match(conditionMessage(cnd), case$own, info = case$id)
    blame <- if (is.null(case$blame)) case$verb else case$blame
    expect_identical(blamed_verb(cnd), blame, info = case$id)
    # No scalar verb leaks a `*_pipeline()` name, whichever frame it blames --
    # the leak M58 closed on the resize guard and M59 closes on the two
    # vocabularies.
    expect_no_match(blamed_verb(cnd), "_pipeline", fixed = TRUE, info = case$id)
  }
})

# --- AC1/AC3: one violating row is refused, a clean column compiles ----------
#
# The mixed form, which is what decides whether a guard sweeps rows or gates the
# whole table (the shape M57's review caught on segment_video_batch). Each case
# pairs a table with ONE violating row against a table of the same shape with
# none: the first must be refused naming the verb, the second must compile.
# Asserting only the first would pass for a guard that refuses every table.

value_check_columns <- function(input) {
  two <- function(...) tibble::tibble(...)
  good_regions <- data.frame(x = 0, y = 0, width = 10, height = 10)
  bad_regions <- data.frame(x = 0, y = 0, width = 10)
  list(
    list(id = "1/width column", verb = "crop_video_batch",
         own = "must be a single FFmpeg expression or number",
         base = list(height = 120),
         jobs_bad = two(input = c(input, input), output = c("a.mp4", "b.mp4"),
                        width = c(160, 0)),
         jobs_ok = two(input = c(input, input), output = c("a.mp4", "b.mp4"),
                       width = c(160, 160))),
    list(id = "2/margin column", verb = "picture_in_picture_batch",
         own = "must be a whole number",
         base = list(),
         jobs_bad = two(main = c(input, input), overlay = c(input, input),
                        output = c("a.mp4", "b.mp4"), margin = c(16, -3)),
         jobs_ok = two(main = c(input, input), overlay = c(input, input),
                       output = c("a.mp4", "b.mp4"), margin = c(16, 16))),
    list(id = "3/regions column", verb = "anonymize_video_batch",
         own = "missing 1 required column",
         base = list(),
         jobs_bad = two(input = c(input, input), output = c("a.mp4", "b.mp4"),
                        regions = list(good_regions, bad_regions)),
         jobs_ok = two(input = c(input, input), output = c("a.mp4", "b.mp4"),
                       regions = list(good_regions, good_regions))),
    list(id = "4/audio column", verb = "compare_videos_batch",
         own = "must be a whole number",
         base = list(),
         # Both rows carry two inputs, so 7 is out of range for the ROW rather
         # than for the argument -- the bound the scalar check cannot express.
         jobs_bad = two(inputs = list(c(input, input), c(input, input)),
                        output = c("a.mp4", "b.mp4"), audio = c(0, 7)),
         jobs_ok = two(inputs = list(c(input, input), c(input, input)),
                       output = c("a.mp4", "b.mp4"), audio = c(0, 1))),
    list(id = "4/per-row input count", verb = "compare_videos_batch",
         own = "must be a whole number",
         base = list(audio = 2, resize = FALSE),
         # The SAME index is legal on a three-input row and out of range on a
         # two-input one, which is what "against each row's own inputs" means.
         jobs_bad = two(inputs = list(rep(input, 3), c(input, input)),
                        output = c("a.mp4", "b.mp4")),
         jobs_ok = two(inputs = list(rep(input, 3), rep(input, 3)),
                       output = c("a.mp4", "b.mp4"))),
    list(id = "5/direction column", verb = "compare_videos_batch",
         own = "must be one of",
         base = list(),
         jobs_bad = two(inputs = list(c(input, input), c(input, input)),
                        output = c("a.mp4", "b.mp4"),
                        direction = c("vertical", "sideways")),
         jobs_ok = two(inputs = list(c(input, input), c(input, input)),
                       output = c("a.mp4", "b.mp4"),
                       direction = c("vertical", "horizontal"))),
    list(id = "6/position column", verb = "picture_in_picture_batch",
         own = "must be one of",
         base = list(),
         jobs_bad = two(main = c(input, input), overlay = c(input, input),
                        output = c("a.mp4", "b.mp4"),
                        position = c("center", "middleish")),
         jobs_ok = two(main = c(input, input), overlay = c(input, input),
                       output = c("a.mp4", "b.mp4"),
                       position = c("center", "topleft")))
  )
}

test_that("one violating row is refused and a clean column compiles", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  input <- make_input()
  for (case in value_check_columns(input)) {
    bad <- catch_call(case$verb, c(case$base, list(jobs = case$jobs_bad)))
    expect_s3_class(bad, "rlang_error")
    expect_match(conditionMessage(bad), case$own, info = case$id)
    expect_identical(blamed_verb(bad), case$verb, info = case$id)

    ok <- catch_call(case$verb, c(case$base, list(jobs = case$jobs_ok)))
    expect_false(inherits(ok, "condition"), info = case$id)
    expect_identical(nrow(ok), 2L, info = case$id)
    expect_true(all(nzchar(ok$command)), info = case$id)
  }
})

# --- AC5: the precedence the front door assigns ------------------------------
#
# A guard moved ahead of the fan-out reports ahead of everything the fan-out
# raised, not only the check the milestone set out to precede. That is M41's
# known cost, and the rule this repo applies to it is that it be tested rather
# than assumed away (D035's second condition).
#
# Each case is wrong in TWO ways. The control is the same call with the
# value-check violation removed, asserted to still raise the other error --
# without it a case would pass for a call that had only ever had one error, and
# the precedence claim would rest on nothing.

expect_precedence <- function(case) {
  ctl <- catch_call(case$verb, case$control)
  expect_s3_class(ctl, "rlang_error")
  expect_match(conditionMessage(ctl), case$other, info = case$id)

  cnd <- catch_call(case$verb, case$args)
  expect_s3_class(cnd, "rlang_error")
  expect_match(conditionMessage(cnd), case$wins, info = case$id)
  expect_no_match(conditionMessage(cnd), case$other, info = case$id)
  expect_identical(blamed_verb(cnd), case$verb, info = case$id)
}

test_that("a contradiction reports before a value check, in the column form", {
  # The two verbs carrying both an M58 contradiction and one of these six value
  # checks, with the bad value in a `jobs` COLUMN: the contradiction reports.
  # M59 no longer claims this ordering — its AC5(a) was re-cut out on 2026-08-07
  # after failing twice as an overbroad statement about the ARGUMENT form, which
  # answers the other way. M61 owns the ordering and makes both forms agree; the
  # column half is kept pinned here meanwhile because D036 still requires it.
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  input <- make_input()
  two <- function(...) tibble::tibble(...)
  cases <- list(
    list(id = "compare: contradiction over direction",
         verb = "compare_videos_batch",
         wins = "needs an audio stream to encode", other = "must be one of",
         args = list(jobs = two(inputs = list(c(input, input)),
                                output = "o.mp4", direction = "sideways"),
                     audio_codec = "aac"),
         control = list(jobs = two(inputs = list(c(input, input)),
                                   output = "o.mp4", direction = "sideways"),
                        audio_codec = "copy")),
    list(id = "pip: contradiction over margin",
         verb = "picture_in_picture_batch",
         wins = "needs an audio stream to encode",
         other = "must be a whole number",
         args = list(jobs = two(main = input, overlay = input,
                                output = "o.mp4", margin = -3),
                     audio_codec = "aac"),
         control = list(jobs = two(main = input, overlay = input,
                                   output = "o.mp4", margin = -3),
                        audio_codec = "copy"))
  )
  for (case in cases) expect_precedence(case)
})

test_that("AC5(a): a value check reports before nvenc availability", {
  # The seam is held EMPTY, so `hardware = "nvenc"` is genuinely unavailable and
  # the control's availability abort is real rather than assumed. Driving it
  # through the option seam is what makes this machine-independent (M54/D035).
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  input <- make_input()
  two <- function(...) tibble::tibble(...)
  good_regions <- data.frame(x = 0, y = 0, width = 10, height = 10)
  bad_regions <- data.frame(x = 0, y = 0, width = 10)
  cases <- list(
    list(id = "crop width", verb = "crop_video_batch",
         wins = "must be a single FFmpeg expression or number",
         other = "nvenc",
         args = list(jobs = two(input = input, output = "o.mp4"),
                     width = 0, height = 120, hardware = "nvenc",
                     video_codec = "libx264"),
         control = list(jobs = two(input = input, output = "o.mp4"),
                        width = 160, height = 120, hardware = "nvenc",
                        video_codec = "libx264")),
    list(id = "anonymize regions", verb = "anonymize_video_batch",
         wins = "missing 1 required column", other = "nvenc",
         args = list(jobs = two(input = input, output = "o.mp4",
                                regions = list(bad_regions)),
                     hardware = "nvenc"),
         control = list(jobs = two(input = input, output = "o.mp4",
                                   regions = list(good_regions)),
                        hardware = "nvenc")),
    list(id = "compare direction", verb = "compare_videos_batch",
         wins = "must be one of", other = "nvenc",
         args = list(jobs = two(inputs = list(c(input, input)),
                                output = "o.mp4", direction = "sideways"),
                     hardware = "nvenc", video_codec = "libx264"),
         control = list(jobs = two(inputs = list(c(input, input)),
                                   output = "o.mp4", direction = "vertical"),
                        hardware = "nvenc", video_codec = "libx264")),
    list(id = "pip position", verb = "picture_in_picture_batch",
         wins = "must be one of", other = "nvenc",
         args = list(jobs = two(main = input, overlay = input,
                                output = "o.mp4", position = "middleish"),
                     hardware = "nvenc", video_codec = "libx264"),
         control = list(jobs = two(main = input, overlay = input,
                                   output = "o.mp4", position = "center"),
                        hardware = "nvenc", video_codec = "libx264"))
  )
  for (case in cases) expect_precedence(case)
})

test_that("AC5(b): a value check reports before ffm_batch's own guards", {
  # `run` is one of the six arguments ffm_batch() alone guards
  # (R/ffm_batch.R:84-98). Every one of these value checks now runs before
  # ffm_batch() is called at all, so a call wrong in both is told about the
  # value. The jobs-SHAPE guards at :75-80 are NOT in this set: all four verbs
  # already pre-empt them, so they are never displaced.
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  input <- make_input()
  two <- function(...) tibble::tibble(...)
  good_regions <- data.frame(x = 0, y = 0, width = 10, height = 10)
  bad_regions <- data.frame(x = 0, y = 0, width = 10)
  cases <- list(
    list(id = "crop width", verb = "crop_video_batch",
         wins = "must be a single FFmpeg expression or number",
         other = "`run` must be",
         args = list(jobs = two(input = input, output = "o.mp4"),
                     width = 0, height = 120, run = "yes"),
         control = list(jobs = two(input = input, output = "o.mp4"),
                        width = 160, height = 120, run = "yes")),
    list(id = "anonymize regions", verb = "anonymize_video_batch",
         wins = "missing 1 required column", other = "`run` must be",
         args = list(jobs = two(input = input, output = "o.mp4",
                                regions = list(bad_regions)),
                     run = "yes"),
         control = list(jobs = two(input = input, output = "o.mp4",
                                   regions = list(good_regions)),
                        run = "yes")),
    list(id = "compare audio index", verb = "compare_videos_batch",
         wins = "must be a whole number", other = "`run` must be",
         args = list(jobs = two(inputs = list(c(input, input)),
                                output = "o.mp4", audio = 7),
                     run = "yes"),
         control = list(jobs = two(inputs = list(c(input, input)),
                                   output = "o.mp4", audio = 0),
                        run = "yes")),
    list(id = "pip margin", verb = "picture_in_picture_batch",
         wins = "must be a whole number", other = "`run` must be",
         args = list(jobs = two(main = input, overlay = input,
                                output = "o.mp4", margin = -3),
                     run = "yes"),
         control = list(jobs = two(main = input, overlay = input,
                                   output = "o.mp4", margin = 16),
                        run = "yes"))
  )
  for (case in cases) expect_precedence(case)
})
