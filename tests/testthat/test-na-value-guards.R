# M080: the one-argument `check_*` value predicates refuse NA with a condition
# that names the argument, instead of crashing bare inside a comparison or
# compiling the NA into the command.

test_that("check_dim() refuses NA of every type, naming its argument", {
  # NA_real_ reached `x > 0`, whose NA made `if (!ok)` raise base R's
  # `missing value where TRUE/FALSE needed` with no argument in it (M64 F4);
  # NA_character_ satisfied is_character(n = 1) and passed straight through.
  for (na in list(NA, NA_integer_, NA_real_, NA_character_)) {
    err <- rlang::catch_cnd(check_dim(na, arg = "width"))
    expect_s3_class(err, "rlang_error")
    expect_match(conditionMessage(err), "`width`", fixed = TRUE)
  }
})

test_that("a scalar verb refuses an NA dimension instead of compiling it", {
  f <- withr::local_tempfile(fileext = ".mp4")
  file.create(f)
  o <- withr::local_tempfile(fileext = ".mp4")
  # This returned `-vf "crop=w=NA:h=100:..."` — a command FFmpeg would have
  # rejected at run time, from a call that succeeded at run = FALSE.
  err <- rlang::catch_cnd(
    crop_video(f, o, NA_character_, 100, run = FALSE)
  )
  expect_s3_class(err, "rlang_error")
  expect_match(conditionMessage(err), "`width`", fixed = TRUE)
})

test_that("the NA refusal is spelled the same way on both forms", {
  f <- withr::local_tempfile(fileext = ".mp4")
  file.create(f)
  o <- withr::local_tempfile(fileext = ".mp4")

  # Scalar form: the argument as the caller typed it, blamed on the verb.
  err <- rlang::catch_cnd(crop_video(f, o, NA_real_, 100, run = FALSE))
  expect_match(conditionMessage(err),
               "`width` must be a single FFmpeg expression or number.",
               fixed = TRUE)
  expect_match(paste(deparse(conditionCall(err)), collapse = " "),
               "crop_video(", fixed = TRUE)

  # `_batch` form: the COLUMN name. An NA CELL never reaches check_dim() —
  # crop_video_batch() types its dimension columns up front, and that guard
  # refuses NA first — so the blame the caller sees is the column guard's, and
  # it names the column rather than a row.
  err <- rlang::catch_cnd(crop_video_batch(
    tibble::tibble(input = c(f, f), width = c(100, NA_real_)),
    height = 100, run = FALSE))
  expect_s3_class(err, "rlang_error")
  expect_match(conditionMessage(err),
               "The width column of `jobs` must not contain NA.", fixed = TRUE)
  expect_match(paste(deparse(conditionCall(err)), collapse = " "),
               "crop_video_batch(", fixed = TRUE)

  # An NA delivered as the verb's own ARGUMENT does reach check_dim(), through
  # check_batch_cell() with no row locator: an argument applies to every row,
  # so naming one would mislead (M66).
  err <- rlang::catch_cnd(crop_video_batch(
    tibble::tibble(input = f), width = NA_real_, height = 100, run = FALSE))
  expect_match(conditionMessage(err),
               "`width` must be a single FFmpeg expression or number.",
               fixed = TRUE)
  expect_false(grepl("offending jobs row", conditionMessage(err), fixed = TRUE))
})

test_that("no one-argument check_ predicate signals a bare error on NA", {
  # The domain is enumerated out of the namespace by formals, not listed here:
  # a predicate added later joins the sweep with no edit. A bare simpleError
  # is base R's, raised from inside a front-door guard with neither the
  # argument's name nor the caller's frame in it.
  ns <- asNamespace("tidymedia")
  preds <- na_sweep_predicates()
  expect_gt(length(preds), 0)
  # The four this milestone reddened must be IN the domain, or the sweep runs
  # over a set that excludes what it was built to catch.
  expect_true(all(c("check_dim", "check_overlay_scale", "check_region_values",
                    "check_codec_needs_reencode") %in% preds))

  vals <- na_values()
  labs <- na_labels()
  for (nm in preds) {
    f <- get(nm, envir = ns)
    for (i in seq_along(vals)) {
      where <- paste(nm, labs[i])
      warned <- character()
      cnd <- withCallingHandlers(
        tryCatch({ f(vals[[i]]); NULL }, error = function(e) e),
        warning = function(w) {
          warned <<- c(warned, paste(class(w), collapse = "/"))
          invokeRestart("muffleWarning")
        })
      # A warning would let an error through unclassified, so it is a failure
      # in its own right rather than something to suppress.
      expect_identical(warned, character(0), info = where)
      # NA is a legal value for some of these (the codec sentinels), and that
      # is a pass: what must never happen is an error that is not the
      # package's own.
      if (is.null(cnd)) next
      expect_true(inherits(cnd, "rlang_error"),
                  info = paste(where, "::", conditionMessage(cnd)))
    }
  }
})

test_that("every verb reaching check_dim() refuses NA naming the carrier", {
  # The verb set is the walk's. The carriers are declared per verb in
  # helper-na-guards.R, and the reader below fails on a verb the walk returns
  # with no entry AND on an entry that omits a carrier the verb accepts, so a
  # declaration cannot quietly cover less than the verb does.
  dir <- withr::local_tempdir()
  p <- file.path(dir, "in.mp4")
  file.create(p)
  o <- file.path(dir, "out.mp4")
  verbs <- check_dim_verbs()
  specs <- check_dim_specs(p, o)
  expect_gt(length(verbs), 0)
  expect_identical(sort(setdiff(verbs, names(specs))), character(0))

  # Completeness: the carrier vocabulary is the union of what the entries
  # declare, and every verb whose formals carry one of those names must
  # declare it as an argument; every `jobs`-taking verb naming one as a column
  # literal in its own body must declare it as a column.
  vocab <- unique(unlist(lapply(specs, function(e)
    vapply(e, function(x) x$arg, character(1)))))
  ns <- asNamespace("tidymedia")
  for (verb in verbs) {
    declared <- function(via) vapply(
      Filter(function(x) identical(x$via, via), specs[[verb]]),
      function(x) x$arg, character(1))
    f <- get(verb, envir = ns)
    expect_identical(
      sort(setdiff(intersect(names(formals(f)), vocab), declared("argument"))),
      character(0), info = paste(verb, "argument carriers"))
    if ("jobs" %in% names(formals(f))) {
      body_txt <- paste(deparse(body(f)), collapse = " ")
      literals <- vocab[vapply(vocab, function(v)
        grepl(paste0('"', v, '"'), body_txt, fixed = TRUE), logical(1))]
      expect_identical(sort(setdiff(literals, declared("column"))),
                       character(0), info = paste(verb, "column carriers"))
    }
  }

  # The refusal itself, over every declared carrier and all four NA types.
  # Five wordings can reach an NA on these paths; anything else means a guard
  # moved.
  na_refusal <- "must be a single FFmpeg expression or number"
  column_na <- c("must not contain NA", "must be numeric (no NA)")
  column_type <- c("must be numeric or character", "must be numeric")
  rate_refusal <- c("must be a single positive number or a string",
                    "must be a single positive number")
  number_refusal <- c("must be a number", "must be a whole number")
  # A non-NA value of each type, for the type-guard control below.
  typed <- list(TRUE, 1L, 1, "1")
  vals <- na_values()
  labs <- na_labels()
  has <- function(msg, pats)
    any(vapply(pats, function(r) grepl(r, msg, fixed = TRUE), logical(1)))

  for (verb in verbs) {
    for (entry in specs[[verb]]) {
      for (i in seq_along(vals)) {
        where <- paste(verb, entry$arg, entry$via, labs[i])
        cnd <- tryCatch({ entry$call(vals[[i]]); NULL }, error = function(e) e)
        expect_false(is.null(cnd), info = paste(where, ":: no abort"))
        if (is.null(cnd)) next
        msg <- conditionMessage(cnd)
        expect_true(inherits(cnd, "rlang_error"), info = where)
        # The carrier is named, either as the argument the caller typed or as
        # the column that carried it.
        expect_true(
          grepl(paste0("`", entry$arg, "`"), msg, fixed = TRUE) ||
            grepl(paste0(entry$arg, " column"), msg, fixed = TRUE),
          info = paste(where, "::", msg))
        expect_true(
          has(msg, c(na_refusal, column_na, column_type, rate_refusal,
                     number_refusal)),
          info = paste(where, "::", msg))
        expect_true(grepl(paste0(verb, "("),
                          paste(deparse(conditionCall(cnd)), collapse = " "),
                          fixed = TRUE), info = where)
        # A TYPE refusal is admissible only where it is really about the type:
        # the same call carrying a non-NA value of that type must be refused
        # the same way. Without this control a column guard could answer "must
        # be numeric or character" to an NA the column's own type accepts, and
        # the sweep would read it as a refusal of the NA.
        if (has(msg, column_type) && !has(msg, column_na)) {
          ctl <- tryCatch({ entry$call(typed[[i]]); NULL },
                          error = function(e) e)
          expect_false(is.null(ctl),
                       info = paste(where, ":: type control did not abort"))
          if (is.null(ctl)) next
          expect_identical(conditionMessage(ctl), msg,
                           info = paste(where, ":: type control"))
        }
      }
    }
  }
})

# M081 -- the flag guards na_sweep_predicates() cannot see -------------------

test_that("the flag-guard walk flags each operator form and only unchecked flags", {
  # Positive controls, walked with the same code the namespace is: one planted
  # predicate per operator form that MUST be flagged, one that checks first and
  # must not be, and one that checks AFTER it has already branched -- which is
  # still a crash, and is what fixes "first" as positional rather than
  # "somewhere in the body".
  controls <- list(
    check_planted_not = function(flag, n) if (!flag) n else NULL,
    check_planted_and = function(flag, n) if (flag && n != 2) NULL,
    check_planted_or = function(flag, n) if (flag || n != 2) NULL,
    check_planted_indirect = function(flag, n) if (!is.null(flag)) n else NULL,
    check_planted_checked = function(flag, n) {
      rlang::check_bool(flag)
      if (!flag) n else NULL
    },
    check_planted_late = function(flag, n) {
      out <- if (!flag) n else NULL
      rlang::check_bool(flag)
      out
    }
  )
  found <- unchecked_flag_guards(controls)
  expect_identical(
    sort(names(found)),
    c("check_planted_and", "check_planted_late", "check_planted_not",
      "check_planted_or")
  )
  # WHICH formal, not just that something was flagged.
  for (nm in names(found)) expect_identical(found[[nm]], "flag", info = nm)
  # `!is.null(flag)` reads a property OF the flag and is not a bare branch, so
  # the walk must stay silent on it; `n` is a required formal of every control
  # and is branched on by none of them, so it must never appear.
  expect_false("check_planted_indirect" %in% names(found))
  expect_false("check_planted_checked" %in% names(found))
  expect_false("n" %in% unlist(found, use.names = FALSE))
})

test_that("no check_ predicate branches on an unchecked required flag", {
  preds <- tm_check_predicates()
  # The domain is walked out of the namespace, never listed here. Shown
  # non-empty, and shown to contain the two predicates M081 repaired -- a walk
  # over a set that excluded them would pass for the wrong reason.
  expect_gt(length(preds), 0)
  expect_true(all(c("check_audio_codec_needs_reencode",
                    "check_resize_needs_two_inputs") %in% names(preds)))
  found <- unchecked_flag_guards(preds)
  expect_identical(
    as.character(names(found)), character(0),
    info = paste("branching on an unchecked flag:",
                 paste(names(found), collapse = ", "))
  )
})
