# contradiction-guard-baseline.R ---------------------------------------------
#
# Regenerate the argument-contradiction front-door baseline from an arbitrary
# git ref, so M58's claim -- "the front-door guard refuses exactly the calls its
# pipeline counterpart refuses" -- is re-derivable evidence rather than an
# implementation-time transcript (M58 T8, AC3). Same shape and the same ref
# machinery as `data-raw/codec-guard-baseline.R`, which this file sources rather
# than copying: two implementations of `git show`-into-an-environment is the
# drift that file's own comments warn about.
#
# The six conditions, each a disagreement between two values the verb already
# holds, and the verbs each is probed on:
#
#   1  video_codec = "copy" with hardware != "none"
#        separate_audio_video, separate_audio_video_batch
#   2  reencode = FALSE with video_codec or hardware set
#        segment_video, segment_video_batch
#   3  reencode = FALSE with audio_codec != "copy"
#        segment_video, segment_video_batch
#   4  audio_codec set with audio = NULL      compare_videos, compare_videos_batch
#   5  resize = TRUE with length(inputs) != 2 compare_videos, compare_videos_batch
#   6  audio_codec set with audio = NULL      picture_in_picture,
#                                             picture_in_picture_batch
#
# Every argument a condition names is probed at a VIOLATING and a NON-VIOLATING
# value, crossed, so each condition's cells cover its whole truth table rather
# than only the corner that fails. Condition 5's two arguments are `resize` and
# the input count, so its cross is resize x {2 inputs, 3 inputs}.
#
# Three FORMS per condition, on the `_batch` verbs:
#
#   scalar  every value passed as an argument (the only form a scalar verb has)
#   column  the value carried in a `jobs` column, every row alike
#   mixed   a two-row table whose rows disagree -- one violating, one not
#
# `mixed` is the form that decides whether a guard sweeps rows or gates the
# whole table, and it is where M57's availability guard was caught gating
# all-or-nothing (that review's F4).
#
# For an abort each cell records the message, the function `conditionCall()`
# names -- the blame target -- and whether the message carries purrr's `In index: <n>`
# marker, which is present exactly when the check ran inside the fan-out rather
# than at the front door. For a success it records the compiled command with
# input paths and the session tempdir scrubbed, so two refs compared on
# different machines do not diff on paths.
#
# The nvenc encoder seam is held FULL (`h264_nvenc` present) for the whole grid.
# Three of the six conditions can only be violated by a call that also names
# `hardware = "nvenc"`, and on a machine without that encoder M57's availability
# guard would abort those cells on both refs -- comparing equal while measuring
# nothing about the contradiction. The precedence between the two guards is a
# separate claim, pinned by test rather than measured here
# (tests/testthat/test-nvenc-front-door.R).
#
# Every probe runs at `run = FALSE`, so no FFmpeg binary is needed and nothing
# is written to disk.
#
# Usage (from the package root):
#
#   source("data-raw/contradiction-guard-baseline.R")
#   before <- contradiction_guard_baseline("origin/master")
#   after  <- contradiction_guard_baseline()
#   contradiction_guard_vacuous(before)   # both empty: every non-violating
#   contradiction_guard_vacuous(after)    #   cell compiled on that ref
#   contradiction_guard_refusals(before, after)   # empty: same calls refused
#   contradiction_guard_blame(before, after)      # the cells whose blame moved

source(file.path("data-raw", "codec-guard-baseline.R"))

# -- the probe grid ----------------------------------------------------------

# One case per (condition, verb, form, cell). `violating` is not measured, it is
# STATED from the condition's own definition: the whole point of the grid is to
# compare a stated expectation against two refs' behaviour, and deriving it from
# either ref's output would make the comparison circular.
#
# `args` is built eagerly from the sample path, one fresh table per case: a
# shared table mutated in place would carry one case's column into the next.
contradiction_guard_cases <- function(s) {
  cases <- list()
  add <- function(condition, verb, form, label, violating, args) {
    cases[[length(cases) + 1L]] <<- list(
      condition = condition, verb = verb, form = form, label = label,
      violating = violating, args = args)
  }

  # -- condition 1: a copied video stream with hardware named -----------------
  for (vc in c("copy", "libx264")) {
    for (hw in c("nvenc", "none")) {
      bad <- identical(vc, "copy") && !identical(hw, "none")
      add(1L, "separate_audio_video", "scalar",
          sprintf("video_codec=%s hardware=%s", vc, hw), bad,
          list(
            infile = s, audiofile = "a.aac", videofile = "v.mp4",
            video_codec = vc, hardware = hw))
      add(1L, "separate_audio_video_batch", "scalar",
          sprintf("video_codec=%s hardware=%s", vc, hw), bad,
          list(
            jobs = tibble::tibble(input = s, audiofile = "a.aac",
                                  videofile = "v.mp4"),
            video_codec = vc, hardware = hw))
      add(1L, "separate_audio_video_batch", "column",
          sprintf("video_codec=%s hardware=%s", vc, hw), bad,
          list(
            jobs = tibble::tibble(input = c(s, s),
                                  audiofile = c("a1.aac", "a2.aac"),
                                  videofile = c("v1.mp4", "v2.mp4"),
                                  video_codec = c(vc, vc)),
            hardware = hw))
    }
    # The mixed form varies only `hardware`: the column is what disagrees.
  }
  for (hw in c("nvenc", "none")) {
    add(1L, "separate_audio_video_batch", "mixed",
        sprintf("video_codec=[libx264,copy] hardware=%s", hw),
        !identical(hw, "none"),
        list(
          jobs = tibble::tibble(input = c(s, s),
                                audiofile = c("a1.aac", "a2.aac"),
                                videofile = c("v1.mp4", "v2.mp4"),
                                video_codec = c("libx264", "copy")),
          hardware = hw))
  }

  # -- condition 2: a stream-copied cut that names an encoder -----------------
  for (re in c(FALSE, TRUE)) {
    for (vc in list("libx264", NULL)) {
      for (hw in c("nvenc", "none")) {
        bad <- !re && (!is.null(vc) || !identical(hw, "none"))
        lab <- sprintf("reencode=%s video_codec=%s hardware=%s", re,
                       if (is.null(vc)) "NULL" else vc, hw)
        add(2L, "segment_video", "scalar", lab, bad,
            list(
              infile = s, start = 0, end = 1, outfiles = "o.mp4",
              reencode = re, video_codec = vc, hardware = hw))
        add(2L, "segment_video_batch", "scalar", lab, bad,
            list(
              jobs = tibble::tibble(input = s, output = "o.mp4",
                                    start = 0, end = 1),
              reencode = re, video_codec = vc, hardware = hw))
        # Both of this condition's table-borne values as columns. NA is the
        # column form of `video_codec = NULL` (D022), so the two forms have to
        # agree on it or the front door would refuse a call that compiles.
        add(2L, "segment_video_batch", "column", lab, bad,
            list(
              jobs = tibble::tibble(
                input = c(s, s), output = c("a.mp4", "b.mp4"),
                start = c(0, 0), end = c(1, 1), reencode = c(re, re),
                video_codec = rep(if (is.null(vc)) NA_character_ else vc, 2L)),
              hardware = hw))
      }
    }
  }
  for (vc in c("libx264", NA)) {
    for (hw in c("nvenc", "none")) {
      # A mixed `reencode` column: row 2 copies, so it violates whenever the
      # call names an encoder at all.
      add(2L, "segment_video_batch", "mixed",
          sprintf("reencode=[TRUE,FALSE] video_codec=%s hardware=%s",
                  if (is.na(vc)) "NA" else vc, hw),
          !is.na(vc) || !identical(hw, "none"),
          list(
            jobs = tibble::tibble(
              input = c(s, s), output = c("a.mp4", "b.mp4"),
              start = c(0, 0), end = c(1, 1), reencode = c(TRUE, FALSE),
              video_codec = c(NA_character_, vc)),
            hardware = hw))
    }
  }

  # -- condition 3: a stream-copied cut that names an audio encoder -----------
  for (re in c(FALSE, TRUE)) {
    for (ac in c("aac", "copy")) {
      bad <- !re && !identical(ac, "copy")
      lab <- sprintf("reencode=%s audio_codec=%s", re, ac)
      add(3L, "segment_video", "scalar", lab, bad,
          list(
            infile = s, start = 0, end = 1, outfiles = "o.mp4",
            reencode = re, audio_codec = ac))
      add(3L, "segment_video_batch", "scalar", lab, bad,
          list(
            jobs = tibble::tibble(input = s, output = "o.mp4",
                                  start = 0, end = 1),
            reencode = re, audio_codec = ac))
      add(3L, "segment_video_batch", "column", lab, bad,
          list(
            jobs = tibble::tibble(input = c(s, s),
                                  output = c("a.mp4", "b.mp4"),
                                  start = c(0, 0), end = c(1, 1),
                                  reencode = c(re, re),
                                  audio_codec = c(ac, ac))))
    }
    add(3L, "segment_video_batch", "mixed",
        sprintf("reencode=%s audio_codec=[copy,aac]", re), !re,
        list(
          jobs = tibble::tibble(input = c(s, s), output = c("a.mp4", "b.mp4"),
                                start = c(0, 0), end = c(1, 1),
                                reencode = c(re, re),
                                audio_codec = c("copy", "aac"))))
  }

  # -- conditions 4 and 6: an audio encoder with no audio mapped --------------
  # `audio` is an input index; NULL (NA in a column) maps no audio at all, which
  # is what leaves a named encoder with nothing to act on.
  for (aud in list(NULL, 0)) {
    for (ac in list("aac", "copy", NULL)) {
      bad <- is.null(aud) && !is.null(ac) && !identical(ac, "copy")
      lab <- sprintf("audio=%s audio_codec=%s",
                     if (is.null(aud)) "NULL" else aud,
                     if (is.null(ac)) "NULL" else ac)
      add(4L, "compare_videos", "scalar", lab, bad,
          list(
            infiles = c(s, s), outfile = "o.mp4", audio = aud,
            audio_codec = ac))
      add(4L, "compare_videos_batch", "scalar", lab, bad,
          list(
            jobs = tibble::tibble(inputs = list(c(s, s)), output = "o.mp4"),
            audio = aud, audio_codec = ac))
      add(4L, "compare_videos_batch", "column", lab, bad,
          list(
            jobs = tibble::tibble(
              inputs = list(c(s, s), c(s, s)), output = c("a.mp4", "b.mp4"),
              audio = rep(if (is.null(aud)) NA_real_ else as.numeric(aud), 2L),
              audio_codec = rep(if (is.null(ac)) NA_character_ else ac, 2L))))
      add(6L, "picture_in_picture", "scalar", lab, bad,
          list(
            main = s, overlay = s, outfile = "o.mp4", audio = aud,
            audio_codec = ac))
      add(6L, "picture_in_picture_batch", "scalar", lab, bad,
          list(
            jobs = tibble::tibble(main = s, overlay = s, output = "o.mp4"),
            audio = aud, audio_codec = ac))
      add(6L, "picture_in_picture_batch", "column", lab, bad,
          list(
            jobs = tibble::tibble(
              main = c(s, s), overlay = c(s, s), output = c("a.mp4", "b.mp4"),
              audio = rep(if (is.null(aud)) NA_real_ else as.numeric(aud), 2L),
              audio_codec = rep(if (is.null(ac)) NA_character_ else ac, 2L))))
    }
  }
  for (ac in c("aac", "copy")) {
    bad <- !identical(ac, "copy")
    add(4L, "compare_videos_batch", "mixed",
        sprintf("audio=[0,NA] audio_codec=%s", ac), bad,
        list(
          jobs = tibble::tibble(inputs = list(c(s, s), c(s, s)),
                                output = c("a.mp4", "b.mp4"),
                                audio = c(0, NA)),
          audio_codec = ac))
    add(6L, "picture_in_picture_batch", "mixed",
        sprintf("audio=[0,NA] audio_codec=%s", ac), bad,
        list(
          jobs = tibble::tibble(main = c(s, s), overlay = c(s, s),
                                output = c("a.mp4", "b.mp4"),
                                audio = c(0, NA)),
          audio_codec = ac))
  }

  # -- condition 5: resize across other than two inputs -----------------------
  for (rs in c(TRUE, FALSE)) {
    for (n in c(2L, 3L)) {
      bad <- rs && n != 2L
      lab <- sprintf("resize=%s inputs=%d", rs, n)
      add(5L, "compare_videos", "scalar", lab, bad,
          list(
            infiles = rep(s, n), outfile = "o.mp4", resize = rs))
      add(5L, "compare_videos_batch", "scalar", lab, bad,
          list(
            jobs = tibble::tibble(inputs = list(rep(s, n)), output = "o.mp4"),
            resize = rs))
      add(5L, "compare_videos_batch", "column", lab, bad,
          list(
            jobs = tibble::tibble(inputs = list(rep(s, n), rep(s, n)),
                                  output = c("a.mp4", "b.mp4"),
                                  resize = c(rs, rs))))
    }
    # Two mixed forms, because this condition has two table-borne values: a
    # disagreeing `resize` column, and rows of different input counts.
    add(5L, "compare_videos_batch", "mixed",
        sprintf("resize=[FALSE,%s] inputs=3", rs), rs,
        list(
          jobs = tibble::tibble(inputs = list(rep(s, 3L), rep(s, 3L)),
                                output = c("a.mp4", "b.mp4"),
                                resize = c(FALSE, rs))))
    add(5L, "compare_videos_batch", "mixed",
        sprintf("resize=%s inputs=[2,3]", rs), rs,
        list(
          jobs = tibble::tibble(inputs = list(rep(s, 2L), rep(s, 3L)),
                                output = c("a.mp4", "b.mp4")),
          resize = rs))
  }

  cases
}

# -- running the grid against a ref ------------------------------------------

# Probe every case in one ref's sources and return a data frame of observations.
# The seam is set here rather than by the caller so both sides of a comparison
# are measured under the same encoder assumption by construction.
contradiction_guard_baseline <- function(ref = NULL, root = ".") {
  env <- codec_guard_env(ref, root)
  sample <- system.file("extdata", "sample.mp4", package = "tidymedia")
  if (!nzchar(sample)) stop("sample.mp4 not found; install the package first")
  old <- options(tidymedia.nvenc_encoders = "h264_nvenc")
  on.exit(options(old), add = TRUE)

  rows <- lapply(contradiction_guard_cases(sample), function(case) {
    args <- case$args
    args$run <- FALSE
    obs <- tryCatch(
      {
        # Call by NAME: do.call() on a function OBJECT records the anonymous
        # function as the condition call and hides the blame target this grid
        # exists to watch (the same trap codec-guard-baseline.R names).
        out <- do.call(case$verb, args, envir = env)
        # A scalar verb returns the compiled command as a character vector; a
        # _batch verb returns the jobs tibble with a `command` column. Reading
        # `out$command` on the first shape aborts on "$ operator is invalid for
        # atomic vectors" and records that as the cell's outcome -- a failure
        # that looks like a refusal and is not one.
        txt <- if (is.data.frame(out)) out$command else as.character(out)
        txt <- gsub(sample, "<in>", txt, fixed = TRUE)
        txt <- gsub(tempdir(), "<tmp>", txt, fixed = TRUE)
        list(kind = "compiled", outcome = paste(txt, collapse = " ||| "),
             call = NA_character_, in_index = FALSE)
      },
      condition = function(cnd) {
        msg <- tryCatch(
          paste(cli::ansi_strip(conditionMessage(cnd)), collapse = "\n"),
          error = function(e) conditionMessage(cnd))
        cl <- conditionCall(cnd)
        list(kind = if (inherits(cnd, "error")) "abort" else "condition",
             outcome = msg,
             # The FUNCTION part only, never the first line of the whole
             # deparsed call: what a cell is being compared on is the blame
             # target, and codec-guard-baseline.R's whole-call form buries it
             # behind a truncated dump of the jobs table.
             call = if (is.null(cl)) NA_character_ else
               paste(deparse(cl[[1]]), collapse = ""),
             in_index = grepl("In index:", msg, fixed = TRUE))
      }
    )
    data.frame(condition = case$condition, verb = case$verb, form = case$form,
               label = case$label, violating = case$violating,
               kind = obs$kind, outcome = obs$outcome, call = obs$call,
               in_index = obs$in_index, stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, rows)
  attr(out, "ref") <- if (is.null(ref)) "<working tree>" else ref
  out
}

# -- reading the result ------------------------------------------------------

# The vacuity screen, run on BOTH sides before any comparison. A cell stated
# non-violating that did not compile is measuring something other than the
# condition -- a schema error, a missing column, a codec the verb refuses -- and
# such a cell compares equal across refs while carrying no evidence. This is the
# screen AC3's "no cell compares equal by both sides failing" asks for.
contradiction_guard_vacuous <- function(baseline) {
  bad <- baseline$violating & baseline$kind == "compiled"
  none <- !baseline$violating & baseline$kind != "compiled"
  out <- baseline[bad | none, c("condition", "verb", "form", "label",
                                "violating", "kind", "outcome")]
  out$problem <- ifelse(out$violating, "stated violating but compiled",
                        "stated clean but did not compile")
  out
}

contradiction_guard_key <- function(d) {
  paste(d$condition, d$verb, d$form, d$label, sep = "\037")
}

contradiction_guard_pair <- function(before, after) {
  only_before <- setdiff(contradiction_guard_key(before),
                         contradiction_guard_key(after))
  only_after <- setdiff(contradiction_guard_key(after),
                        contradiction_guard_key(before))
  if (length(only_before) > 0 || length(only_after) > 0) {
    stop("the two baselines cover different cells; ",
         length(only_before), " only in `before`, ",
         length(only_after), " only in `after`. ",
         "Re-run both sides with the same version of this script.")
  }
  before[match(contradiction_guard_key(after),
               contradiction_guard_key(before)), , drop = FALSE]
}

# AC3's claim, as a query: the cells whose REFUSAL changed. A guard moved to the
# front door must refuse the same calls; only the blame and the moment may move,
# which is what contradiction_guard_blame() below reports. An empty result here
# is the evidence; a non-empty one names the calls whose fate changed.
contradiction_guard_refusals <- function(before, after) {
  b <- contradiction_guard_pair(before, after)
  changed <- b$kind != after$kind |
    (b$kind == "compiled" & after$kind == "compiled" &
       b$outcome != after$outcome)
  data.frame(condition = after$condition, verb = after$verb, form = after$form,
             label = after$label, violating = after$violating,
             before_kind = b$kind, after_kind = after$kind,
             before = b$outcome, after = after$outcome,
             stringsAsFactors = FALSE)[which(changed), , drop = FALSE]
}

# The cells whose BLAME moved -- what the milestone set out to change. Expect
# every violating cell on a fan-out verb here, `before` naming purrr::pmap (or
# a `*_pipeline` function) and `after` naming the verb the user called, with
# `in_index` dropping to FALSE.
contradiction_guard_blame <- function(before, after) {
  b <- contradiction_guard_pair(before, after)
  same_call <- (is.na(b$call) & is.na(after$call)) |
    (!is.na(b$call) & !is.na(after$call) & b$call == after$call)
  changed <- !same_call | b$in_index != after$in_index
  data.frame(condition = after$condition, verb = after$verb, form = after$form,
             label = after$label,
             before_call = b$call, after_call = after$call,
             before_index = b$in_index, after_index = after$in_index,
             stringsAsFactors = FALSE)[which(changed), , drop = FALSE]
}
