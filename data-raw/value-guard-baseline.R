# value-guard-baseline.R -----------------------------------------------------
#
# Regenerate the per-row value-check front-door baseline from an arbitrary git
# ref, so M59's claim -- "the front-door guard refuses exactly the calls its
# current check refuses" -- is re-derivable evidence rather than an
# implementation-time transcript (M59 T8, AC3). Same shape and the same ref
# machinery as `data-raw/contradiction-guard-baseline.R`, which in turn sources
# `data-raw/codec-guard-baseline.R` rather than copying its `git show`-into-an-
# environment helper; this file sources the same one, for the same reason.
#
# The six sites, each a range/shape/vocabulary check on ONE value the verb
# already holds, and the verb each is probed on:
#
#   1  width/height positive-or-expression   crop_video_batch
#   2  margin >= 0                           picture_in_picture_batch
#   3  regions table shape                   anonymize_video_batch
#   4  audio index <= inputs - 1 per row     compare_videos_batch
#   5  direction vocabulary                  compare_videos_batch
#   6  position vocabulary                   picture_in_picture_batch
#
# Each site is probed at an IN-RANGE and an OUT-OF-RANGE value, in up to three
# forms:
#
#   scalar  the value passed as the _batch verb's argument, applied to every row
#   column  the value carried in a `jobs` column, every row alike
#   mixed   a two-row table whose rows disagree -- one violating, one not
#
# `mixed` is the form that decides whether a guard sweeps rows or gates the
# whole table, and it is where M57's availability guard was caught gating
# all-or-nothing (that review's F4).
#
# Two site-specific gaps, recorded rather than silently absent:
#
#   * Site 3 has NO scalar form: anonymize_video_batch() has no `regions`
#     argument at all, so its cells are marked `form = "scalar"` with
#     `exists = FALSE` and are not compared.
#   * Sites 5 and 6's scalar cells are expected IDENTICAL on both refs -- both
#     verbs already vocabulary-checked their scalar argument at the front door
#     before M59 -- so they are marked `informative = FALSE`. They are probed
#     anyway, because a scalar cell that CHANGED would be a regression this
#     grid should show.
#
# For an abort each cell records the message, the function `conditionCall()`
# names -- the blame target -- and whether the message carries purrr's
# `In index: <n>` marker, present exactly when the check ran inside the fan-out
# rather than at the front door. For a success it records the compiled command
# with input paths and the session tempdir scrubbed, so two refs compared on
# different machines do not diff on paths.
#
# The nvenc encoder seam is held FULL (`h264_nvenc` present) for the whole grid,
# as in the M58 script: no cell here names `hardware`, but holding the seam
# fixed keeps the two refs measured under one encoder assumption by
# construction rather than by the machine that ran them.
#
# Every probe runs at `run = FALSE`, so no FFmpeg binary is needed and nothing
# is written to disk.
#
# Usage (from the package root):
#
#   source("data-raw/value-guard-baseline.R")
#   before <- value_guard_baseline("origin/master")
#   after  <- value_guard_baseline()
#   value_guard_vacuous(before)          # both empty: every in-range cell
#   value_guard_vacuous(after)           #   compiled on that ref
#   value_guard_refusals(before, after)  # empty: the same calls are refused
#   value_guard_blame(before, after)     # the cells whose blame moved

source(file.path("data-raw", "codec-guard-baseline.R"))

# -- the probe grid ----------------------------------------------------------

# One case per (site, verb, form, cell). `violating` is not measured, it is
# STATED from the check's own definition: the point of the grid is to compare a
# stated expectation against two refs' behaviour, and deriving it from either
# ref's output would make the comparison circular.
#
# `args` is built eagerly from the sample path, one fresh table per case: a
# shared table mutated in place would carry one case's column into the next.
value_guard_cases <- function(s) {
  cases <- list()
  add <- function(site, verb, form, label, violating, args,
                  exists = TRUE, informative = TRUE) {
    cases[[length(cases) + 1L]] <<- list(
      site = site, verb = verb, form = form, label = label,
      violating = violating, exists = exists, informative = informative,
      args = args)
  }
  two <- function(...) tibble::tibble(...)

  # -- site 1: crop width/height ---------------------------------------------
  # 0 is out of range because check_dim() requires a STRICTLY positive number
  # for a size (`inclusive = FALSE`); an expression string is always legal, so
  # the in-range cell is probed as a number and the expression form is left to
  # the unit tests.
  for (w in c(160, 0)) {
    bad <- w <= 0
    add(1L, "crop_video_batch", "scalar", sprintf("width=%g", w), bad,
        list(jobs = two(input = s, output = "o.mp4"), width = w, height = 120))
    add(1L, "crop_video_batch", "column", sprintf("width=%g", w), bad,
        list(jobs = two(input = c(s, s), output = c("a.mp4", "b.mp4"),
                        width = c(w, w)),
             height = 120))
  }
  add(1L, "crop_video_batch", "mixed", "width=[160,0]", TRUE,
      list(jobs = two(input = c(s, s), output = c("a.mp4", "b.mp4"),
                      width = c(160, 0)),
           height = 120))
  # The same three forms on `height`, the second argument the site covers.
  for (h in c(120, -1)) {
    bad <- h <= 0
    add(1L, "crop_video_batch", "scalar", sprintf("height=%g", h), bad,
        list(jobs = two(input = s, output = "o.mp4"), width = 160, height = h))
    add(1L, "crop_video_batch", "column", sprintf("height=%g", h), bad,
        list(jobs = two(input = c(s, s), output = c("a.mp4", "b.mp4"),
                        height = c(h, h)),
             width = 160))
  }
  add(1L, "crop_video_batch", "mixed", "height=[120,-1]", TRUE,
      list(jobs = two(input = c(s, s), output = c("a.mp4", "b.mp4"),
                      height = c(120, -1)),
           width = 160))

  # -- site 2: picture-in-picture margin -------------------------------------
  for (m in c(16, -3)) {
    bad <- m < 0
    add(2L, "picture_in_picture_batch", "scalar", sprintf("margin=%g", m), bad,
        list(jobs = two(main = s, overlay = s, output = "o.mp4"), margin = m))
    add(2L, "picture_in_picture_batch", "column", sprintf("margin=%g", m), bad,
        list(jobs = two(main = c(s, s), overlay = c(s, s),
                        output = c("a.mp4", "b.mp4"), margin = c(m, m))))
  }
  add(2L, "picture_in_picture_batch", "mixed", "margin=[16,-3]", TRUE,
      list(jobs = two(main = c(s, s), overlay = c(s, s),
                      output = c("a.mp4", "b.mp4"), margin = c(16, -3))))

  # -- site 3: anonymize regions ---------------------------------------------
  # Column-only: anonymize_video_batch() has no `regions` argument, so there is
  # no scalar form to probe. The cell is recorded as nonexistent rather than
  # omitted, so the grid states the gap instead of implying full coverage.
  good <- data.frame(x = 0, y = 0, width = 10, height = 10)
  bad_df <- data.frame(x = 0, y = 0, width = 10)   # no `height` column
  add(3L, "anonymize_video_batch", "scalar", "regions argument", NA,
      NULL, exists = FALSE)
  for (nm in c("complete", "missing height")) {
    cell <- if (identical(nm, "complete")) good else bad_df
    add(3L, "anonymize_video_batch", "column", sprintf("regions=%s", nm),
        !identical(nm, "complete"),
        list(jobs = two(input = c(s, s), output = c("a.mp4", "b.mp4"),
                        regions = list(cell, cell))))
  }
  add(3L, "anonymize_video_batch", "mixed", "regions=[complete,missing]", TRUE,
      list(jobs = two(input = c(s, s), output = c("a.mp4", "b.mp4"),
                      regions = list(good, bad_df))))

  # -- site 4: per-row audio index -------------------------------------------
  # Every row carries two inputs, so the legal indices are 0 and 1 and 7 is out
  # of range for the row rather than for the argument -- which is why the
  # _batch verb's scalar `check_number_whole(min = 0)` cannot catch it.
  for (a in c(0, 7)) {
    bad <- a > 1
    add(4L, "compare_videos_batch", "scalar", sprintf("audio=%g", a), bad,
        list(jobs = two(inputs = list(c(s, s)), output = "o.mp4"), audio = a))
    add(4L, "compare_videos_batch", "column", sprintf("audio=%g", a), bad,
        list(jobs = two(inputs = list(c(s, s), c(s, s)),
                        output = c("a.mp4", "b.mp4"), audio = c(a, a))))
  }
  add(4L, "compare_videos_batch", "mixed", "audio=[0,7]", TRUE,
      list(jobs = two(inputs = list(c(s, s), c(s, s)),
                      output = c("a.mp4", "b.mp4"), audio = c(0, 7))))

  # -- site 5: direction vocabulary ------------------------------------------
  # The scalar cells are expected identical on both refs: compare_videos_batch()
  # already arg-matched its `direction` ARGUMENT at the front door before M59.
  # Only the column form moves.
  for (d in c("vertical", "sideways")) {
    bad <- !d %in% c("horizontal", "vertical")
    add(5L, "compare_videos_batch", "scalar", sprintf("direction=%s", d), bad,
        list(jobs = two(inputs = list(c(s, s)), output = "o.mp4"),
             direction = d),
        informative = FALSE)
    add(5L, "compare_videos_batch", "column", sprintf("direction=%s", d), bad,
        list(jobs = two(inputs = list(c(s, s), c(s, s)),
                        output = c("a.mp4", "b.mp4"), direction = c(d, d))))
  }
  add(5L, "compare_videos_batch", "mixed", "direction=[vertical,sideways]",
      TRUE,
      list(jobs = two(inputs = list(c(s, s), c(s, s)),
                      output = c("a.mp4", "b.mp4"),
                      direction = c("vertical", "sideways"))))

  # -- site 6: position vocabulary -------------------------------------------
  # Same expected-identical scalar cells, for the same reason.
  for (p in c("center", "middleish")) {
    bad <- !p %in% c("topright", "topleft", "bottomright", "bottomleft",
                     "center")
    add(6L, "picture_in_picture_batch", "scalar", sprintf("position=%s", p),
        bad,
        list(jobs = two(main = s, overlay = s, output = "o.mp4"), position = p),
        informative = FALSE)
    add(6L, "picture_in_picture_batch", "column", sprintf("position=%s", p),
        bad,
        list(jobs = two(main = c(s, s), overlay = c(s, s),
                        output = c("a.mp4", "b.mp4"), position = c(p, p))))
  }
  add(6L, "picture_in_picture_batch", "mixed", "position=[center,middleish]",
      TRUE,
      list(jobs = two(main = c(s, s), overlay = c(s, s),
                      output = c("a.mp4", "b.mp4"),
                      position = c("center", "middleish"))))

  cases
}

# -- running the grid against a ref ------------------------------------------

# Probe every case in one ref's sources and return a data frame of observations.
# The seam is set here rather than by the caller so both sides of a comparison
# are measured under the same encoder assumption by construction.
value_guard_baseline <- function(ref = NULL, root = ".") {
  env <- codec_guard_env(ref, root)
  sample <- system.file("extdata", "sample.mp4", package = "tidymedia")
  if (!nzchar(sample)) stop("sample.mp4 not found; install the package first")
  old <- options(tidymedia.nvenc_encoders = "h264_nvenc")
  on.exit(options(old), add = TRUE)

  rows <- lapply(value_guard_cases(sample), function(case) {
    if (!case$exists) {
      return(data.frame(site = case$site, verb = case$verb, form = case$form,
                        label = case$label, violating = NA,
                        exists = FALSE, informative = FALSE,
                        kind = "nonexistent", outcome = NA_character_,
                        call = NA_character_, in_index = NA,
                        stringsAsFactors = FALSE))
    }
    args <- case$args
    args$run <- FALSE
    obs <- tryCatch(
      {
        # Call by NAME: do.call() on a function OBJECT records the anonymous
        # function as the condition call and hides the blame target this grid
        # exists to watch (the same trap codec-guard-baseline.R names).
        out <- do.call(case$verb, args, envir = env)
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
             # The FUNCTION part only: what a cell is compared on is the blame
             # target, and a whole-call deparse buries it behind a truncated
             # dump of the jobs table.
             call = if (is.null(cl)) NA_character_ else
               paste(deparse(cl[[1]]), collapse = ""),
             in_index = grepl("In index:", msg, fixed = TRUE))
      }
    )
    data.frame(site = case$site, verb = case$verb, form = case$form,
               label = case$label, violating = case$violating,
               exists = TRUE, informative = case$informative,
               kind = obs$kind, outcome = obs$outcome, call = obs$call,
               in_index = obs$in_index, stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, rows)
  attr(out, "ref") <- if (is.null(ref)) "<working tree>" else ref
  out
}

# -- reading the result ------------------------------------------------------

# The vacuity screen, run on BOTH sides before any comparison. A cell stated
# in-range that did not compile is measuring something other than the check --
# a schema error, a missing column -- and such a cell compares equal across refs
# while carrying no evidence. This is AC3's "each cell's in-range baseline is
# asserted to succeed on both refs, so no cell compares equal by both sides
# failing".
value_guard_vacuous <- function(baseline) {
  live <- baseline[baseline$exists, , drop = FALSE]
  bad <- live$violating & live$kind == "compiled"
  none <- !live$violating & live$kind != "compiled"
  out <- live[bad | none, c("site", "verb", "form", "label", "violating",
                            "kind", "outcome")]
  out$problem <- ifelse(out$violating, "stated violating but compiled",
                        "stated in-range but did not compile")
  out
}

value_guard_key <- function(d) {
  paste(d$site, d$verb, d$form, d$label, sep = "\037")
}

value_guard_pair <- function(before, after) {
  only_before <- setdiff(value_guard_key(before), value_guard_key(after))
  only_after <- setdiff(value_guard_key(after), value_guard_key(before))
  if (length(only_before) > 0 || length(only_after) > 0) {
    stop("the two baselines cover different cells; ",
         length(only_before), " only in `before`, ",
         length(only_after), " only in `after`. ",
         "Re-run both sides with the same version of this script.")
  }
  before[match(value_guard_key(after), value_guard_key(before)), , drop = FALSE]
}

# AC3's claim, as a query: the cells whose REFUSAL changed. A guard moved to the
# front door must refuse the same calls; only the blame and the moment may move,
# which is what value_guard_blame() below reports. An empty result here is the
# evidence; a non-empty one names the calls whose fate changed.
value_guard_refusals <- function(before, after) {
  b <- value_guard_pair(before, after)
  changed <- b$kind != after$kind |
    (b$kind == "compiled" & after$kind == "compiled" &
       b$outcome != after$outcome)
  changed[is.na(changed)] <- FALSE
  data.frame(site = after$site, verb = after$verb, form = after$form,
             label = after$label, violating = after$violating,
             before_kind = b$kind, after_kind = after$kind,
             before = b$outcome, after = after$outcome,
             stringsAsFactors = FALSE)[which(changed), , drop = FALSE]
}

# The cells whose BLAME moved -- what the milestone set out to change. Expect
# every violating cell in a column or mixed form here, `before` naming
# purrr::pmap and `after` naming the verb the user called, with `in_index`
# dropping to FALSE. The `informative = FALSE` scalar cells of sites 5 and 6
# must NOT appear: those already blamed the verb.
value_guard_blame <- function(before, after) {
  b <- value_guard_pair(before, after)
  same_call <- (is.na(b$call) & is.na(after$call)) |
    (!is.na(b$call) & !is.na(after$call) & b$call == after$call)
  changed <- !same_call | !identical_flag(b$in_index, after$in_index)
  data.frame(site = after$site, verb = after$verb, form = after$form,
             label = after$label, informative = after$informative,
             before_call = b$call, after_call = after$call,
             before_index = b$in_index, after_index = after$in_index,
             stringsAsFactors = FALSE)[which(changed), , drop = FALSE]
}

# Element-wise equality that treats the nonexistent cells' NA as "unchanged"
# rather than propagating NA into the row selection above.
identical_flag <- function(x, y) {
  out <- x == y
  out[is.na(x) & is.na(y)] <- TRUE
  out[is.na(out)] <- FALSE
  out
}
