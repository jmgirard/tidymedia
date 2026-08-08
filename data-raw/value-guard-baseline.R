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
#   value_guard_message_regressions(before, after)   # empty: no cell reads
#                                        #   worse without its blame moving
#   value_guard_blame_regressions(after) # empty: no cell blames anything but
#                                        #   the verb the user called
#   value_guard_blame(before, after)     # the cells whose blame moved
#   value_guard_missing_call(after)      # empty: no abort lost its `call`
#   value_guard_dead_controls(after)     # empty: every crossed cell's control
#                                        #   really does raise the crossed error
#   value_guard_ordering(before, after)  # which error each crossed cell showed,
#                                        #   before and after (M61)

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
                  exists = TRUE, informative = TRUE, crossed = "none",
                  control = FALSE, seam = "h264_nvenc") {
    cases[[length(cases) + 1L]] <<- list(
      site = site, verb = verb, form = form, label = label,
      violating = violating, exists = exists, informative = informative,
      crossed = crossed, control = control, seam = seam,
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

  # A MULTI-ELEMENT vocabulary argument, which is how a caller re-defaults one
  # -- and the cell class this grid originally lacked. M59's review (F1/F2)
  # found a blame-and-message regression reachable only here: the first fix
  # delegated to rlang::arg_match0(), which takes a string, so its own length
  # guard fired before the supplied `error_call` could be honoured. Every cell
  # above passes a single string and could not have caught it. `informative`
  # stays TRUE: unlike the single-string scalar cells, these must NOT change.
  add(5L, "compare_videos_batch", "scalar", "direction=[sideways,up]", TRUE,
      list(jobs = two(inputs = list(c(s, s)), output = "o.mp4"),
           direction = c("sideways", "up")))
  add(5L, "compare_videos_batch", "scalar", "direction=[vertical,horizontal]",
      FALSE,
      list(jobs = two(inputs = list(c(s, s)), output = "o.mp4"),
           direction = c("vertical", "horizontal")))

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
  # The `position` counterpart of the multi-element cells above. This is the
  # exact cell F1 was measured on: two of five values, so the length guard the
  # first fix tripped over fires here and not on `direction`, whose vocabulary
  # happens to be two elements long.
  add(6L, "picture_in_picture_batch", "scalar", "position=[center,topleft]",
      TRUE,
      list(jobs = two(main = s, overlay = s, output = "o.mp4"),
           position = c("center", "topleft")))
  add(6L, "picture_in_picture_batch", "scalar",
      "position=[topleft,topright,bottomright,bottomleft,center]", FALSE,
      list(jobs = two(main = s, overlay = s, output = "o.mp4"),
           position = c("topleft", "topright", "bottomright", "bottomleft",
                        "center")))

  # -- the ordering cells (M61) ----------------------------------------------
  #
  # Every cell above probes ONE mistake. These probe TWO: a value violation
  # crossed with a front-door error that could report instead of it, so the
  # grid measures WHICH of the two the user is shown rather than only whether
  # the call was refused. Four guards (sites 2, 4, 5, 6) plus pip's `audio`
  # (site 7, which had no front-door guard at all before this milestone), each
  # in its scalar-argument and `jobs`-column form, each crossed with the three
  # errors M61-D1 names:
  #
  #   contradiction  the verb's M58 checker -- `audio_codec` with no audio
  #                  carried, or `resize` across other than two inputs. This is
  #                  the one that CHANGES: before, the argument form reported
  #                  the value; after, both forms report the contradiction.
  #   nvenc          check_nvenc_available(), which must still report AFTER the
  #                  value on both refs (M59's AC5(b), this milestone's AC3).
  #   run_guard      ffm_batch()'s own `run` check, likewise still after.
  #
  # Each cell is paired with a CONTROL: the same call with the value in range,
  # which must still be refused BY THE CROSSED ERROR. Without it a cell showing
  # the contradiction would prove nothing -- a call that never had a live second
  # error reports its only one, and the ordering claim would rest on that.
  # Controls are `violating = TRUE` because they are refused calls; what makes
  # them controls rather than cells is `control = TRUE`.
  #
  # The nvenc cells hold the encoder seam EMPTY, against the full seam every
  # other cell in this file runs under: an availability error that cannot fire
  # is not an error this grid can be measured against.
  order_add <- function(site, verb, form, guard, crossed, bad, ok, extra,
                        seam = "h264_nvenc", exists = TRUE) {
    lab <- sprintf("%s/%s x %s", guard, form, crossed)
    add(site, verb, form, lab, TRUE,
        if (exists) c(bad, extra) else NULL,
        exists = exists, crossed = crossed, seam = seam)
    add(site, verb, form, paste(lab, "control"), TRUE,
        if (exists) c(ok, extra) else NULL,
        exists = exists, crossed = crossed, control = TRUE, seam = seam)
  }

  crossings <- list(
    list(name = "contradiction", extra = list(audio_codec = "aac"),
         seam = "h264_nvenc"),
    list(name = "nvenc",
         extra = list(hardware = "nvenc", video_codec = "libx264"),
         seam = character(0)),
    list(name = "run_guard", extra = list(run = "yes"), seam = "h264_nvenc")
  )

  for (x in crossings) {
    # site 5, `direction` on compare_videos_batch()
    order_add(5L, "compare_videos_batch", "scalar", "direction", x$name,
              bad = list(jobs = two(inputs = list(c(s, s)), output = "o.mp4"),
                         direction = "sideways"),
              ok = list(jobs = two(inputs = list(c(s, s)), output = "o.mp4"),
                        direction = "vertical"),
              extra = x$extra, seam = x$seam)
    order_add(5L, "compare_videos_batch", "column", "direction", x$name,
              bad = list(jobs = two(inputs = list(c(s, s), c(s, s)),
                                    output = c("a.mp4", "b.mp4"),
                                    direction = c("sideways", "sideways"))),
              ok = list(jobs = two(inputs = list(c(s, s), c(s, s)),
                                   output = c("a.mp4", "b.mp4"),
                                   direction = c("vertical", "vertical"))),
              extra = x$extra, seam = x$seam)

    # site 6, `position` on picture_in_picture_batch()
    order_add(6L, "picture_in_picture_batch", "scalar", "position", x$name,
              bad = list(jobs = two(main = s, overlay = s, output = "o.mp4"),
                         position = "middleish"),
              ok = list(jobs = two(main = s, overlay = s, output = "o.mp4"),
                        position = "center"),
              extra = x$extra, seam = x$seam)
    order_add(6L, "picture_in_picture_batch", "column", "position", x$name,
              bad = list(jobs = two(main = c(s, s), overlay = c(s, s),
                                    output = c("a.mp4", "b.mp4"),
                                    position = c("middleish", "middleish"))),
              ok = list(jobs = two(main = c(s, s), overlay = c(s, s),
                                   output = c("a.mp4", "b.mp4"),
                                   position = c("center", "center"))),
              extra = x$extra, seam = x$seam)

    # site 2, `margin` on picture_in_picture_batch()
    order_add(2L, "picture_in_picture_batch", "scalar", "margin", x$name,
              bad = list(jobs = two(main = s, overlay = s, output = "o.mp4"),
                         margin = -3),
              ok = list(jobs = two(main = s, overlay = s, output = "o.mp4"),
                        margin = 16),
              extra = x$extra, seam = x$seam)
    order_add(2L, "picture_in_picture_batch", "column", "margin", x$name,
              bad = list(jobs = two(main = c(s, s), overlay = c(s, s),
                                    output = c("a.mp4", "b.mp4"),
                                    margin = c(-3, -3))),
              ok = list(jobs = two(main = c(s, s), overlay = c(s, s),
                                   output = c("a.mp4", "b.mp4"),
                                   margin = c(16, 16))),
              extra = x$extra, seam = x$seam)

    # site 4, `audio` on compare_videos_batch(). The contradiction crossing is
    # `resize`, not `audio_codec`: a non-NULL `audio` is what MAKES the
    # audio_codec contradiction go away, so crossing this guard with it is the
    # cell that cannot exist. `resize` is independent of `audio`, so the rows
    # carry three inputs and ask to resize -- which leaves 7 out of range
    # (0..2) and the resize contradiction live at once.
    cmp_audio <- if (identical(x$name, "contradiction")) {
      list(rows1 = list(rep(s, 3)),
           rows2 = list(rep(s, 3), rep(s, 3)), extra = list(resize = TRUE))
    } else {
      list(rows1 = list(c(s, s)),
           rows2 = list(c(s, s), c(s, s)), extra = x$extra)
    }
    order_add(4L, "compare_videos_batch", "scalar", "audio", x$name,
              bad = list(jobs = two(inputs = cmp_audio$rows1,
                                    output = "o.mp4"), audio = 7),
              ok = list(jobs = two(inputs = cmp_audio$rows1,
                                   output = "o.mp4"), audio = 0),
              extra = cmp_audio$extra, seam = x$seam)
    order_add(4L, "compare_videos_batch", "column", "audio", x$name,
              bad = list(jobs = two(inputs = cmp_audio$rows2,
                                    output = c("a.mp4", "b.mp4"),
                                    audio = c(7, 7))),
              ok = list(jobs = two(inputs = cmp_audio$rows2,
                                   output = c("a.mp4", "b.mp4"),
                                   audio = c(0, 0))),
              extra = cmp_audio$extra, seam = x$seam)

    # The SAME guard at its other bound, because on compare_videos_batch() the
    # two bounds were checked in two places: the argument's lower bound at the
    # top of the verb, above the contradiction sweep, and the upper bound in the
    # per-row sweep below it. D038 records exactly that ("for `audio` even by
    # which bound was crossed"), so probing only 7 would miss the cell that
    # moves.
    order_add(4L, "compare_videos_batch", "scalar", "audio(low)", x$name,
              bad = list(jobs = two(inputs = cmp_audio$rows1,
                                    output = "o.mp4"), audio = -1),
              ok = list(jobs = two(inputs = cmp_audio$rows1,
                                   output = "o.mp4"), audio = 0),
              extra = cmp_audio$extra, seam = x$seam)
    order_add(4L, "compare_videos_batch", "column", "audio(low)", x$name,
              bad = list(jobs = two(inputs = cmp_audio$rows2,
                                    output = c("a.mp4", "b.mp4"),
                                    audio = c(-1, -1))),
              ok = list(jobs = two(inputs = cmp_audio$rows2,
                                   output = c("a.mp4", "b.mp4"),
                                   audio = c(0, 0))),
              extra = cmp_audio$extra, seam = x$seam)

    # site 7, `audio` on picture_in_picture_batch(). New with M61: before it,
    # this index was checked only inside the fan-out closure.
    #
    # The scalar x contradiction cell DOES NOT EXIST, and is recorded as
    # nonexistent rather than omitted: pip's only contradiction is an
    # `audio_codec` with no audio carried, and an `audio` ARGUMENT applies to
    # every row, so supplying one at all removes the contradiction. The column
    # form reaches it because rows may disagree -- one row dropping audio
    # (`NA`) contradicts the encoder while another carries an out-of-range
    # index.
    pip_contradiction <- identical(x$name, "contradiction")
    order_add(7L, "picture_in_picture_batch", "scalar", "audio", x$name,
              bad = list(jobs = two(main = s, overlay = s, output = "o.mp4"),
                         audio = 9),
              ok = list(jobs = two(main = s, overlay = s, output = "o.mp4"),
                        audio = 0),
              extra = x$extra, seam = x$seam,
              exists = !pip_contradiction)
    order_add(7L, "picture_in_picture_batch", "column", "audio", x$name,
              bad = list(jobs = two(main = c(s, s), overlay = c(s, s),
                                    output = c("a.mp4", "b.mp4"),
                                    audio = if (pip_contradiction) c(NA, 9)
                                            else c(9, 9))),
              ok = list(jobs = two(main = c(s, s), overlay = c(s, s),
                                   output = c("a.mp4", "b.mp4"),
                                   audio = if (pip_contradiction) c(NA, 0)
                                           else c(0, 0))),
              extra = x$extra, seam = x$seam)
  }

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
                        crossed = case$crossed, control = case$control,
                        kind = "nonexistent", outcome = NA_character_,
                        call = NA_character_, in_index = NA,
                        stringsAsFactors = FALSE))
    }
    # Per case, because the ordering cells crossed with availability need the
    # seam EMPTY while every other cell needs it full; `old` above restores
    # whatever the caller had.
    options(tidymedia.nvenc_encoders = case$seam)
    args <- case$args
    # `run` is forced FALSE so no cell needs FFmpeg -- except a cell that is
    # ABOUT ffm_batch()'s own `run` guard, which supplies its own bad value and
    # aborts before anything runs.
    if (!"run" %in% names(args)) args$run <- FALSE
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
               crossed = case$crossed, control = case$control,
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
  paste(d$site, d$verb, d$form, d$label, d$crossed, d$control, sep = "\037")
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

# The cells whose abort MESSAGE changed, split by whether their blame moved.
#
# This reader exists because the grid did not have one and a real regression
# walked through the gap (M59 review F4). value_guard_refusals() above compares
# only the OUTCOME KIND -- refused versus compiled -- so a cell that was refused
# on both refs compares equal no matter how differently it reads, and
# value_guard_blame() below sees only conditionCall(). A cell can therefore keep
# its verdict, keep its blame frame, and still start telling the user something
# worse; that is exactly what F2 was.
#
# The split is the whole point, because the two halves have OPPOSITE
# expectations:
#
#   moved_blame = TRUE  -- expected to change. These are the cells the milestone
#     set out to fix, and their `before` text carries purrr's
#     "In index: N / Caused by error in ..." wrapper that the fix removes.
#   moved_blame = FALSE -- must NOT change. A cell whose blame was already right
#     has no reason for its wording to move; anything here is a regression in
#     what the user reads, and is what F1/F2 would have surfaced on this grid.
#
# So the evidence is not "this result is empty" but "its FALSE half is empty",
# which is what value_guard_message_regressions() returns.
value_guard_messages <- function(before, after) {
  b <- value_guard_pair(before, after)
  both_abort <- b$kind == "abort" & after$kind == "abort"
  both_abort[is.na(both_abort)] <- FALSE
  changed <- both_abort & (b$outcome != after$outcome)
  changed[is.na(changed)] <- FALSE
  same_call <- (is.na(b$call) & is.na(after$call)) |
    (!is.na(b$call) & !is.na(after$call) & b$call == after$call)
  data.frame(site = after$site, verb = after$verb, form = after$form,
             label = after$label, crossed = after$crossed,
             control = after$control, moved_blame = !same_call,
             before = b$outcome, after = after$outcome,
             stringsAsFactors = FALSE)[which(changed), , drop = FALSE]
}

# The half that must be empty: a cell that reads differently WITHOUT its blame
# having moved. Empty is the evidence; a non-empty result names the calls whose
# message regressed while every other query in this file stayed green.
#
# Scoped to the cells that probe ONE mistake (`crossed = "none"`), because M61
# added a class of cell this test is the wrong instrument for. An ordering cell
# is wrong in two ways and both errors are blamed on the same verb, so changing
# WHICH of the two reports changes the message with the blame frame untouched --
# which is this milestone's entire deliverable and would read here as four
# regressions. Those cells are not thereby unchecked: value_guard_ordering()
# states which error each must show, on each ref, and is the stricter claim.
value_guard_message_regressions <- function(before, after) {
  m <- value_guard_messages(before, after)
  m[!m$moved_blame & m$crossed == "none", , drop = FALSE]
}

# Blame that moved AWAY from the verb, rather than toward it.
#
# The companion hole to value_guard_message_regressions(), and the other half of
# what let F1 through. value_guard_blame() below reports every cell whose blame
# moved, and the milestone reads a long list there as success -- but "moved" and
# "moved somewhere better" are different claims. F1's `position` cell moved its
# blame FROM `picture_in_picture_batch` TO `rlang::arg_match0(...)`, so it would
# have sat in that success list looking like progress.
#
# The invariant is absolute rather than comparative, which is why it needs no
# `before`: after this milestone every aborting cell in this grid calls a
# `_batch` verb directly, so the only name the user may be shown is that verb's.
# Empty is the evidence.
#
# ONE class of cell is excluded, and it is excluded because the error it raises
# is not one of these verbs': M61's `run_guard` control cells are refused by
# ffm_batch()'s own `run` check, which names `ffm_batch()` and has since long
# before this milestone. Excluding them keeps the invariant about the guards it
# is written for; they are not thereby unmeasured -- value_guard_messages()
# compares their wording across refs and value_guard_ordering() reports which
# error each showed, so a change there still surfaces.
value_guard_blame_regressions <- function(after) {
  own <- value_guard_error_class(after$outcome) != "run_guard"
  own[is.na(own)] <- TRUE
  bad <- own & after$kind == "abort" & !is.na(after$call) &
    after$call != after$verb
  bad[is.na(bad)] <- FALSE
  data.frame(site = after$site, verb = after$verb, form = after$form,
             label = after$label, blamed = after$call,
             message = after$outcome,
             stringsAsFactors = FALSE)[which(bad), , drop = FALSE]
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

# -- the ordering readers (M61) ----------------------------------------------

# Which of the two live errors a crossed cell reported. Classified from the
# message's own wording rather than from where the abort came from, because
# what the milestone is about is what the USER is shown.
#
# The classes are disjoint by construction on this grid: no cell crosses two
# of them at once, so a message matching none is "value" (the guard's own) and
# a message is never counted twice.
value_guard_error_class <- function(msg) {
  ifelse(is.na(msg), NA_character_,
  ifelse(grepl("needs an audio stream to encode", msg, fixed = TRUE) |
           grepl("supports exactly two inputs", msg, fixed = TRUE),
         "contradiction",
  ifelse(grepl("nvenc", msg, fixed = TRUE), "nvenc",
  ifelse(grepl("`run` must be", msg, fixed = TRUE), "run_guard",
         "value"))))
}

# AC1/AC3's claim, as a query: for every crossed cell, which error reported
# before and which reports after -- and, beside it, the control proving the
# crossed error was live on that call at all.
#
# Read it as three blocks:
#
#   crossed = "contradiction"  every cell must read `after = "contradiction"`,
#     in BOTH forms. The column rows are unchanged from before; the scalar rows
#     are the ones that move, and they are the milestone.
#   crossed = "nvenc" / "run_guard"  every cell must read "value" on BOTH refs.
#     These are invariants, not changes: a downward move could have inverted
#     either silently.
#
# A control whose class is not its own `crossed` name is the failure this
# reader exists to catch: it means the crossed error was never live on that
# call, and the cell beside it proves nothing.
value_guard_ordering <- function(before, after) {
  keep <- after$crossed != "none" & after$exists
  b <- value_guard_pair(before, after)
  out <- data.frame(site = after$site, verb = after$verb, form = after$form,
                    label = after$label, crossed = after$crossed,
                    control = after$control,
                    before = value_guard_error_class(b$outcome),
                    after = value_guard_error_class(after$outcome),
                    stringsAsFactors = FALSE)
  out[which(keep), , drop = FALSE]
}

# The controls that failed to establish their crossed error. Empty is the
# evidence; a non-empty result names cells whose ordering claim rests on
# nothing (the failure-identity check, run over the grid rather than by eye).
value_guard_dead_controls <- function(after) {
  o <- value_guard_ordering(after, after)
  o <- o[o$control, , drop = FALSE]
  o[o$after != o$crossed, , drop = FALSE]
}

# AC2's lost-`call` reader. An abort with no `conditionCall()` is the
# unattributed base-R error the Scope Out clause exists to prevent -- the shape
# `check_resize_needs_two_inputs()` degrades to when its type guard is moved
# out from above it ("invalid 'x' type in 'x && y'"). Every aborting cell in
# this grid calls a `_batch` verb directly, so every one must carry a call.
# Empty is the evidence.
value_guard_missing_call <- function(after) {
  bad <- after$kind == "abort" & is.na(after$call)
  bad[is.na(bad)] <- FALSE
  data.frame(site = after$site, verb = after$verb, form = after$form,
             label = after$label, crossed = after$crossed,
             message = after$outcome,
             stringsAsFactors = FALSE)[which(bad), , drop = FALSE]
}

# Element-wise equality that treats the nonexistent cells' NA as "unchanged"
# rather than propagating NA into the row selection above.
identical_flag <- function(x, y) {
  out <- x == y
  out[is.na(x) & is.na(y)] <- TRUE
  out[is.na(out)] <- FALSE
  out
}
