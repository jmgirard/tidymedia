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
#   value_guard_uncovered(after)         # empty: every (verb, value, form,
#                                        #   crossing) AC2 asks for has a cell

source(file.path("data-raw", "codec-guard-baseline.R"))

# -- the ordering cross-product, declared once -------------------------------
#
# AC2 asks for each of M61's guards, in each of its forms, crossed with each
# front-door error M61-D1 names. That is a cross-product, and three review
# rounds each found a different combination of it missing -- always a cell
# nobody had typed out. So the combinations are GENERATED from the three
# declarations below rather than written one call at a time, and
# value_guard_uncovered() re-derives the same product and reports any
# combination the grid did not produce. Completeness holds by construction;
# the reader is what makes it checkable from the grid's own output.
#
# The (verb, value) pairs are AC1's four values in Scope In, plus pip's `audio`
# -- the guard that had no front door at all before this milestone.
VALUE_GUARD_PAIRS <- list(
  c(verb = "compare_videos_batch", value = "direction"),
  c(verb = "compare_videos_batch", value = "audio"),
  c(verb = "picture_in_picture_batch", value = "position"),
  c(verb = "picture_in_picture_batch", value = "margin"),
  c(verb = "picture_in_picture_batch", value = "audio")
)

# The forms AC6's sentence quantifies over: the value as the verb's argument,
# and the value in a `jobs` column. The scalar verbs below take no `jobs`
# table, so they carry only the first, under its own name.
VALUE_GUARD_FORMS <- c("scalar", "column")

# M61-D1's three crossings, per verb. Crossing (1) is the verb's own M58
# contradiction, and `compare_videos_batch()` carries TWO of them in two
# different checkers -- an `audio_codec` naming an encoder with no audio
# carried, and `resize` across other than two inputs -- so each is its own
# member here rather than one "contradiction" standing for both. That
# conflation is what round 2 returned on.
#
# The two scalar verbs cross the contradictions only. They reach these guards
# through the shared `*_pipeline()`, which is below both the availability check
# and `ffm_batch()`, so crossings (2) and (3) are not theirs to order.
VALUE_GUARD_CROSSINGS <- list(
  compare_videos_batch = c("contradiction:audio_codec", "contradiction:resize",
                           "nvenc", "run_guard"),
  picture_in_picture_batch = c("contradiction:audio_codec", "nvenc",
                               "run_guard"),
  compare_videos = c("contradiction:audio_codec", "contradiction:resize"),
  picture_in_picture = c("contradiction:audio_codec")
)

# What each crossing supplies to a call, and what encoder seam it needs. The
# `nvenc` crossing holds the seam EMPTY against the full seam every other cell
# runs under: an availability error that cannot fire is not an error this grid
# can be measured against.
value_guard_crossing_extra <- function(crossing) {
  switch(crossing,
         "contradiction:audio_codec" = list(audio_codec = "aac"),
         "contradiction:resize" = list(resize = TRUE),
         "nvenc" = list(hardware = "nvenc", video_codec = "libx264"),
         "run_guard" = list(run = "yes"),
         stop("unknown crossing: ", crossing))
}

value_guard_crossing_seam <- function(crossing) {
  if (identical(crossing, "nvenc")) character(0) else "h264_nvenc"
}

# The error CLASS a crossing belongs to, which is what value_guard_ordering()
# reads off a message. Two crossings share the `contradiction` class.
value_guard_crossing_class <- function(crossing) sub(":.*$", "", crossing)

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
                  crossing = "none", control = FALSE, seam = "h264_nvenc") {
    cases[[length(cases) + 1L]] <<- list(
      site = site, verb = verb, form = form, label = label,
      violating = violating, exists = exists, informative = informative,
      crossed = crossed, crossing = crossing, control = control, seam = seam,
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
  # the call was refused.
  #
  # The cells are GENERATED from VALUE_GUARD_PAIRS x VALUE_GUARD_FORMS x
  # VALUE_GUARD_CROSSINGS, declared at the top of this file. Nothing below
  # names a combination; each guard supplies only the two things that cannot be
  # derived -- the shape of a call to its verb, and which value violates it --
  # and the driver crosses them. Three review rounds each found one
  # hand-written combination missing, so the enumeration is the fix: a
  # combination can no longer be forgotten, only mis-built, and
  # value_guard_uncovered() reports either.
  #
  # Each cell is paired with a CONTROL: the same call with the value in range,
  # which must still be refused BY THE CROSSED ERROR. Without it a cell showing
  # the contradiction would prove nothing -- a call that never had a live second
  # error reports its only one, and the ordering claim would rest on that.
  # Controls are `violating = TRUE` because they are refused calls; what makes
  # them controls rather than cells is `control = TRUE`.

  # Row shapes. `compare_videos_batch()`'s row count of INPUTS is what makes
  # the `resize` contradiction live, so it is a function of the crossing.
  cmp_rows <- function(form, crossing, ...) {
    n_in <- if (identical(crossing, "contradiction:resize")) 3L else 2L
    if (identical(form, "column")) {
      two(inputs = list(rep(s, n_in), rep(s, n_in)),
          output = c("a.mp4", "b.mp4"), ...)
    } else {
      two(inputs = list(rep(s, n_in)), output = "o.mp4", ...)
    }
  }
  pip_rows <- function(form, crossing, ...) {
    if (identical(form, "column")) {
      two(main = c(s, s), overlay = c(s, s), output = c("a.mp4", "b.mp4"), ...)
    } else {
      two(main = s, overlay = s, output = "o.mp4", ...)
    }
  }

  # Put a value where its form says it goes: an argument beside `jobs` in the
  # scalar form, a column inside `jobs` in the column form. `x[col] <- list(v)`
  # rather than `x[[col]] <- v`, so a `NULL` value is PASSED as NULL rather
  # than dropping the argument -- the two differ to a verb with a non-NULL
  # default, and one control below depends on passing NULL explicitly.
  place <- function(rows_fn, col) {
    function(form, crossing, v) {
      if (identical(form, "column")) {
        cols <- list(v)
        names(cols) <- col
        list(jobs = do.call(rows_fn, c(list(form, crossing), cols)))
      } else {
        out <- list(jobs = rows_fn(form, crossing))
        out[col] <- list(v)
        out
      }
    }
  }

  # A value that violates the same way in both forms, and its in-range twin.
  plain <- function(bad, ok) {
    function(form, crossing) {
      if (identical(form, "column")) list(bad = rep(bad, 2L), ok = rep(ok, 2L))
      else list(bad = bad, ok = ok)
    }
  }

  # `audio` is the one value whose violating spelling depends on the crossing,
  # and the reason is the crossing itself. An `audio` index that is in range
  # gives the encoder something to encode, which REMOVES the `audio_codec`
  # contradiction -- so against that crossing the violating value has to be one
  # the guard refuses AND that still drops the audio. `batch_stream_cell()`
  # resolves any NA-ish length-1 value to `NULL`, which is exactly that; the
  # `nan` variant below pins that it is the is.na() mechanism and not the
  # literal `NA`. Two earlier readings of this cell were measured wrong -- that
  # supplying `audio` at all removes the contradiction, and that exactly one
  # value reaches it -- and both were over-generalizations of the same true
  # fact about IN-RANGE values.
  #
  # The control differs by crossing, and it has to. A control proves the
  # crossed error is live with the value in range -- but against `audio_codec`
  # an in-range index is what removes it. So that control passes the value the
  # violating one RESOLVES to: `NULL` in the scalar form, and in the column
  # form a row that still drops its audio beside a row now in range. Against
  # `nvenc` and the `run` guard, where the crossed error is independent of
  # `audio`, an in-range index is the right control.
  audio_values <- function(bad, ok) {
    function(form, crossing) {
      drops_audio <- identical(crossing, "contradiction:audio_codec")
      if (identical(form, "column")) {
        if (drops_audio) list(bad = c(NA, bad), ok = c(NA, ok))
        else list(bad = rep(bad, 2L), ok = rep(ok, 2L))
      } else {
        if (drops_audio) list(bad = NA, ok = NULL)
        else list(bad = bad, ok = ok)
      }
    }
  }

  # The five (verb, value) pairs, each with the row shape its verb takes and
  # the value that violates it. `label` is the pair's value name, which is what
  # value_guard_uncovered() reads back off the grid.
  guard_specs <- list(
    list(site = 5L, verb = "compare_videos_batch", label = "direction",
         args = place(cmp_rows, "direction"),
         values = plain("sideways", "vertical")),
    list(site = 4L, verb = "compare_videos_batch", label = "audio",
         args = place(cmp_rows, "audio"),
         values = audio_values(7, 0)),
    list(site = 6L, verb = "picture_in_picture_batch", label = "position",
         args = place(pip_rows, "position"),
         values = plain("middleish", "center")),
    list(site = 2L, verb = "picture_in_picture_batch", label = "margin",
         args = place(pip_rows, "margin"),
         values = plain(-3, 16)),
    list(site = 7L, verb = "picture_in_picture_batch", label = "audio",
         args = place(pip_rows, "audio"),
         values = audio_values(9, 0))
  )

  # The scalar verbs (M61 review, F2). `compare_videos()` and
  # `picture_in_picture()` have NO vocabulary guard of their own: `direction`
  # and `position` are checked only inside the shared `*_pipeline()`, so moving
  # that check below the pipeline's contradiction checkers moved these verbs'
  # answer too. Scope Out carves their front doors out only "beyond their
  # shared pipeline", so the change is intended -- but every cell above probes
  # a `_batch` verb, and nothing measured these until M61's review did.
  #
  # They take arguments and no `jobs` table, so their one form is recorded as
  # `argument` rather than `scalar`: a reader cannot then mistake one of these
  # for a `_batch` verb's scalar cell.
  scalar_specs <- list(
    list(site = 5L, verb = "compare_videos", label = "direction",
         args = function(form, crossing, v) {
           n_in <- if (identical(crossing, "contradiction:resize")) 3L else 2L
           list(infiles = rep(s, n_in), outfile = "o.mp4", direction = v)
         },
         values = plain("sideways", "vertical")),
    list(site = 6L, verb = "picture_in_picture", label = "position",
         args = function(form, crossing, v) {
           list(main = s, overlay = s, outfile = "o.mp4", position = v)
         },
         values = plain("middleish", "center"))
  )

  # Variants beyond the cross-product: the SAME guard probed at a second
  # violating value, where that value is checked somewhere else or resolved by
  # a different mechanism. Each names the crossings it is probed against, since
  # a variant is not required at every one -- `audio(low)` cannot cross
  # `audio_codec`, because -1 is not NA-ish and so still carries audio.
  variant_specs <- list(
    # compare's `audio` at its LOWER bound. The two bounds were checked in two
    # places before this milestone -- the argument's lower bound at the top of
    # the verb, above the contradiction sweep, and the upper bound in the
    # per-row sweep below it. D038 records exactly that ("for `audio` even by
    # which bound was crossed"), so probing only 7 would miss the cell that
    # moves.
    list(site = 4L, verb = "compare_videos_batch", label = "audio(low)",
         args = place(cmp_rows, "audio"), values = plain(-1, 0),
         forms = VALUE_GUARD_FORMS,
         crossings = c("contradiction:resize", "nvenc", "run_guard")),
    # `NaN` against the `audio_codec` contradiction on both verbs, which pins
    # that what reaches that pairing is `batch_stream_cell()`'s is.na() test
    # and not the literal `NA` the cells above spell it with.
    list(site = 4L, verb = "compare_videos_batch", label = "audio(NaN)",
         args = place(cmp_rows, "audio"),
         values = function(form, crossing) list(bad = NaN, ok = NULL),
         forms = "scalar", crossings = "contradiction:audio_codec"),
    list(site = 7L, verb = "picture_in_picture_batch", label = "audio(NaN)",
         args = place(pip_rows, "audio"),
         values = function(form, crossing) list(bad = NaN, ok = NULL),
         forms = "scalar", crossings = "contradiction:audio_codec")
  )

  # The driver. One cell and one control per (guard, form, crossing).
  order_add <- function(spec, form, crossing) {
    v <- spec$values(form, crossing)
    lab <- sprintf("%s/%s x %s", spec$label, form, crossing)
    extra <- value_guard_crossing_extra(crossing)
    seam <- value_guard_crossing_seam(crossing)
    cls <- value_guard_crossing_class(crossing)
    add(spec$site, spec$verb, form, lab, TRUE,
        c(spec$args(form, crossing, v$bad), extra),
        crossed = cls, crossing = crossing, seam = seam)
    add(spec$site, spec$verb, form, paste(lab, "control"), TRUE,
        c(spec$args(form, crossing, v$ok), extra),
        crossed = cls, crossing = crossing, control = TRUE, seam = seam)
  }

  for (spec in c(guard_specs, scalar_specs)) {
    forms <- if (spec$verb %in% c("compare_videos", "picture_in_picture")) {
      "argument"
    } else {
      VALUE_GUARD_FORMS
    }
    for (form in forms) {
      for (crossing in VALUE_GUARD_CROSSINGS[[spec$verb]]) {
        order_add(spec, form, crossing)
      }
    }
  }
  for (spec in variant_specs) {
    for (form in spec$forms) {
      for (crossing in spec$crossings) order_add(spec, form, crossing)
    }
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
  old <- options(tidymedia.hardware_encoders = "h264_nvenc")
  on.exit(options(old), add = TRUE)

  rows <- lapply(value_guard_cases(sample), function(case) {
    if (!case$exists) {
      return(data.frame(site = case$site, verb = case$verb, form = case$form,
                        label = case$label, violating = NA,
                        exists = FALSE, informative = FALSE,
                        crossed = case$crossed, crossing = case$crossing,
                        control = case$control,
                        kind = "nonexistent", outcome = NA_character_,
                        call = NA_character_, in_index = NA,
                        stringsAsFactors = FALSE))
    }
    # Per case, because the ordering cells crossed with availability need the
    # seam EMPTY while every other cell needs it full; `old` above restores
    # whatever the caller had.
    options(tidymedia.hardware_encoders = case$seam)
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
               crossed = case$crossed, crossing = case$crossing,
               control = case$control,
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
  sub(":.*$", "", value_guard_error_crossing(msg))
}

# The same classification at the resolution the crossing declaration uses, so a
# verb's two contradictions do not stand for each other. `crossed` collapses
# both to "contradiction", which is the right grain for the ordering claim -- a
# cell must show A contradiction -- but the WRONG grain for validating a
# control, whose job is to prove the specific crossed error live. A control
# checked at class grain passes when the other contradiction fires instead,
# which is the conflation round 2 returned on, in the validator rather than in
# the grid (M61 review round 4, F4).
value_guard_error_crossing <- function(msg) {
  ifelse(is.na(msg), NA_character_,
  ifelse(grepl("needs an audio stream to encode", msg, fixed = TRUE),
         "contradiction:audio_codec",
  ifelse(grepl("supports exactly two inputs", msg, fixed = TRUE),
         "contradiction:resize",
  ifelse(grepl("nvenc", msg, fixed = TRUE), "nvenc",
  ifelse(grepl("`run` must be", msg, fixed = TRUE), "run_guard",
         "value")))))
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
                    crossing = after$crossing, control = after$control,
                    before = value_guard_error_class(b$outcome),
                    after = value_guard_error_class(after$outcome),
                    after_crossing = value_guard_error_crossing(after$outcome),
                    stringsAsFactors = FALSE)
  out[which(keep), , drop = FALSE]
}

# The controls that failed to establish their crossed error. Empty is the
# evidence; a non-empty result names cells whose ordering claim rests on
# nothing (the failure-identity check, run over the grid rather than by eye).
# Compared at CROSSING grain, not class grain: a control for
# `contradiction:resize` that raises the `audio_codec` contradiction instead has
# not established the error its cell is crossed with, and a class-grain check
# would pass it (M61 review round 4, F4).
value_guard_dead_controls <- function(after) {
  o <- value_guard_ordering(after, after)
  o <- o[o$control, , drop = FALSE]
  o[o$after_crossing != o$crossing, , drop = FALSE]
}

# AC2's completeness claim, as a query rather than as vigilance. Empty is the
# evidence; a non-empty result names the (verb, value, form, crossing)
# combinations AC2 asks for that no cell in the grid probes.
#
# It re-derives the cross-product from VALUE_GUARD_PAIRS x VALUE_GUARD_FORMS x
# VALUE_GUARD_CROSSINGS and looks each combination up in the baseline the grid
# actually produced. The cells are generated from the same crossing
# declaration, so this cannot catch a crossing dropped from that list -- what
# it catches is a PAIR dropped from the guard specs, a form never emitted, and
# any combination whose builder produced nothing. The three round-on-round
# failures were all of that kind: a combination nobody wrote.
#
# A variant cell does NOT count for its base value. `audio(low)` and
# `audio(NaN)` probe `audio` at a second violating value, and reading their
# labels back to `audio` would let them stand in for the base guard: deleting
# the whole `compare_videos_batch`/`audio` spec then hid 7 of its 8 lost
# combinations behind the `audio(low)` variant, leaving one row where eight
# were owed (M61 review round 4, F3). So the match is on the bare value name,
# and a variant's parenthesized label never satisfies the product. A variant is
# extra coverage; it is not the coverage AC2 asks for.
value_guard_uncovered <- function(after) {
  live <- after[!after$control & after$crossing != "none", , drop = FALSE]
  value_of <- sub("/.*$", "", live$label)
  have <- paste(live$verb, value_of, live$form, live$crossing, sep = "\037")
  want <- list()
  for (pair in VALUE_GUARD_PAIRS) {
    for (form in VALUE_GUARD_FORMS) {
      for (crossing in VALUE_GUARD_CROSSINGS[[pair[["verb"]]]]) {
        want[[length(want) + 1L]] <- data.frame(
          verb = pair[["verb"]], value = pair[["value"]], form = form,
          crossing = crossing, stringsAsFactors = FALSE)
      }
    }
  }
  want <- do.call(rbind, want)
  key <- paste(want$verb, want$value, want$form, want$crossing, sep = "\037")
  want[!key %in% have, , drop = FALSE]
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
