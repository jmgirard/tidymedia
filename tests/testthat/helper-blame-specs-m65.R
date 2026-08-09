# The M65 blame grid's spec list: every (verb, form, delivery, argument or
# region field, violating value) cell whose abort must name the verb the caller
# typed -- the region, overlay-scale and loudness siblings of M64's crop/scale/
# rate list in helper-blame-specs.R. Same home, for the same reason recorded
# there: `^data-raw$` is in .Rbuildignore, so a list declared there is invisible
# to the grid in exactly the run the release gate uses (LESSONS M51/M59; this
# milestone's gated amendment).
#
# One declaration, read by its consumers: the M65 blocks in
# test-builder-blame-front-door.R, and -- from the source tree --
# data-raw/blame-precedence-m65.R's usage notes. data-raw/
# blame-guard-mutations-m65.py derives its sites from the branch diff and holds
# no copy.
#
# THE LIST IS CLOSED BY INSPECTION, not by a procedure: it was built by reading
# the three verbs' front doors and their `*_pipeline()` helpers for values
# handed to a validating `ffm_*` builder (ffm_drawbox's region fields,
# ffm_overlay's `scale` range, ffm_loudnorm's three loudness targets). Nothing
# enumerates "every leaking argument" for us. What IS mechanical is
# blame_spec_defects_m65() below.
#
# `delivery` is "arg" (the verb's own argument) or "column" (a `jobs` column);
# they are not equivalent (batch_arg_rows() resolves a column OVER the
# argument). Every `_batch` cell appears in both deliveries, with ONE
# exception the reader knows about: `regions` has no argument form on the
# batch verb -- it exists only as the `regions` list-column -- so its batch
# cells are column-only, and its scalar cells argument-only.
#
# The bad REGION cells vary both the row and the field within a multi-row
# frame (AC3): a sweep that read only the first row, or only one field, goes
# red on the cell that hides the bad value elsewhere.
#
# `two_pass` is an axis on the two normalize_audio scalar deliveries: the
# TRUE cells are the calls whose bad target used to cost a wasted analysis
# pass before aborting from the builder. They abort before FFmpeg now, but a
# mutated (sweep-deleted) run reaches run_loudnorm_analysis(), so the grid
# runs them under skip_if_no_ffmpeg() (the milestone's evidence note).
#
# `pinned = TRUE` marks a cell that already blames its verb on master (AC5's
# two type-grain `scale` cells): evidence of no regression, never of a fix,
# and no mutation in data-raw/blame-guard-mutations-m65.py owns it.

blame_specs_m65 <- function(input) {
  one <- function(...) tibble::tibble(...)

  dim_msg <- function(field) {
    paste0("`", field, "` must be a single FFmpeg expression or number")
  }
  scale_range_msg <- "`scale` must be greater than 0 and at most 1"
  scale_type_msg <- "`scale` must be a number"
  loud_msg <- function(arg) paste0("`", arg, "` must be a number between ")

  specs <- list()
  add <- function(...) specs[[length(specs) + 1L]] <<- list(...)

  # --- anonymize_video / _batch: region field values -> ffm_drawbox() --------
  # check_regions() checks structure/type/NA only; the VALUES (0 for a size,
  # -1 for a position) were refused by ffm_drawbox()'s check_dim() until M65.
  # The bad value sits in a different row of a 2-row frame per field.
  region_bad <- list(x = list(bad = -1, row = 2L), y = list(bad = -1, row = 1L),
                     width = list(bad = 0, row = 2L),
                     height = list(bad = 0, row = 1L))
  for (field in names(region_bad)) {
    spec <- region_bad[[field]]
    frame <- data.frame(x = c(10, 20), y = c(10, 20),
                        width = c(30, 40), height = c(30, 40))
    frame[[field]][[spec$row]] <- spec$bad
    add(id = paste0("anonymize_video/regions-", field),
        verb = "anonymize_video", form = "scalar", delivery = "arg",
        argument = "regions", field = field, own = dim_msg(field),
        args = list(infile = input, outfile = "o.mp4", regions = frame))
    add(id = paste0("anonymize_video_batch/regions-", field, "/column"),
        verb = "anonymize_video_batch", form = "batch", delivery = "column",
        argument = "regions", field = field, own = dim_msg(field),
        args = list(jobs = one(input = input, output = "o.mp4",
                               regions = list(frame))))
  }

  # --- picture_in_picture / _batch: the `scale` RANGE -> ffm_overlay() -------
  # AC5's pairing: the existing type check and the new range refusal are
  # distinguished at range grain, each cell asserting the other's wording is
  # absent. The type cells are pinned (the front doors have always carried
  # check_number_decimal(scale)); the range cells are M65's.
  add(id = "picture_in_picture/scale-range", verb = "picture_in_picture",
      form = "scalar", delivery = "arg", argument = "scale",
      own = scale_range_msg, absent = "must be a number",
      args = list(main = input, overlay = input, outfile = "o.mp4", scale = 2))
  add(id = "picture_in_picture/scale-type", verb = "picture_in_picture",
      form = "scalar", delivery = "arg", argument = "scale",
      own = scale_type_msg, absent = "greater than 0 and at most 1",
      pinned = TRUE,
      args = list(main = input, overlay = input, outfile = "o.mp4",
                  scale = "x"))
  pip_jobs <- function(...) {
    one(main = input, overlay = input, output = "o.mp4", ...)
  }
  add(id = "picture_in_picture_batch/scale-range/arg",
      verb = "picture_in_picture_batch", form = "batch", delivery = "arg",
      argument = "scale", own = scale_range_msg, absent = "must be a number",
      args = list(jobs = pip_jobs(), scale = 2))
  add(id = "picture_in_picture_batch/scale-range/column",
      verb = "picture_in_picture_batch", form = "batch", delivery = "column",
      argument = "scale", own = scale_range_msg, absent = "must be a number",
      args = list(jobs = pip_jobs(scale = 2)))
  add(id = "picture_in_picture_batch/scale-type/arg",
      verb = "picture_in_picture_batch", form = "batch", delivery = "arg",
      argument = "scale", own = scale_type_msg,
      absent = "greater than 0 and at most 1", pinned = TRUE,
      args = list(jobs = pip_jobs(), scale = "x"))
  # In the column form the non-numeric refusal is the front-door column TYPE
  # guard, whose wording names the column rather than the argument -- still
  # this verb's own abort, still range-free (AC5's grain), and pinned: the
  # guard predates M65.
  add(id = "picture_in_picture_batch/scale-type/column",
      verb = "picture_in_picture_batch", form = "batch", delivery = "column",
      argument = "scale", own = "column of `jobs` must be numeric",
      absent = "greater than 0 and at most 1", pinned = TRUE,
      args = list(jobs = pip_jobs(scale = "x")))

  # --- normalize_audio / _batch: the three loudness targets -> ffm_loudnorm()
  loud_bad <- list(target_loudness = -100, true_peak = 5, loudness_range = 0)
  for (arg in names(loud_bad)) {
    bad <- loud_bad[[arg]]
    for (two_pass in c(FALSE, TRUE)) {
      scalar_args <- list(infile = input, outfile = "o.wav",
                          two_pass = two_pass)
      scalar_args[[arg]] <- bad
      add(id = paste0("normalize_audio/", arg,
                      if (two_pass) "/two-pass" else "/single-pass"),
          verb = "normalize_audio", form = "scalar", delivery = "arg",
          argument = arg, own = loud_msg(arg),
          needs_ffmpeg = two_pass, args = scalar_args)
    }
    batch_args <- list(jobs = one(input = input, output = "o.wav"))
    batch_args[[arg]] <- bad
    add(id = paste0("normalize_audio_batch/", arg, "/arg"),
        verb = "normalize_audio_batch", form = "batch", delivery = "arg",
        argument = arg, own = loud_msg(arg), args = batch_args)
    col_jobs <- one(input = input, output = "o.wav")
    col_jobs[[arg]] <- bad
    add(id = paste0("normalize_audio_batch/", arg, "/column"),
        verb = "normalize_audio_batch", form = "batch", delivery = "column",
        argument = arg, own = loud_msg(arg), args = list(jobs = col_jobs))
  }

  specs
}

# The completeness reader (AC3). It cannot prove the list COMPLETE -- no
# procedure enumerates "every value a verb hands to a builder" -- so it checks
# what is mechanical: every declared cell names a formal of its verb, a
# column-delivery cell really carries that column, and a `regions` cell names
# a check_regions() field that its delivered frame actually has. A cell whose
# argument, column, or field is a typo would otherwise probe nothing and pass.
blame_spec_defects_m65 <- function(specs) {
  region_fields <- c("x", "y", "width", "height")
  defects <- character(0)
  flag <- function(cell, why) {
    defects <<- c(defects, paste0(cell$id, ": ", why))
  }
  for (cell in specs) {
    fmls <- names(formals(get(cell$verb, envir = asNamespace("tidymedia"))))
    if (identical(cell$argument, "regions")) {
      # A region cell names a check_regions() field, and the frame it delivers
      # -- the `regions` argument (scalar) or the row's `regions` cell (batch)
      # -- must carry that field.
      if (is.null(cell$field) || !cell$field %in% region_fields) {
        named <- if (is.null(cell$field)) "<missing>" else cell$field
        flag(cell, paste0("`", named, "` is not a check_regions() field"))
        next
      }
      frame <- if (identical(cell$form, "batch")) {
        if (!"regions" %in% names(cell$args$jobs)) {
          flag(cell, "no `regions` column in the jobs table")
          next
        }
        cell$args$jobs$regions[[1]]
      } else {
        if (!"regions" %in% fmls || !"regions" %in% names(cell$args)) {
          flag(cell, "`regions` is not a formal, or not passed")
          next
        }
        cell$args$regions
      }
      if (!cell$field %in% names(frame)) {
        flag(cell, paste0("no `", cell$field, "` field in the regions frame"))
      }
    } else {
      if (!cell$argument %in% fmls) {
        flag(cell, paste0("`", cell$argument, "` is not a formal of ",
                          cell$verb, "()"))
      }
      if (identical(cell$delivery, "column")) {
        if (!cell$argument %in% names(cell$args$jobs)) {
          flag(cell, paste0("no `", cell$argument,
                            "` column in the jobs table"))
        }
      } else if (identical(cell$delivery, "arg")) {
        if (!cell$argument %in% names(cell$args)) {
          flag(cell, paste0("`", cell$argument,
                            "` is not passed as an argument"))
        }
        if (!is.null(cell$args$jobs) &&
            cell$argument %in% names(cell$args$jobs)) {
          flag(cell, "argument delivery shadowed by a column of the same name")
        }
      }
    }
  }
  defects
}
