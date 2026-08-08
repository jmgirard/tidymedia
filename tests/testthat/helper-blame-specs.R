# The M64 blame grid's spec list: every (verb, form, delivery, argument,
# violating value) cell whose abort must name the verb the caller typed.
#
# One declaration, read by three consumers: the grid in
# test-builder-blame-front-door.R, and -- from the source tree, since they are
# developer scripts that never run under `R CMD check` --
# data-raw/blame-baseline.R and data-raw/blame-precedence.R. The list lives HERE
# rather than in data-raw/ because `^data-raw$` is in .Rbuildignore: a test
# sourcing it from there finds nothing under `R CMD check` and skips, leaving
# the guard unenforced in exactly the run the release gate uses (LESSONS
# M51/M59). A copy in each place would drift with nothing able to see it.
#
# THE LIST IS CLOSED BY INSPECTION, not by a procedure. It was built by reading
# each verb's front door and its `*_pipeline()` for values handed to a
# validating `ffm_*` builder; nothing enumerates "every leaking argument" for
# us, so a leak nobody read is a leak this grid does not cover. What IS
# mechanical is `blame_spec_defects()` below, which catches a cell naming an
# argument the verb does not have -- a typo, not an omission.
#
# `delivery` is "arg" (the value passed as the verb's own argument) or "column"
# (carried in a `jobs` column). They are not equivalent: batch_arg_rows()
# resolves a column OVER the argument, so a sweep reading only one of them
# passes the other straight through to the fan-out.
#
# `pinned = TRUE` marks a cell that already blames its verb on master. It is
# evidence that this milestone did not REGRESS the case, never evidence that it
# fixed one, and no mutation in data-raw/blame-guard-mutations.py owns it.

blame_specs <- function(input, outdir = tempfile("frames")) {
  one <- function(...) tibble::tibble(...)

  # check_dim() refuses 0 for a size (strictly positive) and -1 for a position
  # (non-negative); check_token() refuses whitespace; resolve_sample_fps()
  # refuses a non-positive rate.
  dim_msg <- "must be a single FFmpeg expression or number"
  # The arg name is part of the assertion: ffm_pixel_format()'s parameter is
  # named `format`, so before M64 this message told the caller to fix an
  # argument the verb does not have (M64-D1). Matching the name pins the fix.
  token_msg <- "`pixel_format` must be a single clean token"
  fps_msg <- "must be a single positive number or a string"
  interval_msg <- "must be a single positive number"

  specs <- list()
  add <- function(...) specs[[length(specs) + 1L]] <<- list(...)

  # --- crop_video / crop_video_batch: width, height, x, y -> ffm_crop() ------
  crop_bad <- list(width = 0, height = 0, x = -1, y = -1)
  for (arg in names(crop_bad)) {
    bad <- crop_bad[[arg]]
    scalar_args <- list(infile = input, outfile = "o.mp4",
                        width = 160, height = 120)
    scalar_args[[arg]] <- bad
    add(id = paste0("crop_video/", arg), verb = "crop_video",
        form = "scalar", delivery = "arg", argument = arg,
        own = dim_msg, args = scalar_args)

    # width/height already blamed this verb before M64: M59 swept them here,
    # and left x/y to the fan-out. Pinned, so the mutation ledger does not
    # credit an M64 sweep with a cell M59 fixed.
    already <- arg %in% c("width", "height")

    batch_args <- list(jobs = one(input = input, output = "o.mp4"),
                       width = 160, height = 120)
    batch_args[[arg]] <- bad
    add(id = paste0("crop_video_batch/", arg, "/arg"),
        verb = "crop_video_batch", form = "batch", delivery = "arg",
        argument = arg, own = dim_msg, pinned = already, args = batch_args)

    col_jobs <- one(input = input, output = "o.mp4")
    col_jobs[[arg]] <- bad
    add(id = paste0("crop_video_batch/", arg, "/column"),
        verb = "crop_video_batch", form = "batch", delivery = "column",
        argument = arg, own = dim_msg, pinned = already,
        args = list(jobs = col_jobs, width = 160, height = 120))
  }

  # --- standardize_video / _batch: width, height, fps -> ffm_scale/ffm_fps,
  #     pixel_format -> ffm_pixel_format() (which renames the arg to `format`) -
  std_bad <- list(width = 0, height = 0, fps = 0, pixel_format = "yuv 420p")
  for (arg in names(std_bad)) {
    bad <- std_bad[[arg]]
    msg <- if (arg == "pixel_format") token_msg else dim_msg
    scalar_args <- list(infile = input, outfile = "o.mp4")
    scalar_args[[arg]] <- bad
    add(id = paste0("standardize_video/", arg), verb = "standardize_video",
        form = "scalar", delivery = "arg", argument = arg,
        own = msg, args = scalar_args)

    batch_args <- list(jobs = one(input = input, output = "o.mp4"))
    batch_args[[arg]] <- bad
    add(id = paste0("standardize_video_batch/", arg, "/arg"),
        verb = "standardize_video_batch", form = "batch", delivery = "arg",
        argument = arg, own = msg, args = batch_args)

    col_jobs <- one(input = input, output = "o.mp4")
    col_jobs[[arg]] <- bad
    add(id = paste0("standardize_video_batch/", arg, "/column"),
        verb = "standardize_video_batch", form = "batch", delivery = "column",
        argument = arg, own = msg, args = list(jobs = col_jobs))
  }

  # --- sample_frames / _batch: the resolved rate -> ffm_fps() ---------------
  #
  # The scalar form already blames itself: resolve_sample_fps() carries `call`
  # and runs at the front door, so nothing here reaches ffm_fps(). Pinned, not
  # fixed. The BATCH form resolves per row inside the fan-out closure, which is
  # the leak this milestone closes.
  rate_bad <- list(fps = list(bad = 0, own = fps_msg, absent = NULL),
                   interval = list(bad = 0, own = interval_msg,
                                   absent = "or a string"))
  for (arg in names(rate_bad)) {
    spec <- rate_bad[[arg]]
    scalar_args <- list(infile = input, outdir = outdir)
    scalar_args[[arg]] <- spec$bad
    add(id = paste0("sample_frames/", arg), verb = "sample_frames",
        form = "scalar", delivery = "arg", argument = arg,
        own = spec$own, absent = spec$absent, pinned = TRUE,
        args = scalar_args)

    batch_args <- list(jobs = one(input = input), outdir = outdir)
    batch_args[[arg]] <- spec$bad
    add(id = paste0("sample_frames_batch/", arg, "/arg"),
        verb = "sample_frames_batch", form = "batch", delivery = "arg",
        argument = arg, own = spec$own, absent = spec$absent,
        args = batch_args)

    col_jobs <- one(input = input)
    col_jobs[[arg]] <- spec$bad
    add(id = paste0("sample_frames_batch/", arg, "/column"),
        verb = "sample_frames_batch", form = "batch", delivery = "column",
        argument = arg, own = spec$own, absent = spec$absent,
        args = list(jobs = col_jobs, outdir = outdir))
  }

  specs
}

# The completeness reader. It cannot tell us the list is COMPLETE -- no
# procedure enumerates "every argument a verb hands to a builder" -- so it
# checks the one thing that is mechanical: every declared cell names an
# argument the verb actually has, and every column-delivery cell really
# carries that column. A cell whose `argument` is a typo would otherwise
# probe nothing and pass, which is how M62's grid read green through two
# defects.
blame_spec_defects <- function(specs) {
  defects <- character(0)
  for (cell in specs) {
    fmls <- names(formals(get(cell$verb, envir = asNamespace("tidymedia"))))
    if (!cell$argument %in% fmls) {
      defects <- c(defects, paste0(cell$id, ": `", cell$argument,
                                   "` is not a formal of ", cell$verb, "()"))
    }
    if (identical(cell$delivery, "column")) {
      if (!cell$argument %in% names(cell$args$jobs)) {
        defects <- c(defects, paste0(cell$id, ": no `", cell$argument,
                                     "` column in the jobs table"))
      }
    } else if (identical(cell$delivery, "arg")) {
      if (!cell$argument %in% names(cell$args)) {
        defects <- c(defects, paste0(cell$id, ": `", cell$argument,
                                     "` is not passed as an argument"))
      }
      if (!is.null(cell$args$jobs) &&
          cell$argument %in% names(cell$args$jobs)) {
        defects <- c(defects, paste0(cell$id, ": argument delivery shadowed ",
                                     "by a column of the same name"))
      }
    }
  }
  defects
}
