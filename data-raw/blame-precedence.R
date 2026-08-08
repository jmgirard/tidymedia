# blame-precedence.R ---------------------------------------------------------
#
# Regenerate M64's precedence evidence from an arbitrary git ref, so the claim
# "the new sweeps reassigned no reporting order except the one the reordering
# table names" is re-derivable rather than an implementation-time transcript
# (M64 T5, AC4).
#
# Same ref machinery as `data-raw/blame-baseline.R`: both source
# `data-raw/codec-guard-baseline.R` for its `git show`-into-an-environment
# helper rather than copying it.
#
# Each CELL is a crossing: one call wrong about two things at once -- a value
# one of M64's new sweeps refuses, AND the condition of one other guard on the
# same verb's path. The recorded `winner` says which complaint the caller gets,
# which is exactly the behaviour a guard's placement silently decides (M41
# review A6, M61). Compare two refs cell-for-cell and the rows whose winner
# moved are the reporting orders this milestone reassigned.
#
# THE CROSSING LIST IS CLOSED BY INSPECTION, not by a procedure. It was built
# by reading each of the five swept verbs' front door and its `*_pipeline()`
# top to bottom and listing every argument-triggerable guard on the path;
# nothing enumerates "every guard" for us. Deliberately not crossed:
# environment-dependent aborts that no argument value triggers on its own
# (ensure_dir()'s uncreatable directory), aborts reachable only at run = TRUE
# (every probe here compiles only), and `sample_frames()`'s scalar guards --
# that verb gained no sweep (pinned, helper-blame-specs.R), so it has no new
# sweep to cross. The NA branches of a guard already crossed via its type
# branch are crossed only where the NA check is a separate abort in the verb's
# own text (the dim-column and fps-column NA loops).
#
# Every cell carries a CONTROL: the same call with the sweep's value put back
# to a legal one, so only the crossed guard's condition remains violated. The
# control must abort with the crossed guard's own wording -- a cell whose
# control does not is measuring a guard that cannot fire on that call, and it
# FAILS (reported by precedence_dead_controls()), never excluded: a dead
# control is how a crossing reads "same winner on both refs" while the crossed
# guard was never live at all (the M58/M61 dead-cell trap, AC4's own wording).
#
# The nvenc encoder seam is held EMPTY for the whole grid, as in
# blame-baseline.R -- here it is load-bearing: the nvenc crossings set
# hardware = "nvenc" and need "this build lists no nvenc encoder" to be true by
# construction on every machine. Every probe runs at `run = FALSE`.
#
# Usage:
#
#   source("data-raw/blame-precedence.R")
#   before <- blame_precedence("origin/master")
#   after  <- blame_precedence()               # the working tree
#   precedence_dead_controls(before)           # empty: every guard live
#   precedence_dead_controls(after)            # empty
#   precedence_unresolved(before)              # empty: every winner identified
#   precedence_unresolved(after)               # empty
#   precedence_flips(before, after)            # the reordering table's rows

source(file.path("data-raw", "codec-guard-baseline.R"))

# -- the crossing grid --------------------------------------------------------

# Message markers, matched against the ansi-stripped condition message to name
# the guard that reported. Each is a fragment of a wording written at exactly
# one site in R/, so matching it identifies the site; the arg name is included
# wherever two guards share a sentence (check_dim() on `width` vs on `x`).
m_dim <- function(arg) paste0("`", arg, "` must be a single FFmpeg expression or number")
m_token <- function(arg) paste0("`", arg, "` must be a single clean token")
# The pixel-format wording is one site (check_token()) under TWO arg spellings:
# the merge-base reaches it through ffm_pixel_format(), whose parameter is
# named `format`, and the branch names the verb's own `pixel_format` (M64-D1).
# The marker accepts both, because both are that same guard reporting -- a
# marker pinned to one spelling reads the other ref's identical guard as
# "unmatched"/"dead" and manufactures a flip that is only the argument rename.
m_pixfmt <- "`(pixel_)?format` must be a single clean token"
m_string <- function(arg) paste0("`", arg, "` must be a single string")
m_fps <- "`fps` must be a single positive number or a string"
m_unreadable <- "can't be found or read"
m_hardware <- "`hardware` must be one of"
m_whole <- "must be a whole number"
m_nvenc <- "is not available"
m_jobs_df <- "must be a data frame with one row per input"
m_jobs_rows <- "must have at least one row"
m_input_col <- "must have an .?input.? column"
m_col_numchr <- function(col) paste0("The .?", col, ".? column of `jobs` must be numeric or character")
m_col_na <- function(col) paste0("The .?", col, ".? column of `jobs` must not contain")
m_col_chr <- function(col) paste0("The .?", col, ".? column of `jobs` must be character")
m_col_num <- function(col) paste0("The .?", col, ".? column of `jobs` must be numeric")
m_dup_out <- "resolve to the same output path"
m_dup_in <- "duplicated .?input.? paths but no .?output.? column"
m_width_req <- "`width` is required"
m_collision <- "would write to the same image sequence"
m_format <- "must be a supported image format"
m_rate_xor <- "Provide exactly one of"

blame_precedence_cells <- function(input,
                                   outdir = file.path(tempdir(),
                                                      "m64-precedence")) {
  one <- function(...) tibble::tibble(...)
  missing_file <- file.path(dirname(input), "no-such-file-m64.mp4")
  cells <- list()

  # One sweep block at a time. `base` is a valid call; `bad`/`good` overlay the
  # swept argument's violating and legal value; each guard supplies `hit`, a
  # function editing the args to violate that guard, plus the marker naming its
  # wording. Cross call = hit(base + bad); control call = hit(base + good).
  declare <- function(sweep, verb, base, bad, good, sweep_marker, guards) {
    for (g in names(guards)) {
      cells[[length(cells) + 1L]] <<- list(
        id = paste0(sweep, " x ", g), sweep = sweep, verb = verb,
        crossed = g, base = base, bad = bad, good = good,
        sweep_marker = sweep_marker, crossed_marker = guards[[g]]$marker,
        hit = guards[[g]]$hit)
    }
  }
  guard <- function(marker, hit) list(marker = marker, hit = hit)
  set <- function(...) {
    overlay <- list(...)
    function(args) { for (nm in names(overlay)) args[nm] <- overlay[nm]; args }
  }

  # --- S1: crop_video's geometry sweep (rep: width = 0) ----------------------
  declare(
    "crop_video/geometry", "crop_video",
    base = list(infile = input, outfile = "o.mp4", width = 160, height = 120),
    bad = list(width = 0), good = list(width = 160),
    sweep_marker = m_dim("width"),
    guards = list(
      `infile-unreadable` = guard(m_unreadable, set(infile = missing_file)),
      `outfile-not-string` = guard(m_string("outfile"), set(outfile = 1)),
      `video_codec-not-string` = guard(m_string("video_codec"),
                                       set(video_codec = 1)),
      `audio_codec-not-string` = guard(m_string("audio_codec"),
                                       set(audio_codec = 1)),
      `hardware-unknown` = guard(m_hardware, set(hardware = "cuda")),
      `audio_stream-fractional` = guard(m_whole, set(audio_stream = 1.5)),
      `nvenc-unavailable` = guard(m_nvenc, set(hardware = "nvenc"))
    ))

  # --- S2: crop_video_batch's x/y extension (rep: x = -1, as argument) -------
  declare(
    "crop_video_batch/xy", "crop_video_batch",
    base = list(jobs = one(input = input, output = "o.mp4"),
                width = 160, height = 120),
    bad = list(x = -1), good = list(x = 10),
    sweep_marker = m_dim("x"),
    guards = list(
      `video_codec-token` = guard(m_token("video_codec"),
                                  set(video_codec = "a b")),
      `audio_codec-token` = guard(m_token("audio_codec"),
                                  set(audio_codec = "a b")),
      `hardware-unknown` = guard(m_hardware, set(hardware = "cuda")),
      `jobs-not-df` = guard(m_jobs_df, set(jobs = "oops")),
      `jobs-empty` = guard(m_jobs_rows,
                           set(jobs = one(input = character(0),
                                          output = character(0)))),
      `input-col-missing` = guard(m_input_col,
                                  set(jobs = one(output = "o.mp4"))),
      `width-required` = guard(m_width_req,
                               function(args) { args$width <- NULL; args }),
      `y-col-type` = guard(m_col_numchr("y"), function(args) {
        args$jobs$y <- TRUE; args }),
      `y-col-na` = guard(m_col_na("y"), function(args) {
        args$jobs$y <- NA_real_; args }),
      `video_codec-col-type` = guard(m_col_chr("video_codec"), function(args) {
        args$jobs$video_codec <- 1; args }),
      `audio_stream-col-type` = guard(m_col_num("audio_stream"),
                                      function(args) {
        args$jobs$audio_stream <- "a"; args }),
      `audio_stream-fractional` = guard(m_whole, set(audio_stream = 1.5)),
      `duplicate-outputs` = guard(m_dup_out,
                                  set(jobs = one(input = c(input, input),
                                                 output = "o.mp4"))),
      `input-missing` = guard(m_unreadable, function(args) {
        args$jobs$input <- missing_file; args }),
      `m59-width-sweep` = guard(m_dim("width"), set(width = 0)),
      `nvenc-unavailable` = guard(m_nvenc, set(hardware = "nvenc"))
    ))

  # --- S3: standardize_video's dimension sweep (rep: width = 0) --------------
  declare(
    "standardize_video/dims", "standardize_video",
    base = list(infile = input, outfile = "o.mp4"),
    bad = list(width = 0), good = list(width = 640),
    sweep_marker = m_dim("width"),
    guards = list(
      `infile-unreadable` = guard(m_unreadable, set(infile = missing_file)),
      `outfile-not-string` = guard(m_string("outfile"), set(outfile = 1)),
      `hardware-unknown` = guard(m_hardware, set(hardware = "cuda")),
      `video_codec-not-string` = guard(m_string("video_codec"),
                                       set(video_codec = 1)),
      `video_codec-token` = guard(m_token("video_codec"),
                                  set(video_codec = "a b")),
      `audio_codec-token` = guard(m_token("audio_codec"),
                                  set(audio_codec = "a b")),
      `pixel_format-token` = guard(m_pixfmt,
                                   set(pixel_format = "yuv 420p")),
      `audio_stream-fractional` = guard(m_whole, set(audio_stream = 1.5)),
      `nvenc-unavailable` = guard(m_nvenc, set(hardware = "nvenc"))
    ))

  # --- S4: standardize_video's pixel_format check, `call`-threaded in the
  #     pipeline (rep: "yuv 420p"). Its position is claimed UNMOVED, so every
  #     one of these must answer the same on both refs -- including nvenc,
  #     which reports before it, unlike on S3. -------------------------------
  declare(
    "standardize_video/pixel_format", "standardize_video",
    base = list(infile = input, outfile = "o.mp4"),
    bad = list(pixel_format = "yuv 420p"), good = list(pixel_format = "yuv420p"),
    sweep_marker = m_pixfmt,
    guards = list(
      `infile-unreadable` = guard(m_unreadable, set(infile = missing_file)),
      `outfile-not-string` = guard(m_string("outfile"), set(outfile = 1)),
      `hardware-unknown` = guard(m_hardware, set(hardware = "cuda")),
      `video_codec-not-string` = guard(m_string("video_codec"),
                                       set(video_codec = 1)),
      `video_codec-token` = guard(m_token("video_codec"),
                                  set(video_codec = "a b")),
      `audio_codec-token` = guard(m_token("audio_codec"),
                                  set(audio_codec = "a b")),
      `dims-sweep` = guard(m_dim("width"), set(width = 0)),
      `audio_stream-fractional` = guard(m_whole, set(audio_stream = 1.5)),
      `nvenc-unavailable` = guard(m_nvenc, set(hardware = "nvenc"))
    ))

  # --- S5a/S5b: standardize_video_batch's sweep, its two checkers ------------
  std_batch_guards <- function(extra = list()) c(list(
    `hardware-unknown` = guard(m_hardware, set(hardware = "cuda")),
    `audio_codec-token` = guard(m_token("audio_codec"),
                                set(audio_codec = "a b")),
    `jobs-not-df` = guard(m_jobs_df, set(jobs = "oops")),
    `jobs-empty` = guard(m_jobs_rows,
                         set(jobs = one(input = character(0),
                                        output = character(0)))),
    `input-col-missing` = guard(m_input_col, set(jobs = one(output = "o.mp4"))),
    `height-col-type` = guard(m_col_numchr("height"), function(args) {
      args$jobs$height <- TRUE; args }),
    `height-col-na` = guard(m_col_na("height"), function(args) {
      args$jobs$height <- NA_real_; args }),
    `pixel_format-col-type` = guard(m_col_chr("pixel_format"), function(args) {
      args$jobs$pixel_format <- 1; args }),
    `video_codec-col-type` = guard(m_col_chr("video_codec"), function(args) {
      args$jobs$video_codec <- 1; args }),
    `audio_stream-col-type` = guard(m_col_num("audio_stream"), function(args) {
      args$jobs$audio_stream <- "a"; args }),
    `video_codec-token` = guard(m_token("video_codec"),
                                set(video_codec = "a b")),
    `audio_stream-fractional` = guard(m_whole, set(audio_stream = 1.5)),
    `duplicate-inputs-no-output` = guard(m_dup_in,
                                         set(jobs = one(input = c(input,
                                                                  input)))),
    `input-missing` = guard(m_unreadable, function(args) {
      args$jobs$input <- missing_file; args }),
    `nvenc-unavailable` = guard(m_nvenc, set(hardware = "nvenc"))
  ), extra)

  declare(
    "standardize_video_batch/dims", "standardize_video_batch",
    base = list(jobs = one(input = input, output = "o.mp4")),
    bad = list(width = 0), good = list(width = 640),
    sweep_marker = m_dim("width"),
    guards = std_batch_guards())

  declare(
    "standardize_video_batch/pixel_format", "standardize_video_batch",
    base = list(jobs = one(input = input, output = "o.mp4")),
    bad = list(pixel_format = "yuv 420p"), good = list(pixel_format = "yuv420p"),
    sweep_marker = m_pixfmt,
    guards = std_batch_guards(list(
      `dims-sweep` = guard(m_dim("width"), set(width = 0)))))

  # --- S6: sample_frames_batch's per-row rate sweep (rep: fps = 0) -----------
  declare(
    "sample_frames_batch/rate", "sample_frames_batch",
    base = list(jobs = one(input = input), outdir = outdir),
    bad = list(fps = 0), good = list(fps = 30),
    sweep_marker = m_fps,
    guards = list(
      `jobs-not-df` = guard(m_jobs_df, set(jobs = "oops")),
      `jobs-empty` = guard(m_jobs_rows,
                           set(jobs = one(input = character(0)))),
      `input-col-missing` = guard(m_input_col, set(jobs = one(x = 1))),
      `format-unsupported` = guard(m_format, set(format = "mp4")),
      `rate-exclusivity` = guard(m_rate_xor, set(interval = 5)),
      `fps-col-type` = guard(m_col_numchr("fps"), function(args) {
        args$jobs$fps <- TRUE; args }),
      `fps-col-na` = guard(m_col_na("fps"), function(args) {
        args$jobs$fps <- NA_real_; args }),
      `outdir-col-type` = guard(m_col_chr("outdir"), function(args) {
        args$jobs$outdir <- 1; args }),
      `pattern-collision` = guard(m_collision,
                                  set(jobs = one(input = c(input, input)))),
      `input-missing` = guard(m_unreadable, function(args) {
        args$jobs$input <- missing_file; args })
    ))

  cells
}

# -- running the grid ---------------------------------------------------------

blame_precedence <- function(ref = NULL, root = ".") {
  env <- codec_guard_env(ref, root)
  sample <- normalizePath(
    file.path(root, "inst", "extdata", "sample.mp4"), mustWork = TRUE)
  old <- options(tidymedia.nvenc_encoders = character(0))
  on.exit(options(old), add = TRUE)

  apply_overlay <- function(args, overlay) {
    for (nm in names(overlay)) args[nm] <- overlay[nm]
    args
  }
  probe <- function(verb, args) {
    f <- get(verb, envir = env)
    if ("run" %in% names(formals(f))) args$run <- FALSE
    if ("parallel" %in% names(formals(f))) args$parallel <- FALSE
    tryCatch(
      {
        # Call by NAME (as in blame-baseline.R): do.call() on a function OBJECT
        # would record the anonymous function as the condition call.
        do.call(verb, args, envir = env)
        list(kind = "compiled", msg = NA_character_)
      },
      condition = function(cnd) {
        msg <- tryCatch(
          paste(cli::ansi_strip(conditionMessage(cnd)), collapse = "\n"),
          error = function(e) conditionMessage(cnd))
        list(kind = if (inherits(cnd, "error")) "abort" else "condition",
             msg = msg)
      }
    )
  }
  # Who reported? The sweep's marker and the crossed guard's marker are checked
  # in that order; a cross message matching NEITHER (or a cross that compiled)
  # is "unresolved" -- a cell measuring something other than its two guards,
  # surfaced by precedence_unresolved() rather than compared as if it answered.
  winner_of <- function(obs, cell) {
    if (obs$kind != "abort") return(paste0("no-abort:", obs$kind))
    if (grepl(cell$sweep_marker, obs$msg)) return("sweep")
    if (grepl(cell$crossed_marker, obs$msg)) return("crossed")
    "unmatched"
  }

  rows <- lapply(blame_precedence_cells(sample), function(cell) {
    cross <- probe(cell$verb, cell$hit(apply_overlay(cell$base, cell$bad)))
    control <- probe(cell$verb, cell$hit(apply_overlay(cell$base, cell$good)))
    control_ok <- control$kind == "abort" &&
      grepl(cell$crossed_marker, control$msg)
    data.frame(
      id = cell$id, verb = cell$verb, crossed = cell$crossed,
      winner = winner_of(cross, cell),
      control = if (control_ok) "live" else
        paste0("dead: ", control$kind, " ",
               sub("\n.*$", "", control$msg %||% "")),
      cross_msg = if (is.na(cross$msg)) NA_character_ else
        sub("\n.*$", "", cross$msg),
      stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, rows)
  attr(out, "ref") <- if (is.null(ref)) "<working tree>" else ref
  out
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# -- reading the result -------------------------------------------------------

# Cells whose control did not abort with the crossed guard's wording. AC4:
# such a cell FAILS -- the crossed guard is not live on that call, so its
# crossing compares "same winner" while measuring nothing. Run on BOTH refs.
precedence_dead_controls <- function(x) {
  x[x$control != "live", c("id", "control")]
}

# Cells whose cross call compiled, or aborted matching neither marker. Same
# standing as a dead control: the cell is not answering the question its
# crossing asks. Run on BOTH refs before trusting a flip comparison.
precedence_unresolved <- function(x) {
  x[!x$winner %in% c("sweep", "crossed"), c("id", "winner", "cross_msg")]
}

# The crossings whose winner moved between two refs. AC4 names the exact set
# this may contain: the nvenc-unavailable crossings of the two `_batch` sweeps,
# where a machine-independent refusal now reports before the machine-dependent
# one (the gated AC4 amendment; D036). Every one of these rows belongs in the
# milestone file's reordering table, naming the call whose answer it changes.
precedence_flips <- function(before, after) {
  stopifnot(identical(before$id, after$id))
  moved <- before$winner != after$winner
  data.frame(id = before$id[moved],
             before = before$winner[moved], after = after$winner[moved],
             stringsAsFactors = FALSE)
}
