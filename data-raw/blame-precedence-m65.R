# blame-precedence-m65.R ------------------------------------------------------
#
# Regenerate M65's precedence evidence from an arbitrary git ref -- the sibling
# of data-raw/blame-precedence.R (M64) for this milestone's six sweeps: the
# region-value sweep at anonymize_video()'s two ends, the overlay-scale range
# sweep at picture_in_picture()'s two ends, and the loudness-target sweep at
# normalize_audio()'s two ends. The runner, the dead-control reader, the
# unresolved reader and the flip reader are all M64's, sourced below and reused
# via its `cells` parameter; only the crossing grid is declared here.
#
# THE CROSSING LIST IS CLOSED BY INSPECTION, not by a procedure: it was built
# by reading each of the six swept front doors and their `*_pipeline()` helpers
# top to bottom and listing every argument-triggerable guard on the path. As in
# M64, deliberately not crossed: environment-dependent aborts no argument value
# triggers (ensure_dir()), aborts reachable only at run = TRUE, and guards on
# paths the swept value cannot share. `normalize_audio`'s crossings carry the
# `two_pass` axis, because the five guards AC4 names live inside the two_pass
# block: the TRUE cells cross them where they sit, and the FALSE cells cross
# the single-pass path's own copies (pipeline checks and the post-block
# check_string). Every two-pass crossed guard sits ABOVE the analysis pass, so
# no cell here -- cross or control -- ever reaches FFmpeg.
#
# Every cell carries a CONTROL (the sweep's value put back to a legal one); a
# cell whose control does not abort with the crossed guard's own wording FAILS,
# it is not excluded (AC4, the M58/M61 dead-cell trap).
#
# The nvenc encoder seam is held EMPTY by the runner, as in M64 -- load-bearing
# for the nvenc crossings. Every probe runs at `run = FALSE`.
#
# Usage:
#
#   source("data-raw/blame-precedence-m65.R")
#   before <- blame_precedence_m65("master")   # the merge-base
#   after  <- blame_precedence_m65()           # the working tree
#   precedence_dead_controls(before); precedence_dead_controls(after)  # empty
#   precedence_unresolved(before); precedence_unresolved(after)        # empty
#   precedence_flips(before, after)    # the milestone reordering table's rows

source(file.path("data-raw", "blame-precedence.R"))

# -- the M65 markers (M64's m_* helpers are in scope from the source() above) --

m_scale_range <- "`scale` must be greater than 0 and at most 1"
m_scale_type <- "`scale` must be a number"
m_loud <- function(arg) paste0("`", arg, "` must be a number between ")
m_contradiction <- "`audio_codec` needs an audio stream to encode"
# "can't be" without the quoted value: cli's quote glyphs vary by locale, and
# the fragment is unique to this guard's wording.
m_copy <- "`audio_codec` can't be"
m_regions_col <- "required column"
m_position <- "`position` must be one of"
m_col_whole_2p <- function(col) {
  paste0("The .?", col, ".? column of `jobs` must be whole numbers")
}
# The fan-in verb's jobs wording counts outputs, not inputs (D015).
m_jobs_df_out <- "must be a data frame with one row per output"

blame_precedence_cells_m65 <- function(input) {
  one <- function(...) tibble::tibble(...)
  missing_file <- file.path(dirname(input), "no-such-file-m65.mp4")
  cells <- list()

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

  bad_regions <- data.frame(x = c(10, 20), y = c(10, 20),
                            width = c(30, 0), height = c(30, 40))
  good_regions <- data.frame(x = c(10, 20), y = c(10, 20),
                             width = c(30, 40), height = c(30, 40))

  # --- S1: anonymize_video's region-value sweep (rep: width = 0, row 2) ------
  # The structure crossing drops the HEIGHT column, never width: the bad width
  # value must stay in the frame, or the cross would carry only one violation
  # and compare nothing.
  declare(
    "anonymize_video/regions", "anonymize_video",
    base = list(infile = input, outfile = "o.mp4", regions = good_regions),
    bad = list(regions = bad_regions),
    good = list(regions = good_regions),
    sweep_marker = m_dim("width"),
    guards = list(
      `infile-unreadable` = guard(m_unreadable, set(infile = missing_file)),
      `outfile-not-string` = guard(m_string("outfile"), set(outfile = 1)),
      `hardware-unknown` = guard(m_hardware, set(hardware = "cuda")),
      `audio_stream-fractional` = guard(m_whole, set(audio_stream = 1.5)),
      `regions-structure` = guard(m_regions_col, function(args) {
        args$regions["height"] <- NULL; args }),
      `color-not-string` = guard(m_string("color"), set(color = 1)),
      `video_codec-token` = guard(m_token("video_codec"),
                                  set(video_codec = "a b")),
      `pixel_format-token` = guard(m_pixfmt, set(pixel_format = "yuv 420p")),
      `nvenc-unavailable` = guard(m_nvenc, set(hardware = "nvenc"))
    ))

  # --- S2: anonymize_video_batch's region-value sweep ------------------------
  declare(
    "anonymize_video_batch/regions", "anonymize_video_batch",
    base = list(jobs = one(input = input, output = "o.mp4",
                           regions = list(good_regions))),
    bad = list(jobs = one(input = input, output = "o.mp4",
                          regions = list(bad_regions))),
    good = list(jobs = one(input = input, output = "o.mp4",
                           regions = list(good_regions))),
    sweep_marker = m_dim("width"),
    guards = list(
      `hardware-unknown` = guard(m_hardware, set(hardware = "cuda")),
      `audio_codec-token` = guard(m_token("audio_codec"),
                                  set(audio_codec = "a b")),
      `video_codec-token` = guard(m_token("video_codec"),
                                  set(video_codec = "a b")),
      `color-col-type` = guard(m_col_chr("color"), function(args) {
        args$jobs$color <- 1; args }),
      `video_codec-col-type` = guard(m_col_chr("video_codec"), function(args) {
        args$jobs$video_codec <- 1; args }),
      `audio_stream-col-type` = guard(m_col_num("audio_stream"),
                                      function(args) {
        args$jobs$audio_stream <- "a"; args }),
      `audio_stream-fractional` = guard(m_whole, set(audio_stream = 1.5)),
      `input-missing` = guard(m_unreadable, function(args) {
        args$jobs$input <- missing_file; args }),
      `regions-structure` = guard(m_regions_col, function(args) {
        # Drop HEIGHT, keeping the bad width value live (as in S1).
        args$jobs$regions[[1]]["height"] <- NULL; args }),
      `color-arg-not-string` = guard(m_string("color"), set(color = 1)),
      `pixel_format-arg-token` = guard(m_pixfmt,
                                       set(pixel_format = "yuv 420p")),
      `nvenc-unavailable` = guard(m_nvenc, set(hardware = "nvenc"))
    ))

  # --- S3: picture_in_picture's scale-range sweep (rep: scale = 2) -----------
  declare(
    "picture_in_picture/scale", "picture_in_picture",
    base = list(main = input, overlay = input, outfile = "o.mp4"),
    bad = list(scale = 2), good = list(scale = 0.25),
    sweep_marker = m_scale_range,
    guards = list(
      `main-unreadable` = guard(m_unreadable, set(main = missing_file)),
      `outfile-not-string` = guard(m_string("outfile"), set(outfile = 1)),
      # No scale-type crossing here: one scalar argument cannot be non-numeric
      # and out-of-range at once, so that cell's control could never be live.
      # AC5's grid cells distinguish the two guards instead.
      `margin-fractional` = guard(m_whole, set(margin = 1.5)),
      `audio-out-of-range` = guard(m_whole, set(audio = 9)),
      `video_codec-not-string` = guard(m_string("video_codec"),
                                       set(video_codec = 1)),
      `hardware-unknown` = guard(m_hardware, set(hardware = "cuda")),
      `contradiction` = guard(m_contradiction, set(audio_codec = "aac")),
      `position-vocab` = guard(m_position, set(position = "middleish")),
      `nvenc-unavailable` = guard(m_nvenc, set(hardware = "nvenc",
                                               video_codec = "libx264"))
    ))

  # --- S4: picture_in_picture_batch's per-row scale-range sweep --------------
  pip_jobs <- function(...) {
    one(main = input, overlay = input, output = "o.mp4", ...)
  }
  for (delivery in c("arg", "column")) {
    overlay_bad <- if (identical(delivery, "arg")) {
      list(scale = 2)
    } else {
      list(jobs = pip_jobs(scale = 2))
    }
    overlay_good <- if (identical(delivery, "arg")) {
      list(scale = 0.25)
    } else {
      list(jobs = pip_jobs(scale = 0.25))
    }
    declare(
      paste0("picture_in_picture_batch/scale/", delivery),
      "picture_in_picture_batch",
      base = list(jobs = pip_jobs()),
      bad = overlay_bad, good = overlay_good,
      sweep_marker = m_scale_range,
      guards = list(
        `scale-arg-type` = guard(m_scale_type, set(scale = "x")),
        `hardware-unknown` = guard(m_hardware, set(hardware = "cuda")),
        `jobs-not-df` = guard(m_jobs_df_out, set(jobs = "oops")),
        `main-col-missing` = guard("must have .?main.?",
                                   set(jobs = one(overlay = input,
                                                  output = "o.mp4"))),
        `duplicate-outputs` = guard(m_dup_out, function(args) {
          args$jobs <- rbind(args$jobs, args$jobs); args }),
        `scale-col-type` = guard(m_col_num("scale"), function(args) {
          args$jobs$scale <- "x"; args }),
        `input-missing` = guard(m_unreadable, function(args) {
          args$jobs$main <- missing_file; args }),
        `contradiction` = guard(m_contradiction, set(audio_codec = "aac")),
        `position-vocab` = guard(m_position, set(position = "middleish")),
        `margin-value` = guard(m_whole, set(margin = -3)),
        `audio-value` = guard(m_whole, set(audio = 9)),
        `nvenc-unavailable` = guard(m_nvenc, set(hardware = "nvenc",
                                                 video_codec = "libx264"))
      ))
  }

  # --- S5: normalize_audio's loudness sweep, both two_pass values ------------
  # rep: target_loudness = -100. The five guards AC4 names live inside the
  # two_pass block (TRUE cells); the FALSE cells cross the single-pass path's
  # own copies of the same conditions.
  declare(
    "normalize_audio/loudness/two-pass", "normalize_audio",
    base = list(infile = input, outfile = "o.wav", two_pass = TRUE),
    bad = list(target_loudness = -100), good = list(target_loudness = -23),
    sweep_marker = m_loud("target_loudness"),
    guards = list(
      `infile-unreadable` = guard(m_unreadable, set(infile = missing_file)),
      `outfile-not-string` = guard(m_string("outfile"), set(outfile = 1)),
      `two_pass-not-bool` = guard("`two_pass` must be `TRUE` or `FALSE`",
                                  set(two_pass = "yes")),
      `channels-fractional` = guard(m_whole, set(channels = 1.5)),
      `sample_rate-fractional` = guard(m_whole, set(sample_rate = 1.5)),
      `audio_stream-fractional` = guard(m_whole, set(audio_stream = 1.5)),
      `audio_codec-copy` = guard(m_copy, set(audio_codec = "copy")),
      `audio_codec-token` = guard(m_token("audio_codec"),
                                  set(audio_codec = "a b"))
    ))
  declare(
    "normalize_audio/loudness/single-pass", "normalize_audio",
    base = list(infile = input, outfile = "o.wav"),
    bad = list(target_loudness = -100), good = list(target_loudness = -23),
    sweep_marker = m_loud("target_loudness"),
    guards = list(
      `infile-unreadable` = guard(m_unreadable, set(infile = missing_file)),
      `outfile-not-string` = guard(m_string("outfile"), set(outfile = 1)),
      `channels-fractional` = guard(m_whole, set(channels = 1.5)),
      `sample_rate-fractional` = guard(m_whole, set(sample_rate = 1.5)),
      `audio_stream-fractional` = guard(m_whole, set(audio_stream = 1.5)),
      `audio_codec-copy` = guard(m_copy, set(audio_codec = "copy")),
      `audio_codec-not-string` = guard(m_string("audio_codec"),
                                       set(audio_codec = 1))
    ))

  # --- S6: normalize_audio_batch's per-row loudness sweep, both deliveries
  #     and both two_pass values ---------------------------------------------
  nrm_jobs <- function(...) one(input = input, output = "o.wav", ...)
  nrm_guards <- function(two_pass) c(
    list(
      `jobs-not-df` = guard(m_jobs_df, set(jobs = "oops")),
      `input-col-missing` = guard(m_input_col,
                                  set(jobs = one(output = "o.wav"))),
      `target-col-type` = guard(m_col_num("target_loudness"),
                                function(args) {
        args$jobs$target_loudness <- "x"; args }),
      `channels-col-na` = guard(m_col_na("channels"), function(args) {
        args$jobs$channels <- NA_real_; args }),
      `audio_codec-copy-arg` = guard(m_copy, set(audio_codec = "copy")),
      `audio_codec-copy-cell` = guard(m_copy, function(args) {
        args$jobs$audio_codec <- "copy"; args }),
      `audio_stream-col-type` = guard(m_col_num("audio_stream"),
                                      function(args) {
        args$jobs$audio_stream <- "a"; args }),
      `audio_stream-fractional` = guard(m_whole, set(audio_stream = 1.5)),
      # Reachable on both passes: single-pass from the per-row pipeline check,
      # two-pass from the block's own hoisted copy. Both sit BELOW the new
      # sweep, so these are flips (added at review return #1 — the first cut
      # of this list omitted them and M65-D1's table undercounted).
      `channels-fractional` = guard(m_whole, set(channels = 1.5)),
      `sample_rate-fractional` = guard(m_whole, set(sample_rate = 1.5)),
      `duplicate-inputs-no-output` = guard(m_dup_in, function(args) {
        args$jobs <- args$jobs[c(1, 1), ]
        args$jobs$output <- NULL
        args }),
      `input-missing` = guard(m_unreadable, function(args) {
        args$jobs$input <- missing_file; args })
    ),
    if (two_pass) list(
      # The two_pass block's own guards, BELOW the new sweep: the flips.
      `channels-col-whole-2p` = guard(m_col_whole_2p("channels"),
                                      function(args) {
        args$jobs$channels <- 0; args }),
      `audio_codec-token-2p` = guard(m_token("audio_codec"),
                                     set(audio_codec = "a b"))
    )
  )
  for (two_pass in c(FALSE, TRUE)) {
    for (delivery in c("arg", "column")) {
      overlay_bad <- if (identical(delivery, "arg")) {
        list(target_loudness = -100)
      } else {
        list(jobs = nrm_jobs(target_loudness = -100))
      }
      overlay_good <- if (identical(delivery, "arg")) {
        list(target_loudness = -23)
      } else {
        list(jobs = nrm_jobs(target_loudness = -23))
      }
      declare(
        paste0("normalize_audio_batch/loudness/", delivery,
               if (two_pass) "/two-pass" else "/single-pass"),
        "normalize_audio_batch",
        base = list(jobs = nrm_jobs(), two_pass = two_pass),
        bad = overlay_bad, good = overlay_good,
        sweep_marker = m_loud("target_loudness"),
        guards = nrm_guards(two_pass))
    }
  }

  cells
}

blame_precedence_m65 <- function(ref = NULL, root = ".") {
  blame_precedence(ref, root, cells = blame_precedence_cells_m65)
}
