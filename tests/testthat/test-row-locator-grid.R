# The M66 row-locator grid: one cell per swept site in the committed triage
# (data-raw/m66-site-triage.tsv), each placing the bad value in a jobs COLUMN
# at a row other than 1 and asserting the refusal (a) is the guard's own --
# the `own` marker names WHICH failure, never that one occurred -- (b) names
# exactly that row, (c) blames the verb the user called, with no fan-out
# leak. `locator = FALSE` cells are the AC2 complement: the same value
# delivered as the verb's own ARGUMENT must refuse with no locator.
#
# data-raw/blame-guard-mutations-m66.py mutates each swept site's row-index
# pass to the constant 1L; the row-2 (and row-3) assertions here are what
# turn those mutations red, so every cell id below is a mutation's owner.

make_locator_input <- function(n = 1) {
  paths <- replicate(n, withr::local_tempfile(fileext = ".mp4",
                                              .local_envir = parent.frame(2)))
  file.create(paths)
  paths
}

locator_specs <- function(input3) {
  one <- function(...) tibble::tibble(...)
  i1 <- input3[[1]]; i2 <- input3[[2]]; i3 <- input3[[3]]
  specs <- list()
  add <- function(...) specs[[length(specs) + 1L]] <<- list(...)

  dim_msg <- "must be a single FFmpeg expression or number"
  token_msg <- "must be a single clean token"
  vocab_msg <- "must be one of"
  whole_msg <- "must be a whole number"

  # --- M64 value sweeps ------------------------------------------------------
  add(id = "crop/width/row3", verb = "crop_video_batch", row = 3,
      args = list(jobs = one(input = input3, output = c("a.mp4", "b.mp4", "c.mp4"),
                             width = c(160, 160, 0)),
                  height = 120), own = dim_msg)
  add(id = "crop/x", verb = "crop_video_batch", row = 2,
      args = list(jobs = one(input = c(i1, i2), output = c("a.mp4", "b.mp4"),
                             x = c(0, -1)),
                  width = 160, height = 120), own = dim_msg)
  add(id = "standardize/fps", verb = "standardize_video_batch", row = 2,
      args = list(jobs = one(input = c(i1, i2), fps = c(30, 0))), own = dim_msg)
  add(id = "standardize/pixel_format", verb = "standardize_video_batch", row = 2,
      args = list(jobs = one(input = c(i1, i2),
                             pixel_format = c("yuv420p", "bad format"))),
      own = paste("`pixel_format`", token_msg))
  add(id = "sample/fps", verb = "sample_frames_batch", row = 2,
      args = list(jobs = one(input = c(i1, i2), fps = c(1, -3)),
                  outdir = withr::local_tempdir(.local_envir = parent.frame(2))),
      own = "must be a single positive number or a string")
  add(id = "sample/interval", verb = "sample_frames_batch", row = 2,
      args = list(jobs = one(input = c(i1, i2), interval = c(1, -3)),
                  outdir = withr::local_tempdir(.local_envir = parent.frame(2))),
      own = "`interval` must be a single positive number")

  # --- M65 value sweeps ------------------------------------------------------
  add(id = "anonymize/regions-structure", verb = "anonymize_video_batch", row = 2,
      args = list(jobs = one(input = c(i1, i2),
                             regions = list(data.frame(x = 0, y = 0, width = 8,
                                                       height = 8),
                                            data.frame(x = 0, y = 0)))),
      own = "missing 2 required columns")
  add(id = "anonymize/region-values", verb = "anonymize_video_batch", row = 2,
      args = list(jobs = one(input = c(i1, i2),
                             regions = list(data.frame(x = 0, y = 0, width = 8,
                                                       height = 8),
                                            data.frame(x = 0, y = 0, width = -5,
                                                       height = 8)))),
      own = dim_msg)
  add(id = "pip/scale", verb = "picture_in_picture_batch", row = 2,
      args = list(jobs = one(main = c(i1, i2), overlay = c(i2, i3),
                             output = c("a.mp4", "b.mp4"), scale = c(0.5, 3))),
      own = "`scale` must be greater than")
  add(id = "normalize/target_loudness", verb = "normalize_audio_batch", row = 2,
      args = list(jobs = one(input = c(i1, i2), target_loudness = c(-16, 7)),
                  output_dir = withr::local_tempdir(.local_envir = parent.frame(2))),
      own = "`target_loudness` must be a number")
  add(id = "normalize/copy-column", verb = "normalize_audio_batch", row = 2,
      args = list(jobs = one(input = c(i1, i2), audio_codec = c("aac", "copy")),
                  output_dir = withr::local_tempdir(.local_envir = parent.frame(2))),
      own = "can't be")
  add(id = "normalize/two-pass-token", verb = "normalize_audio_batch", row = 2,
      args = list(jobs = one(input = c(i1, i2),
                             audio_codec = c("aac", "bad codec")),
                  output_dir = withr::local_tempdir(.local_envir = parent.frame(2)),
                  two_pass = TRUE),
      own = token_msg)
  add(id = "normalize/channels", verb = "normalize_audio_batch", row = 2,
      args = list(jobs = one(input = c(i1, i2), channels = c(2, 2.5)),
                  output_dir = withr::local_tempdir(.local_envir = parent.frame(2)),
                  two_pass = TRUE),
      own = "whole numbers")
  add(id = "normalize/sample_rate", verb = "normalize_audio_batch", row = 2,
      args = list(jobs = one(input = c(i1, i2), sample_rate = c(48000, 0.5)),
                  output_dir = withr::local_tempdir(.local_envir = parent.frame(2)),
                  two_pass = TRUE),
      own = "whole numbers")

  add(id = "extract_frame/timestamp-finite", verb = "extract_frame_batch", row = 2,
      args = list(jobs = one(input = c(i1, i2), timestamp = c(1, Inf)),
                  outdir = withr::local_tempdir(.local_envir = parent.frame(2))),
      own = "must be finite")
  add(id = "extract_frame/frame-whole", verb = "extract_frame_batch", row = 2,
      args = list(jobs = one(input = c(i1, i2), frame = c(1, 2.5)),
                  outdir = withr::local_tempdir(.local_envir = parent.frame(2))),
      own = "must contain whole numbers")

  # --- M58 contradiction sweeps ---------------------------------------------
  add(id = "segment/reencode-video", verb = "segment_video_batch", row = 2,
      args = list(jobs = one(input = c(i1, i2), start = 0, end = 1,
                             video_codec = c(NA, "libx265")),
                  reencode = FALSE),
      own = "need a re-encoding cut")
  # An NA audio_codec cell resolves to NULL, which this contradiction ALSO
  # refuses under reencode = FALSE -- so the clean row must carry "copy",
  # the one value the checker accepts, or the locator truthfully names row 1.
  add(id = "segment/reencode-audio", verb = "segment_video_batch", row = 2,
      args = list(jobs = one(input = c(i1, i2), start = 0, end = 1,
                             audio_codec = c("copy", "mp3")),
                  reencode = FALSE),
      own = "`audio_codec` needs a re-encoding cut")
  # The reshape-discriminating cell (AC2): this verb's sweep reads `jobs`,
  # never the reshaped `long` table, so the bad CALLER row 2 -- reshaped
  # indices 3 and 4 -- must be named as 2 and never 3.
  add(id = "separate/hardware-reshape", verb = "separate_audio_video_batch",
      row = 2, forbid_row = 3,
      args = list(jobs = one(input = c(i1, i2),
                             audiofile = c("a.wav", "b.wav"),
                             videofile = c("a.mp4", "b.mp4"),
                             video_codec = c(NA, "copy")),
                  hardware = "nvenc"),
      own = "`hardware` needs a re-encoding")
  add(id = "compare/needs-audio", verb = "compare_videos_batch", row = 2,
      args = list(jobs = one(inputs = list(c(i1, i2), c(i2, i3)),
                             output = c("a.mp4", "b.mp4"),
                             audio_codec = c(NA, "aac"))),
      own = "needs an audio stream to encode")
  # `resize` delivered as a COLUMN: the locator gates on the resize column's
  # presence (M66 review F1), so the cell carries it there.
  add(id = "compare/resize-three-inputs", verb = "compare_videos_batch", row = 2,
      args = list(jobs = one(inputs = list(c(i1, i2), c(i1, i2, i3)),
                             output = c("a.mp4", "b.mp4"),
                             resize = c(FALSE, TRUE))),
      own = "exactly two inputs")
  add(id = "pip/needs-audio", verb = "picture_in_picture_batch", row = 2,
      args = list(jobs = one(main = c(i1, i2), overlay = c(i2, i3),
                             output = c("a.mp4", "b.mp4"),
                             audio_codec = c(NA, "aac"))),
      own = "needs an audio stream to encode")

  # --- M59 value sweeps ------------------------------------------------------
  add(id = "compare/direction", verb = "compare_videos_batch", row = 2,
      args = list(jobs = one(inputs = list(c(i1, i2), c(i2, i3)),
                             output = c("a.mp4", "b.mp4"),
                             direction = c("horizontal", "diagonal"))),
      own = vocab_msg)
  add(id = "compare/audio-bound", verb = "compare_videos_batch", row = 2,
      args = list(jobs = one(inputs = list(c(i1, i2), c(i2, i3)),
                             output = c("a.mp4", "b.mp4"), audio = c(0, 5))),
      own = whole_msg)
  add(id = "pip/position", verb = "picture_in_picture_batch", row = 2,
      args = list(jobs = one(main = c(i1, i2), overlay = c(i2, i3),
                             output = c("a.mp4", "b.mp4"),
                             position = c("topleft", "diagonal"))),
      own = vocab_msg)
  add(id = "pip/margin", verb = "picture_in_picture_batch", row = 2,
      args = list(jobs = one(main = c(i1, i2), overlay = c(i2, i3),
                             output = c("a.mp4", "b.mp4"), margin = c(10, -4))),
      own = whole_msg)
  add(id = "pip/audio", verb = "picture_in_picture_batch", row = 2,
      args = list(jobs = one(main = c(i1, i2), overlay = c(i2, i3),
                             output = c("a.mp4", "b.mp4"), audio = c(0, 5))),
      own = whole_msg)

  # --- the shared codec-column token loop, per calling verb and column ------
  codec_col_cells <- list(
    list("anonymize_video_batch", "video_codec",
         function(vc) list(jobs = one(input = c(i1, i2),
                                      regions = list(data.frame(x = 0, y = 0, width = 8, height = 8),
                                                     data.frame(x = 0, y = 0, width = 8, height = 8)),
                                      video_codec = vc))),
    list("anonymize_video_batch", "audio_codec",
         function(ac) list(jobs = one(input = c(i1, i2),
                                      regions = list(data.frame(x = 0, y = 0, width = 8, height = 8),
                                                     data.frame(x = 0, y = 0, width = 8, height = 8)),
                                      audio_codec = ac))),
    list("compare_videos_batch", "video_codec",
         function(vc) list(jobs = one(inputs = list(c(i1, i2), c(i2, i3)),
                                      output = c("a.mp4", "b.mp4"),
                                      video_codec = vc))),
    list("compare_videos_batch", "audio_codec",
         function(ac) list(jobs = one(inputs = list(c(i1, i2), c(i2, i3)),
                                      output = c("a.mp4", "b.mp4"),
                                      audio = 0, audio_codec = ac))),
    list("convert_audio_batch", "audio_codec",
         function(ac) list(jobs = one(input = c(i1, i2),
                                      output = c("a.mp3", "b.mp3"),
                                      audio_codec = ac))),
    list("crop_video_batch", "video_codec",
         function(vc) list(jobs = one(input = c(i1, i2), output = c("a.mp4", "b.mp4"),
                                      video_codec = vc),
                           width = 160, height = 120)),
    list("crop_video_batch", "audio_codec",
         function(ac) list(jobs = one(input = c(i1, i2), output = c("a.mp4", "b.mp4"),
                                      audio_codec = ac),
                           width = 160, height = 120)),
    list("extract_audio_batch", "audio_codec",
         function(ac) list(jobs = one(input = c(i1, i2),
                                      output = c("a.mp3", "b.mp3"),
                                      audio_codec = ac))),
    list("normalize_audio_batch", "audio_codec",
         function(ac) list(jobs = one(input = c(i1, i2), audio_codec = ac),
                           output_dir = withr::local_tempdir(.local_envir = parent.frame(2)))),
    list("picture_in_picture_batch", "video_codec",
         function(vc) list(jobs = one(main = c(i1, i2), overlay = c(i2, i3),
                                      output = c("a.mp4", "b.mp4"),
                                      video_codec = vc))),
    list("picture_in_picture_batch", "audio_codec",
         function(ac) list(jobs = one(main = c(i1, i2), overlay = c(i2, i3),
                                      output = c("a.mp4", "b.mp4"),
                                      audio = 0, audio_codec = ac))),
    list("segment_video_batch", "video_codec",
         function(vc) list(jobs = one(input = c(i1, i2), start = 0, end = 1,
                                      video_codec = vc))),
    list("segment_video_batch", "audio_codec",
         function(ac) list(jobs = one(input = c(i1, i2), start = 0, end = 1,
                                      audio_codec = ac))),
    list("separate_audio_video_batch", "video_codec",
         function(vc) list(jobs = one(input = c(i1, i2),
                                      audiofile = c("a.wav", "b.wav"),
                                      videofile = c("a.mp4", "b.mp4"),
                                      video_codec = vc))),
    list("separate_audio_video_batch", "audio_codec",
         function(ac) list(jobs = one(input = c(i1, i2),
                                      audiofile = c("a.wav", "b.wav"),
                                      videofile = c("a.mp4", "b.mp4"),
                                      audio_codec = ac))),
    list("standardize_video_batch", "video_codec",
         function(vc) list(jobs = one(input = c(i1, i2), video_codec = vc))),
    list("standardize_video_batch", "audio_codec",
         function(ac) list(jobs = one(input = c(i1, i2), audio_codec = ac)))
  )
  for (cell in codec_col_cells) {
    add(id = paste0("codec-col/", cell[[1]], "/", cell[[2]]),
        verb = cell[[1]], row = 2,
        args = cell[[3]](c(NA, "bad codec")),
        own = token_msg)
  }

  # --- AC2 complements: the same family, argument-delivered, NO locator -----
  add(id = "arg/pip-scale", verb = "picture_in_picture_batch", locator = FALSE,
      args = list(jobs = one(main = c(i1, i2), overlay = c(i2, i3),
                             output = c("a.mp4", "b.mp4")), scale = 3),
      own = "`scale` must be greater than")
  add(id = "arg/pip-margin", verb = "picture_in_picture_batch", locator = FALSE,
      args = list(jobs = one(main = c(i1, i2), overlay = c(i2, i3),
                             output = c("a.mp4", "b.mp4")), margin = -4),
      own = whole_msg)
  add(id = "arg/normalize-target", verb = "normalize_audio_batch", locator = FALSE,
      args = list(jobs = one(input = c(i1, i2)),
                  output_dir = withr::local_tempdir(.local_envir = parent.frame(2)),
                  target_loudness = 7),
      own = "`target_loudness` must be a number")
  add(id = "arg/segment-contradiction", verb = "segment_video_batch", locator = FALSE,
      args = list(jobs = one(input = c(i1, i2), start = 0, end = 1),
                  reencode = FALSE, video_codec = "libx265"),
      own = "need a re-encoding cut")
  add(id = "arg/compare-direction", verb = "compare_videos_batch", locator = FALSE,
      args = list(jobs = one(inputs = list(c(i1, i2), c(i2, i3)),
                             output = c("a.mp4", "b.mp4")),
                  direction = "diagonal"),
      own = vocab_msg)
  # The two sites M66's review found violating the complement (F1/F2): a
  # scalar `audio` or `resize` against a uniform table offends on every row,
  # so no row may be named.
  add(id = "arg/compare-audio-bound", verb = "compare_videos_batch",
      locator = FALSE,
      args = list(jobs = one(inputs = list(c(i1, i2), c(i2, i3)),
                             output = c("a.mp4", "b.mp4")),
                  audio = 5),
      own = whole_msg)
  add(id = "arg/compare-resize", verb = "compare_videos_batch",
      locator = FALSE,
      args = list(jobs = one(inputs = list(c(i1, i2, i3), c(i1, i2, i3)),
                             output = c("a.mp4", "b.mp4")),
                  resize = TRUE),
      own = "exactly two inputs")

  specs
}

test_that("every swept site names the offending row; argument delivery does not", {
  withr::local_options(tidymedia.nvenc_encoders = character(0))
  input3 <- make_locator_input(3)
  specs <- locator_specs(input3)
  ids <- vapply(specs, `[[`, character(1), "id")
  expect_identical(anyDuplicated(ids), 0L)

  for (cell in specs) {
    cnd <- catch_call(cell$verb, cell$args)
    msg <- cli::ansi_strip(conditionMessage(cnd))
    # WHICH failure: the guard's own marker, before any locator reading.
    expect_match(msg, cell$own, info = cell$id)
    # The blame the locator must not disturb.
    expect_identical(blamed_verb(cnd), cell$verb, info = cell$id)
    expect_no_match(msg, "In index:", fixed = TRUE, info = cell$id)
    expect_no_match(msg, "pmap", fixed = TRUE, info = cell$id)
    if (isFALSE(cell$locator)) {
      expect_no_match(msg, "First offending jobs row", info = cell$id)
    } else {
      expect_match(msg,
                   sprintf("First offending jobs row: %d\\.$", cell$row),
                   info = cell$id)
      if (!is.null(cell$forbid_row)) {
        expect_no_match(msg,
                        sprintf("First offending jobs row: %d\\.", cell$forbid_row),
                        info = cell$id)
      }
    }
  }
})
