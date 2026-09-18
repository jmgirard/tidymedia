# M135: the `quality` grid over the scalar re-encoding verbs.
#
# The verb set is AC1's own filter, read off the namespace at test time: every
# export whose formals include `hardware` and `run` and exclude `jobs`. A ninth
# verb gaining `hardware` joins the grid on its own.
quality_grid_verbs <- function() {
  ns <- asNamespace("tidymedia")
  fns <- mget(getNamespaceExports("tidymedia"), envir = ns, ifnotfound = list(NULL))
  keep <- Filter(function(f) {
    is.function(f) &&
      all(c("hardware", "run") %in% names(formals(f))) &&
      !"jobs" %in% names(formals(f))
  }, fns)
  tm_sort_c(names(keep))
}

# The (video_codec, hardware) pair that resolves to each table row's encoder.
quality_grid_pairs <- function() {
  list(
    libx264 = c(video_codec = "libx264", hardware = "none"),
    libx265 = c(video_codec = "libx265", hardware = "none"),
    h264_nvenc = c(video_codec = "libx264", hardware = "nvenc"),
    hevc_nvenc = c(video_codec = "libx265", hardware = "nvenc"),
    av1_nvenc = c(video_codec = "libaom-av1", hardware = "nvenc"),
    h264_videotoolbox = c(video_codec = "libx264", hardware = "videotoolbox"),
    hevc_videotoolbox = c(video_codec = "libx265", hardware = "videotoolbox")
  )
}

# The rows a verb's `video_codec` and `hardware` formals can resolve to: all
# seven with a `video_codec` formal, the three H.264 rows without one (the
# recipe fixes the family).
quality_grid_rows <- function(fname) {
  fmls <- formals(get(fname, envir = asNamespace("tidymedia")))
  if ("video_codec" %in% names(fmls)) {
    quality_flags()$encoder
  } else {
    c("libx264", "h264_nvenc", "h264_videotoolbox")
  }
}

quality_grid_hw_encoders <- function() {
  c("h264_nvenc", "hevc_nvenc", "av1_nvenc",
    "h264_videotoolbox", "hevc_videotoolbox")
}

# One verb's call from its own formals (nvenc_grid_args(), helper-nvenc-memo.R),
# reset to hardware = "none" and no codec override so each cell sets its own.
quality_grid_args <- function(fname, input) {
  args <- nvenc_grid_args(fname, input)
  args$hardware <- "none"
  args$video_codec <- NULL
  args
}

# The compiled commands a `run = FALSE` call returns: one string for the direct
# verbs, a `command` column from the fan-out, a named vector from the
# separation verb.
quality_grid_commands <- function(out) {
  if (is.data.frame(out)) out$command else as.character(out)
}

quality_grid_input <- function(dir) {
  vid <- file.path(dir, "in.mp4")
  if (!file.exists(vid)) file.create(vid)
  vid
}

expect_quality_flag_after_codec <- function(cmds, encoder, flag, value,
                                            info = NULL) {
  has_codec <- grepl(paste0("-codec:v ", encoder), cmds, fixed = TRUE)
  expect_true(any(has_codec), info = info)
  for (cmd in cmds[has_codec]) {
    codec_at <- regexpr(paste0("-codec:v ", encoder), cmd, fixed = TRUE)
    flag_at <- regexpr(paste(flag, value), cmd, fixed = TRUE)
    expect_gt(flag_at, codec_at, label = paste(info, "flag position"))
  }
}

expect_no_quality_flag_in <- function(cmds, info = NULL) {
  for (cmd in cmds) {
    expect_false(grepl("-crf", cmd, fixed = TRUE), info = info)
    expect_false(grepl("-cq", cmd, fixed = TRUE), info = info)
    expect_false(grepl("-q:v", cmd, fixed = TRUE), info = info)
  }
}
