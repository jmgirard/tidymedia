# M080 — the DOMAIN of the NA-refusal criteria, derived rather than listed.
#
# Two domains live here. `check_dim_verbs()` walks the parsed call graph for
# the exported verbs that reach check_dim(), exactly as helper-input-paths.R
# does for the input-path criteria: membership is fixed by the walk, never by
# this file. `na_sweep_predicates()` enumerates the one-argument `check_*`
# predicates out of the namespace by their formals, so a predicate added later
# joins the sweep without anyone editing a list.

# The exported verbs whose call graph reaches check_dim().
check_dim_verbs <- function(pkg = "tidymedia") {
  graph <- tm_call_graph(pkg)
  exported <- sort(intersect(getNamespaceExports(pkg), names(graph)))
  exported[vapply(exported, function(v) tm_reaches(graph, v, "check_dim"),
                  logical(1))]
}

# The one-argument `check_*` predicates: everything in the namespace matching
# `^check_` whose formals carry exactly one REQUIRED argument, that argument
# not named `jobs`. The `jobs` exclusion is what keeps the table-taking
# predicates out — a table is not a value, and NA is not one of its shapes
# (M080 Scope Out). The filter is mechanical, so a hand-list cannot drift.
na_sweep_predicates <- function(pkg = "tidymedia") {
  ns <- asNamespace(pkg)
  nms <- ls(ns, all.names = TRUE, pattern = "^check_")
  required <- function(nm) {
    f <- formals(get(nm, envir = ns))
    names(f)[vapply(f, function(a) identical(a, quote(expr = )), logical(1))]
  }
  Filter(function(nm) {
    r <- required(nm)
    length(r) == 1L && !identical(r, "jobs")
  }, nms)
}

# The four NA types the criteria quantify over: the logical NA and one of each
# atomic type a dimension argument could plausibly carry.
na_values <- function() {
  list(NA, NA_integer_, NA_real_, NA_character_)
}

na_labels <- function() {
  c("NA", "NA_integer_", "NA_real_", "NA_character_")
}

# Call shapes: for each verb the walk returns, one entry per argument whose
# value reaches check_dim(), naming the argument, how the NA is delivered, and
# a closure that makes an otherwise-legal call carrying it. `p` stands in for a
# readable input and `o` for an output path.
#
# `via` says which spelling the refusal must use: "argument" is the name the
# caller typed, "column" is the `jobs` column the value came from. Both are
# blamed on the verb; the column form is what a `_batch` verb's own column
# guard reports, which is the guard that reaches an NA cell first.
#
# An empty entry is a positive declaration that the verb takes no such
# carrier: its path to check_dim() carries values it computes itself
# (format_for_web()'s fixed web profile), so there is no caller-supplied value
# for an NA to arrive in. picture_in_picture() is NOT such a verb -- its
# `scale` and `margin` are caller-supplied and become the overlay geometry
# check_dim() reads -- so it declares them.
check_dim_specs <- function(p, o) {
  pipe <- function() ffm_files(p, o)
  jobs1 <- function(...) tibble::tibble(input = p, ...)
  pair <- function(...) tibble::tibble(main = p, overlay = p, output = o, ...)
  arg <- function(name, call) list(arg = name, via = "argument", call = call)
  col <- function(name, call) list(arg = name, via = "column", call = call)

  list(
    anonymize_video = list(
      arg("x", function(na) anonymize_video(
        p, o, data.frame(x = na, y = 0, width = 10, height = 10),
        run = FALSE)),
      arg("width", function(na) anonymize_video(
        p, o, data.frame(x = 0, y = 0, width = na, height = 10),
        run = FALSE))
    ),
    anonymize_video_batch = list(
      col("x", function(na) anonymize_video_batch(
        jobs1(regions = list(data.frame(x = na, y = 0, width = 10,
                                        height = 10))), run = FALSE)),
      col("width", function(na) anonymize_video_batch(
        jobs1(regions = list(data.frame(x = 0, y = 0, width = na,
                                        height = 10))), run = FALSE))
    ),
    crop_video = list(
      arg("width", function(na) crop_video(p, o, na, 100, run = FALSE)),
      arg("height", function(na) crop_video(p, o, 100, na, run = FALSE)),
      arg("x", function(na) crop_video(p, o, 100, 100, x = na, run = FALSE)),
      arg("y", function(na) crop_video(p, o, 100, 100, y = na, run = FALSE))
    ),
    crop_video_batch = list(
      arg("width", function(na) crop_video_batch(
        jobs1(), width = na, height = 100, run = FALSE)),
      arg("height", function(na) crop_video_batch(
        jobs1(), width = 100, height = na, run = FALSE)),
      arg("x", function(na) crop_video_batch(
        jobs1(), width = 100, height = 100, x = na, run = FALSE)),
      arg("y", function(na) crop_video_batch(
        jobs1(), width = 100, height = 100, y = na, run = FALSE)),
      col("width", function(na) crop_video_batch(
        jobs1(width = na), height = 100, run = FALSE)),
      col("height", function(na) crop_video_batch(
        jobs1(height = na), width = 100, run = FALSE)),
      col("x", function(na) crop_video_batch(
        jobs1(x = na), width = 100, height = 100, run = FALSE)),
      col("y", function(na) crop_video_batch(
        jobs1(y = na), width = 100, height = 100, run = FALSE))
    ),
    ffm_crop = list(
      arg("width", function(na) ffm_crop(pipe(), na, 100)),
      arg("height", function(na) ffm_crop(pipe(), 100, na)),
      arg("x", function(na) ffm_crop(pipe(), 100, 100, x = na)),
      arg("y", function(na) ffm_crop(pipe(), 100, 100, y = na))
    ),
    ffm_drawbox = list(
      arg("x", function(na) ffm_drawbox(pipe(), x = na)),
      arg("y", function(na) ffm_drawbox(pipe(), y = na)),
      arg("width", function(na) ffm_drawbox(pipe(), width = na)),
      arg("height", function(na) ffm_drawbox(pipe(), height = na)),
      arg("thickness", function(na) ffm_drawbox(pipe(), thickness = na))
    ),
    ffm_fps = list(
      arg("fps", function(na) ffm_fps(pipe(), na))
    ),
    ffm_overlay = list(
      arg("x", function(na) ffm_overlay(ffm_files(c(p, p), o), x = na)),
      arg("y", function(na) ffm_overlay(ffm_files(c(p, p), o), y = na)),
      arg("scale", function(na) ffm_overlay(ffm_files(c(p, p), o), scale = na))
    ),
    ffm_scale = list(
      arg("width", function(na) ffm_scale(pipe(), na, 100)),
      arg("height", function(na) ffm_scale(pipe(), 100, na))
    ),
    format_for_web = list(),
    format_for_web_batch = list(),
    picture_in_picture = list(
      arg("scale", function(na) picture_in_picture(p, p, o, scale = na,
                                                   run = FALSE)),
      arg("margin", function(na) picture_in_picture(p, p, o, margin = na,
                                                    run = FALSE))
    ),
    picture_in_picture_batch = list(
      arg("scale", function(na) picture_in_picture_batch(
        pair(), scale = na, run = FALSE)),
      arg("margin", function(na) picture_in_picture_batch(
        pair(), margin = na, run = FALSE)),
      col("scale", function(na) picture_in_picture_batch(
        pair(scale = na), run = FALSE)),
      col("margin", function(na) picture_in_picture_batch(
        pair(margin = na), run = FALSE))
    ),
    sample_frames = list(
      arg("fps", function(na) sample_frames(p, outdir = tempdir(), fps = na,
                                            run = FALSE)),
      arg("interval", function(na) sample_frames(p, outdir = tempdir(),
                                                 interval = na, run = FALSE))
    ),
    sample_frames_batch = list(
      arg("fps", function(na) sample_frames_batch(
        jobs1(), fps = na, outdir = tempdir(), run = FALSE)),
      col("fps", function(na) sample_frames_batch(
        jobs1(fps = na), outdir = tempdir(), run = FALSE)),
      arg("interval", function(na) sample_frames_batch(
        jobs1(), interval = na, outdir = tempdir(), run = FALSE)),
      col("interval", function(na) sample_frames_batch(
        jobs1(interval = na), outdir = tempdir(), run = FALSE))
    ),
    standardize_video = list(
      arg("width", function(na) standardize_video(p, o, width = na,
                                                  run = FALSE)),
      arg("height", function(na) standardize_video(p, o, height = na,
                                                   run = FALSE)),
      arg("fps", function(na) standardize_video(p, o, fps = na, run = FALSE))
    ),
    standardize_video_batch = list(
      arg("width", function(na) standardize_video_batch(
        jobs1(), width = na, run = FALSE)),
      arg("height", function(na) standardize_video_batch(
        jobs1(), height = na, run = FALSE)),
      arg("fps", function(na) standardize_video_batch(
        jobs1(), fps = na, run = FALSE)),
      col("width", function(na) standardize_video_batch(
        jobs1(width = na), run = FALSE)),
      col("height", function(na) standardize_video_batch(
        jobs1(height = na), run = FALSE)),
      col("fps", function(na) standardize_video_batch(
        jobs1(fps = na), run = FALSE))
    )
  )
}
