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

# M081 — the DOMAIN of the flag-guard criterion, derived rather than listed.
#
# `na_sweep_predicates()` above filters on ONE required formal, so a guard that
# takes a flag AND something else -- check_audio_codec_needs_reencode(),
# check_resize_needs_two_inputs() -- falls outside it and kept branching on an
# unchecked flag after M080 fixed the one-argument twin. Widening that filter
# was rejected: it pulls in check_batch_cell(), whose NA_integer_ row is
# deliberate (R/ffmpeg.R:3395), so it would need an exemption registry. The
# walk below decides membership instead, by SHAPE: a `check_*` predicate that
# makes a required formal the direct operand of `!`, `&&` or `||` without
# first passing it to rlang::check_bool() is branching on a value base R will
# raise `argument is not interpretable as logical` or `missing value where
# TRUE/FALSE needed` on -- a bare simpleError from inside a front-door guard.
#
# Reading the namespace rather than `R/` keeps this alive under `R CMD check`,
# where there is no source tree (M51/M59), and it is the same parsed-call-node
# walk helper-input-paths.R uses: a formal name inside a cli message string is
# text, not an operand, and only operands are read here.

# Every `check_*` function of the namespace, as a named list.
tm_check_predicates <- function(pkg = "tidymedia") {
  ns <- asNamespace(pkg)
  fns <- mget(ls(ns, all.names = TRUE, pattern = "^check_"), envir = ns,
              ifnotfound = list(NULL))
  fns[vapply(fns, is.function, logical(1))]
}

# The names in `required` used as a DIRECT operand of `!`, `&&` or `||`
# anywhere in `e`. `!is.null(x)` does not count: the operand there is the
# is.null() call, and the guard is reading a property of `x`, not `x` itself
# as a flag.
tm_bare_flag_operands <- function(e, required) {
  out <- character()
  walk <- function(e) {
    if (is.call(e)) {
      if (deparse1(e[[1]]) %in% c("!", "&&", "||")) {
        for (i in seq_along(e)[-1]) {
          if (rlang::is_missing(e[[i]]) || is.null(e[[i]])) next
          if (is.name(e[[i]]) && as.character(e[[i]]) %in% required) {
            out <<- c(out, as.character(e[[i]]))
          }
        }
      }
    }
    if (is.call(e) || is.pairlist(e)) {
      for (i in seq_along(e)) {
        if (rlang::is_missing(e[[i]]) || is.null(e[[i]])) next
        walk(e[[i]])
      }
    }
  }
  walk(e)
  unique(out)
}

# The names handed to rlang::check_bool() (either spelling) anywhere in `e`.
tm_check_bool_targets <- function(e) {
  out <- character()
  walk <- function(e) {
    if (is.call(e)) {
      if (deparse1(e[[1]]) %in% c("check_bool", "rlang::check_bool") &&
          length(e) >= 2 && is.name(e[[2]])) {
        out <<- c(out, as.character(e[[2]]))
      }
    }
    if (is.call(e) || is.pairlist(e)) {
      for (i in seq_along(e)) {
        if (rlang::is_missing(e[[i]]) || is.null(e[[i]])) next
        walk(e[[i]])
      }
    }
  }
  walk(e)
  unique(out)
}

# `f`'s required formals branched on before anything checked them. The body's
# top-level statements are read IN ORDER, so a check_bool() below the branch
# does not excuse it -- "first" in the criterion is positional, and a guard
# that checks after it has already branched has already crashed.
tm_unchecked_flags <- function(f) {
  fo <- formals(f)
  required <- names(fo)[vapply(fo, function(a) identical(a, quote(expr = )),
                               logical(1))]
  if (length(required) == 0) return(character())
  b <- body(f)
  stmts <- if (is.call(b) && identical(b[[1]], as.name("{"))) {
    as.list(b)[-1]
  } else {
    list(b)
  }
  checked <- character()
  flagged <- character()
  for (s in stmts) {
    flagged <- c(flagged,
                 setdiff(tm_bare_flag_operands(s, required), checked))
    checked <- c(checked, tm_check_bool_targets(s))
  }
  unique(flagged)
}

# The criterion's domain: every predicate the walk flags, and which of its
# formals. `fns` is a parameter so the positive controls can be walked with
# the same code the namespace is.
unchecked_flag_guards <- function(fns = tm_check_predicates()) {
  out <- lapply(fns, tm_unchecked_flags)
  out[lengths(out) > 0]
}

# The EXPORTED half of the flag-guard criterion. The walk above finds guards
# that branch on an unchecked flag; this one finds the exported verbs whose
# call graph reaches those guards, so the sweep below can pin what a real
# caller sees. Membership is the walk's, never a list: an exported verb that
# starts reaching either guard joins the domain and the sweep fails until a
# call shape is declared for it.
flag_guard_verbs <- function(pkg = "tidymedia") {
  graph <- tm_call_graph(pkg)
  exported <- sort(intersect(getNamespaceExports(pkg), names(graph)))
  targets <- c("check_audio_codec_needs_reencode",
               "check_resize_needs_two_inputs")
  exported[vapply(exported, function(v)
    any(vapply(targets, function(t) tm_reaches(graph, v, t), logical(1))),
    logical(1))]
}

# Call shapes, one entry per verb per DELIVERY FORM, in check_dim_specs()'
# arg/via shape. `via` says which spelling the refusal uses: an argument is
# refused as `` `resize` ``, a `jobs` column as `resize column` -- two
# different guards with two different messages, and asserting only the bare
# name would pass on either for the wrong reason.
flag_guard_specs <- function(p, o) {
  seg <- function(...) tibble::tibble(input = p, start = 0, end = 1, ...)
  cmp <- function(...) tibble::tibble(inputs = list(c(p, p)), output = o, ...)
  arg <- function(name, call) list(arg = name, via = "argument", call = call)
  col <- function(name, call) list(arg = name, via = "column", call = call)

  list(
    compare_videos = list(
      arg("resize", function(x) compare_videos(c(p, p), o, resize = x,
                                               run = FALSE))
    ),
    compare_videos_batch = list(
      arg("resize", function(x) compare_videos_batch(cmp(), resize = x,
                                                     run = FALSE)),
      col("resize", function(x) compare_videos_batch(cmp(resize = x),
                                                     run = FALSE))
    ),
    segment_video = list(
      arg("reencode", function(x) segment_video(p, 0, 1, reencode = x,
                                                run = FALSE))
    ),
    segment_video_batch = list(
      arg("reencode", function(x) segment_video_batch(seg(), reencode = x,
                                                      run = FALSE)),
      col("reencode", function(x) segment_video_batch(seg(reencode = x),
                                                      run = FALSE))
    )
  )
}

# The value forms a flag guard must refuse. SCALAR only, and deliberately: in
# a `jobs` column length is row count, so c(TRUE, TRUE) is a legal two-row
# batch rather than a wrong length, and a length probe would make the
# criterion false on half its domain.
flag_reject_values <- function() c(na_values(), list(1L, "yes"))

flag_reject_labels <- function() c(na_labels(), "1L", "\"yes\"")
