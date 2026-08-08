# input-guard-baseline.R -----------------------------------------------------
#
# Regenerate the missing-input front-door baseline from an arbitrary git ref, so
# M62's claim -- "a call naming an input file that does not exist reports
# against the verb the user called, and nothing else about that call's fate
# moved" -- is re-derivable evidence rather than an implementation-time
# transcript (M62 T5, AC5/AC6). Same shape and the same ref machinery as
# `data-raw/value-guard-baseline.R`, which in turn sources
# `data-raw/codec-guard-baseline.R` rather than copying its `git show`-into-an-
# environment helper; this file sources the same one, for the same reason.
#
# WHAT IS CROSSED WITH WHAT
#
# Every cell in this grid names an input path that does not exist AND commits a
# second, independent offence that the verb also refuses at its front door. The
# grid therefore measures WHICH of the two the caller is shown, not merely that
# the call was refused -- the ordering claim AC6 makes, cell by cell, over the
# whole domain rather than at the two or three sites anyone would think to type.
#
# The crossings, and where each sits relative to the sweep:
#
#   jobs_na          check_batch_jobs()/check_fanin_jobs()'s NA-in-the-input-
#                    carrier guard -- part of the jobs SHAPE block, ABOVE the
#                    sweep, so these cells must read `jobs_na` on both refs
#   column_type      a knob column of the wrong type (check_batch_codec_col()),
#                    also ABOVE the sweep -- same expectation
#   contradiction:*  the M58 argument-contradiction sweep, BELOW the sweep
#   nvenc            check_nvenc_available(), BELOW the sweep
#   run_guard        ffm_batch()'s own `run` guard, below everything
#   value:*          the four per-row value guards D039 moved to the front door
#                    (`direction`, `position`, `margin`, the per-row `audio`
#                    bound), BELOW the contradiction sweep
#
# So the two ABOVE crossings pin AC6's first half ("after each fan-out verb's
# jobs-shape and column-type guards") and the four BELOW ones pin its second
# ("before its M58 contradiction sweep"). Without the first pair the grid could
# not tell a sweep placed correctly from one hoisted to the top of the verb,
# which would answer `input` on every cell and look like success.
#
# NOTHING BELOW NAMES A COMBINATION. The cells are generated from
# INPUT_GUARD_CROSSINGS x INPUT_GUARD_FORMS x the per-verb call shapes, and
# `input_guard_uncovered()` re-derives the same product from the same
# declarations and reports any combination the grid did not produce. This is
# M61's lesson taken forward: three review rounds there each returned a
# different hand-written combination that nobody had typed, so the enumeration
# is the fix -- a combination can no longer be forgotten, only mis-built, and
# the reader reports either.
#
# THE VERB DOMAIN is not declared here. It is `input_guard_verbs()`, the parsed
# call-graph walk in `tests/testthat/helper-input-paths.R`, sourced below: the
# same walk the AC3/AC4 tests quantify over, so this grid and those tests cannot
# disagree about which verbs the criteria reach. A verb the walk returns with no
# entry in INPUT_GUARD_SHAPES is an error, not a silent omission.
#
# FORMS -- how a call's input paths are populated:
#
#   all     every input path in the call is absent, and each is a DIFFERENT
#           absent path
#   one     the call carries 2+ input paths and exactly ONE is absent
#   dup     the call carries 2+ input paths and every one is the SAME absent
#           path
#   factor  every input path is absent and the carrier is a FACTOR column
#           rather than a character one
#
# `one` is the form that decides whether the sweep looks at the whole carrier or
# stops at the first path, and `all`'s paths are distinct for the same reason:
# with one constant path repeated, a checker that named only the first would
# render identically to one that named them all, and the grid could not tell
# them apart. That blindness is what let a verb sweeping its two input columns
# in two separate aborts read as covered (M62 review F2).
#
# `dup` and `factor` are the two axes the M62 review found the first cell set
# could not reach, added with the fix rather than after it:
#
#   dup     one mistyped path shared by N rows is ONE missing file, not N. With
#           `all` distinct and `one` carrying a single absent path, nothing in
#           the grid counted a repeat (M62 review F3).
#   factor  a path column can arrive carrying its paths as factor levels, and a
#           front-door guard that hands one to file.exists() raw degrades to the
#           base error `invalid 'file' argument` blamed on file.exists (M62
#           review F1). input_guard_blame_regressions() is the reader that sees
#           it; it had no cell to see it in.
#
# Both are generated at the `none` crossing only. What they probe is what the
# abort SAYS and who it blames, and a crossed cell says the crossing -- so
# crossing them over the other seven would add cells that measure the crossing
# again under a different label.
#
# A verb whose legal call carries exactly one input path has no `one` or `dup`
# form, and one whose paths do not travel in a plain column (D015's `inputs`
# list-column, and every scalar signature) has no `factor` form. Those cells are
# recorded with `exists = FALSE` rather than omitted, so the grid states the gap
# instead of implying full coverage.
#
# CONTROLS. Each cell is paired with a control: the same call with every input
# path PRESENT, which must still be refused -- by the crossed error, at the
# crossing's own grain. Without it a cell showing the crossed error would prove
# nothing, because a call whose second error was never live reports its only
# one, and the ordering claim would rest on that. `input_guard_dead_controls()`
# is that check run over the grid rather than by eye (the failure-identity rule).
# The `none` crossing's control is the one exception: it has no second error, so
# it must COMPILE, and that is what makes its cell's refusal attributable to the
# missing path at all.
#
# The nvenc encoder seam is held FULL (`h264_nvenc` present) for the whole grid
# except the `nvenc` crossing's own cells, which hold it EMPTY: an availability
# error that cannot fire is not an error this grid can be measured against.
# Holding it fixed keeps two refs measured under one encoder assumption by
# construction rather than by the machine that ran them.
#
# Every probe runs at `run = FALSE`, so no FFmpeg binary is needed and nothing
# is written to disk -- except the cells that are ABOUT ffm_batch()'s `run`
# guard, which supply their own bad value and abort before anything runs.
#
# Usage (from the package root):
#
#   source("data-raw/input-guard-baseline.R")
#   before <- input_guard_baseline("origin/master")
#   after  <- input_guard_baseline()
#   input_guard_vacuous(before)          # both empty: every control that owes a
#   input_guard_vacuous(after)           #   compile compiled on that ref
#   input_guard_refusals(before, after)  # empty: the same calls are refused
#   input_guard_message_regressions(before, after)  # empty: no cell reads worse
#                                        #   without its blame having moved
#   input_guard_blame_regressions(after) # empty: no cell blames anything but
#                                        #   the verb the user called
#   input_guard_missing_call(after)      # empty: no abort lost its `call`
#   input_guard_dead_controls(after)     # empty: every crossed cell's control
#                                        #   really does raise the crossed error
#   input_guard_ordering(before, after)  # which error each cell showed, before
#                                        #   and after -- the move, cell by cell
#   input_guard_misordered(after)        # empty: AC6's precedence holds at
#                                        #   every crossed cell
#   input_guard_unreported(after)        # empty: every UNCROSSED cell reports
#                                        #   the missing path, not something
#                                        #   else it tripped on the way
#   input_guard_unnamed(after)           # empty: every uncrossed cell's abort
#                                        #   names every missing path it
#                                        #   carried, and counts them once each
#   input_guard_uncovered(after)         # empty: every (verb, form, crossing)
#                                        #   the declarations ask for has a cell

source(file.path("data-raw", "codec-guard-baseline.R"))
source(file.path("tests", "testthat", "helper-input-paths.R"))

# -- the declared axes -------------------------------------------------------

# How the call's input paths are populated. See the header.
INPUT_GUARD_FORMS <- c("all", "one", "dup", "factor")

# The crossings each form is generated over; a form absent from this list is
# generated over every crossing its verb declares. Declared here rather than
# decided in the builder, so `input_guard_uncovered()` can re-derive exactly the
# product the grid was asked for.
INPUT_GUARD_FORM_CROSSINGS <- list(dup = "none", factor = "none")

# Whether a (form, crossing, shape) combination is owed a cell, and if so
# whether the shape can express it. Three answers, and the difference matters:
#
#   NA     not owed -- the form is not generated over this crossing at all
#   FALSE  owed but inexpressible by this verb's call shape; recorded as a
#          stated gap (`exists = FALSE`), never silently dropped
#   TRUE   generate the cell
#
# A shape that has been REMOVED (the AC7 mutation) answers TRUE wherever it can:
# with no declaration left there is nothing to say the verb carries one input
# path or a list-column, and a reader that guessed the narrower answer would
# under-report the gap it exists to find.
input_guard_form_applies <- function(form, crossing, shape) {
  only <- INPUT_GUARD_FORM_CROSSINGS[[form]]
  if (!is.null(only) && !crossing %in% only) return(NA)
  multi <- is.null(shape) || is.null(shape$multi) || isTRUE(shape$multi)
  in_column <- is.null(shape) || !is.null(shape$path_cols)
  switch(form,
    one = multi,
    dup = multi,
    factor = in_column,
    TRUE)
}

# The front-door aborts each verb carries, and therefore the crossings its cells
# are generated over. `"none"` is every verb's baseline cell -- the missing path
# and nothing else -- and is what the crossed cells are read against.
#
# A verb declares only the crossings it HAS: `concatenate_videos()` takes no
# codec, no hardware and no per-row knob, so it owes one cell per form and no
# more. Declaring a crossing a verb does not carry is not a silent error -- its
# control cannot raise the crossed error, and `input_guard_dead_controls()`
# names it.
#
# `jobs_na` is declared only where the verb HAS that guard. Six fan-out verbs
# validate their table inline rather than through check_batch_jobs(), and none
# of those six rejects an NA in the input carrier -- so for them the sweep is
# the first thing to read that column and there is no precedence to order.
# Declaring the crossing anyway would put six cells in the grid that measure
# the sweep against nothing; each of the six carries a `column_type` crossing
# instead, so every fan-out verb still has at least one guard ABOVE the sweep
# pinning AC6's first half.
INPUT_GUARD_CROSSINGS <- list(
  # -- the fan-out verbs (reach ffm_batch) -----------------------------------
  anonymize_video_batch = c("none", "column_type", "nvenc", "run_guard"),
  compare_videos_batch = c(
    "none", "jobs_na", "column_type", "contradiction:audio_codec",
    "contradiction:resize", "nvenc", "run_guard", "value:direction",
    "value:audio"),
  concatenate_videos_batch = c("none", "jobs_na", "run_guard"),
  convert_audio_batch = c("none", "jobs_na", "column_type", "run_guard"),
  crop_video_batch = c(
    "none", "jobs_na", "column_type", "nvenc", "run_guard"),
  extract_audio_batch = c("none", "jobs_na", "column_type", "run_guard"),
  extract_frame_batch = c("none", "column_type", "run_guard"),
  format_for_web_batch = c("none", "jobs_na", "nvenc", "run_guard"),
  normalize_audio_batch = c("none", "column_type", "run_guard"),
  picture_in_picture_batch = c(
    "none", "jobs_na", "column_type", "contradiction:audio_codec", "nvenc",
    "run_guard", "value:position", "value:margin", "value:audio"),
  sample_frames_batch = c("none", "column_type", "run_guard"),
  segment_video = c("none", "contradiction:reencode", "nvenc", "run_guard"),
  segment_video_batch = c(
    "none", "column_type", "contradiction:reencode", "nvenc", "run_guard"),
  separate_audio_video_batch = c(
    "none", "jobs_na", "column_type", "contradiction:copy_hardware", "nvenc",
    "run_guard"),
  standardize_video_batch = c("none", "column_type", "nvenc", "run_guard"),
  strip_metadata_batch = c("none", "jobs_na", "run_guard"),

  # -- the scalar verbs (reach ffm_files, not ffm_batch) ---------------------
  #
  # These have no `jobs` table, so no jobs-shape or column-type crossing, and
  # they do not reach ffm_batch()'s `run` guard. `concatenate_videos()` and
  # `compare_videos()` are the two this milestone gives a front door at all;
  # the rest are here because the criteria quantify over the whole walk-derived
  # set and a regression in the twelve that already had one is exactly what this
  # grid is measured across two refs to catch.
  anonymize_video = c("none", "nvenc"),
  compare_videos = c(
    "none", "contradiction:audio_codec", "contradiction:resize", "nvenc",
    "value:direction"),
  concatenate_videos = c("none"),
  convert_audio = c("none"),
  crop_video = c("none", "nvenc"),
  extract_audio = c("none"),
  extract_frame = c("none"),
  format_for_web = c("none", "nvenc"),
  normalize_audio = c("none"),
  picture_in_picture = c(
    "none", "contradiction:audio_codec", "nvenc", "value:position",
    "value:margin"),
  sample_frames = c("none"),
  separate_audio_video = c(
    "none", "contradiction:copy_hardware", "nvenc"),
  standardize_video = c("none", "nvenc"),
  strip_metadata = c("none")
)

# What each crossing supplies to a call: arguments beside `jobs`, extra `jobs`
# columns, whether an NA row is appended to the input carrier, how many input
# slots a row needs, and what encoder seam the cell runs under.
#
# Two crossings need the verb's `shape` to answer, and both for the same reason:
# WHICH column or argument carries the offence is the verb's business, while
# WHAT the offence is belongs to the crossing. `column_type` takes the column
# each shape declares as its wrong-type probe (a codec column on most verbs, a
# `timestamp` or `fps` column on the two that guard no codec), and `nvenc`
# names `video_codec` only where the verb has one -- `format_for_web()` does
# not, and passing it there raises "unused argument" instead of the crossed
# error, which is a cell measuring the grid rather than the package.
input_guard_crossing_parts <- function(crossing, shape) {
  base <- list(args = list(), cols = list(), na_row = FALSE, slots = NULL,
               seam = "h264_nvenc")
  utils::modifyList(base, switch(
    crossing,
    "none" = list(),
    # An NA in the input carrier, which check_batch_jobs()/check_fanin_jobs()
    # refuse ABOVE the sweep. Carried on an EXTRA row so the cell's own missing
    # paths keep their form: a cell that spent one of its input slots on the NA
    # would be measuring a different form than the one it is labelled with.
    "jobs_na" = list(na_row = TRUE),
    # A knob column of the wrong type, also above the sweep.
    "column_type" = list(cols = shape$type_col),
    "contradiction:copy_hardware" = list(
      args = list(video_codec = "copy", hardware = "nvenc")),
    "contradiction:reencode" = list(
      args = list(reencode = FALSE, video_codec = "libx264")),
    "contradiction:audio_codec" = list(args = list(audio_codec = "aac")),
    # `resize = TRUE` contradicts any input count but two, so this crossing is
    # the one that changes how many input slots a row carries.
    "contradiction:resize" = list(args = list(resize = TRUE), slots = 3L),
    "nvenc" = list(
      args = c(list(hardware = "nvenc"),
               if (isTRUE(shape$video_codec_arg)) list(video_codec = "libx264")),
      seam = character(0)),
    "run_guard" = list(args = list(run = "yes")),
    "value:direction" = list(args = list(direction = "sideways")),
    "value:position" = list(args = list(position = "middleish")),
    "value:margin" = list(args = list(margin = -3)),
    # Out of range for a row carrying two inputs, whichever verb: the legal
    # indices are 0 and 1.
    "value:audio" = list(args = list(audio = 7)),
    stop("unknown crossing: ", crossing)
  ))
}

# The crossing an abort MESSAGE belongs to, classified from the wording rather
# than from where the abort came from -- what the milestone is about is what the
# USER is shown. Ordered, and the first match wins; `"other"` is the catch-all
# that keeps an unrecognised message from being silently counted as one of
# these.
input_guard_error_crossing <- function(msg) {
  one <- function(m) {
    if (is.na(m)) return(NA_character_)
    has <- function(p) grepl(p, m, fixed = TRUE)
    if (has("not exist")) return("input")
    # The pipeline's own backstop, whose predicate is READABILITY rather than
    # existence and whose wording M62 deliberately leaves alone (AC1's pinned
    # residual, unified by M63). It is a distinct class here because on the
    # pre-change ref it is what an unguarded verb's missing input reported, and
    # collapsing it into `input` would hide exactly the move AC4 measures.
    if (has("Can't find or read")) return("ffm_files")
    # The NA guards come first because two of their wordings ("The `input`
    # column of `jobs` must not contain `NA`") also carry the phrase the
    # column-TYPE test below matches on.
    if (has("must not contain") || has("must be a character vector of") ||
        has("must have an") || has("must be a list-column")) return("jobs_na")
    if (has("column of")) return("column_type")
    if (has("can't be re-encoded") || has("needs re-encoding") ||
        has("reencode")) return("contradiction:reencode")
    if (has("copy") && has("hardware")) return("contradiction:copy_hardware")
    if (has("needs an audio stream to encode")) {
      return("contradiction:audio_codec")
    }
    if (has("supports exactly two inputs")) return("contradiction:resize")
    if (has("nvenc")) return("nvenc")
    if (has("`run` must be")) return("run_guard")
    if (has("direction")) return("value:direction")
    if (has("position")) return("value:position")
    if (has("margin")) return("value:margin")
    if (has("audio")) return("value:audio")
    "other"
  }
  vapply(msg, one, character(1), USE.NAMES = FALSE)
}

# -- the per-verb call shapes ------------------------------------------------
#
# Each entry says only what cannot be derived: how many input paths a legal call
# to that verb carries, and how to build the call from a list of rows. It never
# says which verbs exist -- that is `input_guard_verbs()`'s job -- and it never
# names a crossing.
#
# `rows` is a list, one element per jobs row, each element a character vector of
# that row's input paths (length `slots`). An NA element is the `jobs_na`
# crossing's extra row. `cols` are extra jobs columns (recycled to nrow) and
# `args` are extra arguments beside `jobs`.

tm_out <- function(i, ext = ".mp4") {
  file.path(tempdir(), sprintf("m62-out-%02d%s", i, ext))
}

# A jobs table over a single-path `input` column. `extra` is a function of the
# row count supplying the per-row columns that verb requires; `type_col` is the
# column the `column_type` crossing puts a wrong-typed value in, named here
# because which column a verb type-guards is the verb's business.
#
# `path_cols` names the jobs columns that CARRY paths as a plain vector, which
# is what the `factor` form re-types. A shape with none -- a list-column or a
# scalar signature -- has no factor form.
tm_shape_input <- function(verb, type_col = list(video_codec = 1), extra = NULL,
                           args = list(), video_codec_arg = TRUE) {
  list(
    slots = 1L, multi = TRUE, type_col = type_col, path_cols = "input",
    video_codec_arg = video_codec_arg,
    build = function(rows, cols, xargs) {
      n <- length(rows)
      jobs <- tibble::tibble(
        input = vapply(rows, function(r) r[[1]], character(1)))
      if (!is.null(extra)) {
        for (nm in names(extra(n))) jobs[[nm]] <- extra(n)[[nm]]
      }
      for (nm in names(cols)) jobs[[nm]] <- rep(cols[[nm]], length.out = n)
      c(list(jobs = jobs), args, xargs)
    })
}

# A jobs table over D015's `inputs` list-column. Every row carries `slots`
# paths; the NA row carries a vector with an NA in it, which is the shape
# check_fanin_jobs() refuses.
tm_shape_inputs <- function(verb, type_col = list(), args = list(),
                            video_codec_arg = FALSE) {
  list(
    slots = 2L, multi = TRUE, type_col = type_col,
    video_codec_arg = video_codec_arg,
    build = function(rows, cols, xargs) {
      n <- length(rows)
      jobs <- tibble::tibble(inputs = rows,
                             output = vapply(seq_len(n), tm_out, character(1)))
      for (nm in names(cols)) jobs[[nm]] <- rep(cols[[nm]], length.out = n)
      c(list(jobs = jobs), args, xargs)
    })
}

# picture_in_picture_batch()'s `main`/`overlay` pair: two input columns rather
# than one column or one list-column, and the third input shape the sweep has to
# handle.
tm_shape_pair <- function(type_col = list(video_codec = 1), args = list()) {
  list(
    slots = 2L, multi = TRUE, type_col = type_col,
    path_cols = c("main", "overlay"), video_codec_arg = TRUE,
    build = function(rows, cols, xargs) {
      n <- length(rows)
      jobs <- tibble::tibble(
        main = vapply(rows, function(r) r[[1]], character(1)),
        overlay = vapply(rows, function(r) r[[2]], character(1)),
        output = vapply(seq_len(n), tm_out, character(1)))
      for (nm in names(cols)) jobs[[nm]] <- rep(cols[[nm]], length.out = n)
      c(list(jobs = jobs), args, xargs)
    })
}

# A scalar verb: one row, its paths passed as arguments. `slots` is how many
# input paths the verb's signature takes, and `multi` follows from it -- a
# one-path verb has no `one` form to probe.
#
# `separate_args` says those slots are SEPARATE single-file arguments rather
# than one vector-valued one: `picture_in_picture(main, overlay)` against
# `compare_videos(infiles)`. Each such argument reports on its own, which is the
# single-file rendering AC2's first clause pins, so input_guard_unnamed() does
# not hold those calls to naming every missing path in one abort.
tm_shape_scalar <- function(slots, build, video_codec_arg = TRUE,
                            separate_args = FALSE) {
  list(slots = slots, multi = slots > 1L, type_col = list(),
       video_codec_arg = video_codec_arg, separate_args = separate_args,
       build = function(rows, cols, xargs) build(rows[[1]], xargs))
}

INPUT_GUARD_SHAPES <- list(
  anonymize_video_batch = tm_shape_input(
    "anonymize_video_batch",
    extra = function(n) list(
      regions = rep(list(data.frame(x = 0, y = 0, width = 10, height = 10)), n),
      output = vapply(seq_len(n), tm_out, character(1)))),
  compare_videos_batch = tm_shape_inputs(
    "compare_videos_batch", type_col = list(video_codec = 1),
    video_codec_arg = TRUE),
  concatenate_videos_batch = tm_shape_inputs("concatenate_videos_batch"),
  convert_audio_batch = tm_shape_input(
    "convert_audio_batch", type_col = list(audio_codec = 1),
    video_codec_arg = FALSE,
    extra = function(n) list(
      output = vapply(seq_len(n), tm_out, character(1), ".mp3"))),
  crop_video_batch = tm_shape_input(
    "crop_video_batch",
    extra = function(n) list(
      output = vapply(seq_len(n), tm_out, character(1))),
    args = list(width = 10, height = 10)),
  extract_audio_batch = tm_shape_input(
    "extract_audio_batch", type_col = list(audio_codec = 1),
    video_codec_arg = FALSE,
    extra = function(n) list(
      output = vapply(seq_len(n), tm_out, character(1), ".aac"))),
  # No codec column at all: its wrong-type probe is the selection column, which
  # is the type guard this verb does carry above the sweep.
  extract_frame_batch = tm_shape_input(
    "extract_frame_batch", type_col = list(timestamp = TRUE),
    video_codec_arg = FALSE,
    extra = function(n) list(
      timestamp = rep(1, n),
      output = vapply(seq_len(n), tm_out, character(1), ".png"))),
  format_for_web_batch = tm_shape_input(
    "format_for_web_batch", type_col = list(), video_codec_arg = FALSE,
    extra = function(n) list(
      output = vapply(seq_len(n), tm_out, character(1)))),
  normalize_audio_batch = tm_shape_input(
    "normalize_audio_batch", type_col = list(audio_codec = 1),
    video_codec_arg = FALSE,
    extra = function(n) list(
      output = vapply(seq_len(n), tm_out, character(1), ".wav"))),
  picture_in_picture_batch = tm_shape_pair(),
  # A per-row `outdir`, not the scalar argument: two rows sharing a directory
  # whose inputs share a basename collide on the frame pattern, and that guard
  # -- not the sweep, and not the crossing -- would be what every multi-row cell
  # here reported.
  sample_frames_batch = tm_shape_input(
    "sample_frames_batch", type_col = list(fps = TRUE),
    video_codec_arg = FALSE,
    extra = function(n) list(
      outdir = file.path(tempdir(), sprintf("m62-frames-%02d", seq_len(n)))),
    args = list(fps = 1)),
  segment_video_batch = tm_shape_input(
    "segment_video_batch",
    extra = function(n) list(start = rep(0, n), end = rep(1, n))),
  separate_audio_video_batch = tm_shape_input(
    "separate_audio_video_batch",
    extra = function(n) list(
      audiofile = vapply(seq_len(n), tm_out, character(1), ".aac"),
      videofile = vapply(seq_len(n), tm_out, character(1)))),
  standardize_video_batch = tm_shape_input(
    "standardize_video_batch",
    extra = function(n) list(
      output = vapply(seq_len(n), tm_out, character(1)))),
  strip_metadata_batch = tm_shape_input(
    "strip_metadata_batch", type_col = list(), video_codec_arg = FALSE,
    extra = function(n) list(
      output = vapply(seq_len(n), tm_out, character(1)))),

  # segment_video() is a SCALAR-signature verb that nonetheless reaches
  # ffm_batch (it fans one input out into many segments), which is why the walk
  # puts it in the fan-out set and why it carries the `run` crossing while the
  # other scalar verbs do not.
  segment_video = tm_shape_scalar(1L, function(p, xargs) c(
    list(infile = p[[1]], start = 0, end = 1), xargs)),

  anonymize_video = tm_shape_scalar(1L, function(p, xargs) c(
    list(infile = p[[1]], outfile = tm_out(1),
         regions = data.frame(x = 0, y = 0, width = 10, height = 10)), xargs)),
  compare_videos = tm_shape_scalar(2L, function(p, xargs) c(
    list(infiles = p, outfile = tm_out(1)), xargs)),
  concatenate_videos = tm_shape_scalar(2L, function(p, xargs) c(
    list(infiles = p, outfile = tm_out(1)), xargs)),
  convert_audio = tm_shape_scalar(1L, function(p, xargs) c(
    list(infile = p[[1]], outfile = tm_out(1, ".mp3")), xargs)),
  crop_video = tm_shape_scalar(1L, function(p, xargs) c(
    list(infile = p[[1]], outfile = tm_out(1), width = 10, height = 10),
    xargs)),
  extract_audio = tm_shape_scalar(1L, function(p, xargs) c(
    list(infile = p[[1]], outfile = tm_out(1, ".aac")), xargs)),
  extract_frame = tm_shape_scalar(1L, function(p, xargs) c(
    list(infile = p[[1]], outfile = tm_out(1, ".png"), timestamp = 1), xargs)),
  format_for_web = tm_shape_scalar(1L, function(p, xargs) c(
    list(infile = p[[1]], outfile = tm_out(1)), xargs),
    video_codec_arg = FALSE),
  normalize_audio = tm_shape_scalar(1L, function(p, xargs) c(
    list(infile = p[[1]], outfile = tm_out(1, ".wav")), xargs)),
  picture_in_picture = tm_shape_scalar(2L, function(p, xargs) c(
    list(main = p[[1]], overlay = p[[2]], outfile = tm_out(1)), xargs),
    separate_args = TRUE),
  sample_frames = tm_shape_scalar(1L, function(p, xargs) c(
    list(infile = p[[1]], outdir = tempdir(), fps = 1), xargs)),
  separate_audio_video = tm_shape_scalar(1L, function(p, xargs) c(
    list(infile = p[[1]], audiofile = tm_out(1, ".aac"),
         videofile = tm_out(1)), xargs)),
  standardize_video = tm_shape_scalar(1L, function(p, xargs) c(
    list(infile = p[[1]], outfile = tm_out(1)), xargs)),
  strip_metadata = tm_shape_scalar(1L, function(p, xargs) c(
    list(infile = p[[1]], outfile = tm_out(1)), xargs))
)

# The domain, taken from the walk. The walk fixes membership; the declarations
# below supply only shape and crossings, and never widen or narrow the set.
#
# What is asserted here and what is left to a reader is a deliberate split. An
# ORPHAN -- a shape or a crossing list for a verb the walk does not return -- is
# asserted, because no reader can see a cell that was never owed. A verb the
# walk returns with no CALL SHAPE is not asserted: it simply generates no cells,
# and `input_guard_uncovered()` reports every combination it owed. Asserting it
# here too would shadow that reader, leaving the completeness claim resting on
# an assertion nobody had tested rather than on the reader AC7 mutates.
input_guard_domain <- function(pkg = "tidymedia") {
  v <- input_guard_verbs(pkg)
  all_verbs <- c(v$fanout, v$scalar)
  missing_cross <- setdiff(all_verbs, names(INPUT_GUARD_CROSSINGS))
  if (length(missing_cross)) {
    stop("verbs with no crossing declaration: ",
         paste(missing_cross, collapse = ", "))
  }
  orphan <- union(setdiff(names(INPUT_GUARD_SHAPES), all_verbs),
                  setdiff(names(INPUT_GUARD_CROSSINGS), all_verbs))
  if (length(orphan)) {
    stop("declarations for verbs the walk does not return: ",
         paste(orphan, collapse = ", "))
  }
  all_verbs
}

# -- the probe grid ----------------------------------------------------------

# One cell and one control per (verb, form, crossing). `absent` is a path that
# does not exist; `present` is the packaged sample.
input_guard_cases <- function(present, absent, verbs = input_guard_domain()) {
  cases <- list()
  # `absent_paths` is the DISTINCT absent paths the cell carries, recorded so
  # input_guard_unnamed() can hold the abort to naming each of them exactly
  # once. Without it the grid can see that a cell was refused but not whether
  # the refusal accounted for every path the caller got wrong.
  add <- function(verb, form, crossing, control, exists, args, seam,
                  absent_paths = character()) {
    cases[[length(cases) + 1L]] <<- list(
      verb = verb, form = form, crossing = crossing, control = control,
      exists = exists, args = args, seam = seam,
      absent_paths = absent_paths)
  }

  for (verb in verbs) {
    shape <- INPUT_GUARD_SHAPES[[verb]]
    # No shape, no cells. See input_guard_domain(): this is the gap
    # input_guard_uncovered() is the instrument for.
    if (is.null(shape)) next
    for (crossing in INPUT_GUARD_CROSSINGS[[verb]]) {
      parts <- input_guard_crossing_parts(crossing, shape)
      slots <- if (is.null(parts$slots)) shape$slots else parts$slots
      cols <- parts$cols
      for (form in INPUT_GUARD_FORMS) {
        # Three answers, per input_guard_form_applies(): NA is not owed at all,
        # FALSE is owed but inexpressible by this shape -- the `one` and `dup`
        # forms need two input paths (a single-path scalar verb has none to
        # spare; a jobs table always does, because a second ROW is a second
        # path), and the `factor` form needs a plain path column.
        applies <- input_guard_form_applies(form, crossing, shape)
        if (is.na(applies)) next
        if (!applies) {
          add(verb, form, crossing, FALSE, FALSE, NULL, parts$seam)
          add(verb, form, crossing, TRUE, FALSE, NULL, parts$seam)
          next
        }
        build_rows <- function(paths) {
          if (identical(shape$slots, 1L) && !is.null(shape$build)) {
            # One path per row: the form is expressed as rows.
            lapply(paths, function(p) p)
          } else {
            # Several paths per row: the form is expressed within one row.
            list(paths)
          }
        }
        n_paths <- if (identical(shape$slots, 1L)) {
          if (identical(form, "all")) 1L else 2L
        } else {
          slots
        }
        # `absent` is a VECTOR of distinct nonexistent paths: `all` and `factor`
        # take a different one per slot, `dup` takes the same one every time,
        # and `one` leaves all but the last slot present.
        cell_paths <- switch(
          form,
          one = c(rep(present, n_paths - 1L), absent[[1]]),
          dup = rep(absent[[1]], n_paths),
          absent[seq_len(n_paths)]
        )
        ctrl_paths <- rep(present, n_paths)
        for (ctl in c(FALSE, TRUE)) {
          paths <- if (ctl) ctrl_paths else cell_paths
          rows <- build_rows(paths)
          if (parts$na_row) {
            rows <- c(rows, list(rep(NA_character_, length(rows[[1]]))))
          }
          args <- shape$build(rows, cols, parts$args)
          if (identical(form, "factor")) {
            for (nm in shape$path_cols) {
              args$jobs[[nm]] <- factor(args$jobs[[nm]])
            }
          }
          add(verb, form, crossing, ctl, TRUE, args, parts$seam,
              if (ctl) character() else unique(setdiff(paths, present)))
        }
      }
    }
  }
  cases
}

# -- running the grid against a ref ------------------------------------------

input_guard_baseline <- function(ref = NULL, root = ".") {
  env <- codec_guard_env(ref, root)
  present <- system.file("extdata", "sample.mp4", package = "tidymedia")
  if (!nzchar(present)) stop("sample.mp4 not found; install the package first")
  # Distinct absent paths, one per input slot the widest shape carries: a cell
  # whose missing paths are all the same string cannot tell "names every
  # missing path" from "names the first" (see the FORMS header).
  absent <- file.path(tempdir(), sprintf("m62-absent-input-%02d.mp4", 1:3))
  if (any(file.exists(absent))) {
    stop("an `absent` path exists: ",
         paste(absent[file.exists(absent)], collapse = ", "))
  }
  old <- options(tidymedia.nvenc_encoders = "h264_nvenc")
  on.exit(options(old), add = TRUE)

  rows <- lapply(input_guard_cases(present, absent), function(case) {
    blank <- data.frame(
      verb = case$verb, form = case$form, crossing = case$crossing,
      control = case$control, exists = case$exists,
      kind = NA_character_, outcome = NA_character_, call = NA_character_,
      in_index = NA, reported = NA_character_,
      absent = paste(case$absent_paths, collapse = "\037"),
      stringsAsFactors = FALSE)
    if (!case$exists) {
      blank$kind <- "nonexistent"
      return(blank)
    }
    options(tidymedia.nvenc_encoders = case$seam)
    args <- case$args
    if (!"run" %in% names(args)) args$run <- FALSE
    obs <- tryCatch(
      {
        # Call by NAME: do.call() on a function OBJECT records the anonymous
        # function as the condition call and hides the blame target this grid
        # exists to watch (the trap codec-guard-baseline.R names).
        out <- do.call(case$verb, args, envir = env)
        txt <- if (is.data.frame(out)) out$command else as.character(out)
        txt <- gsub(present, "<in>", txt, fixed = TRUE)
        txt <- gsub(tempdir(), "<tmp>", txt, fixed = TRUE)
        # The concat demuxer's list file is a fresh tempfile() per call, so its
        # random suffix differs between the two refs' runs and would read as a
        # command change on every concatenate cell. Scrubbing the DIRECTORY is
        # not enough: the randomness is in the basename.
        txt <- gsub("ffm-concat[0-9a-f]+\\.txt", "ffm-concat<rand>.txt", txt)
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
    blank$kind <- obs$kind
    blank$outcome <- obs$outcome
    blank$call <- obs$call
    blank$in_index <- obs$in_index
    blank$reported <- if (identical(obs$kind, "compiled")) {
      NA_character_
    } else {
      input_guard_error_crossing(obs$outcome)
    }
    blank
  })

  out <- do.call(rbind, rows)
  attr(out, "ref") <- if (is.null(ref)) "<working tree>" else ref
  out
}

# -- reading the result ------------------------------------------------------

# The vacuity screen, run on BOTH sides before any comparison. A `none` control
# is a wholly legal call and must COMPILE; a cell that did not is measuring
# something other than the sweep -- a schema error, a column this grid built
# wrong -- and such a cell compares equal across refs while carrying no
# evidence. Every other probe here is expected to abort, so a probe that
# compiled where an abort was owed is the other half of the screen.
input_guard_vacuous <- function(baseline) {
  live <- baseline[baseline$exists, , drop = FALSE]
  # The `factor` form's CONTROLS are excluded from both halves of the screen:
  # whether a factor path column is a legal call is each verb's own contract --
  # the verbs routing through check_batch_jobs() coerce it and compile,
  # segment_video_batch() never has and aborts downstream -- so neither answer
  # is owed and declaring one would be fitting the expectation to the
  # measurement. Those probes are here for the form's CELLS, whose blame
  # input_guard_blame_regressions() reads with no declared expectation at all.
  live <- live[!(live$control & live$form == "factor"), , drop = FALSE]
  owes_compile <- live$control & live$crossing == "none"
  bad <- (owes_compile & live$kind != "compiled") |
    (!owes_compile & live$kind == "compiled")
  out <- live[bad, c("verb", "form", "crossing", "control", "kind", "outcome")]
  out$problem <- ifelse(out$kind == "compiled",
                        "owed an abort but compiled",
                        "owed a compile but was refused")
  out
}

input_guard_key <- function(d) {
  paste(d$verb, d$form, d$crossing, d$control, sep = "\037")
}

input_guard_pair <- function(before, after) {
  only_before <- setdiff(input_guard_key(before), input_guard_key(after))
  only_after <- setdiff(input_guard_key(after), input_guard_key(before))
  if (length(only_before) > 0 || length(only_after) > 0) {
    stop("the two baselines cover different cells; ",
         length(only_before), " only in `before`, ",
         length(only_after), " only in `after`. ",
         "Re-run both sides with the same version of this script.")
  }
  before[match(input_guard_key(after), input_guard_key(before)), , drop = FALSE]
}

# AC5's first claim, as a query: the cells whose FATE changed. Moving a refusal
# to the front door must not change which calls are refused, nor what a call
# that compiles compiles to; only the blame and the moment may move. Empty is
# the evidence.
input_guard_refusals <- function(before, after) {
  b <- input_guard_pair(before, after)
  changed <- b$kind != after$kind |
    (b$kind == "compiled" & after$kind == "compiled" & b$outcome != after$outcome)
  changed[is.na(changed)] <- FALSE
  data.frame(verb = after$verb, form = after$form, crossing = after$crossing,
             control = after$control, before_kind = b$kind,
             after_kind = after$kind, before = b$outcome, after = after$outcome,
             stringsAsFactors = FALSE)[which(changed), , drop = FALSE]
}

# The cells whose abort MESSAGE changed, split by whether their blame moved.
# The split is the point, because the two halves have OPPOSITE expectations:
#
#   moved_blame = TRUE  -- expected. These are the cells the milestone set out
#     to fix, and their `before` text carries purrr's "In index: N / Caused by
#     error in ..." wrapper that the fix removes.
#   moved_blame = FALSE -- must not change, EXCEPT where the milestone
#     deliberately changed which of two live errors reports. That exception is
#     the crossed cells, and it is why the regression reader below is scoped to
#     the `none` ones; the crossed cells are not thereby unchecked --
#     input_guard_ordering() states which error each showed on each ref, which
#     is the stricter claim.
input_guard_messages <- function(before, after) {
  b <- input_guard_pair(before, after)
  both_abort <- b$kind == "abort" & after$kind == "abort"
  both_abort[is.na(both_abort)] <- FALSE
  changed <- both_abort & (b$outcome != after$outcome)
  changed[is.na(changed)] <- FALSE
  same_call <- (is.na(b$call) & is.na(after$call)) |
    (!is.na(b$call) & !is.na(after$call) & b$call == after$call)
  data.frame(verb = after$verb, form = after$form, crossing = after$crossing,
             control = after$control, moved_blame = !same_call,
             before = b$outcome, after = after$outcome,
             stringsAsFactors = FALSE)[which(changed), , drop = FALSE]
}

# The half that must be empty: a cell that reads differently WITHOUT its blame
# having moved and WITHOUT a second live error whose precedence this milestone
# changed. Empty is the evidence.
input_guard_message_regressions <- function(before, after) {
  m <- input_guard_messages(before, after)
  m[!m$moved_blame & m$crossing == "none", , drop = FALSE]
}

# Blame that points anywhere but the verb the user called. The invariant is
# absolute rather than comparative, which is why it needs no `before`: every
# aborting probe in this grid calls an exported verb directly, so the only name
# the caller may be shown is that verb's. Empty is the evidence.
#
# TWO classes are excluded. ffm_batch()'s `run` guard names `ffm_batch()` and
# has since long before this milestone; that exclusion is by the error
# REPORTED, not by the crossing declared, so a `run_guard` cell that starts
# reporting something else is still held to the invariant. The `factor` form's
# CONTROLS are the second: a factor path column is not a call shape every verb
# accepts (segment_video_batch() never coerced one), so where the verb refuses
# it the refusal comes from the pipeline, blamed on purrr::pmap, exactly as it
# did on the pre-change ref -- which input_guard_messages() and
# input_guard_blame() state by comparison rather than by declaration. The
# form's CELLS are held to the invariant in full: a sweep handed a factor
# column raw blames file.exists(), and that cell is what catches it.
input_guard_blame_regressions <- function(after) {
  own <- after$reported != "run_guard"
  own[is.na(own)] <- TRUE
  factor_control <- after$control & after$form == "factor"
  bad <- own & !factor_control & after$kind == "abort" & !is.na(after$call) &
    after$call != after$verb
  bad[is.na(bad)] <- FALSE
  data.frame(verb = after$verb, form = after$form, crossing = after$crossing,
             control = after$control, blamed = after$call,
             message = after$outcome,
             stringsAsFactors = FALSE)[which(bad), , drop = FALSE]
}

# The cells whose BLAME moved -- what the milestone set out to change. Expect
# every fan-out verb's `none` cell here, `before` naming purrr::pmap or
# ffm_files and `after` naming the verb the user called, with `in_index`
# dropping to FALSE.
input_guard_blame <- function(before, after) {
  b <- input_guard_pair(before, after)
  same_call <- (is.na(b$call) & is.na(after$call)) |
    (!is.na(b$call) & !is.na(after$call) & b$call == after$call)
  same_index <- b$in_index == after$in_index
  same_index[is.na(b$in_index) & is.na(after$in_index)] <- TRUE
  same_index[is.na(same_index)] <- FALSE
  changed <- !same_call | !same_index
  data.frame(verb = after$verb, form = after$form, crossing = after$crossing,
             control = after$control, before_call = b$call,
             after_call = after$call, before_index = b$in_index,
             after_index = after$in_index,
             stringsAsFactors = FALSE)[which(changed), , drop = FALSE]
}

# AC6's claim, as a query: for every crossed cell, which error reported before
# and which reports after, beside the control proving the crossed error was live
# on that call at all.
#
# Read it as two blocks:
#
#   crossing above the sweep (`jobs_na`, `column_type`) -- every cell must read
#     the crossing on BOTH refs. These are invariants, not changes: a sweep
#     hoisted above the shape and type guards would invert them silently.
#   crossing below the sweep (`contradiction:*`, `nvenc`, `run_guard`,
#     `value:*`) -- every cell must read the crossing BEFORE and `input` AFTER.
#     That flip is the milestone.
input_guard_ordering <- function(before, after) {
  keep <- after$exists & after$crossing != "none"
  b <- input_guard_pair(before, after)
  out <- data.frame(verb = after$verb, form = after$form,
                    crossing = after$crossing, control = after$control,
                    above = after$crossing %in% INPUT_GUARD_ABOVE,
                    before = b$reported, after = after$reported,
                    stringsAsFactors = FALSE)
  out[which(keep), , drop = FALSE]
}

# The crossings whose guard sits ABOVE the sweep, and which must therefore keep
# reporting after this milestone. Declared, not derived: this is the expectation
# the grid is measured against, and reading it off either ref's behaviour would
# make the comparison circular.
INPUT_GUARD_ABOVE <- c("jobs_na", "column_type")

# The controls that failed to establish their crossed error. Empty is the
# evidence; a non-empty result names cells whose ordering claim rests on
# nothing. Compared at the CROSSING's own grain, never at a coarser one: a
# control for `contradiction:resize` that raises the `audio_codec`
# contradiction instead has not established the error its cell is crossed with
# (the M61 review's F4, kept).
input_guard_dead_controls <- function(after) {
  d <- after[after$exists & after$control & after$crossing != "none", ,
             drop = FALSE]
  bad <- is.na(d$reported) | d$reported != d$crossing
  d[bad, c("verb", "form", "crossing", "reported", "kind", "outcome"),
    drop = FALSE]
}

# AC6's claim as a pass/fail query rather than a table to read. Empty is the
# evidence; a non-empty result names the cells whose precedence is not what the
# milestone promised.
#
# Stated over the AFTER ref alone, deliberately. AC6 is a claim about where the
# sweep sits now, and the pre-change answer is not one value per crossing: a
# verb that already guarded its input reported `input` before as well, and the
# two scalar fan-in verbs reported `ffm_files` for some crossings and the
# crossing itself for others, depending on where their pipeline happened to
# reach ffm_files(). Turning that into a declared `want_before` would be
# fitting the expectation to the measurement. What the before ref is for is
# AC5's claims -- fate, message, blame -- and the ordering TABLE above, which
# shows the move without asserting a single shape for it.
input_guard_misordered <- function(after) {
  o <- input_guard_ordering(after, after)
  o <- o[!o$control, , drop = FALSE]
  want <- ifelse(o$above, o$crossing, "input")
  bad <- is.na(o$after) | o$after != want
  cbind(o[bad, setdiff(names(o), "before"), drop = FALSE], want = want[bad])
}

# The claim the crossed cells cannot make: an UNCROSSED cell -- a missing path
# and nothing else wrong with the call -- must report the missing path. Empty is
# the evidence.
#
# The ordering readers above filter `none` out, because a cell with no second
# error has no precedence to state, and the message readers compare the two refs
# rather than naming an expected error. Between them nothing asserted that a
# `none` cell reports `input` at all, so a cell aborting for a reason of its own
# -- a table this grid built wrong, a guard that fires above the sweep on a
# shape only one form produces -- counted as a refusal and read as coverage.
# That is the vacuity trap one level up from the one input_guard_vacuous()
# screens for, and it is what the `dup` and `factor` forms would otherwise sit
# in: both put a second row or a second type in front of the sweep.
input_guard_unreported <- function(after) {
  d <- after[after$exists & !after$control & after$crossing == "none", ,
             drop = FALSE]
  bad <- is.na(d$reported) | d$reported != "input"
  d[bad, c("verb", "form", "kind", "reported", "call", "outcome"),
    drop = FALSE]
}

# AC2's second clause -- "names every missing path, not the first" -- as a query
# over the whole domain rather than at the two or three call shapes anyone would
# think to type. For every uncrossed cell: each DISTINCT absent path the call
# carried must appear in the abort, and the count the abort states must be the
# number of distinct absent paths. Empty is the evidence.
#
# Both halves are load-bearing, and each catches a defect the other cannot. A
# verb sweeping its two input columns in two aborts names the first and stops
# (M62 review F2): every path named, false. A checker that does not deduplicate
# turns one mistyped path shared by N rows into N missing files (M62 review F3):
# count right, false. Neither was visible while every cell's missing paths were
# one constant string repeated.
#
# The count is read only from the two wordings that state one -- this package's
# and the pipeline backstop's -- and never as "the first integer in the
# message", which would find the `1` in purrr's "In index: 1" wrapper on the
# pre-change ref and the `62` in this grid's own path names on both. A message
# stating no count is held to carrying exactly one absent path, which is the
# single-file rendering's contract.
#
# Stated over the AFTER ref alone, for M62-D1's reason: the pre-change ref
# reported these calls through the pipeline one ROW at a time, so a two-row cell
# named one row's path and stopped, and asserting the claim there would be
# asserting that the defect this milestone removes was already absent. Run it
# over the before ref to SEE the move, never to gate it.
#
# The verbs whose shape declares `separate_args` are excluded, and the exclusion
# is declared rather than discovered: picture_in_picture() takes `main` and
# `overlay` as two SINGLE-FILE arguments, and a single-file argument reporting
# on its own is AC2's first clause, not a violation of its second. Its jobs-table
# sibling, whose two columns are one carrier swept once, is held to the claim in
# full. This is unchanged from the pre-change ref for that verb, which the
# message and blame readers state by comparison.
input_guard_unnamed <- function(after) {
  separate <- vapply(after$verb, function(v)
    isTRUE(INPUT_GUARD_SHAPES[[v]]$separate_args), logical(1),
    USE.NAMES = FALSE)
  after <- after[!separate, , drop = FALSE]
  d <- after[after$exists & !after$control & after$crossing == "none" &
               after$kind == "abort" & nzchar(after$absent), , drop = FALSE]
  if (nrow(d) == 0) return(d[, c("verb", "form", "outcome"), drop = FALSE])
  problem <- vapply(seq_len(nrow(d)), function(i) {
    paths <- strsplit(d$absent[[i]], "\037", fixed = TRUE)[[1]]
    msg <- d$outcome[[i]]
    unnamed <- paths[!vapply(paths, grepl, logical(1), x = msg, fixed = TRUE)]
    if (length(unnamed)) {
      return(paste0("not named: ", paste(basename(unnamed), collapse = ", ")))
    }
    stated <- NA_integer_
    for (pat in c("names? ([0-9]+) files? that",
                  "find or read ([0-9]+) input file")) {
      m <- regmatches(msg, regexec(pat, msg))[[1]]
      if (length(m) == 2L) stated <- as.integer(m[[2]])
    }
    want <- length(paths)
    if (is.na(stated)) {
      if (want != 1L) return(paste0("states no count but carried ", want))
      return(NA_character_)
    }
    if (stated != want) {
      return(paste0("states ", stated, " but carried ", want))
    }
    NA_character_
  }, character(1))
  out <- d[!is.na(problem), c("verb", "form", "outcome"), drop = FALSE]
  out$problem <- problem[!is.na(problem)]
  out
}

# AC5's completeness claim, as a query rather than as vigilance. Empty is the
# evidence; a non-empty result names the (verb, form, crossing) combinations the
# declarations ask for that no cell in the grid probes.
#
# It re-derives the product from the walk-derived domain x
# INPUT_GUARD_CROSSINGS x INPUT_GUARD_FORMS and looks each combination up in
# the baseline the grid actually produced. Because the cells are generated from
# the same declarations, this cannot catch a crossing dropped from the
# declaration -- what it catches is a verb whose shape produced nothing, a form
# never emitted, and any combination whose builder silently failed. Those are
# exactly the failures M61's three review rounds each hit once.
#
# A cell recorded `exists = FALSE` does NOT count as coverage: the `one` form of
# a single-path verb is a stated gap, and letting the gap satisfy the product
# would make the reader agree with any grid at all.
input_guard_uncovered <- function(after, verbs = input_guard_domain()) {
  live <- after[after$exists & !after$control, , drop = FALSE]
  have <- paste(live$verb, live$form, live$crossing, sep = "\037")
  want <- list()
  for (verb in verbs) {
    # A verb whose shape has been removed owes BOTH forms: with no declaration
    # left there is nothing to say it carries only one input path, and a reader
    # that guessed the narrower answer would under-report the very gap it exists
    # to find.
    shape <- INPUT_GUARD_SHAPES[[verb]]
    for (crossing in INPUT_GUARD_CROSSINGS[[verb]]) {
      for (form in INPUT_GUARD_FORMS) {
        applies <- input_guard_form_applies(form, crossing, shape)
        if (is.na(applies) || !applies) next
        want[[length(want) + 1L]] <- data.frame(
          verb = verb, form = form, crossing = crossing,
          stringsAsFactors = FALSE)
      }
    }
  }
  want <- do.call(rbind, want)
  key <- paste(want$verb, want$form, want$crossing, sep = "\037")
  want[!key %in% have, , drop = FALSE]
}

# The lost-`call` reader. An abort with no `conditionCall()` is an unattributed
# base-R error, which is the shape a guard degrades to when a type check is
# moved out from above it. Every aborting probe in this grid calls an exported
# verb directly, so every one must carry a call. Empty is the evidence.
input_guard_missing_call <- function(after) {
  bad <- after$kind == "abort" & is.na(after$call)
  bad[is.na(bad)] <- FALSE
  data.frame(verb = after$verb, form = after$form, crossing = after$crossing,
             control = after$control, message = after$outcome,
             stringsAsFactors = FALSE)[which(bad), , drop = FALSE]
}
