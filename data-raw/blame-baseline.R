# blame-baseline.R ----------------------------------------------------------
#
# Regenerate M64's blame baseline from an arbitrary git ref, so the claim
# "these calls now name the verb, and nothing else about them changed" is
# re-derivable evidence rather than an implementation-time transcript
# (M64 T6, AC3).
#
# Same ref machinery as `data-raw/value-guard-baseline.R`: both source
# `data-raw/codec-guard-baseline.R` for its `git show`-into-an-environment
# helper rather than copying it.
#
# THE CELLS ARE NOT DECLARED HERE. They come from
# `tests/testthat/helper-blame-specs.R`, the same file the test grid reads, so
# the evidence and the assertion cannot describe different grids. That helper
# lives in the test tree because `^data-raw$` is in .Rbuildignore and a test
# sourcing a spec list from here would skip under `R CMD check` (LESSONS
# M51/M59); this script reads it from the source tree, where it always exists.
#
# Each cell records BOTH observables, because they answer different questions
# and only one of them is what this milestone changed:
#
#   call     the function part of conditionCall() -- the blame target, what the
#            console prints as "Error in `<verb>()`". THIS is what M64 moves.
#   outcome  the condition message. This must NOT move: the fix routes each
#            value to the same shared checker the builder already called, so a
#            changed message means a second copy of the wording got written.
#
# A grid recording only `outcome` would have read green over a branch that
# moved no blame at all -- the message is identical either side of the fix.
#
# The nvenc encoder seam is held EMPTY for the whole grid: no cell here names
# `hardware`, and holding it fixed keeps the two refs measured under one
# encoder assumption by construction rather than by the machine that ran them.
# Every probe runs at `run = FALSE`, so no FFmpeg binary is needed.
#
# Usage:
#
#   source("data-raw/blame-baseline.R")
#   before <- blame_baseline("origin/master")
#   after  <- blame_baseline()            # the working tree
#   blame_vacuous(after)                  # empty: every cell actually aborted
#   blame_moves(before, after)            # the blame targets that changed
#   blame_message_drift(before, after)    # empty: no wording was rewritten

source(file.path("data-raw", "codec-guard-baseline.R"))

# The spec list, read from the test tree. `blame_specs()` is the only object
# taken; the completeness reader beside it is the test's business.
blame_cells <- function(input, outdir, root = ".") {
  env <- new.env(parent = globalenv())
  sys.source(file.path(root, "tests", "testthat", "helper-blame-specs.R"),
             envir = env, keep.source = FALSE)
  env$blame_specs(input, outdir)
}

# Probe every cell in one ref's sources and return a data frame of
# observations.
blame_baseline <- function(ref = NULL, root = ".") {
  env <- codec_guard_env(ref, root)
  sample <- system.file("extdata", "sample.mp4", package = "tidymedia")
  if (!nzchar(sample)) stop("sample.mp4 not found; install the package first")
  outdir <- file.path(tempdir(), "blame-frames")
  old <- options(tidymedia.nvenc_encoders = character(0))
  on.exit(options(old), add = TRUE)

  rows <- lapply(blame_cells(sample, outdir, root), function(cell) {
    args <- cell$args
    if (!"run" %in% names(args)) args$run <- FALSE
    obs <- tryCatch(
      {
        # Call by NAME: do.call() on a function OBJECT records the anonymous
        # function as the condition call and hides the very blame target this
        # grid exists to watch.
        out <- do.call(cell$verb, args, envir = env)
        txt <- if (is.data.frame(out)) out$command else as.character(out)
        txt <- gsub(sample, "<in>", txt, fixed = TRUE)
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
             call = if (is.null(cl)) NA_character_ else
               paste(deparse(cl[[1]]), collapse = ""),
             # Present exactly when the value was read inside the fan-out
             # rather than at the front door -- the batch form of the leak.
             in_index = grepl("In index:", msg, fixed = TRUE))
      }
    )
    data.frame(id = cell$id, verb = cell$verb, form = cell$form,
               delivery = cell$delivery, argument = cell$argument,
               pinned = isTRUE(cell$pinned),
               kind = obs$kind, outcome = obs$outcome, call = obs$call,
               in_index = obs$in_index, stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, rows)
  attr(out, "ref") <- if (is.null(ref)) "<working tree>" else ref
  out
}

# -- reading the result ------------------------------------------------------

# The vacuity screen, run on BOTH sides before any comparison. Every cell in
# this grid is a violating call, so every cell must ABORT; one that compiled is
# measuring nothing and would compare equal across refs while carrying no
# evidence (the M58/M61 dead-cell trap).
blame_vacuous <- function(x) {
  x[x$kind != "abort", c("id", "kind", "outcome")]
}

# The blame targets that changed. This is the milestone's headline evidence:
# every non-pinned cell must appear here, and every pinned cell must not.
blame_moves <- function(before, after) {
  stopifnot(identical(before$id, after$id))
  changed <- before$call != after$call | is.na(before$call) != is.na(after$call)
  data.frame(id = before$id[changed], pinned = before$pinned[changed],
             from = before$call[changed], to = after$call[changed],
             stringsAsFactors = FALSE)
}

# Message drift. The fix routes every value to the checker the builder already
# called, so no cell's wording should move. A non-empty result means a second
# copy of a message was written somewhere, and each row belongs in the
# milestone's Deviations table with a reason -- or in a fix.
#
# The `In index:` prefix purrr wraps a fan-out abort in is stripped before
# comparing: its disappearance is the blame move already reported above, not a
# rewording, and leaving it in would flag every batch cell as drifted.
blame_message_drift <- function(before, after) {
  stopifnot(identical(before$id, after$id))
  strip <- function(x) {
    x <- sub("^.*?Caused by error in `[^`]*`:\n", "", x)
    x <- gsub("i In index: [0-9]+\\.\n?", "", x)
    # cli marks the headline bullet of a WRAPPED abort with "! " and leaves it
    # off an unwrapped one, so every batch cell would read as drifted on that
    # character alone. Strip it on both sides: it is punctuation the wrapper
    # added, not wording anyone wrote.
    x <- sub("^!\\s*", "", trimws(x))
    trimws(x)
  }
  b <- strip(before$outcome)
  a <- strip(after$outcome)
  data.frame(id = before$id[b != a], before = b[b != a], after = a[b != a],
             stringsAsFactors = FALSE)
}
