# codec-guard-baseline.R -----------------------------------------------------
#
# Regenerate the codec-argument front-door baseline from an arbitrary git ref,
# so "this milestone's guards reject nothing that worked before" is re-derivable
# evidence rather than an implementation-time transcript (M41 T2/T8, AC4).
#
# For every task verb and `_batch` sibling carrying a `video_codec` or
# `audio_codec` argument that *sets* a codec, this probes five scenarios per
# verb/argument pair and records the observable outcome:
#
#   default  the argument left alone            -> compiled command
#   null     the argument passed NULL           -> compiled command, or an abort
#   na       the argument passed NA             -> an abort (AC1/AC2)
#   number   the argument passed 1              -> an abort (AC2)
#   vec2     the argument passed c("aac","mp3") -> an abort (AC2)
#
# Each scenario is probed twice on a `_batch` verb, once per value of a second
# dimension -- whether `jobs` carries a column of the same name as the argument
# under test:
#
#   col = absent   the jobs table has no such column
#   col = present  the jobs table carries a valid codec in that column
#
# The `present` half exists because `pick()` prefers the column over the scalar
# argument, so the scalar is never read there: a bad value in it used to be
# ignored outright rather than refused. A grid probing only `absent` is
# structurally blind to that path, which is how the first pass at this script
# measured AC4 as clean while the contract had in fact moved on four verbs
# (review F2/F7 -> M41-D2). Scalar verbs have no jobs table and are recorded as
# `absent`.
#
# `verify_media()` is excluded by design: its same-named arguments are *expected
# probe values*, not codec settings, so a guard there would be a contract change
# rather than validation parity (AC2).
#
# For an abort it records the message, the deparsed `conditionCall()` (the AC2
# blame target) and whether the message carries purrr's `In index: <n>` marker
# (AC3: present means the check ran inside the fan-out, not at the front door).
# For a success it records the compiled command with input paths scrubbed, so
# two refs compared on different machines do not diff on temp paths.
#
# Every probe runs at `run = FALSE`, so no FFmpeg binary is needed and nothing
# is written to disk.
#
# Usage (from the package root):
#
#   source("data-raw/codec-guard-baseline.R")
#   before <- codec_guard_baseline("origin/master")  # a git ref
#   after  <- codec_guard_baseline()                 # the working tree
#   codec_guard_diff(before, after)
#
# `codec_guard_diff()` returns the rows whose outcome changed. AC4 asks that no
# `default` or `null` row appear in that diff at either `col` setting, and that
# the `col = present` rows that do appear are confined to the four `_batch` verbs
# M41-D2 names.

# -- loading a ref's sources -------------------------------------------------

# Build the stand-in for the package's *imports* environment: the bindings
# NAMESPACE brings in unqualified. Almost every internal call is already
# `pkg::fun()` qualified, but the `importFrom()` lines are not (`glue()` and
# `tibble()` are called bare), and sourcing outside a namespace would leave
# those unresolved -- surfacing as "could not find function" aborts that
# masquerade as the codec aborts this script exists to measure.
codec_guard_imports <- function(root = ".", ref = NULL) {
  imports <- new.env(parent = globalenv())
  ns <- if (is.null(ref)) {
    readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  } else {
    # Checked like every other git call in this file: an unchecked failure here
    # yields an EMPTY imports env, and every bare glue()/tibble() call then
    # aborts as "could not find function" -- the masquerade the comment above
    # warns about, arriving as a fake codec abort in every row (review F13).
    text <- system2("git", c("-C", shQuote(root), "show",
                             shQuote(paste0(ref, ":NAMESPACE"))),
                    stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(text, "status"))) {
      stop("git show failed for ", ref, ":NAMESPACE: ",
           paste(text, collapse = " "))
    }
    text
  }
  for (line in grep("^importFrom\\(", ns, value = TRUE)) {
    parts <- strsplit(sub("^importFrom\\(([^)]*)\\).*$", "\\1", line), ",")[[1]]
    pkg <- trimws(parts[[1]])
    for (obj in trimws(parts[-1])) {
      assign(obj, get(obj, envir = asNamespace(pkg)), envir = imports)
    }
  }
  for (line in grep("^import\\(", ns, value = TRUE)) {
    pkg <- trimws(sub("^import\\(([^)]*)\\).*$", "\\1", line))
    nsp <- asNamespace(pkg)
    for (obj in getNamespaceExports(nsp)) {
      assign(obj, get(obj, envir = nsp), envir = imports)
    }
  }
  imports
}

# Materialize `R/*.R` as of `ref` into a fresh temp dir via `git show`, source
# them into a new environment, and return it. `ref = NULL` reads the working
# tree instead. The environment's parent is the imports env above, so both the
# `pkg::fun()` calls and the unqualified `importFrom()` ones resolve.
codec_guard_env <- function(ref = NULL, root = ".") {
  env <- new.env(parent = codec_guard_imports(root, ref))

  if (is.null(ref)) {
    files <- sort(list.files(file.path(root, "R"), pattern = "\\.R$",
                             full.names = TRUE))
    if (length(files) == 0) stop("No R/*.R files found under ", root)
  } else {
    listing <- system2("git", c("-C", shQuote(root), "ls-tree", "--name-only",
                               shQuote(ref), "R/"),
                       stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(listing, "status"))) {
      stop("git ls-tree failed for ref ", ref, ": ",
           paste(listing, collapse = " "))
    }
    listing <- grep("\\.R$", listing, value = TRUE)
    if (length(listing) == 0) stop("Ref ", ref, " has no R/*.R files")

    dir <- file.path(tempfile("codec-guard-"), "R")
    dir.create(dir, recursive = TRUE)
    files <- character()
    for (path in listing) {
      text <- system2("git", c("-C", shQuote(root), "show",
                               shQuote(paste0(ref, ":", path))),
                      stdout = TRUE, stderr = TRUE)
      if (!is.null(attr(text, "status"))) {
        stop("git show failed for ", ref, ":", path)
      }
      dest <- file.path(dir, basename(path))
      writeLines(text, dest)
      files <- c(files, dest)
    }
    files <- sort(files)
  }

  for (f in files) sys.source(f, envir = env, keep.source = FALSE)
  env
}

# -- the probe grid ----------------------------------------------------------

# One in-bounds fill box for the anonymize verbs. sample.mp4 is larger than
# 32x32, so this passes check_regions() and ffm_drawbox()'s dimension checks and
# lets the probe reach the codec argument.
codec_guard_regions <- function() {
  data.frame(x = 0, y = 0, width = 32, height = 32)
}

# The AC2 verb/argument set, as a call template per verb. Each entry supplies
# the arguments a probe call needs *besides* the codec argument under test, and
# names which codec arguments that verb carries. `sample` is substituted for the
# input-file placeholder(s) at probe time.
codec_guard_verbs <- function() {
  list(
    # `regions` is a data frame with one row per box (x/y/width/height), and the
    # batch verb carries it as a list-column of such frames.
    anonymize_video = list(
      args = c("video_codec", "audio_codec"),
      call = function(s, o) list(infile = s, outfile = o,
                                 regions = codec_guard_regions())),
    anonymize_video_batch = list(
      args = c("video_codec", "audio_codec"),
      call = function(s, o) list(jobs = tibble::tibble(
        input = s, output = o, regions = list(codec_guard_regions())))),
    compare_videos = list(
      args = c("video_codec", "audio_codec"),
      call = function(s, o) list(infiles = c(s, s), outfile = o)),
    compare_videos_batch = list(
      args = c("video_codec", "audio_codec"),
      call = function(s, o) list(jobs = tibble::tibble(
        inputs = list(c(s, s)), output = o))),
    convert_audio = list(
      args = "audio_codec",
      call = function(s, o) list(infile = s, outfile = sub("\\.mp4$", ".mp3", o))),
    convert_audio_batch = list(
      args = "audio_codec",
      call = function(s, o) list(jobs = tibble::tibble(
        input = s, output = sub("\\.mp4$", ".mp3", o)))),
    crop_video = list(
      args = c("video_codec", "audio_codec"),
      call = function(s, o) list(infile = s, outfile = o,
                                 width = 32, height = 32)),
    crop_video_batch = list(
      args = c("video_codec", "audio_codec"),
      call = function(s, o) list(jobs = tibble::tibble(input = s, output = o),
                                 width = 32, height = 32)),
    extract_audio = list(
      args = "audio_codec",
      call = function(s, o) list(infile = s, outfile = sub("\\.mp4$", ".aac", o))),
    extract_audio_batch = list(
      args = "audio_codec",
      call = function(s, o) list(jobs = tibble::tibble(
        input = s, output = sub("\\.mp4$", ".aac", o)))),
    normalize_audio = list(
      args = "audio_codec",
      call = function(s, o) list(infile = s, outfile = o)),
    normalize_audio_batch = list(
      args = "audio_codec",
      call = function(s, o) list(jobs = tibble::tibble(input = s, output = o))),
    picture_in_picture = list(
      args = c("video_codec", "audio_codec"),
      call = function(s, o) list(main = s, overlay = s, outfile = o)),
    picture_in_picture_batch = list(
      args = c("video_codec", "audio_codec"),
      call = function(s, o) list(jobs = tibble::tibble(
        inputs = list(c(s, s)), output = o))),
    segment_video = list(
      args = c("video_codec", "audio_codec"),
      call = function(s, o) list(infile = s, start = 0, end = 1,
                                 outfiles = o)),
    segment_video_batch = list(
      args = c("video_codec", "audio_codec"),
      call = function(s, o) list(jobs = tibble::tibble(
        input = s, start = 0, end = 1, output = o))),
    separate_audio_video = list(
      args = c("video_codec", "audio_codec"),
      call = function(s, o) list(infile = s,
                                 audiofile = sub("\\.mp4$", ".aac", o),
                                 videofile = o)),
    separate_audio_video_batch = list(
      args = c("video_codec", "audio_codec"),
      call = function(s, o) list(jobs = tibble::tibble(
        input = s, audiofile = sub("\\.mp4$", ".aac", o),
        videofile = o))),
    standardize_video = list(
      args = c("video_codec", "audio_codec"),
      call = function(s, o) list(infile = s, outfile = o)),
    standardize_video_batch = list(
      args = c("video_codec", "audio_codec"),
      call = function(s, o) list(jobs = tibble::tibble(input = s, output = o)))
  )
}

# The five scenarios, as a list of value-producing thunks. `default` is spelled
# by *omitting* the argument, which is why the value is a sentinel rather than
# a value.
codec_guard_scenarios <- list(
  default = quote(OMIT),
  null    = quote(NULL),
  na      = quote(NA),
  number  = quote(1),
  vec2    = quote(c("aac", "mp3"))
)

# A valid codec for the `col = present` half of the grid. It must be a value the
# per-row column guards accept, so that the column genuinely wins the `pick()`
# and the scalar argument is the only thing under test: "copy" is refused
# outright by several verbs and NA is the column form of the NULL sentinel, so
# neither would isolate the scalar.
codec_guard_col_value <- function(arg) {
  if (arg == "video_codec") "libx264" else "aac"
}

# Which `col` settings apply to a verb: a scalar verb has no jobs table, so
# `present` is not a state it can be in.
codec_guard_cols <- function(base) {
  if ("jobs" %in% names(base)) c("absent", "present") else "absent"
}

# -- running the grid --------------------------------------------------------

# Run every scenario for every verb/argument pair against `env`, returning a
# data frame of observations. `input` paths are scrubbed out of compiled
# commands so two machines' baselines compare equal.
codec_guard_baseline <- function(ref = NULL, root = ".", sample = NULL) {
  env <- codec_guard_env(ref, root)
  if (is.null(sample)) sample <- normalizePath(
    file.path(root, "inst", "extdata", "sample.mp4"), mustWork = TRUE)
  outfile <- file.path(tempdir(), "codec-guard-out.mp4")

  verbs <- codec_guard_verbs()
  rows <- list()

  for (verb in names(verbs)) {
    spec <- verbs[[verb]]
    f <- get0(verb, envir = env, inherits = FALSE)
    if (!is.function(f)) {
      # A ref predating the verb (or its rename) has nothing to compare.
      for (arg in spec$args) for (sc in names(codec_guard_scenarios)) {
        for (cl in codec_guard_cols(spec$call(sample, outfile))) {
          rows[[length(rows) + 1]] <- data.frame(
            verb = verb, arg = arg, scenario = sc, col = cl, kind = "absent",
            outcome = NA_character_, call = NA_character_,
            in_index = NA, stringsAsFactors = FALSE)
        }
      }
      next
    }
    if (!all(spec$args %in% names(formals(f)))) {
      for (arg in setdiff(spec$args, names(formals(f)))) {
        for (sc in names(codec_guard_scenarios)) {
          for (cl in codec_guard_cols(spec$call(sample, outfile))) {
            rows[[length(rows) + 1]] <- data.frame(
              verb = verb, arg = arg, scenario = sc, col = cl, kind = "absent",
              outcome = NA_character_, call = NA_character_,
              in_index = NA, stringsAsFactors = FALSE)
          }
        }
      }
    }

    for (arg in intersect(spec$args, names(formals(f)))) {
      for (sc in names(codec_guard_scenarios)) {
       for (cl in codec_guard_cols(spec$call(sample, outfile))) {
        base <- spec$call(sample, outfile)
        base$run <- FALSE
        # `parallel = FALSE` is the default, but AC3 is about this exact path,
        # so the probe states it rather than inheriting it.
        if ("parallel" %in% names(formals(f))) base$parallel <- FALSE
        if (sc != "default") {
          # `base[[arg]] <- NULL` DELETES the element, silently turning the
          # `null` scenario back into `default` -- and the null column is what
          # AC4's before/after comparison rests on. Single-bracket assignment
          # of `list(NULL)` stores a NULL element instead.
          base[arg] <- list(eval(codec_guard_scenarios[[sc]]))
        }
        # The `col = present` half: give `jobs` a column of the same name, which
        # `pick()` prefers over the scalar argument. This is the path where a bad
        # scalar used to be ignored rather than refused (M41-D2).
        if (identical(cl, "present")) {
          base$jobs[[arg]] <- codec_guard_col_value(arg)
        }

        obs <- tryCatch(
          {
            # Call by NAME, not by function object: `do.call(f, ...)` records
            # `(function(infile, ...) ...)(...)` as the condition call, which
            # hides the very blame target AC2 constrains.
            out <- do.call(verb, base, envir = env)
            # Scrub the input path AND the session tempdir: `tempdir()` carries a
            # per-session random suffix, so leaving it in would make two runs
            # differ on every row for no reason that concerns this milestone.
            txt <- as.character(out)
            txt <- gsub(sample, "<in>", txt, fixed = TRUE)
            txt <- gsub(tempdir(), "<tmp>", txt, fixed = TRUE)
            txt <- gsub(normalizePath(tempdir(), winslash = "/"), "<tmp>", txt,
                        fixed = TRUE)
            list(kind = "compiled",
                 outcome = paste(txt, collapse = " ||| "),
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
                   paste(deparse(cl)[[1]], collapse = ""),
                 in_index = grepl("In index:", msg, fixed = TRUE))
          }
        )

        rows[[length(rows) + 1]] <- data.frame(
          verb = verb, arg = arg, scenario = sc, col = cl, kind = obs$kind,
          outcome = obs$outcome, call = obs$call, in_index = obs$in_index,
          stringsAsFactors = FALSE)
       }
      }
    }
  }

  out <- do.call(rbind, rows)
  attr(out, "ref") <- if (is.null(ref)) "<working tree>" else ref
  out
}

# -- comparing two baselines -------------------------------------------------

# Rows whose kind, outcome, call or In-index status differs between two
# baselines. AC4 asks that `scenario %in% c("default", "null")` never appear
# here at either `col` setting; AC2/AC3 expect the `na`/`number`/`vec2` rows to
# appear, with the `call` column moving to the Layer-2 verb and `in_index` moving
# to FALSE.
#
# `col` is part of the row key below: without it the two halves of a batch pair
# collapse onto one key and match() pairs `absent` against `present`.
codec_guard_diff <- function(before, after) {
  key <- function(d) paste(d$verb, d$arg, d$scenario, d$col, sep = "")
  b <- before[match(key(after), key(before)), , drop = FALSE]
  changed <- !identical(nrow(b), 0L) & (
    b$kind != after$kind |
      xor(is.na(b$outcome), is.na(after$outcome)) |
      (!is.na(b$outcome) & !is.na(after$outcome) & b$outcome != after$outcome) |
      xor(is.na(b$call), is.na(after$call)) |
      (!is.na(b$call) & !is.na(after$call) & b$call != after$call) |
      (!is.na(b$in_index) & !is.na(after$in_index) &
         b$in_index != after$in_index)
  )
  changed[is.na(changed)] <- TRUE
  data.frame(
    verb = after$verb[changed], arg = after$arg[changed],
    scenario = after$scenario[changed], col = after$col[changed],
    before_kind = b$kind[changed], after_kind = after$kind[changed],
    before_call = b$call[changed], after_call = after$call[changed],
    before_in_index = b$in_index[changed],
    after_in_index = after$in_index[changed],
    before_outcome = b$outcome[changed], after_outcome = after$outcome[changed],
    stringsAsFactors = FALSE
  )
}

# -- a compact report --------------------------------------------------------

# Print one line per verb/argument/col cell summarizing AC2/AC3 compliance: which
# function the non-string aborts blame, and whether any carries `In index:`. A
# `kinds=compiled` cell is a pair that did NOT refuse the bad value.
codec_guard_report <- function(baseline) {
  bad <- baseline[baseline$scenario %in% c("na", "number", "vec2"), ]
  pairs <- unique(bad[c("verb", "arg", "col")])
  for (i in seq_len(nrow(pairs))) {
    v <- pairs$verb[[i]]
    a <- pairs$arg[[i]]
    cl <- pairs$col[[i]]
    sub <- bad[bad$verb == v & bad$arg == a & bad$col == cl, ]
    calls <- unique(stats::na.omit(sub$call))
    kinds <- unique(sub$kind)
    cat(sprintf("%-28s %-12s col=%-8s kinds=%-20s in_index=%-5s call=%s\n",
                v, a, cl, paste(kinds, collapse = ","),
                any(sub$in_index, na.rm = TRUE),
                paste(calls, collapse = " | ")))
  }
  invisible(baseline)
}
