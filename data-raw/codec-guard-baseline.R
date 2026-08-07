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
#   literal  a plain codec the verb accepts     -> compiled command (M56)
#   copy     the argument passed "copy"         -> compiled command (M56)
#   na       the argument passed NA             -> an abort (AC1/AC2)
#   number   the argument passed 1              -> an abort (AC2)
#   vec2     the argument passed c("aac","mp3") -> an abort (AC2)
#   token    the argument passed "aac -evil"    -> an abort (M56 AC2)
#
# The `literal` and `copy` scenarios are M56's: its AC4 asks that the compiled
# command be byte-identical across each verb's LEGAL codec values, not only at
# the default and at NULL, so a front-door guard that narrowed what a verb
# accepts would show up here rather than in the abort columns. `copy` is probed
# only where the verb accepts it -- the loudness verbs refuse it outright
# (check_audio_codec_not_copy()), and a cell that aborts on both refs compares
# equal while measuring nothing.
#
# Each scenario is probed twice on a `_batch` verb, once per value of a second
# dimension -- whether `jobs` carries a column of the same name as the argument
# under test:
#
#   col = absent   the jobs table has no such column
#   col = present  the jobs table carries a valid codec in that column
#   col = na       the jobs table carries NA in that column
#
# The `present` half exists because `pick()` prefers the column over the scalar
# argument, so the scalar is never read there: a bad value in it used to be
# ignored outright rather than refused. A grid probing only `absent` is
# structurally blind to that path, which is how the first pass at this script
# measured AC4 as clean while the contract had in fact moved on four verbs
# (review F2/F7 -> M41-D2). Scalar verbs have no jobs table and are recorded as
# `absent`.
#
# The `na` half was added by M42 (T1), which asks what a column NA *means* on
# each codec column rather than whether a bad scalar is refused. Every other
# codec column spells "unset" as NA via `check_batch_codec_col()` +
# `batch_codec_cell()`; the point of probing it is that two columns do not
# (`standardize_video_batch`'s `video_codec`, guarded inline against NA) and one
# resolves NA to something other than "unset" (`convert_audio`'s `-q:a 0`,
# D021). Read this half at `scenario = "default"`, where the scalar argument is
# absent and the column is the only thing speaking.
#
# A third dimension covers the three non-string scenarios on a `_batch` verb:
#
#   jobs = valid    the call is wrong only about the codec argument
#   jobs = invalid  `jobs` is not a table either, so two things are wrong
#
# The `invalid` half records WHICH complaint a doubly-invalid call gets, which
# is a behaviour a guard's placement silently decides (review A6).
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
#   codec_guard_vacuous(before); codec_guard_vacuous(after)   # both empty
#   codec_guard_diff(before, after)
#   codec_guard_semantics(after)                     # the M42 NULL/column-NA table
#
# `codec_guard_diff()` returns the rows whose outcome changed, and AC4 names the
# exact set it may contain. Run `codec_guard_vacuous()` on both sides first: it
# lists cells whose `default` call did not compile, and such a cell satisfies
# AC4's before/after comparison while measuring nothing.

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
    # `col_extra` on a SCALAR verb reads oddly until M56: the field is consumed
    # wherever a cell sets a codec, which since M56 includes the `literal` and
    # `copy` scenarios at `col = "absent"`. Without it these two fan-in verbs
    # record D017's "needs an audio stream to encode" at every legal audio
    # value, exactly as their `_batch` siblings did before review A2.
    compare_videos = list(
      args = c("video_codec", "audio_codec"),
      call = function(s, o) list(infiles = c(s, s), outfile = o),
      col_extra = list(audio_codec = list(audio = 0))),
    # col_extra: what else the call needs before a `col = present` run measures
    # anything. An audio_codec COLUMN on a fan-in verb whose `audio` is NULL is
    # refused by D017 ("needs an audio stream to encode") before the scalar
    # argument is reached, so the cell records that unrelated abort at every
    # scenario -- including `default`, which is what makes it vacuous rather
    # than merely noisy (review A2). Naming an input to carry audio puts the
    # cell back on the codec argument, as it is on every other verb.
    compare_videos_batch = list(
      args = c("video_codec", "audio_codec"),
      call = function(s, o) list(jobs = tibble::tibble(
        inputs = list(c(s, s)), output = o)),
      col_extra = list(audio_codec = list(audio = 0))),
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
      call = function(s, o) list(main = s, overlay = s, outfile = o),
      col_extra = list(audio_codec = list(audio = 0))),
    # Fixed two-input shape (D015): named main/overlay columns, NOT the
    # `inputs` list-column the other fan-in verb takes. With the wrong shape
    # every cell here aborted on "Missing columns", so the default/null cells
    # never compiled -- AC4's "no default/null row changed" was vacuous on this
    # verb -- and the `col = present` half merely duplicated `absent` (review
    # A1, the same structural blindness class as round 1's F2/F7).
    picture_in_picture_batch = list(
      args = c("video_codec", "audio_codec"),
      call = function(s, o) list(jobs = tibble::tibble(
        main = s, overlay = s, output = o)),
      col_extra = list(audio_codec = list(audio = 0))),
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
  # Another sentinel: the value depends on which argument is under test, so it
  # is resolved per cell rather than carried here (codec_guard_col_value()).
  literal = quote(LITERAL),
  copy    = quote("copy"),
  na      = quote(NA),
  number  = quote(1),
  vec2    = quote(c("aac", "mp3")),
  token   = quote("aac -evil")
)

# The scenarios whose value is a LEGAL codec setting: these are expected to
# compile, and M56's AC4 rests on their commands being byte-identical across two
# refs. The abort scenarios are the complement.
codec_guard_legal <- c("default", "null", "literal", "copy")

# Whether `verb` accepts "copy" in `arg` at all. Two documented refusals, both
# measured on master rather than assumed:
#
#   audio_codec  the loudness verbs refuse it (check_audio_codec_not_copy()):
#                re-encoding is the whole point of a loudness pass.
#   video_codec  every verb that applies a video filter or seeks accurately
#                refuses it ("Can't apply a video filter while the video codec
#                is set to \"copy\"", and segment_video's frame-accurate-seek
#                variant) -- which is all of them but separate_audio_video(),
#                whose video side is a pass-through remux.
#
# Probing a refusing cell would record a matching pair of aborts, which passes
# the before/after comparison while measuring nothing -- the vacuity trap
# codec_guard_vacuous() exists for.
codec_guard_copy_ok <- function(verb, arg) {
  if (arg == "audio_codec") return(!grepl("^normalize_audio", verb))
  grepl("^separate_audio_video", verb)
}

# The value the jobs column carries, per `col` setting. For `present` it must be
# a value the per-row column guards accept, so that the column genuinely wins the
# `pick()` and the scalar argument is the only thing under test: "copy" is
# refused outright by several verbs and NA is the column form of the NULL
# sentinel, so neither would isolate the scalar. For `na` the NA *is* the
# subject (M42 T1), and it is a logical NA rather than `NA_character_` because
# that is what a jobs table written by hand carries -- `tibble(video_codec = NA)`
# is logical, and `batch_codec_cell()`'s all-NA-logical acceptance (D016) exists
# for exactly that column.
codec_guard_col_value <- function(arg, col = "present") {
  if (identical(col, "na")) return(NA)
  if (arg == "video_codec") "libx264" else "aac"
}

# The cells a verb/scenario is probed in. Two dimensions, both meaningful only
# on a `_batch` verb, which is why a scalar verb gets the single default cell:
#
#   col   whether `jobs` carries a column of the same name as the argument, and
#         if so whether it carries a valid codec (`present`) or NA (`na`)
#   jobs  whether `jobs` itself is a valid table at all
#
# The `jobs = "invalid"` cell exists to pin PRECEDENCE: when a call is wrong
# about both the table and the codec argument, which complaint does it get? Two
# verbs' new guards changed that answer as a side effect (review A6), and no
# template here passes an invalid table, so the grid could not see it. It is
# probed only for the non-string scenarios, since `default`/`null` give the
# guard nothing to complain about and the answer would be the jobs error either
# way.
codec_guard_cells <- function(base, scenario) {
  cells <- list(list(col = "absent", jobs = "valid"))
  if ("jobs" %in% names(base)) {
    cells <- c(cells, list(list(col = "present", jobs = "valid")),
               list(list(col = "na", jobs = "valid")))
    if (!scenario %in% codec_guard_legal) {
      cells <- c(cells, list(list(col = "absent", jobs = "invalid")))
    }
  }
  cells
}

# -- running the grid --------------------------------------------------------

# Run every scenario for every verb/argument pair against `env`, returning a
# data frame of observations. `input` paths are scrubbed out of compiled
# commands so two machines' baselines compare equal.
codec_guard_baseline <- function(ref = NULL, root = ".", sample = NULL,
                                 nvenc = character()) {
  env <- codec_guard_env(ref, root)
  # Pin the nvenc encoder pool for the whole grid (M56 AC4). No cell sets
  # hardware = "nvenc" today, so this changes nothing measured -- it removes the
  # possibility that one ever does and makes the baseline depend on whether the
  # machine running it has an nvenc-capable FFmpeg. The default, an empty pool,
  # is "this build lists no nvenc encoder", which is deterministic everywhere.
  old_opt <- options(tidymedia.nvenc_encoders = nvenc)
  on.exit(options(old_opt), add = TRUE)
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
        if (sc == "copy" && !codec_guard_copy_ok(verb, arg)) next
        for (cell in codec_guard_cells(spec$call(sample, outfile), sc)) {
          rows[[length(rows) + 1]] <- data.frame(
            verb = verb, arg = arg, scenario = sc, col = cell$col,
            jobs = cell$jobs, kind = "absent",
            outcome = NA_character_, call = NA_character_,
            in_index = NA, stringsAsFactors = FALSE)
        }
      }
      next
    }
    if (!all(spec$args %in% names(formals(f)))) {
      for (arg in setdiff(spec$args, names(formals(f)))) {
        for (sc in names(codec_guard_scenarios)) {
          if (sc == "copy" && !codec_guard_copy_ok(verb, arg)) next
          for (cell in codec_guard_cells(spec$call(sample, outfile), sc)) {
            rows[[length(rows) + 1]] <- data.frame(
              verb = verb, arg = arg, scenario = sc, col = cell$col,
              jobs = cell$jobs, kind = "absent",
              outcome = NA_character_, call = NA_character_,
              in_index = NA, stringsAsFactors = FALSE)
          }
        }
      }
    }

    for (arg in intersect(spec$args, names(formals(f)))) {
      for (sc in names(codec_guard_scenarios)) {
       if (sc == "copy" && !codec_guard_copy_ok(verb, arg)) next
       for (cell in codec_guard_cells(spec$call(sample, outfile), sc)) {
        cl <- cell$col
        base <- spec$call(sample, outfile)
        base$run <- FALSE
        # `parallel = FALSE` is the default, but AC3 is about this exact path,
        # so the probe states it rather than inheriting it.
        if ("parallel" %in% names(formals(f))) base$parallel <- FALSE
        if (sc == "literal") {
          # Resolved here rather than in the scenario table: the value depends
          # on which argument is under test, and it is the same one the
          # `col = present` half uses, so the two halves cannot drift.
          base[arg] <- list(codec_guard_col_value(arg))
        } else if (sc != "default") {
          # `base[[arg]] <- NULL` DELETES the element, silently turning the
          # `null` scenario back into `default` -- and the null column is what
          # AC4's before/after comparison rests on. Single-bracket assignment
          # of `list(NULL)` stores a NULL element instead.
          base[arg] <- list(eval(codec_guard_scenarios[[sc]]))
        }
        # The `col = present` / `col = na` halves: give `jobs` a column of the
        # same name, which `pick()` prefers over the scalar argument. `present`
        # is the path where a bad scalar used to be ignored rather than refused
        # (M41-D2); `na` is what a column NA compiles to (M42 T1). `col_extra`
        # applies to both, since it exists to keep the cell on the codec
        # argument rather than on an unrelated abort, and an NA column trips the
        # same unrelated abort a valid one does.
        if (cl %in% c("present", "na")) {
          base$jobs[[arg]] <- codec_guard_col_value(arg, cl)
        }
        # `col_extra` keeps a cell ON the codec argument rather than on an
        # unrelated abort, so it is owed wherever the cell actually SETS a
        # codec: the two column halves, and (M56) the `literal`/`copy`
        # scenarios, which set the scalar argument at `col = "absent"` too.
        # Without it the fan-in verbs' audio_codec cells record D017's "needs an
        # audio stream to encode" at every legal value, which is the same
        # vacuity the column halves already guard against (review A2).
        if (cl %in% c("present", "na") || sc %in% c("literal", "copy")) {
          for (nm in names(spec$col_extra[[arg]])) {
            base[[nm]] <- spec$col_extra[[arg]][[nm]]
          }
        }
        # The precedence cell: `jobs` is not a table at all, so the call is
        # wrong about two things at once and the recorded outcome says which
        # one the verb reports first (review A6).
        if (identical(cell$jobs, "invalid")) {
          base$jobs <- "oops"
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
          verb = verb, arg = arg, scenario = sc, col = cl, jobs = cell$jobs,
          kind = obs$kind,
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
# `col` and `jobs` are part of the row key below: without them the cells of a
# batch pair collapse onto one key and match() pairs `absent` against `present`.
# The separator is a literal \037 (unit separator) so no combination of field
# values can spell another row's key.
#
# The two baselines are required to cover the SAME cells. Matching runs over
# `after`'s keys, so a row present only in `before` would be dropped silently --
# and AC4's claim is that the changed set is exactly an enumerated list, which a
# silently dropped row would falsify without appearing (review A17). Compare the
# key sets and refuse to report a diff over mismatched grids.
codec_guard_diff <- function(before, after) {
  key <- function(d) paste(d$verb, d$arg, d$scenario, d$col, d$jobs, sep ="")
  only_before <- setdiff(key(before), key(after))
  only_after <- setdiff(key(after), key(before))
  if (length(only_before) > 0 || length(only_after) > 0) {
    stop("the two baselines cover different cells; ",
         length(only_before), " only in `before`, ",
         length(only_after), " only in `after`. ",
         "Re-run both sides with the same version of this script.")
  }
  b <- before[match(key(after), key(before)), , drop = FALSE]
  changed <- (
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
    jobs = after$jobs[changed],
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
  bad <- baseline[baseline$scenario %in% c("na", "number", "vec2", "token") &
                    baseline$jobs == "valid", ]
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

# The anti-vacuity check. AC4's force comes from `default` and `null` rows
# COMPILING on both refs and being identical: a cell where the default call
# aborts contributes a matching pair of aborts and so passes the comparison
# while measuring nothing. That is not hypothetical -- picture_in_picture_batch
# sat in this grid with a `jobs` shape it does not accept, so its default and
# null cells aborted on the missing columns and its whole verb was dead weight
# in the evidence (review A1). Run this on any baseline before trusting a diff
# over it: every returned row is a cell whose default call did not compile, and
# a healthy grid returns none.
#
# `col = "na"` is deliberately excluded. This function answers "did the grid
# measure anything here", and it does so by treating a non-compiling default as
# a broken cell. On the `na` half a non-compiling default is the OPPOSITE of a
# broken cell -- it is the finding, a codec column that refuses to spell "unset"
# (M42). Folding the two together would report M42's subject matter as M41's
# instrumentation failure.
#
# `scenarios` defaults to the legal-value set less `null`, which is M56's
# widening: AC4's comparison now spans `literal` and `copy` too, and a cell that
# aborts there is as dead as one that aborts at the default. `null` stays out
# because a verb refusing NULL is a finding about the sentinel (D022), not a
# broken cell -- the same reason `col = "na"` is excluded below.
codec_guard_vacuous <- function(baseline,
                                scenarios = c("default", "literal", "copy")) {
  d <- baseline[baseline$scenario %in% scenarios & baseline$jobs == "valid" &
                  baseline$col %in% c("absent", "present"), ]
  d[d$kind != "compiled", c("verb", "arg", "scenario", "col", "kind", "outcome")]
}

# -- the M42 semantics table -------------------------------------------------

# Pull the codec flag a compiled command actually carries for `arg`, so a row of
# the semantics table says what the outcome MEANS rather than repeating a
# 300-character command. Returns the flag's value, or "-" when the command emits
# no such flag at all (D016's sentinel behaviour), or the several values joined
# by "/" when a fan-out verb compiled more than one command and they disagree.
codec_guard_flag <- function(command, arg) {
  flag <- if (arg == "video_codec") "-codec:v" else "-codec:a"
  hits <- regmatches(command, gregexpr(
    paste0(flag, "\\s+\\S+"), command, perl = TRUE))[[1]]
  if (length(hits) == 0) {
    # convert_audio's NULL means `-q:a 0`, not "emit nothing" (D021), and a row
    # reading "-" would hide the one departure the table exists to record.
    q <- regmatches(command, gregexpr("-q:a\\s+\\S+", command, perl = TRUE))[[1]]
    if (length(q) > 0) return(paste(unique(trimws(q)), collapse = "/"))
    return("-")
  }
  vals <- unique(sub(paste0("^", flag, "\\s+"), "", trimws(hits)))
  paste(vals, collapse = "/")
}

# One row per verb/argument pair: what `NULL` compiles to and what a column `NA`
# compiles to, as of `baseline`. This is M42 T1's deliverable -- the measured
# argument x {NULL, column NA} table the D-entry is chosen from, rather than a
# reading of the source.
#
# `null` is read at `col = "absent"` (the scalar argument is the only thing
# speaking) and `column_na` at `scenario = "default"`, `col = "na"` (the column
# is the only thing speaking). A scalar verb has no column, so its `column_na`
# reads "n/a"; the `_batch` row beside it is where the column lives.
codec_guard_semantics <- function(baseline) {
  cell <- function(verb, arg, scenario, col) {
    r <- baseline[baseline$verb == verb & baseline$arg == arg &
                    baseline$scenario == scenario & baseline$col == col &
                    baseline$jobs == "valid", ]
    if (nrow(r) == 0) return(NA_character_)
    if (r$kind[[1]] == "compiled") codec_guard_flag(r$outcome[[1]], arg)
    else paste0("ABORT: ", sub("\n.*$", "", r$outcome[[1]]))
  }
  pairs <- unique(baseline[c("verb", "arg")])
  pairs <- pairs[order(pairs$arg, pairs$verb), , drop = FALSE]
  data.frame(
    verb = pairs$verb,
    arg = pairs$arg,
    default = vapply(seq_len(nrow(pairs)), function(i)
      cell(pairs$verb[[i]], pairs$arg[[i]], "default", "absent"), ""),
    null = vapply(seq_len(nrow(pairs)), function(i)
      cell(pairs$verb[[i]], pairs$arg[[i]], "null", "absent"), ""),
    column_na = vapply(seq_len(nrow(pairs)), function(i) {
      v <- cell(pairs$verb[[i]], pairs$arg[[i]], "default", "na")
      if (is.na(v)) "n/a (scalar verb)" else v
    }, ""),
    stringsAsFactors = FALSE, row.names = NULL
  )
}
