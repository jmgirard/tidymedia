# nvenc-probe-order-baseline.R -----------------------------------------------
#
# Regenerate M095's AC2 baseline from an arbitrary git ref, so "the reorder
# refuses no call it compiled and compiles no call it refused" is re-derivable
# evidence rather than an implementation-time transcript (M095 T2, AC2). Same
# shape and the same ref machinery as `data-raw/contradiction-guard-baseline.R`,
# which sources `data-raw/codec-guard-baseline.R` for it; this file sources that
# one too rather than copying it, for the reason its own comments give -- two
# implementations of `git show`-into-an-environment is how the two drift apart.
#
# M095 moves the nvenc encoder resolution BELOW the machine-independent argument
# checks in three pipelines, so a caller told nothing but "nvenc is not
# available" now hears about the argument they actually got wrong. The whole
# claim of AC2 is that this moves BLAME and nothing else: every call that
# compiled a command still compiles the same bytes, and every call that was
# refused is still refused.
#
# The MEMBERS are derived, never listed: every exported function whose body
# mentions one of the three reordered pipelines. A verb that starts calling one
# joins this grid on its own, which is the property M70's computed domain has and
# a hand-list does not.
#
# Each member is probed at:
#
#   valid    its own valid argument cell
#   <arg>/<form>   that cell with one formal replaced by one of the five wrong
#                  forms `tm_nvenc_wrong_forms()` holds (a number, a
#                  token-invalid string, NA, a length-2 vector, a list)
#
# crossed with four dimensions the reorder could plausibly disturb:
#
#   hardware     "none" / "nvenc"    -- whether the probe runs at all
#   fallback     FALSE / TRUE        -- which branch of the resolver runs
#   pool         present / absent    -- what the mocked build answers
#   video_codec  caller / sentinel   -- a codec the caller named, or M34/D016's
#                                       NULL "leave the codec alone" sentinel
#
# `video_codec` is crossed rather than pinned to one re-encoding token (M106).
# Pinning left the sentinel arm of every cell unprobed, which is the arm where
# `resolve_hw_encoder()` takes its own branch: it assumes the h264 family rather
# than inferring one, and under `fallback = TRUE` it returns the sentinel rather
# than a software codec. A member with no `video_codec` formal
# (`format_for_web()`, `format_for_web_batch()`) has nothing to cross and
# records `absent`.
#
# The wrong-form cells are a SUPERSET of AC1's kept cells: AC1 drops a cell whose
# reference refusal comes from a frame below the member, and AC2 has no reason to
# drop it -- a call refused by `ffm_finish()` must still be refused by
# `ffm_finish()` afterwards, and including it can only strengthen the claim.
#
# `cached_encoder_names()` is replaced in the ref's own environment rather than
# the encoder-pool option (`tidymedia.hardware_encoders`; `tidymedia.nvenc_encoders` before M099) being set, so the grid exercises the
# fall-through branch AC1 exercises -- the option seam returns above it and would
# leave the memo path unmeasured. `run_program()` is replaced too: a wrong value
# that happens to be TRUTHY (`run = 123`) would otherwise really execute FFmpeg
# on the sample, making the cell's outcome the runner's exit status.
# `tidymedia.check_tracks = FALSE` removes the one other build-time probe, so no
# cell depends on a binary at all.
#
# Every probe runs at `run = FALSE` (except where the cell's own wrong value is
# `run`), so nothing is written to disk.
#
# Usage (from the package root):
#
#   source("data-raw/nvenc-probe-order-baseline.R")
#   before <- nvenc_order_baseline("<merge-base sha>")
#   after  <- nvenc_order_baseline()          # the working tree
#   nvenc_order_vacuous(before)               # empty: every `valid` cell compiled
#   nvenc_order_vacuous(after)
#   nvenc_order_contract_diff(before, after)  # AC2: must be empty
#   nvenc_order_diff(before, after)           # the widest view: the 27 message
#                                             #   moves this milestone makes

source(file.path("data-raw", "codec-guard-baseline.R"))
# For `tm_nvenc_wrong_forms()` and `tm_timeout_call_specs()` alone, so the five
# wrong forms and the per-member valid cell have ONE definition shared with the
# AC1 sweep. Sourcing the file defines functions and evaluates nothing else.
source(file.path("tests", "testthat", "helper-timeout-sweep.R"))

# -- the members -------------------------------------------------------------

# The three pipelines M095 reorders. Named here because they are the SUBJECT of
# the milestone, not a stand-in for a set that could grow.
nvenc_order_pipelines <- c("standardize_pipeline", "format_for_web_pipeline",
                           "anonymize_pipeline")

# Every exported function of `ref` whose body mentions one of them. `all.names()`
# on the body rather than a text grep of the sources: a name in a comment or in
# another function's documentation is not a call, and the ref env is the same
# object the grid is then run against.
nvenc_order_members <- function(env, exports) {
  hit <- vapply(exports, function(nm) {
    f <- get0(nm, envir = env, inherits = FALSE)
    is.function(f) && any(nvenc_order_pipelines %in% all.names(body(f)))
  }, logical(1))
  tm_sort_c(exports[hit])
}

# The ref's export list, read from its own NAMESPACE so a ref predating an export
# is not probed on it.
nvenc_order_exports <- function(ref = NULL, root = ".") {
  ns <- if (is.null(ref)) {
    readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  } else {
    text <- system2("git", c("-C", shQuote(root), "show",
                             shQuote(paste0(ref, ":NAMESPACE"))),
                    stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(text, "status"))) {
      stop("git show failed for ", ref, ":NAMESPACE")
    }
    text
  }
  trimws(sub("^export\\(([^)]*)\\).*$", "\\1", grep("^export\\(", ns,
                                                    value = TRUE)))
}

# -- the probe grid ----------------------------------------------------------

# One valid argument cell per member, from the AC1 sweep's own table, with the
# real sample copied in so a compiled command is a command over real media.
nvenc_order_specs <- function(dir, sample) {
  for (f in c("in.mp4", "in2.mp4")) {
    file.copy(sample, file.path(dir, f), overwrite = TRUE)
  }
  tm_timeout_call_specs(dir)
}

# Every (member, cell) pair: the valid cell plus one per (other formal, wrong
# form). `hardware` is excluded because it is a crossed dimension below, `...`
# because it is not nameable, and `fallback` and `video_codec` because they are
# crossed too -- a cell that overwrote one would silently leave one arm of that
# cross unprobed.
#
# Each spec records whether its member carries a `video_codec` formal, which is
# what the runner crosses on: the value itself is set there, per level, never
# here.
nvenc_order_cells <- function(env, members, specs) {
  forms <- tm_nvenc_wrong_forms()
  out <- list()
  for (nm in members) {
    fmls <- names(formals(get(nm, envir = env, inherits = FALSE)))
    base <- specs[[nm]]
    if (is.null(base)) stop("no valid argument cell recorded for ", nm)
    has_vc <- "video_codec" %in% fmls
    if ("parallel" %in% fmls) base$parallel <- FALSE
    if ("run" %in% fmls) base$run <- FALSE
    out[[paste0(nm, " || valid")]] <- list(name = nm, cell = "valid",
                                           args = base, has_vc = has_vc)
    for (arg in setdiff(fmls, c("hardware", "fallback", "video_codec", "..."))) {
      for (form in names(forms)) {
        args <- base
        args[[arg]] <- forms[[form]]
        out[[paste0(nm, " || ", arg, "/", form)]] <-
          list(name = nm, cell = paste0(arg, "/", form), args = args,
               has_vc = has_vc)
      }
    }
  }
  out
}

nvenc_order_video_codecs <- list(
  # A `"copy"` default under `hardware = "nvenc"` is a contradiction the verb
  # refuses on its own (D036), which would leave every cell of that member
  # measuring the contradiction rather than the reorder -- so the caller arm
  # names a re-encoding token rather than leaving the member's own default.
  caller = "libx264",
  # M34/D016's "leave the codec alone" sentinel.
  sentinel = NULL
)

# Set one crossed `video_codec` level on one cell's argument list.
#
# `args["video_codec"] <- list(NULL)`, never `args$video_codec <- NULL`: the
# second DELETES the element, which would hand the member its own default and
# make the sentinel arm measure something else entirely.
nvenc_order_set_codec <- function(args, level) {
  args["video_codec"] <- nvenc_order_video_codecs[level]
  args
}

# Derived from `hardware_backend_families()` through the shared helper
# (`tm_nvenc_encoder_pools()`, tests/testthat/helper-timeout-sweep.R, sourced
# above), never spelled out here: one definition of "a build with the nvenc
# encoders" for this grid, the AC1 sweep and the seam test alike (M107). Two
# levels, matching what the grid crosses; the `hw` loop below reaches
# videotoolbox against the nvenc pool on purpose, which is the arm where the
# availability abort is what a caller would get if the check under test did not
# fire first.
nvenc_order_pools <- tm_nvenc_encoder_pools()

# -- running the grid --------------------------------------------------------

# Run every cell against `ref`, returning one row per (cell, hardware, fallback,
# pool, video_codec). A compiled command records its bytes with paths scrubbed;
# a refusal records the blamed frame and the message.
nvenc_order_baseline <- function(ref = NULL, root = ".", sample = NULL) {
  env <- codec_guard_env(ref, root)
  members <- nvenc_order_members(env, nvenc_order_exports(ref, root))
  if (length(members) == 0) stop("ref ", ref, " has no member reaching a ",
                                 "reordered pipeline")

  # The nvenc option seam stays UNSET so `nvenc_available()` falls through to
  # `cached_encoder_names()`, which is what the pool below replaces.
  old <- options(tidymedia.nvenc_encoders = NULL, tidymedia.hardware_encoders = NULL,
                 tidymedia.check_tracks = FALSE)
  on.exit(options(old), add = TRUE)

  if (is.null(sample)) sample <- normalizePath(
    file.path(root, "inst", "extdata", "sample.mp4"), mustWork = TRUE)
  dir <- file.path(tempfile("nvenc-order-"))
  dir.create(dir, recursive = TRUE)
  specs <- nvenc_order_specs(dir, sample)
  cells <- nvenc_order_cells(env, members, specs)

  assign("run_program",
         function(location, args, program = "the program", ...) character(0),
         envir = env)

  rows <- list()
  for (pool in names(nvenc_order_pools)) {
    assign("cached_encoder_names",
           local({
             p <- nvenc_order_pools[[pool]]
             function() p
           }),
           envir = env)
    for (key in names(cells)) {
      spec <- cells[[key]]
      # A member with no `video_codec` formal has nothing to cross: one level,
      # recorded as `absent` so its rows are still keyed on the column.
      levels <- if (spec$has_vc) names(nvenc_order_video_codecs) else "absent"
      for (hw in c("none", "nvenc")) {
        for (fb in c(FALSE, TRUE)) {
          for (vc in levels) {
            args <- spec$args
            if (spec$has_vc) args <- nvenc_order_set_codec(args, vc)
            args$hardware <- hw
            args$fallback <- fb
            obs <- tryCatch(
              withCallingHandlers(
                {
                  # By NAME, never by function object: `do.call(f, ...)` records
                  # the anonymous object as the condition call and hides the blame
                  # target this grid exists to watch.
                  out <- do.call(spec$name, args, envir = env)
                  txt <- gsub(dir, "<dir>", as.character(out), fixed = TRUE)
                  txt <- gsub(tempdir(), "<tmp>", txt, fixed = TRUE)
                  list(kind = "compiled",
                       outcome = paste(txt, collapse = " ||| "),
                       call = NA_character_)
                },
                # The `fallback = TRUE` arm emits a message rather than aborting;
                # muffling it keeps the cell's outcome the compiled command.
                message = function(m) invokeRestart("muffleMessage")
              ),
              error = function(e) {
                cl <- conditionCall(e)
                list(kind = "abort",
                     outcome = paste(cli::ansi_strip(conditionMessage(e)),
                                     collapse = "\n"),
                     call = if (is.null(cl)) NA_character_ else
                       paste(deparse(cl[[1]]), collapse = ""))
              }
            )
            rows[[length(rows) + 1]] <- data.frame(
              member = spec$name, cell = spec$cell, hardware = hw,
              fallback = fb, pool = pool, video_codec = vc, kind = obs$kind,
              outcome = obs$outcome, call = obs$call,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }

  out <- do.call(rbind, rows)
  attr(out, "ref") <- if (is.null(ref)) "<working tree>" else ref
  attr(out, "members") <- members
  out
}

# -- comparing two baselines -------------------------------------------------

# The `valid` cells that did NOT compile where a compile was owed. Such a cell
# satisfies AC2's before/after comparison while measuring nothing about a
# compiled command, so both sides are checked for emptiness before a diff is
# believed.
#
# One arm is carved out and it is not a fudge: `hardware = "nvenc"` with
# `fallback = FALSE` against a build listing no nvenc encoder is the availability
# abort doing its job, on a call whose arguments are all valid. Every other arm
# of the cross owes a compiled command.
nvenc_order_vacuous <- function(baseline) {
  unavailable <- baseline$hardware == "nvenc" & !baseline$fallback &
    baseline$pool == "absent"
  baseline[baseline$cell == "valid" & !unavailable &
             baseline$kind != "compiled", , drop = FALSE]
}

# Rows whose kind, compiled command, blamed frame or message differs -- the
# widest comparison, reported for the reader rather than for AC2.
#
# On M095, the milestone this file was written for, it was NOT empty and had to
# not be: the 27 rows it returned were the defect being fixed, each an abort
# whose message changed from "nvenc is not available" to the caller's own
# argument error, with the blamed frame unmoved. That it saw them is also this
# grid's discrimination check -- an instrument that reported nothing there would
# be reporting nothing anywhere. A later milestone's own figure is recorded in
# that milestone's file, never here. The figure is also not re-derivable by this
# script as it now stands: M106 both crossed `video_codec` and dropped that
# argument's wrong-form cells, so the grid the 27 was measured over no longer
# exists here.
#
# The two baselines must cover the same cells. Matching runs over `after`'s keys,
# so a row present only in `before` would vanish silently -- and "empty" is the
# whole claim of the narrower comparison below, which a silently dropped row
# would satisfy without appearing.
nvenc_order_diff <- function(before, after) {
  b <- nvenc_order_align(before, after)
  ne <- function(x, y) xor(is.na(x), is.na(y)) | (!is.na(x) & !is.na(y) & x != y)
  changed <- b$kind != after$kind | ne(b$outcome, after$outcome) |
    ne(b$call, after$call)
  nvenc_order_report(b, after, changed)
}

# AC2's comparison, and only AC2's: the two conditions the criterion states.
#
#   1  every cell that compiled at the merge-base compiles the same bytes now
#   2  the refused set is the same at both refs
#
# A message change on a cell refused at both refs is neither of those -- it is
# AC1's subject, and `nvenc_order_diff()` above is where it is read. This
# function must return zero rows.
nvenc_order_contract_diff <- function(before, after) {
  b <- nvenc_order_align(before, after)
  ne <- function(x, y) xor(is.na(x), is.na(y)) | (!is.na(x) & !is.na(y) & x != y)
  both_compiled <- b$kind == "compiled" & after$kind == "compiled"
  changed <- b$kind != after$kind | (both_compiled & ne(b$outcome, after$outcome))
  nvenc_order_report(b, after, changed)
}

# Line `before` up with `after`, refusing a comparison over mismatched grids.
nvenc_order_align <- function(before, after) {
  key <- function(d) {
    paste(d$member, d$cell, d$hardware, d$fallback, d$pool, d$video_codec,
          sep = "\037")
  }
  only_before <- setdiff(key(before), key(after))
  only_after <- setdiff(key(after), key(before))
  if (length(only_before) > 0 || length(only_after) > 0) {
    stop("the two baselines cover different cells; ",
         length(only_before), " only in `before`, ",
         length(only_after), " only in `after`.")
  }
  before[match(key(after), key(before)), , drop = FALSE]
}

nvenc_order_report <- function(b, after, changed) {
  changed[is.na(changed)] <- TRUE
  data.frame(
    member = after$member[changed], cell = after$cell[changed],
    hardware = after$hardware[changed], fallback = after$fallback[changed],
    pool = after$pool[changed], video_codec = after$video_codec[changed],
    before_kind = b$kind[changed], after_kind = after$kind[changed],
    before_call = b$call[changed], after_call = after$call[changed],
    before_outcome = b$outcome[changed], after_outcome = after$outcome[changed],
    stringsAsFactors = FALSE
  )
}
