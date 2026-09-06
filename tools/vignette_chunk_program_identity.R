#!/usr/bin/env Rscript
# Reports, for every chunk of every vignette under `vignettes/` and of
# `README.Rmd` that starts one of the three media programs, WHICH programs it
# started and whether the chunk's `eval` guard names each of them. Exits 1 if
# any chunk starts a program its guard does not cover.
#
# The README joined the domain in M115, for the reason its sibling script's
# header gives.
#
# Run from the package root, on a machine that HAS ffmpeg, ffprobe and
# mediainfo: Rscript tools/vignette_chunk_program_identity.R
# Requires `pkgload` and `knitr`. A developer tool, kept out of the build by
# `.Rbuildignore`'s `^tools$`.
#
# Why this exists beside `vignette_chunk_guards.R`. That script answers "is this
# chunk guarded at all", by knitting a second pass on a PATH that reaches none
# of the three; because it removes all three together, a chunk that starts
# FFprobe under an FFmpeg-only guard reads as "guarded" there. This script
# answers the other half -- guarded on the RIGHT program -- and it is the half
# that catches a verb whose incidental probe reaches a second binary. Both
# `extract_audio()` chunks in the vignettes were in exactly that position: the
# dropped-track check starts FFprobe, so an `ffmpeg`-only guard left them
# evaluating on a machine that has FFmpeg but not FFprobe.
#
# Method: knit each vignette once on this machine's own PATH with `system()` and
# `system2()` traced, attributing each spawn to the chunk that was running, and
# read each chunk's raw `eval` text from the file. A guard "covers" a program
# when the guard expression names it -- through the vignettes' `has_ffmpeg` /
# `has_ffprobe` / `has_both` / `has_mediainfo` flags or an inline
# `Sys.which("prog")`. Naming, not evaluating: the guard is the expression, and
# on this machine every flag is TRUE.

suppressPackageStartupMessages({
  library(knitr)
})

options(width = 200)

PROGRAMS <- c("ffmpeg", "ffprobe", "mediainfo")

# Per-chunk spawn record, in an environment the tracer planted on
# `system`/`system2` can reach from the traced function's own frame.
STATE <- new.env(parent = emptyenv())
STATE$current <- NA_character_
STATE$hits <- list()
STATE$guards <- list()

# Read the program out of a command line the same way `vignette_chunk_guards.R`
# does -- leading token, then each longer leading run of tokens that names an
# existing file (an unquoted path holding a space), then a quoted leading path.
# Matching on the whole candidate's basename keeps `Sys.which()`'s own shell-out
# ("which ffprobe") from counting.
program_started <- function(command) {
  if (length(command) != 1L) return(character(0))
  line <- trimws(as.character(command[[1L]]))
  tokens <- strsplit(line, "\\s+")[[1L]]
  cands <- tokens[1L]
  if (length(tokens) > 1L) {
    longer <- vapply(
      2:length(tokens),
      function(i) paste(tokens[seq_len(i)], collapse = " "),
      character(1)
    )
    cands <- c(cands, longer[file.exists(longer)])
  }
  if (grepl('^["\']', line)) {
    cands <- c(cands, sub('^(["\'])([^"\']*)\\1.*$', "\\2", line))
  }
  cands <- sub('^(["\'])(.*)\\1$', "\\2", cands)
  name <- tolower(sub("[.]exe$", "", basename(cands)))
  name[name %in% PROGRAMS]
}

record_spawn <- function(command) {
  name <- program_started(command)
  if (length(name) && !is.na(STATE$current)) {
    STATE$hits[[STATE$current]] <- union(STATE$hits[[STATE$current]], name[1L])
  }
  invisible(NULL)
}

# Raw `eval` option text, per chunk, in file order. knitr hands hooks the
# EVALUATED value, which cannot tell `eval = TRUE` from `eval = has_ffmpeg` on a
# machine that has ffmpeg -- and the guard is the expression, not its value.
raw_eval_options <- function(file) {
  lines <- readLines(file, warn = FALSE)
  headers <- grep("^[`]{3}[{]r\\b", lines, value = TRUE)
  vapply(headers, function(h) {
    opts <- sub("^[`]{3}[{]r\\s*,?\\s*", "", sub("[}]\\s*$", "", h))
    m <- regmatches(opts, regexpr("eval\\s*=\\s*[^,]+", opts))
    if (length(m)) trimws(sub("^eval\\s*=\\s*", "", m)) else "-"
  }, character(1), USE.NAMES = FALSE)
}

# The programs a guard expression NAMES, after expanding the vignettes' flags.
covered_programs <- function(guard) {
  expanded <- guard
  expanded <- gsub("has_both", "ffmpeg ffprobe", expanded, fixed = TRUE)
  expanded <- gsub("has_ffmpeg", "ffmpeg", expanded, fixed = TRUE)
  expanded <- gsub("has_ffprobe", "ffprobe", expanded, fixed = TRUE)
  expanded <- gsub("has_mediainfo", "mediainfo", expanded, fixed = TRUE)
  PROGRAMS[vapply(PROGRAMS, function(p) grepl(p, expanded, fixed = TRUE), logical(1))]
}

swept_files <- function() {
  f <- sort(list.files("vignettes", pattern = "[.]Rmd$", full.names = TRUE))
  if (length(f) == 0) {
    stop("no .Rmd files found under vignettes/ -- this sweep has nothing to read")
  }
  if (!file.exists("README.Rmd")) {
    stop("README.Rmd is not here -- run the sweep from the package root")
  }
  normalizePath(c(f, "README.Rmd"))
}

pkgload::load_all(".", quiet = TRUE, export_all = FALSE)

suppressMessages({
  trace(base::system, tracer = quote(record_spawn(command)), print = FALSE)
  trace(base::system2, tracer = quote(record_spawn(command)), print = FALSE)
})

files <- swept_files()
owd <- getwd()

for (f in files) {
  guards <- raw_eval_options(f)
  seen <- 0L
  # An opts hook runs once per chunk, before it is evaluated: the point where
  # the chunk's identity is known and its spawns have not happened yet.
  opts_hooks$set(tm_identity = function(options) {
    seen <<- seen + 1L
    key <- paste(basename(f), options$label)
    STATE$current <- key
    STATE$guards[[key]] <- if (seen <= length(guards)) guards[[seen]] else "?"
    options
  })
  opts_chunk$set(tm_identity = TRUE)
  opts_knit$set(root.dir = tempdir())
  knit(f, output = tempfile(fileext = ".md"), quiet = TRUE)
  setwd(owd)
  if (seen != length(guards)) {
    stop("chunk/header count mismatch in ", basename(f), ": knitr ran ", seen,
         " chunks against ", length(guards), " parsed headers")
  }
  opts_hooks$set(tm_identity = NULL)
}

rows <- lapply(names(STATE$hits), function(key) {
  started <- sort(STATE$hits[[key]])
  guard <- STATE$guards[[key]]
  if (is.null(guard)) guard <- "?"
  missed <- setdiff(started, covered_programs(guard))
  data.frame(
    chunk = key,
    guard = guard,
    started = paste(started, collapse = ","),
    uncovered = if (length(missed)) paste(missed, collapse = ",") else "",
    stringsAsFactors = FALSE
  )
})
report <- do.call(rbind, rows)
report <- report[order(report$chunk), , drop = FALSE]

cat("files swept:", length(files), "\n")
cat("chunks that started a program:", nrow(report), "\n\n")
print(report, row.names = FALSE)

bad <- report[nzchar(report$uncovered), , drop = FALSE]
cat("\nchunks starting a program their guard does not name: ")
if (nrow(bad) == 0) {
  cat("none\n")
} else {
  cat("\n")
  print(bad, row.names = FALSE)
  quit(status = 1L)
}
