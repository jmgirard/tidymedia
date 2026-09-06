#!/usr/bin/env Rscript
# Reports, for every chunk of every vignette under `vignettes/`, whether that
# chunk starts an external program and whether it is guarded on that program's
# presence. Exits 1 if any chunk starts a program and would still be evaluated
# on a machine that does not have it.
#
# Run from the package root, on a machine that HAS ffmpeg, ffprobe and
# mediainfo: Rscript tools/vignette_chunk_guards.R
# Requires `pkgload` and `knitr`. A developer tool, kept out of the build by
# `.Rbuildignore`'s `^tools$`.
#
# Why it MEASURES rather than reading a list of function names. Whether a chunk
# starts a program is a property of the CALL, not of the function: every task
# verb can start FFmpeg, and every one of them called with `run = FALSE`
# compiles a command and starts nothing. A name-based rule reads the dozen
# `run = FALSE` chunks in the existing vignettes as spawning and demands guards
# they do not need; and it would still miss a spawn reached by a route no name
# in the list describes. So the sweep knits each vignette twice and watches what
# actually happens:
#
#   pass 1, on this machine's own PATH -- count the calls each chunk makes to
#           R's two process-starting functions, `system()` and `system2()`;
#   pass 2, in a child process whose PATH reaches none of the three programs --
#           record, per chunk, whether knitr still evaluated it.
#
# A chunk that started a program in pass 1 and is still evaluated in pass 2 is
# UNGUARDED: on a machine without the binaries it would run anyway. Everything
# else is reported with the guard expression it carries, so the listing covers
# every chunk in every vignette and not only the ones a reader expected.
#
# The `system`/`system2` seeds are the same two primitives the timeout sweep
# grows its domain from (tests/testthat/helper-timeout-sweep.R); a spawn made by
# any other route is outside what this measures, as it is there.

suppressPackageStartupMessages({
  library(knitr)
})

options(width = 200)

PROGRAMS <- c("ffmpeg", "ffprobe", "mediainfo")

# Per-chunk spawn tally, in the global environment because that is where the
# tracer planted on `system`/`system2` can see it: a tracer expression is
# evaluated in the traced function's own frame, whose enclosure reaches the base
# namespace and then here.
SPAWNS <- new.env(parent = emptyenv())
SPAWNS$n <- 0L

# Count a process start only when the program being started is one of the three
# media programs. R's own `Sys.which()` shells out (`system("which ffprobe")`),
# and a vignette's guard expression calls it -- counting that would report every
# guarded setup chunk as a spawning chunk, which is the opposite of the truth.
# `system2()` names the program in `command`; `system()` takes a whole command
# line, whose program is its leading token or tokens.
#
# The program is read from the line's leading token, and -- because `R/ffmpeg.R`
# passes the resolved path unquoted, so a binary under a directory whose name
# holds a space ("/Volumes/My Tools/bin/ffmpeg -i ...") arrives split across
# tokens -- from each longer leading run of tokens as well. A multi-token
# candidate counts only when it NAMES AN EXISTING FILE: without that test any
# line whose last token happens to be called ffmpeg matches, and "cp a.mp4
# /tmp/ffmpeg" is reported as a spawn. The sweep runs on a machine that has all
# three programs, so the real spawn's path does exist and the test costs nothing.
# Matching on the whole candidate's BASENAME is what keeps `Sys.which()` out:
# "which ffprobe" holds no "/", so its basename is that whole two-word string.
spawn_record <- function(command) {
  prog <- if (length(command) == 1L) command[[1L]] else return(invisible(NULL))
  line <- trimws(as.character(prog))
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
  # A path the caller DID quote is delimited, so it is a candidate whatever
  # spaces it holds and needs no existence test. Both quote styles: `shQuote()`
  # uses single quotes on unix and double on Windows.
  if (grepl('^["\']', line)) {
    cands <- c(cands, sub('^(["\'])([^"\']*)\\1.*$', "\\2", line))
  }
  # Strip only a wrapping pair, so an argument that merely contains a quote
  # cannot turn a non-match into a match.
  cands <- sub('^(["\'])(.*)\\1$', "\\2", cands)
  names <- tolower(sub("[.]exe$", "", basename(cands)))
  if (any(names %in% PROGRAMS)) SPAWNS$n <- SPAWNS$n + 1L
  invisible(NULL)
}

vignette_files <- function() {
  f <- sort(list.files("vignettes", pattern = "[.]Rmd$", full.names = TRUE))
  if (length(f) == 0) {
    stop("no .Rmd files found under vignettes/ -- the sweep has nothing to read")
  }
  normalizePath(f)
}

# Raw `eval` option text, per chunk, in file order. knitr's hooks hand back the
# EVALUATED value, which cannot tell `eval = TRUE` from `eval = has_ffmpeg` on a
# machine that has ffmpeg -- and the guard is the expression, not its value.
raw_eval_options <- function(file) {
  lines <- readLines(file, warn = FALSE)
  headers <- grep("^[`]{3}[{]r\\b", lines, value = TRUE)
  vapply(headers, function(h) {
    opts <- sub("^[`]{3}[{]r\\s*,?\\s*", "", sub("[}]\\s*$", "", h))
    m <- regmatches(opts, regexpr("\\beval\\s*=\\s*[^,]+", opts))
    if (length(m) == 0) "" else trimws(sub("^eval\\s*=\\s*", "", m))
  }, character(1), USE.NAMES = FALSE)
}

# Knit one vignette, recording per chunk (in order) its label, the evaluated
# `eval` value, and how many processes it started.
knit_pass <- function(file, workdir) {
  rec <- new.env(parent = emptyenv())
  rec$rows <- list()
  SPAWNS$n <- 0L

  knitr::knit_hooks$restore()
  knitr::opts_chunk$restore()
  knitr::opts_knit$restore()

  # opts_hooks fire before a chunk is evaluated; knit_hooks$chunk after it.
  knitr::opts_hooks$set(label = function(options) {
    SPAWNS$n <- 0L
    options
  })
  knitr::knit_hooks$set(chunk = function(x, options) {
    rec$rows[[length(rec$rows) + 1L]] <- list(
      label = options$label, eval = isTRUE(options$eval), spawns = SPAWNS$n
    )
    x
  })

  old <- setwd(workdir)
  on.exit({
    setwd(old)
    knitr::opts_hooks$restore()
    knitr::knit_hooks$restore()
  }, add = TRUE)

  suppressWarnings(suppressMessages(
    knitr::knit(file, output = tempfile(fileext = ".md"), quiet = TRUE)
  ))

  do.call(rbind, lapply(rec$rows, function(r) {
    data.frame(label = r$label, eval = r$eval, spawns = r$spawns,
               stringsAsFactors = FALSE)
  }))
}

# ---- child mode: pass 2 runs here, under a PATH with no media programs ------

if (identical(commandArgs(TRUE)[1], "--child")) {
  file <- commandArgs(TRUE)[2]
  pkgload::load_all(".", quiet = TRUE)
  found <- Sys.which(PROGRAMS)
  if (any(nzchar(found))) {
    stop("the child pass still reaches: ",
         paste(PROGRAMS[nzchar(found)], collapse = ", "))
  }
  res <- knit_pass(file, tempfile_dir <- {
    d <- tempfile(); dir.create(d); d
  })
  saveRDS(res, commandArgs(TRUE)[3])
  quit(save = "no")
}

# ---- parent -----------------------------------------------------------------

pkgload::load_all(".", quiet = TRUE)

found <- Sys.which(PROGRAMS)
if (!all(nzchar(found))) {
  stop("pass 1 needs all of ", paste(PROGRAMS, collapse = ", "),
       " on PATH; missing: ",
       paste(PROGRAMS[!nzchar(found)], collapse = ", "))
}

count_spawns <- function() {
  suppressMessages({
    trace(base::system2, tracer = quote(spawn_record(command)), print = FALSE)
    trace(base::system, tracer = quote(spawn_record(command)), print = FALSE)
  })
}
stop_counting <- function() {
  suppressMessages({
    untrace(base::system2)
    untrace(base::system)
  })
}

# A PATH the three programs are not on. `/usr/bin:/bin` keeps the shell and the
# ordinary system tools R itself may reach for.
bare_path <- function() {
  keep <- c(R.home("bin"), "/usr/bin", "/bin", "/usr/sbin", "/sbin")
  keep <- keep[dir.exists(keep)]
  bad <- vapply(keep, function(d) {
    any(file.exists(file.path(d, PROGRAMS)))
  }, logical(1))
  paste(keep[!bad], collapse = .Platform$path.sep)
}

files <- vignette_files()
all_rows <- list()

for (f in files) {
  raw <- raw_eval_options(f)

  wd <- tempfile(); dir.create(wd)
  count_spawns()
  p1 <- tryCatch(knit_pass(f, wd), finally = stop_counting())
  stop_counting()

  out <- tempfile(fileext = ".rds")
  st <- system2(file.path(R.home("bin"), "Rscript"),
                c(shQuote(normalizePath("tools/vignette_chunk_guards.R")),
                  "--child", shQuote(f), shQuote(out)),
                env = paste0("PATH=", shQuote(bare_path())),
                stdout = FALSE, stderr = FALSE)
  if (st != 0 || !file.exists(out)) {
    stop("pass 2 failed for ", basename(f), " (Rscript exit ", st, ")")
  }
  p2 <- readRDS(out)

  if (nrow(p1) != nrow(p2) || !identical(p1$label, p2$label)) {
    stop("the two passes disagree on the chunks of ", basename(f))
  }
  if (length(raw) != nrow(p1)) {
    stop("parsed ", length(raw), " chunk headers but knit ", nrow(p1),
         " chunks in ", basename(f))
  }

  all_rows[[length(all_rows) + 1L]] <- data.frame(
    vignette = basename(f),
    label = p1$label,
    guard = ifelse(nzchar(raw), raw, "-"),
    eval_here = p1$eval,
    spawns = p1$spawns,
    eval_bare = p2$eval,
    stringsAsFactors = FALSE
  )
}

rows <- do.call(rbind, all_rows)
rows$verdict <- ifelse(
  rows$spawns == 0L, "no spawn",
  ifelse(rows$eval_bare, "UNGUARDED", "guarded")
)

cat("vignettes swept:", length(files), "\n")
cat("chunks:", nrow(rows), "\n")
cat("chunks that started a program:", sum(rows$spawns > 0L), "\n\n")
print(rows, row.names = FALSE, right = FALSE)
cat("\n")

bad <- rows[rows$verdict == "UNGUARDED", ]
if (nrow(bad) == 0) {
  cat("unguarded spawning chunks: none\n")
} else {
  cat("unguarded spawning chunks:\n")
  print(bad, row.names = FALSE, right = FALSE)
  quit(save = "no", status = 1)
}
