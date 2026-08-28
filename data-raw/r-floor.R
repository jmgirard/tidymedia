# M076 -- measure the R version floor the package actually needs, so
# DESCRIPTION's `Depends: R (>= )` states a measurement rather than a guess.
#
# Reproduce (from the package root):
#
#   Rscript data-raw/r-floor.R
#
# Two inputs, printed separately and then maximized:
#
#   (a) SYNTAX. The two R-version-gated forms this package could use are the
#       native pipe `|>` and the backslash lambda `\(`, both R >= 4.1.0. They
#       are counted from the PARSER, not from a regex over source text, so a
#       `|>` inside a roxygen comment or a string does not count. Two surfaces
#       are swept: the package's own code (`R/*.R`) and the examples that
#       actually RUN on a user's machine or on CRAN's -- `man/*.Rd`'s
#       `\examples` sections with `\dontrun{}` and `\donttest{}` blocks
#       excluded, which is what `tools::Rd2ex(commentDontrun = TRUE,
#       commentDonttest = TRUE)` produces.
#
#   (b) DEPENDENCIES. Each versioned entry of DESCRIPTION's `Imports` names a
#       floor version of that package. The floor a user's installer would
#       resolve is that exact version, so this reads `Depends: R` out of THAT
#       version's DESCRIPTION -- fetched from CRAN (the Archive first, then the
#       current contrib dir) and untarred one file at a time -- not out of
#       whatever release happens to be installed here.
#
# The declared floor is the maximum of the two. Base packages (`tools`,
# `utils`) carry no version and are skipped; a fetch that fails stops the run
# rather than silently dropping a dependency from the maximum.

PKG <- normalizePath(".")
if (!file.exists(file.path(PKG, "DESCRIPTION"))) {
  stop("run this from the tidymedia package root", call. = FALSE)
}

SCRATCH <- file.path(tempdir(), "r-floor")
dir.create(SCRATCH, recursive = TRUE, showWarnings = FALSE)

# The parser's own names for the two forms. `text` is not used to identify
# them: a `\` is one character in several places, but the terminal token
# `'\\'` is emitted only for a lambda.
SYNTAX_TOKENS <- c(PIPE = "PIPE", LAMBDA = "'\\\\'")
SYNTAX_FLOOR <- "4.1.0"

# --- (a) the parser sweep ------------------------------------------------------

# Returns one row per occurrence, or a zero-row frame. Parsing is done with
# keep.source so getParseData() has anything to report at all.
scan_file <- function(path, surface) {
  exprs <- tryCatch(parse(path, keep.source = TRUE),
                    error = function(e) {
                      stop(sprintf("could not parse %s: %s", path,
                                   conditionMessage(e)), call. = FALSE)
                    })
  pd <- utils::getParseData(exprs)
  if (is.null(pd) || !nrow(pd)) {
    return(data.frame(surface = character(), file = character(),
                      line = integer(), form = character()))
  }
  hit <- pd$terminal & pd$token %in% SYNTAX_TOKENS
  if (!any(hit)) {
    return(data.frame(surface = character(), file = character(),
                      line = integer(), form = character()))
  }
  form <- names(SYNTAX_TOKENS)[match(pd$token[hit], SYNTAX_TOKENS)]
  data.frame(surface = surface, file = basename(path),
             line = pd$line1[hit], form = form, stringsAsFactors = FALSE)
}

# `R/` -- the shipped package code.
r_files <- file.path(PKG, "R", list.files(file.path(PKG, "R"), "[.]R$"))
code_hits <- do.call(rbind, lapply(r_files, scan_file, surface = "R/"))

# `man/` -- the examples that run. Rd2ex writes one .R file per help page with
# \dontrun and \donttest COMMENTED OUT, so the parser never sees them and the
# criterion's exclusion is enforced by the extractor rather than by a regex.
rd_files <- file.path(PKG, "man", list.files(file.path(PKG, "man"), "[.]Rd$"))
ex_dir <- file.path(SCRATCH, "examples")
dir.create(ex_dir, recursive = TRUE, showWarnings = FALSE)
ex_hits <- NULL
for (rd in rd_files) {
  out <- file.path(ex_dir, sub("[.]Rd$", ".R", basename(rd)))
  ok <- tools::Rd2ex(rd, out, commentDontrun = TRUE, commentDonttest = TRUE)
  # Rd2ex writes nothing for a help page with no \examples section.
  if (!file.exists(out)) next
  h <- scan_file(out, surface = "man/ examples")
  if (nrow(h)) {
    h$file <- basename(rd)
    ex_hits <- rbind(ex_hits, h)
  }
}

syntax_hits <- rbind(code_hits, ex_hits)
syntax_floor <- if (!is.null(syntax_hits) && nrow(syntax_hits)) SYNTAX_FLOOR else NA_character_

cat("=================================================================\n")
cat("(a) R-version-gated syntax in the shipped surface\n")
cat("=================================================================\n")
cat(sprintf("    swept: %d file(s) in R/, %d help page(s) in man/\n",
            length(r_files), length(rd_files)))
if (is.null(syntax_hits) || !nrow(syntax_hits)) {
  cat("    no `|>` and no `\\(` in parsed code -- this input sets no floor\n")
} else {
  for (s in unique(syntax_hits$surface)) {
    sub <- syntax_hits[syntax_hits$surface == s, ]
    sub <- sub[order(sub$file, sub$line), ]
    for (fm in unique(sub$form)) {
      one <- sub[sub$form == fm, ]
      cat(sprintf("      %-14s %-6s %3d occurrence(s), first at %s:%d\n",
                  s, fm, nrow(one), one$file[1], one$line[1]))
    }
  }
  cat(sprintf("\n    (a) floor: %s (%d occurrence(s))\n", syntax_floor, nrow(syntax_hits)))
}

# --- (b) the declared dependency floors ----------------------------------------

imports <- read.dcf(file.path(PKG, "DESCRIPTION"), "Imports")[[1]]
entries <- trimws(strsplit(gsub("\n", " ", imports), ",")[[1]])
entries <- entries[nzchar(entries)]

parse_entry <- function(e) {
  m <- regmatches(e, regexec("^([A-Za-z][A-Za-z0-9.]*)\\s*(?:\\(\\s*>=\\s*([^)]+)\\s*\\))?$", e))[[1]]
  if (!length(m)) stop("could not parse Imports entry: ", e, call. = FALSE)
  list(pkg = m[2], version = if (nzchar(m[3])) trimws(m[3]) else NA_character_)
}
parsed <- lapply(entries, parse_entry)

fetch_description <- function(pkg, ver) {
  dest <- file.path(SCRATCH, sprintf("%s_%s", pkg, ver))
  desc <- file.path(dest, pkg, "DESCRIPTION")
  if (file.exists(desc)) return(desc)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  tgz <- file.path(SCRATCH, sprintf("%s_%s.tar.gz", pkg, ver))
  urls <- c(
    sprintf("https://cran.r-project.org/src/contrib/Archive/%s/%s_%s.tar.gz", pkg, pkg, ver),
    sprintf("https://cloud.r-project.org/src/contrib/%s_%s.tar.gz", pkg, ver)
  )
  got <- FALSE
  for (u in urls) {
    status <- tryCatch(utils::download.file(u, tgz, quiet = TRUE, mode = "wb"),
                       error = function(e) 1L, warning = function(w) 1L)
    # A 404 can arrive as a nonzero status, a condition, or a short HTML body,
    # so all three are checked rather than trusting any one of them.
    if (identical(as.integer(status), 0L) && file.exists(tgz) && file.size(tgz) > 1000L) {
      got <- TRUE
      break
    }
  }
  if (!got) stop(sprintf("could not fetch %s %s from CRAN", pkg, ver), call. = FALSE)
  utils::untar(tgz, files = file.path(pkg, "DESCRIPTION"), exdir = dest)
  if (!file.exists(desc)) {
    stop(sprintf("%s %s tarball contained no DESCRIPTION", pkg, ver), call. = FALSE)
  }
  desc
}

r_floor_of <- function(desc_path) {
  dep <- read.dcf(desc_path, "Depends")[[1]]
  if (is.na(dep)) return(NA_character_)
  m <- regmatches(dep, regexec("R\\s*\\(\\s*>=\\s*([0-9][^)]*?)\\s*\\)", gsub("\n", " ", dep)))[[1]]
  if (!length(m)) NA_character_ else m[2]
}

cat("\n=================================================================\n")
cat("(b) `Depends: R` of each declared Imports floor version\n")
cat("=================================================================\n")
dep_floors <- character()
for (p in parsed) {
  if (is.na(p$version)) {
    cat(sprintf("    %-12s %-10s base/recommended, no version declared -- skipped\n",
                p$pkg, "--"))
    next
  }
  f <- r_floor_of(fetch_description(p$pkg, p$version))
  cat(sprintf("    %-12s %-10s Depends: R %s\n", p$pkg, p$version,
              if (is.na(f)) "(none declared)" else sprintf("(>= %s)", f)))
  if (!is.na(f)) dep_floors <- c(dep_floors, f)
}
dep_floor <- if (length(dep_floors)) {
  as.character(max(numeric_version(dep_floors)))
} else {
  NA_character_
}
cat(sprintf("\n    (b) floor: %s\n",
            if (is.na(dep_floor)) "none -- no Imports floor version declares one" else dep_floor))

# --- the maximum ---------------------------------------------------------------

both <- c(syntax_floor, dep_floor)
both <- both[!is.na(both)]
overall <- if (length(both)) as.character(max(numeric_version(both))) else NA_character_

cat("\n=================================================================\n")
cat("declared R floor = max(a, b)\n")
cat("=================================================================\n")
cat(sprintf("    (a) syntax:       %s\n", if (is.na(syntax_floor)) "none" else syntax_floor))
cat(sprintf("    (b) dependencies: %s\n", if (is.na(dep_floor)) "none" else dep_floor))
cat(sprintf("    maximum:          %s\n", if (is.na(overall)) "none" else overall))
cat(sprintf("\n    DESCRIPTION should read:  Depends: R (>= %s)\n", overall))
