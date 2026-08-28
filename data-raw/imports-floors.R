# M077 -- run the package's test suite against the exact versions DESCRIPTION's
# `Imports` field declares, so each floor states what was measured rather than
# what was assumed.
#
# Reproduce (from the package root):
#
#   Rscript data-raw/imports-floors.R              # baseline, then all floors pinned
#   Rscript data-raw/imports-floors.R --baseline   # the current-dependency run only
#   Rscript data-raw/imports-floors.R --repair     # AC3: on a floor that will
#                                                  # not build here, walk that
#                                                  # package's Archive forward to
#                                                  # the first version that does
#   Rscript data-raw/imports-floors.R --only cli   # pin ONE floor, siblings current
#   TM_LIBROOT=~/floor-libs Rscript data-raw/imports-floors.R   # reuse installs
#   Rscript data-raw/imports-floors.R --walk purrr # list that package's CRAN
#                                                  # Archive versions from the
#                                                  # declared floor forward
#
# WHAT IS PINNED. Every non-base entry of `Imports` -- the entries `read.dcf`
# enumerates, less `tools` and `utils`, which carry no floor -- is installed at
# the version it declares into ONE library, which is then prepended to the
# child's `.libPaths()`. Installs are `dependencies = FALSE`: the pinned set is
# the DIRECT Imports only, so each pinned package's own dependencies, and every
# transitive dependency of this package, resolve to whatever is in the user
# library. A joint pass therefore says the declared floors work TOGETHER against
# current everything-else; it attributes nothing to any single floor. `--only`
# is the attribution tool, run on a failure.
#
# THE LOAD-BEARING CONTROL (carried over from data-raw/withr-floor.R). The pin
# is by `.libPaths()` PRECEDENCE, not isolation -- the user library stays
# reachable, because pkgload and testthat live there and they need it. So a
# failed install would silently fall through to the user library's current
# release and report a green result for the wrong version. Each child therefore
# asserts, per pinned package, both the version it resolves and the DIRECTORY it
# resolves FROM, before anything is loaded, and again for every pinned namespace
# actually loaded once the suite has run. The version string alone does not
# close the hole: the user library holds current releases, so a failed install
# of a package whose current release happens to equal its floor would still
# match on version.
#
# THE SKIP FLOOR. Most execution tests `skip_if` the media binaries are absent,
# so "0 failures" is also true of a run where every such test skipped. Both
# binaries are asserted present before the suite starts, and the pinned run's
# skip count is compared against the baseline run's -- a pinned run that skips
# MORE than the baseline is reported as a failure of the comparison, not as a
# pass.
#
# `pkgload`, `testthat` and `devtools` are harness dependencies, and they load
# from the user library while resolving THEIR OWN imports through the pinned
# library first. A pinned floor old enough to break testthat breaks the harness
# rather than the package; that is reported as a run failure like any other,
# because a floor the suite cannot be run on is not a measured floor.

PKG <- normalizePath(".")
if (!file.exists(file.path(PKG, "DESCRIPTION"))) {
  stop("run this from the tidymedia package root", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
opt_value <- function(flag) {
  i <- match(flag, args)
  if (is.na(i)) return(NA_character_)
  if (i == length(args)) stop(flag, " needs a package name", call. = FALSE)
  args[i + 1L]
}
BASELINE_ONLY <- "--baseline" %in% args
REPAIR <- "--repair" %in% args
ONLY <- opt_value("--only")
WALK <- opt_value("--walk")

# `TM_LIBROOT` persists the pinned libraries across runs, which turns a re-run
# from a half-hour of compiling into minutes. Persisting a library is exactly
# the shape data-raw/withr-floor.R was noted as trusting too far -- a
# half-written install accepted because the directory exists -- so `install_pin`
# below re-reads the INSTALLED DESCRIPTION's Version and removes anything that
# does not match, rather than trusting `dir.exists()`.
LIBROOT <- Sys.getenv("TM_LIBROOT", unset = file.path(tempdir(), "imports-floors-libs"))
SCRATCH <- Sys.getenv("TM_SCRATCH", unset = file.path(tempdir(), "imports-floors-scratch"))
dir.create(LIBROOT, recursive = TRUE, showWarnings = FALSE)
dir.create(SCRATCH, recursive = TRUE, showWarnings = FALSE)

# --- the declared floors -------------------------------------------------------

# `R (>= x)` in Depends is M076's business; this reads Imports only.
imports <- read.dcf(file.path(PKG, "DESCRIPTION"), "Imports")[[1]]
entries <- trimws(strsplit(gsub("\n", " ", imports), ",")[[1]])
entries <- entries[nzchar(entries)]

parse_entry <- function(e) {
  m <- regmatches(e, regexec("^([A-Za-z][A-Za-z0-9.]*)\\s*(?:\\(\\s*>=\\s*([^)]+)\\s*\\))?$", e))[[1]]
  if (!length(m)) stop("could not parse Imports entry: ", e, call. = FALSE)
  list(pkg = m[2], version = if (nzchar(m[3])) trimws(m[3]) else NA_character_)
}
parsed <- lapply(entries, parse_entry)

BASE_PKGS <- rownames(utils::installed.packages(priority = c("base", "recommended")))
pins <- list()
for (p in parsed) {
  if (is.na(p$version)) {
    # Only a base or recommended package legitimately carries no floor. Any
    # other unversioned entry is a floor this script cannot pin, and dropping it
    # silently would leave AC1's "every non-base entry" unmet without saying so.
    if (!p$pkg %in% BASE_PKGS) {
      stop(sprintf("Imports entry `%s` declares no version and is not a base or recommended package",
                   p$pkg), call. = FALSE)
    }
    next
  }
  pins[[p$pkg]] <- p$version
}
if (!length(pins)) stop("no versioned Imports entries found", call. = FALSE)

if (!is.na(ONLY)) {
  if (!ONLY %in% names(pins)) {
    stop(ONLY, " is not a versioned Imports entry", call. = FALSE)
  }
  pins <- pins[ONLY]
}

# --- fetch + install one pinned version ----------------------------------------

fetch_tarball <- function(pkg, ver) {
  tgz <- file.path(SCRATCH, sprintf("%s_%s.tar.gz", pkg, ver))
  if (file.exists(tgz) && file.size(tgz) > 1000L) return(tgz)
  urls <- c(
    sprintf("https://cran.r-project.org/src/contrib/Archive/%s/%s_%s.tar.gz", pkg, pkg, ver),
    sprintf("https://cloud.r-project.org/src/contrib/%s_%s.tar.gz", pkg, ver)
  )
  for (u in urls) {
    # Warnings are muffled rather than read as failure: download.file warns on
    # benign conditions, and a warned-but-complete Archive fetch would otherwise
    # fall through to the current-contrib URL, which 404s for an archived
    # version. What a real failure looks like is checked on the result instead.
    status <- tryCatch(
      withCallingHandlers(
        utils::download.file(u, tgz, quiet = TRUE, mode = "wb"),
        warning = function(w) invokeRestart("muffleWarning")
      ),
      error = function(e) 1L
    )
    if (identical(as.integer(status), 0L) && file.exists(tgz) && file.size(tgz) > 1000L) {
      # A 404 body can clear the size floor, so the tarball is opened rather
      # than trusted: a listing that yields no DESCRIPTION is not a package.
      ok <- tryCatch({
        inside <- utils::untar(tgz, list = TRUE)
        any(basename(inside) == "DESCRIPTION")
      }, error = function(e) FALSE)
      if (isTRUE(ok)) return(tgz)
    }
    unlink(tgz)
  }
  stop(sprintf("could not fetch %s %s from CRAN", pkg, ver), call. = FALSE)
}

# Returns NULL on success, or the tail of the install log on a failed install --
# AC3's case, which the caller records rather than aborting on. `R CMD INSTALL`
# is driven directly rather than through `install.packages()`, because the
# criterion asks for THE ERROR to be recorded and `install.packages()` reduces a
# compiler error to "had non-zero exit status" in a warning.
install_pin <- function(lib, pkg, ver) {
  marker <- file.path(lib, pkg, "DESCRIPTION")
  if (file.exists(marker) &&
      identical(as.character(read.dcf(marker, "Version")[[1]]), ver)) {
    return(NULL)
  }
  # A half-written install from an interrupted run must not be reused: the
  # marker above is the installed DESCRIPTION, and anything short of it is
  # removed rather than trusted.
  unlink(file.path(lib, pkg), recursive = TRUE)
  tgz <- tryCatch(fetch_tarball(pkg, ver), error = function(e) e)
  if (inherits(tgz, "condition")) return(conditionMessage(tgz))
  # The pinned library goes on the install-time path too: `archive` and `purrr`
  # both LinkingTo `cli`, so they must compile against the PINNED cli headers,
  # not the user library's current ones. That is also why the installs are
  # ordered (see `install_order()`).
  ensure_deps(lib, pkg, ver)
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", "--no-test-load", "-l", shQuote(lib), shQuote(tgz)),
    env = c(sprintf("R_LIBS=%s", lib), sprintf("R_MAKEVARS_USER=%s", MAKEVARS)),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(out, "status")
  failed <- !is.null(status) && !identical(as.integer(status), 0L)
  if (!failed && !file.exists(file.path(lib, pkg, "DESCRIPTION"))) failed <- TRUE
  if (failed) {
    err <- grep("^(.*error|ERROR|make:)", out, value = TRUE)
    if (!length(err)) err <- utils::tail(out, 6L)
    return(paste(utils::tail(err, 6L), collapse = " / "))
  }
  got <- as.character(read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")[[1]])
  if (!identical(got, ver)) {
    return(sprintf("installed %s %s where %s was asked for", pkg, got, ver))
  }
  NULL
}

# --- compiling an old version on a new toolchain --------------------------------
#
# Debian and Ubuntu build R with `-Werror=format-security` in `Makeconf`, so a
# `Rf_error(msg)` with a non-literal format -- which rlang 1.1.0 and archive
# 1.1.1 both have, and which later releases of both fixed -- is a compile ERROR
# there and a warning everywhere else. That is a policy of the distribution
# doing the measuring, not a property of the floor, so the harness restores the
# vanilla behaviour rather than reporting those two floors as unbuildable. `+=`
# appends to the Makeconf value: the user Makevars is included after it.
#
# This is the ONLY compiler flag the harness changes, and it is disclosed in the
# D-entry: a user compiling these versions from source ON a hardened distro does
# hit these errors.
MAKEVARS <- file.path(SCRATCH, "Makevars")
writeLines(c(
  "CFLAGS += -Wno-error=format-security",
  "CXXFLAGS += -Wno-error=format-security",
  "CXX11FLAGS += -Wno-error=format-security",
  "CXX14FLAGS += -Wno-error=format-security",
  "CXX17FLAGS += -Wno-error=format-security",
  "CXX20FLAGS += -Wno-error=format-security"
), MAKEVARS)

# --- the pinned version's own dependencies, at current versions -----------------
#
# `dependencies = FALSE` pins the direct Imports and nothing else, but a pinned
# OLD version can need a package the CURRENT release no longer does -- tibble
# 3.1.4 needs `ellipsis` and `fansi`, which tibble 3.3.1 dropped, so neither is
# in the environment. Those are installed at their current CRAN versions, which
# is where AC4 says every non-pinned package sits. They go into the pinned
# library because it is writable and already first on the path; being there
# makes them no more pinned than anything else in the user library.
ensure_deps <- function(lib, pkg, ver) {
  want <- setdiff(deps_of(pkg, ver), c("R", BASE_PKGS))
  have <- rownames(utils::installed.packages(lib.loc = c(lib, .libPaths())))
  miss <- setdiff(want, have)
  if (!length(miss)) return(invisible(character()))
  cat(sprintf("      (%s %s also needs %s -- installing at current CRAN versions)\n",
              pkg, ver, paste(miss, collapse = ", ")))
  utils::install.packages(miss, lib = lib, repos = "https://cloud.r-project.org",
                          quiet = TRUE)
  still <- setdiff(miss, rownames(utils::installed.packages(lib.loc = c(lib, .libPaths()))))
  if (length(still)) {
    stop(sprintf("could not install %s, needed by %s %s",
                 paste(still, collapse = ", "), pkg, ver), call. = FALSE)
  }
  invisible(miss)
}

# --- install order among the pinned set ----------------------------------------
#
# `dependencies = FALSE` does not mean order-free: a pinned package that another
# pinned package LinkingTo-compiles against has to be in the library first, or
# the compile silently picks up the user library's current headers and the pin
# is not what was measured. The order is read from each PINNED version's own
# DESCRIPTION -- the tarball's, not the installed release's -- so it is right
# for the versions being measured rather than for today's.

deps_of <- function(pkg, ver) {
  tgz <- fetch_tarball(pkg, ver)
  dest <- file.path(SCRATCH, sprintf("desc-%s-%s", pkg, ver))
  desc <- file.path(dest, pkg, "DESCRIPTION")
  if (!file.exists(desc)) {
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    utils::untar(tgz, files = file.path(pkg, "DESCRIPTION"), exdir = dest)
  }
  if (!file.exists(desc)) stop(pkg, " ", ver, " tarball contained no DESCRIPTION", call. = FALSE)
  fields <- read.dcf(desc, c("Depends", "Imports", "LinkingTo"))
  flat <- paste(fields[!is.na(fields)], collapse = ",")
  nm <- trimws(sub("\\s*\\(.*", "", strsplit(gsub("\n", " ", flat), ",")[[1]]))
  nm[nzchar(nm)]
}

install_order <- function(pins) {
  need <- lapply(names(pins), function(p) intersect(deps_of(p, pins[[p]]), names(pins)))
  names(need) <- names(pins)
  done <- character()
  while (length(done) < length(pins)) {
    ready <- setdiff(names(pins)[vapply(need, function(d) all(d %in% done), logical(1))], done)
    if (!length(ready)) {
      # A cycle among the pinned set would otherwise spin here. CRAN forbids
      # one, so this is a "cannot happen" that says so rather than hanging.
      stop("dependency cycle among the pinned packages: ",
           paste(setdiff(names(pins), done), collapse = ", "), call. = FALSE)
    }
    done <- c(done, sort(ready))
  }
  done
}

# --- who requires what of the pinned packages -----------------------------------
#
# R enforces, at LOAD time, the version requirements every INSTALLED package
# declares. So pinning a floor is not just a question of whether that version
# builds: a current package elsewhere in the environment declaring
# `rlang (>= 1.1.7)` makes `rlang` 1.1.0 unloadable, and the suite never
# starts. Those requirers split in two, and the two are not treated alike:
#
#   * a requirer inside THIS package's runtime closure -- the recursive
#     dependencies of its own `Imports` -- is something a user installing
#     tidymedia gets. Its requirement is real, and a floor below it is a floor
#     that does not work. That floor moves.
#   * a requirer outside it -- testthat, pkgload, furrr and the rest of the
#     test harness -- is something no user installs. Moving a runtime floor to
#     satisfy the test harness would raise what users must have for a reason
#     they never see, so the HARNESS is held back instead, to the newest
#     version the pinned floors permit.
#
# Which packages were held back, and what forced each one, is what AC4's entry
# reports.

REQ_FIELDS <- c("Depends", "Imports", "LinkingTo")

# Every `pkg (>= ver)` an installed package declares, restricted to the pinned
# names. Returns a data frame of requirer / required / version.
declared_reqs <- function(names_wanted) {
  db <- utils::installed.packages(fields = REQ_FIELDS)
  out <- list()
  for (r in rownames(db)) {
    flat <- gsub("\n", " ", paste(db[r, REQ_FIELDS][!is.na(db[r, REQ_FIELDS])], collapse = ","))
    for (q in names_wanted) {
      m <- regmatches(flat, regexpr(sprintf("(^|[^A-Za-z0-9._])%s\\s*\\(\\s*>=\\s*[^)]+\\)", q), flat))
      if (!length(m) || !nzchar(m)) next
      v <- sub(".*>=\\s*([^) ]+).*", "\\1", m)
      out[[length(out) + 1L]] <- data.frame(requirer = r, required = q, version = v,
                                            stringsAsFactors = FALSE)
    }
  }
  if (!length(out)) {
    return(data.frame(requirer = character(), required = character(),
                      version = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, out)
}

# The recursive Depends/Imports/LinkingTo closure of a set of packages, read
# from what is installed. A package the closure names but that is not installed
# is skipped rather than fetched: it cannot be a live requirer here.
runtime_closure <- function(start) {
  db <- utils::installed.packages(fields = REQ_FIELDS)
  seen <- character()
  todo <- start
  while (length(todo)) {
    p <- todo[[1]]; todo <- todo[-1]
    if (p %in% seen || !p %in% rownames(db)) next
    seen <- c(seen, p)
    flat <- gsub("\n", " ", paste(db[p, REQ_FIELDS][!is.na(db[p, REQ_FIELDS])], collapse = ","))
    nm <- trimws(sub("\\s*\\(.*", "", strsplit(flat, ",")[[1]]))
    todo <- c(todo, setdiff(nm[nzchar(nm)], seen))
  }
  setdiff(seen, c("R", BASE_PKGS))
}

# What ONE version of a package requires of the pinned names, read from ITS OWN
# tarball rather than from whatever release happens to be installed. This is
# what makes a pinned requirer honest: current `dplyr` requires
# `rlang (>= 1.1.7)`, but dplyr is itself pinned at 1.1.0 here, and 1.1.0's own
# requirement is the one in force.
tarball_reqs <- function(pkg, ver, names_wanted) {
  tgz <- fetch_tarball(pkg, ver)
  dest <- file.path(SCRATCH, sprintf("desc-%s-%s", pkg, ver))
  desc <- file.path(dest, pkg, "DESCRIPTION")
  if (!file.exists(desc)) {
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    utils::untar(tgz, files = file.path(pkg, "DESCRIPTION"), exdir = dest)
  }
  f <- read.dcf(desc, REQ_FIELDS)
  flat <- gsub("\n", " ", paste(f[!is.na(f)], collapse = ","))
  out <- list()
  for (q in setdiff(names_wanted, pkg)) {
    m <- regmatches(flat, regexpr(sprintf("(^|[^A-Za-z0-9._])%s\\s*\\(\\s*>=\\s*[^)]+\\)", q), flat))
    if (!length(m) || !nzchar(m)) next
    out[[length(out) + 1L]] <- data.frame(requirer = pkg, required = q,
                                          version = sub(".*>=\\s*([^) ]+).*", "\\1", m),
                                          stringsAsFactors = FALSE)
  }
  if (!length(out)) {
    return(data.frame(requirer = character(), required = character(),
                      version = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, out)
}

# The pinned requirements one candidate version would VIOLATE, as text.
reqs_on_pins <- function(pkg, ver, pins) {
  r <- tarball_reqs(pkg, ver, names(pins))
  if (!nrow(r)) return(character())
  bad <- r[numeric_version(r$version) > numeric_version(unlist(pins)[r$required]), ]
  if (!nrow(bad)) character() else sprintf("%s (>= %s)", bad$required, bad$version)
}

# Walk a held-back package's versions NEWEST first and take the first whose own
# requirements the pinned floors satisfy -- "the newest version the pinned
# floors permit", which is what AC4's entry has to name.
newest_compatible <- function(pkg, pins) {
  vs <- rev(archive_versions(pkg, "0.0.0"))
  for (v in vs) {
    if (!length(reqs_on_pins(pkg, v, pins))) return(v)
  }
  stop(sprintf("no version of %s is compatible with the pinned floors", pkg), call. = FALSE)
}

# --- AC3: walk a package's CRAN Archive listing forward -------------------------

archive_versions <- function(pkg, from) {
  url <- sprintf("https://cran.r-project.org/src/contrib/Archive/%s/", pkg)
  html <- tryCatch(readLines(url, warn = FALSE), error = function(e) character())
  vers <- unique(regmatches(html, regexpr(sprintf("%s_[0-9][^\"]*?\\.tar\\.gz", pkg), html)))
  vers <- sub(sprintf("^%s_", pkg), "", sub("\\.tar\\.gz$", "", vers))
  # The Archive holds only superseded versions; the current release lives in
  # src/contrib and would otherwise be missing from the end of the walk.
  cur <- tryCatch({
    db <- utils::available.packages(repos = "https://cloud.r-project.org")
    if (pkg %in% rownames(db)) unname(db[pkg, "Version"]) else NA_character_
  }, error = function(e) NA_character_)
  if (!is.na(cur)) vers <- c(vers, cur)
  vers <- unique(vers[!is.na(vers)])
  vers <- vers[numeric_version(vers) >= numeric_version(from)]
  as.character(sort(numeric_version(vers)))
}

if (!is.na(WALK)) {
  if (!WALK %in% names(pins) && is.na(ONLY)) {
    # A walk is asked of a package by name, and a typo would otherwise print an
    # empty list that reads like "no later versions exist".
    stop(WALK, " is not a versioned Imports entry", call. = FALSE)
  }
  from <- pins[[WALK]]
  vs <- archive_versions(WALK, from)
  cat(sprintf("%s versions from the declared floor %s forward:\n", WALK, from))
  cat(paste0("    ", vs, collapse = "\n"), "\n")
  quit(save = "no")
}

# --- the child that runs the suite ---------------------------------------------

pins_env <- paste(sprintf("%s=%s", names(pins), unlist(pins)), collapse = ";")

child <- file.path(SCRATCH, "suite.R")
writeLines(c(
  sprintf('PKG <- "%s"', PKG),
  'lib <- Sys.getenv("TM_LIB")',
  'pins <- Sys.getenv("TM_PINS")',
  'pins <- if (nzchar(pins)) {',
  '  kv <- strsplit(strsplit(pins, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)',
  '  stats::setNames(vapply(kv, `[`, "", 2L), vapply(kv, `[`, "", 1L))',
  '} else character()',
  '',
  '# AC2: the binaries first. Most execution tests skip_if() these are absent,',
  '# so a run without them is a run that proves nothing about the floors.',
  'for (b in c("ffmpeg", "mediainfo")) {',
  '  if (!nzchar(Sys.which(b))) stop("`", b, "` is not on PATH", call. = FALSE)',
  '}',
  '',
  '# The load-bearing control: version AND provenance, per pinned package,',
  '# before anything is loaded. A failed install falls through to the user',
  '# library, and only the directory check catches that.',
  'norm <- function(p) normalizePath(p, winslash = "/", mustWork = TRUE)',
  'if (nzchar(lib)) {',
  '  for (p in names(pins)) {',
  '    where <- norm(dirname(find.package(p)))',
  '    got <- as.character(utils::packageVersion(p))',
  '    cat(sprintf("  %-10s %-8s from %s\\n", p, got, where))',
  '    if (!identical(got, unname(pins[[p]]))) {',
  '      stop(sprintf("%s resolves %s where %s was pinned", p, got, pins[[p]]), call. = FALSE)',
  '    }',
  '    if (!identical(where, norm(lib))) {',
  '      stop(sprintf("%s resolves from %s, not the pinned library %s", p, where, lib), call. = FALSE)',
  '    }',
  '  }',
  '} else {',
  '  for (p in names(pins)) {',
  '    cat(sprintf("  %-10s %-8s from %s\\n", p, as.character(utils::packageVersion(p)),',
  '                norm(dirname(find.package(p)))))',
  '  }',
  '}',
  '',
  'setwd(PKG)',
  'suppressMessages(pkgload::load_all(PKG, quiet = TRUE, export_all = FALSE))',
  'res <- testthat::test_dir("tests/testthat", package = "tidymedia",',
  '                          reporter = "silent", stop_on_failure = FALSE,',
  '                          load_package = "none")',
  'df <- as.data.frame(res)',
  'by_file <- stats::aggregate(cbind(nb = df$nb, failed = df$failed,',
  '                                  skipped = as.integer(df$skipped),',
  '                                  error = as.integer(df$error)),',
  '                            by = list(file = df$file), FUN = sum)',
  'by_file <- by_file[order(by_file$file), ]',
  'cat("\\n  file                                         pass fail skip\\n")',
  'for (i in seq_len(nrow(by_file))) {',
  '  r <- by_file[i, ]',
  '  cat(sprintf("  %-44s %4d %4d %4d\\n", r$file, r$nb - r$failed, r$failed, r$skipped))',
  '}',
  '',
  '# Provenance again, after the suite: a pinned package that was resolved',
  '# correctly above but LOADED from elsewhere (a namespace another package',
  '# pulled in first) would otherwise pass the up-front check and still have run',
  '# the wrong code.',
  'if (nzchar(lib)) {',
  '  for (p in intersect(names(pins), loadedNamespaces())) {',
  '    where <- norm(dirname(getNamespaceInfo(p, "path")))',
  '    if (!identical(where, norm(lib))) {',
  '      stop(sprintf("%s was LOADED from %s, not the pinned library", p, where), call. = FALSE)',
  '    }',
  '  }',
  '}',
  '',
  'failed <- sum(by_file$failed)',
  'errored <- sum(by_file$error)',
  'cat(sprintf("\\nTOTALS pass=%d fail=%d skip=%d files=%d\\n",',
  '            sum(by_file$nb) - failed, failed, sum(by_file$skipped), nrow(by_file)))',
  'if (failed || errored) {',
  '  bad <- df[df$failed > 0 | df$error, c("file", "test")]',
  '  for (i in seq_len(nrow(bad))) cat("  FAIL ", bad$file[i], " :: ", bad$test[i], "\\n", sep = "")',
  '  # A list of failing block NAMES says nothing about WHY, and "why" is the',
  '  # whole question when the run is meant to attribute a failure to a floor.',
  '  cat("\\n  first failure message per file:\\n")',
  '  shown <- character()',
  '  for (blk in res) {',
  '    if (blk$file %in% shown) next',
  '    msgs <- Filter(function(r) inherits(r, c("expectation_failure", "expectation_error")), blk$results)',
  '    if (!length(msgs)) next',
  '    shown <- c(shown, blk$file)',
  '    cat("  ---- ", blk$file, " :: ", blk$test, "\\n", sep = "")',
  '    cat("       ", gsub("\\n", "\\n       ", conditionMessage(msgs[[1]])), "\\n", sep = "")',
  '  }',
  '  stop(sprintf("%d failing test(s) under this library", failed + errored), call. = FALSE)',
  '}'
), child)

run_child <- function(label, lib) {
  cat("\n================", label, "================\n")
  if (nzchar(lib)) cat("library:", lib, "\n")
  env <- c(sprintf("TM_LIB=%s", lib), sprintf("TM_PINS=%s", pins_env), "NOT_CRAN=true")
  if (nzchar(lib)) env <- c(env, sprintf("R_LIBS=%s", lib))
  # A wall-clock bound on the child. Without one, a single hung spawn -- an
  # ffmpeg blocked opening a FIFO ignores SIGTERM, which is how the suite's own
  # timeout fixtures can wedge on Linux -- stops the run with no output at all,
  # for as long as anyone leaves it. `timeout` seconds is not a result; it is a
  # run that has to be repeated, and it says so.
  limit <- as.numeric(Sys.getenv("TM_RUN_TIMEOUT", unset = "2400"))
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(child),
                                  env = env, stdout = TRUE, stderr = TRUE,
                                  timeout = limit))
  cat(out, sep = "\n")
  cat("\n")
  status <- attr(out, "status")
  # A non-zero child exit is a failed run even when the printed table looks
  # calm -- the child stops on a provenance mismatch before it prints anything.
  if (identical(as.integer(status), 124L)) {
    stop(sprintf("the %s run was still going after %g s and was killed -- a wedged spawn, not a result; re-run it",
                 label, limit), call. = FALSE)
  }
  if (!is.null(status) && !identical(as.integer(status), 0L)) {
    stop(sprintf("the %s run exited %s", label, status), call. = FALSE)
  }
  line <- grep("^TOTALS ", out, value = TRUE)
  if (length(line) != 1L) stop("no TOTALS line from the ", label, " run", call. = FALSE)
  nums <- as.integer(regmatches(line, gregexpr("[0-9]+", line))[[1]])
  stats::setNames(as.list(nums), c("pass", "fail", "skip", "files"))
}

# --- drive it ------------------------------------------------------------------

base <- run_child("BASELINE (current dependencies)", "")
if (BASELINE_ONLY) quit(save = "no")

lib <- file.path(LIBROOT, if (is.na(ONLY)) "all" else ONLY)
dir.create(lib, recursive = TRUE, showWarnings = FALSE)

# --- what the environment requires of the pinned floors -------------------------

cat("\n---- requirements the environment places on the pinned floors ----\n")
CLOSURE <- runtime_closure(names(pins))
gather_reqs <- function(pins) {
  outside <- declared_reqs(names(pins))
  outside <- outside[!outside$requirer %in% names(pins), , drop = FALSE]
  inside <- do.call(rbind, lapply(names(pins), function(p) tarball_reqs(p, pins[[p]], names(pins))))
  rbind(outside, inside)
}
holdbacks <- list()
moves <- character()
for (round in 1:5) {
  reqs <- gather_reqs(pins)
  reqs <- reqs[numeric_version(reqs$version) > numeric_version(unlist(pins)[reqs$required]), , drop = FALSE]
  if (!nrow(reqs)) break
  rt <- reqs[reqs$requirer %in% CLOSURE, , drop = FALSE]
  if (nrow(rt)) {
    # A requirer inside this package's runtime closure is something a user
    # installing tidymedia gets, so the floor below it does not work and moves.
    for (q in unique(rt$required)) {
      sub <- rt[rt$required == q, , drop = FALSE]
      need <- as.character(max(numeric_version(sub$version)))
      moves <- c(moves, sprintf("%s: %s -> %s (required by %s)", q, pins[[q]], need,
                                paste(sprintf("%s %s (>= %s)", sub$requirer,
                                              vapply(sub$requirer, function(r)
                                                as.character(utils::packageVersion(r)), ""),
                                              sub$version), collapse = ", ")))
      cat(sprintf("  MOVE     %-10s %s -> %s  (runtime closure: %s)\n", q, pins[[q]], need,
                  paste(unique(sub$requirer), collapse = ", ")))
      pins[[q]] <- need
    }
    next
  }
  # Everything left is outside the runtime closure: the test harness. No user
  # installs it, so it is held back rather than allowed to raise a floor.
  for (r in unique(reqs$requirer)) {
    sub <- reqs[reqs$requirer == r, , drop = FALSE]
    forced <- paste(sprintf("%s (>= %s)", sub$required, sub$version), collapse = ", ")
    v <- newest_compatible(r, pins)
    cat(sprintf("  HOLDBACK %-10s %s -> %s  (its current release needs %s)\n",
                r, as.character(utils::packageVersion(r)), v, forced))
    holdbacks[[r]] <- list(version = v, forced = forced,
                           was = as.character(utils::packageVersion(r)))
  }
  break
}
if (!length(moves) && !length(holdbacks)) cat("  none -- every declared floor satisfies the environment as it stands\n")
pins_env <- paste(sprintf("%s=%s", names(pins), unlist(pins)), collapse = ";")
cat("\n---- installing the declared floors into", lib, "----\n")
order <- install_order(pins)
cat("    install order (LinkingTo/Imports among the pinned set first):",
    paste(order, collapse = " -> "), "\n")
failures <- list()
for (p in order) {
  err <- install_pin(lib, p, pins[[p]])
  cat(sprintf("  %-10s %-8s %s\n", p, pins[[p]], if (is.null(err)) "installed" else "FAILED"))
  if (!is.null(err)) {
    cat("      ", err, "\n", sep = "")
    failures[[p]] <- err
  }
}
if (length(failures)) {
  cat("\n---- AC3: walking each failed floor forward through the CRAN Archive ----\n")
  if (!REPAIR) {
    stop(sprintf("%d declared floor(s) do not install here: %s -- re-run with --repair to walk them forward",
                 length(failures), paste(names(failures), collapse = ", ")), call. = FALSE)
  }
  probe <- file.path(LIBROOT, "walk")
  dir.create(probe, recursive = TRUE, showWarnings = FALSE)
  moved <- character()
  for (p in names(failures)) {
    # The walk library carries the ALREADY-INSTALLED pins plus the user
    # library, so a candidate compiles against the same headers the joint run
    # will hand it -- a `cli` candidate found against current cli headers would
    # not be the version the joint run then measures.
    vs <- archive_versions(p, pins[[p]])
    vs <- vs[vs != pins[[p]]]
    cat(sprintf("  %s: %d candidate version(s) after %s\n", p, length(vs), pins[[p]]))
    found <- NA_character_
    for (v in vs) {
      e <- install_pin(lib, p, v)
      cat(sprintf("    %-8s %s\n", v, if (is.null(e)) "INSTALLS" else "fails"))
      if (is.null(e)) { found <- v; break }
    }
    if (is.na(found)) {
      stop(sprintf("no version of %s from %s forward installs here", p, pins[[p]]), call. = FALSE)
    }
    moved <- c(moved, sprintf("%s: %s -> %s", p, pins[[p]], found))
    pins[[p]] <- found
  }
  cat("\n  floors that moved:\n")
  cat(paste0("    ", moved, collapse = "\n"), "\n")
  # The pins the children assert must be the MOVED ones, or the joint run below
  # would assert the declared floor against a library holding the moved version.
  pins_env <- paste(sprintf("%s=%s", names(pins), unlist(pins)), collapse = ";")
}

if (length(holdbacks)) {
  cat("\n---- holding the test harness back to what the floors permit ----\n")
  for (r in names(holdbacks)) {
    err <- install_pin(lib, r, holdbacks[[r]]$version)
    cat(sprintf("  %-10s %-8s %s\n", r, holdbacks[[r]]$version,
                if (is.null(err)) "installed" else "FAILED"))
    if (!is.null(err)) stop(sprintf("could not hold %s back to %s: %s", r,
                                    holdbacks[[r]]$version, err), call. = FALSE)
  }
  # The child asserts these the same way it asserts the pins: a held-back
  # package that quietly resolved its current release from the user library
  # would put the requirement back that holding it removed.
  hold_env <- paste(sprintf("%s=%s", names(holdbacks),
                            vapply(holdbacks, `[[`, "", "version")), collapse = ";")
  pins_env <- paste(c(pins_env, hold_env), collapse = ";")
}

pinned <- run_child(if (is.na(ONLY)) "PINNED (all declared floors)" else sprintf("PINNED (%s only)", ONLY), lib)

cat("\n=================================================================\n")
cat("comparison\n")
cat("=================================================================\n")
cat(sprintf("    baseline  pass=%d fail=%d skip=%d over %d files\n",
            base$pass, base$fail, base$skip, base$files))
cat(sprintf("    pinned    pass=%d fail=%d skip=%d over %d files\n",
            pinned$pass, pinned$fail, pinned$skip, pinned$files))
if (!identical(pinned$skip, base$skip)) {
  stop(sprintf("skip count moved: %d pinned vs %d baseline -- a pinned run that skips more has not exercised what the baseline did",
               pinned$skip, base$skip), call. = FALSE)
}
if (length(moves)) {
  cat("\n    floors that MOVED (a runtime-closure requirement the declared version fails):\n")
  cat(paste0("      ", moves, collapse = "\n"), "\n")
} else {
  cat("\n    no floor moved\n")
}
if (length(holdbacks)) {
  cat("\n    packages HELD BACK so the floors could load (AC4 names these):\n")
  for (r in names(holdbacks)) {
    cat(sprintf("      %-10s %s -> %s, forced by its %s\n", r, holdbacks[[r]]$was,
                holdbacks[[r]]$version, holdbacks[[r]]$forced))
  }
}
cat("\n    the declared floors load, and the suite passes on them\n")
