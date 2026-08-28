# M077 -- run the package's test suite against the exact versions DESCRIPTION's
# `Imports` field declares, so each floor states what was measured rather than
# what was assumed.
#
# Reproduce (from the package root):
#
#   Rscript data-raw/imports-floors.R              # baseline, then all floors pinned
#   Rscript data-raw/imports-floors.R --baseline   # the current-dependency run only
#   Rscript data-raw/imports-floors.R --only cli   # pin ONE floor, siblings current
#   TM_LIBROOT=~/floor-libs Rscript data-raw/imports-floors.R   # reuse installs
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
ONLY <- opt_value("--only")

# `TM_LIBROOT` persists the pinned libraries across runs, which turns a re-run
# from a half-hour of compiling into minutes. Persisting a library is exactly
# the shape data-raw/withr-floor.R was noted as trusting too far -- a
# half-written install accepted because the directory exists -- so `install_pin`
# below re-reads the INSTALLED DESCRIPTION's Version and removes anything that
# does not match, rather than trusting `dir.exists()`.
# `path.expand` here, not at the call site: `R CMD INSTALL -l` receives this
# path shQuote'd (a library root may contain a space), and a `~` inside single
# quotes is not expanded by the shell. Expanding once, at the source, is what
# makes `TM_LIBROOT=~/floor libs` mean the directory it looks like it means.
LIBROOT <- path.expand(Sys.getenv("TM_LIBROOT", unset = file.path(tempdir(), "imports-floors-libs")))
SCRATCH <- path.expand(Sys.getenv("TM_SCRATCH", unset = file.path(tempdir(), "imports-floors-scratch")))
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

# Still the broad set, and still right for what it is used for below: which of
# a pinned version's own dependencies do NOT need installing (`ensure_deps`) and
# which packages are not part of the runtime closure (`runtime_closure`).
BASE_PKGS <- rownames(utils::installed.packages(priority = c("base", "recommended")))

# THE UNVERSIONED CARVE-OUT, NAMED. `priority = c("base", "recommended")` is a
# property of the R installation doing the measuring, not of this DESCRIPTION:
# it waves through every one of ~30 packages, so an unversioned `MASS` -- a
# floor nobody declared and this script cannot pin -- is skipped in silence.
# The carve-out is the two unversioned entries DESCRIPTION actually declares.
# Add one here when DESCRIPTION adds one, deliberately.
UNVERSIONED_OK <- c("tools", "utils")

pins <- list()
for (p in parsed) {
  if (is.na(p$version)) {
    if (!p$pkg %in% UNVERSIONED_OK) {
      stop(sprintf("Imports entry `%s` declares no version and is not one of the unversioned entries this script knows about (%s) -- it is a floor that cannot be pinned, and skipping it would leave `every non-base entry` unmet without saying so",
                   p$pkg, paste(UNVERSIONED_OK, collapse = ", ")), call. = FALSE)
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

# THE ONE TEST OF "is this a package tarball", used by both branches of
# `fetch_tarball()` below. Three things clear a size floor without being a
# package: a gzip truncated by an interrupted download, an HTTP error body, and
# a well-formed tarball of something that carries no `DESCRIPTION`. The size
# floor recognizes none of them.
is_package_tarball <- function(tgz) {
  if (!file.exists(tgz) || file.size(tgz) <= 1000L) return(FALSE)
  inside <- tryCatch(suppressWarnings(utils::untar(tgz, list = TRUE)),
                     error = function(e) NULL)
  if (is.null(inside)) return(FALSE)
  # The listing alone is not enough. `untar(list = TRUE)` shells out to `tar`,
  # and a gzip truncated PAST the DESCRIPTION entry still prints the entries
  # read before the end -- so the listing says "package" about a file that will
  # not extract. `tar` reports that by exiting non-zero, which is what this
  # reads.
  st <- attr(inside, "status")
  if (!is.null(st) && !identical(as.integer(st), 0L)) return(FALSE)
  any(basename(inside) == "DESCRIPTION")
}

fetch_tarball <- function(pkg, ver) {
  tgz <- file.path(SCRATCH, sprintf("%s_%s.tar.gz", pkg, ver))
  # The CACHE branch validates on the same terms as the download branch. It used
  # to return on the size floor alone, so a defect that landed once in a
  # persisted TM_SCRATCH was handed to every later run for as long as the
  # directory survived.
  if (file.exists(tgz)) {
    if (is_package_tarball(tgz)) return(tgz)
    cat(sprintf("      (cached %s %s is not a readable package tarball -- refetching)\n",
                pkg, ver))
    unlink(tgz)
  }
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
    if (identical(as.integer(status), 0L) && is_package_tarball(tgz)) return(tgz)
    unlink(tgz)
  }
  stop(sprintf("could not fetch %s %s from CRAN", pkg, ver), call. = FALSE)
}

# Returns NULL on success, or the tail of the install log on a failed install,
# which the caller prints for every floor before aborting on the set of them.
# `R CMD INSTALL` is driven directly rather than through `install.packages()`,
# because THE ERROR is what a failed floor has to report and
# `install.packages()` reduces a compiler error to "had non-zero exit status"
# in a warning.
# What an installed entry was compiled against: for every PINNED package this
# one LinkingTo-depends on, the version sitting in `lib` at the moment the
# compile ran. Written beside the installed DESCRIPTION and re-read on reuse.
PIN_STAMP <- "tidymedia-floor-pin.dcf"

linkingto_state <- function(lib, pkg, ver, pins) {
  lt <- intersect(linkingto_of(pkg, ver), names(pins))
  if (!length(lt)) return("(none)")
  paste(vapply(sort(lt), function(q) {
    d <- file.path(lib, q, "DESCRIPTION")
    v <- if (file.exists(d)) as.character(read.dcf(d, "Version")[[1]]) else "(absent)"
    sprintf("%s=%s", q, v)
  }, ""), collapse = " ")
}

# `Version` alone is not enough to reuse an entry in a persisted TM_LIBROOT.
# `archive` and `purrr` both LinkingTo `cli`: move the `cli` floor and reinstall
# it, and those two still carry the right Version over binaries compiled against
# the headers of the version before it -- a pin the run then reports as
# measured. The stamp records what the entry was actually linked against, so a
# changed LinkingTo dependency reinstalls the dependent. An entry with no stamp
# at all -- a library from before this check existed -- is not reused either.
can_reuse <- function(lib, pkg, ver, pins) {
  marker <- file.path(lib, pkg, "DESCRIPTION")
  stamp <- file.path(lib, pkg, PIN_STAMP)
  if (!file.exists(marker) || !file.exists(stamp)) return(FALSE)
  if (!identical(as.character(read.dcf(marker, "Version")[[1]]), ver)) return(FALSE)
  identical(as.character(read.dcf(stamp, "LinkedAgainst")[[1]]),
            linkingto_state(lib, pkg, ver, pins))
}

install_pin <- function(lib, pkg, ver, pins) {
  if (can_reuse(lib, pkg, ver, pins)) return(NULL)
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
    # No `--no-test-load`: "installs" and "loads" are different claims, and the
    # one this run needs is the second. A floor that compiles and then cannot
    # be loaded is a failed floor, reported here rather than met later as a
    # suite that will not start.
    c("CMD", "INSTALL", "-l", shQuote(lib), shQuote(tgz)),
    # shQuote for the same reason run_child does it: system2(env = ) pastes these
  # into a `sh -c` line, so a TM_LIBROOT containing a space would end the
  # assignment and R_LIBS would never reach the install.
  env = c(sprintf("R_LIBS=%s", shQuote(lib)), sprintf("R_MAKEVARS_USER=%s", shQuote(MAKEVARS))),
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
  writeLines(c(sprintf("Package: %s", pkg),
               sprintf("Version: %s", ver),
               sprintf("LinkedAgainst: %s", linkingto_state(lib, pkg, ver, pins))),
             file.path(lib, pkg, PIN_STAMP))
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

# Just the LinkingTo field of ONE version, read from its own tarball. This is
# the edge that makes a stale reuse dangerous: a LinkingTo dependency is
# compiled against, so a library entry can carry the right `Version` and a
# binary built against headers that are no longer there.
linkingto_of <- function(pkg, ver) {
  tgz <- fetch_tarball(pkg, ver)
  dest <- file.path(SCRATCH, sprintf("desc-%s-%s", pkg, ver))
  desc <- file.path(dest, pkg, "DESCRIPTION")
  if (!file.exists(desc)) {
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    utils::untar(tgz, files = file.path(pkg, "DESCRIPTION"), exdir = dest)
  }
  f <- read.dcf(desc, "LinkingTo")[[1]]
  if (is.na(f)) return(character())
  nm <- trimws(sub("\\s*\\(.*", "", strsplit(gsub("\n", " ", f), ",")[[1]]))
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

# --- reconciling the declared floors with what the environment requires --------

gather_reqs <- function(pins) {
  outside <- declared_reqs(names(pins))
  outside <- outside[!outside$requirer %in% names(pins), , drop = FALSE]
  inside <- do.call(rbind, lapply(names(pins), function(p) tarball_reqs(p, pins[[p]], names(pins))))
  rbind(outside, inside)
}

# WHO MAY BE HELD BACK. Not "everything outside the runtime closure": that
# definition is a description of this host, not a decision, and off the
# container it will happily downgrade whatever unrelated package happens to
# declare a requirement the floors miss. The set is NAMED, and it is the two
# packages D055 item 2 records as actually held back -- `testthat` 3.1.10 and
# `furrr` 0.3.1. A requirer outside the runtime closure and outside this set is
# something nobody decided to hold back, so the run stops and says which
# package it is: extending the set, or moving a floor, is a judgement about
# what is being measured and belongs to whoever is measuring.
HOLDBACK_SET <- c("testthat", "furrr")

MAX_ROUNDS <- 5L

# `gather` and `pick` are arguments rather than the globals they resolve to in
# the run, so the two ways this can refuse -- a stray requirer, and rounds that
# never settle -- are reachable from data-raw/floor-probes.R without a network
# or an install. Returns the reconciled pins with the moves and holdbacks that
# produced them; aborts rather than returning a set it has not settled.
reconcile <- function(pins, closure, gather, pick,
                      version_of = function(p) as.character(utils::packageVersion(p))) {
  holdbacks <- list()
  moves <- character()
  settled <- FALSE
  for (round in seq_len(MAX_ROUNDS)) {
    reqs <- gather(pins)
    reqs <- reqs[numeric_version(reqs$version) > numeric_version(unlist(pins)[reqs$required]), , drop = FALSE]
    if (!nrow(reqs)) { settled <- TRUE; break }
    rt <- reqs[reqs$requirer %in% closure, , drop = FALSE]
    if (nrow(rt)) {
      # A requirer inside this package's runtime closure is something a user
      # installing tidymedia gets, so the floor below it does not work and moves.
      for (q in unique(rt$required)) {
        req <- rt[rt$required == q, , drop = FALSE]
        need <- as.character(max(numeric_version(req$version)))
        moves <- c(moves, sprintf("%s: %s -> %s (required by %s)", q, pins[[q]], need,
                                  paste(sprintf("%s %s (>= %s)", req$requirer,
                                                vapply(req$requirer, version_of, ""),
                                                req$version), collapse = ", ")))
        cat(sprintf("  MOVE     %-10s %s -> %s  (runtime closure: %s)\n", q, pins[[q]], need,
                    paste(unique(req$requirer), collapse = ", ")))
        pins[[q]] <- need
      }
      next
    }
    # Everything left is outside the runtime closure. No user installs it, so it
    # is held back rather than allowed to raise a floor -- but only if it is one
    # of the packages named above.
    stray <- setdiff(unique(reqs$requirer), HOLDBACK_SET)
    if (length(stray)) {
      stop(sprintf(paste0("%s requires more of the pinned floors than they declare, ",
                          "and is not one of the harness packages this run may hold back (%s). ",
                          "Either it belongs in HOLDBACK_SET or a floor has to move -- ",
                          "neither is a call this script makes on its own."),
                   paste(stray, collapse = ", "), paste(HOLDBACK_SET, collapse = ", ")),
           call. = FALSE)
    }
    for (r in unique(reqs$requirer)) {
      req <- reqs[reqs$requirer == r, , drop = FALSE]
      forced <- paste(sprintf("%s (>= %s)", req$required, req$version), collapse = ", ")
      v <- pick(r, pins)
      cat(sprintf("  HOLDBACK %-10s %s -> %s  (its current release needs %s)\n",
                  r, version_of(r), v, forced))
      holdbacks[[r]] <- list(version = v, forced = forced, was = version_of(r))
    }
    # `pick()` chose, per held-back package, a version whose own requirements
    # the pins already satisfy, so holding them back is the last step rather
    # than another round.
    settled <- TRUE
    break
  }
  if (!settled) {
    # Falling out of the loop used to be indistinguishable from settling in one
    # round: the run went on to install pins that still violated a requirement
    # somewhere, and reported whatever the suite then did.
    stop(sprintf("floors and requirements did not reconcile in %d rounds -- the run has not established what to pin, so it has nothing to measure",
                 MAX_ROUNDS), call. = FALSE)
  }
  list(pins = pins, moves = moves, holdbacks = holdbacks)
}

# --- a package's CRAN Archive listing, oldest first -----------------------------
#
# Used by `newest_compatible()` to find the newest release of a held-back
# harness package that the pinned floors permit.

# One call, named, so data-raw/floor-probes.R can hand `archive_versions()` a
# failed fetch without a network.
cran_db <- function() utils::available.packages(repos = "https://cloud.r-project.org")

archive_versions <- function(pkg, from) {
  url <- sprintf("https://cran.r-project.org/src/contrib/Archive/%s/", pkg)
  # A NETWORK FAILURE IS NOT A FACT ABOUT CRAN. Both reads below used to fall
  # back to "nothing found", which is indistinguishable from "this package has
  # no archived versions" and from "no version later than the floor exists" --
  # and this list is what `newest_compatible()` searches, so an empty one there
  # reads as "no version of %s is compatible with the pinned floors". The run
  # stops instead, because the difference is not one it can recover.
  html <- tryCatch(readLines(url, warn = FALSE), error = function(e) e)
  if (inherits(html, "condition")) {
    stop(sprintf("could not read %s's CRAN Archive listing at %s: %s",
                 pkg, url, conditionMessage(html)), call. = FALSE)
  }
  vers <- unique(regmatches(html, regexpr(sprintf("%s_[0-9][^\"]*?\\.tar\\.gz", pkg), html)))
  vers <- sub(sprintf("^%s_", pkg), "", sub("\\.tar\\.gz$", "", vers))
  # The Archive holds only superseded versions; the current release lives in
  # src/contrib and would otherwise be missing from the end of the list.
  db <- tryCatch(cran_db(), error = function(e) e)
  if (inherits(db, "condition")) {
    stop(sprintf("could not fetch the CRAN package database: %s", conditionMessage(db)),
         call. = FALSE)
  }
  # `available.packages()` reports a failed fetch as a WARNING and an empty
  # matrix, not an error, so the row count is checked as well as the class.
  if (!nrow(db)) {
    stop("the CRAN package database came back empty -- that is a failed fetch, not a CRAN with no packages in it",
         call. = FALSE)
  }
  cur <- if (pkg %in% rownames(db)) unname(db[pkg, "Version"]) else NA_character_
  if (!is.na(cur)) vers <- c(vers, cur)
  vers <- unique(vers[!is.na(vers)])
  vers <- vers[numeric_version(vers) >= numeric_version(from)]
  as.character(sort(numeric_version(vers)))
}

# --- the child that runs the suite ---------------------------------------------

pins_env <- paste(sprintf("%s=%s", names(pins), unlist(pins)), collapse = ";")

child <- file.path(SCRATCH, "suite.R")
writeLines(c(
  sprintf('PKG <- "%s"', PKG),
  '# The mode is explicit, and the child refuses to run without its handles.',
  '# An earlier revision inferred "baseline" from an EMPTY TM_LIB, and when a',
  '# quoting bug stopped both variables from reaching the child at all, every',
  '# assertion below silently turned itself off while the run still reported a',
  '# green suite. A control that can quietly not run is not a control.',
  'mode <- Sys.getenv("TM_MODE")',
  'if (!mode %in% c("baseline", "pinned")) stop("TM_MODE must be baseline or pinned, got ", sQuote(mode), call. = FALSE)',
  'lib <- Sys.getenv("TM_LIB")',
  'pins <- Sys.getenv("TM_PINS")',
  'if (!nzchar(pins)) stop("TM_PINS did not reach the child", call. = FALSE)',
  'if (identical(mode, "pinned") && !nzchar(lib)) stop("TM_LIB did not reach the child", call. = FALSE)',
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
  'if (identical(mode, "pinned")) {',
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
  '# Three files build a named pipe with no writer and run a program against it,',
  '# expecting the package\'s own limit to kill it. On this runner a blocked',
  '# ffmpeg ignores SIGTERM (measured: survives kill -TERM, dies on kill -KILL)',
  '# and system2(stdout = TRUE, input = , timeout = ) does not escalate: one',
  '# isolated run took 191.8 s against a 2 s limit, and six full-suite runs never',
  '# came back at all. Nothing about a dependency floor is involved -- the',
  '# baseline wedges the same way -- so BOTH runs leave the same three out.',
  '#',
  '# By filter, not by copying the directory: a copy elsewhere on disk changes',
  '# what the doc tests can see (`man/` sits two levels up from the test dir),',
  '# and 15 assertions quietly turned into skips when this was tried that way.',
  'EXCLUDE <- c("test-with-timeout.R", "test-runtime-timeout.R", "test-timeout-silence.R")',
  'all_files <- list.files("tests/testthat", "^test-.*[.]R$")',
  'want <- setdiff(all_files, EXCLUDE)',
  'if (length(want) != length(all_files) - length(EXCLUDE)) {',
  '  stop("the exclusion list names a file the suite does not have", call. = FALSE)',
  '}',
  'cat("  excluded from BOTH runs:", paste(EXCLUDE, collapse = ", "),',
  '    sprintf("(%d of %d files run)\\n", length(want), length(all_files)))',
  'res <- testthat::test_dir("tests/testthat", package = "tidymedia",',
  '                          filter = "with-timeout|runtime-timeout|timeout-silence",',
  '                          invert = TRUE,',
  '                          reporter = "silent", stop_on_failure = FALSE,',
  '                          load_package = "none")',
  'df <- as.data.frame(res)',
  'by_file <- stats::aggregate(cbind(nb = df$nb, failed = df$failed,',
  '                                  skipped = as.integer(df$skipped),',
  '                                  error = as.integer(df$error)),',
  '                            by = list(file = df$file), FUN = sum)',
  'by_file <- by_file[order(by_file$file), ]',
  '# The `error` column is PRINTED, not only summed. It was aggregated here and',
  '# then dropped from every line the run reports, so a transcribed table read',
  '# as a clean pass over a file whose tests had errored -- the child stopped on',
  '# the total either way, but the table quoted afterwards said nothing about it.',
  'cat("\\n  file                                         pass fail  err skip\\n")',
  'for (i in seq_len(nrow(by_file))) {',
  '  r <- by_file[i, ]',
  '  cat(sprintf("  %-44s %4d %4d %4d %4d\\n", r$file, r$nb - r$failed, r$failed,',
  '              r$error, r$skipped))',
  '}',
  '',
  '# Provenance again, after the suite: a pinned package that was resolved',
  '# correctly above but LOADED from elsewhere (a namespace another package',
  '# pulled in first) would otherwise pass the up-front check and still have run',
  '# the wrong code.',
  'if (identical(mode, "pinned")) {',
  '  for (p in intersect(names(pins), loadedNamespaces())) {',
  '    where <- norm(dirname(getNamespaceInfo(p, "path")))',
  '    if (!identical(where, norm(lib))) {',
  '      stop(sprintf("%s was LOADED from %s, not the pinned library", p, where), call. = FALSE)',
  '    }',
  '  }',
  '}',
  '',
  '# What the filter actually selected, checked rather than trusted: an `invert`',
  '# that stopped working would run three files fewer, or three more, in silence.',
  'ran <- sort(unique(by_file$file))',
  'if (!identical(ran, sort(want))) {',
  '  stop(sprintf("ran %d files, expected %d; unexpected: %s; missing: %s",',
  '               length(ran), length(want),',
  '               paste(setdiff(ran, want), collapse = ", "),',
  '               paste(setdiff(want, ran), collapse = ", ")), call. = FALSE)',
  '}',
  'failed <- sum(by_file$failed)',
  'errored <- sum(by_file$error)',
  'cat(sprintf("\\nTOTALS pass=%d fail=%d err=%d skip=%d files=%d\\n",',
  '            sum(by_file$nb) - failed, failed, errored, sum(by_file$skipped), nrow(by_file)))',
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
  # shQuote is load-bearing: system2(env = ) pastes these into a `sh -c` line,
  # so an unquoted `;` inside TM_PINS ends the assignment and starts a new
  # command -- which is exactly how the assertions above came to be skipped.
  env <- c(sprintf("TM_MODE=%s", if (nzchar(lib)) "pinned" else "baseline"),
           sprintf("TM_LIB=%s", shQuote(lib)),
           sprintf("TM_PINS=%s", shQuote(pins_env)),
           "NOT_CRAN=true")
  if (nzchar(lib)) env <- c(env, sprintf("R_LIBS=%s", shQuote(lib)))
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
  stats::setNames(as.list(nums), c("pass", "fail", "err", "skip", "files"))
}

# Everything above this line is definitions. `TM_DEFS_ONLY` stops here, so
# data-raw/floor-probes.R can plant defects against those functions without
# starting a measurement. A signalled condition rather than a `return()`:
# `source()` evaluates top-level expressions one at a time, and there is no
# function here to return from.
if (nzchar(Sys.getenv("TM_DEFS_ONLY"))) {
  stop(structure(class = c("tm_defs_only", "error", "condition"),
                 list(message = "sourced for its definitions only", call = NULL)))
}

# --- drive it ------------------------------------------------------------------

base <- run_child("BASELINE (current dependencies)", "")
if (BASELINE_ONLY) quit(save = "no")

lib <- file.path(LIBROOT, if (is.na(ONLY)) "all" else ONLY)
dir.create(lib, recursive = TRUE, showWarnings = FALSE)

# --- what the environment requires of the pinned floors -------------------------

cat("\n---- requirements the environment places on the pinned floors ----\n")
rec <- reconcile(pins, runtime_closure(names(pins)), gather_reqs, newest_compatible)
pins <- rec$pins
moves <- rec$moves
holdbacks <- rec$holdbacks
if (!length(moves) && !length(holdbacks)) cat("  none -- every declared floor satisfies the environment as it stands\n")
pins_env <- paste(sprintf("%s=%s", names(pins), unlist(pins)), collapse = ";")
cat("\n---- installing the declared floors into", lib, "----\n")
order <- install_order(pins)
cat("    install order (LinkingTo/Imports among the pinned set first):",
    paste(order, collapse = " -> "), "\n")
failures <- list()
for (p in order) {
  err <- install_pin(lib, p, pins[[p]], pins)
  cat(sprintf("  %-10s %-8s %s\n", p, pins[[p]], if (is.null(err)) "installed" else "FAILED"))
  if (!is.null(err)) {
    cat("      ", err, "\n", sep = "")
    failures[[p]] <- err
  }
}
if (length(failures)) {
  # A floor that does not install here is a floor this run cannot measure, and
  # the errors above are the whole of what the run has to say about it. There
  # is no walk-it-forward mode: choosing a replacement version is a decision
  # about what DESCRIPTION should declare, not something a measurement makes on
  # its own.
  stop(sprintf("%d declared floor(s) do not install here: %s",
               length(failures), paste(names(failures), collapse = ", ")),
       call. = FALSE)
}

if (length(holdbacks)) {
  cat("\n---- holding the test harness back to what the floors permit ----\n")
  for (r in names(holdbacks)) {
    err <- install_pin(lib, r, holdbacks[[r]]$version, pins)
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
cat(sprintf("    baseline  pass=%d fail=%d err=%d skip=%d over %d files\n",
            base$pass, base$fail, base$err, base$skip, base$files))
cat(sprintf("    pinned    pass=%d fail=%d err=%d skip=%d over %d files\n",
            pinned$pass, pinned$fail, pinned$err, pinned$skip, pinned$files))
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
# What passed is what was PINNED, which is the declared set only when nothing
# moved. Saying "the declared floors" after a MOVE would report a version
# nobody declared as evidence for the one that is written down.
if (length(moves)) {
  cat("\n    the pinned floors load and the suite passes on them, but the pinned set\n")
  cat("    is NOT the declared set -- apply the moves above to DESCRIPTION and re-run\n")
} else {
  cat("\n    the declared floors load, and the suite passes on them\n")
}
