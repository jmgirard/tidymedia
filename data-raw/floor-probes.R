# M079 -- plant a defect at each input the floor-measurement scripts
# distinguish, and check that the script REFUSES it rather than returning a
# number nobody measured.
#
# Reproduce (from the package root):
#
#   Rscript data-raw/floor-probes.R            # every probe
#   Rscript data-raw/floor-probes.R --offline  # skip the four that fetch
#
# WHY PROBES AND NOT A READING. Every fault these scripts had been carrying was
# a branch that returned a plausible value on input it could not handle -- a
# size floor that accepted a truncated download, a `sub()` that returned its
# whole input on no match, a network failure that read as "no later versions
# exist". None of them announced itself; each was found by reading the code and
# could be reintroduced by the next edit. So the refusals are exercised.
#
# ONE PROBE PER INPUT CLASS, AND FORM VARIED WITHIN A CLASS. The M52 lesson:
# planting the same defect in several places tests the same branch several
# times. What separates branches here is the FORM of the defect, so the cache
# class carries four -- and the two truncations are not redundant, because a
# gzip truncated PAST the DESCRIPTION entry still lists it and only the exit
# status gives it away. That is the probe that failed against the validator
# this milestone first wrote.
#
# The scripts are sourced with `TM_DEFS_ONLY` set, which stops each of them
# just above its driver, so the functions can be called without starting a
# measurement. Probes that need a DESCRIPTION the repo does not have run the
# real script from a STAGED package root -- a temp directory of symlinks to
# `R/`, `man/` and the rest, with the one modified DESCRIPTION written into it.
# Nothing here writes to the repo.

PKG <- normalizePath(".")
if (!file.exists(file.path(PKG, "DESCRIPTION"))) {
  stop("run this from the tidymedia package root", call. = FALSE)
}
OFFLINE <- "--offline" %in% commandArgs(trailingOnly = TRUE)
SCRATCH <- file.path(tempdir(), "floor-probes")
dir.create(SCRATCH, recursive = TRUE, showWarnings = FALSE)

# --- the verdict table ---------------------------------------------------------

results <- list()
probe <- function(id, what, expect, got) {
  ok <- isTRUE(all.equal(expect, got))
  results[[length(results) + 1L]] <<- list(id = id, what = what, ok = ok,
                                           expect = expect, got = got)
  cat(sprintf("  %-5s %-58s %s\n", id, what, if (ok) "ok" else "FAILED"))
  if (!ok) {
    cat(sprintf("        expected: %s\n        got:      %s\n",
                paste(format(expect), collapse = " "),
                paste(format(got), collapse = " ")))
  }
  invisible(ok)
}

# Did this call refuse, and does the message name the reason? "It errored" is
# not the claim -- a probe that passes on any error would pass on a typo in the
# probe itself.
refused <- function(expr, pattern) {
  msg <- tryCatch({ force(expr); NA_character_ }, error = conditionMessage)
  if (is.na(msg)) return("returned normally")
  if (!grepl(pattern, msg)) return(sprintf("refused, but for another reason: %s", msg))
  "refused"
}

# --- sourcing a script for its definitions only --------------------------------

defs_of <- function(script) {
  env <- new.env(parent = globalenv())
  old <- Sys.getenv("TM_DEFS_ONLY", unset = NA)
  Sys.setenv(TM_DEFS_ONLY = "1")
  on.exit(if (is.na(old)) Sys.unsetenv("TM_DEFS_ONLY") else Sys.setenv(TM_DEFS_ONLY = old))
  hit <- FALSE
  withCallingHandlers(
    tryCatch(sys.source(file.path(PKG, "data-raw", script), envir = env,
                        keep.source = FALSE),
             tm_defs_only = function(c) hit <<- TRUE),
    message = function(m) invokeRestart("muffleMessage")
  )
  if (!hit) {
    stop(sprintf("%s ran past its TM_DEFS_ONLY guard -- the guard is the only thing keeping this harness from starting a measurement",
                 script), call. = FALSE)
  }
  env
}

# --- a package root with one field changed, built from symlinks ----------------

stage_root <- function(description) {
  root <- file.path(SCRATCH, paste0("root-", basename(tempfile(""))))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  for (e in setdiff(list.files(PKG, all.files = TRUE, no.. = TRUE), c("DESCRIPTION", ".git"))) {
    file.symlink(file.path(PKG, e), file.path(root, e))
  }
  writeLines(description, file.path(root, "DESCRIPTION"))
  root
}

# The scripts read their package root from `normalizePath(".")`, so the working
# directory IS the input under test. `Rscript` has no cwd argument, so it is
# set around the call and restored on the way out.
run_script_at <- function(root, script, args = character()) {
  old <- setwd(root)
  on.exit(setwd(old))
  suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                           c(shQuote(file.path(PKG, "data-raw", script)), args),
                           stdout = TRUE, stderr = TRUE))
}

# --- the planted tarballs ------------------------------------------------------
#
# Built here rather than committed: a committed binary fixture would need its
# own provenance record, and these are three `tar` invocations.

make_tarballs <- function() {
  d <- file.path(SCRATCH, "tarballs")
  unlink(d, recursive = TRUE)
  dir.create(file.path(d, "src", "probepkg", "R"), recursive = TRUE)
  writeLines(c("Package: probepkg", "Version: 1.0.0"),
             file.path(d, "src", "probepkg", "DESCRIPTION"))
  # Big and poorly compressible, so a truncation can land well AFTER the
  # DESCRIPTION entry with plenty of archive still missing.
  filler <- function(n) paste0("x", seq_len(n), " <- \"",
                               vapply(seq_len(n), function(i)
                                 paste(sample(letters, 40L, replace = TRUE), collapse = ""), ""),
                               "\"")
  set.seed(79L)
  writeLines(filler(40000L), file.path(d, "src", "probepkg", "R", "big.R"))
  dir.create(file.path(d, "src", "notapkg", "sub"), recursive = TRUE)
  writeLines(filler(40000L), file.path(d, "src", "notapkg", "sub", "y.txt"))

  tarup <- function(dir, out) {
    st <- system2("tar", c("-czf", shQuote(out), "-C", shQuote(file.path(d, "src")),
                           shQuote(dir)),
                  env = "COPYFILE_DISABLE=1", stdout = TRUE, stderr = TRUE)
    if (!file.exists(out)) stop("could not build the probe tarball ", out, call. = FALSE)
    out
  }
  good <- tarup("probepkg", file.path(d, "good.tar.gz"))
  nodesc <- tarup("notapkg", file.path(d, "nodesc.tar.gz"))

  raw <- readBin(good, "raw", file.size(good))
  # EARLY truncation: `tar` cannot read even the first entry.
  writeBin(raw[seq_len(4000L)], file.path(d, "trunc-early.tar.gz"))
  # LATE truncation: the DESCRIPTION entry is read and listed, and `tar` reports
  # the missing tail only by exiting non-zero. This is the form a listing check
  # alone does not catch.
  late <- file.path(d, "trunc-late.tar.gz")
  writeBin(raw[seq_len(as.integer(length(raw) * 0.9))], late)
  # An HTTP error body, over the 1000-byte size floor the fetch used to trust.
  writeLines(rep("<html><body>404 Not Found</body></html>", 60L),
             file.path(d, "httperr.tar.gz"))
  # The late-truncation fixture only tests what it is for if `tar` still lists
  # DESCRIPTION out of it. A fixture that truncated too early would pass the
  # validator for the wrong reason and quietly stop testing the exit status.
  listed <- suppressWarnings(tryCatch(utils::untar(late, list = TRUE), error = function(e) character()))
  if (!any(basename(listed) == "DESCRIPTION")) {
    stop("the late-truncation fixture no longer lists DESCRIPTION, so it is not testing the exit-status branch",
         call. = FALSE)
  }
  list(good = good, nodesc = nodesc,
       trunc_early = file.path(d, "trunc-early.tar.gz"),
       trunc_late = late,
       httperr = file.path(d, "httperr.tar.gz"))
}

TB <- make_tarballs()

# ===============================================================================
cat("\n== AC1: a cached file that is not a package tarball ==\n")
# ===============================================================================

imports <- defs_of("imports-floors.R")
withr_env <- defs_of("withr-floor.R")
rfloor <- defs_of("r-floor.R")

for (nm in c("imports-floors.R", "withr-floor.R", "r-floor.R")) {
  env <- list("imports-floors.R" = imports, "withr-floor.R" = withr_env,
              "r-floor.R" = rfloor)[[nm]]
  v <- env$is_package_tarball
  probe(paste0("A1-", substr(nm, 1, 3)), sprintf("%s: a real package tarball is accepted", nm),
        TRUE, v(TB$good))
  probe(paste0("A2-", substr(nm, 1, 3)), sprintf("%s: gzip truncated before DESCRIPTION", nm),
        FALSE, v(TB$trunc_early))
  probe(paste0("A3-", substr(nm, 1, 3)), sprintf("%s: gzip truncated AFTER DESCRIPTION", nm),
        FALSE, v(TB$trunc_late))
  probe(paste0("A4-", substr(nm, 1, 3)), sprintf("%s: an HTTP error body over the size floor", nm),
        FALSE, v(TB$httperr))
  probe(paste0("A5-", substr(nm, 1, 3)), sprintf("%s: a tarball carrying no DESCRIPTION", nm),
        FALSE, v(TB$nodesc))
}

# The listing check the download branch had always run, applied to the same five
# inputs. It is here to show WHY the exit status is read as well: this is the
# probe that separates the validator from the one this milestone first wrote.
listing_only <- function(tgz) {
  if (!file.exists(tgz) || file.size(tgz) <= 1000L) return(FALSE)
  inside <- tryCatch(suppressWarnings(utils::untar(tgz, list = TRUE)),
                     error = function(e) NULL)
  if (is.null(inside)) return(FALSE)
  any(basename(inside) == "DESCRIPTION")
}
probe("A6", "a listing-only check WOULD accept the late truncation", TRUE,
      listing_only(TB$trunc_late))

# ===============================================================================
if (!OFFLINE) {
cat("\n== AC1: the cache branch refuses and refetches ==\n")
# ===============================================================================
# `rappdirs` 0.3.3 is a declared floor and a small download. The defect is
# planted AT THE CACHE PATH the fetch would reuse.

for (form in c("trunc_early", "trunc_late", "httperr", "nodesc")) {
  # A FRESH source per form, with `SCRATCH` overridden in the script's own
  # environment: every function the script defines closes over that environment,
  # so the cache path they all compute is the planted one.
  e <- defs_of("imports-floors.R")
  e$SCRATCH <- file.path(SCRATCH, paste0("cache-", form))
  dir.create(e$SCRATCH, recursive = TRUE, showWarnings = FALSE)
  planted <- file.path(e$SCRATCH, "rappdirs_0.3.3.tar.gz")
  file.copy(TB[[form]], planted)
  before <- tools::md5sum(planted)[[1]]
  got <- suppressWarnings(e$fetch_tarball("rappdirs", "0.3.3"))
  probe(paste0("B", match(form, c("trunc_early", "trunc_late", "httperr", "nodesc"))),
        sprintf("planted %s at the cache path is refused and refetched", form),
        TRUE,
        identical(got, planted) && e$is_package_tarball(got) &&
          !identical(tools::md5sum(got)[[1]], before))
}
}

# ===============================================================================
cat("\n== AC2: reusing an installed library entry ==\n")
# ===============================================================================
# A fake library, so the guard is exercised without half an hour of compiling.
# `linkingto_of` is overridden to say `archive` LinkingTo-depends on `cli`,
# which is what it does say -- reading it for real would need the tarball.

fake_lib <- function(ver, linked) {
  lib <- file.path(SCRATCH, paste0("lib-", basename(tempfile(""))))
  dir.create(file.path(lib, "archive"), recursive = TRUE)
  dir.create(file.path(lib, "cli"), recursive = TRUE)
  writeLines(c("Package: archive", paste("Version:", ver)),
             file.path(lib, "archive", "DESCRIPTION"))
  writeLines(c("Package: cli", "Version: 3.4.0"), file.path(lib, "cli", "DESCRIPTION"))
  if (!is.null(linked)) {
    writeLines(c("Package: archive", paste("Version:", ver),
                 paste("LinkedAgainst:", linked)),
               file.path(lib, "archive", imports$PIN_STAMP))
  }
  lib
}
reuse_env <- defs_of("imports-floors.R")
# `archive` does LinkingTo-depend on `cli`; reading that for real would need the
# tarball, and what is under test here is the guard, not the read.
reuse_env$linkingto_of <- function(pkg, ver) "cli"
PINS <- list(archive = "1.1.1", cli = "3.4.0")
can <- function(lib) reuse_env$can_reuse(lib, "archive", "1.1.1", PINS)

probe("C1", "right Version, stamp matches the installed cli -> reuse", TRUE,
      can(fake_lib("1.1.1", "cli=3.4.0")))
probe("C2", "right Version, stamp names an OLDER cli -> reinstall", FALSE,
      can(fake_lib("1.1.1", "cli=3.3.0")))
probe("C3", "right Version, no stamp at all (a pre-M079 library) -> reinstall", FALSE,
      can(fake_lib("1.1.1", NULL)))
probe("C4", "wrong Version -> reinstall", FALSE,
      can(fake_lib("1.0.0", "cli=3.4.0")))

# ===============================================================================
if (!OFFLINE) {
cat("\n== AC2: a library root with a ~ and a space ==\n")
# ===============================================================================
# M077 F17 left this unverified: the path reaches `R CMD INSTALL -l` inside
# single quotes, where a shell does not expand `~`.

tilde_root <- "~/tm floor probe/lib"
unlink(path.expand(dirname(tilde_root)), recursive = TRUE)
dir.create(path.expand(tilde_root), recursive = TRUE, showWarnings = FALSE)
e <- defs_of("imports-floors.R")
e$SCRATCH <- file.path(SCRATCH, "tilde")
dir.create(e$SCRATCH, recursive = TRUE, showWarnings = FALSE)
err <- e$install_pin(path.expand(tilde_root), "rappdirs", "0.3.3", list(rappdirs = "0.3.3"))
probe("D1", "install into a ~-and-space library root reports no error", NULL, err)
probe("D2", "and the entry is there, with its linkage stamp", TRUE,
      file.exists(file.path(path.expand(tilde_root), "rappdirs", "DESCRIPTION")) &&
        file.exists(file.path(path.expand(tilde_root), "rappdirs", imports$PIN_STAMP)))
probe("D3", "a second call reuses it rather than reinstalling", NULL,
      e$install_pin(path.expand(tilde_root), "rappdirs", "0.3.3", list(rappdirs = "0.3.3")))
unlink(path.expand(dirname(tilde_root)), recursive = TRUE)
}

# ===============================================================================
cat("\n== AC3: a DESCRIPTION that cannot be read ==\n")
# ===============================================================================

desc_with <- function(field, value) {
  d <- read.dcf(file.path(PKG, "DESCRIPTION"))
  d[1, field] <- value
  f <- file.path(SCRATCH, paste0("desc-", basename(tempfile(""))))
  write.dcf(d, f)
  f
}
r_floor_of <- rfloor$r_floor_of
probe("E1", "Depends: R (>= 4.0.0) reads as 4.0.0", "4.0.0",
      r_floor_of(desc_with("Depends", "R (>= 4.0.0)")))
probe("E2", "Depends: R (> 4.0) is refused, not read as 'none declared'", "refused",
      refused(r_floor_of(desc_with("Depends", "R (> 4.0)")), "not a `>=` floor"))
probe("E3", "Depends: R (== 4.1.0) is refused", "refused",
      refused(r_floor_of(desc_with("Depends", "R (== 4.1.0)")), "not a `>=` floor"))
probe("E4", "a package name ending in R does not become the R floor", "3.1.0",
      r_floor_of(desc_with("Depends", "DoseFindingR (>= 2.0), R (>= 3.1.0)")))

# withr-floor.R reads the floor in its driver, so the probe runs the script from
# a staged root whose Imports has lost the `(>= )`.
staged <- stage_root(readLines(desc_with("Imports", "cli (>= 3.4.0), withr")))
out <- run_script_at(staged, "withr-floor.R")
probe("E5", "withr-floor.R refuses an Imports with no `withr (>= )`", TRUE,
      any(grepl("declares no `withr \\(>= ...\\)` floor", out, fixed = FALSE)))

# The unversioned carve-out, in both scripts, against an unversioned MASS --
# a `recommended` package, so `priority = c("base", "recommended")` waved it
# through and `UNVERSIONED_OK` does not.
d <- read.dcf(file.path(PKG, "DESCRIPTION"))
d[1, "Imports"] <- paste0(d[1, "Imports"], ",\n    MASS")
f <- file.path(SCRATCH, "desc-mass"); write.dcf(d, f)
staged_mass <- stage_root(readLines(f))
for (sc in c("r-floor.R", "imports-floors.R")) {
  out <- run_script_at(staged_mass, sc)
  probe(paste0("E6-", substr(sc, 1, 3)),
        sprintf("%s refuses an unversioned MASS instead of skipping it", sc), TRUE,
        any(grepl("MASS.*declares no version", out)))
}

# ===============================================================================
cat("\n== AC4: a failed fetch is not 'nothing there' ==\n")
# ===============================================================================

av <- defs_of("imports-floors.R")
av$readLines <- function(...) stop("simulated network failure")
probe("F1", "a failed Archive listing is refused, not read as 'no versions'", "refused",
      refused(av$archive_versions("cli", "0.0.0"), "could not read cli's CRAN Archive listing"))

av2 <- defs_of("imports-floors.R")
av2$readLines <- function(...) character()
av2$cran_db <- function() matrix(character(), nrow = 0, ncol = 1,
                                 dimnames = list(NULL, "Version"))
probe("F2", "an empty CRAN database is refused, not read as 'no versions'", "refused",
      refused(av2$archive_versions("cli", "0.0.0"), "came back empty"))

av3 <- defs_of("imports-floors.R")
av3$readLines <- function(...) character()
av3$cran_db <- function() stop("simulated repository failure")
probe("F3", "a failed available.packages() is refused", "refused",
      refused(av3$archive_versions("cli", "0.0.0"), "could not fetch the CRAN package database"))

# `timeout-bound.R`'s summary column.
tb <- defs_of("timeout-bound.R")
probe("F4", "the summary reads the case's own `observed elapsed`", "42.03",
      tb$observed_elapsed(c("limit set:             2 s",
                            "observed elapsed:      42.03 s",
                            "driver wall clock:     44.25 s"), "A1"))
probe("F5", "a case that never reported one is not given a number", "(none)",
      tb$observed_elapsed(c("limit set:             2 s"), "A1"))

# ===============================================================================
cat("\n== AC5: who may be held back, and settling ==\n")
# ===============================================================================

reqs <- function(...) {
  rows <- list(...)
  if (!length(rows)) {
    return(data.frame(requirer = character(), required = character(),
                      version = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, lapply(rows, function(r)
    data.frame(requirer = r[[1]], required = r[[2]], version = r[[3]],
               stringsAsFactors = FALSE)))
}
PINS5 <- list(cli = "3.4.0", rlang = "1.2.0")
rec <- imports$reconcile
pick <- function(pkg, pins) "0.0.1"

probe("G1", "nothing to reconcile settles in one round",
      list(pins = PINS5, moves = character(), holdbacks = list()),
      rec(PINS5, character(), function(p) reqs(), pick))

probe("G2", "a named harness package is held back", c("testthat"),
      names(rec(PINS5, character(),
                function(p) reqs(list("testthat", "rlang", "9.9.9")), pick)$holdbacks))

probe("G3", "a requirer outside the named set stops the run", "refused",
      refused(rec(PINS5, character(),
                  function(p) reqs(list("someRandomPkg", "rlang", "9.9.9")), pick),
              "not one of the harness packages this run may hold back"))

# A gather that keeps demanding a higher `cli` than whatever it was just given:
# the floor moves every round and the loop never settles.
never <- function(pins) reqs(list("dplyr", "cli", paste0(pins$cli, ".1")))
probe("G4", "rounds that never settle stop the run", "refused",
      refused(rec(PINS5, "dplyr", never, pick), "did not reconcile in 5 rounds"))

# ===============================================================================
cat("\n== AC6: the deleted modes ==\n")
# ===============================================================================

hits <- suppressWarnings(system2("grep", c("-rn", "-e", shQuote("[-][-]repair"),
                                           "-e", shQuote("[-][-]walk"),
                                           shQuote(file.path(PKG, "data-raw"))),
                                 stdout = TRUE, stderr = FALSE))
# The label deliberately does not spell the two flags: the criterion is a grep
# over data-raw/, and a probe that named them would be its own only match.
probe("H1", "neither deleted mode's flag appears anywhere under data-raw/", 0L, length(hits))
probe("H2", "--only still refuses a name that is not a versioned Imports entry", TRUE,
      any(grepl("is not a versioned Imports entry",
                run_script_at(PKG, "imports-floors.R", c("--only", "nosuch")))))

# ===============================================================================

cat("\n=================================================================\n")
failed <- Filter(function(r) !r$ok, results)
cat(sprintf("%d probe(s), %d failed\n", length(results), length(failed)))
if (length(failed)) {
  for (r in failed) cat(sprintf("  FAILED %-5s %s\n", r$id, r$what))
  stop(sprintf("%d probe(s) failed", length(failed)), call. = FALSE)
}
cat("every planted defect was refused\n")
