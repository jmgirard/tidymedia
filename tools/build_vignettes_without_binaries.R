#!/usr/bin/env Rscript
# Builds every vignette on a PATH that reaches pandoc but reaches none of
# ffmpeg, ffprobe or mediainfo, which is the state a machine without the
# command-line tools is in. A vignette that needs a binary it cannot find fails
# the build, so a clean run here is the evidence that every chunk is guarded.
#
# Run from the package root: Rscript tools/build_vignettes_without_binaries.R
# A developer tool, kept out of the build by `.Rbuildignore`'s `^tools$`.
#
# The reduced PATH is built rather than edited down, because on this machine
# pandoc and ffmpeg live in the same directory (/opt/homebrew/bin), so dropping
# the directory drops pandoc with it and the build would then fail for a reason
# that has nothing to do with the guards. A scratch directory holding a symlink
# to pandoc alone is what separates them.
#
# Each vignette's own setup chunk writes its `Sys.which()` answers to stderr, so
# the log this prints carries, from inside the build, the fact that the three
# programs were not reachable.
#
# `PATH` alone does not make a program unreachable. `find_program()` falls back
# to a location remembered by `set_program()`, under `tools::R_user_dir()` and,
# for a location set before 0.2.0, under `rappdirs::user_config_dir()`. A
# developer machine that has ever run `set_ffmpeg()` still resolves FFmpeg on
# the reduced `PATH` through that file, and the build would then be green
# whether or not the chunks are guarded -- which is the whole thing this script
# exists to show. So the config seam is redirected at empty scratch directories
# and the assertion below is made on `find_*()`, the call the package itself
# makes, rather than on `Sys.which()` alone.

PROGRAMS <- c("ffmpeg", "ffprobe", "mediainfo")

pandoc <- Sys.which("pandoc")
if (!nzchar(pandoc)) stop("pandoc is not on PATH; the build needs it")

shim <- tempfile("pandoc-only-"); dir.create(shim)
file.symlink(pandoc, file.path(shim, "pandoc"))

keep <- c(shim, R.home("bin"), "/usr/bin", "/bin", "/usr/sbin", "/sbin")
keep <- keep[dir.exists(keep)]
keep <- keep[!vapply(keep, function(d) {
  any(file.exists(file.path(d, PROGRAMS)))
}, logical(1))]
path <- paste(keep, collapse = .Platform$path.sep)

old <- Sys.getenv("PATH")
Sys.setenv(PATH = path)
on.exit(Sys.setenv(PATH = old), add = TRUE)

cat("PATH for this build:", path, "\n")
found <- Sys.which(c(PROGRAMS, "pandoc"))
print(found)
if (any(nzchar(found[PROGRAMS]))) {
  stop("the reduced PATH still reaches: ",
       paste(PROGRAMS[nzchar(found[PROGRAMS])], collapse = ", "))
}
if (!nzchar(found[["pandoc"]])) stop("the reduced PATH lost pandoc")

# Redirect both places a remembered location can live at one empty scratch
# directory: `R_USER_CONFIG_DIR` for `tools::R_user_dir()`, `XDG_CONFIG_HOME`
# for the pre-0.2.0 `rappdirs` path. `HOME` is deliberately left alone -- moving
# it would move the user library with it and break the build for an unrelated
# reason -- so the two resolved paths are checked to have actually landed under
# the scratch directory rather than assumed to have.
config <- tempfile("no-binaries-config-"); dir.create(config)
old_env <- Sys.getenv(c("R_USER_CONFIG_DIR", "XDG_CONFIG_HOME"),
                      names = TRUE, unset = NA)
Sys.setenv(R_USER_CONFIG_DIR = config, XDG_CONFIG_HOME = config)
on.exit({
  set <- old_env[!is.na(old_env)]
  if (length(set)) do.call(Sys.setenv, as.list(set))
  unset <- names(old_env)[is.na(old_env)]
  if (length(unset)) Sys.unsetenv(unset)
}, add = TRUE)

seams <- c(
  current = tools::R_user_dir("tidymedia", "config"),
  legacy = path.expand(rappdirs::user_config_dir("tidymedia", "R"))
)
cat("config seam redirected to:", config, "\n")
print(seams)
# Compared as written, with runs of "/" collapsed: the seam directories do not
# exist yet, so there is nothing for normalizePath() to resolve, and on macOS it
# would rewrite the scratch path's /var to /private/var and never match.
collapse_slashes <- function(x) gsub("/+", "/", x)
astray <- seams[!startsWith(collapse_slashes(seams), collapse_slashes(config))]
if (length(astray)) {
  stop("a config seam still resolves outside the scratch directory: ",
       paste(names(astray), unname(astray), sep = "=", collapse = ", "))
}

# Ask the package's own resolver, not `Sys.which()`: this is the call every
# chunk's work goes through, so a NULL from each of the three is what says the
# build below cannot reach them by any route.
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
resolved <- suppressWarnings(list(
  ffmpeg = tidymedia::find_ffmpeg(),
  ffprobe = tidymedia::find_ffprobe(),
  mediainfo = tidymedia::find_mediainfo()
))
for (p in PROGRAMS) {
  cat("find_", p, "(): ", if (is.null(resolved[[p]])) "NULL" else resolved[[p]],
      "\n", sep = "")
}
still <- PROGRAMS[!vapply(resolved[PROGRAMS], is.null, logical(1))]
if (length(still)) {
  stop("find_*() still resolves: ", paste(still, collapse = ", "))
}

cat("\n")
devtools::build_vignettes()
cat("\nvignettes built with none of ", paste(PROGRAMS, collapse = ", "),
    " reachable -- not on PATH and not through a remembered location\n", sep = "")
