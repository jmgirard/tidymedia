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

cat("\n")
devtools::build_vignettes()
cat("\nvignettes built with none of ", paste(PROGRAMS, collapse = ", "),
    " reachable\n", sep = "")
