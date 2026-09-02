# M102 -- the two corrupt-archive fixtures AC3 names, and the generator that
# makes them from scratch.
#
# Reproduce (from the package root):
#
#   Rscript data-raw/corrupt-archive-fixtures.R
#
# PROVENANCE. Neither fixture is downloaded from anywhere. Both are built here
# from a payload this script generates under `set.seed(102)`, so the committed
# bytes are reproducible from this file alone and carry no third-party content.
# The starting point is a real 7z archive written by `archive::archive_write_files()`
# (libarchive), holding one text file; the two fixtures are that archive damaged
# in the two different places libarchive fails at.
#
# WHY TWO. `install_on_win()` must turn any extraction failure into one classed
# refusal, and libarchive reaches its failure by two different routes:
#
#   * `not-an-archive.7z` -- bytes that are not an archive at all. libarchive
#     fails while OPENING, in `archive_read_open1()`, before it has read a
#     single entry ("Unrecognized archive format").
#   * `corrupt-payload.7z` -- a well-formed 7z signature header followed by a
#     packed stream that has been zeroed. libarchive opens the archive and
#     reads its entry, then fails while DECOMPRESSING, in
#     `archive_read_data_block()` ("Decompression failed").
#
# A single fixture would leave the other route untested, and the two routes
# differ in how far the extraction gets before it gives up -- which is exactly
# what the temp-file cleanup and the classed refusal have to survive.
#
# THE SELF-CHECK. Committing a "corrupt" archive that libarchive happily reads
# would leave AC3's tests green over nothing, so this script refuses to write a
# fixture it has not just watched fail, and refuses to run at all if the
# undamaged archive does not extract. The undamaged archive is the control:
# it is what says the damage, and not the build, is why the other two fail.

set.seed(102)

pkg <- normalizePath(".")
if (!file.exists(file.path(pkg, "DESCRIPTION"))) {
  stop("run this from the tidymedia package root", call. = FALSE)
}
dest <- file.path(pkg, "tests", "testthat", "fixtures")
dir.create(dest, recursive = TRUE, showWarnings = FALSE)

# --- the undamaged archive -----------------------------------------------------

# Compressible, deterministic, and big enough that the packed stream is a
# region of its own rather than a handful of bytes tangled up with the header.
src <- file.path(tempdir(), "m102-payload")
dir.create(src, showWarnings = FALSE)
writeLines(
  vapply(1:400, function(i) paste(sample(letters, 40, replace = TRUE), collapse = ""), character(1)),
  file.path(src, "payload.txt")
)
good <- file.path(tempdir(), "m102-good.7z")
unlink(good)
archive::archive_write_files(good, file.path(src, "payload.txt"))
raw_good <- readBin(good, "raw", file.size(good))

extract_error <- function(path) {
  out <- file.path(tempdir(), paste0("m102-out-", basename(path)))
  unlink(out, recursive = TRUE)
  dir.create(out, recursive = TRUE)
  tryCatch(
    {
      archive::archive_extract(path, dir = out)
      NA_character_
    },
    error = function(cnd) conditionMessage(cnd)
  )
}

if (!is.na(extract_error(good))) {
  stop("the undamaged control archive does not extract -- the fixtures below ",
       "would prove nothing about the damage", call. = FALSE)
}

# --- fixture 1: not an archive at all -------------------------------------------

not_archive <- file.path(dest, "not-an-archive.7z")
writeBin(
  charToRaw("tidymedia M102 fixture: these bytes are not an archive.\n"),
  not_archive
)

# --- fixture 2: a well-formed 7z header over a corrupt payload -------------------

# The 7z signature header is the first 32 bytes; the packed stream starts at 33.
# Zeroing a run inside it leaves the header libarchive validates on open intact,
# so the failure lands in the decompressor rather than in the opener.
corrupt <- raw_good
corrupt[33:96] <- as.raw(0x00)
corrupt_payload <- file.path(dest, "corrupt-payload.7z")
writeBin(corrupt, corrupt_payload)

# --- the self-check ------------------------------------------------------------

for (f in c(not_archive, corrupt_payload)) {
  msg <- extract_error(f)
  if (is.na(msg)) {
    unlink(f)
    stop("fixture ", basename(f), " extracted cleanly -- not committing an ",
         "archive that AC3's tests could pass over", call. = FALSE)
  }
  cat(sprintf("%-22s %d bytes  ->  %s\n", basename(f), file.size(f), msg))
}
