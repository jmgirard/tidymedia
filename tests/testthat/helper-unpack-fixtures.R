# Shared by test-unpack-cleanup.R and test-program-management.R: both drive
# the committed corrupt-archive fixtures through the real libarchive, and
# both have to say where a fixture's entry lands and whether this platform
# can delete a file the extraction still holds open (M103).

tm_unpack_deletes_open_files <- function() {
  .Platform$OS.type != "windows"
}

# Where a fixture's entry lands under the destination, relative to it.
#
# Derived from the fixture's own `archive::archive()` listing rather than
# hard-coded: the generator baked its absolute temp path into the entry name,
# so the committed name is machine-specific and `strip_components = 1` -- what
# tm_unpack() passes -- drops only the leading slash of it. Repeated
# separators are collapsed because that is the shape `list.files()` reports
# the same path in, and the two have to compare.
#
# NULL where the fixture has no readable listing: archive::archive() errors
# outright on not-an-archive.7z, which is the same refusal-at-open that makes
# that fixture write nothing.
tm_fixture_entry <- function(fixture) {
  listed <- tryCatch(
    archive::archive(testthat::test_path("fixtures", fixture))$path,
    error = function(cnd) NULL
  )
  if (is.null(listed) || !length(listed)) return(NULL)
  gsub("/+", "/", sub("^/+", "", listed[[1]]))
}
