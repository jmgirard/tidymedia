# A failed unpack leaves the destination as it found it (M103 AC1/AC2).
#
# Every cell below drives the REAL archive::archive_extract() against a
# committed fixture -- no mock stands between the test and libarchive -- so
# what is asserted is what libarchive actually left on disk and what the
# cleanup actually removed. The two fixtures reach libarchive's two failure
# routes (data-raw/corrupt-archive-fixtures.R): one refused at open, one
# failing mid-read after it has created entries.

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

# The ancestor chain of a relative path, outermost first.
tm_entry_ancestors <- function(path) {
  out <- character(0)
  parent <- dirname(path)
  while (!parent %in% c(".", "/", "")) {
    out <- c(parent, out)
    parent <- dirname(parent)
  }
  out
}

# A destination directory, plus the before-snapshot of it, so a cell says what
# it set up and then asserts against exactly that.
tm_dest <- function() {
  d <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "dest")
  dir.create(d, recursive = TRUE)
  d
}

tm_run_unpack <- function(fixture, dir) {
  tm_unpack(testthat::test_path("fixtures", fixture), dir)
}


test_that("a failed unpack leaves an empty destination empty", {
  # AC1/AC2: the first starting state, both routes.
  for (fixture in c("not-an-archive.7z", "corrupt-payload.7z")) {
    d <- tm_dest()
    out <- tm_run_unpack(fixture, d)

    expect_null(out$files, label = fixture)
    expect_identical(out$leftovers, character(0), label = fixture)
    expect_identical(
      list.files(d, recursive = TRUE, all.files = TRUE, include.dirs = TRUE,
                 no.. = TRUE),
      character(0),
      label = paste(fixture, "destination is empty again")
    )
  }
})


test_that("a failed unpack leaves a file at a path it does not write alone", {
  # AC1/AC2: the third starting state, both routes. The assertion is the
  # criterion's own -- byte-identical, mtime unmoved -- read off the file
  # rather than off the cleanup's own bookkeeping.
  for (fixture in c("not-an-archive.7z", "corrupt-payload.7z")) {
    d <- tm_dest()
    keep <- file.path(d, "keepme.txt")
    writeLines("the caller's own file", keep)
    before_bytes <- readBin(keep, "raw", file.size(keep))
    before_mtime <- file.info(keep)$mtime

    out <- tm_run_unpack(fixture, d)

    expect_null(out$files, label = fixture)
    expect_identical(out$leftovers, character(0), label = fixture)
    expect_true(file.exists(keep), label = fixture)
    expect_identical(
      readBin(keep, "raw", file.size(keep)), before_bytes,
      label = paste(fixture, "kept file is byte-identical")
    )
    expect_identical(
      file.info(keep)$mtime, before_mtime,
      label = paste(fixture, "kept file's mtime is unmoved")
    )
    # Nothing the extraction created survives beside it.
    expect_identical(
      list.files(d, recursive = TRUE, all.files = TRUE, include.dirs = TRUE,
                 no.. = TRUE),
      "keepme.txt",
      label = fixture
    )
  }
})


test_that("a failed unpack removes a file it overwrote and keeps the directory it found", {
  # AC1/AC2: the second starting state -- a file sitting exactly where the
  # fixture writes -- and the fourth, a pre-existing directory the fixture
  # writes INTO holding an entry of the caller's own.
  #
  # The two are one cell because the fixture writes one entry: pre-creating
  # its whole ancestor chain makes every one of those directories
  # pre-existing, which is what puts the amendment's rule on trial. libarchive
  # truncates the colliding file to zero bytes, so the cleanup removes it
  # (D046's created-or-changed rule); the directories and the sibling are
  # never touched.
  fixture <- "corrupt-payload.7z"
  entry <- tm_fixture_entry(fixture)
  expect_false(is.null(entry))

  d <- tm_dest()
  ancestors <- tm_entry_ancestors(entry)
  expect_gt(length(ancestors), 0)
  dir.create(file.path(d, dirname(entry)), recursive = TRUE)
  writeLines("the caller's own bytes", file.path(d, entry))
  sibling <- file.path(d, dirname(entry), "sibling.txt")
  writeLines("a neighbour the extraction never touches", sibling)
  sibling_bytes <- readBin(sibling, "raw", file.size(sibling))
  sibling_mtime <- file.info(sibling)$mtime

  out <- tm_run_unpack(fixture, d)

  expect_null(out$files)
  expect_identical(out$leftovers, character(0))
  # The overwritten file is gone -- the emptied file D046 exists to clear.
  expect_false(file.exists(file.path(d, entry)))
  # The directory the extraction wrote into is NOT: removing it recursively
  # would have taken the sibling with it, which is the whole point.
  for (a in ancestors) {
    expect_true(dir.exists(file.path(d, a)), label = a)
  }
  expect_true(file.exists(sibling))
  expect_identical(readBin(sibling, "raw", file.size(sibling)), sibling_bytes)
  expect_identical(file.info(sibling)$mtime, sibling_mtime)
})


test_that("a failed unpack leaves a nested subdirectory it does not write into alone", {
  # AC1/AC2: the fourth starting state for the route with no readable
  # listing. not-an-archive.7z writes nothing at all, so any nested directory
  # is one it does not write into, and the assertion is that the whole subtree
  # survives untouched.
  d <- tm_dest()
  nested <- file.path(d, "sub", "deeper")
  dir.create(nested, recursive = TRUE)
  writeLines("nested", file.path(nested, "n.txt"))
  before <- tm_dir_snapshot(d)

  out <- tm_run_unpack("not-an-archive.7z", d)

  expect_null(out$files)
  expect_identical(out$leftovers, character(0))
  expect_identical(tm_dir_snapshot(d), before)
})


test_that("an added file that will not delete is reported, not silently dropped", {
  # AC2's first mock cell. The seam is made to fail on the non-recursive call
  # only, and the starting state pre-creates the ancestor chain so the ONLY
  # thing the cleanup targets is the overwritten file: the leftover the test
  # reads back is that file and nothing else.
  fixture <- "corrupt-payload.7z"
  entry <- tm_fixture_entry(fixture)
  expect_false(is.null(entry))

  d <- tm_dest()
  dir.create(file.path(d, dirname(entry)), recursive = TRUE)
  writeLines("the caller's own bytes", file.path(d, entry))

  testthat::local_mocked_bindings(
    tm_unlink = function(path, recursive = FALSE) {
      if (recursive) return(unlink(path, recursive = TRUE, expand = FALSE))
      1L
    }
  )
  out <- tm_run_unpack(fixture, d)

  expect_null(out$files)
  expect_identical(out$leftovers, entry)
  expect_true(file.exists(file.path(d, entry)))
})


test_that("an added directory that will not delete is reported, not silently dropped", {
  # AC2's second mock cell, a different KIND of target rather than a second
  # location for the same one: the recursive removal of a created directory.
  # The control above fails the file call; this one fails the directory call,
  # so between them each branch of the removal is seen to report.
  fixture <- "corrupt-payload.7z"
  entry <- tm_fixture_entry(fixture)
  expect_false(is.null(entry))
  topmost <- tm_entry_ancestors(entry)[[1]]

  d <- tm_dest()
  testthat::local_mocked_bindings(
    tm_unlink = function(path, recursive = FALSE) {
      if (recursive) return(1L)
      unlink(path, expand = FALSE)
    }
  )
  out <- tm_run_unpack(fixture, d)

  expect_null(out$files)
  # The created chain is one target, reported by its topmost directory; the
  # file inside it was removed by the non-recursive call that still works.
  expect_identical(out$leftovers, topmost)
  expect_true(dir.exists(file.path(d, topmost)))
  expect_false(file.exists(file.path(d, entry)))
})


test_that("a succeeding unpack removes nothing and reports no leftovers", {
  # The control the check-discrimination rule asks for: a run where the report
  # must stay silent. Without it, a cleanup that removed everything
  # unconditionally would pass every cell above.
  d <- tm_dest()
  keep <- file.path(d, "keepme.txt")
  writeLines("the caller's own file", keep)

  src <- withr::local_tempdir()
  writeLines(rep("payload", 50), file.path(src, "payload.txt"))
  good <- file.path(withr::local_tempdir(), "good.7z")
  archive::archive_write_files(good, file.path(src, "payload.txt"))

  out <- tm_unpack(good, d)

  expect_false(is.null(out$files))
  expect_identical(out$leftovers, character(0))
  expect_true(file.exists(keep))
  expect_true(any(grepl("payload[.]txt$", out$files)))
})
