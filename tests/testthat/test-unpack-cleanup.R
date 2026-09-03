# A failed unpack leaves the destination as it found it (M103 AC1/AC2).
#
# Every cell below drives the REAL archive::archive_extract() against a
# committed fixture -- no mock stands between the test and libarchive -- so
# what is asserted is what libarchive actually left on disk and what the
# cleanup actually removed. The two fixtures reach libarchive's two failure
# routes (data-raw/corrupt-archive-fixtures.R): one refused at open, one
# failing mid-read after it has created entries.
#
# AC1 promises removal as BEST-EFFORT, and the assertions here are written to
# that promise rather than past it. Windows will not delete a file another
# handle still holds, and the handle the failed extraction wrote through is
# libarchive's own -- not an R connection, and freed by neither `gc()` nor
# waiting, both measured on the windows-latest CI leg at M103. So the
# everywhere-assertion is "gone, or named as a leftover that really is still
# there, and nothing the caller had is touched"; the stronger "gone, and the
# leftover list is empty" is asserted on the platforms that can keep it.

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

tm_dest <- function() {
  d <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "dest")
  dir.create(d, recursive = TRUE)
  d
}

tm_run_unpack <- function(fixture, dir) {
  tm_unpack(testthat::test_path("fixtures", fixture), dir)
}

tm_entries <- function(dir) {
  list.files(
    dir,
    recursive = TRUE, all.files = TRUE, include.dirs = TRUE, no.. = TRUE
  )
}

# Whether each of `paths` is named by `leftovers`, or sits under something
# named: naming a created directory names the subtree it still holds.
tm_named_by <- function(paths, leftovers) {
  vapply(
    paths,
    function(p) {
      length(leftovers) > 0L &&
        any(p == leftovers | startsWith(p, paste0(leftovers, "/")))
    },
    logical(1)
  )
}

# The bytes and mtime of a file, for `kept` below.
tm_kept_file <- function(path) {
  list(
    bytes = readBin(path, "raw", file.size(path)),
    mtime = file.info(path)$mtime
  )
}

# AC1's promise, asserted whole.
#
# `kept` is what the TEST put in the destination, as a named list: each name a
# path relative to the destination, each value either NULL for a directory or
# `tm_kept_file()`'s record of what a file held. Both come from the test's own
# setup and from the fixture's own archive listing, never from the cleanup's
# idea of what it added -- an expectation read off the thing under test would
# be blind in exactly the dimension it reads.
tm_expect_left_as_found <- function(dir, out, kept = list(), label = "") {
  expect_null(out$files, label = label)

  # Nothing the caller had is gone, and nothing of it has changed.
  for (rel in names(kept)) {
    path <- file.path(dir, rel)
    expect_true(
      file.exists(path) || dir.exists(path),
      label = paste(label, rel, "survives")
    )
    if (!is.null(kept[[rel]])) {
      expect_identical(
        readBin(path, "raw", file.size(path)), kept[[rel]]$bytes,
        label = paste(label, rel, "is byte-identical")
      )
      expect_identical(
        file.info(path)$mtime, kept[[rel]]$mtime,
        label = paste(label, rel, "has its mtime unmoved")
      )
    }
  }

  # Every leftover the call names really is still there. A report naming
  # something it had in fact deleted would fail here.
  for (rel in out$leftovers) {
    path <- file.path(dir, rel)
    expect_true(
      file.exists(path) || dir.exists(path),
      label = paste(label, "named leftover is really there:", rel)
    )
  }

  # And nothing survives unnamed: everything still in the destination is
  # either something the caller put there or something the call reported.
  # This is the assertion a silent drop fails.
  survivors <- setdiff(tm_entries(dir), names(kept))
  expect_identical(
    survivors[!tm_named_by(survivors, out$leftovers)], character(0),
    label = paste(label, "nothing survives unnamed")
  )

  # Where the platform lets a failed extraction's files be deleted at all,
  # the promise is unconditional: nothing of the extraction's is left, and
  # there is nothing to report.
  if (tm_unpack_deletes_open_files()) {
    expect_identical(
      out$leftovers, character(0),
      label = paste(label, "leftover list is empty")
    )
    expect_identical(
      survivors, character(0),
      label = paste(label, "destination holds only what the caller put there")
    )
  }
}


test_that("a failed unpack leaves an empty destination empty", {
  # AC1/AC2: the first starting state, both routes.
  for (fixture in c("not-an-archive.7z", "corrupt-payload.7z")) {
    d <- tm_dest()
    tm_expect_left_as_found(d, tm_run_unpack(fixture, d), label = fixture)
  }
})


test_that("a failed unpack leaves a file at a path it does not write alone", {
  # AC1/AC2: the third starting state, both routes.
  for (fixture in c("not-an-archive.7z", "corrupt-payload.7z")) {
    d <- tm_dest()
    keep <- file.path(d, "keepme.txt")
    writeLines("the caller's own file", keep)
    kept <- list("keepme.txt" = tm_kept_file(keep))

    tm_expect_left_as_found(d, tm_run_unpack(fixture, d), kept, label = fixture)
  }
})


test_that("a failed unpack removes a file it overwrote and keeps the directory it found", {
  # AC1/AC2: the second starting state -- a file sitting exactly where the
  # fixture writes -- and the fourth, a pre-existing directory the fixture
  # writes INTO holding an entry of the caller's own.
  #
  # The two are one cell because the fixture writes one entry: pre-creating
  # its whole ancestor chain makes every one of those directories
  # pre-existing, which is what puts the amendment's rule on trial. The
  # directories and the sibling are never touched, on any platform -- a
  # recursive removal of a directory the extraction merely wrote into would
  # have taken the sibling with it, and that is what this cell exists to
  # catch.
  fixture <- "corrupt-payload.7z"
  entry <- tm_fixture_entry(fixture)
  expect_false(is.null(entry))

  d <- tm_dest()
  ancestors <- tm_entry_ancestors(entry)
  expect_gt(length(ancestors), 0)
  dir.create(file.path(d, dirname(entry)), recursive = TRUE)
  writeLines("the caller's own bytes", file.path(d, entry))
  sibling_rel <- paste0(dirname(entry), "/sibling.txt")
  writeLines("a neighbour the extraction never touches", file.path(d, sibling_rel))

  kept <- c(
    stats::setNames(vector("list", length(ancestors)), ancestors),
    stats::setNames(list(tm_kept_file(file.path(d, sibling_rel))), sibling_rel)
  )
  out <- tm_run_unpack(fixture, d)
  tm_expect_left_as_found(d, out, kept)

  # The overwritten file is the one entry at issue: gone, or named. Its
  # ancestors are pre-existing, so `kept` above already required every one of
  # them to survive whichever way that went.
  expect_true(
    !file.exists(file.path(d, entry)) || identical(out$leftovers, entry)
  )
})


test_that("a failed unpack leaves a nested subdirectory it does not write into alone", {
  # AC1/AC2: the fourth starting state for the route with no readable
  # listing. not-an-archive.7z writes nothing at all, so the whole subtree
  # survives untouched on every platform and there is nothing to report.
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
  # reads back is that file and nothing else. The mock decides the outcome, so
  # this cell reads the same on every platform.
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
  # The cell above fails the file call; this one fails the directory call, so
  # between them each branch of the removal is seen to report.
  #
  # The file call is left real here, which is why the file's own fate is
  # asserted only where the platform can delete it: elsewhere it is reported
  # alongside the directory rather than instead of it, and the whole-promise
  # check below covers that.
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
  # The created chain survives the failed recursive call and is reported by
  # its topmost directory, on every platform.
  expect_true(topmost %in% out$leftovers)
  expect_true(dir.exists(file.path(d, topmost)))
  for (rel in out$leftovers) {
    expect_true(
      file.exists(file.path(d, rel)) || dir.exists(file.path(d, rel)),
      label = paste("named leftover is really there:", rel)
    )
  }
  if (tm_unpack_deletes_open_files()) {
    expect_identical(out$leftovers, topmost)
    expect_false(file.exists(file.path(d, entry)))
  }
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
