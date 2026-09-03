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
  # AC2's first mock cell: the seam failing on a file the extraction CREATED.
  #
  # The starting state pre-creates the entry's ancestor chain and nothing
  # else. That is what makes the file a created one rather than a removed
  # one's stand-in: with the chain already there the extraction adds no
  # directory, so `added$dirs` is empty and the created file is not swept up
  # by a recursive call on its parent -- it reaches the non-recursive branch,
  # which is the branch this cell fails. Pre-writing the entry as well, which
  # this cell used to do, would have made the seam fail on a file the
  # extraction TRUNCATED; that case is the real-libarchive cell above.
  #
  # The mock decides the outcome, so this cell reads the same on every
  # platform.
  fixture <- "corrupt-payload.7z"
  entry <- tm_fixture_entry(fixture)
  expect_false(is.null(entry))

  d <- tm_dest()
  dir.create(file.path(d, dirname(entry)), recursive = TRUE)
  # The file is the extraction's, not the test's: it is not there beforehand.
  expect_false(file.exists(file.path(d, entry)))

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
  # The entry inside that directory is left where it is rather than deleted
  # one by one: a file under a created directory the recursive call could not
  # remove is already covered by that directory's name, and the path may not
  # resolve where it looks like it does (the symlink cell below). It is
  # reported alongside the directory, so nothing survives unnamed. This is
  # what the cell asserted the other way round before M103 review pass 3.
  expect_true(file.exists(file.path(d, entry)))
  expect_true(entry %in% out$leftovers)
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


# An entry file.info() cannot stat (M103 AC1) --------------------------------

# `file.info()` returns NA for every field of an entry it cannot stat -- a
# broken symlink is the reachable case, measured 2026-09-02 -- and NA is the
# one value a classification has to decide rather than propagate: an NA
# `isdir` used as a subscript selects NA_character_, which is neither a path
# to remove nor a path to report.

test_that("an unstattable entry is classified as an added file, not as NA", {
  # The frame-level cell, which runs on every platform because it needs no
  # symlink: `tm_snapshot_added()` is a pure function of two snapshots, so the
  # NA a broken symlink produces can be handed to it directly.
  before <- data.frame(
    path = "kept.txt", size = 3, mtime = 1, isdir = FALSE,
    stringsAsFactors = FALSE
  )
  after <- data.frame(
    path = c("kept.txt", "unstattable"),
    size = c(3, NA_real_),
    mtime = c(1, NA_real_),
    isdir = c(FALSE, NA),
    stringsAsFactors = FALSE
  )

  added <- tm_snapshot_added(before, after)
  expect_identical(added$files, "unstattable")
  expect_identical(added$dirs, character(0))
})

test_that("an added entry that cannot be statted is removed, not silently dropped", {
  skip_on_os("windows")
  d <- tm_dest()
  before <- tm_dir_snapshot(d)
  expect_true(file.symlink(file.path(d, "nowhere"), file.path(d, "broken")))

  left <- tm_remove_added(d, before, tm_dir_snapshot(d))

  expect_identical(left, character(0))
  expect_identical(tm_entries(d), character(0))
})

test_that("an added entry that cannot be statted and will not delete is reported", {
  # The other half of AC1's "removed or named": with the seam failing, the
  # entry the cleanup could not delete has to come back to the caller.
  skip_on_os("windows")
  d <- tm_dest()
  before <- tm_dir_snapshot(d)
  expect_true(file.symlink(file.path(d, "nowhere"), file.path(d, "broken")))

  testthat::local_mocked_bindings(tm_unlink = function(path, recursive = FALSE) 1L)
  left <- tm_remove_added(d, before, tm_dir_snapshot(d))

  expect_identical(left, "broken")
  expect_identical(tm_entries(d), "broken")
})

test_that("an unstattable entry the caller already had is left alone", {
  # The control: the same NA fields, but present in BOTH snapshots. Nothing
  # this extraction added, so nothing to remove and nothing to report.
  skip_on_os("windows")
  d <- tm_dest()
  expect_true(file.symlink(file.path(d, "nowhere"), file.path(d, "broken")))
  before <- tm_dir_snapshot(d)
  writeLines("added by the extraction", file.path(d, "new.txt"))

  left <- tm_remove_added(d, before, tm_dir_snapshot(d))

  expect_identical(left, character(0))
  expect_identical(tm_entries(d), "broken")
})

# An entry whose TYPE the extraction changed (M103 AC1) ----------------------

# The classification has two subscripts and three possible `isdir` values on
# each side, so the question it has to answer is not "is this path new" but
# "does this path hold, now, something this extraction put here". A path the
# caller held as a FILE and the failed extraction replaced with a DIRECTORY is
# the case that separates the two readings: it is in `before`, so it is not
# new, and its `isdir` is TRUE, so it is not a changed file. Under the first
# reading it lands in neither bucket and is neither removed nor named
# (measured 2026-09-02); under the second it is a directory this extraction
# created, because it did not exist AS A DIRECTORY before.

test_that("a caller's file the extraction replaced with a directory is a created directory", {
  # Frame-level, so it runs on every platform: `tm_snapshot_added()` is a pure
  # function of two snapshots.
  before <- data.frame(
    path = "p", size = 22, mtime = 1, isdir = FALSE,
    stringsAsFactors = FALSE
  )
  after <- data.frame(
    path = c("p", "p/q"),
    size = c(4096, 7),
    mtime = c(2, 2),
    isdir = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  added <- tm_snapshot_added(before, after)
  expect_identical(added$dirs, "p")
})

test_that("a caller's directory the extraction replaced with a file is a changed file", {
  # The mirror case, which the same rule has to get the other way round: the
  # path holds a file now, so it is removed by name, never with a subtree.
  before <- data.frame(
    path = "p", size = 4096, mtime = 1, isdir = TRUE,
    stringsAsFactors = FALSE
  )
  after <- data.frame(
    path = "p", size = 4096, mtime = 1, isdir = FALSE,
    stringsAsFactors = FALSE
  )

  added <- tm_snapshot_added(before, after)
  expect_identical(added$files, "p")
  expect_identical(added$dirs, character(0))
})

test_that("a directory replacing a caller's file is removed, not silently dropped", {
  d <- tm_dest()
  writeLines("the caller's own file", file.path(d, "p"))
  before <- tm_dir_snapshot(d)
  # What a failed extraction does to that path: the file goes, a directory of
  # the same name takes its place, and an entry lands inside it.
  unlink(file.path(d, "p"))
  dir.create(file.path(d, "p"))
  writeLines("written by the extraction", file.path(d, "p", "q"))

  left <- tm_remove_added(d, before, tm_dir_snapshot(d))

  expect_identical(left, character(0))
  expect_identical(tm_entries(d), character(0))
})

test_that("a directory replacing a caller's file that will not delete is reported", {
  # The other half of AC1's "removed or named".
  d <- tm_dest()
  writeLines("the caller's own file", file.path(d, "p"))
  before <- tm_dir_snapshot(d)
  unlink(file.path(d, "p"))
  dir.create(file.path(d, "p"))
  writeLines("written by the extraction", file.path(d, "p", "q"))

  testthat::local_mocked_bindings(tm_unlink = function(path, recursive = FALSE) 1L)
  left <- tm_remove_added(d, before, tm_dir_snapshot(d))

  # Both, because the removal targeted both: the directory it created at that
  # path and the file it wrote inside it.
  expect_identical(left, c("p", "p/q"))
})

# The classification is total (M103 AC2) -------------------------------------

# AC1 has now been returned twice on the same shape -- an entry the
# classification sorted into neither bucket was neither removed nor named --
# and each time the repair closed one case. This cell asserts the property
# instead: over a snapshot pair holding every combination of (in `before` or
# not) x (`isdir` FALSE, TRUE or NA on each side) x (moved or not), every
# entry the comparison shows this extraction added is reachable by the
# removal, and every entry it does not is not. `added` is computed here from
# AC1's own words rather than from the function under test: new, or changed,
# except a pre-existing directory that is still a directory, which AC1 exempts
# by name because its mtime moves the instant a child lands in it.

test_that("every entry the comparison shows added is reachable, and no other is", {
  rows <- list(
    # name                     before                      after
    list("new-file",           NULL,                       c(7, 2, FALSE)),
    list("new-dir",            NULL,                       c(4096, 2, TRUE)),
    list("new-unstattable",    NULL,                       c(NA, NA, NA)),
    list("kept-file",          c(3, 1, FALSE),             c(3, 1, FALSE)),
    list("truncated-file",     c(22, 1, FALSE),            c(0, 2, FALSE)),
    list("file-now-dir",       c(22, 1, FALSE),            c(4096, 2, TRUE)),
    list("dir-now-file",       c(4096, 1, TRUE),           c(4096, 1, FALSE)),
    list("kept-dir",           c(4096, 1, TRUE),           c(4096, 1, TRUE)),
    list("touched-dir",        c(4096, 1, TRUE),           c(4096, 2, TRUE)),
    list("kept-unstattable",   c(NA, NA, NA),              c(NA, NA, NA)),
    list("file-now-unstat",    c(22, 1, FALSE),            c(NA, NA, NA))
  )
  frame <- function(which) {
    keep <- Filter(function(r) !is.null(r[[which]]), rows)
    data.frame(
      path = vapply(keep, `[[`, character(1), 1L),
      size = as.numeric(vapply(keep, function(r) r[[which]][1], numeric(1))),
      mtime = as.numeric(vapply(keep, function(r) r[[which]][2], numeric(1))),
      isdir = as.logical(vapply(keep, function(r) r[[which]][3], numeric(1))),
      stringsAsFactors = FALSE
    )
  }
  before <- frame(2L)
  after <- frame(3L)

  # AC1's own definition of added, written out independently of the code.
  same <- function(x, y) if (is.na(x) && is.na(y)) TRUE else !is.na(x) &&
    !is.na(y) && identical(x, y)
  expected_added <- vapply(seq_len(nrow(after)), function(i) {
    j <- match(after$path[i], before$path)
    if (is.na(j)) return(TRUE)
    was_dir <- isTRUE(before$isdir[j])
    is_dir <- isTRUE(after$isdir[i])
    if (was_dir && is_dir) return(FALSE)
    !(same(after$size[i], before$size[j]) &&
        same(after$mtime[i], before$mtime[j]) &&
        same(after$isdir[i], before$isdir[j]))
  }, logical(1))

  added <- tm_snapshot_added(before, after)
  reachable <- after$path %in% c(added$files, added$dirs) |
    tm_named_by(after$path, added$dirs)

  expect_identical(
    sort(after$path[reachable]), sort(after$path[expected_added])
  )
})


# The cleanup never reaches outside the destination (M103 AC1) ---------------

# `list.files(recursive = TRUE)` descends THROUGH a directory symlink, so a
# snapshot path is not guaranteed to name something under the destination: a
# symlink the extraction creates reads as a created directory whose children
# are the link target's. The recursive `unlink()` removes the LINK, so on a
# platform that can delete it there is nothing left to walk -- but the
# best-effort design means that call is allowed to fail, and the file loop
# then walked paths resolving outside the destination entirely (M103 review
# pass 3).

test_that("a file under a created directory that would not delete is left alone", {
  # Portable, and the general form of the symlink case below: whatever the
  # entry is, a file under a created directory the recursive call could not
  # remove is not removed one by one. It is already covered by that
  # directory's name in the report, which is D082's bound.
  d <- tm_dest()
  before <- tm_dir_snapshot(d)
  dir.create(file.path(d, "made", "deeper"), recursive = TRUE)
  writeLines("the extraction's", file.path(d, "made", "deeper", "f.txt"))
  after <- tm_dir_snapshot(d)

  left <- testthat::with_mocked_bindings(
    tm_remove_added(d, before, after),
    tm_unlink = function(path, recursive = FALSE) {
      if (recursive) return(1L)
      unlink(path, expand = FALSE)
    }
  )

  expect_true("made" %in% left)
  expect_true(dir.exists(file.path(d, "made", "deeper")))
  expect_true(file.exists(file.path(d, "made", "deeper", "f.txt")))
  # Nothing survives unnamed: the file sits under a directory that is named.
  survivors <- tm_entries(d)
  expect_identical(survivors[!tm_named_by(survivors, left)], character(0))
})

test_that("the cleanup never deletes through a directory symlink it created", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  outside <- file.path(root, "outside")
  dir.create(outside)
  precious <- file.path(outside, "precious.txt")
  writeLines("the caller's own data, outside the install directory", precious)

  d <- file.path(root, "dest")
  dir.create(d)
  before <- tm_dir_snapshot(d)
  expect_true(file.symlink(outside, file.path(d, "link")))
  after <- tm_dir_snapshot(d)
  # The walk really does descend through the link -- without this the cell
  # would pass for the wrong reason.
  expect_true("link/precious.txt" %in% after$path)

  left <- testthat::with_mocked_bindings(
    tm_remove_added(d, before, after),
    tm_unlink = function(path, recursive = FALSE) {
      if (recursive) return(1L)
      unlink(path, expand = FALSE)
    }
  )

  # The file outside the destination is untouched. It is reported by the name
  # it has UNDER the destination, alongside the link itself, so nothing the
  # cleanup targeted goes unnamed.
  expect_true(file.exists(precious))
  expect_setequal(left, c("link", "link/precious.txt"))
  expect_true(file.exists(file.path(d, "link")))
})

test_that("a removed directory takes its own children with it, seam working", {
  # The control for the two cells above: with the removal working, the same
  # created chain goes entirely and there is nothing to report. Without this,
  # a cleanup that had stopped removing anything at all would pass them both.
  d <- tm_dest()
  before <- tm_dir_snapshot(d)
  dir.create(file.path(d, "made", "deeper"), recursive = TRUE)
  writeLines("the extraction's", file.path(d, "made", "deeper", "f.txt"))
  after <- tm_dir_snapshot(d)

  expect_identical(tm_remove_added(d, before, after), character(0))
  expect_identical(tm_entries(d), character(0))
})


# What the caller had and the cleanup removed (M103 AC7) --------------------

test_that("a failed unpack reports the caller's own entries it removed", {
  # D082 removes a pre-existing file the failed extraction wrote over. That
  # is the one deletion a refusal cannot describe as leaving the directory as
  # it found it, so `tm_unpack()` reports it separately from the leftovers.
  fixture <- "corrupt-payload.7z"
  entry <- tm_fixture_entry(fixture)
  expect_false(is.null(entry))

  d <- tm_dest()
  dir.create(file.path(d, dirname(entry)), recursive = TRUE)
  writeLines("the caller's own bytes", file.path(d, entry))

  out <- tm_run_unpack(fixture, d)

  expect_null(out$files)
  if (tm_unpack_deletes_open_files()) {
    expect_identical(out$removed_yours, entry)
    expect_false(file.exists(file.path(d, entry)))
  } else {
    # Where the platform could not delete it, it is a leftover instead --
    # and it is never both.
    expect_identical(out$removed_yours, character(0))
    expect_identical(out$leftovers, entry)
  }
})

test_that("a failed unpack that touched nothing of the caller's reports none", {
  # The control: the same fixture against a destination whose file sits where
  # the archive does not write. Without it, a report that always named the
  # caller's files would pass the cell above.
  d <- tm_dest()
  writeLines("mine", file.path(d, "keepme.txt"))

  out <- tm_run_unpack("corrupt-payload.7z", d)

  expect_identical(out$removed_yours, character(0))
  expect_true(file.exists(file.path(d, "keepme.txt")))
})
