# M096: a wrong argument is refused by the verb, not by `purrr::pmap()` or after
# FFmpeg runs.
#
# The instrument first. M094 corrupted each member's FIRST argument with `123`
# and nothing else, and that shape let two front doors through three review
# rounds: `123` is a number where every member's first argument wants a path, a
# job table or a pipeline, so it only ever exercised the guards at the top of
# each verb. `tm_timeout_corrupt_specs()` now crosses every formal with the five
# wrong forms M095 crossed the nvenc probe with, and the sweep decides which
# cells it can speak for rather than being told.

test_that("the widened sweep quantifies over a domain it computes, not a list", {
  dir <- withr::local_tempdir()
  cells <- tm_timeout_corrupt_specs(dir)

  # Non-empty, and non-empty for the right reason: the members are exactly the
  # timeout domain, all of it. A table that silently shrank -- M094's did, by
  # dropping the three members whose valid cell has no arguments, one of which
  # is the `ffmpeg_codecs()` this milestone exists to guard -- would leave every
  # expectation below vacuously true.
  members <- tm_sort_c(unique(vapply(cells, function(x) x$name, character(1))))
  expect_equal(members, tm_sort_c(tm_timeout_domain()))
  expect_gt(length(members), 0)

  # Every member is crossed with all five wrong forms and with every formal it
  # has except `...`, which is not nameable by a caller.
  ns <- asNamespace("tidymedia")
  for (m in members) {
    args <- setdiff(names(formals(get(m, envir = ns))), "...")
    got <- tm_sort_c(unique(vapply(
      Filter(function(x) identical(x$name, m), cells),
      function(x) x$arg, character(1)
    )))
    expect_equal(got, tm_sort_c(args), info = m)
  }
  forms <- tm_sort_c(unique(vapply(cells, function(x) x$form, character(1))))
  expect_equal(forms, tm_sort_c(names(tm_nvenc_wrong_forms())))
  expect_equal(length(forms), 5L)
})

test_that("the sweep can tell a kept cell from a dropped one", {
  # The discrimination check the census rests on: `kept` has to come from the
  # frame that was blamed and not from the cell being refused at all. Two
  # planted cells, one refused by the member itself and one refused below it,
  # have to come back on opposite sides -- otherwise a census of 456 drops
  # proves nothing.
  dir <- withr::local_tempdir()
  vid <- file.path(dir, "in.mp4")
  file.create(vid)
  planted <- list(
    own = list(
      name = "extract_audio", arg = "infile", form = "number",
      args = list(infile = 123, outfile = file.path(dir, "o.m4a"), run = FALSE)
    ),
    below = list(
      name = "segment_video", arg = "outfiles", form = "number",
      args = list(infile = vid, start = 0, end = 1, outfiles = 123,
                  run = FALSE)
    )
  )
  res <- tm_corrupt_limit_sweep(planted)
  expect_identical(res$kept, c(TRUE, FALSE))
  expect_identical(res$refused_by, c("extract_audio", "purrr::pmap"))
})

test_that("an invalid limit displaces no argument error, at every formal", {
  dir <- withr::local_tempdir()
  cells <- tm_timeout_corrupt_specs(dir)
  res <- tm_corrupt_limit_sweep(cells)
  expect_equal(nrow(res), length(cells))

  # A sweep whose kept set emptied would report zero mismatches for the wrong
  # reason, so the count is asserted before the property is.
  expect_gt(sum(res$kept), 0)

  # AC1. Every kept cell says the same thing -- blamed frame and message both --
  # under all five invalid `tidymedia.timeout` values as it says with no limit
  # set at all. The referent is measured per cell, never recorded, so the leg is
  # indifferent to each verb's own wording.
  expect_equal(res$cell[nzchar(res$mismatch)], character())

  # AC1's other half: the cells the sweep cannot speak for are dropped by that
  # same measurement and named with the frame that refused them, so the classes
  # this milestone leaves open stay visible rather than being quietly absent.
  dropped <- tm_sort_c(unique(paste0(
    res$member[!res$kept], "/", res$arg[!res$kept], " -> ",
    res$refused_by[!res$kept]
  )))
  expect_equal(dropped, tm_corrupt_dropped_master())
})
