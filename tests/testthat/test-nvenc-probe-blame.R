# M095: a wrong argument outranks the nvenc availability probe.
#
# The question every test here asks is one question: does asking for GPU
# encoding change what a caller is told about an argument they got wrong? It
# must not. `hardware = "nvenc"` makes the package ask the local FFmpeg build
# what encoders it has, and three pipelines used to ask BEFORE checking values
# the answer cannot possibly affect -- so a malformed `pixel_format` came back
# as "nvenc encoder is not available".
#
# The sweep enumerates its own domain (`tm_nvenc_wrong_arg_cells()`,
# helper-timeout-sweep.R) rather than naming the verbs: M094's three review
# rounds each found one more instance of this class by hand, which is what a
# hand-list buys.

test_that("the sweep quantifies over a domain it computes, not a list", {
  dir <- withr::local_tempdir()
  cells <- tm_nvenc_wrong_arg_cells(dir)

  # Non-empty, and non-empty for the right reason: the members are exactly the
  # timeout domain's `hardware`-carrying exports. A glob that silently emptied
  # would leave every expectation below vacuously true.
  members <- tm_sort_c(unique(vapply(cells, function(x) x$name, character(1))))
  expect_equal(
    members,
    tm_sort_c(intersect(tm_timeout_domain(), nvenc_hardware_exports()))
  )
  expect_gt(length(members), 0)

  # Every member is crossed with all five wrong forms and with every formal it
  # has except `hardware` (the crossed axis) and `...` (not nameable).
  ns <- asNamespace("tidymedia")
  for (m in members) {
    args <- setdiff(names(formals(get(m, envir = ns))), c("hardware", "..."))
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

test_that("the encoder pool is mocked, and the mock is what the sweep reads", {
  # The instrument's own load-bearing claim. `nvenc_available()` reads the
  # `tidymedia.nvenc_encoders` option seam before it falls through to
  # `cached_encoder_names()`, so a sweep that left that option set would measure
  # the option and never the mock -- and the `absent` pool, the whole reason the
  # defect is visible at all, would silently answer "present".
  dir <- withr::local_tempdir()
  vid <- file.path(dir, "in.mp4")
  file.create(vid)
  cells <- list(probe = list(
    name = "standardize_video", arg = "pixel_format", form = "token",
    args = list(infile = vid, outfile = file.path(dir, "o.mp4"),
                video_codec = "libx264", run = FALSE)
  ))
  absent <- tm_nvenc_sweep(cells, character())
  present <- tm_nvenc_sweep(cells, c("h264_nvenc", "hevc_nvenc", "av1_nvenc"))
  # Same call, two mocked builds, two different answers: the mock is consulted.
  expect_match(absent$nvenc, "not available", fixed = TRUE)
  expect_false(grepl("not available", present$nvenc, fixed = TRUE))
})

test_that("a dropped cell is dropped by measurement, naming its refusing frame", {
  dir <- withr::local_tempdir()
  cells <- tm_nvenc_wrong_arg_cells(dir)
  sweep <- tm_nvenc_sweep(cells, tm_nvenc_encoder_pools()$present)

  expect_equal(nrow(sweep), length(cells))
  expect_gt(sum(sweep$kept), 0)

  dropped <- tm_sort_c(unique(paste0(
    sweep$member[!sweep$kept], "/", sweep$arg[!sweep$kept],
    " -> ", sweep$refused_by[!sweep$kept]
  )))
  expect_equal(dropped, tm_nvenc_dropped_master())

  # Every kept cell really was refused by the member itself -- the property
  # "kept" is defined by, restated here so a change to the predicate cannot
  # quietly widen the set the next test quantifies over.
  kept <- sweep[sweep$kept, ]
  expect_equal(sub(" \\|\\| .*$", "", kept$none), kept$member)
})

test_that("asking for nvenc changes nothing a caller is told about an argument", {
  # AC1. For every kept cell -- every (member, argument, wrong form) the member
  # itself refuses -- the condition under `hardware = "nvenc"` is identical to
  # the one under `hardware = "none"`, blamed frame and message alike, whether
  # the mocked build lists the nvenc encoders or lists none.
  #
  # The `absent` pool is the one that matters and the one a machine cannot be
  # trusted to supply: with the encoders present the probe succeeds and the
  # argument error is reached anyway, so a sweep run only against a real
  # nvenc-capable FFmpeg would measure nothing. `tm_nvenc_mismatch_master()`
  # records the 27 cells that failed this on the merge-base, which is what shows
  # the sweep able to see the defect it now reports absent.
  dir <- withr::local_tempdir()
  cells <- tm_nvenc_wrong_arg_cells(dir)
  expect_gt(length(tm_nvenc_mismatch_master()), 0)

  for (pool in names(tm_nvenc_encoder_pools())) {
    sweep <- tm_nvenc_sweep(cells, tm_nvenc_encoder_pools()[[pool]])
    bad <- sweep[sweep$kept & !sweep$match, ]
    expect_equal(nrow(bad), 0L, info = paste(pool, paste(bad$cell,
                                                         collapse = ", ")))
  }
})

test_that("an invalid session limit does not displace the argument error either", {
  # AC3, the class D074 disclosed as unfixed and M094's review measured. With a
  # `tidymedia.timeout` the option cannot use, `nvenc_available()` refuses the
  # limit -- and it did so before the argument checks the resolution sat above,
  # so a bad `pixel_format` came back as a bad limit. Sinking the resolution
  # fixes both displacements at once, which is why this leg reuses AC1's cells
  # rather than a set of its own.
  #
  # The pool is mocked PRESENT deliberately. Absent, the availability abort
  # would be a second reason the cell could fail and the leg would no longer be
  # about the limit; present is also the only state in which the
  # nvenc-available branch executes at all (the M094 lesson).
  #
  # Measured on the merge-base b538e63: 27 mismatching cells under every one of
  # the five invalid forms, the same 27 AC1 records.
  dir <- withr::local_tempdir()
  cells <- tm_nvenc_wrong_arg_cells(dir)
  pool <- tm_nvenc_encoder_pools()$present

  forms <- tm_timeout_bad_forms()
  expect_gt(length(forms), 0)
  for (nm in names(forms)) {
    sweep <- tm_nvenc_sweep(cells, pool, limit = forms[[nm]])
    expect_gt(sum(sweep$kept), 0)
    bad <- sweep[sweep$kept & !sweep$match, ]
    expect_equal(nrow(bad), 0L,
                 info = paste(nm, paste(bad$cell, collapse = ", ")))
  }
})
