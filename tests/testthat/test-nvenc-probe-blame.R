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

test_that("the wrong-argument cells the probe outranks are the recorded ones", {
  # The BEFORE measurement, kept green on master so the instrument is shown able
  # to see the defect before the fix removes it. T3 replaces this expectation
  # with the zero-mismatch one AC1 states.
  dir <- withr::local_tempdir()
  cells <- tm_nvenc_wrong_arg_cells(dir)

  present <- tm_nvenc_sweep(cells, tm_nvenc_encoder_pools()$present)
  expect_equal(sum(present$kept & !present$match), 0L)

  absent <- tm_nvenc_sweep(cells, tm_nvenc_encoder_pools()$absent)
  expect_equal(
    tm_sort_c(absent$cell[absent$kept & !absent$match]),
    tm_nvenc_mismatch_master()
  )
})
