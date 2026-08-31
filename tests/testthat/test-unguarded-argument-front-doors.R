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
      name = "segment_video", arg = "fallback", form = "number",
      args = list(infile = vid, start = 0, end = 1, fallback = 123,
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

  # `tm_corrupt_dropped_master()` is the MERGE-BASE census, so the live sweep
  # must differ from it by exactly the two cells this milestone's guards closed
  # and by nothing else. Stated as a two-way difference rather than as equality
  # with a second recorded table: this way the record shows what the change did,
  # and a guard that quietly stopped refusing something -- or started refusing
  # something new -- fails on the second expectation rather than being absorbed
  # into a re-recorded list.
  expect_equal(
    setdiff(tm_corrupt_dropped_master(), dropped),
    c("ffmpeg_codecs/sort_by_type -> :", "segment_video/outfiles -> purrr::pmap")
  )
  expect_equal(setdiff(dropped, tm_corrupt_dropped_master()), character())

  # `segment_video/outfiles -> <none>` survives in both, and correctly: it is
  # the token form, the legal filename `"bad fmt!"`, which the verb compiled
  # before this milestone and still compiles (AC4).
  expect_true("segment_video/outfiles -> <none>" %in% dropped)
})

# AC2: segment_video()'s `outfiles` --------------------------------------------

test_that("segment_video() refuses a wrong `outfiles` itself, not purrr::pmap()", {
  dir <- withr::local_tempdir()
  vid <- file.path(dir, "in.mp4")
  file.create(vid)

  # The seven values `ffm_files()` refuses inside the fan-out today, measured
  # before the guard landed. Three at `start` of length 1 vary the FORM; four at
  # length 2 vary the POSITION as well, because a guard that checked only the
  # first or only the last element would pass a one-element probe set and still
  # leak the other end.
  probes <- list(
    number = list(start = 0, end = 1, outfiles = 123),
    missing = list(start = 0, end = 1, outfiles = NA),
    list = list(start = 0, end = 1, outfiles = list(1)),
    na_second = list(start = c(0, 0.5), end = c(0.5, 1),
                     outfiles = c("a.mp4", NA)),
    na_first = list(start = c(0, 0.5), end = c(0.5, 1),
                    outfiles = c(NA, "b.mp4")),
    number_pair = list(start = c(0, 0.5), end = c(0.5, 1),
                       outfiles = c(1, 2)),
    list_pair = list(start = c(0, 0.5), end = c(0.5, 1),
                     outfiles = list(1, 2))
  )
  for (nm in names(probes)) {
    p <- probes[[nm]]
    cnd <- tryCatch(
      segment_video(vid, start = p$start, end = p$end, outfiles = p$outfiles,
                    run = FALSE),
      error = function(e) e
    )
    expect_s3_class(cnd, "error")
    # The whole condition, not the frame alone: `In index:` is message text and
    # never a frame, so asserting it against the frame would be vacuous.
    whole <- paste0(blamed_verb(cnd), " || ",
                    cli::ansi_strip(conditionMessage(cnd)))
    expect_identical(blamed_verb(cnd), "segment_video", info = nm)
    expect_false(grepl("pmap", whole, fixed = TRUE), info = nm)
    expect_false(grepl("In index:", whole, fixed = TRUE), info = nm)
  }

  # The length-2 form at `start` of length 1 is still the length check's, not
  # the new guard's: the guard sits below it, so the message a caller who
  # miscounted their segments reads has not moved.
  cnd <- tryCatch(
    segment_video(vid, start = 0, end = 1, outfiles = c(1, 2), run = FALSE),
    error = function(e) e
  )
  expect_identical(blamed_verb(cnd), "segment_video")
  expect_match(cli::ansi_strip(conditionMessage(cnd)),
               "same length as", fixed = TRUE)
})

test_that("a wrong `outfiles` outranks the nvenc availability probe", {
  # AC2's siting clause, and the reason it is in the criterion at all: with the
  # guard below `check_nvenc_available()`, this call reports "nvenc encoder is
  # not available" on a build without nvenc and reports the `outfiles` error on
  # a build with it -- a machine deciding which of a caller's own mistakes they
  # are told about, which is the failure D075 exists to prevent.
  dir <- withr::local_tempdir()
  vid <- file.path(dir, "in.mp4")
  file.create(vid)
  withr::local_options(tidymedia.nvenc_encoders = NULL)
  local_mocked_bindings(cached_encoder_names = function() character(),
                        .package = "tidymedia")
  cnd <- tryCatch(
    segment_video(vid, start = 0, end = 1, outfiles = 123,
                  video_codec = "libx264", hardware = "nvenc", run = FALSE),
    error = function(e) e
  )
  msg <- cli::ansi_strip(conditionMessage(cnd))
  expect_identical(blamed_verb(cnd), "segment_video")
  expect_match(msg, "outfiles", fixed = TRUE)
  expect_false(grepl("not available", msg, fixed = TRUE))

  # The control: with the `outfiles` value put right, the same call DOES report
  # the missing encoder -- so the assertion above is the guard outranking a live
  # abort and not an abort that never fires.
  cnd2 <- tryCatch(
    segment_video(vid, start = 0, end = 1, outfiles = file.path(dir, "a.mp4"),
                  video_codec = "libx264", hardware = "nvenc", run = FALSE),
    error = function(e) e
  )
  expect_match(cli::ansi_strip(conditionMessage(cnd2)), "not available",
               fixed = TRUE)
})

test_that("the guard refuses nothing segment_video() compiled before", {
  # AC4, and AC2's two compiled-today controls in the same table: `"bad fmt!"`
  # is a legal output filename and `list("a.mp4")` is what the per-row fan-out
  # really receives, so a guard written against `outfiles` as a whole object
  # would refuse both.
  dir <- withr::local_tempdir()
  expect_identical(tm_outfiles_commands(dir), tm_outfiles_baseline())
})

# AC3: ffmpeg_codecs()'s `sort_by_type` ----------------------------------------

test_that("ffmpeg_codecs() refuses a wrong `sort_by_type` as its sibling does, spawning nothing", {
  forms <- tm_nvenc_wrong_forms()
  for (form in names(forms)) {
    spawns <- 0L
    local_mocked_bindings(
      guard_timeout = function(program, limit, expr, ...) {
        spawns <<- spawns + 1L
        character(0)
      },
      .package = "tidymedia"
    )
    codecs <- tryCatch(ffmpeg_codecs(sort_by_type = forms[[form]]),
                       error = function(e) e)
    encoders <- tryCatch(ffmpeg_encoders(sort_by_type = forms[[form]]),
                         error = function(e) e)
    expect_s3_class(codecs, "error")
    expect_s3_class(encoders, "error")
    expect_identical(
      cli::ansi_strip(conditionMessage(codecs)),
      cli::ansi_strip(conditionMessage(encoders)),
      info = form
    )
    # AC3's other half. Before the guard, `ffmpeg_codecs()` ran the binary and
    # only then failed on an `if`, so a call it should have refused outright
    # cost a process; `ffmpeg_encoders()` never did.
    expect_identical(spawns, 0L, info = form)
  }
})

test_that("ffmpeg_codecs() still returns both orderings", {
  skip_if_not(nzchar(Sys.which("ffmpeg")), "FFmpeg not installed")
  sorted <- ffmpeg_codecs(TRUE)
  plain <- ffmpeg_codecs(FALSE)
  # AC4's `ffmpeg_codecs()` half stated as a contract rather than as this
  # build's 539 rows: both calls return the same codecs, and only the order
  # differs -- by type then name, or by name alone.
  expect_setequal(sorted$name, plain$name)
  expect_equal(sorted[order(sorted$type, sorted$name), ], sorted,
               ignore_attr = TRUE)
  expect_equal(plain[order(plain$name), ], plain, ignore_attr = TRUE)
})
