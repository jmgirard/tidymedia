# probe_all(parallel =) and the probe_*() shortcuts (M53).
#
# The parallel path can emit TWO distinct warnings in one call: the
# sequential-plan guard (borrowed from ffm_batch(), class
# "tidymedia_sequential_plan") and the unprobeable-file report. expect_warning()
# speaks about one condition at a time, so the warning-contract tests below
# collect every warning and assert on the one they mean -- a total count would
# be an artifact of which plan the test run happens to have active.

# Run `expr` capturing and muffling every warning. Returns list(value, warnings),
# where `warnings` is a list of the condition objects (message AND class both
# available to the caller).
collect_warnings <- function(expr) {
  seen <- list()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      seen[[length(seen) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = seen)
}

# The unprobeable-file warnings among a collected set: probe_all()'s own report,
# excluding the sequential-plan guard and anything future/furrr emits.
file_warnings <- function(warnings) {
  Filter(function(w) grepl("Could not probe", conditionMessage(w)), warnings)
}


# AC1 -- parity and row order ---------------------------------------------

test_that("probe_all(parallel = TRUE) matches parallel = FALSE exactly", {
  skip_if_no_ffprobe()
  infile <- make_test_video()
  missing <- file.path(tempdir(), "tm-does-not-exist-xyz.mp4")
  # A duplicated path and an unprobeable file in one vector: the case
  # test-ffprobe.R already exercises sequentially, now asserted across paths.
  files <- c(infile, missing, infile)

  seq_out <- collect_warnings(probe_all(files))$value
  par_out <- collect_warnings(probe_all(files, parallel = TRUE))$value

  expect_identical(par_out, seq_out)
})

test_that("probe_all(parallel = TRUE) keeps rows in INPUT order", {
  skip_if_no_ffprobe()
  infile <- make_test_video()
  other <- make_test_video()
  missing <- file.path(tempdir(), "tm-does-not-exist-xyz.mp4")
  files <- c(missing, infile, other, infile)

  out <- collect_warnings(probe_all(files, parallel = TRUE))$value

  # One container row per input, in the vector's own order -- not deduplicated,
  # not sorted by `file`. The duplicate is what makes this a real assertion:
  # a sort would coalesce or reorder it.
  expect_identical(out$container$file, files)
})

test_that("probe_all(parallel = TRUE) types columns exactly as the sequential path", {
  skip_if_no_ffprobe()
  infile <- make_test_video()
  files <- c(infile, infile)

  typed <- collect_warnings(probe_all(files, parallel = TRUE))$value
  raw <- collect_warnings(probe_all(files, typed = FALSE, parallel = TRUE))$value

  expect_identical(typed, probe_all(files))
  expect_identical(raw, probe_all(files, typed = FALSE))
  # typed = FALSE really is the unconverted form, so the two are distinguishable
  # and the pair above is not two copies of one assertion.
  expect_false(identical(typed, raw))
})


# AC2 -- the argument is load-bearing -------------------------------------

test_that("probe_all(parallel = TRUE) routes through furrr, parallel = FALSE does not", {
  skip_if_no_ffprobe()
  skip_if_not_installed("furrr")
  infile <- make_test_video()

  # Mutation probe (M39's trick): if the implementation stopped consulting
  # `parallel` -- accepting it and always taking the purrr branch -- this stub
  # is never reached and `reached` stays FALSE. Asserting the default's
  # behaviour instead would pass either way, which is why the argument gets a
  # stub rather than an output comparison: under the default sequential plan
  # both branches return byte-identical results.
  reached <- FALSE
  local_mocked_bindings(
    future_map = function(.x, .f, ...) {
      reached <<- TRUE
      purrr::map(.x, .f)
    },
    .package = "furrr"
  )

  collect_warnings(probe_all(infile, parallel = TRUE))
  expect_true(reached)

  reached <- FALSE
  probe_all(infile, parallel = FALSE)
  expect_false(reached)
})

test_that("probe_all() rejects a non-logical `parallel`", {
  expect_error(probe_all("a.mp4", parallel = "yes"))
  expect_error(probe_all("a.mp4", parallel = NA))
})


# AC3 -- the unprobeable-file warning contract ----------------------------

test_that("two unprobeable files raise exactly ONE file warning naming both", {
  skip_if_no_ffprobe()
  infile <- make_test_video()
  miss_a <- file.path(tempdir(), "tm-missing-a-xyz.mp4")
  miss_b <- file.path(tempdir(), "tm-missing-b-xyz.mp4")
  files <- c(infile, miss_a, miss_b)

  for (par in c(FALSE, TRUE)) {
    got <- file_warnings(collect_warnings(probe_all(files, parallel = par))$warnings)
    # One warning for the whole call -- not one per unprobeable file, and not
    # one per worker.
    expect_length(got, 1L)
    msg <- conditionMessage(got[[1L]])
    expect_match(msg, basename(miss_a), fixed = TRUE)
    expect_match(msg, basename(miss_b), fixed = TRUE)
  }
})

test_that("probe_all(parallel = TRUE) warns about a sequential plan", {
  skip_if_no_ffprobe()
  infile <- make_test_video()

  got <- collect_warnings(probe_all(infile, parallel = TRUE))$warnings
  guard <- Filter(
    function(w) inherits(w, "tidymedia_sequential_plan"), got
  )
  # probe_all() is a terminal entry point: nothing downstream will warn for it,
  # so parallel = TRUE under the default sequential plan says so here (M53 T1).
  expect_length(guard, 1L)
})

test_that("probe_all(parallel = FALSE) never warns about the plan", {
  skip_if_no_ffprobe()
  infile <- make_test_video()

  got <- collect_warnings(probe_all(infile, parallel = FALSE))$warnings
  expect_length(Filter(function(w) inherits(w, "tidymedia_sequential_plan"), got), 0L)
})


# AC4 -- furrr is needed only on the parallel path ------------------------

test_that("furrr is required on the parallel path and untouched on the sequential one", {
  skip_if_no_ffprobe()
  infile <- make_test_video()

  # furrr is installed here, so "unavailable" is staged by making the package's
  # own availability check report it missing. The stub raises rlang's real
  # condition class, and the assertion keys off that class rather than off the
  # message text, which rlang owns and may reword.
  local_mocked_bindings(
    check_installed = function(pkg, ...) {
      rlang::abort("stub", class = "rlib_error_package_not_found")
    },
    .package = "rlang"
  )

  expect_error(
    probe_all(infile, parallel = TRUE),
    class = "rlib_error_package_not_found"
  )
  # The sequential path never reaches the check, so it completes normally with
  # furrr masked -- which is what keeps furrr a Suggests-only dependency.
  expect_s3_class(probe_all(infile, parallel = FALSE)$container, "tbl_df")
})


# AC5 -- the probe_*() shortcuts ------------------------------------------

test_that("the probe_*() shortcuts pass `parallel` through on the infile branch", {
  skip_if_no_ffprobe()
  skip_if_not_installed("furrr")
  infile <- make_test_video()

  shortcuts <- list(
    probe_container = probe_container,
    probe_streams = probe_streams,
    probe_video = probe_video,
    probe_audio = probe_audio
  )

  for (nm in names(shortcuts)) {
    reached <- FALSE
    local_mocked_bindings(
      future_map = function(.x, .f, ...) {
        reached <<- TRUE
        purrr::map(.x, .f)
      },
      .package = "furrr"
    )
    collect_warnings(shortcuts[[nm]](infile = infile, parallel = TRUE))
    expect_true(reached, info = nm)
  }
})

test_that("the probe_*() shortcuts ignore `parallel` when given a probe object", {
  skip_if_no_ffprobe()
  skip_if_not_installed("furrr")
  infile <- make_test_video()
  p <- probe_all(infile)

  reached <- FALSE
  local_mocked_bindings(
    future_map = function(.x, .f, ...) {
      reached <<- TRUE
      purrr::map(.x, .f)
    },
    .package = "furrr"
  )

  # `parallel` is consumed only where `typed` is -- on the infile branch. With a
  # probe object in hand there is nothing to fan out, so no furrr call happens
  # and the results match the plain call.
  expect_identical(probe_container(probe = p, parallel = TRUE), p$container)
  expect_identical(probe_streams(probe = p, parallel = TRUE), p$streams)
  expect_false(reached)
})

test_that("the probe_*() shortcuts still reject both/neither of probe and infile", {
  expect_error(probe_container(parallel = TRUE))
  expect_error(
    probe_container(probe = list(), infile = "a.mp4", parallel = TRUE)
  )
})
