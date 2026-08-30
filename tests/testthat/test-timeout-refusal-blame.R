# M094: an invalid `tidymedia.timeout` is refused by the function the caller
# typed.
#
# `resolve_timeout()` has always refused a value base R would mishandle, but it
# was reached from wherever the limit happened to be read first -- so 47 of the
# 53 exports in `tm_timeout_domain()` aborted naming `ffm_run(object)`,
# `ffm_batch(jobs, <the whole deparsed builder>)`, `mediainfo_read(file, inform)`
# or `purrr::map(infile, probe_one)`. `tm_timeout_blame_master()` records that
# state (measured at ae5ff1c); this file is the sweep that closes it.
#
# The domain is COMPUTED (`tm_timeout_domain()`), not recalled, for M70's
# reason: an export that starts reaching a spawn joins the sweep on its own.

test_that("the sweep runs over a non-empty domain with a cell for each member", {
  dir <- withr::local_tempdir()
  dom <- tm_timeout_domain()
  specs <- tm_timeout_call_specs(dir)
  # Guards on the guard: a domain that silently emptied, or a member with no
  # argument cell, would make every expectation below vacuously true.
  expect_gt(length(dom), 0)
  expect_setequal(names(tm_timeout_blame_master()), dom)
  expect_true(all(dom %in% names(specs)))
})

test_that("every domain member blames itself for an invalid limit (AC1, AC2)", {
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  forms <- tm_timeout_bad_forms()
  # The override stays UNSET for this sweep, so `has_nvenc()` takes the branch
  # that reads the limit and is held to the same rule as everything else.
  withr::local_options(list(tidymedia.nvenc_encoders = NULL))

  for (name in tm_timeout_domain()) {
    for (form in names(forms)) {
      expect_identical(
        tm_blame_head(name, specs[[name]], forms[[form]]), name,
        info = paste(name, form)
      )
    }
  }
})

test_that("a set encoder override leaves `has_nvenc()` nothing to refuse", {
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  withr::local_options(list(tidymedia.nvenc_encoders = "h264_nvenc"))
  for (form in names(tm_timeout_bad_forms())) {
    limit <- tm_timeout_bad_forms()[[form]]
    expect_identical(
      tm_blame_head("has_nvenc", specs$has_nvenc, limit), "<none>",
      info = form
    )
    # And the answer is still the override's, not a stale or spawned one.
    expect_true(
      withr::with_options(list(tidymedia.timeout = limit), has_nvenc("h264"))
    )
  }
})

test_that("the refusal does not wait for `run = TRUE` (AC3)", {
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  withr::local_options(list(tidymedia.nvenc_encoders = NULL))
  carries_run <- Filter(
    function(nm) {
      "run" %in% names(formals(get(nm, envir = asNamespace("tidymedia"))))
    },
    tm_timeout_domain()
  )
  # The asymmetry this closes was measured on master: `extract_audio(run =
  # FALSE)` raised nothing while `extract_audio_batch(run = FALSE)` aborted, so
  # a domain that lost the scalar half would leave the sweep green on the wrong
  # thing.
  expect_true("extract_audio" %in% carries_run)
  expect_true("extract_audio_batch" %in% carries_run)

  for (name in carries_run) {
    for (form in names(tm_timeout_bad_forms())) {
      args <- specs[[name]]
      args$run <- FALSE
      expect_identical(
        tm_blame_head(name, args, tm_timeout_bad_forms()[[form]]), name,
        info = paste(name, "run = FALSE", form)
      )
    }
  }
})

test_that("the refusal does not wait for the fan-out (AC3)", {
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  withr::local_options(list(tidymedia.nvenc_encoders = NULL))
  carries_parallel <- Filter(
    function(nm) {
      "parallel" %in%
        names(formals(get(nm, envir = asNamespace("tidymedia"))))
    },
    tm_timeout_domain()
  )
  expect_gt(length(carries_parallel), 0)

  # No `skip_if_not_installed("furrr")` here on purpose. A machine-independent
  # refusal reports before a machine-dependent one (D036), and `furrr` is what
  # every one of these checks next -- so a correct front door never reaches
  # `check_installed()`, and a cell that DID reach it would fail here rather
  # than skip, which is the point.
  for (name in carries_parallel) {
    for (form in names(tm_timeout_bad_forms())) {
      args <- specs[[name]]
      args$parallel <- TRUE
      expect_identical(
        tm_blame_head(name, args, tm_timeout_bad_forms()[[form]]), name,
        info = paste(name, "parallel = TRUE", form)
      )
    }
  }
})
