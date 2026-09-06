# M112 AC1: `call` is an internal implementation detail, so no exported
# function publishes it as a formal -- the Rd usage line a reader copies from
# must not offer an argument only tidymedia's own internals have a value for.

test_that("the sweep reads a non-empty domain it did not derive", {
  sweep <- tm_export_formals()

  # Stated independently of the sweep: these four exports exist and the sweep
  # must have examined them. Without this the sweep passes vacuously on an
  # empty or broken domain (a renamed package, a namespace that failed to
  # load), which is exactly the silent-empty failure it would otherwise hide.
  expect_true(all(
    c("set_program", "hardware_encoder", "ffm_files", "mediainfo_template")
      %in% sweep$export
  ))
  expect_gt(nrow(sweep), 50)
})

test_that("the sweep's predicate goes red on a formal named `call`", {
  # The positive control has to run THE SWEEP, not a hand-rolled restatement of
  # what it does: a control that only checks `"call" %in% names(formals(f))`
  # leaves `tm_export_formals()`'s own `has_call` column untested, so breaking
  # the predicate to something uniformly FALSE would keep every test here
  # green. So point the sweep at a namespace known to export a function with a
  # `call` formal. rlang is an Imports dependency, and `rlang::abort()` has
  # taken `call` since 1.0.0.
  control <- tm_export_formals("rlang")
  expect_gt(nrow(control), 50)
  expect_true("abort" %in% control$export)
  expect_true(control$has_call[control$export == "abort"])
  expect_true("call" %in% strsplit(
    control$formals[control$export == "abort"], ", ", fixed = TRUE
  )[[1]])

  # And the column is not uniformly TRUE either -- a predicate stuck the other
  # way would pass everything above.
  expect_false(all(control$has_call))
  expect_false(control$has_call[control$export == "is_missing"])
})

test_that("no exported function has a formal named `call`", {
  sweep <- tm_export_formals()
  hits <- sweep$export[sweep$has_call]

  expect_identical(hits, character(0))
})
