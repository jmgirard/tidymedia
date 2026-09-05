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
  # The positive control, from a function the package really has: a formal
  # named `call` is detected. A green sweep below means the two exports lost
  # theirs, not that the detector stopped looking.
  probe <- names(formals(tidymedia:::hardware_encoder_available))
  expect_true("call" %in% probe)
  expect_true("call" %in% names(formals(function(x, call) NULL)))
})

test_that("no exported function has a formal named `call`", {
  sweep <- tm_export_formals()
  hits <- sweep$export[sweep$has_call]

  expect_identical(hits, character(0))
})
