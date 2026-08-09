# M67 AC2/AC3: the memo is one answer for the whole package, not one per verb.
#
# H -- every exported function taking `hardware` -- is read from the namespace
# at test time (helper-nvenc-memo.R), and each cell's call is built from that
# function's own formals. Nothing here is hand-listed, so a verb that gains
# `hardware` later is measured without anyone remembering to add it.

test_that("H is read from the namespace and is not empty", {
  # A floor, not a count: it fails if the enumeration silently collapses and
  # reports a vacuous pass over zero verbs. Sixteen is the measured count at
  # M67; a verb gaining `hardware` later raises it.
  h <- nvenc_hardware_exports()
  expect_gte(length(h), 16L)
  expect_true(all(c("standardize_video", "standardize_video_batch") %in% h))
})

# The per-cell abort control (AC2): a cell that aborts measures nothing (M41),
# so each cell asserts it did not.
#
# Deliberately NOT `expect_no_error(expr, message = <label>)`: that `message`
# is passed through as the matcher's REGEXP, not as a failure label, so an
# abort whose text does not match the label records no expectation at all and
# propagates -- turning the run red, but without naming the verb and without
# measuring the rest of H. Catching the condition here keeps the failure
# labelled and lets the loop finish the remaining cells.
expect_cell_ran <- function(verb, expr) {
  err <- tryCatch({
    force(expr)
    NULL
  }, error = function(e) conditionMessage(e))
  expect_null(
    err,
    info = paste0(verb, ": the cell aborted, so it measured nothing")
  )
}

test_that("each verb asks FFmpeg once, from a cold memo", {
  # AC2. One cell per member of H, each starting cold, each asserting the call
  # did not abort -- a cell that aborts measures nothing (M41).
  f <- make_input()
  for (verb in nvenc_hardware_exports()) {
    probes <- local_encoder_probe_counter()
    args <- nvenc_grid_args(verb, f)
    expect_cell_ran(verb, do.call(verb, args, envir = asNamespace("tidymedia")))
    expect_identical(probes(), 1L, info = verb)
    forget_ffmpeg_capabilities()
  }
})

test_that("one answer serves every verb in a session", {
  # AC3. The same grid, run without discarding the memo between cells: the
  # first verb warms it and no later verb re-asks. A per-function memo passes
  # the test above and fails this one.
  f <- make_input()
  probes <- local_encoder_probe_counter()

  for (verb in nvenc_hardware_exports()) {
    expect_cell_ran(
      verb,
      do.call(verb, nvenc_grid_args(verb, f), envir = asNamespace("tidymedia"))
    )
  }
  expect_identical(probes(), 1L)
})
