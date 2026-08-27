# M70: no timeout is silent.
#
# M69 shipped a hand-written partition of the package's timeout behavior and
# three review passes each found one more member it omitted (D048). The domain
# is derived here instead, from the package's own call graph, and every guard in
# this file quantifies over what the sweep returns rather than over a list
# anyone wrote down.

# T1: the sweep ---------------------------------------------------------------

test_that("the swept domain is the recorded membership", {
  # The drift guard. It reddens when an export starts or stops reaching a spawn
  # primitive, which is the event M69's list could not see.
  expect_identical(tm_timeout_domain(), tm_timeout_recorded_domain())
})

test_that("membership comes from the call graph, not from the record", {
  # Mutation probe. With no spawn primitive to reach, nothing reaches one: if
  # the domain survived this it would be reading the recorded list somewhere.
  graph <- tm_symbol_graph()
  expect_length(tm_reaches_spawn(graph, seeds = character()), 0L)

  # And a single seed must give strictly less than the real pair, so the closure
  # is doing work rather than returning the namespace.
  both <- tm_reaches_spawn(graph)
  expect_true(length(tm_reaches_spawn(graph, seeds = "system2")) < length(both))
})

test_that("the closure excludes the pure compilation surface", {
  # Non-vacuity from the other side: D024's pure surface runs no binary from any
  # path, so a sweep that returned everything would show up here.
  reaches <- tm_reaches_spawn()
  for (f in c("ffm_compile", "ffm_crop", "ffm_scale", "ffm_trim", "ffm")) {
    expect_false(f %in% reaches, info = f)
  }
})

test_that("run_program() is derived into the closure rather than seeded", {
  # `tm_spawn_primitives` names only base R's two spawns. run_program() is the
  # package's own wrapper over system2() and has to be FOUND, not listed --
  # listing it would make the sweep a hand-list again.
  expect_false("run_program" %in% tm_spawn_primitives)
  expect_true("run_program" %in% tm_reaches_spawn())
})

test_that("the absorber partition is the reaching functions that can swallow", {
  # Explanatory rather than the domain (see the helper): these are the reaching
  # functions installing a handler from R's own condition API in their own body,
  # so they are where a silence can come from. A new one appearing here without
  # a guard below is the thing to look at.
  expect_identical(
    tm_timeout_absorbers(),
    c("capture_version", "count_audio_streams", "ffm_batch", "ffm_run",
      "run_separation_audio", "verify_media")
  )
})

test_that("the lazy condition wrappers are outside the closure", {
  # guard_timeout() and absorb_timeout() take the spawn as a promise, so neither
  # names a spawn primitive itself. That is why the closure does not collapse:
  # were they in it, every caller of run_program() would inherit "installs a
  # handler" and the partition above would be the whole package.
  reaches <- tm_reaches_spawn()
  expect_false("guard_timeout" %in% reaches)
  expect_false("absorb_timeout" %in% reaches)
})

test_that("every swept function has a call spec", {
  # The procedural bound. The domain is computed; a member with no way to be
  # called fails here rather than being quietly left out of the grid below.
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  expect_identical(sort(names(specs)), tm_timeout_domain())
})

test_that("the sweep sees a spawn reached through a function passed as a value", {
  # Why this sweep does not reuse M62's call-head graph, pinned as the
  # discrepancy it is rather than asserted in prose. `probe_all()` reaches
  # FFprobe only through `purrr::map(infile, probe_one)`, where `probe_one` is
  # an argument and never a call head -- so a head-only walk drops it, the four
  # `probe_*()` accessors and `verify_media()` out of the domain, and the
  # silence rule would quietly stop covering the package's main metadata reader.
  expect_false("probe_one" %in% tm_call_graph()[["probe_all"]])
  expect_true("probe_one" %in% tm_symbol_graph()[["probe_all"]])
  for (f in c("probe_all", "probe_audio", "probe_container", "probe_streams",
              "probe_video", "verify_media")) {
    expect_true(f %in% tm_timeout_domain(), info = f)
  }
})
