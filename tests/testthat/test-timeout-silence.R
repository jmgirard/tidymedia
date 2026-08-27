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

# T2: count_audio_streams() ---------------------------------------------------

# The probe D024 licenses. Its outcome may change nothing but whether a
# diagnostic is signalled -- so a warning is inside the licence where a changed
# count is not, and the counts below have to be the ones the silent version
# returned.

# ffprobe token vector -> the fake output. `hit` names the inputs whose probe
# the limit kills; every other input reports `tracks` audio streams.
local_probe_timeout <- function(hit, tracks = 3L, limit = 2,
                                env = parent.frame()) {
  withr::local_options(tidymedia.timeout = limit, .local_envir = env)
  testthat::local_mocked_bindings(
    find_ffprobe = function(...) "/usr/bin/ffprobe",
    run_program = function(location, args, program = "the program", ...) {
      file <- args[[which(args == "-i") + 1L]]
      if (file %in% hit) abort_timeout(program, limit)
      rep("0", tracks)
    },
    .package = "tidymedia",
    .env = env
  )
}

test_that("a timed-out track probe warns once per call, not once per file", {
  files <- c("a.mkv", "b.mkv", "c.mkv")
  local_probe_timeout(hit = c("a.mkv", "b.mkv"))

  warns <- NULL
  counts <- withCallingHandlers(
    count_audio_streams_all(files),
    warning = function(w) {
      warns <<- c(warns, list(w))
      invokeRestart("muffleWarning")
    }
  )

  # Exactly one warning for two timed-out inputs, and it says how many.
  expect_length(warns, 1L)
  expect_s3_class(warns[[1]], "tidymedia_probe_timeout")
  expect_match(cli::ansi_strip(conditionMessage(warns[[1]])), "2")
  expect_match(cli::ansi_strip(conditionMessage(warns[[1]])), "timed out")
})

test_that("the counts a timed-out probe returns are the silent version's", {
  # D024's licence holds only while the probe's outcome changes nothing but
  # whether a diagnostic fires. NA is what the pre-M70 code returned for a
  # killed probe, and it is what has to come back now.
  files <- c("a.mkv", "b.mkv", "c.mkv")
  local_probe_timeout(hit = c("a.mkv", "b.mkv"))
  counts <- suppressWarnings(count_audio_streams_all(files))
  expect_identical(counts, c(NA_integer_, NA_integer_, 3L))
})

test_that("a timed-out probe still reports the dropped tracks it could count", {
  # The diagnostic the probe exists for is unaffected: the input it DID read
  # still gets its dropped-track warning, and the two it could not are skipped
  # exactly as an unreadable file is.
  files <- c("a.mkv", "b.mkv", "c.mkv")
  local_probe_timeout(hit = c("a.mkv", "b.mkv"))
  counts <- suppressWarnings(count_audio_streams_all(files))
  msg <- tryCatch({
    warn_dropped_audio(files, counts)
    NULL
  }, warning = function(w) cli::ansi_strip(conditionMessage(w)))
  expect_match(msg, "c.mkv", fixed = TRUE)
  expect_no_match(msg, "a.mkv", fixed = TRUE)
})

test_that("count_audio_streams() keeps answering NA for every other failure", {
  # The sentinel is for the limit alone. An unreadable file, a missing binary
  # and a non-zero exit all stay silent NAs -- D024's fail-open consequence.
  testthat::local_mocked_bindings(
    find_ffprobe = function(...) "/usr/bin/ffprobe",
    run_program = function(...) structure("", status = 1L),
    .package = "tidymedia"
  )
  expect_no_warning(
    expect_identical(count_audio_streams_all("a.mkv"), NA_integer_)
  )
})

# T3: tool_versions() ---------------------------------------------------------

# ffm_batch(manifest = TRUE) records which FFmpeg built each output. A killed
# version probe recorded NA there and said nothing, which reads in the manifest
# exactly like a missing binary (D048).

test_that("a timed-out version probe warns once and names both tools", {
  testthat::local_mocked_bindings(
    find_ffmpeg = function(...) "/usr/bin/ffmpeg",
    find_ffprobe = function(...) "/usr/bin/ffprobe",
    run_program = function(location, args, program = "the program", ...) {
      abort_timeout(program, 2)
    },
    .package = "tidymedia"
  )
  warns <- NULL
  versions <- withCallingHandlers(
    tool_versions(),
    warning = function(w) {
      warns <<- c(warns, list(w))
      invokeRestart("muffleWarning")
    }
  )
  # One warning for two killed probes, not one per tool.
  expect_length(warns, 1L)
  expect_s3_class(warns[[1]], "tidymedia_probe_timeout")
  msg <- cli::ansi_strip(conditionMessage(warns[[1]]))
  expect_match(msg, "timed out")
  expect_match(msg, "FFmpeg", fixed = TRUE)
  expect_match(msg, "FFprobe", fixed = TRUE)

  # The manifest's own shape is untouched: NA is what a version that could not
  # be read has always been recorded as.
  expect_identical(versions, list(ffmpeg = NA_character_,
                                  ffprobe = NA_character_))
})

test_that("only the killed probe is named when one tool answers", {
  testthat::local_mocked_bindings(
    find_ffmpeg = function(...) "/usr/bin/ffmpeg",
    find_ffprobe = function(...) "/usr/bin/ffprobe",
    run_program = function(location, args, program = "the program", ...) {
      if (program == "FFprobe") abort_timeout(program, 2)
      "ffmpeg version 8.1.2 Copyright (c)"
    },
    .package = "tidymedia"
  )
  msg <- tryCatch({
    v <- tool_versions()
    NULL
  }, warning = function(w) cli::ansi_strip(conditionMessage(w)))
  expect_match(msg, "FFprobe", fixed = TRUE)
  expect_no_match(msg, "FFmpeg", fixed = TRUE)

  v <- suppressWarnings(tool_versions())
  expect_identical(v$ffmpeg, "8.1.2")
  expect_identical(v$ffprobe, NA_character_)
})

test_that("a missing binary still records NA silently", {
  # D024's fail-open consequence, unchanged: only the limit speaks.
  testthat::local_mocked_bindings(
    find_ffmpeg = function(...) "",
    find_ffprobe = function(...) "",
    .package = "tidymedia"
  )
  expect_no_warning(
    expect_identical(tool_versions(),
                     list(ffmpeg = NA_character_, ffprobe = NA_character_))
  )
})
