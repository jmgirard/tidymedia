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
  for (f in c("ffm_compile", "ffm_crop", "ffm_scale", "ffm_trim", "ffm_files")) {
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
  #
  # separate_audio_video() joined the partition in M088: it HOLDS the audio
  # run's condition so the video command can still run, and holds the video
  # run's own so the audio failure is the one reported. A held timeout is not a
  # swallowed one -- the audio condition is re-raised unchanged -- and the guard
  # for that is this file's abort half, which requires this verb's forced
  # timeout to reach the caller carrying `tidymedia_timeout`.
  expect_identical(
    tm_timeout_absorbers(),
    c("capture_version", "count_audio_streams", "ffm_batch", "ffm_run",
      "program_status", "run_separation_audio", "separate_audio_video",
      "verify_media")
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
  # discrepancy it is rather than asserted in prose. `probe_all_impl()` reaches
  # FFprobe only through `purrr::map(infile, probe_one)`, where `probe_one` is
  # an argument and never a call head -- so a head-only walk drops it, its
  # `probe_all()` wrapper, the four `probe_*()` accessors and `verify_media()`
  # out of the domain, and the silence rule would quietly stop covering the
  # package's main metadata reader.
  expect_false("probe_one" %in% tm_call_graph()[["probe_all_impl"]])
  expect_true("probe_one" %in% tm_symbol_graph()[["probe_all_impl"]])
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

# T9: ffm_batch() -------------------------------------------------------------

# The third no-warning path, and the one M69's hand-list never reached: the T1
# sweep found it. ffm_batch() records every job failure as `success = FALSE` and
# signals nothing, so a reached limit was silent through it and through the 15
# `_batch` verbs and segment_video(), which fan out through it.

# Time out the encode for `hit` inputs only; every other job succeeds.
local_batch_timeout <- function(hit, limit = 2, env = parent.frame()) {
  withr::local_options(tidymedia.timeout = limit, .local_envir = env)
  testthat::local_mocked_bindings(
    find_ffmpeg = function(...) "/usr/bin/ffmpeg",
    ffm_run = function(object, verify = NULL) {
      if (object$input %in% hit) abort_timeout("FFmpeg", limit)
      invisible(character(0))
    },
    .package = "tidymedia",
    .env = env
  )
}

test_that("a timed-out job is no longer silent in a batch", {
  dir <- withr::local_tempdir()
  ins <- file.path(dir, c("a.mp4", "b.mp4", "c.mp4"))
  file.create(ins)
  jobs <- tibble::tibble(input = ins, output = file.path(dir, c("1", "2", "3")))
  local_batch_timeout(hit = ins[1:2])

  warns <- NULL
  out <- withCallingHandlers(
    ffm_batch(jobs, function(input, output, ...) ffm_files(input, output)),
    warning = function(w) {
      warns <<- c(warns, list(w))
      invokeRestart("muffleWarning")
    }
  )
  hits <- Filter(function(w) inherits(w, "tidymedia_batch_timeout"), warns)
  expect_length(hits, 1L)
  expect_match(cli::ansi_strip(conditionMessage(hits[[1]])), "2")
  expect_match(cli::ansi_strip(conditionMessage(hits[[1]])), "timed out")

  # The documented result is unchanged: the timed-out rows are the failed ones.
  expect_identical(out$success, c(FALSE, FALSE, TRUE))
})

test_that("a batch that fails for any other reason stays as quiet as before", {
  # T9 is bounded to the limit. A non-zero exit, a missing binary, a bad
  # filter -- all still record success = FALSE and signal nothing, which is the
  # contract every _batch verb's @return documents today.
  dir <- withr::local_tempdir()
  ins <- file.path(dir, c("a.mp4", "b.mp4"))
  file.create(ins)
  jobs <- tibble::tibble(input = ins, output = file.path(dir, c("1", "2")))
  testthat::local_mocked_bindings(
    find_ffmpeg = function(...) "/usr/bin/ffmpeg",
    ffm_run = function(object, verify = NULL) cli::cli_abort("nope"),
    .package = "tidymedia"
  )
  out <- expect_no_warning(
    ffm_batch(jobs, function(input, output, ...) ffm_files(input, output))
  )
  expect_identical(out$success, c(FALSE, FALSE))
})

test_that("a verification the limit kills is counted too", {
  # verify_media() re-raises a timeout (AC3), and ffm_batch() records a verify
  # error as `verified = NA`. Without this the encode succeeds, the probe hangs,
  # and the batch reports an unverified row with no reason given.
  dir <- withr::local_tempdir()
  ins <- file.path(dir, "a.mp4")
  file.create(ins)
  jobs <- tibble::tibble(input = ins, output = file.path(dir, "1"))
  withr::local_options(tidymedia.timeout = 2)
  testthat::local_mocked_bindings(
    find_ffmpeg = function(...) "/usr/bin/ffmpeg",
    ffm_run = function(object, verify = NULL) invisible(character(0)),
    verify_media = function(...) abort_timeout("FFprobe", 2),
    .package = "tidymedia"
  )
  msg <- tryCatch({
    ffm_batch(jobs, function(input, output, ...) ffm_files(input, output),
              verify = list(width = 320))
    NULL
  }, tidymedia_batch_timeout = function(w) cli::ansi_strip(conditionMessage(w)))
  expect_match(msg, "verification", ignore.case = TRUE)
})

# AC1: the grid ---------------------------------------------------------------

test_that("no swept function absorbs a forced timeout silently", {
  # The criterion, quantified over what the sweep returns rather than over
  # anything written down. A new export that reaches a spawn joins this grid on
  # its own and fails here until it is made to speak.
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  for (name in tm_timeout_domain()) {
    res <- tm_force_timeout(name, specs[[name]])
    expect_true(
      res$aborted || res$warned,
      info = paste0(
        name, " absorbed the timeout silently: ",
        if (is.null(res$error)) "no condition at all"
        else paste(class(res$error), collapse = "/")
      )
    )
  }
})

test_that("the grid spawns nothing, so it measures the package not the PATH", {
  # CI's macOS and Windows runners install no media binaries. The forcing is
  # injected at run_program() and guard_timeout(), the two wrappers standing in
  # front of every system()/system2() call the package makes, so no cell runs a
  # binary -- and a change that let one through would leave the grid measuring
  # whether the runner happened to have ffmpeg.
  spawners <- names(Filter(
    function(x) any(c("system", "system2") %in% x),
    tm_symbol_graph()
  ))
  expect_setequal(spawners, c("ffmpeg", "ffprobe", "mediainfo", "run_program"))
  # Every one of those four is intercepted: run_program() directly, and the
  # three hatches through guard_timeout(), which wraps their system() call.
  # Read as text, not through a name walk: these are ARGUMENT NAMES in the
  # local_mocked_bindings() call, and all.names() records call heads and
  # symbols, never the names arguments are supplied under.
  forcing <- paste(deparse(body(tm_force_timeout)), collapse = " ")
  expect_match(forcing, "run_program = ", fixed = TRUE)
  expect_match(forcing, "guard_timeout = ", fixed = TRUE)
  # And resolution is intercepted too (M113). Interception at the spawn
  # wrappers alone still lets a member read the PATH to decide WHETHER to
  # spawn, which is how `program_status()` came to answer differently on a
  # runner with no binaries than on a developer's machine.
  expect_match(forcing, "find_program = ", fixed = TRUE)
  for (f in c("ffmpeg", "ffprobe", "mediainfo")) {
    expect_true("guard_timeout" %in% tm_symbol_graph()[[f]], info = f)
  }
})

test_that("the grid answers the same on a machine with no media binaries", {
  # The behavioral half of the claim the test above makes by reading text.
  # M113: CI's macOS and Windows runners install none of the four programs, and
  # both went red on this grid and on the blame sweep with
  # `program_status absorbed the timeout silently: no condition at all`.
  # `program_status()` asks a program for its version only for a program it
  # resolved, so with nothing to resolve it spawned nothing, reached no limit,
  # and read as silent -- the one cell whose answer came from the runner rather
  # than from the package.
  #
  # This is that runner, made local: an empty `PATH`, an empty config dir and an
  # empty legacy dir are the three places `find_program()` looks, and here they
  # hold nothing. `tm_force_timeout()` mocks `find_program()` itself, so no cell
  # below consults them; that is the property being pinned, and dropping the
  # mock turns this red on a machine that does have the binaries installed while
  # leaving the grid above green.
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  cfg <- withr::local_tempdir()
  withr::local_envvar(PATH = "", R_USER_CONFIG_DIR = cfg)
  withr::local_options(list(tidymedia.hardware_encoders = NULL))
  local_mocked_bindings(tm_legacy_config_dir = function() cfg)
  for (name in tm_timeout_domain()) {
    res <- tm_force_timeout(name, specs[[name]])
    expect_true(
      res$aborted || res$warned,
      info = paste0(
        name, " went silent with no binaries on the machine: ",
        if (is.null(res$error)) "no condition at all"
        else paste(class(res$error), collapse = "/")
      )
    )
  }
})

test_that("the abort half of the grid names the class D047 promises", {
  # The two halves are not interchangeable: an abort has to carry
  # `tidymedia_timeout` so a caller can catch it, and a warning has to say the
  # limit was the reason. `expect_true(aborted || warned)` above would pass on a
  # bare error, which is what this pins.
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  # separate_audio_video() is here from M088 on: it is the one member that
  # catches a timeout and re-raises it, so "aborted" alone would pass on a
  # rebuilt bare error and the class is what says the held condition is the
  # condition the caller gets.
  aborting <- c("extract_audio", "ffm_run", "ffmpeg", "ffprobe", "mediainfo",
                "separate_audio_video", "verify_media")
  for (name in aborting) {
    res <- tm_force_timeout(name, specs[[name]])
    expect_s3_class(res$error, "tidymedia_timeout")
  }
})

test_that("the grid would catch a function that swallows the timeout", {
  # Mutation probe for the criterion itself. The grid asserts a disjunction, so
  # a forcing that silently stopped working would pass 53 times and measure
  # nothing. Standing a swallowing implementation in front of one member proves
  # the verdict discriminates.
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  testthat::local_mocked_bindings(
    strip_metadata = function(infile, outfile, run = TRUE) {
      tryCatch(run_program(infile, outfile, program = "FFmpeg"),
               error = function(e) NULL)
    },
    .package = "tidymedia"
  )
  res <- tm_force_timeout("strip_metadata", specs$strip_metadata)
  expect_false(res$aborted || res$warned)
})

# The anchors: a real hung binary ---------------------------------------------

# The grid injects `abort_timeout()`'s condition rather than hanging a binary 53
# times -- base R's SIGINT/SIGTERM/SIGKILL ladder costs ~42 s per hang on Linux
# (M69/D047), and the members taking no file argument cannot be reached by a
# FIFO at all. These tie the injected object to what a binary really produces.

test_that("the injected condition is the one a real hung binary produces", {
  blocked <- local_blocking_input()
  withr::local_options(tidymedia.timeout = 2)
  real <- tryCatch(ffprobe(paste("-i", shQuote(blocked))),
                   error = function(e) e)
  injected <- tm_force_timeout("ffprobe", list(command = "-version"))$error

  expect_s3_class(real, "tidymedia_timeout")
  expect_identical(class(real), class(injected))
  expect_identical(real$tm_program, injected$tm_program)
  expect_identical(real$tm_limit, injected$tm_limit)
})

test_that("a real hung track probe warns through the verb that ran it", {
  # T2's anchor: FFprobe genuinely blocks on the FIFO's header, so this is the
  # warning a user gets, not one a mock arranged.
  skip_if_no_ffprobe()
  blocked <- local_blocking_input()
  withr::local_options(tidymedia.timeout = 2)
  expect_warning(
    count_audio_streams_all(blocked),
    class = "tidymedia_probe_timeout"
  )
})

test_that("a real hung batch job warns rather than reporting a bare failure", {
  # T9's anchor. The row still reads `success = FALSE`, as every _batch verb's
  # @return documents; what is new is being told why.
  blocked <- local_blocking_input()
  out <- withr::local_tempfile(fileext = ".mp4")
  withr::local_options(tidymedia.timeout = 2)
  jobs <- tibble::tibble(input = blocked, output = out)
  expect_warning(
    res <- ffm_batch(jobs, function(input, output, ...) ffm_files(input, output)),
    class = "tidymedia_batch_timeout"
  )
  expect_identical(res$success, FALSE)
})

# T4: no sentinel on a public return ------------------------------------------

# `@param parallel` promises "Output is identical either way, rows included and
# in the same order". A `tm_timed_out` attribute on probe_all()'s return broke
# that the moment one path set it, and an attribute on a public return survives
# every documented operation the caller might do to the list.

test_that("probe_all() carries no timeout attribute, hung or not", {
  local_probe_timeout(hit = "a.mkv")
  testthat::local_mocked_bindings(
    probe_one = function(file) {
      if (file == "a.mkv") {
        structure(list(program = "FFprobe", limit = 2),
                  class = "tidymedia_absorbed_timeout")
      } else {
        list(container = tibble::tibble(duration = "1"),
             streams = tibble::tibble(codec_name = "h264"))
      }
    },
    .package = "tidymedia"
  )
  out <- suppressWarnings(probe_all(c("a.mkv", "b.mkv")))
  expect_null(attr(out, "tm_timed_out"))
  expect_identical(names(attributes(out)), "names")
})

test_that("the parallel and sequential returns are identical with a hung file", {
  skip_if_not_installed("furrr")
  testthat::local_mocked_bindings(
    probe_one = function(file) {
      if (file == "a.mkv") {
        structure(list(program = "FFprobe", limit = 2),
                  class = "tidymedia_absorbed_timeout")
      } else {
        list(container = tibble::tibble(duration = "1"),
             streams = tibble::tibble(codec_name = "h264"))
      }
    },
    .package = "tidymedia"
  )
  files <- c("a.mkv", "b.mkv")
  seq_out <- suppressWarnings(probe_all(files, parallel = FALSE))
  par_out <- suppressWarnings(probe_all(files, parallel = TRUE))
  expect_identical(seq_out, par_out)
})

test_that("verify_media() still refuses on a hung probe, through its own path", {
  # The refusal now comes from the shared probe body re-raising under
  # `absorb = FALSE`, not from an attribute verify_media() reads off the return.
  testthat::local_mocked_bindings(
    probe_one = function(file) {
      structure(list(program = "FFprobe", limit = 2),
                class = "tidymedia_absorbed_timeout")
    },
    .package = "tidymedia"
  )
  dir <- withr::local_tempdir()
  f <- file.path(dir, "a.mkv")
  file.create(f)
  err <- expect_error(verify_media(f, width = 1920),
                      class = "tidymedia_timeout")
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "FFprobe", fixed = TRUE)
  # And it refuses rather than reporting every property as a mismatch.
  expect_no_match(msg, "expected", fixed = TRUE)
})

test_that("probe_all() still keeps the NA row and warns for a hung file", {
  # The absorbing half is unchanged: one hung file must not discard a corpus.
  testthat::local_mocked_bindings(
    probe_one = function(file) {
      if (file == "a.mkv") {
        structure(list(program = "FFprobe", limit = 2),
                  class = "tidymedia_absorbed_timeout")
      } else {
        list(container = tibble::tibble(duration = "1"),
             streams = tibble::tibble(codec_name = "h264"))
      }
    },
    .package = "tidymedia"
  )
  expect_warning(out <- probe_all(c("a.mkv", "b.mkv")), "timed out")
  expect_identical(nrow(out$container), 2L)
  expect_true(is.na(out$container$duration[[1]]))
})

# T5: one literal per program -------------------------------------------------

test_that("each program is named by one literal across the abort paths", {
  # probe_one() said "ffprobe" while the Layer 0 hatch said "FFprobe", so the
  # same hung binary was named two ways depending on which door the caller came
  # through (M69 review J2).
  expect_identical(
    sort(unique(unname(tm_program_literals()))),
    c("FFmpeg", "FFprobe", "MediaInfo")
  )
})

test_that("the literal guard reddens when a literal VARIES, not just when cut", {
  # The mutation probe AC4 asks for. Deleting the assertion above would
  # re-certify the reader rather than the code, which is exactly the defect J2
  # found -- so the mutant changes the LITERAL and the reader has to see it.
  mutant <- list(
    faked = function(file) {
      run_program(find_ffprobe(), c("-i", file), program = "ffprobe")
    }
  )
  found <- tm_program_literals(mutant)
  expect_identical(unname(found), "ffprobe")
  expect_false(
    identical(sort(unique(unname(c(tm_program_literals(), found)))),
              c("FFmpeg", "FFprobe", "MediaInfo"))
  )
})

test_that("the guard reads the real bodies, not a list handed to it", {
  # Non-vacuity from the other side: the default argument must actually reach
  # the namespace, so a `fns` that defaulted to something empty would show here.
  found <- tm_program_literals()
  expect_gt(length(found), 5L)
  expect_true("probe_one" %in% names(found))
  expect_true("ffprobe" %in% names(found))
})

test_that("a real hung FFprobe is named the same through either door", {
  # AC4's stated evidence, against the real call path: probe_one() absorbs the
  # timeout into a sentinel and ffprobe() aborts, and the two have to agree
  # about what timed out.
  skip_if_no_ffprobe()
  blocked <- local_blocking_input()
  withr::local_options(tidymedia.timeout = 2)
  absorbed <- probe_one(blocked)
  hatch <- tryCatch(ffprobe(paste("-i", shQuote(blocked))),
                    error = function(e) e)
  expect_true(is_absorbed_timeout(absorbed))
  expect_s3_class(hatch, "tidymedia_timeout")
  expect_identical(absorbed$program, hatch$tm_program)
})

# T6: the docs state the uniform rule -----------------------------------------

# These replace M69's scoped-claim and no-warning-disclosure guards, retired in
# test-runtime-timeout.R with the disclosure they fenced. M69 had to describe
# three behaviors and say the description was not a partition, because nothing
# enumerated its domain. The rule is uniform now, and the sweep is what lets the
# docs say so.

test_that("both docs state that a reached limit is never silent", {
  src <- doc_timeout_sources()
  skip_if(is.null(src$rd) || is.null(src$news), "docs not available")
  for (nm in c("rd", "news")) {
    txt <- src[[nm]]
    expect_match(txt, "never silent", info = nm)
    # Both halves named, because a reader acting on an NA row needs the second
    # as much as a reader catching an abort needs the first.
    expect_match(txt, "abort", info = nm)
    expect_match(txt, "warn", info = nm)
    # And the claim that the lists are derived rather than recalled, which is
    # the only reason the uniform rule can be stated at all.
    expect_match(txt, "call graph", fixed = TRUE, info = nm)
  }
})

test_that("M69's disclosure is gone from both docs", {
  # The retired half. Restoring the three-way description -- or the sentence
  # that admitted it was not a partition -- reddens here.
  src <- doc_timeout_sources()
  skip_if(is.null(src$rd) || is.null(src$news), "docs not available")
  for (nm in c("rd", "news")) {
    txt <- src[[nm]]
    expect_no_match(txt, "no warning", info = nm)
    expect_no_match(txt, "not a complete", info = nm)
    expect_no_match(txt, "three answers", info = nm)
    # The internal names M69 had to expose because it could not describe the
    # behavior any other way. A user cannot call either of them.
    expect_no_match(txt, "count_audio_streams", fixed = TRUE, info = nm)
    expect_no_match(txt, "tool_versions", fixed = TRUE, info = nm)
  }
})

test_that("the doc guard reddens on the text it fences, not on its absence", {
  # Mutation probe. Every assertion above is a substring grep, and a substring
  # grep is how M69's over-broad claim shipped green once already: "abort" was
  # present the whole time the doc said EVERY timed-out call aborts. So the
  # probe restores the retired sentences into a stand-in text and checks the
  # same assertions fail on it.
  mutant <- paste(
    "What a reached limit does depends on the call, and there are three",
    "answers rather than two. Two internal paths absorb it with no warning at",
    "all: count_audio_streams() and tool_versions(). These lists are not a",
    "complete partition of the package."
  )
  expect_no_match(mutant, "never silent")
  expect_match(mutant, "no warning")
  expect_match(mutant, "not a complete")
  expect_match(mutant, "count_audio_streams", fixed = TRUE)
  expect_no_match(mutant, "call graph", fixed = TRUE)
})

test_that("probe_all() still blames itself for its own argument refusals", {
  # T4 moved probe_all()'s body into probe_all_impl(). Without the threaded
  # `call`, every refusal here would read "Error in `probe_all_impl()`" and name
  # a function no caller can reach (M64/M65).
  # BY NAME: do.call() on the function object records the object itself as the
  # condition call and hides the blame target this guard exists to watch
  # (helper-blame.R).
  for (bad in list(list(1), list("x", typed = "no"), list("x", parallel = 1))) {
    err <- tryCatch(do.call("probe_all", bad, envir = asNamespace("tidymedia")),
                    error = function(e) e)
    expect_identical(blamed_verb(err), "probe_all")
  }
})
