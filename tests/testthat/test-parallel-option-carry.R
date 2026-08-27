# The carrier itself, in one process ------------------------------------------
#
# These run everywhere: they are about what carry_options() does to an option
# list, which needs no worker. The parallel cases below need a plan and a fake
# binary and skip accordingly.

test_that("carry_options() runs .f under the values captured in the parent", {
  withr::local_options(
    tidymedia.timeout = 42, tidymedia.nvenc_encoders = "h264_nvenc"
  )
  seen <- carry_options(function() {
    list(
      timeout = getOption("tidymedia.timeout"),
      encoders = getOption("tidymedia.nvenc_encoders")
    )
  })
  # Change the parent AFTER the wrap: what the wrapper installs must be what
  # was captured, not what happens to be set when the mapped call runs.
  options(tidymedia.timeout = 7, tidymedia.nvenc_encoders = "hevc_nvenc")
  expect_equal(seen(), list(timeout = 42, encoders = "h264_nvenc"))
})

test_that("carry_options() restores the prior values when .f returns", {
  withr::local_options(
    tidymedia.timeout = 42, tidymedia.nvenc_encoders = "h264_nvenc"
  )
  wrapped <- carry_options(function() NULL)
  withr::local_options(
    tidymedia.timeout = 7, tidymedia.nvenc_encoders = "hevc_nvenc"
  )
  wrapped()
  expect_equal(getOption("tidymedia.timeout"), 7)
  expect_equal(getOption("tidymedia.nvenc_encoders"), "hevc_nvenc")
})

test_that("carry_options() restores the prior values when .f aborts", {
  withr::local_options(
    tidymedia.timeout = 42, tidymedia.nvenc_encoders = "h264_nvenc"
  )
  wrapped <- carry_options(function() {
    cli::cli_abort("mapped call failed", class = "tm_test_failure")
  })
  withr::local_options(
    tidymedia.timeout = 7, tidymedia.nvenc_encoders = "hevc_nvenc"
  )
  expect_error(wrapped(), class = "tm_test_failure")
  expect_equal(getOption("tidymedia.timeout"), 7)
  expect_equal(getOption("tidymedia.nvenc_encoders"), "hevc_nvenc")
})

test_that("an option unset in the parent is unset for the mapped call", {
  withr::local_options(tidymedia.nvenc_encoders = NULL)
  wrapped <- carry_options(function() {
    # getOption()'s default only applies when the name is absent from the
    # option list, so this distinguishes "unset" from "set to NULL".
    getOption("tidymedia.nvenc_encoders", default = "absent")
  })
  withr::local_options(tidymedia.nvenc_encoders = "hevc_nvenc")
  expect_equal(wrapped(), "absent")
  expect_equal(getOption("tidymedia.nvenc_encoders"), "hevc_nvenc")
})

test_that("an unset timeout is carried as the no-limit sentinel", {
  withr::local_options(tidymedia.timeout = NULL)
  wrapped <- carry_options(function() getOption("tidymedia.timeout"))
  expect_equal(wrapped(), 0)
})

test_that("a bad limit is refused when the carrier is built, before .f runs", {
  ran <- FALSE
  for (bad in list(0.5, -1, NA, "2")) {
    withr::local_options(tidymedia.timeout = bad)
    expect_error(
      carry_options(function() ran <<- TRUE),
      class = "rlang_error"
    )
  }
  expect_false(ran)
})

# The parallel harness --------------------------------------------------------
#
# Everything below needs three things the machine may not have: a POSIX shell
# for the fake binaries, a `future` cluster, and workers running the same
# tidymedia source this file is testing. Each is a skip, and the milestone's
# evidence comes from a run where none of them fired.

# tm_fake_programs(): stand-in `ffmpeg` and `ffprobe` that log and then hang.
#
# Every invocation is appended to one shared log, so a spawn inside a worker is
# visible from the parent afterwards. `-encoders` answers immediately -- the
# encoder probe is a question, not a run, and a batch that only compiles must
# not take 30 seconds to prove it asked. Everything else sleeps well past any
# limit these tests set, which is what makes the kill observable.
tm_fake_programs <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  log <- file.path(dir, "calls.log")
  for (prog in c("ffmpeg", "ffprobe")) {
    path <- file.path(dir, prog)
    writeLines(
      c(
        "#!/bin/sh",
        sprintf('printf "%%s %%s\\n" "%s" "$*" >> "%s"', prog, log),
        "case \" $* \" in",
        "  *-encoders*)",
        "    echo ' ------'",
        "    echo ' V....D h264_nvenc           NVIDIA NVENC H.264 encoder'",
        "    echo ' V....D hevc_nvenc           NVIDIA NVENC hevc encoder'",
        "    exit 0 ;;",
        "esac",
        "sleep 30"
      ),
      path
    )
    Sys.chmod(path, "0755")
  }
  list(dir = dir, log = log)
}

# tm_fake_calls(): the fake binaries' log, one line per invocation.
tm_fake_calls <- function(fake) {
  if (!file.exists(fake$log)) return(character(0))
  readLines(fake$log, warn = FALSE)
}

# tm_carry_fingerprint(): what the carrier looks like in THIS process.
#
# `devtools::test()` runs the parent against the source and the workers against
# the INSTALLED package, so a worker can silently be running last release's
# code. Comparing the carrier's own source between the two is the only check
# that catches that; a version number would not, since a dev version is
# installed too.
tm_carry_fingerprint <- function() {
  ns <- tryCatch(asNamespace("tidymedia"), error = function(e) NULL)
  if (is.null(ns)) return(NA_character_)
  paste(
    vapply(
      c("carry_options", "carried_option_values"),
      function(f) {
        tryCatch(
          paste(deparse(body(get(f, envir = ns))), collapse = ""),
          error = function(e) NA_character_
        )
      },
      character(1)
    ),
    collapse = "\n"
  )
}

# local_carry_harness(): fake binaries first on PATH, then a fresh two-worker
# cluster, then two things asserted before any cell is trusted.
#
# The cluster is booted AFTER the PATH edit and is built fresh for each test
# rather than reusing `future`'s cached one, for two reasons: a cached cluster
# was started under the real PATH and would never see the fakes, and a worker
# that has already answered an encoder probe carries that memo into the next
# test, which is exactly what the AC2 control counts.
local_carry_harness <- function(env = parent.frame(), workers = 2L) {
  testthat::skip_on_cran()
  testthat::skip_on_os("windows")
  testthat::skip_if_not_installed("furrr")
  testthat::skip_if_not_installed("future")
  testthat::skip_if_not_installed("parallelly")

  fake <- tm_fake_programs(env)
  withr::local_path(fake$dir, action = "prefix", .local_envir = env)

  cl <- parallelly::makeClusterPSOCK(workers)
  withr::defer(parallel::stopCluster(cl), envir = env)
  old_plan <- future::plan(future::cluster, workers = cl)
  withr::defer(future::plan(old_plan), envir = env)

  probes <- seq_len(workers * 2L)
  opts <- furrr::furrr_options(chunk_size = 1, seed = TRUE)

  fingerprints <- unlist(furrr::future_map_chr(
    probes, function(i) tm_carry_fingerprint(), .options = opts
  ))
  if (!all(fingerprints == tm_carry_fingerprint())) {
    testthat::skip(
      "workers load an installed tidymedia whose carrier differs from the source"
    )
  }

  # Not a skip: the PATH edit either reached the workers or the fixture is
  # broken, and a green run on the real FFmpeg would be evidence of nothing.
  seen <- unlist(furrr::future_map_chr(
    probes, function(i) unname(Sys.which("ffmpeg")), .options = opts
  ))
  # Sys.which() resolves symlinks (macOS's /var -> /private/var), so compare
  # realpaths rather than strings.
  testthat::expect_equal(
    unique(seen), normalizePath(file.path(fake$dir, "ffmpeg"))
  )

  fake
}

# tm_batch_jobs(): an N-row jobs table over readable inputs.
tm_batch_jobs <- function(n, env = parent.frame()) {
  tibble::tibble(
    input = vapply(seq_len(n), function(i) make_input(env = env), character(1)),
    output = file.path(withr::local_tempdir(.local_envir = env),
                       sprintf("out%d.mp4", seq_len(n)))
  )
}

tm_plain_pipeline <- function(input, output, ...) {
  ffm_files(input, output)
}

# AC1 -- the limit reaches the worker, in each site's own shape ----------------

test_that("a limit set in the parent kills FFmpeg inside an ffm_batch worker", {
  fake <- local_carry_harness()
  jobs <- tm_batch_jobs(2)
  withr::local_options(tidymedia.timeout = 1)

  expect_warning(
    out <- ffm_batch(jobs, function(input, output, ...) {
      tidymedia::ffm_files(input, output)
    }, run = TRUE, parallel = TRUE),
    class = "tidymedia_batch_timeout"
  )

  expect_equal(out$success, rep(FALSE, 2))
  # The kill has to be what the worker actually attempted, not merely a FALSE:
  # the fake logs every invocation, so an empty log would mean the row failed
  # for some other reason entirely.
  expect_true(any(grepl("^ffmpeg ", tm_fake_calls(fake))))
})

test_that("a limit set in the parent kills FFprobe inside a probe_all worker", {
  fake <- local_carry_harness()
  files <- c(make_input(), make_input())
  withr::local_options(tidymedia.timeout = 1)

  expect_warning(
    res <- probe_all(files, parallel = TRUE),
    regexp = "timed out rather than being unreadable"
  )

  expect_equal(nrow(res$container), 2L)
  expect_equal(sort(res$container$file), sort(files))
  # An NA row: `file` is the only column a file that never answered can fill.
  expect_equal(setdiff(names(res$container), "file"), character(0))
  expect_true(any(grepl("^ffprobe ", tm_fake_calls(fake))))
})

test_that("a limit set in the parent aborts a two-pass loudnorm worker", {
  fake <- local_carry_harness()
  jobs <- tm_batch_jobs(2)
  withr::local_options(tidymedia.timeout = 1)

  expect_error(
    normalize_audio_batch(jobs, two_pass = TRUE, parallel = TRUE),
    class = "tidymedia_timeout"
  )
  expect_true(any(grepl("^ffmpeg ", tm_fake_calls(fake))))
})

test_that("every parallel fan-out in the package has a case above", {
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "package sources are not on disk in this run")

  files <- list.files(r_dir, pattern = "[.][Rr]$", full.names = TRUE)
  sites <- unlist(lapply(files, function(f) {
    hits <- grep("furrr::future_", readLines(f, warn = FALSE), value = FALSE)
    if (!length(hits)) return(character(0))
    paste0(basename(f), ":", hits)
  }))
  # The domain must not be silently empty: a rename of furrr's call would leave
  # this guard green over nothing.
  expect_gt(length(sites), 0)

  covered <- c(
    "ffm_batch.R" = "pipeline build (AC2: the encoder probe is its only spawn)",
    "ffm_batch.R" = "job run (AC1)",
    "ffprobe.R" = "probe_all (AC1)",
    "loudnorm_two_pass.R" = "loudnorm phase 1 (AC1)"
  )
  expect_setequal(sub(":.*$", "", sites), unique(names(covered)))
  expect_equal(length(sites), length(covered))
})
