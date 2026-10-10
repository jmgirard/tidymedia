# M138 AC5 and AC6: a remembered location is read from the filesystem, not from
# whatever this machine's `which` does with a path, and a file that cannot be
# run is told apart from one that is not there.
#
# AC5's instrument is a stipulated model, not a measurement. CRAN's two Fedora
# check flavors answered `Sys.which()` with `""` for absolute stub executables
# the suite had just created, which failed 82 tests through this seam; the log
# is quoted in M138's record. `Sys.which()` on this machine does answer for an
# absolute path, so the failure mode is reproducible here only by stipulating
# it. The r-hub Fedora run is what measures the real machines.

# `Sys.which()` as the Fedora images answered it: nothing for a path, the real
# answer for a bare name. The original function object is captured before the
# binding is replaced, so the bare-name branch is the real one and the mock
# cannot recurse into itself.
tm_local_pathless_which <- function(env = parent.frame()) {
  real_which <- get("Sys.which", envir = baseenv())
  testthat::local_mocked_bindings(
    Sys.which = function(names) {
      answers <- ifelse(
        grepl("[/\\\\]", names), "", unname(real_which(names))
      )
      stats::setNames(answers, names)
    },
    .package = "base",
    .env = env
  )
}

test_that("a remembered location is still found where `which` answers nothing for a path", {
  # AC5. On the pre-M138 code both assertions about `find_ffmpeg()` failed:
  # `Sys.which(location) == ""` sent it down the gone branch, so it warned
  # `tidymedia_location_gone` and returned NULL for a stub that was there. The
  # two lines above them are premises about the model and the filesystem, and
  # they passed at the merge base too, which is what makes them premises.
  dirs <- tm_redirect_config()
  stub <- tm_stub_executable()
  tm_write_location(dirs$new, "ffmpeg", stub)
  tm_local_pathless_which()
  # The model does what the test needs it to do.
  expect_identical(unname(Sys.which(stub)), "")
  expect_true(file.exists(stub))

  expect_no_warning(found <- find_ffmpeg())
  expect_identical(found, stub)
})

test_that("set_program() and tm_usable_binary() agree with the filesystem, not with `which`", {
  # AC5's other two readers. Both answered the Fedora images' `""` before M138:
  # set_program() aborted `tidymedia_program_not_found` for a file that was
  # there, and install_on_win() refused a good build as unusable.
  #
  # The install-side reader is asserted under `tm_local_windows()`, which holds
  # `tm_os()` at "windows", because that is the platform install_on_win() runs
  # on and the one whose gate (D086) refuses the call outright here. The helper
  # itself is platform-free, so the mock changes nothing about what it answers;
  # it is what makes the assertion the install's own reading rather than a
  # neighbouring one.
  dirs <- tm_redirect_config()
  stub <- tm_stub_executable()
  tm_local_pathless_which()

  expect_true(set_program("ffmpeg", stub, confirm = FALSE))
  expect_identical(readLines(tm_config_file("ffmpeg", dirs$new)), stub)

  tm_local_windows()
  expect_identical(tm_os(), "windows")
  expect_true(tm_usable_binary(stub))
  expect_identical(tm_program_state(stub), "runnable")
})

test_that("a bare name is still searched for on PATH", {
  # The other half of the split: a location holding no directory separator is a
  # NAME, and `Sys.which()` is the instrument for that question. The model above
  # leaves bare names alone, so this is the real search.
  skip_on_os("windows")
  stub <- tm_stub_executable(name = "onpath")
  withr::local_path(dirname(stub))
  tm_local_pathless_which()

  expect_identical(tm_program_state(basename(stub)), "runnable")
  expect_true(tm_runnable_program(basename(stub)))
  # And a bare name nothing on PATH answers for is still absent.
  expect_identical(tm_program_state("tm-no-such-program"), "absent")
})

test_that("a remembered location that cannot be run says so, and is not reported as gone", {
  # AC6. Before M138 both states answered `Sys.which() == ""` and both were
  # reported as "no longer seems to exist", which sent the caller looking for a
  # file that was in front of them.
  skip_on_os("windows")
  dirs <- tm_redirect_config()
  dir <- withr::local_tempdir()
  unrunnable <- file.path(dir, "ffmpeg")
  writeLines("#!/bin/sh", unrunnable)
  Sys.chmod(unrunnable, "0644")
  # The premises: a real, non-empty file, and the bit is what decides.
  expect_true(file.exists(unrunnable))
  expect_gt(file.size(unrunnable), 0)
  skip_if_not(file.access(unrunnable, 1L) != 0L,
              "the fixture is still executable after chmod 0644")
  tm_write_location(dirs$new, "ffmpeg", unrunnable)

  cnd <- expect_warning(
    found <- find_ffmpeg(),
    class = "tidymedia_location_not_executable"
  )
  expect_null(found)
  expect_identical(cnd$tm_program, "ffmpeg")
  expect_identical(cnd$tm_location, unrunnable)
  expect_match(cli::ansi_strip(conditionMessage(cnd)), "cannot be run",
               fixed = TRUE)
  # The wording the old class owns is NOT what this one says.
  expect_no_match(cli::ansi_strip(conditionMessage(cnd)),
                  "no longer seems to exist", fixed = TRUE)
})

test_that("a remembered location that is absent still reports as gone", {
  # AC6's other arm, and the control for the test above: the split must not have
  # moved every refusal onto the new class.
  dirs <- tm_redirect_config()
  gone <- file.path(withr::local_tempdir(), "gone", "ffmpeg")
  expect_false(file.exists(gone))
  tm_write_location(dirs$new, "ffmpeg", gone)

  cnd <- expect_warning(
    found <- find_ffmpeg(),
    class = "tidymedia_location_gone"
  )
  expect_null(found)
  expect_match(cli::ansi_strip(conditionMessage(cnd)),
               "no longer seems to exist", fixed = TRUE)
})

test_that("program_status() surfaces the not-runnable location", {
  # The new class joins the audible partition, decided rather than inherited: a
  # file the caller can repair is exactly what D088 says must not arrive as a
  # silent NA.
  skip_on_os("windows")
  dirs <- tm_redirect_config()
  dir <- withr::local_tempdir()
  unrunnable <- file.path(dir, "ffmpeg")
  writeLines("#!/bin/sh", unrunnable)
  Sys.chmod(unrunnable, "0644")
  skip_if_not(file.access(unrunnable, 1L) != 0L,
              "the fixture is still executable after chmod 0644")
  tm_write_location(dirs$new, "ffmpeg", unrunnable)

  got <- tm_collect_warnings(program_status())
  classes <- vapply(got$warnings, function(w) class(w)[[1]], character(1))
  expect_true("tidymedia_location_not_executable" %in% classes)
  expect_true(is.na(got$value$location[got$value$program == "ffmpeg"]))
})
