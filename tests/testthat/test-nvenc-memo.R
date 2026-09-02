# M67: the encoder-name pool is asked of FFmpeg once per session, not once per
# call. Counting at ffmpeg_encoders() rather than at find_ffmpeg(): ffmpeg()
# shells out through system(), not run_program(), so that is the seam where the
# binary would actually be consulted (the test-nvenc-docs.R:71-75 pattern).

test_that("the encoder pool is asked for once per session, across codecs", {
  # AC1. Two codecs, so a per-codec memo fails this: h264 warms it, hevc must
  # not re-ask, and the repeated h264 must not either.
  forget_ffmpeg_capabilities()
  withr::defer(forget_ffmpeg_capabilities())
  withr::local_options(tidymedia.hardware_encoders = NULL) # force the real probe
  probes <- 0L
  local_mocked_bindings(
    ffmpeg_encoders = function(...) {
      probes <<- probes + 1L
      tibble::tibble(name = c("h264_nvenc", "hevc_nvenc"))
    }
  )

  expect_true(has_hardware_encoder("h264", "nvenc"))
  expect_identical(probes, 1L)
  expect_true(has_hardware_encoder("hevc", "nvenc"))
  expect_identical(probes, 1L)
  expect_true(has_hardware_encoder("h264", "nvenc"))
  expect_identical(probes, 1L)
})

test_that("a cold memo still reaches FFmpeg", {
  # The control for the test above: `probes == 1L` there must mean "asked once",
  # never "the mock never bound". A discarded memo re-asks.
  forget_ffmpeg_capabilities()
  withr::defer(forget_ffmpeg_capabilities())
  withr::local_options(tidymedia.hardware_encoders = NULL)
  probes <- 0L
  local_mocked_bindings(
    ffmpeg_encoders = function(...) {
      probes <<- probes + 1L
      tibble::tibble(name = "h264_nvenc")
    }
  )

  has_hardware_encoder("h264", "nvenc")
  expect_identical(probes, 1L)
  forget_ffmpeg_capabilities()
  has_hardware_encoder("h264", "nvenc")
  expect_identical(probes, 2L)
})

test_that("the option seam is read before the memo, so it wins mid-session", {
  # AC6. The memo sits BELOW the getOption() seam: a warm memo must not be able
  # to override an option set after it was warmed, and consulting the option
  # must not consult -- or populate -- the memo.
  forget_ffmpeg_capabilities()
  withr::defer(forget_ffmpeg_capabilities())
  withr::local_options(tidymedia.hardware_encoders = NULL)
  probes <- 0L
  local_mocked_bindings(
    ffmpeg_encoders = function(...) {
      probes <<- probes + 1L
      tibble::tibble(name = "h264_nvenc")
    }
  )

  expect_true(has_hardware_encoder("h264", "nvenc")) # warms the memo
  expect_identical(probes, 1L)

  withr::local_options(tidymedia.hardware_encoders = character(0))
  expect_false(has_hardware_encoder("h264", "nvenc"))
  expect_identical(probes, 1L)
})

test_that("refresh_ffmpeg_capabilities() sends the next call back to FFmpeg", {
  # AC4, route one: the exported discard.
  probes <- local_encoder_probe_counter()

  expect_true(has_hardware_encoder("h264", "nvenc"))
  expect_identical(probes(), 1L)
  expect_true(has_hardware_encoder("h264", "nvenc"))
  expect_identical(probes(), 1L)

  expect_null(refresh_ffmpeg_capabilities())
  expect_true(has_hardware_encoder("h264", "nvenc"))
  expect_identical(probes(), 2L)
})

test_that("set_program() sends the next call back to FFmpeg", {
  # AC4, route two: repointing tidymedia at a binary discards what was
  # remembered about the previous one. The config dir is redirected to a
  # tempdir so the test never writes to the user's real configuration:
  # R_USER_CONFIG_DIR is what tools::R_user_dir() reads (M097), where the
  # rappdirs mock this once used would still resolve and redirect nothing.
  ffmpeg_path <- Sys.which("ffmpeg")
  skip_if(!nzchar(ffmpeg_path), "ffmpeg not available")

  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  probes <- local_encoder_probe_counter()

  expect_true(has_hardware_encoder("h264", "nvenc"))
  expect_identical(probes(), 1L)
  expect_true(has_hardware_encoder("h264", "nvenc"))
  expect_identical(probes(), 1L)

  set_program("ffmpeg", unname(ffmpeg_path))
  expect_true(has_hardware_encoder("h264", "nvenc"))
  expect_identical(probes(), 2L)
})

test_that("ffmpeg_encoders() itself stays uncached", {
  # AC5. The memo is has_hardware_encoder()'s, not ffmpeg_encoders()'. A caller who wants a
  # fresh answer about this build always has one: the exported query reaches the
  # binary every time it is called. Counted by mocking ffmpeg() -- the execution
  # seam ffmpeg_encoders() actually reaches, which shells out via system() and
  # so is invisible to a run_program() mock.
  shells <- 0L
  local_mocked_bindings(
    ffmpeg = function(command) {
      shells <<- shells + 1L
      c(
        "Encoders:",
        " ------",
        " V....D h264_nvenc           NVIDIA NVENC H.264 encoder"
      )
    }
  )

  expect_identical(ffmpeg_encoders()$name, "h264_nvenc")
  expect_identical(shells, 1L)
  expect_identical(ffmpeg_encoders()$name, "h264_nvenc")
  expect_identical(shells, 2L)
})

test_that("an option set before the memo is warmed never warms it", {
  # The other half of AC6: a session that only ever sets the option leaves the
  # memo cold, so unsetting it later still reaches FFmpeg.
  forget_ffmpeg_capabilities()
  withr::defer(forget_ffmpeg_capabilities())
  probes <- 0L
  local_mocked_bindings(
    ffmpeg_encoders = function(...) {
      probes <<- probes + 1L
      tibble::tibble(name = "h264_nvenc")
    }
  )

  withr::with_options(
    list(tidymedia.hardware_encoders = "h264_nvenc"),
    expect_true(has_hardware_encoder("h264", "nvenc"))
  )
  expect_identical(probes, 0L)
  expect_identical(ls(.tm_capabilities), character())

  withr::local_options(tidymedia.hardware_encoders = NULL)
  expect_true(has_hardware_encoder("h264", "nvenc"))
  expect_identical(probes, 1L)
})
