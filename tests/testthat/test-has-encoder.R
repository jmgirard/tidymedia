# M138 AC3: has_encoder() answers whether this FFmpeg build has one encoder.
# Each clause of that criterion is a test of its own below.
#
# The absent-encoder state is not observable on a machine whose build has the
# encoder, so the pool is mocked here rather than hidden. The committed wrapper
# `tools/ffmpeg_build_model/ffmpeg` is the other instrument, and it is what the
# check and suite runs of AC2 use; these tests are the CI-portable half.

test_that("has_encoder() reads the pool, so a build without an encoder answers FALSE", {
  # AC3's first clause. The mock is the whole instrument: `libx264` is present
  # on the machine that wrote this, so a real probe could only ever answer TRUE.
  forget_ffmpeg_capabilities()
  withr::defer(forget_ffmpeg_capabilities())
  local_mocked_bindings(
    find_ffmpeg = function() "/usr/bin/ffmpeg",
    cached_encoder_names = function() c("libopenh264", "libmp3lame", "aac")
  )

  expect_false(has_encoder("libx264"))
  # The control: the same call against a name the pool does list. Without it a
  # predicate that answered FALSE for everything would pass the line above.
  expect_true(has_encoder("libopenh264"))
  expect_true(has_encoder("aac"))
})

test_that("has_encoder() answers one logical, and refuses a name that is not one string", {
  local_mocked_bindings(
    find_ffmpeg = function() "/usr/bin/ffmpeg",
    cached_encoder_names = function() "libx264"
  )

  answer <- has_encoder("libx264")
  expect_true(is.logical(answer))
  expect_length(answer, 1L)
  expect_null(names(answer))

  # Blamed on the function the caller typed, not on the internal that threads
  # `call` (D087). The vector form is the one `%in%` would silently answer for.
  cnd <- expect_error(has_encoder(c("libx264", "aac")), class = "rlang_error")
  expect_identical(rlang::call_name(conditionCall(cnd)), "has_encoder")
  expect_error(has_encoder(1), class = "rlang_error")
  expect_error(has_encoder(NA_character_), class = "rlang_error")
})

test_that("has_encoder() answers FALSE where no FFmpeg is found, and says nothing about it", {
  # AC3's no-location clause, run through the real find_program() rather than a
  # mock of it: PATH is emptied and both config libraries are redirected, so the
  # plain not-found branch is what answers.
  tm_redirect_config()
  forget_ffmpeg_capabilities()
  withr::defer(forget_ffmpeg_capabilities())
  # The premise, asserted rather than assumed.
  expect_null(suppressWarnings(find_ffmpeg()))

  expect_no_warning(answer <- has_encoder("libx264"))
  expect_false(answer)
})

test_that("has_encoder() still passes on a warning naming a location you can repair", {
  # AC3's D088 clause. A remembered location whose binary has gone is a machine
  # the caller can repair, so the answer FALSE must not arrive silently -- a
  # silent FALSE sends them off to pick a different encoder when the real fault
  # is the remembered path.
  dirs <- tm_redirect_config()
  forget_ffmpeg_capabilities()
  withr::defer(forget_ffmpeg_capabilities())
  gone <- file.path(withr::local_tempdir(), "gone", "ffmpeg")
  tm_write_location(dirs$new, "ffmpeg", gone)
  expect_false(file.exists(gone))

  cnd <- expect_warning(
    answer <- has_encoder("libx264"),
    class = "tidymedia_location_gone"
  )
  expect_identical(cnd$tm_program, "ffmpeg")
  expect_false(answer)
})

test_that("has_encoder() asks FFmpeg once per session, and asks again after a refresh", {
  # AC3's memo clause. Counted at ffmpeg_encoders() rather than at the spawn,
  # which is the seam test-nvenc-memo.R counts at and for the same reason.
  forget_ffmpeg_capabilities()
  withr::defer(forget_ffmpeg_capabilities())
  probes <- 0L
  local_mocked_bindings(
    find_ffmpeg = function() "/usr/bin/ffmpeg",
    ffmpeg_encoders = function(...) {
      probes <<- probes + 1L
      tibble::tibble(name = c("libx264", "aac"))
    }
  )

  expect_true(has_encoder("libx264"))
  expect_identical(probes, 1L)
  expect_true(has_encoder("aac"))
  expect_false(has_encoder("libx265"))
  expect_identical(probes, 1L)

  # The control for the count: a discarded memo re-asks, so `1L` above means
  # "asked once" rather than "the mock never bound".
  refresh_ffmpeg_capabilities()
  expect_true(has_encoder("libx264"))
  expect_identical(probes, 2L)
})

test_that("has_encoder() does not read the hardware-encoder option", {
  # AC3's option clause. `tidymedia.hardware_encoders` names the hardware pool
  # of the machine, so reading it here would answer FALSE for every software
  # encoder in any session that set it, and 144 test_that() blocks set it
  # (counted 2026-10-10 over `tests/testthat/*.R`).
  forget_ffmpeg_capabilities()
  withr::defer(forget_ffmpeg_capabilities())
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
  local_mocked_bindings(
    find_ffmpeg = function() "/usr/bin/ffmpeg",
    cached_encoder_names = function() c("libx264", "aac")
  )

  expect_true(has_encoder("libx264"))
  expect_false(has_encoder("h264_nvenc"))
  # And the sibling still reads the option, so the two questions stay apart.
  expect_true(has_hardware_encoder("h264", "nvenc"))
})

test_that("has_encoder() refuses an unusable session limit in its own name", {
  # AC3's D074/D075 clause. The limit is read above the location question, so
  # the refusal does not depend on whether this machine has FFmpeg.
  withr::local_options(tidymedia.timeout = -1)
  # The memo mock records whether it was reached, so this test fails if the
  # limit were read after the memo rather than before it (M138 review).
  memo_read <- FALSE
  local_mocked_bindings(
    find_ffmpeg = function() "/usr/bin/ffmpeg",
    cached_encoder_names = function() {
      memo_read <<- TRUE
      "libx264"
    }
  )

  cnd <- expect_error(has_encoder("libx264"), class = "rlang_error")
  expect_match(conditionMessage(cnd), "tidymedia.timeout", fixed = TRUE)
  expect_identical(rlang::call_name(conditionCall(cnd)), "has_encoder")
  expect_false(memo_read)
})

test_that("has_encoder() refuses an unusable session limit on a machine with no FFmpeg", {
  # The other half of the clause above: the refusal is machine-independent,
  # which is what siting the limit read above the location question buys.
  tm_redirect_config()
  withr::local_options(tidymedia.timeout = "soon")

  cnd <- expect_error(has_encoder("libx264"), class = "rlang_error")
  expect_match(conditionMessage(cnd), "tidymedia.timeout", fixed = TRUE)
})
