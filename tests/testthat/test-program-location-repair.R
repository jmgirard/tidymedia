# What find_program() does with a remembered location it cannot use, and what
# program_status() lets the caller hear about it (M116). The fixtures --
# tm_redirect_config(), tm_stub_executable(), tm_write_location,
# tm_program_vocabulary -- live in helper-program-config.R.

# One find_*() export per program name, so the tests below exercise the four
# exported doors rather than the shared body four times.
tm_find_export <- list(
  ffmpeg = find_ffmpeg,
  ffprobe = find_ffprobe,
  ffplay = find_ffplay,
  mediainfo = find_mediainfo
)

# The two shapes a config file can hold that are not one location. Written
# through writeLines()/file.create() rather than set_program(), because
# set_program() cannot produce either -- which is why the state was only ever
# reachable by hand-editing the file or by a truncated write.
tm_malformed_forms <- list(
  empty = function(path) file.create(path),
  two_lines = function(path) writeLines(c("/one/place", "/another/place"), path)
)


# find_program(): a config file that is not one location ----------------------

test_that("a config file that is not one location warns and returns NULL", {
  # AC1. Both axes crossed: each of the four exported find_*() doors against
  # each of the two malformed shapes. Before M116 the empty form reached
  # `if (logical(0))` ("argument is of length zero") and the two-line form
  # `if (c(FALSE, FALSE))` ("the condition has length > 1"), so every cell here
  # aborted rather than warned.
  for (program in tm_program_vocabulary) {
    for (form in names(tm_malformed_forms)) {
      dirs <- tm_redirect_config()
      path <- tm_config_file(program, dirs$new)
      tm_malformed_forms[[form]](path)
      info <- paste(program, form)

      condition <- tryCatch(
        tm_find_export[[program]](),
        warning = function(w) w
      )
      expect_s3_class(condition, "tidymedia_location_unreadable")
      expect_identical(condition$tm_program, program, info = info)
      expect_identical(condition$tm_file, path, info = info)

      expect_null(suppressWarnings(tm_find_export[[program]]()), info = info)
    }
  }
})

test_that("the unreadable-location warning names the file and both repairs", {
  # The message, pinned once rather than in every cell above. It must name the
  # file the caller has to fix, and both calls that fix it: unset_program()
  # removes the remembered location, set_program() replaces it.
  dirs <- tm_redirect_config()
  path <- tm_config_file("ffmpeg", dirs$new)
  file.create(path)

  condition <- tryCatch(find_ffmpeg(), warning = function(w) w)
  message <- cli::ansi_strip(conditionMessage(condition))

  expect_match(message, "ffmpeg", fixed = TRUE)
  expect_match(message, basename(path), fixed = TRUE)
  expect_match(message, "unset_ffmpeg()", fixed = TRUE)
  expect_match(message, "set_ffmpeg()", fixed = TRUE)
})

test_that("a malformed legacy file is read, and a readable current file wins", {
  # find_program() falls back to the pre-0.2.0 directory only when no current
  # file exists, so the guard has to hold on that arm too -- and must not fire
  # when the current file is fine and only the legacy one is broken.
  dirs <- tm_redirect_config()
  file.create(tm_config_file("ffmpeg", dirs$legacy))
  condition <- tryCatch(find_ffmpeg(), warning = function(w) w)
  expect_s3_class(condition, "tidymedia_location_unreadable")
  expect_identical(condition$tm_file, tm_config_file("ffmpeg", dirs$legacy))

  stub <- tm_stub_executable()
  tm_write_location(dirs$new, "ffmpeg", stub)
  expect_identical(expect_no_warning(find_ffmpeg()), stub)
})


# find_program(): a remembered location whose binary is gone ------------------

# A path Sys.which() cannot resolve under the emptied PATH, absolute so no
# search enters the answer. Never created on disk: this is the state where the
# remembered binary has been moved or deleted.
tm_vanished_location <- function(dirs, program) {
  file.path(dirs$root, "gone", paste0(program, "-binary"))
}

test_that("a remembered location whose binary is gone warns with its own class", {
  # AC2's class half, one cell per program. The location the caller has to
  # repair rides on the condition, so a handler can name it without parsing
  # the message.
  for (program in tm_program_vocabulary) {
    dirs <- tm_redirect_config()
    gone <- tm_vanished_location(dirs, program)
    tm_write_location(dirs$new, program, gone)

    condition <- tryCatch(tm_find_export[[program]](), warning = function(w) w)

    expect_s3_class(condition, "tidymedia_location_gone")
    expect_identical(condition$tm_program, program, info = program)
    expect_identical(condition$tm_location, gone, info = program)
    expect_null(suppressWarnings(tm_find_export[[program]]()), info = program)
  }
})

test_that("the gone-location warning offers both repairs, and the installer only where it runs", {
  # AC2's advice half, crossing operating system against program. The
  # set_program() advice M113 shipped stays; unset_program() is added because
  # it is the call that clears the remembered location itself; and the
  # install_on_win() offer appears on exactly the cells the not-found branch
  # offers it on -- Windows, and a program the installer registers. The
  # windows + mediainfo cell is the one that says the second half of that
  # condition is still being read.
  for (os in c("windows", "darwin", "linux")) {
    for (program in tm_program_vocabulary) {
      dirs <- tm_redirect_config()
      local_mocked_bindings(tm_os = function(...) os, .package = "tidymedia")
      tm_write_location(dirs$new, program, tm_vanished_location(dirs, program))
      info <- paste(os, program)

      condition <- tryCatch(tm_find_export[[program]](), warning = function(w) w)
      message <- cli::ansi_strip(conditionMessage(condition))

      expect_match(message, "no longer seems to exist", fixed = TRUE, info = info)
      expect_match(message, paste0("set_", program, "()"), fixed = TRUE, info = info)
      expect_match(message, paste0("unset_", program, "()"), fixed = TRUE, info = info)
      offered <- identical(os, "windows") && program %in% tm_install_registers
      if (offered) {
        expect_match(message, "install_on_win()", fixed = TRUE, info = info)
      } else {
        expect_no_match(message, "install_on_win()", fixed = TRUE, info = info)
      }
    }
  }
})

test_that("the not-found warning offers the installer on the same cells", {
  # The branch the offer was already on, re-asserted through the shared helper
  # rather than through the inline condition it replaced. Both branches must
  # answer alike, which is the whole reason there is one helper.
  for (os in c("windows", "darwin", "linux")) {
    for (program in tm_program_vocabulary) {
      tm_redirect_config()
      local_mocked_bindings(tm_os = function(...) os, .package = "tidymedia")
      info <- paste(os, program)

      condition <- tryCatch(tm_find_export[[program]](), warning = function(w) w)
      message <- cli::ansi_strip(conditionMessage(condition))

      expect_match(message, "Failed to find", fixed = TRUE, info = info)
      if (identical(os, "windows") && program %in% tm_install_registers) {
        expect_match(message, "install_on_win()", fixed = TRUE, info = info)
      } else {
        expect_no_match(message, "install_on_win()", fixed = TRUE, info = info)
      }
    }
  }
})


# unset_program(): the memo and a removal that only partly took ---------------

# Seed the capability memo and hand back a predicate for "still there". The
# real environment rather than a mock of forget_ffmpeg_capabilities(): what
# AC5 asks is that the memo is EMPTY afterwards, and a counter of calls to the
# dropper cannot say that.
tm_seed_capability_memo <- function(env = parent.frame()) {
  withr::defer(forget_ffmpeg_capabilities(), envir = env)
  .tm_capabilities$encoder_names <- c("h264_nvenc", "libx264")
  invisible(NULL)
}

tm_memo_is_empty <- function() {
  length(ls(.tm_capabilities, all.names = TRUE)) == 0L
}

test_that("a partial removal drops the memo before it aborts", {
  # AC5. Both forms: the current-directory file goes and the legacy one stays,
  # then the reverse. Either way find_program() now answers from a different
  # file than it did before the call, so what was memoized about the binary the
  # old one named cannot be trusted -- and the abort must not be what decides
  # that (D089).
  for (kept in c("new", "legacy")) {
    dirs <- tm_redirect_config()
    stub <- tm_stub_executable()
    tm_write_location(dirs$new, "ffmpeg", stub)
    tm_write_location(dirs$legacy, "ffmpeg", stub)
    survivor <- tm_config_file("ffmpeg", dirs[[kept]])
    tm_seed_capability_memo()
    expect_false(tm_memo_is_empty(), info = kept)

    # A seam that removes every file but one leaves that one exactly where a
    # permission failure would, which is M113's shape for firing this branch.
    local_mocked_bindings(
      tm_unlink = function(path, recursive = FALSE) {
        unlink(setdiff(path, survivor))
        0L
      },
      .package = "tidymedia"
    )

    condition <- tryCatch(unset_program("ffmpeg"), error = function(e) e)

    expect_s3_class(condition, "tidymedia_location_not_removed")
    expect_identical(condition$tm_files, survivor, info = kept)
    expect_true(file.exists(survivor), info = kept)
    expect_true(tm_memo_is_empty(), info = kept)
  }
})

test_that("a removal that took nothing leaves the memo alone", {
  # The other side of the same rule, and the case that says the drop is keyed
  # on what was removed rather than on the abort being raised: nothing about
  # the resolved binary changed, so re-probing it would cost a process spawn
  # for no reason.
  dirs <- tm_redirect_config()
  stub <- tm_stub_executable()
  tm_write_location(dirs$new, "ffmpeg", stub)
  tm_write_location(dirs$legacy, "ffmpeg", stub)
  tm_seed_capability_memo()
  local_mocked_bindings(
    tm_unlink = function(path, recursive = FALSE) 1L,
    .package = "tidymedia"
  )

  condition <- tryCatch(unset_program("ffmpeg"), error = function(e) e)

  expect_s3_class(condition, "tidymedia_location_not_removed")
  expect_length(condition$tm_files, 2L)
  expect_false(tm_memo_is_empty())
})

test_that("a removal that took everything still drops the memo", {
  # The success path M113 shipped, re-asserted through the one call site the
  # partial-removal rule left: a whole-family regression here would mean the
  # drop moved above the abort and stopped covering the ordinary case.
  dirs <- tm_redirect_config()
  stub <- tm_stub_executable()
  tm_write_location(dirs$new, "ffmpeg", stub)
  tm_write_location(dirs$legacy, "ffmpeg", stub)
  tm_seed_capability_memo()

  expect_true(unset_program("ffmpeg"))

  expect_true(tm_memo_is_empty())
})


# program_status(): which warnings the report lets through ---------------------

test_that("program_status() surfaces a stale location and stays quiet about an absent program", {
  # AC3. Four programs in three states at once: one remembered location whose
  # binary is gone, one never configured, and two that resolve. Exactly one
  # warning comes out, and it is the one naming a file the caller can repair --
  # the absent program's NA row already IS its answer (D088).
  dirs <- tm_redirect_config()
  stub <- tm_stub_executable()
  gone <- file.path(dirs$root, "gone", "ffmpeg-binary")
  tm_write_location(dirs$new, "ffmpeg", gone)
  for (program in c("ffplay", "mediainfo")) {
    tm_write_location(dirs$new, program, stub)
  }

  got <- tm_collect_warnings(program_status())

  expect_length(got$warnings, 1L)
  expect_s3_class(got$warnings[[1]], "tidymedia_location_gone")
  expect_identical(got$warnings[[1]]$tm_program, "ffmpeg")
  expect_identical(got$value$program, tm_program_vocabulary)
  expect_identical(
    got$value$location,
    c(NA_character_, NA_character_, stub, stub)
  )
})

test_that("program_status() surfaces an unreadable config file and reports the other three", {
  # AC1's report half. The malformed file's row is NA in both columns -- the
  # report never aborts on it, which is what it did before M116 -- and the
  # other three rows read exactly as the same call reads with no malformed
  # file present at all.
  for (broken in tm_program_vocabulary) {
    dirs <- tm_redirect_config()
    stub <- tm_stub_executable()
    for (program in setdiff(tm_program_vocabulary, broken)) {
      tm_write_location(dirs$new, program, stub)
    }
    baseline <- suppressWarnings(program_status())

    file.create(tm_config_file(broken, dirs$new))
    got <- tm_collect_warnings(program_status())

    expect_length(got$warnings, 1L)
    expect_s3_class(got$warnings[[1]], "tidymedia_location_unreadable")
    expect_identical(got$warnings[[1]]$tm_program, broken, info = broken)
    expect_identical(got$value$program, tm_program_vocabulary, info = broken)
    is_broken <- got$value$program == broken
    expect_true(is.na(got$value$location[is_broken]), info = broken)
    expect_true(is.na(got$value$version[is_broken]), info = broken)
    expect_identical(got$value[!is_broken, ], baseline[!is_broken, ], info = broken)
  }
})

test_that("program_status() stays silent when nothing is configured at all", {
  # The case the surfacing must not reach. Four programs, none found, no
  # config files: four plain not-found warnings, none of which the caller
  # hears, because the table already says it in both columns.
  tm_redirect_config()
  got <- tm_collect_warnings(program_status())
  expect_length(got$warnings, 0L)
  expect_identical(got$value$location, rep(NA_character_, 4L))
})
