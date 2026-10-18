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
  # removes the remembered location, set_program() replaces it. Both are named
  # as the package exports them -- there is no unset_ffmpeg(), and
  # unset_program() takes no default (D079), so the advice carries the
  # argument. The two spellings no longer share a substring, which is what let
  # the set_program() assertion below pass while its bullet was deleted (M116
  # review [O]3).
  dirs <- tm_redirect_config()
  path <- tm_config_file("ffmpeg", dirs$new)
  file.create(path)

  condition <- tryCatch(find_ffmpeg(), warning = function(w) w)
  message <- cli::ansi_strip(conditionMessage(condition))

  expect_match(message, "ffmpeg", fixed = TRUE)
  expect_match(message, basename(path), fixed = TRUE)
  expect_match(message, "unset_program(\"ffmpeg\")", fixed = TRUE)
  expect_match(message, "set_ffmpeg()", fixed = TRUE)
  # Every advised call is one the package exports.
  expect_no_match(message, "unset_ffmpeg", fixed = TRUE)
})

test_that("a config file holding one empty line is unreadable too", {
  # The guard rejects three shapes, not two: it also refuses a single line with
  # no characters in it, because a location that is not a location resolves
  # nowhere. Empty, not merely blank -- the leg is !nzchar(), so a line holding
  # only spaces passes here and takes the gone-location branch instead. AC1
  # crosses the empty-file and two-line forms; this cell is what instruments
  # the help page's and NEWS's claim about the third (M116 review [O]6).
  dirs <- tm_redirect_config()
  path <- tm_config_file("ffmpeg", dirs$new)
  writeLines("", path)

  condition <- tryCatch(find_ffmpeg(), warning = function(w) w)
  expect_s3_class(condition, "tidymedia_location_unreadable")
  expect_identical(condition$tm_program, "ffmpeg")
  expect_identical(condition$tm_file, path)
  expect_null(suppressWarnings(find_ffmpeg()))
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


# find_program(): a folder where the location file should be ----------------

test_that("a folder at a remembered-location path warns and returns NULL", {
  # M140 AC1 and AC2, and M141 AC1 and AC5. `file.exists()` is TRUE for a
  # folder, so before M140 the lookup reached `readLines()` on it and stopped
  # with "cannot open the connection" (measured 2026-10-17). Every program,
  # and every layout of folders with no file at either path: the current path
  # only, the pre-0.2.0 path only, and both, each folder empty or not. The
  # warning names the first folder in read order. Its advice is
  # unset_program() only when every folder is empty, since unset_program()
  # refuses a folder that holds anything (M141, D104).
  withr::local_options(cli.width = 1000)
  layouts <- list(
    new_empty = c(new = "empty"),
    new_full = c(new = "full"),
    legacy_empty = c(legacy = "empty"),
    legacy_full = c(legacy = "full"),
    both_empty = c(new = "empty", legacy = "empty"),
    new_empty_legacy_full = c(new = "empty", legacy = "full"),
    new_full_legacy_empty = c(new = "full", legacy = "empty"),
    both_full = c(new = "full", legacy = "full")
  )
  for (program in tm_program_vocabulary) {
    for (layout in names(layouts)) {
      dirs <- tm_redirect_config()
      shape <- layouts[[layout]]
      paths <- vapply(
        names(shape), function(w) tm_config_file(program, dirs[[w]]),
        character(1)
      )
      for (w in names(shape)) {
        dir.create(paths[[w]])
        if (shape[[w]] == "full") file.create(file.path(paths[[w]], "kept"))
      }
      info <- paste(program, layout)

      condition <- tryCatch(
        tm_find_export[[program]](),
        warning = function(w) w,
        error = function(e) e
      )
      expect_s3_class(condition, "tidymedia_location_unreadable")
      expect_identical(condition$tm_program, program, info = info)
      expect_identical(condition$tm_file, unname(paths[[1]]), info = info)

      message <- cli::ansi_strip(conditionMessage(condition))
      expect_match(message, paths[[1]], fixed = TRUE, info = info)
      full <- unname(paths[shape == "full"])
      if (length(full)) {
        expect_match(message, "by hand", fixed = TRUE, info = info)
        for (f in full) expect_match(message, f, fixed = TRUE, info = info)
        expect_no_match(message, "unset_program", fixed = TRUE, info = info)
      } else {
        expect_match(
          message, paste0("unset_program(\"", program, "\")"),
          fixed = TRUE, info = info
        )
        expect_no_match(message, "by hand", fixed = TRUE, info = info)
      }

      expect_null(
        expect_no_error(suppressWarnings(tm_find_export[[program]]())),
        info = info
      )
    }
  }
})

test_that("a folder at the current path does not hide a pre-0.2.0 file", {
  # M141 AC1. Before M141 the lookup fell back to the pre-0.2.0 path only when
  # nothing existed at the current path, so a folder there stopped it from
  # reading a usable older file. The folder's own contents do not matter.
  for (program in tm_program_vocabulary) {
    for (contents in c("empty", "full")) {
      dirs <- tm_redirect_config()
      stub <- tm_stub_executable()
      folder <- tm_config_file(program, dirs$new)
      dir.create(folder)
      if (contents == "full") file.create(file.path(folder, "kept"))
      tm_write_location(dirs$legacy, program, stub)
      info <- paste(program, contents)

      expect_identical(
        expect_no_warning(tm_find_export[[program]]()), stub, info = info
      )
      got <- tm_collect_warnings(program_status())
      expect_length(got$warnings, 0L)
      expect_identical(
        got$value$location[got$value$program == program], stub, info = info
      )
    }
  }
})


# find_program(): a location file the lookup cannot read --------------------

test_that("a location file without read permission warns and returns NULL", {
  # M141 AC2. `file.exists()` is TRUE for such a file, so before M141 the
  # lookup reached `readLines()`, which warned "cannot open file ... Permission
  # denied" and stopped with "cannot open the connection". A
  # mode-0000 file is unreadable on Unix to anyone but root, and Windows has no
  # such mode, so the cell skips where `file.access()` still reports it readable.
  skip_on_os("windows")
  withr::local_options(cli.width = 1000)
  for (program in tm_program_vocabulary) {
    for (where in c("new", "legacy")) {
      dirs <- tm_redirect_config()
      stub <- tm_stub_executable()
      path <- tm_config_file(program, dirs[[where]])
      tm_write_location(dirs[[where]], program, stub)
      Sys.chmod(path, "0000")
      withr::defer(Sys.chmod(path, "0644"))
      skip_if(file.access(path, 4L) == 0L, "the process can read a mode-0000 file")
      info <- paste(program, where)

      condition <- tryCatch(
        tm_find_export[[program]](),
        warning = function(w) w,
        error = function(e) e
      )
      expect_s3_class(condition, "tidymedia_location_unreadable")
      expect_identical(condition$tm_program, program, info = info)
      expect_identical(condition$tm_file, path, info = info)
      message <- cli::ansi_strip(conditionMessage(condition))
      expect_match(message, "read permission", fixed = TRUE, info = info)
      expect_match(
        message, paste0("unset_program(\"", program, "\")"),
        fixed = TRUE, info = info
      )

      got <- tm_collect_warnings(program_status())
      expect_identical(got$value$program, tm_program_vocabulary, info = info)
      expect_length(got$warnings, 1L)
      expect_s3_class(got$warnings[[1]], "tidymedia_location_unreadable")
      expect_identical(got$warnings[[1]]$tm_file, path, info = info)
    }
  }
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
      # set_program()'s per-program wrapper is exported and named that way;
      # unset_program() has no wrapper, so it is named with its argument. The
      # two share no substring, so each assertion fails on its own bullet's
      # removal -- the pairing that did not hold before M116's return.
      expect_match(message, paste0("set_", program, "()"), fixed = TRUE, info = info)
      expect_match(message, paste0("unset_program(\"", program, "\")"),
                   fixed = TRUE, info = info)
      expect_no_match(message, paste0("unset_", program), fixed = TRUE, info = info)
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

test_that("program_status() reports a folder at a remembered-location path", {
  # M140 AC3. Before M140 the folder stopped the whole report with "cannot open
  # the connection". The other three programs are not configured, so their
  # not-found warnings stay muffled and the folder's is the one heard.
  dirs <- tm_redirect_config()
  dir.create(tm_config_file("ffmpeg", dirs$new))

  got <- tm_collect_warnings(program_status())

  expect_identical(got$value$program, tm_program_vocabulary)
  expect_length(got$warnings, 1L)
  expect_s3_class(got$warnings[[1]], "tidymedia_location_unreadable")
  # The advice reaches the report's caller too, not only find_ffmpeg()'s. The
  # folder is empty, so the advice is unset_program() (M141 AC5).
  expect_match(
    cli::ansi_strip(conditionMessage(got$warnings[[1]])),
    "unset_program(\"ffmpeg\")",
    fixed = TRUE
  )
})

# set_program(): a folder where the location file should be ------------------

# The four shapes a folder at a location path can take (M141). Only the first
# is empty: a hidden file or an empty subfolder is still something the caller
# put there, so set_program() and unset_program() must leave it (D104).
tm_folder_shapes <- list(
  empty = function(path) dir.create(path),
  file = function(path) {
    dir.create(path)
    file.create(file.path(path, "kept"))
  },
  hidden = function(path) {
    dir.create(path)
    file.create(file.path(path, ".DS_Store"))
  },
  subfolder = function(path) {
    dir.create(path)
    dir.create(file.path(path, "inner"))
  }
)

test_that("set_program() replaces an empty folder at the current path", {
  # M141 AC3. Before M141 writeLines() stopped on the folder with "cannot open
  # the connection" (M140 measured it). Every program, through the exported
  # set_program() with confirm = FALSE.
  for (program in tm_program_vocabulary) {
    dirs <- tm_redirect_config()
    stub <- tm_stub_executable()
    path <- tm_config_file(program, dirs$new)
    tm_folder_shapes$empty(path)

    expect_true(set_program(program, stub, confirm = FALSE), info = program)
    expect_false(dir.exists(path), info = program)
    expect_identical(readLines(path), stub, info = program)
  }
})

test_that("set_program() refuses a folder that holds anything, before any prompt", {
  # M141 AC3. With confirm = TRUE the session cannot be asked, so a refusal of
  # the folder's class shows the folder check comes first: the consent
  # refusal would carry `tidymedia_confirmation_unavailable` instead.
  withr::local_options(rlang_interactive = FALSE, cli.width = 1000)
  for (shape in setdiff(names(tm_folder_shapes), "empty")) {
    for (confirm in c(FALSE, TRUE)) {
      dirs <- tm_redirect_config()
      stub <- tm_stub_executable()
      path <- tm_config_file("ffmpeg", dirs$new)
      tm_folder_shapes[[shape]](path)
      before <- list.files(path, all.files = TRUE, no.. = TRUE)
      info <- paste(shape, confirm)

      condition <- expect_error(
        set_program("ffmpeg", stub, confirm = confirm),
        class = "tidymedia_location_folder"
      )
      expect_identical(condition$tm_program, "ffmpeg", info = info)
      expect_identical(condition$tm_files, path, info = info)
      message <- cli::ansi_strip(conditionMessage(condition))
      expect_match(message, path, fixed = TRUE, info = info)
      expect_match(message, "by hand", fixed = TRUE, info = info)

      expect_true(dir.exists(path), info = info)
      expect_identical(
        list.files(path, all.files = TRUE, no.. = TRUE), before, info = info
      )
    }
  }
})

test_that("a folder tidymedia cannot read never counts as empty", {
  # M141 claim audit. An unreadable folder lists as empty, so tm_folder_empty()
  # asks about read permission first, and set_program(), unset_program() and
  # the lookup all treat such a folder as one that holds something. Mode 0333
  # takes away read permission on Unix to anyone but root, and Windows has no
  # such mode.
  skip_on_os("windows")
  withr::local_options(cli.width = 1000)
  dirs <- tm_redirect_config()
  path <- tm_config_file("ffmpeg", dirs$new)
  dir.create(path)
  Sys.chmod(path, "0333")
  withr::defer(Sys.chmod(path, "0755"))
  skip_if(file.access(path, 4L) == 0L, "the process can read a mode-0333 folder")

  condition <- expect_error(
    set_program("ffmpeg", tm_stub_executable(), confirm = FALSE),
    class = "tidymedia_location_folder"
  )
  expect_match(
    cli::ansi_strip(conditionMessage(condition)), "cannot read", fixed = TRUE
  )
  expect_error(unset_program("ffmpeg"), class = "tidymedia_location_folder")
  expect_true(dir.exists(path))

  warning <- tryCatch(find_ffmpeg(), warning = function(w) w)
  message <- cli::ansi_strip(conditionMessage(warning))
  expect_match(message, "by hand", fixed = TRUE)
  expect_no_match(message, "unset_program", fixed = TRUE)
})

test_that("set_program()'s consent covers removing an empty folder", {
  # M141 AC3. The prompt and the refusal with no one to ask both name the
  # folder. An approval removes it and writes the file, and a decline leaves
  # it in place.
  withr::local_options(cli.width = 1000)
  for (answer in c(TRUE, FALSE)) {
    dirs <- tm_redirect_config()
    stub <- tm_stub_executable()
    path <- tm_config_file("ffmpeg", dirs$new)
    tm_folder_shapes$empty(path)
    prompts <- character(0)
    local_mocked_bindings(tm_confirm = function(prompt, ...) {
      prompts <<- c(prompts, prompt)
      answer
    })

    expect_identical(set_program("ffmpeg", stub), answer)
    expect_length(prompts, 1L)
    # The folder's own line, not the "By writing:" line, which names the same
    # path (M141 review diff-bug #9).
    folder_line <- grep("empty folder", strsplit(prompts, "\n")[[1]], value = TRUE)
    expect_length(folder_line, 1L)
    expect_match(folder_line, path, fixed = TRUE)
    if (answer) {
      expect_identical(readLines(path), stub)
    } else {
      expect_true(dir.exists(path))
    }
  }
})

test_that("set_program()'s refusal with no one to ask names the empty folder", {
  # M141 AC3. Its own test, because the tm_confirm() mock above lasts until the
  # end of the test that sets it.
  withr::local_options(cli.width = 1000)
  dirs <- tm_redirect_config()
  stub <- tm_stub_executable()
  path <- tm_config_file("ffmpeg", dirs$new)
  tm_folder_shapes$empty(path)
  withr::local_options(rlang_interactive = FALSE)
  condition <- expect_error(
    set_program("ffmpeg", stub),
    class = "tidymedia_confirmation_unavailable"
  )
  message <- cli::ansi_strip(conditionMessage(condition))
  folder_line <- grep("empty folder", strsplit(message, "\n")[[1]], value = TRUE)
  expect_length(folder_line, 1L)
  expect_match(folder_line, path, fixed = TRUE)
  expect_true(dir.exists(path))
})

test_that("set_program() asks about the folder again after consent", {
  # M141 review diff-bug #3 and #4. The removal is recursive, so the folder is
  # checked again after the prompt. An entry added while the prompt waits makes
  # the call refuse and keep it. A folder removed while the prompt waits leaves
  # nothing to remove, and the call writes the file.
  for (change in c("gains_entry", "vanishes")) {
    dirs <- tm_redirect_config()
    stub <- tm_stub_executable()
    path <- tm_config_file("ffmpeg", dirs$new)
    tm_folder_shapes$empty(path)
    local_mocked_bindings(tm_confirm = function(prompt, ...) {
      if (change == "gains_entry") {
        file.create(file.path(path, "added"))
      } else {
        unlink(path, recursive = TRUE)
      }
      TRUE
    })

    if (change == "gains_entry") {
      expect_error(
        set_program("ffmpeg", stub),
        class = "tidymedia_location_folder"
      )
      expect_true(file.exists(file.path(path, "added")))
    } else {
      expect_true(set_program("ffmpeg", stub))
      expect_identical(readLines(path), stub)
    }
  }
})

test_that("the folder refusal counts the folders it names", {
  # M141 review diff-bug #5. unset_program() can list two folders.
  withr::local_options(cli.width = 1000)
  dirs <- tm_redirect_config()
  for (d in c(dirs$new, dirs$legacy)) {
    tm_folder_shapes$file(tm_config_file("ffmpeg", d))
  }
  condition <- expect_error(
    unset_program("ffmpeg"),
    class = "tidymedia_location_folder"
  )
  message <- cli::ansi_strip(conditionMessage(condition))
  expect_match(message, "paths for ffmpeg: they are folders", fixed = TRUE)
  expect_match(message, "Delete them by hand", fixed = TRUE)

  dirs <- tm_redirect_config()
  tm_folder_shapes$file(tm_config_file("ffmpeg", dirs$new))
  condition <- expect_error(
    unset_program("ffmpeg"),
    class = "tidymedia_location_folder"
  )
  message <- cli::ansi_strip(conditionMessage(condition))
  expect_match(message, "path for ffmpeg: it is a folder", fixed = TRUE)
  expect_match(message, "Delete it by hand", fixed = TRUE)
})

test_that("set_program() stops when the empty folder will not go", {
  # M141 AC3. The removal goes through the tm_unlink() seam, so a seam that
  # removes nothing stands in for a folder the filesystem keeps. The call stops
  # with the removal class and writes nothing.
  dirs <- tm_redirect_config()
  stub <- tm_stub_executable()
  path <- tm_config_file("ffmpeg", dirs$new)
  tm_folder_shapes$empty(path)
  local_mocked_bindings(tm_unlink = function(path, recursive = FALSE) 1L)

  condition <- expect_error(
    set_program("ffmpeg", stub, confirm = FALSE),
    class = "tidymedia_location_not_removed"
  )
  expect_identical(condition$tm_program, "ffmpeg")
  expect_identical(condition$tm_files, path)
  expect_true(dir.exists(path))
  expect_length(list.files(path, all.files = TRUE, no.. = TRUE), 0L)
})

# set_program(): a location it cannot write ----------------------------------

# Run a set_program() call that must stop, and hand back the error and every
# warning raised before it. Before M143 base R's "cannot open file" warning
# came first, then its "cannot open the connection" error.
tm_set_refusal <- function(expr) {
  warns <- list()
  condition <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warns[[length(warns) + 1L]] <<- w
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  list(condition = condition, warnings = warns)
}

test_that("set_program() refuses a location file it cannot write", {
  # M143 AC1. Mode 0444 takes away write permission on Unix to anyone but
  # root. On Windows Sys.chmod() sets only the read-only attribute, so the
  # test runs on Unix only. The file keeps its line: the call
  # writes in place and does not replace a file the caller made read-only.
  skip_on_os("windows")
  dirs <- tm_redirect_config()
  path <- tm_config_file("ffmpeg", dirs$new)
  writeLines("/the/old/ffmpeg", path)
  Sys.chmod(path, "0444")
  withr::defer(Sys.chmod(path, "0644"))
  skip_if(file.access(path, 2L) == 0L, "the process can write a mode-0444 file")

  result <- tm_set_refusal(
    set_program("ffmpeg", tm_stub_executable(), confirm = FALSE)
  )
  expect_length(result$warnings, 0L)
  expect_s3_class(result$condition, "tidymedia_location_not_written")
  expect_identical(result$condition$tm_program, "ffmpeg")
  expect_identical(result$condition$tm_file, path)
  expect_identical(readLines(path), "/the/old/ffmpeg")
})

test_that("set_program() refuses a location folder it cannot create or write in", {
  # M143 AC2. Case "parent": the folder for the location file is not there,
  # and the folder above it has no write permission, so dir.create() fails.
  # Case "folder": the folder is there without write permission and holds no
  # location file, so writeLines() fails.
  skip_on_os("windows")
  for (case in c("parent", "folder")) {
    dirs <- tm_redirect_config()
    path <- tm_config_file("ffmpeg", dirs$new)
    if (case == "parent") {
      unlink(dirs$new, recursive = TRUE)
      shut <- dirname(dirs$new)
    } else {
      shut <- dirs$new
    }
    Sys.chmod(shut, "0555")
    withr::defer(Sys.chmod(shut, "0755"))
    skip_if(file.access(shut, 2L) == 0L, "the process can write a mode-0555 folder")

    result <- tm_set_refusal(
      set_program("ffmpeg", tm_stub_executable(), confirm = FALSE)
    )
    expect_length(result$warnings, 0L)
    expect_s3_class(result$condition, "tidymedia_location_not_written")
    expect_identical(result$condition$tm_program, "ffmpeg", info = case)
    expect_identical(result$condition$tm_file, path, info = case)
    expect_false(file.exists(path), info = case)
    if (case == "parent") expect_false(dir.exists(dirs$new))
  }
})


# unset_program(): a folder where the location file should be ----------------

# Lay out what each of the two paths holds: "location" (a location file), "empty"
# or another tm_folder_shapes name (a folder), or nothing when absent.
tm_lay_out <- function(dirs, program, layout, stub) {
  paths <- c(
    new = tm_config_file(program, dirs$new),
    legacy = tm_config_file(program, dirs$legacy)
  )
  for (w in names(layout)) {
    if (layout[[w]] == "location") {
      writeLines(stub, paths[[w]])
    } else {
      tm_folder_shapes[[layout[[w]]]](paths[[w]])
    }
  }
  paths
}

# Everything at or under both paths, for "nothing changed" assertions.
tm_tree <- function(paths) {
  unlist(lapply(unname(paths), function(p) {
    c(p[file.exists(p)], list.files(p, all.files = TRUE, no.. = TRUE,
                                    recursive = TRUE, include.dirs = TRUE,
                                    full.names = TRUE))
  }))
}

test_that("unset_program() removes empty folders with the location files", {
  # M141 AC4. Before M141 it removed files only, and refused a folder with
  # advice about write access. Every layout of empty folders and files across
  # the two paths, both orientations.
  layouts <- list(
    c(new = "empty"),
    c(legacy = "empty"),
    c(new = "empty", legacy = "empty"),
    c(new = "empty", legacy = "location"),
    c(new = "location", legacy = "empty")
  )
  for (program in tm_program_vocabulary) {
    for (layout in layouts) {
      dirs <- tm_redirect_config()
      paths <- tm_lay_out(dirs, program, layout, tm_stub_executable())
      info <- paste(program, paste(names(layout), layout, collapse = " "))

      expect_true(expect_invisible(unset_program(program)), info = info)
      expect_false(any(file.exists(paths)), info = info)
    }
  }
})

test_that("unset_program() removes the rest and keeps a folder that holds anything", {
  # M143 AC3 and D105. Each non-empty shape at each path, beside a location
  # file or an empty folder at the other path. Before M143 the call refused
  # and removed nothing, so a location file beside such a folder could not be
  # forgotten. Now the file or empty folder goes, the full folder and its
  # entries stay, and a warning of its own class names the folder after the
  # removal. The memo goes with a removed location file only (D089, D104).
  for (shape in setdiff(names(tm_folder_shapes), "empty")) {
    for (where in c("new", "legacy")) {
      for (other in c("location", "empty")) {
        dirs <- tm_redirect_config()
        other_where <- setdiff(c("new", "legacy"), where)
        layout <- stats::setNames(c(shape, other), c(where, other_where))
        paths <- tm_lay_out(dirs, "ffmpeg", layout, tm_stub_executable())
        full <- paths[[where]]
        kept <- tm_tree(full)
        tm_seed_capability_memo()
        info <- paste(shape, where, other)

        result <- tm_collect_warnings(
          expect_invisible(unset_program("ffmpeg"))
        )
        expect_true(result$value, info = info)
        expect_length(result$warnings, 1L)
        warning <- result$warnings[[1]]
        expect_s3_class(warning, "tidymedia_location_folder_kept")
        expect_identical(warning$tm_program, "ffmpeg", info = info)
        expect_identical(warning$tm_files, full, info = info)
        expect_false(file.exists(paths[[other_where]]), info = info)
        expect_identical(tm_tree(full), kept, info = info)
        expect_identical(tm_memo_is_empty(), other == "location", info = info)
      }
    }
  }
})

test_that("unset_program() keeps a folder it cannot read beside a location file", {
  # M143 AC3. A folder the process cannot read never counts as empty (M141),
  # so it stays and the warning names it. Mode 0333 takes away read
  # permission on Unix to anyone but root, and Windows has no such mode.
  skip_on_os("windows")
  dirs <- tm_redirect_config()
  paths <- tm_lay_out(dirs, "ffmpeg", c(legacy = "location"),
                      tm_stub_executable())
  dir.create(paths[["new"]])
  Sys.chmod(paths[["new"]], "0333")
  withr::defer(Sys.chmod(paths[["new"]], "0755"))
  skip_if(file.access(paths[["new"]], 4L) == 0L,
          "the process can read a mode-0333 folder")

  result <- tm_collect_warnings(unset_program("ffmpeg"))
  expect_true(result$value)
  expect_length(result$warnings, 1L)
  expect_s3_class(result$warnings[[1]], "tidymedia_location_folder_kept")
  expect_identical(result$warnings[[1]]$tm_files, paths[["new"]])
  expect_true(dir.exists(paths[["new"]]))
  expect_false(file.exists(paths[["legacy"]]))
})

test_that("unset_program() refuses when every path that holds anything holds a full folder", {
  # M141 AC4, narrowed by M143 AC4 and D105. Each non-empty shape at each
  # path, with nothing or a second non-empty folder at the other path. The
  # refusal lists every non-empty folder, and both paths stay as they were.
  # "full" puts a second folder of the same shape at the other path, so
  # tm_files must list both.
  others <- c("none", "full")
  for (shape in setdiff(names(tm_folder_shapes), "empty")) {
    for (where in c("new", "legacy")) {
      for (other in others) {
        dirs <- tm_redirect_config()
        layout <- stats::setNames(shape, where)
        other_where <- setdiff(c("new", "legacy"), where)
        if (other == "full") {
          layout[[other_where]] <- shape
        } else if (other != "none") {
          layout[[other_where]] <- other
        }
        paths <- tm_lay_out(dirs, "ffmpeg", layout, tm_stub_executable())
        before <- tm_tree(paths)
        info <- paste(shape, where, other)

        condition <- expect_error(
          unset_program("ffmpeg"),
          class = "tidymedia_location_folder"
        )
        expect_identical(condition$tm_program, "ffmpeg", info = info)
        full <- unname(paths[names(layout)[layout == shape]])
        full <- full[order(match(full, paths))]
        expect_identical(condition$tm_files, full, info = info)
        expect_identical(tm_tree(paths), before, info = info)
      }
    }
  }
})

test_that("unset_program() keeps the memo when it removes only empty folders", {
  # M141 AC4 and D104. A folder never names the binary the lookup resolves to,
  # so removing one leaves the capability memo alone. A location file removed
  # in the same call still drops it (D089).
  dirs <- tm_redirect_config()
  tm_lay_out(dirs, "ffmpeg", c(new = "empty", legacy = "empty"), "unused")
  tm_seed_capability_memo()
  expect_true(unset_program("ffmpeg"))
  expect_false(tm_memo_is_empty())

  dirs <- tm_redirect_config()
  tm_lay_out(dirs, "ffmpeg", c(new = "empty", legacy = "location"),
             tm_stub_executable())
  tm_seed_capability_memo()
  expect_true(unset_program("ffmpeg"))
  expect_true(tm_memo_is_empty())
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
