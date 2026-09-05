# M107: a backend with no encoder for the codec's family is refused by the verb
# the caller typed, on BOTH `fallback` arms.
#
# `hardware_backend_families()` is a two-key table, and a pair it omits --
# `(videotoolbox, av1)`, `(nvenc, prores)`, `(videotoolbox, prores)` -- is a
# wrong argument rather than a machine without something: no build of FFmpeg
# will ever grow a videotoolbox AV1 encoder for the package to find. That
# refusal already fired at the verb under `fallback = FALSE`, because
# `check_hardware_available()` sweeps the codecs at each fan-out verb's front
# door. Under `fallback = TRUE` that front door returned early, so the refusal
# was left to `resolve_hw_encoder()` while the pipeline was being built -- which
# in a `_batch` verb is inside `purrr::pmap()`, the frame D035's front-door call
# exists to keep out of the caller's error.
#
# The domain is COMPUTED (`nvenc_hardware_exports()`, helper-nvenc-memo.R): a
# seventeenth export gaining `hardware` joins this sweep on its own.
#
# Every cell's VALID arguments come from `tm_timeout_call_specs()`
# (helper-timeout-sweep.R), never hand-built here. A hand-built cell omits a
# required formal and aborts on the missing argument instead -- measured
# 2026-09-04, 12 of the 14 members with a `video_codec` formal did exactly that.

# One software codec token per family `codec_family()` can infer, so a cell
# names a family through the same inference a caller's own token goes through.
oot_family_codecs <- function() {
  c(h264 = "libx264", hevc = "libx265", av1 = "libaom-av1", prores = "prores_ks")
}

# The (backend, family) pairs the table OMITS and the ones it HOLDS, both read
# off the table rather than listed: a family added to a backend's row moves
# itself from one set to the other.
oot_pairs <- function(held) {
  fams <- hardware_backend_families()
  out <- list()
  for (hw in hardware_backends()) {
    row <- fams[[hw]]
    fs <- if (held) row else setdiff(hardware_codec_families(), row)
    for (f in fs) out[[paste(hw, f, sep = "/")]] <- list(hardware = hw, family = f)
  }
  out
}

# The members this sweep can reach, and the ones it cannot -- partitioned by
# reading each member's own `formals()`, never by naming a member. A verb with
# no `video_codec` argument has no way to name a codec family at all, so no cell
# of this sweep exists for it.
oot_members <- function() {
  ns <- asNamespace("tidymedia")
  has_vc <- vapply(
    nvenc_hardware_exports(),
    function(nm) "video_codec" %in% names(formals(get(nm, envir = ns))),
    logical(1)
  )
  list(reachable = names(has_vc)[has_vc], unreachable = names(has_vc)[!has_vc])
}

# The forms a member can name a codec in: the scalar argument always, and the
# `jobs` column as well where the member fans out over one (M109). Read off
# `formals()`, never listed -- a verb that grows a `jobs` argument joins the
# second form on its own, the way it already joins the sweep.
oot_forms <- function(nm) {
  fmls <- names(formals(get(nm, envir = asNamespace("tidymedia"))))
  c("scalar", if ("jobs" %in% fmls) "jobs")
}

# A two-row `jobs` table naming two codecs, for the column form.
#
# TWO rows, and the omitted family second: `check_hardware_available()` reduces
# a column to `unique()` families in column order, so a one-row column and a
# column whose first family is already the omitted one are both swept by a
# family loop that reads `families[1]` alone. This shape is not.
#
# Row 2's OUTPUT paths are renamed and its inputs are not. The two are told
# apart by what is on disk -- `tm_timeout_call_specs()` creates the inputs and
# creates no output -- rather than by naming a column per member. Without it,
# four of the seven fan-out verbs refuse the duplicated row for colliding
# outputs before the codec is ever read, which would blame the verb for the
# wrong reason (measured 2026-09-05).
oot_jobs <- function(jobs, codecs) {
  two <- jobs[c(1, 1), , drop = FALSE]
  for (col in names(two)) {
    v <- two[[col]]
    if (!is.character(v) || file.exists(v[[2]])) next
    two[[col]][[2]] <- sub("([^/]+)$", "2-\\1", v[[2]])
  }
  two$video_codec <- codecs
  two
}

# The in-table codec a column form puts AHEAD of the omitted one: the first
# family of the pair's own backend row, through the same token table the rest
# of the sweep names families with.
oot_in_table_codec <- function(hardware) {
  oot_family_codecs()[[hardware_backend_families()[[hardware]][[1]]]]
}

# One member's valid cell, with the codec, backend and fallback arm crossed in.
# `run`/`parallel` are forced off so a cell that is NOT refused spawns nothing.
oot_args <- function(nm, specs, codec, hardware, fallback, form = "scalar") {
  fmls <- names(formals(get(nm, envir = asNamespace("tidymedia"))))
  args <- specs[[nm]]
  if (is.null(args)) {
    stop("no tm_timeout_call_specs() cell for ", nm, call. = FALSE)
  }
  args$video_codec <- codec
  args$hardware <- hardware
  args$fallback <- fallback
  if ("parallel" %in% fmls) args$parallel <- FALSE
  if ("run" %in% fmls) args$run <- FALSE
  if (identical(form, "jobs")) {
    # The column REPLACES the scalar, so the cell measures the column arm and
    # not both at once.
    args$video_codec <- NULL
    args$jobs <- oot_jobs(args$jobs, c(oot_in_table_codec(hardware), codec))
  }
  args
}

test_that("the sweep's domain and its argument cells are computed, not listed", {
  # The instrument's own non-emptiness. A member set that silently emptied, or a
  # pair set that did, would leave every expectation below vacuously true.
  m <- oot_members()
  expect_gt(length(m$reachable), 0)
  expect_setequal(c(m$reachable, m$unreachable), nvenc_hardware_exports())

  ns <- asNamespace("tidymedia")
  for (nm in m$unreachable) {
    expect_false("video_codec" %in% names(formals(get(nm, envir = ns))), info = nm)
  }

  # Every family the sweep names is one `codec_family()` actually infers from
  # the token, and the token table covers the whole family vocabulary.
  fc <- oot_family_codecs()
  expect_setequal(names(fc), hardware_codec_families())
  for (f in names(fc)) expect_identical(codec_family(fc[[f]]), f, info = f)

  # Omitted and held partition the full cross, and neither half is empty.
  omitted <- names(oot_pairs(held = FALSE))
  held <- names(oot_pairs(held = TRUE))
  full <- as.vector(outer(hardware_backends(), hardware_codec_families(),
                          paste, sep = "/"))
  expect_setequal(c(omitted, held), full)
  expect_length(intersect(omitted, held), 0L)
  expect_gt(length(omitted), 0)
  expect_gt(length(held), 0)

  # And every reachable member has a valid cell to build from.
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  for (nm in m$reachable) expect_false(is.null(specs[[nm]]), info = nm)

  # The form partition, on the same terms (M109): every reachable member has the
  # scalar form, has the column form exactly when it takes a `jobs` argument,
  # and the column half is not empty -- a `oot_forms()` that quietly stopped
  # returning "jobs" would leave the sweep passing over half the cells it
  # claims, with nothing red.
  ns2 <- asNamespace("tidymedia")
  with_jobs <- character()
  for (nm in m$reachable) {
    forms <- oot_forms(nm)
    expect_true("scalar" %in% forms, info = nm)
    takes_jobs <- "jobs" %in% names(formals(get(nm, envir = ns2)))
    expect_identical("jobs" %in% forms, takes_jobs, info = nm)
    if (takes_jobs) with_jobs <- c(with_jobs, nm)
  }
  expect_gt(length(with_jobs), 0)

  # And a column form really is a column the front door reads as several
  # families, in the order the cell built them: `check_hardware_available()`
  # sweeps `batch_video_codecs()`'s output, so a cell whose column collapsed to
  # one family, or put the omitted family first, would be swept identically by
  # a family loop reading `families[1]` alone.
  for (nm in with_jobs) {
    for (pair in oot_pairs(held = FALSE)) {
      jobs <- oot_args(nm, specs, fc[[pair$family]], pair$hardware,
                       FALSE, "jobs")$jobs
      codecs <- ns2$batch_video_codecs(jobs, NULL)
      info <- paste(nm, pair$hardware, pair$family, sep = "/")
      expect_true(is.list(codecs), info = info)
      expect_length(codecs, 2L)
      expect_identical(codecs[[1]], oot_in_table_codec(pair$hardware), info = info)
      expect_identical(codecs[[2]], fc[[pair$family]], info = info)
      expect_identical(nrow(jobs), 2L)
      # Row 2's output paths really were made distinct. No character column
      # carries the same non-existent path twice, which is exactly what the
      # duplicate-output refusal keys on -- and that refusal firing first would
      # make every cell below pass for the wrong reason.
      for (col in names(jobs)) {
        v <- jobs[[col]]
        if (!is.character(v) || file.exists(v[[1]])) next
        expect_false(identical(v[[1]], v[[2]]), info = paste(info, col))
      }
    }
  }
})

test_that("an out-of-table pair is refused by the verb on both fallback arms", {
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  members <- oot_members()$reachable
  fc <- oot_family_codecs()

  # The build is answered from the option seam rather than a mock, and it is
  # answered GENEROUSLY: every encoder either backend could have is listed. An
  # out-of-table pair has no encoder name in that list to begin with, so a cell
  # refused here is refused for the pair and never for an absent encoder.
  fams <- hardware_backend_families()
  withr::local_options(tidymedia.hardware_encoders = unlist(
    lapply(names(fams), function(hw) paste0(fams[[hw]], "_", hw)),
    use.names = FALSE
  ))

  bad <- character()
  for (nm in members) {
    # Both forms a member can name the codec in (M109). The scalar form reaches
    # `check_hardware_available()`'s scalar arm; the column form reaches the
    # `is.list()` arm and the family loop above it, which the scalar form
    # cannot distinguish from a loop that reads its first element only.
    for (form in oot_forms(nm)) {
      for (pair in oot_pairs(held = FALSE)) {
        for (fb in c(FALSE, TRUE)) {
          label <- paste(nm, form, pair$hardware, pair$family, fb, sep = "/")
          cnd <- tryCatch(
            do.call(nm, oot_args(nm, specs, fc[[pair$family]], pair$hardware,
                                 fb, form),
                    envir = asNamespace("tidymedia")),
            error = function(e) e
          )
          if (!inherits(cnd, "error")) {
            bad <- c(bad, paste0(label, " -> <not refused>"))
            next
          }
          # Identity, not merely failure: the frame blamed AND the sentence it
          # carried. A cell that keeps naming the verb but starts saying
          # something else has still changed what the caller reads.
          frame <- blamed_verb(cnd)
          msg <- cli::ansi_strip(conditionMessage(cnd))
          wanted <- paste0(pair$hardware, " has no \"", pair$family, "\" encoder.")
          if (!identical(frame, nm) || !startsWith(msg, wanted)) {
            bad <- c(bad, paste0(label, " -> ", frame, ": ",
                                 sub("\n.*$", "", msg)))
          }
        }
      }
    }
  }
  expect_equal(bad, character())
})

test_that("a codec naming no family at all is refused by the verb too", {
  # The second class the same siting fixes, and the one NEWS.md's entry claims
  # alongside the first: `codec_family()` has to place the token before the
  # table can be consulted at all, so a token it cannot place is refused at the
  # same front door, on the same two arms. Blamed one frame down before M107,
  # exactly as the out-of-table pair was.
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  fc <- oot_family_codecs()
  codec <- tm_nvenc_unmappable_codec()

  # A generous build, for the sweep above's reason: nothing here is refused for
  # want of an encoder.
  fams <- hardware_backend_families()
  withr::local_options(tidymedia.hardware_encoders = unlist(
    lapply(names(fams), function(hw) paste0(fams[[hw]], "_", hw)),
    use.names = FALSE
  ))

  # The control first: the same call with a codec that DOES map is not refused
  # at all in these cells, so a refusal below is the token's and not the cell's.
  bad <- character()
  for (nm in oot_members()$reachable) {
    for (hw in hardware_backends()) {
      for (fb in c(FALSE, TRUE)) {
        label <- paste(nm, hw, fb, sep = "/")
        ok <- tryCatch(
          do.call(nm, oot_args(nm, specs, fc[["h264"]], hw, fb),
                  envir = asNamespace("tidymedia")),
          error = function(e) e
        )
        if (inherits(ok, "error")) {
          bad <- c(bad, paste0(label, " -> control refused: ", blamed_verb(ok)))
          next
        }
        cnd <- tryCatch(
          do.call(nm, oot_args(nm, specs, codec, hw, fb),
                  envir = asNamespace("tidymedia")),
          error = function(e) e
        )
        if (!inherits(cnd, "error")) {
          bad <- c(bad, paste0(label, " -> <not refused>"))
          next
        }
        msg <- cli::ansi_strip(conditionMessage(cnd))
        if (!identical(blamed_verb(cnd), nm) ||
            !grepl("No hardware encoder family maps to that codec", msg,
                   fixed = TRUE)) {
          bad <- c(bad, paste0(label, " -> ", blamed_verb(cnd)))
        }
      }
    }
  }
  expect_equal(bad, character())
})

test_that("an in-table encoder the build lacks still falls back with a message", {
  # The no-regression half. `fallback = TRUE` must keep meaning what
  # `@param fallback` documents for the pairs the table DOES hold: a build
  # without the encoder falls back to software and says so, rather than
  # aborting.
  dir <- withr::local_tempdir()
  specs <- tm_timeout_call_specs(dir)
  members <- oot_members()$reachable
  fc <- oot_family_codecs()

  # An empty build: no encoder of either backend is listed, so every held pair
  # is a pair this FFmpeg does not have.
  withr::local_options(tidymedia.hardware_encoders = character())

  bad <- character()
  for (nm in members) {
    for (pair in oot_pairs(held = TRUE)) {
      label <- paste(nm, pair$hardware, pair$family, sep = "/")
      args <- oot_args(nm, specs, fc[[pair$family]], pair$hardware, TRUE)
      msgs <- character()
      cnd <- tryCatch(
        withCallingHandlers(
          do.call(nm, args, envir = asNamespace("tidymedia")),
          message = function(m) {
            msgs <<- c(msgs, cli::ansi_strip(conditionMessage(m)))
            invokeRestart("muffleMessage")
          }
        ),
        error = function(e) e
      )
      if (inherits(cnd, "error")) {
        bad <- c(bad, paste0(label, " -> aborted: ", blamed_verb(cnd)))
      } else if (!any(grepl("falling back", msgs, fixed = TRUE))) {
        bad <- c(bad, paste0(label, " -> no fallback message"))
      }
    }
  }
  expect_equal(bad, character())
})
