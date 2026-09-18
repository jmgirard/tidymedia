# M135 AC1-AC4: `quality` on every scalar re-encoding verb, cell by cell.
#
# The verb set and the row set are both computed (helper-quality-grid.R). Each
# cell sets the option seam both ways (LESSONS M094): present, so the flag
# lands after `-codec:v <encoder>`; absent under fallback = TRUE, so the value
# is dropped with a message. Refusals are counted against a probe counter so
# "before any FFmpeg process starts" is measured, not assumed.

test_that("AC1: every scalar verb taking `hardware` and `run` has quality = NULL", {
  verbs <- quality_grid_verbs()
  expect_gte(length(verbs), 8L)
  for (v in verbs) {
    fmls <- formals(get(v, envir = asNamespace("tidymedia")))
    expect_true("quality" %in% names(fmls), info = v)
    expect_null(fmls$quality, info = v)
    # After `fallback`, as the gate chose.
    expect_identical(
      which(names(fmls) == "quality"), which(names(fmls) == "fallback") + 1L,
      info = v
    )
  }
})

test_that("AC2: every reachable row emits its flag after the codec; NULL emits none", {
  dir <- withr::local_tempdir()
  input <- quality_grid_input(dir)
  withr::local_options(
    tidymedia.hardware_encoders = quality_grid_hw_encoders(),
    tidymedia.check_tracks = FALSE
  )
  tbl <- quality_flags()
  cells <- 0L
  for (v in quality_grid_verbs()) {
    f <- get(v, envir = asNamespace("tidymedia"))
    for (enc in quality_grid_rows(v)) {
      row <- tbl[tbl$encoder == enc, ]
      pair <- quality_grid_pairs()[[enc]]
      args <- quality_grid_args(v, input)
      args$hardware <- pair[["hardware"]]
      if ("video_codec" %in% names(formals(f))) {
        args$video_codec <- pair[["video_codec"]]
      }
      value <- row$min + 1
      args$quality <- value
      cmds <- quality_grid_commands(do.call(v, args, envir = asNamespace("tidymedia")))
      expect_quality_flag_after_codec(cmds, enc, row$flag, value,
                                      info = paste(v, enc))
      args["quality"] <- list(NULL)
      cmds <- quality_grid_commands(do.call(v, args, envir = asNamespace("tidymedia")))
      expect_true(any(grepl(paste0("-codec:v ", enc), cmds, fixed = TRUE)),
                  info = paste(v, enc, "NULL"))
      expect_no_quality_flag_in(cmds, info = paste(v, enc, "NULL"))
      cells <- cells + 1L
    }
  }
  # The census: 7 rows per verb with a video_codec formal, 3 for format_for_web.
  expect_identical(cells, sum(vapply(quality_grid_verbs(), function(v) {
    length(quality_grid_rows(v))
  }, integer(1))))
})

# The refusal blames the verb, and nothing has asked FFmpeg.
expect_quality_refusal <- function(f, fname, args, pattern, count) {
  before <- count()
  err <- rlang::catch_cnd(do.call(fname, args, envir = asNamespace("tidymedia")))
  expect_s3_class(err, "rlang_error")
  expect_identical(rlang::call_name(err$call), fname)
  expect_match(conditionMessage(err), pattern, fixed = TRUE)
  expect_identical(count(), before)
  invisible(err)
}

test_that("AC3: a wrong `quality` is refused before any FFmpeg process starts", {
  dir <- withr::local_tempdir()
  input <- quality_grid_input(dir)
  withr::local_options(tidymedia.check_tracks = FALSE)
  count <- local_encoder_probe_counter(names = quality_grid_hw_encoders())
  for (v in quality_grid_verbs()) {
    f <- get(v, envir = asNamespace("tidymedia"))
    has_codec <- "video_codec" %in% names(formals(f))
    base <- quality_grid_args(v, input)
    if (has_codec) base$video_codec <- "libx264"

    # Not one finite number, under software and under a backend.
    for (hw in c("none", "nvenc")) {
      for (bad in list("23", c(20, 23), NA_real_, Inf, TRUE)) {
        args <- base
        args$hardware <- hw
        args$quality <- bad
        expect_quality_refusal(f, v, args, "quality", count)
      }
    }
    # Outside the range for the resolved encoder, on every reachable row.
    for (enc in quality_grid_rows(v)) {
      pair <- quality_grid_pairs()[[enc]]
      row <- quality_flags()[quality_flags()$encoder == enc, ]
      args <- base
      args$hardware <- pair[["hardware"]]
      if (has_codec) args$video_codec <- pair[["video_codec"]]
      args$quality <- row$max + 1
      expect_quality_refusal(f, v, args, enc, count)
      args$quality <- row$min - 1
      expect_quality_refusal(f, v, args, enc, count)
    }
    if (has_codec) {
      # An encoder the table lacks: an alias and a software encoder outside it.
      for (enc in c("h264", "libvpx-vp9")) {
        args <- base
        args$video_codec <- enc
        args$quality <- 23
        expect_quality_refusal(f, v, args, enc, count)
      }
      # A stream copy, and no codec at all under hardware = "none".
      args <- base
      args$video_codec <- "copy"
      args$quality <- 23
      expect_quality_refusal(f, v, args, "copy", count)
      args <- base
      args["video_codec"] <- list(NULL)
      args$hardware <- "none"
      args$quality <- 23
      expect_quality_refusal(f, v, args, "video_codec", count)
    } else {
      # No `video_codec` formal: every encoder this verb can resolve to is in
      # the table, so "an encoder the table lacks" names no reachable call.
      expect_true(all(quality_grid_rows(v) %in% quality_flags()$encoder))
    }
    if ("reencode" %in% names(formals(f))) {
      # A stream-copying cut runs no encoder.
      args <- base
      args["video_codec"] <- list(NULL)
      args$reencode <- FALSE
      args$quality <- 23
      expect_quality_refusal(f, v, args, "reencode = FALSE", count)
    }
  }
  expect_identical(count(), 0L)
})

test_that("AC1's census: 8 verbs, 52 reachable (verb, row) pairs", {
  verbs <- quality_grid_verbs()
  expect_length(verbs, 8L)
  expect_identical(sum(vapply(verbs, function(v) length(quality_grid_rows(v)),
                              integer(1))), 52L)
})

test_that("AC4: a fallback drops `quality`, says so, and emits no flag", {
  dir <- withr::local_tempdir()
  input <- quality_grid_input(dir)
  withr::local_options(
    tidymedia.hardware_encoders = character(),
    tidymedia.check_tracks = FALSE
  )
  for (v in quality_grid_verbs()) {
    f <- get(v, envir = asNamespace("tidymedia"))
    for (hw in c("nvenc", "videotoolbox")) {
      args <- quality_grid_args(v, input)
      if ("video_codec" %in% names(formals(f))) args$video_codec <- "libx264"
      args$hardware <- hw
      args$fallback <- TRUE
      args$quality <- 20
      msgs <- character()
      out <- withCallingHandlers(
        do.call(v, args, envir = asNamespace("tidymedia")),
        message = function(m) {
          msgs <<- c(msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      )
      expect_true(any(grepl("`quality` = 20 is dropped", msgs, fixed = TRUE)),
                  info = paste(v, hw))
      expect_true(any(grepl("is not available", msgs, fixed = TRUE)),
                  info = paste(v, hw))
      cmds <- quality_grid_commands(out)
      expect_true(any(grepl("-codec:v libx264", cmds, fixed = TRUE)),
                  info = paste(v, hw))
      expect_no_quality_flag_in(cmds, info = paste(v, hw))
    }
  }
})
