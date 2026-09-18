# M136 AC1-AC3: `quality` on every batch re-encoding verb, as an argument and
# as a jobs column, cell by cell.
#
# The verb set and the row set are both computed (helper-quality-batch.R,
# helper-quality-grid.R). Each AC2 cell is a two-job table whose column is
# c(NA, value): the first job keeps its encoder default (D022's column form of
# NULL) and the second carries the flag after `-codec:v`. Refusals are counted
# against a probe counter so "before any FFmpeg process starts" is measured.

test_that("AC1: every batch verb taking `hardware` and `jobs` has quality = NULL", {
  verbs <- quality_batch_verbs()
  expect_length(verbs, 8L)
  for (v in verbs) {
    fmls <- formals(get(v, envir = asNamespace("tidymedia")))
    expect_true("quality" %in% names(fmls), info = v)
    expect_null(fmls$quality, info = v)
    # After `fallback`, as on the scalar verbs (M135).
    expect_identical(
      which(names(fmls) == "quality"), which(names(fmls) == "fallback") + 1L,
      info = v
    )
  }
})

test_that("AC2: a c(NA, value) column leaves job 1 alone and flags job 2 after its codec", {
  dir <- withr::local_tempdir()
  input <- quality_grid_input(dir)
  withr::local_options(
    tidymedia.hardware_encoders = quality_grid_hw_encoders(),
    tidymedia.check_tracks = FALSE
  )
  tbl <- quality_flags()
  cells <- 0L
  for (v in quality_batch_verbs()) {
    f <- get(v, envir = asNamespace("tidymedia"))
    for (enc in quality_batch_rows(v)) {
      row <- tbl[tbl$encoder == enc, ]
      pair <- quality_grid_pairs()[[enc]]
      args <- quality_batch_args(v, input)
      args$hardware <- pair[["hardware"]]
      if ("video_codec" %in% names(formals(f))) {
        args$video_codec <- pair[["video_codec"]]
      }
      # Both ends of the range on the second job: min + 1 inside it, max on it.
      for (value in c(row$min + 1, row$max)) {
        args$jobs$quality <- c(NA, value)
        out <- do.call(v, args, envir = asNamespace("tidymedia"))
        expect_no_quality_flag_in(quality_batch_job_commands(out, 1L),
                                  info = paste(v, enc, value, "job 1"))
        expect_quality_flag_after_codec(quality_batch_video_command(out, 2L),
                                        enc, row$flag, value,
                                        info = paste(v, enc, value, "job 2"))
      }
      # The argument form applies to every job.
      args$jobs$quality <- NULL
      args$quality <- row$min + 1
      out <- do.call(v, args, envir = asNamespace("tidymedia"))
      for (job in 1:2) {
        expect_quality_flag_after_codec(quality_batch_video_command(out, job),
                                        enc, row$flag, row$min + 1,
                                        info = paste(v, enc, "argument job", job))
      }
      # A column NA wins over the argument: the column form of NULL (D022).
      args$jobs$quality <- c(NA, NA)
      out <- do.call(v, args, envir = asNamespace("tidymedia"))
      expect_no_quality_flag_in(out$command, info = paste(v, enc, "NA over arg"))
      expect_true(any(grepl(paste0("-codec:v ", enc), out$command, fixed = TRUE)),
                  info = paste(v, enc, "NA over arg"))
      cells <- cells + 1L
    }
  }
  # The census: 7 rows per verb with a video_codec formal, 3 for format_for_web.
  expect_identical(cells, 52L)
})

# The refusal blames the verb, names the row, and nothing has asked FFmpeg.
expect_batch_quality_refusal <- function(fname, args, pattern, count,
                                         row = 2L) {
  before <- count()
  err <- rlang::catch_cnd(do.call(fname, args, envir = asNamespace("tidymedia")))
  expect_s3_class(err, "rlang_error")
  expect_identical(rlang::call_name(err$call), fname)
  expect_match(conditionMessage(err), pattern, fixed = TRUE)
  if (!is.na(row)) {
    expect_match(conditionMessage(err),
                 sprintf("First offending jobs row: %d.", row), fixed = TRUE)
  } else {
    expect_no_match(conditionMessage(err), "offending jobs row", fixed = TRUE)
  }
  expect_identical(count(), before)
  invisible(err)
}

test_that("AC3: a wrong `quality` cell is refused before any row runs, naming the verb and the row", {
  dir <- withr::local_tempdir()
  input <- quality_grid_input(dir)
  withr::local_options(tidymedia.check_tracks = FALSE)
  count <- local_encoder_probe_counter(names = quality_grid_hw_encoders())
  for (v in quality_batch_verbs()) {
    f <- get(v, envir = asNamespace("tidymedia"))
    has_codec <- "video_codec" %in% names(formals(f))
    base <- quality_batch_args(v, input)
    if (has_codec) base$video_codec <- "libx264"

    # A column that is neither numeric nor logical all-NA, under software and
    # under a backend. The row named is the first cell that is not NA.
    for (hw in c("none", "nvenc")) {
      for (bad in list(c("23", "20"), c(TRUE, FALSE), c(NA, TRUE))) {
        args <- base
        args$hardware <- hw
        args$jobs$quality <- bad
        expect_batch_quality_refusal(v, args, "must be numeric", count,
                                     row = which(!is.na(bad))[1])
      }
    }
    # A cell outside the range for that row's resolved encoder, both ends,
    # on every reachable row.
    for (enc in quality_batch_rows(v)) {
      pair <- quality_grid_pairs()[[enc]]
      row <- quality_flags()[quality_flags()$encoder == enc, ]
      args <- base
      args$hardware <- pair[["hardware"]]
      if (has_codec) args$video_codec <- pair[["video_codec"]]
      args$jobs$quality <- c(NA, row$max + 1)
      expect_batch_quality_refusal(v, args, enc, count)
      args$jobs$quality <- c(NA, row$min - 1)
      expect_batch_quality_refusal(v, args, enc, count)
    }
    # A wrong whole-batch argument: no row is named, since it applies to all.
    args <- base
    args$quality <- 52
    expect_batch_quality_refusal(v, args, "libx264", count, row = NA)
    if (has_codec) {
      # A cell on a row whose video_codec is "copy".
      args <- base
      args$jobs$video_codec <- c("libx264", "copy")
      args$jobs$quality <- c(NA, 23)
      expect_batch_quality_refusal(v, args, "copy", count)
      # A cell on a row whose video_codec is NA with the scalar video_codec =
      # NULL under hardware = "none": no encoder to apply to.
      args <- base
      args["video_codec"] <- list(NULL)
      args$hardware <- "none"
      args$jobs$video_codec <- c("libx264", NA)
      args$jobs$quality <- c(NA, 23)
      expect_batch_quality_refusal(v, args, "video_codec", count)
    }
    if ("reencode" %in% names(formals(f))) {
      # A cell on a stream-copying cut runs no encoder (M135 condition 2b).
      args <- base
      args["video_codec"] <- list(NULL)
      args$jobs$reencode <- c(TRUE, FALSE)
      args$jobs$quality <- c(NA, 23)
      expect_batch_quality_refusal(v, args, "reencode = FALSE", count)
    }
  }
  expect_identical(count(), 0L)
})

test_that("AC1's census: 8 batch verbs, 52 reachable (verb, row) pairs", {
  verbs <- quality_batch_verbs()
  expect_length(verbs, 8L)
  expect_identical(sum(vapply(verbs, function(v) length(quality_batch_rows(v)),
                              integer(1))), 52L)
})
