# M139: after FFmpeg exits non-zero in ffm_run(), a codec this build does not
# list as encodable is named on the condition, under its own class.
#
# The absent-encoder state is not observable on a machine whose build has the
# encoder, so most cells mock the exit and the two listings: they are the
# CI-portable half. The wrapper cells at the end use M138's
# `tools/ffmpeg_build_model/ffmpeg`, which hides an encoder from a real FFmpeg
# and makes a run naming it fail the way Fedora's build does. They run only
# where a real FFmpeg is present and the source tree's `tools/` is reachable.

# A failed run, the two listings, and a count of how often each listing was
# read. `encoders`/`codecs` are the answers; a function there is called instead,
# so a cell can make the read fail.
local_failed_run <- function(encoders = c("libopenh264", "aac", "libmp3lame"),
                             codecs = c("h264", "aac", "mp3"),
                             status = 8L, env = parent.frame()) {
  reads <- new.env()
  reads$encoders <- 0L
  reads$codecs <- 0L
  answer <- function(x) if (is.function(x)) x() else x
  testthat::local_mocked_bindings(
    run_program = function(...) structure(character(), status = status),
    cached_encoder_names = function() {
      reads$encoders <- reads$encoders + 1L
      answer(encoders)
    },
    cached_encodable_codecs = function() {
      reads$codecs <- reads$codecs + 1L
      answer(codecs)
    },
    .env = env
  )
  reads
}

pipeline_with <- function(video = NULL, audio = NULL, env = parent.frame()) {
  infile <- make_input(env = env)
  out <- withr::local_tempfile(fileext = ".mp4", .local_envir = env)
  ffm_codec(ffm_files(infile, out), audio = audio, video = video)
}

exit_classes <- c("tidymedia_ffmpeg_exit", "rlang_error", "error", "condition")
diagnosed_classes <- c("tidymedia_encoder_unavailable", exit_classes)

test_that("an encoder the build does not list is named on the exit condition", {
  reads <- local_failed_run()
  cnd <- tryCatch(ffm_run(pipeline_with(video = "libx264", audio = "aac")),
                  error = function(e) e)

  expect_identical(class(cnd), diagnosed_classes)
  expect_identical(cnd$tm_status, 8L)
  expect_identical(cnd$tm_encoder, "libx264")
  expect_identical(cnd$tm_stream, "video")
  msg <- cli::ansi_strip(conditionMessage(cnd))
  expect_match(msg, "FFmpeg exited with status 8.", fixed = TRUE)
  expect_match(msg, 'This FFmpeg build lists no video encoder "libx264".',
               fixed = TRUE)
  # The bullets the merge base prints are still there.
  expect_match(msg, "FFmpeg's error output is printed above.", fixed = TRUE)
  expect_match(msg, "The failing command was:", fixed = TRUE)
  # The message states what the build lacks, never that the lack caused the
  # exit: an unlisted codec can sit on a stream the output does not have.
  expect_no_match(msg, "because|caused|due to", perl = TRUE)
  # Called directly, the frame named is ffm_run()'s own.
  expect_identical(rlang::call_name(conditionCall(cnd)), "ffm_run")
  # The codec listing was read only for the name the encoder list lacked.
  expect_identical(reads$encoders, 1L)
  expect_identical(reads$codecs, 1L)
})

test_that("a codec name the build encodes is not diagnosed (h264, mp3)", {
  # `-c:v h264` and `-c:a mp3` run on a build whose `-encoders` lists neither
  # name. The control is the same call with an unlisted name, so a diagnosis
  # that never fired would fail here.
  reads <- local_failed_run()
  cnd <- tryCatch(ffm_run(pipeline_with(video = "h264", audio = "mp3")),
                  error = function(e) e)
  expect_identical(class(cnd), exit_classes)
  expect_identical(reads$codecs, 1L)

  ctrl <- tryCatch(ffm_run(pipeline_with(video = "nosuch264", audio = "mp3")),
                   error = function(e) e)
  expect_identical(class(ctrl), diagnosed_classes)
  expect_identical(ctrl$tm_encoder, "nosuch264")
})

test_that("both codecs missing are both named, video first", {
  local_failed_run()
  cnd <- tryCatch(ffm_run(pipeline_with(video = "libx264", audio = "libfdk_aac")),
                  error = function(e) e)
  expect_identical(class(cnd), diagnosed_classes)
  expect_identical(cnd$tm_encoder, c("libx264", "libfdk_aac"))
  expect_identical(cnd$tm_stream, c("video", "audio"))
  msg <- cli::ansi_strip(conditionMessage(cnd))
  expect_match(msg, 'lists no audio encoder "libfdk_aac".', fixed = TRUE)
})

test_that("copy, an unset codec and a hardware-table name are never asked about", {
  for (args in list(
    list(video = "copy", audio = "copy"),
    list(),
    list(video = "h264_nvenc"),
    list(video = "hevc_videotoolbox", audio = "copy")
  )) {
    reads <- local_failed_run(encoders = character(), codecs = character())
    cnd <- tryCatch(ffm_run(do.call(pipeline_with, args)),
                    error = function(e) e)
    label <- paste(names(args), unlist(args), collapse = ", ")
    expect_identical(class(cnd), exit_classes, info = label)
    expect_identical(reads$encoders, 0L, info = label)
    expect_identical(reads$codecs, 0L, info = label)
  }
  # The excluded set is computed from the backend table by
  # hardware_encoder_names(). This pins it to the five names the table holds
  # today.
  expect_setequal(
    hardware_encoder_names(),
    c("h264_nvenc", "hevc_nvenc", "av1_nvenc",
      "h264_videotoolbox", "hevc_videotoolbox")
  )
})

test_that("a run FFmpeg completes never reads either listing", {
  reads <- local_failed_run()
  local_mocked_bindings(run_program = function(...) character())
  expect_no_error(ffm_run(pipeline_with(video = "libx264", audio = "aac")))
  expect_identical(reads$encoders, 0L)
  expect_identical(reads$codecs, 0L)
})

test_that("a listing that cannot be read leaves the merge-base condition", {
  local_failed_run(encoders = function() stop("listing broke"))
  expect_no_warning(
    cnd <- tryCatch(ffm_run(pipeline_with(video = "libx264")),
                    error = function(e) e)
  )
  expect_identical(class(cnd), exit_classes)
  expect_null(cnd$tm_encoder)
  msg <- cli::ansi_strip(conditionMessage(cnd))
  expect_no_match(msg, "lists no", fixed = TRUE)
  expect_match(msg, "FFmpeg exited with status 8.", fixed = TRUE)
})

# Run `expr`, collecting every warning it signals and the error it ends with,
# from the one evaluation.
collect_conditions <- function(expr) {
  warnings <- list()
  err <- tryCatch(
    withCallingHandlers(
      {
        expr
        NULL
      },
      warning = function(w) {
        warnings[[length(warnings) + 1L]] <<- w
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  list(warnings = warnings, error = err)
}

test_that("a limit reached during the listing warns once and leaves the condition", {
  reads <- local_failed_run(encoders = function() abort_timeout("FFmpeg", 5))
  got <- collect_conditions(ffm_run(pipeline_with(video = "libx264")))

  # One warning, of the probe-timeout class, naming the frame the error names.
  expect_length(got$warnings, 1L)
  w <- got$warnings[[1]]
  expect_s3_class(w, "tidymedia_probe_timeout")
  expect_identical(w$tm_limit, 5)
  expect_match(cli::ansi_strip(conditionMessage(w)), "timed out after 5 seconds",
               fixed = TRUE)
  expect_identical(rlang::call_name(conditionCall(w)), "ffm_run")
  # The same run's error is the merge-base exit condition.
  expect_identical(class(got$error), exit_classes)
  expect_identical(reads$encoders, 1L)
})

test_that("a single-file verb's two runs warn once when the listing hangs", {
  # separate_audio_video() runs two pipelines. Both fail here, and the listing
  # read reaches the limit on the first, so the second skips the diagnosis.
  bad <- "nosuchenc"
  asks <- 0L
  local_mocked_bindings(
    run_program = function(location, args, ...) {
      if (any(args %in% bad)) structure(character(), status = 8L) else character()
    },
    cached_encoder_names = function() {
      asks <<- asks + 1L
      abort_timeout("FFmpeg", 5)
    },
    cached_encodable_codecs = function() character()
  )
  f <- make_input()
  dir <- withr::local_tempdir()
  got <- collect_conditions(
    separate_audio_video(f, file.path(dir, "a.m4a"), file.path(dir, "v.mp4"),
                         audio_codec = bad, video_codec = bad, audio_stream = 0)
  )
  classes <- vapply(got$warnings, function(w) class(w)[[1]], character(1))
  expect_identical(classes, "tidymedia_probe_timeout")
  expect_identical(asks, 1L)
  expect_s3_class(got$error, "tidymedia_ffmpeg_exit")
})

test_that("the codec listing is remembered once read, and only once read", {
  forget_ffmpeg_capabilities()
  withr::defer(forget_ffmpeg_capabilities())
  asks <- 0L
  fail <- TRUE
  local_mocked_bindings(ffmpeg_codecs = function(...) {
    asks <<- asks + 1L
    if (fail) stop("no listing")
    tibble::tibble(name = c("h264", "pcm_bluray"), encoding = c(TRUE, FALSE))
  })
  # A failed read stores nothing, so the next diagnosis asks again.
  expect_error(cached_encodable_codecs(), "no listing")
  expect_null(.tm_capabilities$encodable_codecs)
  fail <- FALSE
  expect_identical(cached_encodable_codecs(), "h264")
  expect_identical(cached_encodable_codecs(), "h264")
  expect_identical(asks, 2L)
  # The routes that discard the encoder pool discard this listing too: all of
  # them go through forget_ffmpeg_capabilities() (D089).
  forget_ffmpeg_capabilities()
  expect_null(.tm_capabilities$encodable_codecs)
})

test_that("the codec listing has one reader, the diagnosis", {
  graph <- tm_call_graph()
  callers <- names(Filter(function(callees) "cached_encodable_codecs" %in% callees,
                          graph))
  expect_identical(callers, "diagnose_missing_encoders")
})

test_that("the hardware availability abort carries the same class and fields", {
  # The option seam is the instrument: it names this machine's hardware
  # encoders, so `character(0)` is a machine with none, and no FFmpeg is asked.
  withr::local_options(tidymedia.hardware_encoders = character(0))
  f <- make_input()
  out <- withr::local_tempfile(fileext = ".mp4")
  cnd <- tryCatch(
    standardize_video(f, out, hardware = "nvenc", run = FALSE),
    error = function(e) e
  )
  expect_identical(
    class(cnd),
    c("tidymedia_encoder_unavailable", "rlang_error", "error", "condition")
  )
  expect_identical(cnd$tm_encoder, "h264_nvenc")
  expect_identical(cnd$tm_stream, "video")
  expect_identical(rlang::call_name(conditionCall(cnd)), "standardize_video")

  # The fan-out front door reaches the same function, so the same class.
  jobs <- tibble::tibble(input = f, output = out)
  bcnd <- tryCatch(
    standardize_video_batch(jobs, hardware = "videotoolbox", run = FALSE),
    error = function(e) e
  )
  expect_s3_class(bcnd, "tidymedia_encoder_unavailable")
  expect_identical(bcnd$tm_encoder, "h264_videotoolbox")

  # The control: the machine has the encoder, and the call compiles.
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
  expect_no_error(standardize_video(f, out, hardware = "nvenc", run = FALSE))
})

# The single-file blame sweep (AC3) -------------------------------------------

# The domain is computed, never listed: every export the namespace call graph
# shows reaching ffm_run() and not ffm_batch(), less ffm_batch() itself, plus
# ffm_run() by name (it does not reach itself in the graph).
tm_single_run_domain <- function() {
  graph <- tm_call_graph()
  exports <- getNamespaceExports("tidymedia")
  reach <- vapply(exports, function(e) {
    isTRUE(tm_reaches(graph, e, "ffm_run")) &&
      !isTRUE(tm_reaches(graph, e, "ffm_batch"))
  }, logical(1))
  sort(c(setdiff(exports[reach], "ffm_batch"), "ffm_run"))
}

# Exports in that domain whose pipeline carries no codec but "copy" under any
# arguments, so no call of theirs can fail on a missing encoder. Read off their
# compiled commands: two set `-codec:v copy -codec:a copy` and take no codec
# argument, and two set no `-codec` at all.
tm_single_run_no_codec <- c(
  concatenate_videos = "copies both streams; no codec argument",
  strip_metadata = "copies both streams; no codec argument",
  extract_frame = "sets no codec; the image format follows the file name",
  sample_frames = "sets no codec; the image format follows the file name"
)

# One call per export that sets a codec the mocked build lacks. `bad` is that
# codec. format_for_web() has no codec argument and hardcodes libx264 and aac,
# which the mocked build also lacks.
tm_single_run_calls <- function(f, f2, dir, bad) {
  out <- file.path(dir, "out.mp4")
  regions <- data.frame(x = 0, y = 0, width = 8, height = 8)
  list(
    anonymize_video = list(f, out, regions, video_codec = bad),
    compare_videos = list(c(f, f2), out, video_codec = bad),
    convert_audio = list(f, file.path(dir, "out.m4a"), audio_codec = bad),
    crop_video = list(f, out, width = 8, height = 8, video_codec = bad),
    extract_audio = list(f, file.path(dir, "out.m4a"), audio_codec = bad),
    ffm_run = list(ffm_codec(ffm_files(f, out), video = bad)),
    format_for_web = list(f, out),
    normalize_audio = list(f, file.path(dir, "out.m4a"), audio_codec = bad),
    picture_in_picture = list(f, f2, out, video_codec = bad),
    separate_audio_video = list(f, file.path(dir, "a.m4a"),
                                file.path(dir, "v.mp4"),
                                audio_codec = bad, video_codec = "copy"),
    standardize_video = list(f, out, video_codec = bad)
  )
}

test_that("a single-file call names the function the caller typed", {
  bad <- "nosuchenc"
  # A run fails only where its arguments name a codec the mocked build lacks,
  # so the other half of separate_audio_video() succeeds, as the criterion
  # requires for that verb.
  local_mocked_bindings(
    run_program = function(location, args, ...) {
      if (any(args %in% c(bad, "libx264", "aac"))) {
        structure(character(), status = 8L)
      } else {
        character()
      }
    },
    cached_encoder_names = function() character(),
    cached_encodable_codecs = function() character()
  )
  f <- make_input()
  f2 <- make_input()
  dir <- withr::local_tempdir()
  calls <- tm_single_run_calls(f, f2, dir, bad)

  domain <- tm_single_run_domain()
  # The domain is not empty, and the calls and the listed exceptions together
  # are exactly it: a new export reaching ffm_run() must be placed in one.
  expect_gt(length(domain), 10L)
  expect_setequal(c(names(calls), names(tm_single_run_no_codec)), domain)
  expect_length(intersect(names(calls), names(tm_single_run_no_codec)), 0L)

  for (name in names(calls)) {
    cnd <- tryCatch(
      # By name, so the call the condition records carries the function's
      # name rather than the function object.
      suppressWarnings(do.call(name, calls[[name]],
                               envir = asNamespace("tidymedia"))),
      error = function(e) e
    )
    expect_s3_class(cnd, "tidymedia_encoder_unavailable")
    expect_identical(rlang::call_name(conditionCall(cnd)), name, info = name)
  }

  # separate_audio_video() runs two pipelines, and the call above fails only
  # its audio half. Here the audio half copies and succeeds, and the video half
  # names the missing encoder, so the video run's own frame is what is read.
  vcnd <- tryCatch(
    suppressWarnings(separate_audio_video(
      f, file.path(dir, "a2.m4a"), file.path(dir, "v2.mp4"),
      audio_codec = "copy", video_codec = bad
    )),
    error = function(e) e
  )
  expect_s3_class(vcnd, "tidymedia_encoder_unavailable")
  expect_identical(vcnd$tm_stream, "video")
  expect_identical(rlang::call_name(conditionCall(vcnd)), "separate_audio_video")
})

test_that("the listed exceptions compile no codec but copy", {
  f <- make_input()
  f2 <- make_input()
  dir <- withr::local_tempdir()
  commands <- list(
    concatenate_videos = concatenate_videos(c(f, f2), file.path(dir, "o.mp4"),
                                            run = FALSE),
    strip_metadata = strip_metadata(f, file.path(dir, "o.mp4"), run = FALSE),
    extract_frame = extract_frame(f, file.path(dir, "o.png"), timestamp = 0.1,
                                  run = FALSE),
    sample_frames = sample_frames(f, dir, fps = 1, run = FALSE)
  )
  expect_setequal(names(commands), names(tm_single_run_no_codec))
  for (name in names(commands)) {
    codecs <- regmatches(commands[[name]],
                         gregexpr("-codec:[av] \\S+", commands[[name]]))[[1]]
    expect_true(all(grepl(" copy$", codecs)), info = name)
  }
})

test_that("a condition the diagnosis does not touch keeps naming ffm_run()", {
  # A verb's run that fails with every codec listed: the merge-base exit
  # condition, whose frame is still ffm_run()'s although the verb set
  # `tm_call` on the pipeline.
  local_mocked_bindings(
    run_program = function(...) structure(character(), status = 8L),
    cached_encoder_names = function() c("libx264", "aac"),
    cached_encodable_codecs = function() character()
  )
  f <- make_input()
  out <- withr::local_tempfile(fileext = ".mp4")
  cnd <- tryCatch(standardize_video(f, out, audio_codec = "aac"),
                  error = function(e) e)
  expect_identical(class(cnd), exit_classes)
  expect_identical(rlang::call_name(conditionCall(cnd)), "ffm_run")
})

# The batch warning (AC4) ------------------------------------------------------

# A jobs table whose `vc`/`ac` columns are the codecs, and a mocked run that
# fails exactly where a row names one of `failing`.
local_batch_run <- function(failing, env = parent.frame()) {
  testthat::local_mocked_bindings(
    run_program = function(location, args, ...) {
      if (any(args %in% failing)) structure(character(), status = 8L) else character()
    },
    cached_encoder_names = function() c("libx264", "aac"),
    cached_encodable_codecs = function() c("h264", "aac"),
    .env = env
  )
}

batch_jobs <- function(vc, ac, env = parent.frame()) {
  n <- length(vc)
  tibble::tibble(
    input = vapply(seq_len(n), function(i) make_input(env = env), character(1)),
    output = vapply(seq_len(n), function(i) {
      withr::local_tempfile(fileext = ".mp4", .local_envir = env)
    }, character(1)),
    vc = vc,
    ac = ac
  )
}

build_row <- function(input, output, vc, ac) {
  ffm_codec(ffm_files(input, output), video = vc, audio = ac)
}

test_that("a batch runs every row and warns once with the diagnosed rows", {
  # Row 2 lacks its video encoder, row 3 its audio encoder, row 4 fails for
  # another reason with every codec listed, and rows 1 and 5 succeed.
  local_batch_run(c("nosuch_v", "nosuch_a", "boom"))
  rows <- batch_jobs(
    vc = c("libx264", "nosuch_v", "libx264", "h264", "libx264"),
    ac = c("aac", "aac", "nosuch_a", "aac", "aac")
  )
  # Row 4 fails on an output option, outside the pipeline's codecs.
  f <- function(input, output, vc, ac) {
    p <- build_row(input, output, vc, ac)
    if (identical(vc, "h264")) p <- ffm_output_options(p, "-boom boom")
    p
  }
  for (progress in c(FALSE, TRUE)) {
    w <- NULL
    res <- withCallingHandlers(
      ffm_batch(rows, f, progress = progress),
      tidymedia_encoder_unavailable = function(cnd) {
        expect_null(w)
        w <<- cnd
        invokeRestart("muffleWarning")
      }
    )
    info <- paste("progress =", progress)
    expect_identical(res$success, c(TRUE, FALSE, FALSE, FALSE, TRUE), info = info)
    expect_s3_class(w, "warning")
    expect_identical(w$tm_row, c(2L, 3L), info = info)
    expect_identical(w$tm_encoder, c("nosuch_v", "nosuch_a"), info = info)
    expect_identical(w$tm_stream, c("video", "audio"), info = info)
    msg <- cli::ansi_strip(conditionMessage(w))
    expect_match(msg, "2 jobs failed while naming an encoder", fixed = TRUE)
    expect_match(msg, 'No video encoder "nosuch_v": row 2.', fixed = TRUE)
    expect_match(msg, 'No audio encoder "nosuch_a": row 3.', fixed = TRUE)
  }
})

test_that("one encoder missing on several rows is one bullet naming them all", {
  local_batch_run("nosuch_v")
  jobs <- batch_jobs(vc = c("nosuch_v", "libx264", "nosuch_v"),
                     ac = c("aac", "aac", "aac"))
  w <- tryCatch(ffm_batch(jobs, build_row),
                tidymedia_encoder_unavailable = function(cnd) cnd)
  expect_identical(w$tm_row, c(1L, 3L))
  expect_match(cli::ansi_strip(conditionMessage(w)),
               'No video encoder "nosuch_v": rows 1, 3.', fixed = TRUE)
})

test_that("a batch whose listing read hangs warns once and reads once", {
  # Three failing rows. The listing read reaches the limit on the first; the
  # later rows skip the diagnosis, and the batch gives one warning (D049).
  asks <- 0L
  local_mocked_bindings(
    run_program = function(...) structure(character(), status = 8L),
    cached_encoder_names = function() {
      asks <<- asks + 1L
      abort_timeout("FFmpeg", 5)
    },
    cached_encodable_codecs = function() character()
  )
  jobs <- batch_jobs(vc = rep("nosuch_v", 3), ac = rep("aac", 3))
  for (progress in c(FALSE, TRUE)) {
    asks <- 0L
    info <- paste("progress =", progress)
    got <- collect_conditions(
      res <- ffm_batch(jobs, build_row, progress = progress)
    )
    expect_null(got$error, info = info)
    classes <- vapply(got$warnings, function(w) class(w)[[1]], character(1))
    expect_identical(classes, "tidymedia_probe_timeout", info = info)
    expect_identical(got$warnings[[1]]$tm_limit, 5, info = info)
    expect_identical(asks, 1L, info = info)
    expect_identical(res$success, c(FALSE, FALSE, FALSE), info = info)
  }
})

test_that("another check's probe timeout does not skip the video diagnosis", {
  # separate_audio_video() skips the video run's diagnosis only for the
  # encoder check's own timeout warning. A timeout warning from another check
  # in the audio half (here a stand-in without `tm_check`) leaves the video
  # condition diagnosed; the control, carrying `tm_check = "encoders"`, does
  # skip it.
  local_failed_run(encoders = character(), codecs = character())
  f <- make_input()
  dir <- withr::local_tempdir()
  video_error_for <- function(check) {
    local_mocked_bindings(run_separation_audio = function(...) {
      cli::cli_warn("stand-in timeout", class = "tidymedia_probe_timeout",
                    tm_check = check)
      cli::cli_abort("audio failed", class = "tidymedia_ffmpeg_exit",
                     tm_status = 8L)
    })
    cnd <- tryCatch(
      suppressWarnings(separate_audio_video(
        f, file.path(dir, "a.m4a"), file.path(dir, "v.mp4"),
        audio_codec = "aac", video_codec = "nosuch_v"
      )),
      error = function(e) e
    )
    cnd$tm_video_error
  }
  other <- video_error_for(NULL)
  expect_s3_class(other, "tidymedia_encoder_unavailable")
  expect_identical(other$tm_encoder, "nosuch_v")
  own <- video_error_for("encoders")
  expect_s3_class(own, "tidymedia_ffmpeg_exit")
  expect_false(inherits(own, "tidymedia_encoder_unavailable"))
})

test_that("the two listing reads ask FFmpeg with -hide_banner", {
  # FFmpeg prints its version banner to stderr, which ffmpeg() does not
  # capture. The reads pass -hide_banner so it does not reach the console.
  commands <- character()
  local_mocked_bindings(ffmpeg = function(command) {
    commands <<- c(commands, command)
    character()
  })
  try(ffmpeg_encoders(), silent = TRUE)
  try(ffmpeg_codecs(), silent = TRUE)
  expect_identical(commands, c("-hide_banner -encoders", "-hide_banner -codecs"))
})

test_that("successful runs and dry runs read neither listing", {
  # AC2's success and dry-run paths, at the three sites that run pipelines:
  # a batch, the two separation runs, and a verb's run = FALSE.
  reads <- local_failed_run()
  local_mocked_bindings(run_program = function(...) character())
  jobs <- batch_jobs(vc = c("libx264", "nosuch_v"), ac = c("aac", "aac"))
  expect_no_warning(res <- ffm_batch(jobs, build_row))
  expect_identical(res$success, c(TRUE, TRUE))
  f <- make_input()
  dir <- withr::local_tempdir()
  separate_audio_video(f, file.path(dir, "a.m4a"), file.path(dir, "v.mp4"),
                       audio_codec = "nosuch_a", video_codec = "nosuch_v",
                       audio_stream = 0)
  standardize_video(f, file.path(dir, "s.mp4"), video_codec = "nosuch_v",
                    run = FALSE)
  expect_identical(reads$encoders, 0L)
  expect_identical(reads$codecs, 0L)

  # The control: the same counters move on a failed run, so the zeros above
  # are the paths' doing and not a counter that never counts.
  local_mocked_bindings(run_program = function(...) {
    structure(character(), status = 8L)
  })
  expect_error(standardize_video(f, file.path(dir, "s2.mp4"),
                                 video_codec = "nosuch_v"),
               class = "tidymedia_encoder_unavailable")
  expect_identical(reads$encoders, 1L)
  expect_identical(reads$codecs, 1L)
})

test_that("a batch with no diagnosed row, and a dry-run batch, do not warn", {
  local_batch_run("boom")
  jobs <- batch_jobs(vc = c("libx264", "libx264"), ac = c("aac", "aac"))
  expect_no_warning(res <- ffm_batch(jobs, build_row))
  expect_identical(res$success, c(TRUE, TRUE))

  local_batch_run("nosuch_v")
  bad <- batch_jobs(vc = c("nosuch_v", "nosuch_v"), ac = c("aac", "aac"))
  expect_no_warning(dry <- ffm_batch(bad, build_row, run = FALSE))
  expect_false("success" %in% names(dry))
})

test_that("a batch verb's codec column reaches the warning by its row", {
  # The column overrides the argument on row 2 only; the diagnosis reads the
  # codec off the pipeline, so the override is what it sees.
  local_batch_run("nosuch_v")
  jobs <- batch_jobs(vc = c("libx264", "libx264"), ac = c("aac", "aac"))
  jobs <- tibble::tibble(input = jobs$input, output = jobs$output,
                         video_codec = c(NA, "nosuch_v"))
  w <- tryCatch(
    standardize_video_batch(jobs, video_codec = "libx264", audio_codec = "aac"),
    tidymedia_encoder_unavailable = function(cnd) cnd
  )
  expect_identical(w$tm_row, 2L)
  expect_identical(w$tm_encoder, "nosuch_v")
})

# Wrapper cells ---------------------------------------------------------------

local_build_model <- function(hide, env = parent.frame()) {
  skip_if_no_ffmpeg()
  skip_on_os("windows")
  dir <- normalizePath(testthat::test_path("..", "..", "tools", "ffmpeg_build_model"),
                       mustWork = FALSE)
  testthat::skip_if_not(file.exists(file.path(dir, "ffmpeg")),
                        "the source tree's tools/ffmpeg_build_model is not here")
  real_path <- Sys.getenv("PATH")
  tm_redirect_config(env = env)
  withr::local_envvar(
    PATH = paste(dir, real_path, sep = .Platform$path.sep),
    TIDYMEDIA_HIDE_ENCODERS = hide,
    .local_envir = env
  )
  forget_ffmpeg_capabilities()
  withr::defer(forget_ffmpeg_capabilities(), envir = env)
  testthat::skip_if_not(identical(dirname(find_ffmpeg()), dir),
                        "the build-model wrapper does not resolve first")
  invisible(dir)
}

test_that("a real run naming a hidden encoder is diagnosed (wrapper)", {
  infile <- make_test_video()
  local_build_model("libx264")
  out <- withr::local_tempfile(fileext = ".mp4")
  p <- ffm_codec(ffm_files(infile, out), video = "libx264", audio = "aac")
  cnd <- tryCatch(ffm_run(p), error = function(e) e)
  expect_identical(class(cnd), diagnosed_classes)
  expect_identical(cnd$tm_encoder, "libx264")
  expect_false(identical(cnd$tm_status, 0L))

  # The control: the same run with a different encoder hidden succeeds, so the
  # failure above is the hidden encoder's.
  withr::local_envvar(TIDYMEDIA_HIDE_ENCODERS = "libx265")
  forget_ffmpeg_capabilities()
  out2 <- withr::local_tempfile(fileext = ".mp4")
  p2 <- ffm_codec(ffm_files(infile, out2), video = "libx264", audio = "aac")
  expect_no_error(ffm_run(p2))
})

test_that("parallel workers carry the diagnosed rows back (wrapper)", {
  # Mocks do not reach a worker, so this cell runs real FFmpeg through the
  # wrapper in two fresh PSOCK workers. The workers load an INSTALLED tidymedia
  # (M53), so the cell skips unless their diagnosis is this source's.
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")
  infile <- make_test_video()
  local_build_model("libx264")
  cl <- parallel::makePSOCKcluster(2L)
  withr::defer(parallel::stopCluster(cl))
  old_plan <- future::plan(future::cluster, workers = cl)
  withr::defer(future::plan(old_plan))
  fingerprint <- function() {
    ns <- tryCatch(asNamespace("tidymedia"), error = function(e) NULL)
    if (is.null(ns) || !exists("diagnose_missing_encoders", envir = ns)) {
      return(NA_character_)
    }
    paste(deparse(body(get("diagnose_missing_encoders", envir = ns))),
          collapse = "")
  }
  seen <- unlist(furrr::future_map_chr(1:4, function(i) fingerprint(),
                                       .options = furrr::furrr_options(chunk_size = 1)))
  if (!isTRUE(all(seen == fingerprint()))) {
    skip("workers load an installed tidymedia whose diagnosis differs from the source")
  }

  jobs <- tibble::tibble(
    input = c(infile, infile),
    output = c(withr::local_tempfile(fileext = ".mp4"),
               withr::local_tempfile(fileext = ".mp4")),
    vc = c("libx264", "mpeg4"),
    ac = c("aac", "aac")
  )
  w <- NULL
  res <- withCallingHandlers(
    ffm_batch(jobs, build_row, parallel = TRUE),
    tidymedia_encoder_unavailable = function(cnd) {
      w <<- cnd
      invokeRestart("muffleWarning")
    },
    warning = function(cnd) invokeRestart("muffleWarning")
  )
  expect_identical(res$success, c(FALSE, TRUE))
  expect_identical(w$tm_row, 1L)
  expect_identical(w$tm_encoder, "libx264")
})

test_that("a real codec-name run is not diagnosed while its encoder is hidden (wrapper)", {
  # `h264` is a codec name the wrapper does not intercept, and `-codecs` still
  # marks it encodable. An unknown output option makes the run fail, so the
  # diagnosis does run, reads the real listings, and must leave the plain exit
  # condition (M139 review pass 2: without the forced failure the run succeeded
  # and this cell could not fail).
  infile <- make_test_video()
  local_build_model("libx264")
  out <- withr::local_tempfile(fileext = ".mp4")
  p <- ffm_codec(ffm_files(infile, out), video = "h264", audio = "aac")
  p <- ffm_output_options(p, "-boom boom")
  cnd <- tryCatch(ffm_run(p), error = function(e) e)
  expect_identical(class(cnd), exit_classes)
  expect_false(identical(cnd$tm_status, 0L))
})
