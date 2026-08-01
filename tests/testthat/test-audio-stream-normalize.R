# M49: `audio_stream` on normalize_audio() (+ _batch) and on the two-pass
# analysis pass, under a FIRST-TRACK unselected case rather than D026's
# every-track one (D028).
#
# This verb emitted NO `-map` before M49, so FFmpeg's implicit selection chose
# for it: one stream of each type, preferring whichever audio track carries the
# container's DEFAULT disposition. Measured on-branch before any source edit
# (ffmpeg 8.1.2, macOS; a 3-audio-track .mkv, eng/spa/fra, DEFAULT moved to
# track 2): the output carried only `fra` -- the THIRD track. The change here is
# therefore DETERMINISM, not cardinality: one track before, one track after, but
# now a stated one.
#
# Why the carve-out, and why it is measured rather than argued: under an
# every-track map (`-map 0:a?`) the two-pass analysis pass prints one JSON
# measurement block PER MAPPED TRACK (three, measured on this fixture) while
# classify_loudnorm_output() reads hit[[1]] (R/loudnorm_two_pass.R), so every
# mapped track would be corrected with track 0's measurements, silently. Making
# that uniform needs per-stream filter options the linear builder has no slot
# for -- its own ROADMAP candidate row.
#
# The analysis pass carries the AUDIO half only (`0:a:0?` or `0:a:<n>`, no
# `0:v?`): it writes to `-f null` and has no output for a video selection to
# describe. What it must never do is measure a different track from the one the
# correction pass normalizes.
#
# The pre-M49 commands, recorded from master at a4fc322 and committed here as
# templates taking the map arguments -- templates rather than a comparison
# against "what master returns today", which stops being checkable the moment
# this branch merges (M47's lesson, M48's shape). Both carried no map at all, so
# every `maps` default is the empty string.
#
#   correction: -y -i "<f>" -af "loudnorm=I=-23:TP=-1:LRA=7" -codec:v copy \
#                  "out.mkv"
#   analysis:   -y -i "<f>" -af "loudnorm=I=-23:TP=-1:LRA=7:print_format=json" \
#                  -f null "-"

normalize_command <- function(infile, maps = "", outfile = "out.mkv") {
  paste0(
    '-y -i "', infile, '" -af "loudnorm=I=-23:TP=-1:LRA=7" -codec:v copy ',
    maps, '"', outfile, '"'
  )
}

analysis_command <- function(infile, maps = "") {
  paste0(
    '-y -i "', infile, '"',
    ' -af "loudnorm=I=-23:TP=-1:LRA=7:print_format=json" -f null ',
    maps, '"-"'
  )
}

# Count -map arguments. A containment assertion cannot see a duplicate, and
# ffm_map() appends since M43, so the count is the discriminator (M43/M45/M47).
norm_map_count <- function(cmd) {
  vapply(cmd, function(x) {
    sum(gregexpr("-map ", x, fixed = TRUE)[[1]] > 0)
  }, integer(1), USE.NAMES = FALSE)
}

normalize_of <- function(f, ...) normalize_audio(f, "out.mkv", ..., run = FALSE)

normalize_jobs <- function(f, ...) {
  tibble::tibble(input = c(f, f), output = c("a.mkv", "b.mkv"), ...)
}


# AC2: the scalar verb ---------------------------------------------------------

test_that("normalize_audio() with no selection compiles the first audio track", {
  f <- make_input()
  expect_identical(normalize_of(f),
                   normalize_command(f, "-map 0:v? -map 0:a:0? "))
  expect_identical(normalize_of(f, audio_stream = NULL), normalize_of(f))
  expect_identical(norm_map_count(normalize_of(f)), 2L)
})

test_that("normalize_audio()'s unselected case is NOT the every-track map", {
  # The discriminator for the carve-out. Without this, a later change back to
  # D026's uniform `0:a?` would leave every assertion above green.
  f <- make_input()
  expect_false(grepl("-map 0:a? ", normalize_of(f), fixed = TRUE))
})

test_that("normalize_audio()'s unselected audio map carries the `?`", {
  # Load-bearing and independently measured: a bare `-map 0:a:0` exits 234 on a
  # video-only input, where master (emitting no map) exited 0. Without the `?`
  # this milestone would ship a regression on an ordinary research input.
  f <- make_input()
  expect_match(normalize_of(f), "-map 0:a:0?", fixed = TRUE)
})

test_that("normalize_audio(audio_stream = ) narrows the audio map only", {
  f <- make_input()
  expect_identical(normalize_of(f, audio_stream = 2),
                   normalize_command(f, "-map 0:v? -map 0:a:2 "))
  expect_false(grepl("0:a:2?", normalize_of(f, audio_stream = 2), fixed = TRUE))
})

test_that("normalize_audio(audio_stream = 0) names the same track as NULL but keeps no `?`", {
  # Unlike the pass-through verbs, NULL and 0 select the SAME track here. They
  # still compile differently, and the difference is the `?`: an explicit 0 on
  # an input with no audio must stay an FFmpeg error (D023), while the unset
  # case must keep master's exit 0.
  f <- make_input()
  expect_identical(normalize_of(f, audio_stream = 0),
                   normalize_command(f, "-map 0:v? -map 0:a:0 "))
  expect_false(identical(normalize_of(f, audio_stream = 0), normalize_of(f)))
})


# AC2: the audio-only-container rule ------------------------------------------

# Added at M49's review send-back. `-map 0:v?` forces the video stream into the
# output muxer, and the `?` covers an ABSENT stream, never a REJECTING muxer, so
# the first version of this milestone broke every audio-only output container:
# measured on `inst/extdata/sample.mp4` (ffmpeg 8.1.2), `.wav`/`.mp3`/`.aac`/
# `.opus` went from exit 0 on master to exit 234, and `.mka` silently GAINED a
# video stream where master wrote audio only. Master got this right by
# delegating to FFmpeg's implicit selection, which is muxer-aware -- but that is
# the same mechanism that picked the audio track nondeterministically, and only
# the AUDIO half needed removing. The video half now follows the output
# container, decided from the caller's own output path so the compile stays
# binary-free (D024).

test_that("an audio-only output container omits the video map", {
  f <- make_input()
  for (ext in c("wav", "mp3", "aac", "flac", "ogg", "opus", "m4a", "mka")) {
    cmd <- normalize_audio(f, paste0("out.", ext), run = FALSE)
    expect_false(grepl("-map 0:v?", cmd, fixed = TRUE),
                 label = paste("video map present for", ext))
    expect_match(cmd, "-map 0:a:0?", fixed = TRUE)
    expect_identical(norm_map_count(cmd), 1L)
  }
})

test_that("a video output container keeps the video map", {
  f <- make_input()
  for (ext in c("mp4", "mkv", "mov", "avi", "webm", "m4v")) {
    cmd <- normalize_audio(f, paste0("out.", ext), run = FALSE)
    expect_match(cmd, "-map 0:v? -map 0:a:0?", fixed = TRUE)
    expect_identical(norm_map_count(cmd), 2L)
  }
})

test_that("the container rule reads the output, not the input", {
  # The discriminator: a video INPUT written to an audio container takes the
  # audio-only shape, and an audio input written to a video container does not.
  # Reading the input would need a probe, which run = FALSE forbids (D024).
  f <- make_input(ext = "mp4")
  g <- make_input(ext = "wav")
  expect_false(grepl("0:v?", normalize_audio(f, "out.wav", run = FALSE), fixed = TRUE))
  expect_match(normalize_audio(g, "out.mkv", run = FALSE), "-map 0:v?", fixed = TRUE)
})

test_that("an unknown output extension keeps the video map", {
  # The list only ever ADDS working cases: anything not named in it compiles the
  # pass-through shape, which is what every video container needs.
  f <- make_input()
  expect_match(normalize_audio(f, "out.somethingnew", run = FALSE),
               "-map 0:v?", fixed = TRUE)
})

test_that("the container rule is case-insensitive", {
  f <- make_input()
  expect_false(grepl("0:v?", normalize_audio(f, "out.WAV", run = FALSE), fixed = TRUE))
})

test_that("a named track still narrows the audio map in an audio container", {
  f <- make_input()
  expect_identical(normalize_audio(f, "out.wav", audio_stream = 2, run = FALSE),
                   normalize_command(f, "-map 0:a:2 ", outfile = "out.wav"))
})

test_that("the batch verb applies the container rule per row", {
  # Outputs are per-row, so the rule is too -- a jobs table mixing an audio and
  # a video destination must compile two different map shapes.
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.wav", "b.mkv"))
  out <- normalize_audio_batch(jobs, run = FALSE)
  expect_identical(norm_map_count(out$command), c(1L, 2L))
  expect_false(grepl("0:v?", out$command[[1]], fixed = TRUE))
  expect_match(out$command[[2]], "-map 0:v?", fixed = TRUE)
})

test_that("the analysis pass is unaffected by the output container", {
  # It writes to `-f null` and never had a video map, so the rule cannot reach
  # it; the two passes must still name the same audio track.
  f <- make_input()
  expect_identical(
    ffm_compile(loudnorm_analysis_pipeline(f)),
    analysis_command(f, "-map 0:a:0? ")
  )
})


# AC2: the batch sibling -------------------------------------------------------

test_that("the normalize_audio_batch() argument reaches every row", {
  f <- make_input()
  out <- normalize_audio_batch(normalize_jobs(f), audio_stream = 2, run = FALSE)
  expect_true(all(grepl("-map 0:v? -map 0:a:2", out$command, fixed = TRUE)))
  expect_identical(norm_map_count(out$command), c(2L, 2L))
})

test_that("a normalize_audio_batch() audio_stream column overrides the argument per row", {
  f <- make_input()
  out <- normalize_audio_batch(normalize_jobs(f, audio_stream = c(1, NA)),
                               audio_stream = 2, run = FALSE)
  # NA is the column form of NULL, which on THIS verb means the first track --
  # it does not fall back to the argument, which is what an ABSENT column means.
  expect_match(out$command[[1]], "-map 0:v? -map 0:a:1", fixed = TRUE)
  expect_match(out$command[[2]], "-map 0:v? -map 0:a:0?", fixed = TRUE)
})

test_that("a one-row normalize_audio_batch() call matches the scalar call byte for byte", {
  f <- make_input()
  for (sel in list(NULL, 2)) {
    expect_identical(
      normalize_audio_batch(tibble::tibble(input = f, output = "out.mkv"),
                            audio_stream = sel, run = FALSE)$command,
      normalize_audio(f, "out.mkv", audio_stream = sel, run = FALSE)
    )
  }
})

test_that("a wrongly typed normalize_audio_batch() audio_stream column aborts up front", {
  f <- make_input()
  for (bad in list(c("0", "1"), c(TRUE, FALSE))) {
    err <- expect_error(
      normalize_audio_batch(normalize_jobs(f, audio_stream = bad), run = FALSE)
    )
    expect_match(conditionMessage(err), "audio_stream")
    # This verb's NA means the FIRST track, not every track -- the hint has to
    # say so, which is why check_batch_audio_col() takes `na_means` (M40).
    expect_match(conditionMessage(err), "keep the first audio track")
    expect_identical(rlang::call_name(conditionCall(err)),
                     "normalize_audio_batch")
  }
})

test_that("a bad audio_stream cell names the CALLER's row, not the reshaped one", {
  # The two-pass path corrects jobs[!silent, ], so a per-row abort from inside
  # the fan-out would index the reshaped table (M45 review F4). Checked up
  # front instead, against the caller's row numbers.
  f <- make_input()
  err <- expect_error(
    normalize_audio_batch(normalize_jobs(f, audio_stream = c(0, -1)),
                          run = FALSE)
  )
  expect_match(conditionMessage(err), "audio_stream")
  expect_match(conditionMessage(err), "2")
})

test_that("a scalar normalize_audio_batch(audio_stream = NA) aborts rather than compiling the default", {
  f <- make_input()
  expect_error(
    normalize_audio_batch(normalize_jobs(f), audio_stream = NA, run = FALSE),
    "audio_stream"
  )
})


# AC3: the analysis pass names the track the correction pass normalizes --------

test_that("the analysis pipeline maps the first audio track when unselected", {
  # Asserted on loudnorm_analysis_pipeline() directly, never through a verb
  # call: D013 makes the analysis pass run BEFORE `run` is consulted, so no
  # two_pass = TRUE call can yield this command without executing FFmpeg.
  f <- make_input()
  expect_identical(
    ffm_compile(loudnorm_analysis_pipeline(f)),
    analysis_command(f, "-map 0:a:0? ")
  )
  expect_identical(
    ffm_compile(loudnorm_analysis_pipeline(f, audio_stream = NULL)),
    ffm_compile(loudnorm_analysis_pipeline(f))
  )
})

test_that("the analysis pipeline maps a named track", {
  f <- make_input()
  expect_identical(
    ffm_compile(loudnorm_analysis_pipeline(f, audio_stream = 2)),
    analysis_command(f, "-map 0:a:2 ")
  )
})

test_that("the analysis pipeline carries the audio half only", {
  # It writes to `-f null` and has no output for a video selection to describe.
  # One map, never two.
  f <- make_input()
  for (sel in list(NULL, 0, 2)) {
    cmd <- ffm_compile(loudnorm_analysis_pipeline(f, audio_stream = sel))
    expect_identical(norm_map_count(cmd), 1L)
    expect_false(grepl("0:v", cmd, fixed = TRUE))
  }
})

test_that("the analysis pipeline never maps every audio track", {
  # The specific failure D028 exists to prevent: `0:a?` makes loudnorm print one
  # JSON block per mapped track while the parser reads only the first.
  f <- make_input()
  expect_false(
    grepl("0:a?", ffm_compile(loudnorm_analysis_pipeline(f)), fixed = TRUE)
  )
})

test_that("the analysis and correction commands never name different tracks", {
  # The invariant AC3 exists for, stated as one comparison rather than as two
  # separate expectations that could drift apart.
  f <- make_input()
  audio_map <- function(cmd) {
    m <- regmatches(cmd, regexpr("0:a[^\" ]*", cmd))
    if (length(m) == 0) NA_character_ else m
  }
  for (sel in list(NULL, 0, 1, 2)) {
    analysis <- ffm_compile(loudnorm_analysis_pipeline(f, audio_stream = sel))
    correction <- normalize_of(f, audio_stream = sel)
    expect_identical(audio_map(analysis), audio_map(correction))
  }
})


# AC7 / D024: run = FALSE stays binary-free on the single-pass path ------------

test_that("normalize_audio()'s new argument runs no binary under run = FALSE", {
  f <- make_input()
  # Count invocations rather than stop()ing in the mock: these call sites sit
  # under tryCatch() in places, which swallows a raising mock and leaves the
  # test green with the gate it exists to pin deleted (M44).
  n <- 0L
  local_mocked_bindings(
    run_program = function(...) {
      n <<- n + 1L
      list(status = 0L, stdout = character(), stderr = character())
    },
    find_ffmpeg = function(...) {
      n <<- n + 1L
      "ffmpeg"
    },
    find_ffprobe = function(...) {
      n <<- n + 1L
      "ffprobe"
    }
  )
  normalize_audio(f, "out.mkv", audio_stream = 2, run = FALSE)
  normalize_audio_batch(normalize_jobs(f), audio_stream = 2, run = FALSE)
  expect_identical(n, 0L)
  # Prove the mock is in scope rather than silently inert: without this, `n == 0`
  # is equally consistent with "no binary ran" and "the mock never bound"
  # (M39/M44). two_pass = TRUE is the call that must trip it -- D024's stated
  # sole exception to the pure run = FALSE surface.
  try(normalize_audio(f, "out.mkv", two_pass = TRUE, run = FALSE), silent = TRUE)
  expect_gt(n, 0L)
})

test_that("a bad audio_stream aborts before the two-pass analysis runs", {
  # Hoisted with channels/sample_rate: an unchecked index would abort from the
  # correction pipeline AFTER the analysis pass had already burned a run.
  f <- make_input()
  n <- 0L
  local_mocked_bindings(
    run_program = function(...) {
      n <<- n + 1L
      list(status = 0L, stdout = character(), stderr = character())
    },
    find_ffmpeg = function(...) {
      n <<- n + 1L
      "ffmpeg"
    }
  )
  expect_error(
    normalize_audio(f, "out.mkv", two_pass = TRUE, audio_stream = -1),
    "audio_stream"
  )
  expect_identical(n, 0L)
})


# AC4: execution on a 3-audio-track .mkv with DEFAULT moved off track 0 --------

test_that("normalize_audio() with no selection carries the first audio track", {
  # T1's baseline: on master this wrote `fra`, the DEFAULT-disposition track.
  skip_if_no_ffmpeg()
  infile <- make_multitrack_video(default_track = 2)
  out <- withr::local_tempfile(fileext = ".mkv")
  normalize_audio(infile, out)
  expect_identical(audio_languages(out), "eng")
})

test_that("normalize_audio(audio_stream = ) writes exactly the named track", {
  skip_if_no_ffmpeg()
  infile <- make_multitrack_video(default_track = 2)
  out <- withr::local_tempfile(fileext = ".mkv")
  normalize_audio(infile, out, audio_stream = 1)
  expect_identical(audio_languages(out), "spa")
})

test_that("normalize_audio() still exits 0 on a video-only input", {
  # The `?` regression check. A bare `-map 0:a:0` exits 234 here; master, with
  # no map at all, exited 0 and this must keep doing so.
  skip_if_no_ffmpeg()
  infile <- make_silent_video()
  out <- withr::local_tempfile(fileext = ".mp4")
  expect_no_error(normalize_audio(infile, out))
  expect_true(file.exists(out) && file.size(out) > 0)
})

test_that("a named track the input lacks is an FFmpeg error, not an R one", {
  # D023/D026's third bullet, end to end. The discriminator is WHERE the error
  # comes from: an R-side range check would make this pass for the wrong reason,
  # so the message must name FFmpeg rather than the argument.
  skip_if_no_ffmpeg()
  infile <- make_multitrack_video(default_track = 2)
  out <- withr::local_tempfile(fileext = ".mkv")
  err <- expect_error(normalize_audio(infile, out, audio_stream = 9))
  expect_match(conditionMessage(err), "FFmpeg")
})

test_that("two-pass normalization measures and corrects the same track", {
  # The end-to-end form of AC3: the analysis pass runs for real here, so a
  # mismatch between the two commands would surface as a correction built from
  # another track's measurements.
  skip_if_no_ffmpeg()
  infile <- make_multitrack_video(default_track = 2)
  out <- withr::local_tempfile(fileext = ".mkv")
  cmd <- normalize_audio(infile, out, two_pass = TRUE, audio_stream = 1)
  expect_match(cmd, "-map 0:v? -map 0:a:1", fixed = TRUE)
  expect_identical(audio_languages(out), "spa")
})

test_that("the two-pass batch path carries a per-row audio_stream column", {
  # The batch two-pass path has two fan-outs -- the analysis phase and the
  # correction phase -- and the column has to reach BOTH. It also RESHAPES the
  # jobs table between them: the correction runs over jobs[!silent, ], so a
  # column that did not travel with the subset would be read off by one row.
  #
  # The silent row is therefore load-bearing and deliberately FIRST. Without it
  # `!silent` is all-TRUE, the subset is the identity, and this test asserts
  # nothing about the seam it names -- the false coverage M49's review caught
  # in its first version. With the silent row first, a misaligned column would
  # read the silent row's NA (the first track, `eng`) instead of row 2's 2.
  skip_if_no_ffmpeg()
  quiet <- make_silent_audio()
  loud <- make_multitrack_video(default_track = 2)
  a <- withr::local_tempfile(fileext = ".mka")
  b <- withr::local_tempfile(fileext = ".mkv")
  jobs <- tibble::tibble(input = c(quiet, loud), output = c(a, b),
                         audio_stream = c(NA, 2))
  # Assign inside expect_warning() rather than taking its return value: it
  # returns the condition, not the expression's value.
  res <- NULL
  expect_warning(res <- normalize_audio_batch(jobs, two_pass = TRUE), "silent")
  expect_identical(res$silent, c(TRUE, FALSE))
  expect_true(is.na(res$command[[1]]))
  expect_match(res$command[[2]], "-map 0:v? -map 0:a:2", fixed = TRUE)
  # The discriminator: `fra` is track 2, `eng` is what a one-row misalignment
  # would have produced.
  expect_identical(audio_languages(b), "fra")
})

test_that("no output container that worked before this milestone fails now", {
  # AC8, added at the review send-back. This is the coverage whose ABSENCE let a
  # green suite sit over a real regression: nothing in the package normalized to
  # an audio container, so `-map 0:v?` breaking every one of them was invisible.
  #
  # The reference column is master's measured behavior on this same input
  # (ffmpeg 8.1.2, inst/extdata/sample.mp4), recorded at the send-back:
  #   exit 0, audio only  -- wav mp3 aac opus flac mka
  #   exit 0, video+audio -- m4a mp4 mkv
  #   exit 234            -- ogg webm  (pre-existing; NOT this milestone's)
  # `.m4a` is the one deliberate divergence from master: it carried video there
  # and carries none here, because a caller who writes `.m4a` means audio.
  skip_if_no_ffprobe()
  infile <- system.file("extdata", "sample.mp4", package = "tidymedia")
  skip_if_not(nzchar(infile), "packaged sample video unavailable")
  audio_only <- c("wav", "mp3", "aac", "opus", "flac", "m4a", "mka")
  for (ext in c(audio_only, "mp4", "mkv")) {
    out <- withr::local_tempfile(fileext = paste0(".", ext))
    expect_no_error(normalize_audio(infile, out))
    expect_gt(file.size(out), 0)
    types <- stream_types(out)
    expect_true("audio" %in% types)
    # An audio-only container carries no video stream; a video one still does.
    if (ext %in% audio_only) {
      expect_false("video" %in% types, label = paste("video stream in", ext))
    } else {
      expect_true("video" %in% types, label = paste("no video stream in", ext))
    }
  }
})

test_that("the audio written to an audio-only container is actually normalized", {
  # Exit 0 and a non-empty file are not enough: the point of the verb is the
  # loudness. Re-measure the .wav output and require it near the target, so a
  # future "fix" that drops the filter or writes the source through unchanged
  # fails here rather than passing on file size.
  skip_if_no_ffmpeg()
  src <- make_dynamic_audio()
  out <- withr::local_tempfile(fileext = ".wav")
  normalize_audio(src, out, target_loudness = -23, two_pass = TRUE)
  measured <- run_loudnorm_analysis(out, target_loudness = -23)$i
  expect_lt(abs(measured - (-23)), 1)
})

test_that("the two-pass analysis pass yields exactly one measurement block", {
  # The measured reason for the carve-out, asserted rather than only recorded:
  # under an every-track map this fixture prints three blocks and the parser
  # silently uses the first.
  skip_if_no_ffmpeg()
  infile <- make_multitrack_video(default_track = 2)
  p <- loudnorm_analysis_pipeline(infile)
  out <- run_program(find_ffmpeg(), ffm_args(p), program = "FFmpeg",
                     input = "", stderr = TRUE)
  expect_identical(sum(grepl('"input_i"', out)), 1L)
})
