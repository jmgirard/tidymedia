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
# The verb no longer carries video AT ALL (D030). Two earlier attempts to state
# the video half both failed: `-map 0:v?` unconditionally broke every audio-only
# destination (exit 234 -- the `?` covers an ABSENT stream, not a REFUSING
# muxer), and enumerating the containers that refuse it missed six more
# (.w64/.mpa/.voc/.sbc/.latm/.adts). So the question was removed rather than
# answered again: this is an audio verb whose output is one audio stream, like
# extract_audio() and convert_audio(). No `-codec:v copy` either, since it named
# a stream that is never mapped.
#
# The unselected map carries NO trailing `?`, and that is measured rather than
# stylistic. When EVERY map specifier is optional and matches nothing, FFmpeg
# DISCARDS the maps and reverts to default stream selection: `-map 0:a:5?` on a
# video+audio file writes video AND audio. This verb emits exactly one map, so
# "all maps matched nothing" is reachable by an ordinary input -- a silent
# screen recording -- and with a `?` that call would exit 0 while silently
# writing the video through, via the very DEFAULT-disposition heuristic this
# milestone removes. Without it the input fails loudly at exit 234.
#
# The pre-M49 commands, recorded from master at a4fc322 and committed here as
# templates -- templates rather than a comparison against "what master returns
# today", which stops being checkable the moment this branch merges (M47's
# lesson, M48's shape). Both carried no map at all.
#
#   correction: -y -i "<f>" -af "loudnorm=I=-23:TP=-1:LRA=7" -codec:v copy \
#                  "out.mkv"
#   analysis:   -y -i "<f>" -af "loudnorm=I=-23:TP=-1:LRA=7:print_format=json" \
#                  -f null "-"

normalize_command <- function(infile, maps = "-map \"0:a:0\" ", outfile = "out.mkv") {
  paste0(
    '-y -i "', infile, '" -af "loudnorm=I=-23:TP=-1:LRA=7',
    ',asetnsamples=n=4096:p=0" ',
    maps, '"', outfile, '"'
  )
}

analysis_command <- function(infile, maps = "-map \"0:a:0\" ") {
  paste0(
    '-y -i "', infile, '"',
    ' -af "loudnorm=I=-23:TP=-1:LRA=7:print_format=json',
    ',asetnsamples=n=4096:p=0" -f null ',
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
                   normalize_command(f))
  expect_identical(normalize_of(f, audio_stream = NULL), normalize_of(f))
  expect_identical(norm_map_count(normalize_of(f)), 1L)
})

test_that("normalize_audio()'s unselected case is NOT the every-track map", {
  # The discriminator for the carve-out. Without this, a later change back to
  # D026's uniform `0:a?` would leave every assertion above green.
  f <- make_input()
  expect_false(grepl("-map \"0:a?\" ", normalize_of(f), fixed = TRUE))
})

test_that("normalize_audio(audio_stream = ) narrows the audio map only", {
  f <- make_input()
  expect_identical(normalize_of(f, audio_stream = 2),
                   normalize_command(f, "-map \"0:a:2\" "))
  expect_false(grepl("0:a:2?", normalize_of(f, audio_stream = 2), fixed = TRUE))
})

test_that("normalize_audio(audio_stream = 0) compiles exactly what NULL does", {
  # Unlike the pass-through verbs, NULL and 0 select the same track here, and
  # since D030 dropped the `?` they now compile the SAME command byte for byte.
  # That is the intended collapse: an input with no audio is an FFmpeg error
  # either way, so the two spellings have nothing left to differ about.
  f <- make_input()
  expect_identical(normalize_of(f, audio_stream = 0),
                   normalize_command(f, "-map \"0:a:0\" "))
  expect_identical(normalize_of(f, audio_stream = 0), normalize_of(f))
})


# AC2 / AC8: the output container is irrelevant --------------------------------

# The property that replaced the container list. Nothing about the compiled
# command depends on the output extension any more, which is what makes the
# "did we enumerate every audio-only container?" question unanswerable-by-
# construction rather than merely answered.

test_that("the compiled command is identical whatever the output container", {
  f <- make_input()
  base <- normalize_audio(f, "out.mkv", run = FALSE)
  for (ext in c("wav", "mp3", "aac", "opus", "flac", "m4a", "mka", "w64",
                "mpa", "voc", "sbc", "latm", "adts", "mp4", "mov", "webm",
                "somethingnew")) {
    cmd <- normalize_audio(f, paste0("out.", ext), run = FALSE)
    expect_identical(sub('"out\\.[^"]*"$', "", cmd), sub('"out\\.mkv"$', "", base),
                     label = paste("command differs for", ext))
  }
})

test_that("no compiled command ever maps video", {
  f <- make_input()
  for (out in c("out.mkv", "out.mp4", "out.wav", "out.w64")) {
    for (sel in list(NULL, 0, 2)) {
      cmd <- normalize_audio(f, out, audio_stream = sel, run = FALSE)
      expect_false(grepl("0:v", cmd, fixed = TRUE),
                   label = paste("video map in", out))
      expect_identical(norm_map_count(cmd), 1L)
    }
  }
})

test_that("no compiled command names a video codec", {
  # `-codec:v copy` named a stream that is never mapped; the compiled command is
  # the product (D001), so it is gone rather than merely inert.
  f <- make_input()
  expect_false(grepl("-codec:v", normalize_audio(f, "out.mkv", run = FALSE),
                     fixed = TRUE))
})

test_that("the unselected map carries no trailing `?`", {
  # The discriminator for D030's measured reason. With a `?`, an input with no
  # audio matches nothing, FFmpeg discards the map and reverts to default
  # selection, and the video is written through in silence.
  f <- make_input()
  expect_false(grepl("0:a:0?", normalize_audio(f, "out.mkv", run = FALSE),
                     fixed = TRUE))
  expect_match(normalize_audio(f, "out.mkv", run = FALSE), "-map \"0:a:0\"",
               fixed = TRUE)
})

test_that("the batch verb is container-independent too", {
  f <- make_input()
  jobs <- tibble::tibble(input = c(f, f), output = c("a.wav", "b.mkv"))
  out <- normalize_audio_batch(jobs, run = FALSE)
  expect_identical(norm_map_count(out$command), c(1L, 1L))
  expect_false(any(grepl("0:v", out$command, fixed = TRUE)))
})


# AC2: the batch sibling -------------------------------------------------------

test_that("the normalize_audio_batch() argument reaches every row", {
  f <- make_input()
  out <- normalize_audio_batch(normalize_jobs(f), audio_stream = 2, run = FALSE)
  expect_true(all(grepl("-map \"0:a:2\"", out$command, fixed = TRUE)))
  expect_identical(norm_map_count(out$command), c(1L, 1L))
})

test_that("a normalize_audio_batch() audio_stream column overrides the argument per row", {
  f <- make_input()
  out <- normalize_audio_batch(normalize_jobs(f, audio_stream = c(1, NA)),
                               audio_stream = 2, run = FALSE)
  # NA is the column form of NULL, which on THIS verb means the first track --
  # it does not fall back to the argument, which is what an ABSENT column means.
  expect_match(out$command[[1]], "-map \"0:a:1\"", fixed = TRUE)
  expect_match(out$command[[2]], "-map \"0:a:0\"", fixed = TRUE)
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
    analysis_command(f)
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
    analysis_command(f, "-map \"0:a:2\" ")
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

test_that("an input with no audio is an FFmpeg error, not a silent video copy", {
  # D030. The `?` is gone precisely so this fails: with it, the single map
  # matches nothing, FFmpeg discards the map, reverts to default selection, and
  # writes the VIDEO through at exit 0 -- the heuristic this milestone removes,
  # returning through the back door on the one input that can reach it.
  skip_if_no_ffmpeg()
  infile <- make_silent_video()
  out <- withr::local_tempfile(fileext = ".wav")
  err <- expect_error(normalize_audio(infile, out))
  expect_match(conditionMessage(err), "FFmpeg")
  # And nothing usable was written: no video sneaked through.
  expect_false(file.exists(out) && file.size(out) > 0)
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
  expect_match(cmd, "-map \"0:a:1\"", fixed = TRUE)
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
  expect_match(res$command[[2]], "-map \"0:a:2\"", fixed = TRUE)
  # The discriminator: `fra` is track 2, `eng` is what a one-row misalignment
  # would have produced.
  expect_identical(audio_languages(b), "fra")
})

test_that("every plausible output container works, audio-only or not", {
  # AC8. This is the coverage whose ABSENCE let a green suite sit over a real
  # regression twice: nothing in the package normalized to an audio container,
  # so first an unconditional `-map 0:v?` and then an incomplete list of
  # audio-only containers both went unnoticed.
  #
  # The list deliberately includes the six that the enumeration missed
  # (.w64 .mpa .voc .sbc .latm .adts) alongside the obvious ones, because those
  # are what falsified the previous approach. `.wma` is excluded and recorded
  # here as failing on master too (exit 234), so it is not this milestone's.
  skip_if_no_ffmpeg()
  skip_if_no_ffprobe()
  infile <- system.file("extdata", "sample.mp4", package = "tidymedia")
  skip_if_not(nzchar(infile), "packaged sample video unavailable")
  for (ext in c("wav", "mp3", "aac", "opus", "flac", "m4a", "mka", "oga",
                "w64", "mpa", "voc", "sbc", "latm", "adts", "mp4", "mkv",
                "mov")) {
    out <- withr::local_tempfile(fileext = paste0(".", ext))
    expect_no_error(normalize_audio(infile, out))
    expect_gt(file.size(out), 0)
    # Audio out, and never video -- whatever the container would have allowed.
    types <- stream_types(out)
    expect_identical(sum(types == "audio"), 1L,
                     label = paste("audio stream count in", ext))
    expect_false("video" %in% types, label = paste("video stream in", ext))
  }
})

test_that("a multi-track input still yields exactly one audio stream", {
  # The matrix above runs on a single-track fixture, so "one audio stream" and
  # "has audio" are indistinguishable there: a regression mapping `0:a?` would
  # stay green. This is the discriminator AC8's count actually needs.
  skip_if_no_ffprobe()
  infile <- make_multitrack_video(default_track = 2)
  for (ext in c("wav", "mka", "mkv", "mp4")) {
    out <- withr::local_tempfile(fileext = paste0(".", ext))
    normalize_audio(infile, out)
    types <- stream_types(out)
    expect_identical(sum(types == "audio"), 1L,
                     label = paste("audio stream count in", ext))
    expect_false("video" %in% types, label = paste("video stream in", ext))
  }
  # And it is the FIRST track, not the DEFAULT-disposition one.
  out <- withr::local_tempfile(fileext = ".mka")
  normalize_audio(infile, out)
  expect_identical(audio_languages(out), "eng")
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

test_that("the containers whose encoder takes the frame size it is handed work", {
  # Regression, FFmpeg 9. Single-pass `loudnorm` resamples to 192 kHz and hands
  # its consumer 192000-sample frames. An encoder with a FIXED frame size is
  # re-framed by FFmpeg on the way in, which is why the fifteen other containers
  # in the matrix above never saw this; flac and vorbis take whatever frame they
  # are given, and 192000 is past flac's 65535-sample block ceiling, so both died
  # at `Could not open encoder before EOF` (exit 234) leaving a zero-byte file.
  # The matrix above turns red on this too, but only by container name -- this
  # test names the cause, and the duration assertion fences the way the fix can
  # go wrong.
  skip_if_no_ffmpeg()
  skip_if_no_ffprobe()
  infile <- system.file("extdata", "sample.mp4", package = "tidymedia")
  skip_if_not(nzchar(infile), "packaged sample video unavailable")
  source_duration <- audio_duration(infile)
  for (ext in c("flac", "oga")) {
    out <- withr::local_tempfile(fileext = paste0(".", ext))
    expect_no_error(normalize_audio(infile, out))
    expect_gt(file.size(out), 0)
    # A re-chunk that PADS its last frame writes up to one frame of silence past
    # the source's length, which the file-size assertion above cannot see.
    expect_equal(audio_duration(out), source_duration, tolerance = 0.005,
                 label = paste("audio duration in", ext))
  }
})
