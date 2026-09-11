# M125 -- one cell per output-collision form per export, and a control per cell.
#
# The DOMAIN is computed, never listed: every export reaching ffm_batch() in the
# package call graph (input_guard_verbs(), helper-input-paths.R). The cells
# supply only the SHAPE of a colliding call and of its control, and the
# completeness test in test-batch-output-collision.R fails on a domain export
# that has no cell.
#
# Every path is relative to the scratch directory local_collision_files()
# makes the working directory, so a destination reaches the message as a short
# string the assertion can match whole. The inputs exist (the verbs sweep them);
# no destination does.
#
# A cell's `bad` and `ok` calls differ only in what their destinations resolve
# to. Where the export can start a program before it hands off to ffm_batch(),
# both calls set the arguments that make it start one -- a hardware backend with
# `fallback = FALSE` (the capability memo is cleared per call), `audio_stream`
# left unset with the track check on, `two_pass = TRUE` -- and `spawns` names the
# `run` values under which the control is expected to start one. The control
# asserting that is what shows a bad call's "no program started" is not vacuous.
tm_collision_cells <- function() {
  tb <- tibble::tibble
  hw <- list(hardware = "nvenc", fallback = FALSE)
  both <- c("FALSE", "TRUE")
  regions <- function(n) {
    rep(list(data.frame(x = 0, y = 0, width = 10, height = 10)), n)
  }
  cell <- function(verb, form, bad, ok, dest, spawns = character()) {
    list(verb = verb, form = form, bad = bad, ok = ok, dest = dest,
         spawns = spawns)
  }
  aa <- c("a.mp4", "a.mp4")

  list(
    cell("anonymize_video_batch", "output column",
         c(list(jobs = tb(input = aa, output = c("o.mp4", "o.mp4"),
                          regions = regions(2))), hw),
         c(list(jobs = tb(input = aa, output = c("o1.mp4", "o2.mp4"),
                          regions = regions(2))), hw),
         "o.mp4", both),
    cell("anonymize_video_batch", "derived output (repeated input)",
         c(list(jobs = tb(input = aa, regions = regions(2))), hw),
         c(list(jobs = tb(input = c("a.mp4", "b.mp4"),
                          regions = regions(2))), hw),
         "a.mp4", both),

    cell("compare_videos_batch", "output column",
         c(list(jobs = tb(inputs = list(c("a.mp4", "b.mp4"), c("a.mp4", "b.mp4")),
                          output = c("o.mp4", "o.mp4"))), hw),
         c(list(jobs = tb(inputs = list(c("a.mp4", "b.mp4"), c("a.mp4", "b.mp4")),
                          output = c("o1.mp4", "o2.mp4"))), hw),
         "o.mp4", both),

    cell("concatenate_videos_batch", "output column",
         list(jobs = tb(inputs = list(c("a.mp4", "b.mp4"), c("a.mp4", "b.mp4")),
                        output = c("o.mp4", "o.mp4"))),
         list(jobs = tb(inputs = list(c("a.mp4", "b.mp4"), c("a.mp4", "b.mp4")),
                        output = c("o1.mp4", "o2.mp4"))),
         "o.mp4"),

    cell("convert_audio_batch", "output column",
         list(jobs = tb(input = c("a.wav", "a.wav"),
                        output = c("o.mp3", "o.mp3"))),
         list(jobs = tb(input = c("a.wav", "a.wav"),
                        output = c("o1.mp3", "o2.mp3"))),
         "o.mp3", "TRUE"),

    cell("crop_video_batch", "output column",
         c(list(jobs = tb(input = aa, output = c("o.mp4", "o.mp4")),
                width = 10, height = 10), hw),
         c(list(jobs = tb(input = aa, output = c("o1.mp4", "o2.mp4")),
                width = 10, height = 10), hw),
         "o.mp4", both),
    cell("crop_video_batch", "derived output (repeated input)",
         c(list(jobs = tb(input = aa), width = 10, height = 10), hw),
         c(list(jobs = tb(input = c("a.mp4", "b.mp4")), width = 10,
                height = 10), hw),
         "a_cropped.mp4", both),

    cell("extract_audio_batch", "output column",
         list(jobs = tb(input = aa, output = c("o.aac", "o.aac"))),
         list(jobs = tb(input = aa, output = c("o1.aac", "o2.aac"))),
         "o.aac", "TRUE"),

    cell("extract_frame_batch", "output column",
         list(jobs = tb(input = aa, timestamp = c(1, 2),
                        output = c("f.png", "f.png"))),
         list(jobs = tb(input = aa, timestamp = c(1, 2),
                        output = c("f1.png", "f2.png"))),
         "f.png"),
    # Numbering restarts per input and the extension is the image format, so
    # two inputs sharing a stem under different containers derive one name.
    cell("extract_frame_batch", "derived output (shared stem)",
         list(jobs = tb(input = c("a.mp4", "a.mkv"), timestamp = c(1, 1))),
         list(jobs = tb(input = c("a.mp4", "b.mp4"), timestamp = c(1, 1))),
         "a_1.png"),

    cell("format_for_web_batch", "output column",
         c(list(jobs = tb(input = aa, output = c("o.mp4", "o.mp4"))), hw),
         c(list(jobs = tb(input = aa, output = c("o1.mp4", "o2.mp4"))), hw),
         "o.mp4", both),
    # Every derived name ends `_web.mp4`, whatever the source container.
    cell("format_for_web_batch", "derived output (shared stem)",
         c(list(jobs = tb(input = c("a.mov", "a.mkv"))), hw),
         c(list(jobs = tb(input = c("a.mov", "b.mp4"))), hw),
         "a_web.mp4", both),

    cell("normalize_audio_batch", "output column",
         list(jobs = tb(input = c("a.wav", "a.wav"),
                        output = c("o.wav", "o.wav"))),
         list(jobs = tb(input = c("a.wav", "a.wav"),
                        output = c("o1.wav", "o2.wav"))),
         "o.wav", "TRUE"),
    cell("normalize_audio_batch", "output column, two_pass = TRUE",
         list(jobs = tb(input = c("a.wav", "a.wav"),
                        output = c("o.wav", "o.wav")), two_pass = TRUE),
         list(jobs = tb(input = c("a.wav", "a.wav"),
                        output = c("o1.wav", "o2.wav")), two_pass = TRUE),
         "o.wav", both),
    cell("normalize_audio_batch", "derived output (repeated input)",
         list(jobs = tb(input = c("a.wav", "a.wav"))),
         list(jobs = tb(input = c("a.wav", "b.wav"))),
         "a.wav", "TRUE"),
    cell("normalize_audio_batch", "derived output (repeated input), two_pass = TRUE",
         list(jobs = tb(input = c("a.wav", "a.wav")), two_pass = TRUE),
         list(jobs = tb(input = c("a.wav", "b.wav")), two_pass = TRUE),
         "a.wav", both),

    cell("picture_in_picture_batch", "output column",
         c(list(jobs = tb(main = aa, overlay = c("b.mp4", "b.mp4"),
                          output = c("o.mp4", "o.mp4"))), hw),
         c(list(jobs = tb(main = aa, overlay = c("b.mp4", "b.mp4"),
                          output = c("o1.mp4", "o2.mp4"))), hw),
         "o.mp4", both),

    cell("sample_frames_batch", "derived directory (repeated input)",
         list(jobs = tb(input = aa), fps = 1),
         list(jobs = tb(input = c("a.mp4", "b.mp4")), fps = 1),
         "a_frames/a_%06d.png"),
    cell("sample_frames_batch", "outdir column",
         list(jobs = tb(input = c("x/a.mp4", "y/a.mp4"), outdir = c("d", "d")),
              fps = 1),
         list(jobs = tb(input = c("x/a.mp4", "y/a.mp4"), outdir = c("d1", "d2")),
              fps = 1),
         "d/a_%06d.png"),
    cell("sample_frames_batch", "outdir argument",
         list(jobs = tb(input = c("x/a.mp4", "y/a.mp4")), outdir = "d", fps = 1),
         list(jobs = tb(input = c("x/a.mp4", "y/b.mp4")), outdir = "d", fps = 1),
         "d/a_%06d.png"),

    cell("segment_video", "outfiles argument",
         c(list(infile = "a.mp4", start = c(0, 1), end = c(1, 2),
                outfiles = c("s.mp4", "s.mp4")), hw),
         c(list(infile = "a.mp4", start = c(0, 1), end = c(1, 2),
                outfiles = c("s1.mp4", "s2.mp4")), hw),
         "s.mp4", both),

    cell("segment_video_batch", "output column",
         c(list(jobs = tb(input = aa, start = c(0, 1), end = c(1, 2),
                          output = c("o.mp4", "o.mp4"))), hw),
         c(list(jobs = tb(input = aa, start = c(0, 1), end = c(1, 2),
                          output = c("o1.mp4", "o2.mp4"))), hw),
         "o.mp4", both),

    cell("separate_audio_video_batch", "audiofile and videofile in one row",
         c(list(jobs = tb(input = "a.mp4", audiofile = "s.mkv",
                          videofile = "s.mkv"), video_codec = "libx264"), hw),
         c(list(jobs = tb(input = "a.mp4", audiofile = "s.mka",
                          videofile = "s.mkv"), video_codec = "libx264"), hw),
         "s.mkv", both),
    cell("separate_audio_video_batch", "audiofile column across rows",
         c(list(jobs = tb(input = aa, audiofile = c("au.mka", "au.mka"),
                          videofile = c("v1.mkv", "v2.mkv")),
                video_codec = "libx264"), hw),
         c(list(jobs = tb(input = aa, audiofile = c("au1.mka", "au2.mka"),
                          videofile = c("v1.mkv", "v2.mkv")),
                video_codec = "libx264"), hw),
         "au.mka", both),
    cell("separate_audio_video_batch", "videofile column across rows",
         c(list(jobs = tb(input = aa, audiofile = c("au1.mka", "au2.mka"),
                          videofile = c("v.mkv", "v.mkv")),
                video_codec = "libx264"), hw),
         c(list(jobs = tb(input = aa, audiofile = c("au1.mka", "au2.mka"),
                          videofile = c("v1.mkv", "v2.mkv")),
                video_codec = "libx264"), hw),
         "v.mkv", both),
    cell("separate_audio_video_batch", "audiofile of one row, videofile of another",
         c(list(jobs = tb(input = aa, audiofile = c("p.mka", "au2.mka"),
                          videofile = c("v1.mkv", "p.mka")),
                video_codec = "libx264"), hw),
         c(list(jobs = tb(input = aa, audiofile = c("p.mka", "au2.mka"),
                          videofile = c("v1.mkv", "v2.mkv")),
                video_codec = "libx264"), hw),
         "p.mka", both),

    cell("standardize_video_batch", "output column",
         c(list(jobs = tb(input = aa, output = c("o.mp4", "o.mp4"))), hw),
         c(list(jobs = tb(input = aa, output = c("o1.mp4", "o2.mp4"))), hw),
         "o.mp4", both),
    cell("standardize_video_batch", "derived output (repeated input)",
         c(list(jobs = tb(input = aa)), hw),
         c(list(jobs = tb(input = c("a.mp4", "b.mp4"))), hw),
         "a.mp4", both),

    cell("strip_metadata_batch", "output column",
         list(jobs = tb(input = aa, output = c("o.mp4", "o.mp4"))),
         list(jobs = tb(input = aa, output = c("o1.mp4", "o2.mp4"))),
         "o.mp4"),
    cell("strip_metadata_batch", "derived output (repeated input)",
         list(jobs = tb(input = aa)),
         list(jobs = tb(input = c("a.mp4", "b.mp4"))),
         "a_stripped.mp4")
  )
}

# The scratch tree every cell's relative paths name, made the working directory
# for the calling test.
local_collision_files <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_dir(dir, .local_envir = env)
  dir.create("x")
  dir.create("y")
  file.create(c("a.mp4", "b.mp4", "a.mkv", "a.mov", "a.wav", "b.wav",
                "x/a.mp4", "y/a.mp4", "y/b.mp4"))
  invisible(dir)
}

# Call `verb` with `args` and `run`, and report the condition it ended on and
# every program it tried to start.
#
# guard_timeout() is where every system()/system2() call in the namespace is
# evaluated (tm_spawn_interception_complete(), helper-timeout-sweep.R), and the
# stub never forces `expr`, so a recorded name is a program that would have
# started and none did. find_program() is stubbed so a probe that first
# resolves its binary -- the track check does -- reaches the spawn on a machine
# without one. ffm_batch() is stubbed so a call the front door lets through
# ends there, and a refusal made inside the runner cannot pass for the verb's.
tm_collision_run <- function(verb, args, run) {
  spawned <- character()
  args$run <- run
  testthat::local_mocked_bindings(
    find_program = function(program = "ffmpeg", ...) {
      file.path("/nonexistent", program[[1]])
    },
    guard_timeout = function(program, limit, expr, ...) {
      spawned <<- c(spawned, program)
      cli::cli_abort("M125 stub: {program} would start here.",
                     class = "tm_m125_spawn")
    },
    ffm_batch = function(...) {
      cli::cli_abort("M125 stub: the call reached ffm_batch().",
                     class = "tm_m125_batch")
    },
    .package = "tidymedia"
  )
  withr::local_options(tidymedia.hardware_encoders = NULL,
                       tidymedia.check_tracks = TRUE,
                       tidymedia.timeout = NULL)
  forget_ffmpeg_capabilities()
  cnd <- tryCatch(
    suppressWarnings(do.call(verb, args, envir = asNamespace("tidymedia"))),
    error = function(e) e
  )
  list(cnd = cnd, spawned = spawned)
}

# Whether `cnd` or any condition it wraps carries `class` -- purrr wraps an
# error raised inside pmap(), which is where the two-pass analysis spawns.
tm_cnd_has <- function(cnd, class) {
  while (inherits(cnd, "condition")) {
    if (inherits(cnd, class)) return(TRUE)
    cnd <- cnd$parent
  }
  FALSE
}
