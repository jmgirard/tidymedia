# M70: the domain the silence rule quantifies over, COMPUTED rather than
# recalled.
#
# M69 promised a partition of the package's timeout behavior and wrote it by
# hand. Three review passes each found one more member the list omitted, and the
# third return's own finding named `remove_audio()` -- a function this package
# does not export (D048). A promise whose domain is fixed by what the author
# recalled is not repaired by recalling harder, so the domain is derived here
# from the package's own call graph.
#
# The route is M59's: `mget(ls(asNamespace("tidymedia"), all.names = TRUE))`
# reads the INSTALLED namespace, so this runs under `R CMD check` as well as
# `devtools::test()` -- a sweep that reads the source tree finds nothing in
# exactly the run the release gate uses (LESSONS M51/M59).

# The spawn primitives. `run_program()` is deliberately NOT listed: it is a
# package function whose own body names `system2()`, so the closure below picks
# it up. Listing it would make the sweep partly a hand-list again.
tm_spawn_primitives <- c("system", "system2")

# R's own condition API -- the handler set, recorded here rather than inferred.
# A function that installs none of these cannot stop a condition raised beneath
# it: it propagates by R's semantics. That is the stated reason such a function
# is out of the absorber partition below, and it is the reason the partition is
# explanatory rather than the domain itself (see `tm_timeout_domain()`).
tm_condition_api <- c(
  "tryCatch", "withCallingHandlers", "try", "try_fetch",
  "suppressWarnings", "suppressMessages"
)

# tm_symbol_graph(): name -> every symbol its body mentions.
#
# NOT M62's `tm_call_graph()` (`helper-input-paths.R`), and the difference is
# load-bearing rather than a second copy of one graph (M40). That one records
# the HEAD of each call, which is right for its question and wrong for this one:
# `probe_all()` reaches a spawn only through `purrr::map(infile, probe_one)`,
# where `probe_one` is an argument and never a head, so a call-head walk drops
# `probe_all()` and the four `probe_*()` accessors and `verify_media()` out of
# the domain entirely.
#
# The two guards therefore err in OPPOSITE directions on purpose. M62's asks
# which verbs must carry a check, so a spurious member there is a false alarm.
# This one asks where a hang can hide, so a missing member is a silent hang --
# the exact failure M69 shipped three times. Mentioning a name it does not call
# costs one test cell; not mentioning one it does costs the promise.
#
# `all.names()` on the language object rather than a regex over `deparse()`:
# both read the same object, but the parser already knows which tokens are
# names, so a program name inside a STRING ("run_program failed") cannot fake
# an edge.
tm_symbol_graph <- function() {
  ns <- asNamespace("tidymedia")
  objs <- mget(ls(ns, all.names = TRUE), envir = ns, ifnotfound = list(NULL))
  fns <- objs[vapply(objs, is.function, logical(1))]
  lapply(fns, function(f) unique(all.names(body(f))))
}

# tm_spawn_sites(): the namespace functions that name a spawn primitive in
# their OWN body -- the seed set `tm_reaches_spawn()` grows its closure from.
#
# Named rather than left inline (M070 wrote it as the first statement of
# `tm_reaches_spawn()`, which returns only the closure) because M072 asks a
# question about the seeds themselves: a per-call limit has to arrive at every
# place a process is actually started, and that is this set, not the closure
# around it. `seeds` stays a parameter for the same reason it is one below.
tm_spawn_sites <- function(graph = tm_symbol_graph(),
                           seeds = tm_spawn_primitives) {
  sort(names(graph)[
    vapply(graph, function(x) any(seeds %in% x), logical(1))
  ])
}

# tm_reaches_spawn(): every namespace function from which a spawn primitive is
# reachable. `seeds` is a parameter so a test can vary it -- passing an empty
# seed set must collapse the result, which is what proves membership comes from
# the closure and not from the recorded list.
tm_reaches_spawn <- function(graph = tm_symbol_graph(),
                             seeds = tm_spawn_primitives) {
  reaches <- tm_spawn_sites(graph, seeds)
  repeat {
    grown <- union(reaches, names(graph)[
      vapply(graph, function(x) any(reaches %in% x), logical(1))
    ])
    if (setequal(grown, reaches)) break
    reaches <- grown
  }
  sort(reaches)
}

# tm_timeout_domain(): the exported functions the silence rule binds.
#
# EXPORTED, because "a condition the caller can see" is a claim about the
# caller, and the only caller a package can promise anything to is the one
# outside it. An internal helper legitimately hands its outcome up by return
# value -- `probe_one()` returns the absorbed-timeout sentinel and `probe_all()`
# turns it into the warning -- so a rule quantified over internals would forbid
# the package's own working design.
#
# NO handler filter is applied here, and that is deliberate. Every spawn goes
# through `run_program()`, which installs `guard_timeout()`'s handler, so
# "absorption is possible somewhere below" is true of every member and filtering
# on it excludes nothing. `tm_timeout_absorbers()` keeps the partition as the
# explanation it is.
tm_timeout_domain <- function() {
  sort(intersect(getNamespaceExports("tidymedia"), tm_reaches_spawn()))
}

# tm_timeout_absorbers(): the reaching functions that install a handler in their
# OWN body -- the ones that can turn a timeout into a return value instead of
# letting it through. Explanatory, not the domain: it is what a reader consults
# to see where a silence could come from.
tm_timeout_absorbers <- function(graph = tm_symbol_graph()) {
  reaches <- tm_reaches_spawn(graph)
  installs <- names(graph)[
    vapply(graph, function(x) any(tm_condition_api %in% x), logical(1))
  ]
  sort(intersect(reaches, installs))
}

# tm_timeout_recorded_domain(): membership as it stands, recorded so drift is
# visible. This is NOT the domain -- `tm_timeout_domain()` computes that -- and
# the difference is the whole point of M70: an export that starts reaching a
# spawn joins the computed set on its own, and this list reddens until someone
# looks at it.
tm_timeout_recorded_domain <- function() {
  sort(c(
    "anonymize_video", "anonymize_video_batch", "compare_videos",
    "compare_videos_batch", "concatenate_videos", "concatenate_videos_batch",
    "convert_audio", "convert_audio_batch", "crop_video", "crop_video_batch",
    "extract_audio", "extract_audio_batch", "extract_frame",
    "extract_frame_batch", "ffm_batch", "ffm_run", "ffmpeg", "ffmpeg_codecs",
    "ffmpeg_encoders", "ffprobe", "format_for_web", "format_for_web_batch",
    "get_duration", "get_frame_rate", "get_height", "get_sample_rate",
    "get_width", "has_nvenc", "mediainfo", "mediainfo_parameter",
    "mediainfo_query", "mediainfo_summary", "mediainfo_template",
    "normalize_audio", "normalize_audio_batch", "picture_in_picture",
    "picture_in_picture_batch", "probe_all", "probe_audio", "probe_container",
    "probe_streams", "probe_video", "sample_frames", "sample_frames_batch",
    "segment_video", "segment_video_batch", "separate_audio_video",
    "separate_audio_video_batch", "standardize_video",
    "standardize_video_batch", "strip_metadata", "strip_metadata_batch",
    "verify_media"
  ))
}

# tm_timeout_call_specs(): one valid argument set per domain member, so a forced
# timeout can be driven THROUGH each of them.
#
# This is a hand-written table, and it is bounded by a procedure anyway: the
# domain comes from the sweep, and a member with no cell here fails
# `test-timeout-silence.R` rather than being quietly skipped. That is the
# property M69's hand-list did not have.
#
# The files are empty placeholders. Nothing reads them: every spawn is
# intercepted before a binary sees the path, and the verbs only need the paths
# to exist for their own `check_file_exists()`.
tm_timeout_call_specs <- function(dir) {
  vid <- file.path(dir, "in.mp4")
  vid2 <- file.path(dir, "in2.mp4")
  for (f in c(vid, vid2)) if (!file.exists(f)) file.create(f)
  outv <- file.path(dir, "out.mp4")
  outa <- file.path(dir, "out.m4a")
  png <- file.path(dir, "out.png")
  regions <- tibble::tibble(x = 0, y = 0, width = 10, height = 10)

  jobs_v <- tibble::tibble(input = vid, output = outv)
  jobs_a <- tibble::tibble(input = vid, output = outa)
  jobs_multi <- tibble::tibble(inputs = list(c(vid, vid2)), output = outv)

  list(
    anonymize_video = list(infile = vid, outfile = outv, regions = regions),
    anonymize_video_batch =
      list(jobs = tibble::tibble(input = vid, output = outv,
                                 regions = list(regions))),
    compare_videos = list(infiles = c(vid, vid2), outfile = outv),
    compare_videos_batch = list(jobs = jobs_multi),
    concatenate_videos = list(infiles = c(vid, vid2), outfile = outv),
    concatenate_videos_batch = list(jobs = jobs_multi),
    convert_audio = list(infile = vid, outfile = outa),
    convert_audio_batch = list(jobs = jobs_a),
    crop_video = list(infile = vid, outfile = outv, width = 10, height = 10),
    crop_video_batch = list(jobs = jobs_v, width = 10, height = 10),
    extract_audio = list(infile = vid, outfile = outa),
    extract_audio_batch = list(jobs = jobs_a),
    extract_frame = list(infile = vid, outfile = png, timestamp = 1),
    extract_frame_batch =
      list(jobs = tibble::tibble(input = vid, output = png, timestamp = 1)),
    ffm_batch = list(jobs = jobs_v,
                     .f = function(input, output, ...) ffm(input, output)),
    ffm_run = list(object = ffm(vid, outv)),
    ffmpeg = list(command = "-version"),
    ffmpeg_codecs = list(),
    ffmpeg_encoders = list(),
    ffprobe = list(command = "-version"),
    format_for_web = list(infile = vid, outfile = outv),
    format_for_web_batch = list(jobs = jobs_v),
    get_duration = list(file = vid),
    get_frame_rate = list(file = vid),
    get_height = list(file = vid),
    get_sample_rate = list(file = vid),
    get_width = list(file = vid),
    has_nvenc = list(),
    mediainfo = list(command = "--Version"),
    mediainfo_parameter =
      list(file = vid, section = "General", parameter = "Duration"),
    mediainfo_query =
      list(file = vid, section = "General", parameters = "Duration"),
    mediainfo_summary = list(file = vid),
    mediainfo_template = list(file = vid),
    normalize_audio = list(infile = vid, outfile = outa),
    normalize_audio_batch = list(jobs = jobs_a),
    picture_in_picture = list(main = vid, overlay = vid2, outfile = outv),
    picture_in_picture_batch =
      list(jobs = tibble::tibble(main = vid, overlay = vid2, output = outv)),
    probe_all = list(infile = vid),
    probe_audio = list(infile = vid),
    probe_container = list(infile = vid),
    probe_streams = list(infile = vid),
    probe_video = list(infile = vid),
    sample_frames = list(infile = vid, outdir = dir, fps = 1),
    sample_frames_batch =
      list(jobs = tibble::tibble(input = vid, outdir = dir), fps = 1),
    segment_video = list(infile = vid, start = 0, end = 1, outfiles = outv),
    segment_video_batch =
      list(jobs = tibble::tibble(input = vid, start = 0, end = 1,
                                 output = outv)),
    separate_audio_video =
      list(infile = vid, audiofile = outa, videofile = outv),
    separate_audio_video_batch =
      list(jobs = tibble::tibble(input = vid, audiofile = outa,
                                 videofile = outv)),
    standardize_video = list(infile = vid, outfile = outv),
    standardize_video_batch = list(jobs = jobs_v),
    strip_metadata = list(infile = vid, outfile = outv),
    strip_metadata_batch = list(jobs = jobs_v),
    verify_media = list(file = vid, width = 320)
  )
}

# tm_force_timeout(): drive a forced timeout through one domain member and
# report every condition it signalled.
#
# The timeout is INJECTED at the package's two spawn wrappers -- `run_program()`
# and `guard_timeout()`, which between them stand in front of every
# `system()`/`system2()` call the package makes -- rather than produced by a
# real hung binary. The injected object is `abort_timeout()`'s own, the same
# condition the kernel path raises, and the writer-less-FIFO tests at the end of
# this file are what tie that object to what a hung binary really produces. A
# FIFO per member would cost ~42 s each (base R's SIGINT/SIGTERM/SIGKILL ladder,
# M69/D047) and could not reach the members that take no file argument at all.
#
# Injecting at the wrappers rather than at `is_timeout()` is what keeps this
# platform-independent: forcing the VERDICT would still let the three Layer 0
# hatches really shell out, and on a machine with no media binaries
# `system(intern = TRUE)` raises a cmdError on "command not found" before the
# verdict is ever consulted -- so those cells would measure the runner's PATH.
# CI's macOS and Windows runners install no media binaries at all.
#
# What each cell therefore asks is exactly AC1's question: given a timeout
# signalled beneath it, does this function let the caller see it? Whether the
# wrappers themselves detect a real kill is M69's question, answered by
# `is_timeout()`'s own tests and by the FIFO anchors.
tm_force_timeout <- function(name, args, limit = 2) {
  warns <- character()
  err <- NULL
  withr::with_options(list(tidymedia.timeout = limit), {
    testthat::local_mocked_bindings(
      run_program = function(location, args, program = "the program", ...) {
        abort_timeout(program, limit)
      },
      guard_timeout = function(program, limit, expr, ...) {
        abort_timeout(program, limit)
      },
      .package = "tidymedia"
    )
    withCallingHandlers(
      tryCatch(
        do.call(name, args, envir = asNamespace("tidymedia")),
        error = function(e) err <<- e
      ),
      warning = function(w) {
        warns <<- c(warns, cli::ansi_strip(conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    )
  })
  list(
    error = err,
    aborted = inherits(err, "tidymedia_timeout"),
    warned = any(grepl("timed out", warns, fixed = TRUE)),
    warnings = warns
  )
}

# local_blocking_input(): a FIFO nobody writes to, so a media program blocks on
# its header forever and the test does not race the machine's encoding speed.
#
# Defined here rather than in M69's test file because two suites now anchor
# against it: M69's execution tests and M70's grid, which needs the REAL hang to
# tie its injected condition to what a binary actually produces. A second copy
# is how the two stop agreeing (M40). The reasoning behind the fixture and its
# skips is in test-runtime-timeout.R, above the tests that use it.
#
# Windows has no mkfifo, so the gate skips there. The fixture is built INSIDE
# the gate -- a platform that cannot create it must not reach the creation
# call -- and the gate skips rather than fail()s, because testthat::fail()
# RECORDS a failure and RETURNS, falling on into the operation it guards (M68).
local_blocking_input <- function(env = parent.frame()) {
  skip_on_os("windows")
  skip_on_cran()
  skip_if_no_ffmpeg()
  path <- withr::local_tempfile(fileext = ".mp4", .local_envir = env)
  ok <- suppressWarnings(system2("mkfifo", shQuote(path)))
  if (!identical(as.integer(ok), 0L) || !file.exists(path)) {
    skip("could not create a FIFO to block on")
  }
  path
}

# tm_program_literals(): every program name a timeout refusal can be built from.
#
# AC4 asks that one literal names each program across every path that can abort
# about it. The receiving functions are named because they are the ones that
# BUILD a refusal -- run_program()/guard_timeout()/abort_timeout() take the name
# straight into the message, and capture_version() hands it to run_program().
# The alternative, sweeping every "ffmpeg"-ish string in the namespace, would
# rope in find_program("ffmpeg") and set_program("mediainfo"), where the
# lowercase form is the binary's real name and not a display literal.
#
# `fns` is a parameter so the mutation probe can feed a mutant body in: a guard
# whose only falsifier is deleting it re-certifies the mock rather than the
# code, which is the defect this task exists to fix.
tm_program_naming_calls <- c("run_program", "guard_timeout", "abort_timeout",
                             "capture_version")

tm_program_literals <- function(fns = NULL) {
  if (is.null(fns)) {
    ns <- asNamespace("tidymedia")
    objs <- mget(ls(ns, all.names = TRUE), envir = ns, ifnotfound = list(NULL))
    fns <- objs[vapply(objs, is.function, logical(1))]
  }
  out <- list()
  walk <- function(e, where) {
    if (is.call(e)) {
      head <- e[[1]]
      if (is.name(head) && as.character(head) %in% tm_program_naming_calls) {
        lit <- tm_program_arg(e, as.character(head))
        if (!is.null(lit)) {
          out[[length(out) + 1L]] <<- stats::setNames(lit, where)
        }
      }
    }
    if (is.call(e) || is.pairlist(e)) {
      for (i in seq_along(e)) {
        if (rlang::is_missing(e[[i]]) || is.null(e[[i]])) next
        walk(e[[i]], where)
      }
    }
  }
  for (nm in names(fns)) walk(body(fns[[nm]]), nm)
  unlist(out)
}

# The program name's position differs by callee, and three of the four take it
# positionally at their own spawn sites -- guard_timeout("FFmpeg", limit, ...)
# in the Layer 0 hatches especially. A named argument wins where one is given;
# otherwise the k-th argument that carries no name is read, which is what
# R itself would match.
tm_program_positions <- list(
  run_program = list(name = "program", pos = 3L),
  guard_timeout = list(name = "program", pos = 1L),
  abort_timeout = list(name = "program", pos = 1L),
  capture_version = list(name = "name", pos = 2L)
)

tm_program_arg <- function(e, callee) {
  spec <- tm_program_positions[[callee]]
  args <- as.list(e)[-1]
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  val <- if (spec$name %in% nms) {
    args[[which(nms == spec$name)[[1]]]]
  } else {
    unnamed <- which(!nzchar(nms))
    if (length(unnamed) >= spec$pos) args[[unnamed[[spec$pos]]]] else NULL
  }
  if (is.character(val) && length(val) == 1L) val else NULL
}

