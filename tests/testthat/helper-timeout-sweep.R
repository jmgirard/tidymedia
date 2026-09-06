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
    "get_width", "has_hardware_encoder", "mediainfo", "mediainfo_parameter",
    "mediainfo_query", "mediainfo_template",
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
                     .f = function(input, output, ...) ffm_files(input, output)),
    ffm_run = list(object = ffm_files(vid, outv)),
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
    has_hardware_encoder = list(hardware = "nvenc"),
    mediainfo = list(command = "--Version"),
    mediainfo_parameter =
      list(file = vid, section = "General", parameter = "Duration"),
    mediainfo_query =
      list(file = vid, section = "General", parameters = "Duration"),
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


# M094: what an invalid `tidymedia.timeout` did on master, recorded ----------

# tm_timeout_bad_forms(): the invalid values `resolve_timeout()`'s own comment
# names (R/timeout.R:19-26) -- the negative, the fractional, the missing, the
# string and the length-2 forms. Recorded here rather than at each test site so
# the sweep and the wording test quantify over the same set, and so a form
# added to the checker's comment has one place to join.
tm_timeout_bad_forms <- function() {
  list(
    negative = -1,
    fractional = 0.5,
    missing = NA,
    string = "2",
    length_two = c(1, 2)
  )
}

# tm_blame_head(): drive one domain member under `limit` and report the head of
# whatever condition it raised.
#
# The member is called BY NAME, for helper-blame.R's reason: do.call() on a
# function OBJECT records the anonymous object as the condition call and hides
# the blame target this sweep exists to watch. `"<none>"` is a real answer, not
# a failure -- AC1's `has_hardware_encoder()` carve-out is exactly that cell.
tm_blame_head <- function(name, args, limit) {
  withr::with_options(list(tidymedia.timeout = limit), {
    cnd <- tryCatch(
      do.call(name, args, envir = asNamespace("tidymedia")),
      error = function(e) e
    )
    if (!inherits(cnd, "error")) "<none>" else blamed_verb(cnd)
  })
}

# tm_timeout_blame_master(): the head of the condition each domain member raised
# on master under an invalid limit -- measured 2026-08-30 at ae5ff1c, identical
# across all five `tm_timeout_bad_forms()` values at every member.
#
# Recorded because AC1's claim is a CHANGE, and the repo held no referent for
# what the blame was before. Six members already named themselves; the other 47
# named a function the caller never typed, which is the defect. `tm_blame_head()`
# regenerates any cell of this table.
tm_timeout_blame_master <- function() {
  ffm_run_class <- c(
    "anonymize_video", "compare_videos", "concatenate_videos", "convert_audio",
    "crop_video", "extract_audio", "extract_frame", "format_for_web",
    "normalize_audio", "picture_in_picture", "sample_frames",
    "separate_audio_video", "standardize_video", "strip_metadata"
  )
  ffm_batch_class <- c(
    "anonymize_video_batch", "compare_videos_batch", "concatenate_videos_batch",
    "convert_audio_batch", "crop_video_batch", "extract_audio_batch",
    "extract_frame_batch", "format_for_web_batch", "normalize_audio_batch",
    "picture_in_picture_batch", "sample_frames_batch", "segment_video",
    "segment_video_batch", "separate_audio_video_batch",
    "standardize_video_batch", "strip_metadata_batch"
  )
  ffmpeg_class <- c("ffmpeg_codecs", "ffmpeg_encoders", "has_hardware_encoder")
  mediainfo_parameter_class <- c(
    "get_duration", "get_frame_rate", "get_height", "get_sample_rate",
    "get_width"
  )
  mediainfo_read_class <- c(
    "mediainfo_query", "mediainfo_template"
  )
  probe_map_class <- c(
    "probe_all", "probe_audio", "probe_container", "probe_streams",
    "probe_video", "verify_media"
  )
  # The six that already named themselves.
  correct <- c("ffm_batch", "ffm_run", "ffmpeg", "ffprobe", "mediainfo",
               "mediainfo_parameter")
  out <- c(
    stats::setNames(rep("ffm_run", length(ffm_run_class)), ffm_run_class),
    stats::setNames(rep("ffm_batch", length(ffm_batch_class)),
                    ffm_batch_class),
    stats::setNames(rep("ffmpeg", length(ffmpeg_class)), ffmpeg_class),
    stats::setNames(rep("mediainfo_parameter",
                        length(mediainfo_parameter_class)),
                    mediainfo_parameter_class),
    stats::setNames(rep("mediainfo_read", length(mediainfo_read_class)),
                    mediainfo_read_class),
    stats::setNames(rep("purrr::map", length(probe_map_class)),
                    probe_map_class),
    stats::setNames(correct, correct)
  )
  out[order(names(out))]
}

# tm_timeout_reached_master(): what each domain member did when the limit was
# actually REACHED, on master -- "abort" or "warn", measured 2026-08-30 at
# ae5ff1c through `tm_force_timeout()`.
#
# D049 promises only that a reached limit is never SILENT, so the grid at
# test-timeout-silence.R records the disjunction and nothing finer. This table
# records which of the two each member does, so AC6 can say the blame sweep left
# that choice alone rather than only that it left some condition behind.
tm_timeout_reached_master <- function() {
  aborts <- c(
    "anonymize_video", "compare_videos", "concatenate_videos", "convert_audio",
    "crop_video", "extract_audio", "extract_frame", "ffm_run", "ffmpeg",
    "ffmpeg_codecs", "ffmpeg_encoders", "ffprobe", "format_for_web",
    "has_hardware_encoder", "mediainfo", "normalize_audio", "picture_in_picture",
    "sample_frames", "separate_audio_video", "standardize_video",
    "strip_metadata", "verify_media"
  )
  dom <- tm_timeout_domain()
  stats::setNames(ifelse(dom %in% aborts, "abort", "warn"), dom)
}

# M094: what the valid and unset paths did on master, recorded ---------------

# tm_spawn_interception_complete(): is mocking `guard_timeout()` enough to see
# every spawn the package makes?
#
# `tm_spawn_sites()` computes which namespace functions name `system`/`system2`
# in their own body. This asks the next question about that same computed set:
# at every one of them, is the spawn call syntactically an ARGUMENT of a
# `guard_timeout(...)` call? It is an argument, and `guard_timeout()` evaluates
# it lazily inside its own handler, so a mocked `guard_timeout()` that never
# forces `expr` intercepts the spawn rather than counting it after the fact.
#
# Without this the counter below would be trusted for a reason nobody checked:
# a new spawn added OUTSIDE the wrapper would be invisible to it and the AC5
# assertion would read "0 spawns" while a process ran.
#
# `fns` is a parameter for `tm_program_literals()`'s reason, a few lines below:
# a guard whose only falsifier is deleting it re-certifies nothing. Feeding it a
# mutant body that spawns outside the wrapper is what shows it can say FALSE.
tm_spawn_interception_complete <- function(graph = tm_symbol_graph(),
                                           fns = NULL) {
  if (is.null(fns)) {
    ns <- asNamespace("tidymedia")
    sites <- tm_spawn_sites(graph)
    fns <- stats::setNames(lapply(sites, get, envir = ns), sites)
  }
  all(vapply(fns, function(f) {
    ok <- TRUE
    walk <- function(e, guarded) {
      if (!is.call(e)) return(invisible(NULL))
      head <- e[[1]]
      if (is.name(head) && as.character(head) %in% tm_spawn_primitives &&
          !guarded) {
        ok <<- FALSE
      }
      is_guard <- is.name(head) && identical(as.character(head),
                                             "guard_timeout")
      for (i in seq_along(e)) {
        if (rlang::is_missing(e[[i]]) || is.null(e[[i]])) next
        # Every ARGUMENT of guard_timeout() is guarded, its head is not: R
        # binds arguments lazily, and the mock that stands in for the wrapper
        # returns without forcing any of them.
        walk(e[[i]], guarded || (is_guard && i > 1L))
      }
    }
    walk(body(f), FALSE)
    ok
  }, logical(1)))
}


# tm_scrub_paths(): make one digest comparable across sessions and checkouts.
#
# Two volatile paths reach a compiled command. The fixture directory is the
# caller's, passed in. The other is the session temp dir, where
# `concatenate_videos()` writes the `-f concat` list file under a name randomized
# per CALL -- so two runs of the same code in one session already differ there,
# and comparing that string would compare R's RNG rather than the package.
# tm_dir_pattern(): `dir` as a regex matching it with EITHER path separator at
# every position.
#
# On Windows one directory reaches the digest with a MIX of them -- R's
# tempdir() hands back forward slashes and the verbs concatenate Windows'
# backslashes onto it -- and str() prints each backslash doubled. A `fixed =
# TRUE` substitution of `dir` therefore matched nothing there, so every member's
# digest carried the runner's absolute path and no comparison against the
# recorded table could pass. It went unseen while an earlier failure in the same
# file stopped testthat before this comparison ran (M094 T13).
#
# Only the SEPARATORS are made flexible. The digest's own `\"` escapes are left
# alone, so a reading taken where the separator is already "/" is byte-identical
# to what it was -- which is why the fixture recorded on macOS stays valid.
tm_dir_pattern <- function(dir) {
  parts <- strsplit(gsub("\\\\", "/", dir), "/+")[[1]]
  escaped <- gsub("([][{}()+*^$|?.\\\\])", "\\\\\\1", parts)
  paste(escaped, collapse = "[/\\\\]+")
}

tm_scrub_paths <- function(x, dir) {
  x <- gsub(tm_dir_pattern(dir), "<dir>", x)
  x <- gsub(tm_dir_pattern(normalizePath(tempdir(), winslash = "/",
                                         mustWork = FALSE)), "<tmp>", x)
  x <- gsub(tm_dir_pattern(tempdir()), "<tmp>", x)
  x <- gsub("ffm-concat[0-9a-f]+(\\.txt)?", "<concat-list>", x)
  # The separator JOINING the two placeholders survives both substitutions, and
  # it is the platform's: `concatenate_videos()` builds this path with
  # file.path(), so Windows writes `<tmp>\<concat-list>` where POSIX writes
  # `<tmp>/<concat-list>`. That is the runner showing through a digest that is
  # meant to compare the package, so it is normalized like the separators inside
  # each path (M094 T13).
  gsub("<tmp>[/\\\\]+<concat-list>", "<tmp>/<concat-list>", x)
}

# tm_spawn_trace(): run one domain member under `limit` with every spawn
# intercepted, and report what came back and how many spawns it took.
#
# `guard_timeout()` is the single mock, which `tm_spawn_interception_complete()`
# above proves is enough. It returns a fixed empty-output vector rather than
# anything a real program would print: the question here is whether the two refs
# do the SAME thing, not what FFmpeg says, and a canned answer keeps the reading
# off the runner's PATH and its media binaries (M070's reason for injecting at
# the wrappers rather than at a real hang).
#
# The return value is reduced to a printed digest with the fixture directory
# scrubbed, so two refs measured in two checkouts compare as values rather than
# as two temp paths.
tm_spawn_trace <- function(name, args, limit, dir) {
  # The digest is printed output, so it has to be printed under the same
  # conventions everywhere. Without this the recorded table and the live reading
  # disagree on tibble's dimension glyph alone -- testthat runs with
  # `cli.unicode = FALSE` and a plain Rscript does not, so the same value prints
  # `1 x 5` in one and `1 <times> 5` in the other.
  testthat::local_reproducible_output()
  spawns <- 0L
  warns <- character()
  err <- NULL
  value <- NULL
  opts <- if (is.null(limit)) {
    list(tidymedia.timeout = NULL)
  } else {
    list(tidymedia.timeout = limit)
  }
  withr::with_options(opts, {
    testthat::local_mocked_bindings(
      guard_timeout = function(program, limit, expr, ...) {
        spawns <<- spawns + 1L
        character(0)
      },
      # The three locators are pinned too, so the reading is the package's and
      # not the runner's PATH: without this a machine with no media binaries
      # measures `Could not locate FFmpeg` for half the domain and the recorded
      # table stops being comparable anywhere else. CI's macOS and Windows
      # runners install no media binaries at all (M070's reason, same trap).
      find_ffmpeg = function() "/nonexistent/ffmpeg",
      find_ffprobe = function() "/nonexistent/ffprobe",
      find_mediainfo = function() "/nonexistent/mediainfo",
      .package = "tidymedia"
    )
    withCallingHandlers(
      tryCatch(
        value <- do.call(name, args, envir = asNamespace("tidymedia")),
        error = function(e) err <<- e
      ),
      warning = function(w) {
        warns <<- c(warns, cli::ansi_strip(conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    )
  })
  digest <- paste(
    utils::capture.output(utils::str(
      value, max.level = 2L, give.attr = FALSE,
      # str()'s 128-character default truncates a compiled FFmpeg command well
      # before its output path, which would leave the digest unable to tell two
      # different commands apart -- the dimension this comparison exists to
      # watch.
      nchar.max = 4000L, vec.len = 20L
    )),
    collapse = " | "
  )
  list(
    spawns = spawns,
    value = tm_scrub_paths(digest, dir),
    error = if (is.null(err)) NA_character_ else class(err)[[1]],
    warnings = length(warns)
  )
}

# tm_blame_condition(): the whole condition one domain member raised under
# `limit`, message and class vector both.
#
# Separate from `tm_blame_head()` because AC4 asks two things that
# conditionCall() cannot answer: whether every member says the SAME sentence,
# and whether the six FFprobe readers still arrive wrapped in purrr's indexed
# error. The message is ansi-stripped, and the caller pins `cli.width` -- cli
# wraps to the console, so an unpinned width compares two terminals.
tm_blame_condition <- function(name, args, limit) {
  # Same reason as tm_spawn_trace(): cli wraps and decorates to the console, and
  # this compares message text across 53 members and one reference.
  testthat::local_reproducible_output()
  withr::with_options(list(tidymedia.timeout = limit), {
    cnd <- tryCatch(
      do.call(name, args, envir = asNamespace("tidymedia")),
      error = function(e) e
    )
    if (!inherits(cnd, "error")) return(NULL)
    list(
      message = cli::ansi_strip(conditionMessage(cnd)),
      classes = class(cnd)
    )
  })
}

# tm_resolve_timeout_message(): the sentence the ONE checker site writes for a
# given invalid value, read from that site rather than from any verb.
#
# This is AC4's referent. Comparing the 53 members only to each other would go
# green on 53 copies of a second, drifted wording; comparing them to what
# `resolve_timeout()` itself produces is what pins them to the single
# `rlang::check_number_whole()` call in R/timeout.R.
tm_resolve_timeout_message <- function(limit) {
  testthat::local_reproducible_output()
  withr::with_options(list(tidymedia.timeout = limit), {
    cli::ansi_strip(
      tryCatch(resolve_timeout(), error = function(e) conditionMessage(e))
    )
  })
}

# tm_timeout_valid_baseline(): the recorded pre-change return values and spawn
# counts. Regenerate with data-raw/timeout-valid-baseline.R.
#
# The provenance the generator attaches is CHECKED here, not merely carried.
# Recording it and never reading it makes the reproducibility rule an attribute
# nobody consults: a blob regenerated from the wrong ref, or by hand, would keep
# comparing green against the wrong reading (M094 review F10). The ref is pinned
# to the sha this milestone branched from, so a re-record against a later master
# has to be a deliberate edit here.
tm_timeout_valid_baseline_ref <- "ae5ff1c"

tm_timeout_valid_baseline <- function() {
  table <- readRDS(testthat::test_path("fixtures", "timeout-valid-baseline.rds"))
  if (!tm_provenance_ok(table)) {
    testthat::fail(
      "the recorded baseline's provenance is missing or names another source"
    )
  }
  # The blob was recorded at ae5ff1c, where the availability predicate was
  # exported as `has_nvenc()`; the rename to `has_hardware_encoder()` changed
  # the export's name and nothing it spawns or returns, so the recorded cell is
  # read under the current name rather than re-recorded from a ref that cannot
  # name it. The blob itself stays as its generator wrote it. A re-recorded
  # blob carries the new key, at which point this remap is dead and must go.
  stopifnot("has_nvenc" %in% names(table))
  names(table)[names(table) == "has_nvenc"] <- "has_hardware_encoder"
  # The same treatment for a cell whose export is GONE rather than renamed.
  # `mediainfo_summary()` was a second exported name for `mediainfo_template()`
  # -- one object, two `export()` lines -- and M112 removed it. Its recorded
  # cell is byte-identical to `mediainfo_template`'s, which is still in the
  # table, so dropping it removes a duplicate reading rather than a measurement.
  # Asserted, not assumed: an unequal pair means the blob measured two
  # different things and this drop would be hiding one of them.
  stopifnot(identical(table[["mediainfo_summary"]],
                      table[["mediainfo_template"]]))
  table[["mediainfo_summary"]] <- NULL
  table
}

# tm_provenance_ok(): the provenance predicate, separate so it can be shown to
# say NO. A checker whose only falsifier is deleting it certifies nothing.
tm_provenance_ok <- function(table, ref = tm_timeout_valid_baseline_ref) {
  prov <- attr(table, "provenance")
  is.list(prov) &&
    all(c("source", "generator", "seed", "recorded") %in% names(prov)) &&
    grepl(ref, prov$source, fixed = TRUE) &&
    identical(prov$generator, "data-raw/timeout-valid-baseline.R")
}

# tm_baseline_shape_ok(): does the recorded BLOB still describe what
# tm_spawn_trace() measures?
#
# The provenance predicate above reads the attribute, so a blob whose contents
# no longer match the helper that produced them passes it (M094 review G5). The
# coupling is real: `value` is `str()` output, and an edit to tm_spawn_trace()'s
# print conventions or its returned fields would leave 106 cells mismatching
# with nothing to say WHY. This compares one live trace's shape -- its field
# names and their types -- against every recorded cell, so that edit fails once
# and names itself instead of arriving as a wall of diffs.
#
# Shape only, deliberately: the VALUES are what the AC5 comparison is for, and a
# predicate that recomputed them would be that comparison written twice.
tm_baseline_shape_ok <- function(table, live) {
  shape <- function(x) {
    is.list(x) &&
      identical(names(x), names(live)) &&
      vapply(names(live), function(f) identical(class(x[[f]]), class(live[[f]])),
             logical(1)) |> all() &&
      is.character(x$value) && length(x$value) == 1L && nzchar(x$value)
  }
  is.list(table) && length(table) > 0 &&
    all(vapply(table, function(cell) {
      is.list(cell) && identical(names(cell), c("unset", "valid")) &&
        shape(cell$unset) && shape(cell$valid)
    }, logical(1)))
}

# tm_timeout_variant_specs(): the argument cells the one-cell-per-member table
# above cannot carry, and where M094's review found the refusal still missing.
#
# `tm_timeout_call_specs()` holds exactly one valid argument set per member, so
# an argument that steers the verb down a DIFFERENT path -- a GPU encode, the
# frame half of extract_frame()'s "provide exactly one of" pair, a two-pass
# normalization -- is invisible to every sweep built on it. Each of those three
# hid a member that went on blaming a function the caller never typed (M094
# review F2, F4, F3).
#
# The axes are computed from `formals()`, not listed, for the reason the domain
# itself is computed (M70): a verb that gains `hardware` joins this table on its
# own. `extract_frame` is named because its pair is a documented argument
# contract, not a formal a sweep can see.
#
# Every cell here is exercised under an INVALID limit only, where AC5 pins the
# spawn count at 0 -- so `hardware = "nvenc"` never asks a real FFmpeg anything.
tm_timeout_variant_specs <- function(dir) {
  specs <- tm_timeout_call_specs(dir)
  out <- list()
  for (nm in tm_timeout_domain()) {
    fmls <- names(formals(get(nm, envir = asNamespace("tidymedia"))))
    if ("hardware" %in% fmls) {
      args <- specs[[nm]]
      args$hardware <- "nvenc"
      # A re-encoding codec is named where the verb has one, because a
      # `video_codec = "copy"` default plus `hardware = "nvenc"` is a
      # contradiction the verb refuses on its own (D036) -- a correct refusal
      # that would leave this cell testing that instead of the limit.
      if ("video_codec" %in% fmls) args$video_codec <- "libx264"
      out[[paste0(nm, " [hardware = nvenc]")]] <- list(name = nm, args = args)
    }
    if ("two_pass" %in% fmls) {
      args <- specs[[nm]]
      args$two_pass <- TRUE
      out[[paste0(nm, " [two_pass = TRUE]")]] <- list(name = nm, args = args)
    }
  }
  frame_args <- specs$extract_frame
  frame_args$timestamp <- NULL
  frame_args$frame <- 1
  out[["extract_frame [frame = ]"]] <-
    list(name = "extract_frame", args = frame_args)
  out
}

# tm_refusal_head(): `tm_blame_head()` with the condition's IDENTITY checked
# too.
#
# The head alone cannot tell M094's refusal from any other error raised in the
# same frame, so a member that aborted on something else entirely would read as
# a pass (M094 review F9). This compares the message to what the one checker
# site writes for that value before reporting the head, and names what it saw
# otherwise -- so a wrong-condition cell fails with the wrong condition in the
# message rather than going quietly green.
tm_refusal_head <- function(name, args, limit) {
  reference <- tm_resolve_timeout_message(limit)
  testthat::local_reproducible_output()
  withr::with_options(list(tidymedia.timeout = limit), {
    cnd <- tryCatch(
      do.call(name, args, envir = asNamespace("tidymedia")),
      error = function(e) e
    )
    if (!inherits(cnd, "error")) return("<none>")
    msg <- cli::ansi_strip(conditionMessage(cnd))
    if (!identical(msg, reference)) {
      return(paste0("<other: ", blamed_verb(cnd), ": ", msg, ">"))
    }
    blamed_verb(cnd)
  })
}

# M094 review round 2, widened at M096: the cells that carry a WRONG argument --

# tm_timeout_corrupt_specs(): every (member, formal, wrong form) cell -- each
# member of the domain with ONE of its own formals replaced by one of the five
# wrong forms `tm_nvenc_wrong_forms()` names.
#
# Every cell in `tm_timeout_call_specs()` and `tm_timeout_variant_specs()`
# carries VALID arguments, so no leg built on them can see the refusal DISPLACE
# an error the caller's own call earned -- which is how one class of that defect
# survived round 1 at four verbs and recurred at nine more (review F1, G1).
#
# M094 corrupted `args[[1]]` with `123` alone. That single form on a single
# formal is what let two front doors through three review rounds: `123` is a
# number where every member's FIRST argument wants a path, a job table or a
# pipeline, so the narrow instrument saw only the guards that sit at the top of
# each verb. An argument whose only guard is a token check, a missingness
# check, a length check or a shape check is invisible to it, and an argument
# that is not the first is invisible to it entirely. The cross-product is the
# widening: every formal, and the five forms M095 crossed the nvenc probe with,
# which between them span type, token shape, missingness, length and container.
#
# `...` is excluded because it is not an argument a caller can hand a wrong
# value to by name. `run = FALSE` and `parallel = FALSE` go into the base cell
# so a cell that IS refused spawns nothing on the way there; the sweep below
# intercepts the spawns a truthy wrong value would otherwise reach.
#
# Every member is present, including the three whose valid cell in
# `tm_timeout_call_specs()` holds no user-chosen value -- `ffmpeg_codecs`,
# `ffmpeg_encoders` and `has_hardware_encoder` all have formals to corrupt even
# though their cell is empty (or, for `has_hardware_encoder`, holds only the
# backend its required `hardware` argument has no default for), and
# `ffmpeg_codecs`'s is the argument M096 exists to guard.
tm_timeout_corrupt_specs <- function(dir) {
  specs <- tm_timeout_call_specs(dir)
  forms <- tm_nvenc_wrong_forms()
  ns <- asNamespace("tidymedia")
  out <- list()
  for (nm in tm_timeout_domain()) {
    fmls <- names(formals(get(nm, envir = ns)))
    base <- specs[[nm]]
    if ("parallel" %in% fmls) base$parallel <- FALSE
    if ("run" %in% fmls) base$run <- FALSE
    for (arg in setdiff(fmls, "...")) {
      for (form in names(forms)) {
        args <- base
        args[[arg]] <- forms[[form]]
        out[[paste0(nm, " [", arg, " = ", form, "]")]] <-
          list(name = nm, arg = arg, form = form, args = args)
      }
    }
  }
  out
}

# tm_corrupt_limit_sweep(): drive every cell with no limit set, and again under
# each invalid `tidymedia.timeout` in `tm_timeout_bad_forms()`.
#
# The no-limit reading is the referent, measured rather than recorded: what a
# call reports with no limit set at all is what it must still report under one
# base R cannot use. That keeps the leg indifferent to each verb's own wording
# and to any later change in it.
#
# A cell is KEPT when that referent was refused by the member ITSELF -- the
# blamed frame is the member's own name -- and dropped otherwise, whether the
# refusal came from a frame below the member or never came. Dropping is a
# measurement here and never a list: `tm_corrupt_dropped_master()` records what
# the measurement returned so a reader can see it, but the sweep consults no
# list to decide.
#
# Spawns are intercepted at the two wrappers `tm_force_timeout()` uses, for
# `tm_nvenc_sweep()`'s reason: without it a cell whose wrong value happens to be
# TRUTHY really executes FFmpeg on an empty placeholder file, so the frame that
# refused it is FFmpeg's exit status on this machine and a missing binary on a
# runner with none. `character(0)` is a clean exit with no output, so such a
# cell reads as "not refused", which is what it is.
tm_corrupt_limit_sweep <- function(cells) {
  # Before the mocked bindings below, never after. `tm_timeout_corrupt_specs()`
  # computes its own domain by reading each function's `body()` off the
  # namespace and asking which of them reach a spawn primitive; once
  # `run_program()` and `guard_timeout()` are mocked, no body names one, the
  # domain halves, and the census silently runs over half of what it claims.
  # Measured: 1530 cells forced here, 705 forced inside the mocks. The working
  # directory is NOT what matters -- forcing from another directory with no
  # mocks in force still yields 1530 (corrected M96 review F4).
  force(cells)
  testthat::local_reproducible_output()
  # The sweep runs from a scratch directory, and this is load-bearing rather
  # than tidy. A cell that corrupts a path argument with the token form hands a
  # verb the RELATIVE path "bad fmt!", and `sample_frames()`'s `outdir` creates
  # what it is given -- so a sweep run from the package root leaves a directory
  # called "bad fmt!" behind it, and from then on `file.exists("bad fmt!")` is
  # TRUE and every later path cell reads that token as a perfectly good path.
  # Measured: a sweep run twice from the working tree turned eight of M095's
  # kept cells into dropped ones the second time, and reddened
  # test-nvenc-probe-blame.R, which measures the same paths.
  scratch <- withr::local_tempdir()
  withr::local_dir(scratch)
  withr::local_options(tidymedia.hardware_encoders = NULL)
  testthat::local_mocked_bindings(
    run_program = function(location, args, program = "the program", ...) {
      character(0)
    },
    guard_timeout = function(program, limit, expr, ...) character(0),
    # The encoder pool and the three locators are pinned for the same reason,
    # one layer up: without them a cell that reaches the nvenc probe or a
    # binary lookup measures the runner's FFmpeg build and its PATH, and the
    # census below stops being the same table on two machines -- and the
    # encoder pool is remembered for the session, so it need not even be the
    # same table twice on one. `character()` is the build with no nvenc
    # encoders -- the
    # pool that makes an availability abort fire, so a cell reading as kept
    # under it is kept everywhere.
    cached_encoder_names = function() character(),
    find_ffmpeg = function() "/nonexistent/ffmpeg",
    find_ffprobe = function() "/nonexistent/ffprobe",
    find_mediainfo = function() "/nonexistent/mediainfo",
    .package = "tidymedia"
  )
  forms <- tm_timeout_bad_forms()
  # Warnings are muffled, not measured. What this sweep reads is the condition
  # that ABORTS a call; a cell whose mocked spawn returns `character(0)` makes
  # the six `probe_*()` readers warn that they could not probe the file, which
  # is an artifact of the interception and not of any behavior under test.
  muffled <- function(expr) {
    withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
  }
  rows <- lapply(names(cells), function(key) {
    spec <- cells[[key]]
    ref <- muffled(withr::with_options(
      list(tidymedia.timeout = NULL),
      tm_nvenc_condition(spec$name, spec$args)
    ))
    head <- sub(" \\|\\| .*$", "", ref)
    kept <- identical(head, spec$name)
    bad <- character()
    if (kept) {
      for (fm in names(forms)) {
        got <- muffled(withr::with_options(
          list(tidymedia.timeout = forms[[fm]]),
          tm_nvenc_condition(spec$name, spec$args)
        ))
        if (!identical(got, ref)) bad <- c(bad, fm)
      }
    }
    tibble::tibble(
      cell = key, member = spec$name, arg = spec$arg, form = spec$form,
      none = ref, kept = kept, refused_by = head,
      mismatch = paste(bad, collapse = ",")
    )
  })
  do.call(rbind, rows)
}

# tm_corrupt_master_ref: the commit the census below was measured at -- the tip
# of the default branch when this branch was cut, so a reader can regenerate it.
tm_corrupt_master_ref <- "4063faa"

# tm_corrupt_dropped_master(): every (member, argument) the widened sweep DROPS,
# named with the frame that refused it -- measured 2026-08-31 at
# `tm_corrupt_master_ref`, before either M096 guard landed.
#
# Recorded for `tm_nvenc_dropped_master()`'s reason: AC1 drops a cell BY
# MEASUREMENT and never by a list, and a measurement nobody can see is
# indistinguishable from a list nobody wrote down. It is compared to a live
# sweep rather than consulted by one.
#
# Eight frames appear. `<none>` is a cell no front door refuses at all, and most
# often means the wrong form is not wrong for that formal -- `123` is a
# perfectly good `fps`, `width` or `tolerance`. `ffm_finish`, `ffm_batch` and
# `if` are the gate booleans `run` and `parallel`, refused by the runner rather
# than the verb; `purrr::pmap` is the per-row fan-out; `hardware_encoder` and `<-`
# are two single-member classes. Those five classes are named in this
# milestone's Scope Out and carried by a ROADMAP row; they are here so they stay
# visible. `:` is the sixth and the one that does not stay: `ffmpeg_codecs()`
# reaching its own output parsing with an unchecked `sort_by_type`, which is
# what AC3 closes.
#
# What that `:` entry does and does NOT prove (M96 review F8). Under this
# sweep's mocks `ffmpeg("-codecs")` returns `character(0)`, so the parse below
# it aborts at `1:integer(0)` for EVERY value -- measured at
# `tm_corrupt_master_ref`: `TRUE`, `FALSE` and `123` all give "argument of
# length 0". So the drop says only that no front door refused the cell; it is
# form-independent and is NOT evidence that `sort_by_type` was unguarded. AC3's
# defect is shown by the spawn-count and message-parity test in
# test-unguarded-argument-front-doors.R, not by this entry. What the entry is
# good for is the two-way difference: the cell is refused by `ffmpeg_codecs`
# itself at HEAD and so leaves this list, which a guard sited BELOW
# `resolve_timeout()` would not have achieved.
#
# A pair whose two entries name different frames (`segment_video/outfiles`,
# `anonymize_video_batch/color`) is not an inconsistency: different wrong forms
# of one argument are caught at different depths.
tm_corrupt_dropped_master <- function() {
  tm_sort_c(c(
    "anonymize_video/audio_stream -> <none>",
    "anonymize_video/color -> <none>",
    "anonymize_video/outfile -> <none>",
    "anonymize_video/run -> ffm_finish",
    "anonymize_video_batch/audio_stream -> <none>",
    "anonymize_video_batch/color -> <none>",
    "anonymize_video_batch/color -> purrr::pmap",
    "anonymize_video_batch/fallback -> purrr::pmap",
    "anonymize_video_batch/parallel -> ffm_batch",
    "anonymize_video_batch/pixel_format -> purrr::pmap",
    "anonymize_video_batch/run -> ffm_batch",
    "compare_videos/outfile -> <none>",
    "compare_videos/run -> ffm_finish",
    "compare_videos_batch/fallback -> purrr::pmap",
    "compare_videos_batch/parallel -> ffm_batch",
    "compare_videos_batch/run -> ffm_batch",
    "concatenate_videos/outfile -> <none>",
    "concatenate_videos/run -> ffm_finish",
    "concatenate_videos_batch/parallel -> ffm_batch",
    "concatenate_videos_batch/run -> ffm_batch",
    "convert_audio/audio_stream -> <none>",
    "convert_audio/outfile -> <none>",
    "convert_audio/run -> ffm_finish",
    "convert_audio_batch/audio_stream -> <none>",
    "convert_audio_batch/parallel -> ffm_batch",
    "convert_audio_batch/run -> ffm_batch",
    "crop_video/audio_stream -> <none>",
    "crop_video/height -> <none>",
    "crop_video/outfile -> <none>",
    "crop_video/run -> ffm_finish",
    "crop_video/width -> <none>",
    "crop_video/x -> <none>",
    "crop_video/y -> <none>",
    "crop_video_batch/audio_stream -> <none>",
    "crop_video_batch/fallback -> purrr::pmap",
    "crop_video_batch/height -> <none>",
    "crop_video_batch/parallel -> ffm_batch",
    "crop_video_batch/run -> ffm_batch",
    "crop_video_batch/width -> <none>",
    "crop_video_batch/x -> <none>",
    "crop_video_batch/y -> <none>",
    "extract_audio/audio_stream -> <none>",
    "extract_audio/outfile -> <none>",
    "extract_audio/run -> ffm_finish",
    "extract_audio_batch/audio_stream -> <none>",
    "extract_audio_batch/parallel -> ffm_batch",
    "extract_audio_batch/run -> ffm_batch",
    "extract_frame/outfile -> <none>",
    "extract_frame/run -> ffm_finish",
    "extract_frame/timestamp -> <none>",
    "extract_frame_batch/format -> <none>",
    "extract_frame_batch/parallel -> ffm_batch",
    "extract_frame_batch/run -> ffm_batch",
    "ffmpeg/command -> <none>",
    "ffmpeg_codecs/sort_by_type -> :",
    "ffprobe/command -> <none>",
    "format_for_web/audio_stream -> <none>",
    "format_for_web/outfile -> <none>",
    "format_for_web/run -> ffm_finish",
    "format_for_web_batch/audio_stream -> <none>",
    "format_for_web_batch/fallback -> purrr::pmap",
    "format_for_web_batch/parallel -> ffm_batch",
    "format_for_web_batch/run -> ffm_batch",
    "get_duration/file -> <none>",
    "get_frame_rate/file -> <none>",
    "get_height/file -> <none>",
    "get_sample_rate/file -> <none>",
    "get_width/file -> <none>",
    "has_hardware_encoder/codec -> hardware_encoder",
    "mediainfo/command -> <none>",
    "mediainfo_parameter/file -> <none>",
    "mediainfo_parameter/parameter -> <none>",
    "mediainfo_parameter/section -> <none>",
    "mediainfo_query/file -> <none>",
    "mediainfo_query/names -> <none>",
    "mediainfo_query/parameters -> <none>",
    "mediainfo_query/section -> <none>",
    "mediainfo_summary/file -> <none>",
    "mediainfo_template/file -> <none>",
    "normalize_audio/audio_stream -> <none>",
    "normalize_audio/channels -> <none>",
    "normalize_audio/outfile -> <none>",
    "normalize_audio/run -> ffm_finish",
    "normalize_audio/sample_rate -> <none>",
    "normalize_audio_batch/audio_stream -> <none>",
    "normalize_audio_batch/channels -> <none>",
    "normalize_audio_batch/channels -> purrr::pmap",
    "normalize_audio_batch/parallel -> ffm_batch",
    "normalize_audio_batch/run -> ffm_batch",
    "normalize_audio_batch/sample_rate -> <none>",
    "normalize_audio_batch/sample_rate -> purrr::pmap",
    "picture_in_picture/margin -> <none>",
    "picture_in_picture/outfile -> <none>",
    "picture_in_picture/run -> ffm_finish",
    "picture_in_picture_batch/fallback -> purrr::pmap",
    "picture_in_picture_batch/margin -> <none>",
    "picture_in_picture_batch/parallel -> ffm_batch",
    "picture_in_picture_batch/run -> ffm_batch",
    "probe_all/infile -> <none>",
    "probe_audio/infile -> <none>",
    "probe_container/infile -> <none>",
    "probe_streams/infile -> <none>",
    "probe_video/infile -> <none>",
    "sample_frames/fps -> <none>",
    "sample_frames/outdir -> <none>",
    "sample_frames/prefix -> <none>",
    "sample_frames/run -> ffm_finish",
    "sample_frames_batch/fps -> <none>",
    "sample_frames_batch/outdir -> <none>",
    "sample_frames_batch/parallel -> ffm_batch",
    "sample_frames_batch/run -> ffm_batch",
    "segment_video/audio_stream -> <none>",
    "segment_video/end -> <none>",
    "segment_video/fallback -> purrr::pmap",
    "segment_video/infile -> <none>",
    "segment_video/outfiles -> <none>",
    "segment_video/outfiles -> purrr::pmap",
    "segment_video/parallel -> ffm_batch",
    "segment_video/run -> ffm_batch",
    "segment_video/start -> <none>",
    "segment_video_batch/audio_stream -> <none>",
    "segment_video_batch/fallback -> purrr::pmap",
    "segment_video_batch/parallel -> ffm_batch",
    "segment_video_batch/run -> ffm_batch",
    "separate_audio_video/audio_stream -> <none>",
    "separate_audio_video/audiofile -> <none>",
    "separate_audio_video/infile -> <none>",
    "separate_audio_video/run -> <none>",
    "separate_audio_video/run -> if",
    "separate_audio_video/videofile -> <none>",
    "separate_audio_video_batch/audio_stream -> <none>",
    "separate_audio_video_batch/parallel -> ffm_batch",
    "separate_audio_video_batch/run -> ffm_batch",
    "standardize_video/audio_stream -> <none>",
    "standardize_video/fps -> <none>",
    "standardize_video/height -> <none>",
    "standardize_video/infile -> <none>",
    "standardize_video/outfile -> <none>",
    "standardize_video/run -> ffm_finish",
    "standardize_video/width -> <none>",
    "standardize_video_batch/audio_stream -> <none>",
    "standardize_video_batch/fallback -> purrr::pmap",
    "standardize_video_batch/fps -> <none>",
    "standardize_video_batch/height -> <none>",
    "standardize_video_batch/parallel -> ffm_batch",
    "standardize_video_batch/run -> ffm_batch",
    "standardize_video_batch/width -> <none>",
    "strip_metadata/infile -> <none>",
    "strip_metadata/outfile -> <none>",
    "strip_metadata/run -> ffm_finish",
    "strip_metadata_batch/parallel -> ffm_batch",
    "strip_metadata_batch/run -> ffm_batch",
    "verify_media/audio_codec -> <-",
    "verify_media/audio_codec -> <none>",
    "verify_media/duration -> <-",
    "verify_media/duration -> <none>",
    "verify_media/file -> <none>",
    "verify_media/height -> <-",
    "verify_media/height -> <none>",
    "verify_media/sample_rate -> <-",
    "verify_media/sample_rate -> <none>",
    "verify_media/tolerance -> <none>",
    "verify_media/video_codec -> <-",
    "verify_media/video_codec -> <none>",
    "verify_media/width -> <-",
    "verify_media/width -> <none>"
  ))
}

# tm_masked_condition(): the whole condition one call raised, as one comparable
# string -- the head that was blamed and the sentence it carried.
#
# Both halves matter here. A member whose argument error survives the limit but
# starts naming a different frame has moved blame the milestone did not intend
# to move, and a member that reports the limit's sentence instead has masked the
# argument error outright; comparing head and message together catches each.
# `"<none>"` is a real answer and a failing one for this leg: the call was given
# an argument every front door refuses.
tm_masked_condition <- function(name, args, limit) {
  testthat::local_reproducible_output()
  withr::with_options(list(tidymedia.timeout = limit), {
    cnd <- tryCatch(
      do.call(name, args, envir = asNamespace("tidymedia")),
      error = function(e) e
    )
    if (!inherits(cnd, "error")) return("<none>")
    paste0(blamed_verb(cnd), " || ", cli::ansi_strip(conditionMessage(cnd)))
  })
}

# M095: a wrong argument, crossed with the nvenc availability probe -----------

# tm_nvenc_wrong_forms(): five wrong values spanning the axes an argument is
# free in -- type, token shape, missingness, length, and container.
#
# Five rather than one because `tm_timeout_corrupt_specs()` above corrupts with
# `123` alone, and a number is refused by a type check at the top of every front
# door. An argument whose only guard is a token check (`pixel_format`), a
# missingness check (`NA`), a length check, or a shape check is invisible to
# that single form, and each of those guards sits at a different depth -- which
# is the depth this milestone is measuring against the probe.
tm_nvenc_wrong_forms <- function() {
  list(
    number = 123,
    token = "bad fmt!",
    missing = NA,
    length_two = c(1, 2),
    list = list(1)
  )
}

# tm_nvenc_unmappable_codec(): a codec token that is WELL FORMED and names no
# codec family -- deliberately NOT a member of the table above (M107).
#
# The seam test wants a `video_codec` that survives `check_token()` and is then
# refused by `codec_family()`, because all five forms above are refused by a
# type or shape check and none of them gets that far. It is a separate helper
# rather than a sixth entry because the table above is a table of values that
# are WRONG for any argument, spanning type, token shape, missingness, length
# and container, and "notacodec" is none of those: it is a legal string
# everywhere except as a `video_codec` under a hardware backend. Crossing it
# over every formal of every member was measured 2026-09-04 -- 1535 cells to
# 1842, and 26 new census entries recording that nothing refuses it -- which is
# a measurement of its legality, not of a guard.
tm_nvenc_unmappable_codec <- function() "notacodec"

# tm_nvenc_wrong_arg_cells(): every (member, other formal, wrong form) cell.
#
# The members are computed -- `tm_timeout_domain()` filtered to the ones whose
# `formals()` carry `hardware` -- for the reason the domain itself is computed
# (M70): a verb that gains `hardware` joins this sweep on its own. The ARGUMENTS
# are computed the same way, off each member's own `formals()`, so an argument
# added to a verb joins too.
#
# `hardware` is excluded because it is the axis being crossed, and `...` because
# it is not an argument a caller can hand a wrong value to by name. `video_codec`
# is set to a re-encoding token in the base cell for `tm_timeout_variant_specs()`'s
# reason: the `"copy"` default plus `hardware = "nvenc"` is a contradiction the
# verb refuses on its own (D036), which would leave every cell of that member
# measuring the contradiction instead of its own argument. `run = FALSE` and
# `parallel = FALSE` keep a cell that IS refused from spawning anything.
tm_nvenc_wrong_arg_cells <- function(dir) {
  specs <- tm_timeout_call_specs(dir)
  forms <- tm_nvenc_wrong_forms()
  ns <- asNamespace("tidymedia")
  out <- list()
  # The two capability helpers are out of this domain for the reason
  # nvenc_hardware_exports() states: they grew a `hardware` argument too, but
  # this sweep crosses each member's OTHER formals with wrong forms and asks
  # which frame refused them, and the helpers have no pipeline to blame.
  for (nm in setdiff(tm_timeout_domain(), nvenc_hardware_helpers())) {
    fmls <- names(formals(get(nm, envir = ns)))
    if (!"hardware" %in% fmls) next
    base <- specs[[nm]]
    if ("video_codec" %in% fmls) base$video_codec <- "libx264"
    if ("parallel" %in% fmls) base$parallel <- FALSE
    if ("run" %in% fmls) base$run <- FALSE
    for (arg in setdiff(fmls, c("hardware", "..."))) {
      for (form in names(forms)) {
        args <- base
        args[[arg]] <- forms[[form]]
        out[[paste0(nm, " [", arg, " = ", form, "]")]] <-
          list(name = nm, arg = arg, form = form, args = args)
      }
    }
  }
  out
}

# tm_nvenc_condition(): one call's condition as a single comparable string --
# the frame that was blamed and the sentence it carried.
#
# Both halves, for `tm_masked_condition()`'s reason: a cell that keeps naming
# the member but starts saying something else has still changed what the caller
# reads. `"<none>"` is a real answer -- a cell the member does not refuse at all.
tm_nvenc_condition <- function(name, args) {
  cnd <- tryCatch(
    do.call(name, args, envir = asNamespace("tidymedia")),
    error = function(e) e
  )
  if (!inherits(cnd, "error")) return("<none>")
  paste0(blamed_verb(cnd), " || ", cli::ansi_strip(conditionMessage(cnd)))
}

# tm_nvenc_sweep(): drive every cell under `hardware = "none"` and again under
# `hardware = "nvenc"`, with the encoder pool fixed to `encoders`.
#
# The mock is installed ONCE around the whole loop rather than per cell, and
# `tidymedia.hardware_encoders` is unset inside it: `nvenc_available()` reads that
# option seam FIRST and only falls through to `cached_encoder_names()` when it
# is NULL, so an option left set by another file would answer every cell and
# the mock -- the thing that makes `encoders = character()` mean "this build has
# no nvenc" -- would never be consulted.
#
# The `hardware = "none"` reference is re-measured inside each mock state rather
# than once outside, so the pair being compared differs in `hardware` and in
# nothing else.
#
# `limit` is AC3's axis: a `tidymedia.timeout` value the option refuses, set for
# the `hardware = "nvenc"` arm ONLY. The reference arm stays unlimited on
# purpose -- the question is whether a session limit changes what a caller is
# told about their argument, so the thing it is compared against must be the
# answer with no limit at all. `NULL` means no limit is set on either arm.
#
# Returns one row per cell: the reference condition, the nvenc condition,
# whether the reference was refused BY THE MEMBER ITSELF (`kept`), and the frame
# that refused it when it was not.
tm_nvenc_sweep <- function(cells, encoders, limit = NULL) {
  testthat::local_reproducible_output()
  withr::local_options(tidymedia.hardware_encoders = NULL)
  # Every spawn is intercepted too, at the two wrappers `tm_force_timeout()` uses
  # -- `run_program()` and `guard_timeout()` stand in front of every
  # `system()`/`system2()` call the package makes. Without this, a cell whose
  # wrong value happens to be TRUTHY (`run = 123`) really executes FFmpeg on an
  # empty placeholder file, so the frame that refused it is FFmpeg's exit status
  # on this machine and a missing binary on a runner with none -- a cell whose
  # answer is the runner's PATH, not the package's behavior. `character(0)` is a
  # clean exit with no output, so such a cell reads as "not refused", which is
  # what it is.
  testthat::local_mocked_bindings(
    cached_encoder_names = function() encoders,
    run_program = function(location, args, program = "the program", ...) {
      character(0)
    },
    guard_timeout = function(program, limit, expr, ...) character(0),
    .package = "tidymedia"
  )
  # `spec`, never `cell`: tibble() evaluates its arguments in sequence with the
  # earlier columns in scope, so a `cell =` column would shadow the loop
  # variable for every column after it.
  rows <- lapply(names(cells), function(key) {
    spec <- cells[[key]]
    none_args <- spec$args
    none_args$hardware <- "none"
    nvenc_args <- spec$args
    nvenc_args$hardware <- "nvenc"
    ref <- tm_nvenc_condition(spec$name, none_args)
    got <- if (is.null(limit)) {
      tm_nvenc_condition(spec$name, nvenc_args)
    } else {
      withr::with_options(
        list(tidymedia.timeout = limit),
        tm_nvenc_condition(spec$name, nvenc_args)
      )
    }
    head <- sub(" \\|\\| .*$", "", ref)
    tibble::tibble(
      cell = key, member = spec$name, arg = spec$arg, form = spec$form,
      none = ref, nvenc = got,
      kept = identical(head, spec$name),
      refused_by = head
    )
  })
  out <- do.call(rbind, rows)
  out$match <- out$none == out$nvenc
  out
}

# tm_hardware_encoder_pools(): the mocked builds every instrument in this repo
# hands `cached_encoder_names()`, DERIVED from `hardware_backend_families()`
# rather than spelled out (M107). Three levels:
#
#   nvenc         every encoder the nvenc row covers, and nothing else
#   videotoolbox  every encoder the videotoolbox row covers, and nothing else
#   absent        no encoder at all
#
# Derived because the literal `c("h264_nvenc", "hevc_nvenc", "av1_nvenc")` had
# been written out at three sites, and a fourth family added to the nvenc row
# would have left all three describing a build the package no longer resolves
# against. The encoder name is the family and the backend joined by "_", which
# is how FFmpeg spells every member of both rows and how `hardware_encoder()`
# builds the name it looks for.
#
# A cross-backend level is deliberately NOT collapsed away: mocking the nvenc
# pool while asking for `hardware = "videotoolbox"` is the harder half of a
# cross, because there the availability abort is what the caller would get if
# the check under test did not fire first.
tm_hardware_encoder_pools <- function() {
  fams <- asNamespace("tidymedia")$hardware_backend_families()
  pools <- lapply(names(fams), function(hw) paste0(fams[[hw]], "_", hw))
  names(pools) <- names(fams)
  c(pools, list(absent = character()))
}

# tm_nvenc_encoder_pools(): the two mocked answers AC1 crosses every kept cell
# with -- a build that has the nvenc encoders and one that has none. The second
# is what makes the availability abort fire, and so what a cell must survive for
# the argument error to have outranked the probe.
#
# Two levels, not the three above: adding a videotoolbox level here would add a
# third arm to a sweep whose recorded master tables are keyed on the nvenc
# probe, which is a different measurement from the one AC1 makes.
tm_nvenc_encoder_pools <- function() {
  # `[[` and not `$`: a renamed or restructured nvenc row must ERROR here.
  # Under `$` it would return NULL, both pool levels would answer "no encoders",
  # and AC1's present/absent discrimination would go vacuous with nothing red.
  list(
    present = tm_hardware_encoder_pools()[["nvenc"]],
    absent = tm_hardware_encoder_pools()[["absent"]]
  )
}

# tm_sort_c(): sort in C collation, never the session's locale.
#
# The recorded tables below are compared element-by-element against a live
# sweep, and R's default sort follows LC_COLLATE -- which orders "segment/x"
# against "segment_video/x" differently on a C runner than on this machine. A
# table recorded under one collation and compared under another fails for a
# reason that has nothing to do with the behavior under test.
tm_sort_c <- function(x) sort(x, method = "radix")

# tm_nvenc_probe_master_ref: the commit the tables below were measured at -- the
# tip of master when this branch was cut, so a reader can regenerate them.
tm_nvenc_probe_master_ref <- "b538e63"

# tm_nvenc_dropped_master(): every (member, argument) the AC1 sweep DROPS,
# named with the frame that refused it -- measured 2026-08-31 at
# `tm_nvenc_probe_master_ref` and identical under both encoder pools.
#
# Recorded because AC1 drops a cell BY MEASUREMENT and never by a list, and a
# measurement nobody can see is indistinguishable from a list nobody wrote down.
# Three frames appear. `<none>` is an argument no front door refuses at all --
# a path string that is only ever opened, or a value that is legal (`123` is a
# perfectly good `audio_stream`). `ffm_finish`/`ffm_batch` are the gate booleans
# `run` and `parallel`, refused by the runner rather than the verb. `purrr::pmap`
# is the per-row fan-out. The last two are the two classes the ROADMAP carries
# and this milestone does not close; they are here so they stay visible.
#
# A pair whose two entries name different frames (`segment_video/outfiles`,
# `anonymize_video_batch/color`) is not an inconsistency: different wrong forms
# of one argument are caught at different depths.
tm_nvenc_dropped_master <- function() {
  c(
    "anonymize_video/audio_stream -> <none>",
    "anonymize_video/color -> <none>",
    "anonymize_video/outfile -> <none>",
    "anonymize_video/run -> ffm_finish",
    "anonymize_video_batch/audio_stream -> <none>",
    "anonymize_video_batch/color -> <none>",
    "anonymize_video_batch/color -> purrr::pmap",
    "anonymize_video_batch/fallback -> purrr::pmap",
    "anonymize_video_batch/parallel -> ffm_batch",
    "anonymize_video_batch/pixel_format -> purrr::pmap",
    "anonymize_video_batch/run -> ffm_batch",
    "compare_videos/outfile -> <none>",
    "compare_videos/run -> ffm_finish",
    "compare_videos_batch/fallback -> purrr::pmap",
    "compare_videos_batch/parallel -> ffm_batch",
    "compare_videos_batch/run -> ffm_batch",
    "crop_video/audio_stream -> <none>",
    "crop_video/height -> <none>",
    "crop_video/outfile -> <none>",
    "crop_video/run -> ffm_finish",
    "crop_video/width -> <none>",
    "crop_video/x -> <none>",
    "crop_video/y -> <none>",
    "crop_video_batch/audio_stream -> <none>",
    "crop_video_batch/fallback -> purrr::pmap",
    "crop_video_batch/height -> <none>",
    "crop_video_batch/parallel -> ffm_batch",
    "crop_video_batch/run -> ffm_batch",
    "crop_video_batch/width -> <none>",
    "crop_video_batch/x -> <none>",
    "crop_video_batch/y -> <none>",
    "format_for_web/audio_stream -> <none>",
    "format_for_web/outfile -> <none>",
    "format_for_web/run -> ffm_finish",
    "format_for_web_batch/audio_stream -> <none>",
    "format_for_web_batch/fallback -> purrr::pmap",
    "format_for_web_batch/parallel -> ffm_batch",
    "format_for_web_batch/run -> ffm_batch",
    "picture_in_picture/margin -> <none>",
    "picture_in_picture/outfile -> <none>",
    "picture_in_picture/run -> ffm_finish",
    "picture_in_picture_batch/fallback -> purrr::pmap",
    "picture_in_picture_batch/margin -> <none>",
    "picture_in_picture_batch/parallel -> ffm_batch",
    "picture_in_picture_batch/run -> ffm_batch",
    "segment_video/audio_stream -> <none>",
    "segment_video/end -> <none>",
    "segment_video/fallback -> purrr::pmap",
    "segment_video/outfiles -> <none>",
    "segment_video/outfiles -> purrr::pmap",
    "segment_video/parallel -> ffm_batch",
    "segment_video/run -> ffm_batch",
    "segment_video/start -> <none>",
    "segment_video_batch/audio_stream -> <none>",
    "segment_video_batch/fallback -> purrr::pmap",
    "segment_video_batch/parallel -> ffm_batch",
    "segment_video_batch/run -> ffm_batch",
    "separate_audio_video/audio_stream -> <none>",
    "separate_audio_video/audiofile -> <none>",
    "separate_audio_video/run -> <none>",
    "separate_audio_video/run -> if",
    "separate_audio_video/videofile -> <none>",
    "separate_audio_video_batch/audio_stream -> <none>",
    "separate_audio_video_batch/parallel -> ffm_batch",
    "separate_audio_video_batch/run -> ffm_batch",
    "standardize_video/audio_stream -> <none>",
    "standardize_video/fps -> <none>",
    "standardize_video/height -> <none>",
    "standardize_video/outfile -> <none>",
    "standardize_video/run -> ffm_finish",
    "standardize_video/width -> <none>",
    "standardize_video_batch/audio_stream -> <none>",
    "standardize_video_batch/fallback -> purrr::pmap",
    "standardize_video_batch/fps -> <none>",
    "standardize_video_batch/height -> <none>",
    "standardize_video_batch/parallel -> ffm_batch",
    "standardize_video_batch/run -> ffm_batch",
    "standardize_video_batch/width -> <none>"
  )
}

# tm_nvenc_mismatch_master(): the kept cells whose condition under
# `hardware = "nvenc"` did NOT match its `hardware = "none"` reference on
# master -- the defect this milestone fixes, measured 2026-08-31 at
# `tm_nvenc_probe_master_ref`.
#
# 27 cells, all of them in the three pipelines that resolve the encoder above a
# machine-independent check, and every one of them reports the nvenc
# availability abort where the caller's own argument error belongs. They appear
# only under the `absent` pool: with the encoders present the probe succeeds and
# the argument error is reached anyway, which is why a sweep run on a machine
# whose FFmpeg happens to have nvenc would have measured nothing.
#
# `video_codec` is absent from this list. It is the one argument of the three
# verbs that already reported first, because its token check sits above the
# resolution -- which is why the disclosure that named it as the example was
# wrong (AC5).
tm_nvenc_mismatch_master <- function() {
  c(
    "anonymize_video [audio_codec = length_two]",
    "anonymize_video [audio_codec = list]",
    "anonymize_video [audio_codec = missing]",
    "anonymize_video [audio_codec = number]",
    "anonymize_video [audio_codec = token]",
    "anonymize_video [audio_stream = length_two]",
    "anonymize_video [audio_stream = list]",
    "anonymize_video [audio_stream = missing]",
    "anonymize_video [audio_stream = token]",
    "format_for_web [audio_stream = length_two]",
    "format_for_web [audio_stream = list]",
    "format_for_web [audio_stream = missing]",
    "format_for_web [audio_stream = token]",
    "standardize_video [audio_codec = length_two]",
    "standardize_video [audio_codec = list]",
    "standardize_video [audio_codec = missing]",
    "standardize_video [audio_codec = number]",
    "standardize_video [audio_codec = token]",
    "standardize_video [audio_stream = length_two]",
    "standardize_video [audio_stream = list]",
    "standardize_video [audio_stream = missing]",
    "standardize_video [audio_stream = token]",
    "standardize_video [pixel_format = length_two]",
    "standardize_video [pixel_format = list]",
    "standardize_video [pixel_format = missing]",
    "standardize_video [pixel_format = number]",
    "standardize_video [pixel_format = token]"
  )
}

# M096: what `segment_video()` COMPILED at the merge base -----------------------

# tm_outfiles_cells(): the `outfiles` values AC4's grid and AC2's two
# compiled-today controls name, as one table of calls.
#
# `run = FALSE` throughout: the question is what the verb compiles, not what
# FFmpeg does with it, and a compiled command is the same on a runner with no
# media binaries. `"bad fmt!"` is a legal output filename -- a space and a bang
# are nothing an output path may not contain -- and `list("a.mp4")` is a
# one-element list whose element is a string, which is what the per-row fan-out
# actually receives. Both are here because a front-door guard written as a check
# on `outfiles` AS A WHOLE would refuse them, and neither is refused today.
tm_outfiles_cells <- function(dir) {
  vid <- file.path(dir, "in.mp4")
  if (!file.exists(vid)) file.create(vid)
  list(
    "outfiles = NULL, start len 2" =
      list(infile = vid, start = c(0, 0.5), end = c(0.5, 1), outfiles = NULL),
    "outfiles = character vector, start len 2" =
      list(infile = vid, start = c(0, 0.5), end = c(0.5, 1),
           outfiles = c(file.path(dir, "a.mp4"), file.path(dir, "b.mp4"))),
    "outfiles = 'bad fmt!', start len 1" =
      list(infile = vid, start = 0, end = 1, outfiles = "bad fmt!"),
    "outfiles = list('a.mp4'), start len 1" =
      list(infile = vid, start = 0, end = 1,
           outfiles = list(file.path(dir, "a.mp4")))
  )
}

# tm_outfiles_commands(): drive those cells and return one scrubbed string per
# cell -- the segments' commands joined, or the message if the call was refused.
#
# The fixture directory is scrubbed to `<dir>` for `tm_spawn_trace()`'s reason:
# two checkouts measured in two temp directories would differ in the path and in
# nothing else, and this comparison exists to see everything else.
tm_outfiles_commands <- function(dir) {
  testthat::local_reproducible_output()
  cells <- tm_outfiles_cells(dir)
  vapply(names(cells), function(k) {
    res <- tryCatch(
      do.call("segment_video", c(cells[[k]], list(run = FALSE)),
              envir = asNamespace("tidymedia")),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      return(paste0("<error: ", cli::ansi_strip(conditionMessage(res)), ">"))
    }
    tm_scrub_paths(paste(res$command, collapse = " ;; "), dir)
  }, character(1))
}

# tm_outfiles_baseline(): what those cells compiled at `tm_corrupt_master_ref`,
# measured 2026-08-31 in a worktree at that commit.
#
# Recorded because AC4's claim is that the guard refuses nothing the verb
# compiled before, and a comparison against the same checkout cannot see a
# change both sides make. `tm_outfiles_commands()` regenerates any cell.
tm_outfiles_baseline <- function() {
  c(
    "outfiles = NULL, start len 2" = paste(
      '-y -i "<dir>/in.mp4" -codec:a copy -ss 0 -to 0.5 -map "0:v?" -map',
      '"0:a?" "<dir>/in_1.mp4" ;; -y -i "<dir>/in.mp4" -codec:a copy -ss 0.5',
      '-to 1 -map "0:v?" -map "0:a?" "<dir>/in_2.mp4"'
    ),
    "outfiles = character vector, start len 2" = paste(
      '-y -i "<dir>/in.mp4" -codec:a copy -ss 0 -to 0.5 -map "0:v?" -map',
      '"0:a?" "<dir>/a.mp4" ;; -y -i "<dir>/in.mp4" -codec:a copy -ss 0.5 -to',
      '1 -map "0:v?" -map "0:a?" "<dir>/b.mp4"'
    ),
    "outfiles = 'bad fmt!', start len 1" = paste(
      '-y -i "<dir>/in.mp4" -codec:a copy -ss 0 -to 1 -map "0:v?" -map "0:a?"',
      '"bad fmt!"'
    ),
    "outfiles = list('a.mp4'), start len 1" = paste(
      '-y -i "<dir>/in.mp4" -codec:a copy -ss 0 -to 1 -map "0:v?" -map "0:a?"',
      '"<dir>/a.mp4"'
    )
  )
}
