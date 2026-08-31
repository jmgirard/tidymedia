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
# a failure -- AC1's `has_nvenc()` carve-out is exactly that cell.
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
  ffmpeg_class <- c("ffmpeg_codecs", "ffmpeg_encoders", "has_nvenc")
  mediainfo_parameter_class <- c(
    "get_duration", "get_frame_rate", "get_height", "get_sample_rate",
    "get_width"
  )
  mediainfo_read_class <- c(
    "mediainfo_query", "mediainfo_summary", "mediainfo_template"
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
    "has_nvenc", "mediainfo", "normalize_audio", "picture_in_picture",
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

# M094 review round 2: the cells that carry a WRONG argument -----------------

# tm_timeout_corrupt_specs(): each member's own cell with its FIRST argument
# replaced by `123`.
#
# Every cell in `tm_timeout_call_specs()` and `tm_timeout_variant_specs()`
# carries VALID arguments, so no leg built on them can see the refusal DISPLACE
# an error the caller's own call earned -- which is how one class of that defect
# survived round 1 at four verbs and recurred at nine more (review F1, G1). This
# is the missing axis, and it is generic rather than hand-written: `123` is a
# number where every member's first argument wants a path, a job table or a
# pipeline, and each front door refuses it on argument shape alone -- no file is
# read, no binary is looked up, and nothing is spawned, so the answer is the
# same on a runner with no media binaries as it is here.
#
# The three members that take no arguments at all (`ffmpeg_codecs`,
# `ffmpeg_encoders`, `has_nvenc`) have nothing to corrupt and are absent; the
# caller checks the count rather than trusting the table's length.
tm_timeout_corrupt_specs <- function(dir) {
  specs <- tm_timeout_call_specs(dir)
  out <- list()
  for (nm in names(specs)) {
    args <- specs[[nm]]
    if (length(args) == 0) next
    args[[1]] <- 123
    out[[nm]] <- args
  }
  out
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
