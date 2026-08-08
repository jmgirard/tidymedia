# M62 — the DOMAIN of the input-path front-door criteria, derived rather than
# listed.
#
# Membership is fixed by a walk over parsed CALL NODES in each namespace
# function's body, never by a substring search of the deparsed text: a name
# appearing inside a cli message string is not a call, and `ffm_manifest`'s
# "Run the batch with `ffm_batch(..., manifest = TRUE)` first." message is
# exactly that case. Reading the namespace rather than the source tree also
# keeps the guard alive under `R CMD check`, where there is no `R/` (M51/M59).

# Every function name CALLED in `f`'s body.
tm_callees <- function(f) {
  out <- character()
  walk <- function(e) {
    if (is.call(e)) {
      head <- e[[1]]
      if (is.name(head)) out <<- c(out, as.character(head))
    }
    if (is.call(e) || is.pairlist(e)) {
      for (i in seq_along(e)) {
        # An empty symbol -- the missing index in `x[, 1]` -- errors
        # "argument is missing" on ANY use, including binding it and testing
        # it, so it is tested in place with rlang::is_missing() and never
        # touched. It contributes no call head either way.
        if (rlang::is_missing(e[[i]]) || is.null(e[[i]])) next
        walk(e[[i]])
      }
    }
  }
  walk(body(f))
  unique(out)
}

# Every namespace function's body as text, for the abort-site counts. Reading
# the namespace rather than `R/` keeps these alive under `R CMD check`, which
# runs against an installed package with no source tree (M51/M59).
tm_namespace_bodies <- function(pkg = "tidymedia") {
  ns <- asNamespace(pkg)
  fns <- mget(ls(ns, all.names = TRUE), envir = ns, ifnotfound = list(NULL))
  fns <- fns[vapply(fns, is.function, logical(1))]
  vapply(fns, function(f) paste(deparse(body(f)), collapse = " "), character(1))
}

# The package's internal call graph, restricted to functions it defines.
tm_call_graph <- function(pkg = "tidymedia") {
  ns <- asNamespace(pkg)
  fns <- mget(ls(ns, all.names = TRUE), envir = ns, ifnotfound = list(NULL))
  fns <- fns[vapply(fns, is.function, logical(1))]
  lapply(fns, tm_callees)
}

tm_reaches <- function(graph, from, target) {
  seen <- character()
  queue <- from
  while (length(queue)) {
    this <- queue[[1]]
    queue <- queue[-1]
    if (this %in% seen) next
    seen <- c(seen, this)
    if (target %in% graph[[this]]) return(TRUE)
    queue <- c(queue, intersect(graph[[this]], names(graph)))
  }
  FALSE
}

# The two verb sets the criteria quantify over.
input_guard_verbs <- function(pkg = "tidymedia") {
  graph <- tm_call_graph(pkg)
  exported <- sort(intersect(getNamespaceExports(pkg), names(graph)))
  fanout <- exported[vapply(exported, function(v)
    tm_reaches(graph, v, "ffm_batch"), logical(1))]
  reaches_files <- exported[vapply(exported, function(v)
    tm_reaches(graph, v, "ffm_files"), logical(1))]
  list(fanout = fanout, scalar = setdiff(reaches_files, fanout))
}

# M63 -- a file that EXISTS and cannot be read, or NULL where this platform
# will not make one (a process running as root reads a mode-000 file anyway).
# The fixture is VERIFIED with the same predicate the guard uses rather than
# assumed from Sys.chmod()'s return, so a test built on it is testing
# unreadability and not a chmod that quietly did nothing.
tm_unreadable_path <- function(dir, name = "m63-unreadable-input.mp4") {
  p <- file.path(dir, name)
  file.create(p)
  Sys.chmod(p, "000")
  if (!file.exists(p) || file.access(p, mode = 4) == 0) {
    # Hand the mode back before giving up: on a platform where the chmod did not
    # take, a file left mode-000 is one the caller's temp-dir cleanup may not be
    # able to remove.
    Sys.chmod(p, "600")
    return(NULL)
  }
  p
}

# Call-shape specs: what a LEGAL call to each verb looks like, with `p` standing
# in for an input path. The specs supply only the shape — which arguments and
# jobs columns the verb requires — never which verbs exist; that is the walk's
# job, and the tests below fail when the walk returns a verb no spec covers.
input_guard_specs <- function() {
  out <- function(ext = ".mp4") file.path(tempdir(), paste0("m62-out", ext))
  jobs1 <- function(p, ...) tibble::tibble(input = p, ...)
  list(
    anonymize_video_batch = function(p) anonymize_video_batch(
      jobs1(p, regions = list(
        data.frame(x = 0, y = 0, width = 10, height = 10))), run = FALSE),
    compare_videos_batch = function(p) compare_videos_batch(
      tibble::tibble(inputs = list(c(p, p)), output = out()), run = FALSE),
    concatenate_videos_batch = function(p) concatenate_videos_batch(
      tibble::tibble(inputs = list(c(p, p)), output = out()), run = FALSE),
    convert_audio_batch = function(p) convert_audio_batch(
      jobs1(p, output = out(".mp3")), run = FALSE),
    crop_video_batch = function(p) crop_video_batch(
      jobs1(p), width = 10, height = 10, run = FALSE),
    extract_audio_batch = function(p) extract_audio_batch(
      jobs1(p, output = out(".aac")), run = FALSE),
    extract_frame_batch = function(p) extract_frame_batch(
      jobs1(p, timestamp = 1), run = FALSE),
    format_for_web_batch = function(p) format_for_web_batch(
      jobs1(p), run = FALSE),
    normalize_audio_batch = function(p) normalize_audio_batch(
      jobs1(p, output = out(".wav")), run = FALSE),
    picture_in_picture_batch = function(p) picture_in_picture_batch(
      tibble::tibble(main = p, overlay = p, output = out()), run = FALSE),
    sample_frames_batch = function(p) sample_frames_batch(
      jobs1(p), fps = 1, outdir = tempdir(), run = FALSE),
    segment_video = function(p) segment_video(p, 0, 1, run = FALSE),
    segment_video_batch = function(p) segment_video_batch(
      jobs1(p, start = 0, end = 1), run = FALSE),
    separate_audio_video_batch = function(p) separate_audio_video_batch(
      jobs1(p, audiofile = out(".aac"), videofile = out()), run = FALSE),
    standardize_video_batch = function(p) standardize_video_batch(
      jobs1(p), run = FALSE),
    strip_metadata_batch = function(p) strip_metadata_batch(
      jobs1(p, output = out()), run = FALSE),

    anonymize_video = function(p) anonymize_video(
      p, out(), regions = list(c(0, 0, 10, 10)), run = FALSE),
    compare_videos = function(p) compare_videos(c(p, p), out(), run = FALSE),
    concatenate_videos = function(p) concatenate_videos(c(p, p), out(),
                                                        run = FALSE),
    convert_audio = function(p) convert_audio(p, out(".mp3"), run = FALSE),
    crop_video = function(p) crop_video(p, out(), width = 10, height = 10,
                                        run = FALSE),
    extract_audio = function(p) extract_audio(p, out(".aac"), run = FALSE),
    extract_frame = function(p) extract_frame(p, out(".png"), time = 1,
                                              run = FALSE),
    format_for_web = function(p) format_for_web(p, out(), run = FALSE),
    normalize_audio = function(p) normalize_audio(p, out(".wav"), run = FALSE),
    picture_in_picture = function(p) picture_in_picture(p, p, out(),
                                                        run = FALSE),
    sample_frames = function(p) sample_frames(p, outdir = tempdir(), fps = 1,
                                              run = FALSE),
    separate_audio_video = function(p) separate_audio_video(
      p, out(".aac"), out(), run = FALSE),
    standardize_video = function(p) standardize_video(p, out(), run = FALSE),
    strip_metadata = function(p) strip_metadata(p, out(), run = FALSE)
  )
}
