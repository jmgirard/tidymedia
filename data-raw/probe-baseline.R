# probe-baseline.R -----------------------------------------------------------
#
# Record the pre-change FFprobe baseline M52 compares against: for a set of
# synthetic fixtures, both the raw `-of compact` text a single combined call
# returns AND the tibbles the PRE-CHANGE `probe_one()` built from the same file
# via its `-show_format` + per-stream `-show_streams` loop.
#
# The pairing is the whole point. A baseline that recorded only the tibbles
# would have to be re-checked by regenerating the fixtures and re-probing them,
# which makes every assertion depend on a binary, on that binary's version, and
# on lavfi encoding the same bytes twice. Recording the compact TEXT beside the
# pre-change tibbles turns AC2 into a pure function test: feed the recorded text
# to the new parser and it must rebuild the recorded tibbles exactly. That test
# needs no FFprobe at all and cannot go green for an environmental reason.
#
# `codec_guard_env()` (data-raw/codec-guard-baseline.R, M41) is reused verbatim
# to materialize a git ref's `R/*.R` into a sourced environment. It is not
# codec-specific despite its name, and a second copy of that machinery here
# would be a second thing to keep correct.
#
# Usage (from the package root, with ffmpeg and ffprobe on PATH):
#
#   source("data-raw/probe-baseline.R")
#   probe_baseline_write("HEAD")     # or any pre-change ref
#
# It writes tests/testthat/fixtures/probe-baseline.rds. Re-run it only to
# re-derive the baseline from a different ref; the committed .rds is the
# artifact the suite reads.

source("data-raw/codec-guard-baseline.R", chdir = FALSE)

# -- fixtures ----------------------------------------------------------------
#
# Six synthetic inputs, all built from ffmpeg's lavfi sources so nothing is
# checked in as media. Each entry carries `cmds`, one or more ffmpeg argument
# strings run in order, with `{out}` standing for the fixture's final path and
# `{scratch}` for an intermediate. They are chosen to exercise the parse paths
# that differ between the two writers rather than to be realistic media:
#
#   plain     video + audio, the ordinary case
#   five      two video + three tagged audio tracks: several streams, so a
#             per-stream loop and a single call visibly disagree on spawns
#   silent    video and no audio
#   audioonly audio and no video
#   hostile   one tag per escape the compact writer emits -- a literal `|`, an
#             embedded newline, a carriage return and a backslash. This is the
#             fixture AC3 rests on, and the one whose newline tag corrupts the
#             PRE-change per-stream parse (AC4).
#   rotated   a display matrix, i.e. stream SIDE DATA -- the nested section the
#             two writers name differently (`rotation` bare under `default=nw=1`,
#             `side_datum/display_matrix:rotation` under `-of compact`), and the
#             one essentially every phone video carries. Added after review
#             round 1, where the absence of any side-data fixture is what let a
#             renamed `rotation` column reach review. Its own pre-change output
#             is corrupt in AC4's way, because the old writer prints the matrix
#             across four lines and the old parser read three of them as
#             columns; the amended AC2 exempts it on that ground and requires
#             the real columns to survive instead.
probe_baseline_fixtures <- function() {
  list(
    plain = list(
      ext = ".mp4",
      cmds = paste(
        "-y -v error -f lavfi -i testsrc=duration=1:size=64x64:rate=10",
        "-f lavfi -i sine=frequency=440:duration=1",
        "-c:v libx264 -c:a aac -shortest -pix_fmt yuv420p '{out}'")),
    five = list(
      ext = ".mkv",
      cmds = paste(
        "-y -v error -f lavfi -i testsrc=duration=1:size=64x64:rate=10",
        "-f lavfi -i sine=frequency=300:duration=1",
        "-f lavfi -i sine=frequency=600:duration=1",
        "-f lavfi -i sine=frequency=900:duration=1",
        "-f lavfi -i color=c=black:s=64x64:d=1",
        "-map 0:v -map 1:a -map 2:a -map 3:a -map 4:v",
        "-c:v libx264 -c:a aac -b:a 32k -pix_fmt yuv420p",
        "-metadata:s:a:0 language=eng",
        "-metadata:s:a:1 language=spa",
        "-metadata:s:a:2 language=fra '{out}'")),
    silent = list(
      ext = ".mp4",
      cmds = paste(
        "-y -v error -f lavfi -i testsrc=duration=1:size=64x64:rate=10",
        "-c:v libx264 -pix_fmt yuv420p '{out}'")),
    audioonly = list(
      ext = ".m4a",
      cmds = paste(
        "-y -v error -f lavfi -i sine=frequency=440:duration=1",
        "-c:a aac '{out}'")),
    # The escape fixture. Each value is written through a shell single-quoted
    # string, so the newline and CR are real control characters in the tag and
    # not the two-character sequences `\n` / `\r`.
    hostile = list(
      ext = ".mkv",
      cmds = paste0(
        "-y -v error -f lavfi -i testsrc=duration=1:size=64x64:rate=10 ",
        "-f lavfi -i sine=frequency=440:duration=1 ",
        "-c:v libx264 -c:a aac -shortest -pix_fmt yuv420p ",
        "-metadata:s:v:0 'title=pipe|here' ",
        "-metadata:s:a:0 'title=line\nbreak' ",
        "-metadata 'comment=carriage\rreturn' ",
        "-metadata 'title=back\\slash' ",
        "'{out}'")),
    # Two passes, because `-display_rotation` is an INPUT option: encoding it
    # onto the output is rejected, and `-metadata:s:v:0 rotate=90` was measured
    # to write a tag that produces no side data at all. Remuxing with `-c copy`
    # attaches the display matrix without re-encoding.
    rotated = list(
      ext = ".mp4",
      scratch_ext = ".mp4",
      cmds = c(
        paste("-y -v error -f lavfi -i testsrc=duration=1:size=64x48:rate=10",
              "-c:v libx264 -pix_fmt yuv420p '{scratch}'"),
        "-y -v error -display_rotation 90 -i '{scratch}' -c copy '{out}'"))
  )
}

# Build every fixture into `dir` and return a named vector of paths.
probe_baseline_build <- function(dir = tempfile("probe-baseline-")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  specs <- probe_baseline_fixtures()
  paths <- character()
  for (nm in names(specs)) {
    spec <- specs[[nm]]
    path <- file.path(dir, paste0(nm, spec$ext))
    scratch_ext <- if (is.null(spec$scratch_ext)) ".mp4" else spec$scratch_ext
    scratch <- file.path(dir, paste0(nm, "-scratch", scratch_ext))
    for (cmd in spec$cmds) {
      cmd <- gsub("{out}", path, cmd, fixed = TRUE)
      cmd <- gsub("{scratch}", scratch, cmd, fixed = TRUE)
      out <- system2("ffmpeg", cmd, stdout = TRUE, stderr = TRUE)
    }
    if (!file.exists(path)) {
      stop("fixture ", nm, " was not written: ", paste(out, collapse = " "))
    }
    paths[[nm]] <- path
  }
  paths
}

# -- recording ---------------------------------------------------------------

# The single combined call M52 moves to, issued directly rather than through the
# package, so the recorded text is what the NEW code will receive and does not
# depend on the ref being loaded.
probe_baseline_compact <- function(path) {
  out <- system2("ffprobe",
                 shQuote(c("-i", path, "-v", "quiet", "-show_format",
                           "-show_streams", "-of", "compact=escape=c")),
                 stdout = TRUE, stderr = FALSE)
  if (!is.null(attr(out, "status"))) {
    stop("ffprobe failed on ", path)
  }
  as.character(out)
}

# Replace every occurrence of a fixture's own path with a stable token, so a
# baseline recorded on one machine compares equal on another. The path reaches
# the output twice: as `probe_all()`'s `file` column and as FFprobe's own
# `filename` format field.
probe_baseline_scrub <- function(x, path, token) {
  if (is.data.frame(x)) {
    for (col in names(x)) {
      if (is.character(x[[col]])) {
        x[[col]] <- gsub(path, token, x[[col]], fixed = TRUE)
        x[[col]] <- gsub(basename(path), basename(token), x[[col]], fixed = TRUE)
      }
    }
    return(x)
  }
  x
}

# Record the whole baseline against `ref`: for each fixture, the compact text
# plus the pre-change probe_one() and probe_all() results.
probe_baseline_record <- function(ref = "HEAD", root = ".") {
  env <- codec_guard_env(ref, root)
  paths <- probe_baseline_build()

  entries <- list()
  for (nm in names(paths)) {
    path <- paths[[nm]]
    token <- file.path("<fixtures>", basename(path))

    one <- env$probe_one(path)
    all_typed <- env$probe_all(path, typed = TRUE)
    all_raw <- env$probe_all(path, typed = FALSE)

    entries[[nm]] <- list(
      compact = probe_baseline_compact(path),
      # probe_one() carries no `file` column, but its container row carries
      # FFprobe's `filename`, so it needs scrubbing too.
      one = list(
        container = probe_baseline_scrub(one$container, path, token),
        streams = probe_baseline_scrub(one$streams, path, token)),
      typed = list(
        container = probe_baseline_scrub(all_typed$container, path, token),
        streams = probe_baseline_scrub(all_typed$streams, path, token)),
      untyped = list(
        container = probe_baseline_scrub(all_raw$container, path, token),
        streams = probe_baseline_scrub(all_raw$streams, path, token)),
      # The path the recorded compact text was produced from, and the token it
      # was scrubbed to. The suite re-applies exactly this substitution to what
      # it parses out of `compact`, so a comparison against the recorded
      # tibbles is not a comparison of two different temp directories.
      path = path,
      token = token,
      # The spawn count the pre-change loop pays for this file, so AC1's "the
      # count was nb_streams + 1 before" is recorded evidence rather than a
      # claim about code that no longer exists.
      spawns_before = nrow(one$streams) + 1L
    )
  }

  structure(
    entries,
    provenance = list(
      generator = "data-raw/probe-baseline.R",
      ref = ref,
      recorded = as.character(Sys.Date()),
      ffmpeg = system2("ffmpeg", "-version", stdout = TRUE)[[1]],
      ffprobe = system2("ffprobe", "-version", stdout = TRUE)[[1]],
      fixtures = vapply(probe_baseline_fixtures(),
                        function(s) paste(s$cmds, collapse = " && "), "")
    )
  )
}

probe_baseline_write <- function(ref = "HEAD", root = ".") {
  baseline <- probe_baseline_record(ref, root)
  dest <- file.path(root, "tests", "testthat", "fixtures", "probe-baseline.rds")
  saveRDS(baseline, dest, version = 2)
  message("wrote ", dest, " (", length(baseline), " fixtures)")
  invisible(baseline)
}
