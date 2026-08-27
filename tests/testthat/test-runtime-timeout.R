# M69: the `tidymedia.timeout` seam.
#
# The resolver is the whole contract's front door: every spawn site passes what
# it returns straight to base R's `timeout=`. Two of its rules exist because
# base R's own handling is unsafe as a package default (measured on R 4.6.1):
#   - a value below 1 TRUNCATES to 0, which is base R's sentinel for "no
#     limit", so `0.5` would leave the call completely unbounded -- the exact
#     silent failure this milestone removes. Non-whole values are refused.
#   - `system2()` accepts `"2"` and `c(1, 2)` without complaint, so nothing
#     downstream will catch a malformed option for us.

test_that("resolve_timeout() returns 0 when the option is unset", {
  withr::local_options(tidymedia.timeout = NULL)
  expect_identical(resolve_timeout(), 0)
})

test_that("resolve_timeout() returns a whole positive limit unchanged", {
  withr::local_options(tidymedia.timeout = 30)
  expect_identical(resolve_timeout(), 30)
  withr::local_options(tidymedia.timeout = 1L)
  expect_identical(resolve_timeout(), 1)
})

test_that("resolve_timeout() accepts an explicit 0 as 'no limit'", {
  withr::local_options(tidymedia.timeout = 0)
  expect_identical(resolve_timeout(), 0)
})

test_that("resolve_timeout() refuses a fractional limit, naming whole seconds", {
  # 0.5 is the dangerous one: base R truncates it to 0 and the call runs
  # unbounded. 1.9 is refused for the same reason (it silently becomes 1).
  withr::local_options(tidymedia.timeout = 0.5)
  expect_error(resolve_timeout(), "whole number")
  withr::local_options(tidymedia.timeout = 1.9)
  expect_error(resolve_timeout(), "whole number")
})

test_that("resolve_timeout() names the option in its refusal", {
  withr::local_options(tidymedia.timeout = 0.5)
  expect_error(resolve_timeout(), "tidymedia.timeout")
})

test_that("resolve_timeout() refuses a negative, NA, string or vector limit", {
  withr::local_options(tidymedia.timeout = -1)
  expect_error(resolve_timeout())
  withr::local_options(tidymedia.timeout = NA_real_)
  expect_error(resolve_timeout())
  # base system2() ACCEPTS both of these, so only this guard catches them.
  withr::local_options(tidymedia.timeout = "2")
  expect_error(resolve_timeout())
  withr::local_options(tidymedia.timeout = c(1, 2))
  expect_error(resolve_timeout())
})

# timeout_status() ------------------------------------------------------------

# AC4: a timeout is recognized by the `status` attribute, never by matching R's
# warning text, whose wording is translated under a non-English locale (M46).
# AC6: status 124 is only a timeout when a limit was actually in force -- a
# program may exit 124 for its own reasons.

test_that("timeout_status() reports a timeout only when a limit was in force", {
  out <- structure(character(0), status = 124L)
  expect_true(is_timeout(out, limit = 2))
  expect_false(is_timeout(out, limit = 0))
})

test_that("timeout_status() does not read any other status as a timeout", {
  expect_false(is_timeout(structure(character(0), status = 1L), limit = 2))
  expect_false(is_timeout(structure(character(0), status = 234L), limit = 2))
  expect_false(is_timeout(character(0), limit = 2))
})

test_that("is_timeout() accepts the status however system2() types it", {
  # system() and system2() have both been seen returning the status as double
  # and as integer; the comparison must not depend on which.
  expect_true(is_timeout(structure(character(0), status = 124), limit = 2))
  expect_true(is_timeout(structure(character(0), status = 124L), limit = 2))
})

test_that("abort_timeout() names the program and the limit in seconds", {
  err <- expect_error(abort_timeout("FFmpeg", 30))
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "FFmpeg")
  expect_match(msg, "30")
  expect_match(msg, "second")
})

test_that("abort_timeout() signals a distinct condition class", {
  # The class is what lets a caller distinguish a timeout from any other
  # failure -- probe_all() and count_audio_streams() deliberately absorb both
  # today, and the class is what would let a later change separate them.
  err <- expect_error(abort_timeout("FFmpeg", 30))
  expect_s3_class(err, "tidymedia_timeout")
})

test_that("abort_timeout() names the option so the caller can raise it", {
  err <- expect_error(abort_timeout("FFmpeg", 30))
  expect_match(cli::ansi_strip(conditionMessage(err)), "tidymedia.timeout")
})

# The four spawn sites (AC1, AC2) ---------------------------------------------

# The domain of AC1 is the four sites named in the milestone's Scope, not
# "every spawn site" -- a source regex cannot enumerate that, and the plan's
# criteria audit cut the universal for exactly that reason. These read the
# functions' own bodies, so a site that stops passing the limit reddens here.

timeout_site_bodies <- function() {
  list(
    ffmpeg    = tidymedia::ffmpeg,
    ffprobe   = tidymedia::ffprobe,
    mediainfo = tidymedia::mediainfo,
    run_program = get("run_program", envir = asNamespace("tidymedia"))
  )
}

test_that("each of the four spawn sites passes a resolved limit to timeout=", {
  for (nm in names(timeout_site_bodies())) {
    src <- paste(deparse(body(timeout_site_bodies()[[nm]])), collapse = "\n")
    expect_match(src, "resolve_timeout(", fixed = TRUE,
                 info = paste(nm, "must resolve the limit"))
    expect_match(src, "timeout = limit", fixed = TRUE,
                 info = paste(nm, "must pass it to timeout="))
    expect_match(src, "guard_timeout(", fixed = TRUE,
                 info = paste(nm, "must route through the shared guard"))
  }
})

test_that("with the option unset, each site resolves a limit of 0", {
  withr::local_options(tidymedia.timeout = NULL)
  expect_identical(resolve_timeout(), 0)
})

test_that("guard_timeout() re-raises a non-timeout warning but drops it on a timeout", {
  # Layer 0 has always let a non-zero exit warn; that must survive.
  expect_warning(
    guard_timeout("FFmpeg", 0, {warning("ordinary"); structure("x", status = 1L)}),
    "ordinary"
  )
  # On a timeout the held warning is dropped -- it carries the command line and
  # the input= temp path -- and replaced by the package's own message.
  expect_error(
    guard_timeout("FFmpeg", 2, {warning("secret /tmp/path"); structure("x", status = 124L)}),
    "timed out"
  )
})

test_that("guard_timeout(suppress = TRUE) discards warnings, as run_program() always has", {
  expect_no_warning(
    guard_timeout("x", 0, {warning("ordinary"); "ok"}, suppress = TRUE)
  )
})

test_that("no warning at all escapes a timed-out guard (AC7, locale-free)", {
  # Asserted as "no warning", never as a match on `timed out after`: R's warning
  # text is translated under a non-English locale, so a text match would pass
  # while the command line still leaked (M46).
  expect_no_warning(
    tryCatch(
      guard_timeout("FFmpeg", 2, {warning("timed out after 2s"); structure("x", status = 124L)}),
      error = function(e) NULL
    )
  )
})

# Execution: a real program actually gets killed (AC3, AC5, AC7) ---------------

# These are the criterion's real evidence. The body-reading tests above prove
# the limit is PASSED; only these prove it BITES.
#
# The hang is produced by a FIFO nobody writes to: FFmpeg blocks reading its
# header forever, so the test does not race the machine's encoding speed the way
# "encode 600 seconds of video" would. A slow-command fixture that a fast host
# finishes before the limit would pass this file while measuring nothing.
#
# Windows has no mkfifo, so the gate skips there. Per M68, the fixture is built
# INSIDE the gate -- a platform that cannot create it must not reach the
# creation call -- and the gate skips rather than fail()s, because
# testthat::fail() RECORDS a failure and RETURNS, falling on into the operation
# it guards.
#
# These tests are SLOW on Linux and the slowness is the feature under test, not
# waste: base R escalates SIGINT -> SIGTERM -> SIGKILL across limit + 40 s, and
# an FFmpeg blocked on the FIFO rides that ladder to the end, so each of these
# costs ~42 s there against ~2 s on macOS. Five of them is four minutes of a
# CRAN check spent waiting, which is not a reasonable thing to ask of CRAN's
# machines -- so they skip there. `devtools::check()` and the CI workflow both
# set NOT_CRAN, so the release gate and every push still run them; only CRAN's
# own submission check opts out.
# local_blocking_input() itself lives in helper-timeout-sweep.R, so M70's grid
# can anchor against the same real hang rather than building a second fixture
# (M40). Everything above about why it is a FIFO, and why it skips, still
# applies to it.

test_that("ffmpeg() aborts at the limit instead of blocking forever", {
  blocked <- local_blocking_input()
  out <- withr::local_tempfile(fileext = ".mp4")
  withr::local_options(tidymedia.timeout = 2)
  start <- Sys.time()
  err <- expect_error(
    ffmpeg(paste("-y -i", shQuote(blocked), shQuote(out))),
    class = "tidymedia_timeout"
  )
  expect_lt(as.numeric(difftime(Sys.time(), start, units = "secs")), 60)
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "FFmpeg")
  expect_match(msg, "2 seconds")
})

test_that("ffprobe() aborts at the limit", {
  skip_if_no_ffprobe()
  blocked <- local_blocking_input()
  withr::local_options(tidymedia.timeout = 2)
  start <- Sys.time()
  err <- expect_error(ffprobe(paste("-i", shQuote(blocked))),
                      class = "tidymedia_timeout")
  expect_lt(as.numeric(difftime(Sys.time(), start, units = "secs")), 60)
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "FFprobe")
  expect_match(msg, "2 seconds")
})

test_that("the abort names the limit but never R's command line or temp path", {
  # AC7's other half: R's timeout warning embeds the full command line and the
  # input= temp file. The package's message replaces it.
  blocked <- local_blocking_input()
  out <- withr::local_tempfile(fileext = ".mp4")
  withr::local_options(tidymedia.timeout = 2)
  err <- expect_error(ffmpeg(paste("-y -i", shQuote(blocked), shQuote(out))))
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_no_match(msg, tempdir(), fixed = TRUE)
})

test_that("no R timeout warning escapes the call (AC7, locale-free)", {
  blocked <- local_blocking_input()
  out <- withr::local_tempfile(fileext = ".mp4")
  withr::local_options(tidymedia.timeout = 2)
  # Asserted as "no warning at all", never as a match on R's English text:
  # under a translated locale a text match passes while the warning still leaks.
  expect_no_warning(
    tryCatch(ffmpeg(paste("-y -i", shQuote(blocked), shQuote(out))),
             error = function(e) NULL)
  )
})

test_that("ffm_run() aborts at the limit and states D046's disposition (AC5)", {
  blocked <- local_blocking_input()
  out <- withr::local_tempfile(fileext = ".mp4")
  withr::local_options(tidymedia.timeout = 2)
  start <- Sys.time()
  err <- expect_error(ffm_run(ffm(blocked, out)), class = "tidymedia_timeout")
  expect_lt(as.numeric(difftime(Sys.time(), start, units = "secs")), 60)
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "FFmpeg")
  expect_match(msg, "2 seconds")
  # FFmpeg blocks on the FIFO's header BEFORE it opens the output, so it writes
  # nothing and D046 correctly has nothing to report -- the disposition bullet
  # is absent, not missing. What must hold is that no output was left behind.
  expect_false(file.exists(out))
})

test_that("a timed-out ffm_run() removes what the killed run DID write (AC5)", {
  # The written case cannot be produced by the FIFO: FFmpeg never reaches the
  # output. Racing a real encode against the limit would make the test depend on
  # the host's speed, which is the failure mode M31/M46 both paid for. So the
  # kill is injected at the seam instead, leaving the cleanup path -- the thing
  # AC5 is about -- entirely real.
  skip_if_no_ffmpeg()
  video <- make_test_video()
  out <- withr::local_tempfile(fileext = ".mp4")
  withr::local_options(tidymedia.timeout = 2)
  calls <- 0L
  local_mocked_bindings(
    run_program = function(...) {
      calls <<- calls + 1L
      # What a killed FFmpeg leaves behind: a partial output on disk.
      writeLines("half an mp4", out)
      abort_timeout("FFmpeg", 2)
    }
  )
  err <- expect_error(ffm_run(ffm(video, out)), class = "tidymedia_timeout")
  # Count the invocation rather than trusting the mock ran: a mock that is
  # never reached makes every assertion below vacuous (M44).
  expect_identical(calls, 1L)
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "timed out")
  expect_match(msg, "was removed")
  expect_false(file.exists(out))
})

test_that("ffm_run() keeps an output the killed run never wrote, per D046", {
  blocked <- local_blocking_input()
  out <- withr::local_tempfile(fileext = ".mp4")
  writeLines("pre-existing", out)
  keep <- readLines(out)
  withr::local_options(tidymedia.timeout = 2)
  err <- expect_error(ffm_run(ffm(blocked, out)), class = "tidymedia_timeout")
  expect_match(cli::ansi_strip(conditionMessage(err)), "left as it was")
  # D046's rule is applied unchanged: FFmpeg blocked on the input and never
  # opened the output, so what was already there survives.
  expect_true(file.exists(out))
  expect_identical(readLines(out), keep)
})

test_that("with the option unset the same blocking input is NOT bounded", {
  # The control. Without it every assertion above would pass even if the abort
  # came from something other than the timeout -- a FIFO input is unusual
  # enough that FFmpeg could plausibly refuse it outright.
  blocked <- local_blocking_input()
  out <- withr::local_tempfile(fileext = ".mp4")
  withr::local_options(tidymedia.timeout = NULL)
  # Bounded by the harness, not by the package: if the package were still
  # imposing a limit this would abort instead of being killed at 8 s.
  res <- withCallingHandlers(
    system2(find_ffmpeg(), shQuote(c("-y", "-i", blocked, out)),
            stdout = TRUE, input = "", timeout = 8),
    warning = function(w) invokeRestart("muffleWarning")
  )
  expect_identical(as.integer(attr(res, "status")), 124L)
})

# The readers absorb a timeout instead of losing the batch (D047) --------------

# D047 puts a timeout on the same footing as any other unreadable file for the
# metadata readers. The defect these fence is not "the hung file fails" -- it is
# that the hung file used to take every OTHER file's result down with it, which
# is what a 500-file corpus with one bad file actually costs.

test_that("probe_all() yields an NA row for a hung file and keeps the rest", {
  skip_if_no_ffprobe()
  blocked <- local_blocking_input()
  good <- make_test_video()
  withr::local_options(tidymedia.timeout = 2)
  info <- NULL
  expect_warning(info <- probe_all(c(good, blocked, good)), "Could not probe")
  # Three rows, in input order, and the two readable files still carry data:
  # the assertion that fails without the absorber is that this returned at all.
  expect_identical(nrow(info$container), 3L)
  expect_identical(info$container$file, c(good, blocked, good))
  expect_true(is.na(info$container$format_name[[2]]))
  expect_false(is.na(info$container$format_name[[1]]))
})

test_that("mediainfo_parameter() returns NA for a hung file and keeps the rest", {
  # The kill is injected at the run_program() seam rather than produced by a
  # FIFO: MediaInfo's behavior on a named pipe is not something this suite can
  # pin down across platforms, and the loop's recovery -- the thing F3 is about
  # -- stays entirely real either way.
  one <- withr::local_tempfile(fileext = ".mp4")
  two <- withr::local_tempfile(fileext = ".mp4")
  writeLines("not really a video", one)
  writeLines("not really a video", two)
  calls <- 0L
  local_mocked_bindings(
    find_mediainfo = function(...) "mediainfo",
    run_program = function(...) {
      calls <<- calls + 1L
      if (calls == 2L) abort_timeout("MediaInfo", 2)
      "1920"
    }
  )
  vals <- NULL
  expect_warning(
    vals <- mediainfo_parameter(c(one, two), section = "Video",
                                parameter = "Width"),
    "Could not read"
  )
  # Both files were attempted, and the first file's value survived the second
  # file's timeout.
  expect_identical(calls, 2L)
  expect_identical(vals, c(1920L, NA_integer_))
})

test_that("mediainfo_query() returns an NA row for a hung file and keeps the rest", {
  # mediainfo_read() is the third absorber site and the one the first return
  # shipped untested; mediainfo_query() and mediainfo_template() both reach it.
  one <- withr::local_tempfile(fileext = ".mp4")
  two <- withr::local_tempfile(fileext = ".mp4")
  writeLines("not really a video", one)
  writeLines("not really a video", two)
  calls <- 0L
  local_mocked_bindings(
    find_mediainfo = function(...) "mediainfo",
    run_program = function(...) {
      calls <<- calls + 1L
      if (calls == 1L) abort_timeout("MediaInfo", 2)
      c("Width, Height", "1920, 1080")
    }
  )
  res <- NULL
  expect_warning(
    res <- mediainfo_query(c(one, two), section = "Video",
                           parameters = c("Width", "Height")),
    "Could not read"
  )
  expect_identical(calls, 2L)
  expect_identical(nrow(res), 2L)
  # The hung file is row 1 and NA; the file after it still carries its values,
  # which is the property the absorber exists for.
  expect_true(is.na(res$Width[[1]]))
  expect_identical(res$Width[[2]], 1920L)
})

# A hung file is named as hung, not merged into "unreadable" -------------------

# The readers still return the same NA row D047 specifies. What changed is the
# diagnosis: a caller who set a limit precisely to catch hangs could not tell a
# hang from a corrupt file, and one caller -- verify_media() -- turned that
# ambiguity into an actively wrong message.

test_that("probe_all()'s warning says which files timed out", {
  skip_if_no_ffprobe()
  blocked <- local_blocking_input()
  good <- make_test_video()
  withr::local_options(tidymedia.timeout = 2)
  expect_warning(probe_all(c(good, blocked)), "timed out rather than")
})

test_that("probe_all() does not mention a timeout when nothing timed out", {
  # The control: without it the assertion above would pass on a reader that
  # always says "timed out", which would be the same defect facing the other way.
  skip_if_no_ffprobe()
  missing <- file.path(tempdir(), "no-such-file-XYZ.mp4")
  good <- make_test_video()
  withr::local_options(tidymedia.timeout = NULL)
  w <- tryCatch(probe_all(c(good, missing)), warning = function(w) w)
  expect_s3_class(w, "warning")
  expect_no_match(cli::ansi_strip(conditionMessage(w)), "timed out")
})

test_that("verify_media() refuses on a timed-out probe instead of failing every check", {
  # The defect: probe_all() absorbs the timeout, every expectation reads
  # `actual = NA`, and ffm_run(verify=) then aborts blaming a successful encode
  # for producing the wrong width. A probe that never answered is not an answer.
  video <- make_test_video()
  withr::local_options(tidymedia.timeout = 2)
  calls <- 0L
  local_mocked_bindings(
    run_program = function(...) {
      calls <<- calls + 1L
      abort_timeout("FFprobe", 2)
    }
  )
  err <- NULL
  # No warning either: the probe's "could not read" warning describes the same
  # event the abort describes, and telling the caller twice is noise.
  expect_no_warning(
    err <- expect_error(
      verify_media(video, width = 64, video_codec = "h264"),
      class = "tidymedia_timeout"
    )
  )
  expect_identical(calls, 1L)
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "FFprobe")
  expect_match(msg, "2 seconds")
  # The wrong message must be gone, not merely joined by a right one.
  expect_no_match(msg, "expected", fixed = TRUE)
})

test_that("verify_media() still surfaces the probe's warning when nothing timed out", {
  # The other half of the hold-and-replay: holding the warning is only correct
  # because it is replayed when the abort does NOT happen. Without this, an
  # unreadable file would go from "warned about" to silent, which is a
  # regression the timeout test cannot see.
  skip_if_no_ffprobe()
  bad <- withr::local_tempfile(fileext = ".mp4")
  writeLines("not a video at all", bad)
  withr::local_options(tidymedia.timeout = NULL)
  res <- NULL
  w <- expect_warning(
    res <- verify_media(bad, width = 64),
    "Could not probe"
  )
  # Replayed as the condition it was, not re-signalled from its text.
  expect_s3_class(w, "rlang_warning")
  expect_false(res$pass[[1]])
})

# The Layer 0 hatch leaves its partial output behind (F7's narrowed claim) -----

test_that("a timed-out ffmpeg() leaves what the killed run wrote", {
  # ?tidymedia and NEWS.md both say the raw hatch does NOT clean up after a
  # timeout, because it never parses the argument string and so cannot know
  # which argument is the output. That claim needs a run that actually writes
  # something before it hangs, which a FIFO input cannot produce -- FFmpeg
  # blocks on the header before opening its output. A stand-in binary that
  # writes and then blocks produces it deterministically, without racing a real
  # encode against the limit on an unknown host (the M31/M46 failure mode).
  skip_on_os("windows")
  out <- withr::local_tempfile(fileext = ".mp4")
  fake <- withr::local_tempfile(fileext = ".sh")
  writeLines(
    c("#!/bin/sh", 'printf partial > "$1"', "exec sleep 600"),
    fake
  )
  Sys.chmod(fake, "0755")
  local_mocked_bindings(find_ffmpeg = function(...) shQuote(fake))
  withr::local_options(tidymedia.timeout = 2)
  start <- Sys.time()
  expect_error(ffmpeg(shQuote(out)), class = "tidymedia_timeout")
  expect_lt(as.numeric(difftime(Sys.time(), start, units = "secs")), 60)
  # The kill really happened after the write, so the surviving file is the
  # documented behavior and not an artifact of nothing having run.
  expect_true(file.exists(out))
  expect_identical(readLines(out, warn = FALSE), "partial")
})

# Documentation (AC8) ---------------------------------------------------------

test_that("?tidymedia documents the option's name, unit, default and effect", {
  rd <- rd_sources()
  skip_if(is.null(rd), "no Rd source available")
  # Read through the shared two-shape reader: under R CMD check there is no
  # man/ dir, and a source-tree-only guard would silently SKIP in exactly the
  # run the release gate uses (M51).
  hit <- rd[grepl("tidymedia-package", names(rd))]
  expect_length(hit, 1)
  txt <- hit[[1]]
  expect_match(txt, "tidymedia.timeout", fixed = TRUE)
  expect_match(txt, "second")            # the unit
  expect_match(txt, "no limit")          # what the default means
  expect_match(txt, "abort")             # what reaching it does
})

# The four assertions above are substring greps, and a substring grep is how the
# over-broad claim shipped green once already: "abort" was present the whole
# time the doc said EVERY timed-out call aborts, which stopped being true when
# the readers began absorbing. These two guards fence the scoped claim instead,
# so restoring the unqualified sentence reddens them.

doc_timeout_sources <- function() {
  rd <- rd_sources()
  hit <- if (is.null(rd)) NULL else rd[grepl("tidymedia-package", names(rd))]
  news <- if (file.exists("../../NEWS.md")) {
    "../../NEWS.md"
  } else {
    p <- system.file("NEWS.md", package = "tidymedia")
    if (nzchar(p)) p else NULL
  }
  list(
    rd = if (length(hit) == 1L) hit[[1]] else NULL,
    news = if (is.null(news)) NULL else
      paste(readLines(news, warn = FALSE), collapse = "\n")
  )
}

test_that("both docs scope the abort and name the readers that absorb instead", {
  src <- doc_timeout_sources()
  skip_if(is.null(src$rd) || is.null(src$news), "docs not available")
  for (nm in c("rd", "news")) {
    txt <- src[[nm]]
    # The absorbing half must be stated, not merely implied by silence: it is
    # the half a reader acting on an NA row needs.
    expect_match(txt, "absorb", info = nm)
    expect_match(txt, "probe_all", fixed = TRUE, info = nm)
    expect_match(txt, "verify_media", fixed = TRUE, info = nm)
    # And the unqualified universal must be absent. This is the assertion that
    # reddens if the old sentence comes back.
    expect_no_match(txt, "A call that reaches the limit aborts", fixed = TRUE,
                    info = nm)
    expect_no_match(txt, "a call that\nreaches it aborts", fixed = TRUE,
                    info = nm)
  }
})

# The scoped claim above names two behaviors, and for most of this milestone
# that was believed to be all of them. It is not: count_audio_streams() and
# tool_versions() swallow a timeout and return NA with NO warning at
# all, so a bounded hang under remove_audio() is invisible. Three review passes
# each found one more member of a hand-written list, which is what a promise
# bounded by recollection rather than by a procedure does -- and the third
# return's own finding named remove_audio(), a function this package does not
# export. M69 therefore states
# the no-warning paths instead of claiming a two-way partition, and M70 makes
# absorption uniform. This guard reddens if that disclosure is dropped before
# the behavior it discloses is fixed.
test_that("both docs disclose the paths that absorb with no warning", {
  src <- doc_timeout_sources()
  skip_if(is.null(src$rd) || is.null(src$news), "docs not available")
  for (nm in c("rd", "news")) {
    txt <- src[[nm]]
    expect_match(txt, "count_audio_streams", fixed = TRUE, info = nm)
    # The verbs that reach it, so a reader can tell whether their own call is
    # affected without reading the package source. separate_audio_video() is
    # the discriminating one: extract_audio() and convert_audio() are already
    # named in the absorbing paragraph above, so asserting either of those
    # would pass on the old two-way text too.
    expect_match(txt, "separate_audio_video", fixed = TRUE, info = nm)
    expect_match(txt, "tool_versions", fixed = TRUE, info = nm)
    # The claim itself: these absorb WITHOUT the warning the readers give.
    expect_match(txt, "no warning", info = nm)
    # And the three-way description must not be read as a partition.
    expect_match(txt, "not a complete", info = nm)
  }
})

test_that("both docs disclose that the abort can lag the limit", {
  src <- doc_timeout_sources()
  skip_if(is.null(src$rd) || is.null(src$news), "docs not available")
  for (nm in c("rd", "news")) {
    txt <- src[[nm]]
    expect_match(txt, "40 seconds", fixed = TRUE, info = nm)
    expect_match(txt, "guarantee", info = nm)
  }
})

test_that("NEWS.md carries the entry", {
  # Two shapes, for the same reason the Rd guard has two: under R CMD check the
  # tests run against an INSTALLED package with no source tree. NEWS.md IS
  # installed into the package root, so the guard runs in both shapes rather
  # than skipping in exactly the run the release gate uses (M51).
  news <- if (file.exists("../../NEWS.md")) {
    "../../NEWS.md"
  } else {
    p <- system.file("NEWS.md", package = "tidymedia")
    if (nzchar(p)) p else NULL
  }
  skip_if(is.null(news), "NEWS.md not available in either shape")
  txt <- paste(readLines(news, warn = FALSE), collapse = "\n")
  expect_match(txt, "tidymedia.timeout", fixed = TRUE)
})
