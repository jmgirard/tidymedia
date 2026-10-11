# Session-scoped capability memo ---------------------------------------------

# The package's one piece of mutable session state: what this FFmpeg build told
# us about itself. Before M67 every `hardware = "nvenc"` row re-ran
# `ffmpeg -encoders`, so an N-row batch paid N process spawns for an answer that
# cannot change unless the binary does (D044).
#
# Lifetime is the R session. Four routes discard it, and D089 is the census:
# the exported `refresh_ffmpeg_capabilities()`, `set_program()`, a successful
# `unset_program()`, and an `unset_program()` that removed at least one of the
# two remembered files before failing on the other -- the file it did delete
# may be the one lookups were answering from. An `unset_program()` that removed
# nothing does not discard: nothing about the resolved binary changed. The memo
# is per-process, so `parallel = TRUE` workers each keep their own (D044). The
# caller's `tidymedia.hardware_encoders` override IS carried into a worker
# (R/timeout.R, M071); this memo is not, which is why a worker with no override
# still asks its own binary.
.tm_capabilities <- new.env(parent = emptyenv())

# cached_encoder_names(): the encoder-name pool, asked of FFmpeg at most once per
# session. Deliberately sited BELOW has_hardware_encoder()'s getOption() seam:
# tests set `tidymedia.hardware_encoders` to control the answer (145
# `test_that()` blocks name it, counted 2026-10-10 by parsing
# `tests/testthat/test*.R`), and a memo above the seam would make them
# order-dependent.
cached_encoder_names <- function() {
  if (is.null(.tm_capabilities$encoder_names)) {
    .tm_capabilities$encoder_names <- ffmpeg_encoders()$name
  }
  .tm_capabilities$encoder_names
}

# cached_encodable_codecs(): the codec names this build can encode, the rows of
# `ffmpeg -codecs` marked with E, remembered for the session once read, beside
# the encoder pool. A codec name is not an encoder name: `-c:v h264` and `-c:a mp3`
# run on a build whose `-encoders` lists neither, because FFmpeg maps a codec
# name to one of its encoders. Read by one caller, diagnose_missing_encoders(),
# and only after FFmpeg has already failed (M139). A failed read stores nothing,
# so the next diagnosis asks again. ffmpeg_codecs() itself stays uncached
# (D044): it is the exported view of the build as it is now.
cached_encodable_codecs <- function() {
  if (is.null(.tm_capabilities$encodable_codecs)) {
    codecs <- ffmpeg_codecs()
    .tm_capabilities$encodable_codecs <- codecs$name[codecs$encoding]
  }
  .tm_capabilities$encodable_codecs
}

#' Forget what tidymedia remembers about your FFmpeg build
#'
#' Discard the package's record of which encoders and codecs your FFmpeg build
#' has. The next query then asks FFmpeg again.
#'
#' The first call in an R session to \code{\link{has_encoder}}, or one that
#' uses \code{hardware = "nvenc"} or \code{hardware = "videotoolbox"}, asks
#' FFmpeg which encoders it has. The package remembers that answer for the rest
#' of the session. Later calls reuse
#' it and do not start FFmpeg again each time, so a large batch stays fast.
#'
#' A run that FFmpeg refuses can also fill the record. After such a run,
#' \code{\link{ffm_run}} can ask FFmpeg for its encoders and for the codecs it
#' can encode, to name a missing encoder in the error. The package remembers
#' both lists in the same way. A list that FFmpeg did not give is not
#' remembered. The section "When the build lacks an encoder" on
#' \code{\link{ffm_run}} says when the lists are asked for.
#'
#' So the package does not see a change to your FFmpeg build until you discard
#' the record. Examples of a change are a new FFmpeg install, a new graphics
#' card (GPU) driver, or a different FFmpeg program. There are three ways to
#' discard the record:
#'
#' \itemize{
#'   \item Call \code{refresh_ffmpeg_capabilities()} yourself, at any time.
#'   \item Call \code{\link{set_program}} (or \code{\link{set_ffmpeg}}). It
#'     discards the record for you, because the record describes the old
#'     program.
#'   \item Call \code{\link{unset_program}} and have it remove something. When
#'     it forgets a saved location, the package can find a different program. A
#'     call that removed nothing keeps the record, because the program in use
#'     did not change. A call that removed one saved file and then failed on
#'     another discards the record. The file it removed may have named the
#'     program that the record came from.
#' }
#'
#' The glossary in \code{vignette("tidymedia")} explains media terms such as
#' encoder and hardware encoder.
#'
#' @section Parallel workers:
#' Each R process keeps its own record, and a worker does not get the record of
#' your session. So in a batch on \code{W} workers, each worker asks FFmpeg
#' once. Your session can also ask once, before the jobs start. Discarding the
#' record in your session does not reach the workers.
#'
#' The \code{tidymedia.hardware_encoders} option works in a different way. The
#' package copies your value into each worker for the duration of the call, and
#' then puts back the worker's own value. So a batch under your setting does not
#' ask FFmpeg for an encoder list at all. Every worker gives the same answer as
#' your session.
#'
#' @section Functions that never use the record:
#' \code{\link{ffmpeg_encoders}} and \code{\link{ffmpeg_codecs}} ask FFmpeg on
#' every call. So they always show the build as it is now, whether or not you
#' called this function.
#'
#' @return \code{NULL}, invisibly. Called for its side effect.
#' @seealso \code{\link{has_hardware_encoder}} uses the remembered answer.
#'   \code{\link{hardware_encoder}} gives the encoder name without asking
#'   FFmpeg.
#'   \code{\link{ffmpeg_encoders}} always gives a fresh encoder list.
#'   \code{\link{set_program}} points the package at a different FFmpeg program.
#' @family capability functions
#' @examples
#' # After installing FFmpeg, or a GPU driver or OS update mid-session:
#' refresh_ffmpeg_capabilities()
#' @export
refresh_ffmpeg_capabilities <- function() {
  forget_ffmpeg_capabilities()
}

# forget_ffmpeg_capabilities(): drop everything memoized about the current
# FFmpeg build, so the next capability query asks the binary again. The internal
# half of refresh_ffmpeg_capabilities(); also called by set_program().
forget_ffmpeg_capabilities <- function() {
  rm(
    list = ls(.tm_capabilities, all.names = TRUE),
    envir = .tm_capabilities
  )
  invisible(NULL)
}
