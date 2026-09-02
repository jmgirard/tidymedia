# Session-scoped capability memo ---------------------------------------------

# The package's one piece of mutable session state: what this FFmpeg build told
# us about itself. Before M67 every `hardware = "nvenc"` row re-ran
# `ffmpeg -encoders`, so an N-row batch paid N process spawns for an answer that
# cannot change unless the binary does (D044).
#
# Lifetime is the R session. It is discarded only explicitly, via the exported
# `refresh_ffmpeg_capabilities()`, or implicitly by `set_program()`, which is the
# one package call that can repoint us at a different binary. The memo is
# per-process, so `parallel = TRUE` workers each keep their own (D044). The
# caller's `tidymedia.hardware_encoders` override IS carried into a worker
# (R/timeout.R, M071); this memo is not, which is why a worker with no override
# still asks its own binary.
.tm_capabilities <- new.env(parent = emptyenv())

# cached_encoder_names(): the encoder-name pool, asked of FFmpeg at most once per
# session. Deliberately sited BELOW has_hardware_encoder()'s getOption() seam: ~80 test call
# sites set `tidymedia.hardware_encoders` to control the answer, and a memo above
# the seam would make them order-dependent.
cached_encoder_names <- function() {
  if (is.null(.tm_capabilities$encoder_names)) {
    .tm_capabilities$encoder_names <- ffmpeg_encoders()$name
  }
  .tm_capabilities$encoder_names
}

#' Forget what tidymedia remembers about your FFmpeg build
#'
#' Discard the session-scoped record of which encoders your FFmpeg build
#' provides, so the next capability query asks FFmpeg again.
#'
#' The first hardware-encoding call in an R session -- \code{hardware =
#' "nvenc"} or \code{hardware = "videotoolbox"}, whichever comes first --
#' asks FFmpeg which encoders it has; later calls reuse that answer rather than starting a new
#' FFmpeg process per call, which is what makes a large batch practical. The
#' answer is remembered for the rest of the session, so a build that changes
#' underneath you -- a fresh FFmpeg install, a new GPU driver, a different
#' binary -- is not seen until the record is discarded. There are two ways to
#' discard it:
#'
#' \itemize{
#'   \item call \code{refresh_ffmpeg_capabilities()} yourself, at any time;
#'   \item call \code{\link{set_program}} (or \code{\link{set_ffmpeg}}), which
#'     discards it for you, since pointing tidymedia at a different binary
#'     invalidates everything remembered about the old one.
#' }
#'
#' The record is per R process, and it does not travel to a worker. So unless
#' you have set \code{tidymedia.hardware_encoders} yourself, a batch running on
#' \code{W} workers asks FFmpeg \code{W} times rather than once, and
#' discarding the record in the parent does not reach them.
#'
#' Setting that option is different: the value you set is carried into each
#' worker for the duration of the call, and the worker's own value is put back
#' afterwards. A batch built under your override therefore asks FFmpeg
#' for no encoder list at all, and every worker answers as the parent would.
#'
#' \code{\link{ffmpeg_encoders}} and \code{\link{ffmpeg_codecs}} are never
#' remembered: they query FFmpeg on every call, so they always report the build
#' as it is now, whether or not this function has been called.
#'
#' @return \code{NULL}, invisibly. Called for its side effect.
#' @seealso \code{\link{has_hardware_encoder}} and \code{\link{hardware_encoder}} for the
#'   queries that use the remembered answer, \code{\link{ffmpeg_encoders}} for
#'   an always-fresh encoder list, and \code{\link{set_program}} to point
#'   tidymedia at a different binary.
#' @family capability functions
#' @examples
#' # After installing FFmpeg or an NVIDIA driver mid-session:
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
