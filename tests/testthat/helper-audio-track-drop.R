# Collect EVERY tidymedia_dropped_audio condition an expression signals, and the
# expression's outcome. tryCatch(warning = ) stops at the first one, which
# cannot tell one warning from two -- and "exactly one" is what M075's criteria
# and M082's unchanged-default criterion both promise. Only our own class is
# muffled, so an unrelated warning still surfaces the way it would without this
# handler. `value` is the expression's result, or the error condition if it
# threw: the ordering tests need both halves of a call that warns and then
# fails.
#
# Shared by test-audio-track-drop.R and test-check-tracks-seam.R (M082); it was
# a file-local copy in the former until the seam's tests needed the same count.
catch_drop <- function(expr) {
  caught <- list()
  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        if (inherits(w, "tidymedia_dropped_audio")) {
          caught[[length(caught) + 1L]] <<- w
          invokeRestart("muffleWarning")
        }
      }
    ),
    error = function(e) e
  )
  list(warnings = caught, value = value)
}
