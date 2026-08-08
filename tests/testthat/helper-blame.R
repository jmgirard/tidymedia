# Blame-reading helpers shared by the front-door guard suites (M58's
# contradictions, M59's value checks, M61's ordering between the two).
#
# One definition, for the M40 reason: these were written twice, identically, in
# test-contradiction-front-door.R and test-value-check-front-door.R, and a third
# copy is how the two stop agreeing. A helper file is loaded before every test
# file, so all three suites read the same implementation.

# The function part of conditionCall() -- what the console prints as
# "Error in `<verb>()`", and the whole point of a front-door guard.
blamed_verb <- function(cnd) {
  cl <- conditionCall(cnd)
  if (is.null(cl)) return(NA_character_)
  paste(deparse(cl[[1]]), collapse = "")
}

# Call a verb BY NAME and return the condition rather than raising it. By name
# because do.call() on a function object records the anonymous function as the
# condition call and hides the blame target these suites exist to watch.
catch_call <- function(verb, args) {
  # Only DEFAULT `run`, never override it: some cases pass a malformed `run` on
  # purpose, and clobbering it would leave those asserting the absence of an
  # error the call no longer had.
  if (is.null(args$run)) args$run <- FALSE
  tryCatch(
    do.call(verb, args, envir = asNamespace("tidymedia")),
    error = function(e) e
  )
}
