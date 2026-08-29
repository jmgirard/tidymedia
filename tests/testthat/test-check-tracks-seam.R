# M082: `tidymedia.check_tracks`, the seam that switches off D024's
# dropped-audio-track probe and the FFprobe call it costs per distinct input.


# resolve_check_tracks() (AC1) ---------------------------------------------

test_that("an unset option answers TRUE", {
  withr::local_options(list(tidymedia.check_tracks = NULL))
  expect_true(resolve_check_tracks())
})

test_that("the two legal values come back unchanged", {
  withr::local_options(list(tidymedia.check_tracks = TRUE))
  expect_true(resolve_check_tracks())
  withr::local_options(list(tidymedia.check_tracks = FALSE))
  expect_false(resolve_check_tracks())
})

test_that("a value that is not one logical is refused, naming the option", {
  # Not isTRUE(): that reads every value below as FALSE and would silently
  # REMOVE the check from a session that asked to keep it. Each case asserts
  # which refusal it got -- the option's name plus the kind of value named --
  # so one message cannot stand in for another.
  cases <- list(
    list(value = "yes",         says = 'not the string'),
    list(value = NA,            says = 'not `NA`'),
    list(value = c(TRUE, TRUE), says = 'not a logical vector'),
    list(value = 1,             says = 'not the number 1')
  )
  for (case in cases) {
    withr::local_options(list(tidymedia.check_tracks = case$value))
    msg <- tryCatch(resolve_check_tracks(), error = function(e) {
      cli::ansi_strip(conditionMessage(e))
    })
    expect_match(msg, "`tidymedia.check_tracks` must be `TRUE` or `FALSE`",
                 fixed = TRUE)
    expect_match(gsub("\n", " ", msg), case$says, fixed = TRUE)
  }
})

test_that("the refusal blames the caller's frame, not the resolver", {
  # `call` is threaded so a verb's abort says the verb, the way
  # resolve_timeout()'s does. Without it every message here would read
  # "in resolve_check_tracks()".
  withr::local_options(list(tidymedia.check_tracks = "yes"))
  outer <- function() resolve_check_tracks()
  cnd <- tryCatch(outer(), error = function(e) e)
  expect_identical(rlang::call_name(conditionCall(cnd)), "outer")
})
