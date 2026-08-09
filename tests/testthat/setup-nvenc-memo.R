# M67: the encoder-pool memo is session state, and a test suite is one session.
# Without this, the first file to warm the memo silently changes what every
# later file measures -- a probe-counting test in file B would read zero probes
# because file A had already asked.
#
# testthat's state inspector is the only per-test hook it offers: it runs
# immediately before and immediately after each `test_that()` block. Returning
# NULL every time means it reports no state change of its own, so it resets
# without also asserting.
testthat::set_state_inspector(function() {
  forget_ffmpeg_capabilities()
  NULL
})

withr::defer(testthat::set_state_inspector(NULL), teardown_env())
