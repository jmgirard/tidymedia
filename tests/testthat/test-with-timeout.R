# M072: one call carries its own limit.
#
# The session-wide option (M69/D047) answers "how long may anything take in
# this session"; `with_timeout()` answers "how long may THIS take", without
# disturbing the answer to the first. Every test below therefore states what
# the session option was before and after, because the second half of the
# promise is that the call leaves it exactly as it found it.

# The seeds a per-call limit has to reach -------------------------------------
#
# `tm_timeout_domain()` (M70) is the set of exports a timeout can be seen
# THROUGH; this is the smaller set where a process is actually started, and it
# is the one a limit has to arrive at. Recorded so a new spawn site reddens it:
# a fifth site added without carrying the limit is exactly the silent hang the
# option seam was built to stop.

test_that("the package starts a process at exactly the four recorded sites", {
  expect_setequal(
    tm_spawn_sites(),
    c("ffmpeg", "ffprobe", "mediainfo", "run_program")
  )
})

test_that("spawn-site membership is read off the body, not off the record", {
  # An empty seed set must collapse the result. Without this the assertion
  # above could be satisfied by a helper that returned its own recorded list.
  expect_equal(tm_spawn_sites(seeds = character(0)), character(0))
  # And a seed no body names adds nobody.
  expect_equal(tm_spawn_sites(seeds = "no_such_primitive"), character(0))
})
