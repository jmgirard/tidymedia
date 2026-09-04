# M106: no function reaches the hardware-encoder resolver without a stated
# reason it is safe to.
#
# The failure this guards is not hypothetical: emit_video_codec() itself was
# such a function until M106, and M095's own review found it by hand. A list
# would have gone stale the next time a pipeline grew a resolver call, so the
# domain is recomputed here from the namespace and the ledger is checked
# against it in both directions.

test_that("the ledger names exactly the functions that call the resolver", {
  sites <- tm_hw_encoder_sites()

  # Non-empty, and non-empty for the right reason. A domain that silently
  # emptied -- a renamed resolver, a namespace that failed to load -- would
  # make every expectation below vacuously true.
  expect_gt(length(sites), 0L)
  expect_true("emit_video_codec" %in% sites)

  ledger <- tm_hw_encoder_ledger()
  expect_setequal(sites, names(ledger))
  expect_true(all(ledger %in% c("literal", "checked-above", "emit-half")))
})

test_that("each site's own body bears out its stated disposition", {
  ledger <- tm_hw_encoder_ledger()
  for (name in names(ledger)) {
    expect_identical(
      tm_hw_encoder_disposition_holds(name, ledger[[name]]),
      TRUE,
      info = name
    )
  }
})

test_that("the disposition check can tell a safe site from an unsafe one", {
  # The discrimination check. Every expectation above is a green from a
  # function that says TRUE; these three are the same function saying no, one
  # per way a site can fail its stated reason.
  expect_type(
    tm_hw_encoder_disposition_holds("format_for_web_pipeline", "checked-above"),
    "character"
  )
  expect_type(
    tm_hw_encoder_disposition_holds("anonymize_pipeline", "literal"),
    "character"
  )
  # anonymize_pipeline() checks with check_token(), not check_video_codec(), so
  # it fails the stricter emit-half reading -- which is what makes emit-half a
  # narrower claim than checked-above rather than a synonym for it.
  expect_type(
    tm_hw_encoder_disposition_holds("anonymize_pipeline", "emit-half"),
    "character"
  )
})
