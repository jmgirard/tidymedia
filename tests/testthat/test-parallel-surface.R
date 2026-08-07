# Pins WHICH exported functions take `parallel`, so the vignette prose that
# enumerates them cannot rot silently (M55).
#
# `vignettes/batch.Rmd` states the rule in words — every `*_batch` verb,
# `segment_video()`, and the five `probe_*()` readers — and prose has no way to
# notice an export gaining or losing the argument. This test reads the set off
# the namespace rather than off anyone's memory: an export that gains `parallel`
# fails here, and so does one that loses it, either way pointing at the prose
# that now needs rewriting.
#
# The function filter is load-bearing, not defensive: `.data` is exported and is
# not a function, and `formals()` warns on a non-function.
#
# The lookups below inherit deliberately. A re-export is listed in NAMESPACE but
# bound in the imports env rather than the namespace itself, so `.data` is not
# found at all under `inherits = FALSE`; inheriting resolves it. Nothing is
# resolved spuriously, since every name here comes from the export list.

# Verified against the namespace at M55 (2026-08-07). Grouped as the vignette
# groups them, so a failure reads as "which group changed".
expected_parallel <- sort(c(
  # `ffm_batch()` and the fifteen `*_batch` verbs built on it.
  "ffm_batch",
  "anonymize_video_batch", "compare_videos_batch", "concatenate_videos_batch",
  "convert_audio_batch", "crop_video_batch", "extract_audio_batch",
  "extract_frame_batch", "format_for_web_batch", "normalize_audio_batch",
  "picture_in_picture_batch", "sample_frames_batch", "segment_video_batch",
  "separate_audio_video_batch", "standardize_video_batch",
  "strip_metadata_batch",
  # The metadata readers, which fan out per file.
  "probe_all", "probe_audio", "probe_container", "probe_streams", "probe_video",
  # The one scalar verb that fans out — over its own segments.
  "segment_video"
))

test_that("the set of exports taking `parallel` is the set the vignettes claim", {
  ns <- asNamespace("tidymedia")
  exports <- getNamespaceExports("tidymedia")

  objs <- lapply(exports, get, envir = ns)
  names(objs) <- exports
  fns <- objs[vapply(objs, is.function, logical(1))]

  # A floor, not a count: it fails if the enumeration collapses (an empty
  # namespace, a filter that drops everything) and reports a vacuous pass.
  expect_gte(length(fns), length(expected_parallel))

  takes <- vapply(fns, function(f) "parallel" %in% names(formals(f)), logical(1))
  expect_identical(sort(names(fns)[takes]), expected_parallel)
})

test_that("`.data` is why the function filter is needed", {
  ns <- asNamespace("tidymedia")
  skip_if_not(".data" %in% getNamespaceExports("tidymedia"))

  # Pins the premise of the filter above: were it dropped, this export would
  # reach formals() and warn.
  expect_false(is.function(get(".data", envir = ns)))
})
