# The codec-argument family: which verbs carry a `video_codec` / `audio_codec`
# argument that SETS a codec, and what else a call to each one needs.
#
# Two test files sweep this family and would drift if each kept its own list:
# test-codec-arg-front-door.R asserts what each argument REFUSES (M41), and
# test-codec-null-na-semantics.R asserts what `NULL` and a column `NA` MEAN
# (M42/D022). Both also assert the list is complete against the package's
# exports, so a verb that gains a codec argument later joins the sweep or fails.
#
# `verify_media()` is excluded by design: its same-named arguments are expected
# probe VALUES, not codec settings, so sweeping them would test a different
# contract. The exclusion is asserted, not assumed -- see the completeness tests.
codec_family_pairs <- function() {
  v <- c("video_codec", "audio_codec")
  a <- "audio_codec"
  list(
    list(verb = "anonymize_video",            args = v),
    list(verb = "anonymize_video_batch",      args = v),
    list(verb = "compare_videos",             args = v),
    list(verb = "compare_videos_batch",       args = v),
    list(verb = "convert_audio",              args = a),
    list(verb = "convert_audio_batch",        args = a),
    list(verb = "crop_video",                 args = v),
    list(verb = "crop_video_batch",           args = v),
    list(verb = "extract_audio",              args = a),
    list(verb = "extract_audio_batch",        args = a),
    list(verb = "normalize_audio",            args = a),
    list(verb = "normalize_audio_batch",      args = a),
    list(verb = "picture_in_picture",         args = v),
    list(verb = "picture_in_picture_batch",   args = v),
    list(verb = "segment_video",              args = v),
    list(verb = "segment_video_batch",        args = v),
    list(verb = "separate_audio_video",       args = v),
    list(verb = "separate_audio_video_batch", args = v),
    list(verb = "standardize_video",          args = v),
    list(verb = "standardize_video_batch",    args = v)
  )
}

# Arguments each verb needs besides the codec under test, using `input` as the
# input path and `out` as an output stem. Mirrors data-raw/codec-guard-baseline.R,
# which measures the same grid against a git ref.
codec_family_call <- function(verb, input, out) {
  regions <- data.frame(x = 0, y = 0, width = 32, height = 32)
  switch(
    verb,
    anonymize_video            = list(infile = input, outfile = out,
                                      regions = regions),
    anonymize_video_batch      = list(jobs = tibble::tibble(
                                        input = input, output = out,
                                        regions = list(regions))),
    compare_videos             = list(infiles = c(input, input), outfile = out),
    compare_videos_batch       = list(jobs = tibble::tibble(
                                        inputs = list(c(input, input)),
                                        output = out)),
    convert_audio              = list(infile = input, outfile = "a.mp3"),
    convert_audio_batch        = list(jobs = tibble::tibble(
                                        input = input, output = "a.mp3")),
    crop_video                 = list(infile = input, outfile = out,
                                      width = 32, height = 32),
    crop_video_batch           = list(jobs = tibble::tibble(
                                        input = input, output = out),
                                      width = 32, height = 32),
    extract_audio              = list(infile = input, outfile = "a.aac"),
    extract_audio_batch        = list(jobs = tibble::tibble(
                                        input = input, output = "a.aac")),
    normalize_audio            = list(infile = input, outfile = out),
    normalize_audio_batch      = list(jobs = tibble::tibble(
                                        input = input, output = out)),
    picture_in_picture         = list(main = input, overlay = input,
                                      outfile = out),
    # Named main/overlay columns (D015), not an `inputs` list-column: with the
    # wrong shape every call aborts on the missing columns before reaching the
    # codec argument these files exist to test (M41 review A1).
    picture_in_picture_batch   = list(jobs = tibble::tibble(
                                        main = input, overlay = input,
                                        output = out)),
    segment_video              = list(infile = input, start = 0, end = 1,
                                      outfiles = out),
    segment_video_batch        = list(jobs = tibble::tibble(
                                        input = input, start = 0, end = 1,
                                        output = out)),
    separate_audio_video       = list(infile = input, audiofile = "a.aac",
                                      videofile = out),
    separate_audio_video_batch = list(jobs = tibble::tibble(
                                        input = input, audiofile = "a.aac",
                                        videofile = out)),
    standardize_video          = list(infile = input, outfile = out),
    standardize_video_batch    = list(jobs = tibble::tibble(
                                        input = input, output = out)),
    stop("no call template for ", verb)
  )
}

# A valid codec for a `col = "present"` run. It must be a value the per-row
# column guards accept, so the column genuinely wins the verb's internal
# `pick()` and the scalar argument is the only thing under test: "copy" is
# refused outright by several verbs, and NA is the column form of the NULL
# sentinel, so neither would isolate the scalar.
codec_family_col_value <- function(arg) {
  if (arg == "video_codec") "libx264" else "aac"
}

# What else a call needs before an `audio_codec` assertion measures anything on
# the two fan-in composites. They map no audio unless `audio` names an input
# index (D009), and D017 refuses an audio encoder with no audio mapped -- so
# without this the audio_codec cells record an unrelated abort at every value,
# including the default (M41 review A2). Empty for every other verb.
codec_family_extra <- function(verb, arg) {
  fanin <- c("compare_videos", "compare_videos_batch",
             "picture_in_picture", "picture_in_picture_batch")
  if (arg == "audio_codec" && verb %in% fanin) list(audio_input = 0) else list()
}
