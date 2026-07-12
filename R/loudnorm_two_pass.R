# Two-pass loudnorm internals (M16) --------------------------------------------
#
# The analyze-then-build execution pattern behind normalize_audio(two_pass =
# TRUE): an analysis pass measures the input, its stderr is parsed for the
# measured values, and those drive a linear correction pass. All command
# assembly stays in Layer 1 (D002); these helpers only build/parse and are
# never exported.

# loudnorm_analysis_pipeline() -------------------------------------------------

# Build the analysis-pass pipeline: bare loudnorm with the target values and
# `print_format=json`, discarding output via `-f null -`. This measures the
# input as-is (FFmpeg's canonical two-pass recipe) -- no downmix/resample, which
# belong to the correction/output stage. The measured values are read back from
# this pass's stderr by parse_loudnorm_measurements().
loudnorm_analysis_pipeline <- function(input,
                                       target_loudness = -23,
                                       true_peak = -1,
                                       loudness_range = 7) {
  p <- ffm_files(input, "-")
  p <- ffm_loudnorm(p, target_loudness = target_loudness, true_peak = true_peak,
                    loudness_range = loudness_range, print_format = "json")
  ffm_output_options(p, "-f null")
}

# parse_loudnorm_measurements() ------------------------------------------------

# Extract the five measured values from a loudnorm `print_format=json` block in
# FFmpeg's captured stderr (a character vector, one line per element). A small
# regex per key avoids a JSON dependency (D011 spirit); each key sits on its own
# line in the block. Maps FFmpeg's analysis keys onto the correction params:
# input_i/tp/lra/thresh -> measured_I/TP/LRA/thresh and target_offset -> offset.
# Aborts cleanly when the block is absent, incomplete, or non-numeric -- there is
# no correction to build without a full, finite measurement.
parse_loudnorm_measurements <- function(output, call = rlang::caller_env()) {
  keys <- c(i = "input_i", tp = "input_tp", lra = "input_lra",
            thresh = "input_thresh", offset = "target_offset")
  extract_one <- function(json_key) {
    pat <- sprintf('"%s"[[:space:]]*:[[:space:]]*"([^"]*)"', json_key)
    hit <- grep(pat, output, value = TRUE, perl = TRUE)
    if (length(hit) == 0) return(NA_character_)
    sub(sprintf(".*%s.*", pat), "\\1", hit[[1]], perl = TRUE)
  }
  raw <- vapply(keys, extract_one, character(1))
  num <- suppressWarnings(as.numeric(raw))
  names(num) <- names(keys)
  bad <- is.na(num) | !is.finite(num)
  if (any(bad)) {
    cli::cli_abort(c(
      "Could not parse the {.code loudnorm} measurement from FFmpeg's output.",
      "x" = "Missing or non-finite value{?s}: {.field {names(keys)[bad]}}.",
      "i" = "The analysis pass must print a JSON measurement block \\
             ({.code print_format=json})."
    ), call = call)
  }
  list(i = num[["i"]], tp = num[["tp"]], lra = num[["lra"]],
       thresh = num[["thresh"]], offset = num[["offset"]])
}
