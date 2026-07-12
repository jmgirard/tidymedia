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

# run_loudnorm_analysis() ------------------------------------------------------

# Execute the analysis pass and return the parsed measured values. This is the
# analyze step of the analyze-then-build orchestrator: it always touches the
# binary (unlike ffm_compile(), which stays pure), so it lives here beside
# ffm_run() rather than inside compilation. stderr is captured (merged into the
# returned vector by run_program(stderr = TRUE)); a non-zero FFmpeg exit aborts
# before any correction is attempted.
run_loudnorm_analysis <- function(input,
                                  target_loudness = -23,
                                  true_peak = -1,
                                  loudness_range = 7,
                                  call = rlang::caller_env()) {
  p <- loudnorm_analysis_pipeline(input, target_loudness, true_peak,
                                  loudness_range)
  out <- run_program(find_ffmpeg(), ffm_args(p), program = "FFmpeg",
                     input = "", stderr = TRUE, call = call)
  status <- attr(out, "status")
  if (!is.null(status)) {
    cli::cli_abort(c(
      "The {.code loudnorm} analysis pass failed \\
       (FFmpeg exited with status {status}).",
      "i" = "The failing command was: \\
             {.code ffmpeg {ffm_compile(p)}}"
    ), call = call)
  }
  parse_loudnorm_measurements(out, call = call)
}

# run_loudnorm_analysis_batch() ------------------------------------------------

# Phase 1 of normalize_audios(two_pass = TRUE): run M16's analysis pass once per
# input row, honoring per-row loudness targets, and return the list of captured
# stderr outputs (one character vector per row, carrying run_program()'s `status`
# attr on a non-zero FFmpeg exit). Parsing and row-level fail-fast are deferred
# to assemble_measured(), which can name the offending row; running and parsing
# are split here so a bad row is reported by index rather than aborting anonymously
# mid-fan-out. Parallelism follows the active future plan; the sequential-plan
# warning is left to the Phase 2 ffm_batch() call so it fires exactly once.
run_loudnorm_analysis_batch <- function(inputs, target_loudness, true_peak,
                                        loudness_range, parallel) {
  analyze_one <- function(input, target_loudness, true_peak, loudness_range) {
    p <- loudnorm_analysis_pipeline(input, target_loudness, true_peak,
                                    loudness_range)
    run_program(find_ffmpeg(), ffm_args(p), program = "FFmpeg", input = "",
                stderr = TRUE)
  }
  args <- list(input = inputs, target_loudness = target_loudness,
               true_peak = true_peak, loudness_range = loudness_range)
  if (parallel) {
    furrr::future_pmap(args, analyze_one)
  } else {
    purrr::pmap(args, analyze_one)
  }
}

# assemble_measured() ----------------------------------------------------------

# Map Phase 1's per-row analysis outputs to the five measured columns, reusing
# M16's parser per row. Returns a tibble with one row per input and columns
# measured_I/measured_TP/measured_LRA/measured_thresh/offset (FFmpeg-arg
# spellings) ready to bind onto the jobs table. Fail-fast: a row whose analysis
# pass exited non-zero (`status` attr) or whose stderr holds no parseable finite
# measurement block is collected, and the function aborts naming every offending
# row before any correction command is built.
assemble_measured <- function(outputs, call = rlang::caller_env()) {
  parsed <- lapply(outputs, function(out) {
    if (!is.null(attr(out, "status"))) return(NULL)
    tryCatch(parse_loudnorm_measurements(out), error = function(e) NULL)
  })
  bad <- which(vapply(parsed, is.null, logical(1)))
  if (length(bad) > 0) {
    cli::cli_abort(c(
      "The {.code loudnorm} analysis pass did not yield usable measurements.",
      "x" = "Job{cli::qty(bad)}{?s} {.val {bad}} failed to run or printed no \\
             parseable measurement block.",
      "i" = "Every row must produce a finite JSON measurement block before the \\
             correction pass can be built."
    ), call = call)
  }
  tibble::tibble(
    measured_I = vapply(parsed, `[[`, numeric(1), "i"),
    measured_TP = vapply(parsed, `[[`, numeric(1), "tp"),
    measured_LRA = vapply(parsed, `[[`, numeric(1), "lra"),
    measured_thresh = vapply(parsed, `[[`, numeric(1), "thresh"),
    offset = vapply(parsed, `[[`, numeric(1), "offset")
  )
}

# run_normalize_correction() ---------------------------------------------------

# Phase 2 of normalize_audios(two_pass = TRUE): build (and optionally run) one
# linear correction command per row of a jobs table already augmented with the
# five measured columns (measured_I/TP/LRA/thresh/offset) by Phase 1. A thin
# fan-out over ffm_batch() (D007) sharing normalize_audio_pipeline() with the
# scalar/single-pass paths, so channels/sample_rate/-codec:v copy and the
# per-value validation are inherited by construction. The measured columns arrive
# via `...` (pmap-style) and thread back as the `measured` list, switching each
# row to linear normalization; a per-row knob column overrides the scalar arg of
# the same name, exactly as the single-pass builder does. `...` also forwards
# ffm_batch options (verify/manifest/...) to the runner.
run_normalize_correction <- function(jobs, target_loudness, true_peak,
                                     loudness_range, channels, sample_rate,
                                     run, parallel, ...) {
  ffm_batch(
    jobs,
    function(input, output, ...) {
      dots <- list(...)
      pick <- function(nm, default) if (nm %in% names(dots)) dots[[nm]] else default
      normalize_audio_pipeline(
        input, output,
        target_loudness = pick("target_loudness", target_loudness),
        true_peak = pick("true_peak", true_peak),
        loudness_range = pick("loudness_range", loudness_range),
        channels = pick("channels", channels),
        sample_rate = pick("sample_rate", sample_rate),
        measured = list(
          i = dots[["measured_I"]], tp = dots[["measured_TP"]],
          lra = dots[["measured_LRA"]], thresh = dots[["measured_thresh"]],
          offset = dots[["offset"]]
        )
      )
    },
    run = run,
    parallel = parallel,
    ...
  )
}
