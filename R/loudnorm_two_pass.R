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

# classify_loudnorm_output() ---------------------------------------------------

# Classify an analysis-pass stderr capture (a character vector, one line per
# element) into one of three outcomes:
#   "ok"          -- a full, finite JSON measurement block; the five measured
#                    values are returned in `measured`.
#   "silent"      -- digital silence: FFmpeg reports `input_i = "-inf"`. This is
#                    a recognized outcome, not a parse failure (M18).
#   "unparseable" -- the block is absent, incomplete, or non-finite for a
#                    non-silence reason; the offending keys are in `bad`.
# A small regex per key avoids a JSON dependency (D011 spirit); each key sits on
# its own line. Shared by parse_loudnorm_measurements() (scalar) and
# assemble_measured() (batch) so silence detection lives once. Maps FFmpeg's
# analysis keys onto the correction params: input_i/tp/lra/thresh ->
# measured_I/TP/LRA/thresh and target_offset -> offset.
classify_loudnorm_output <- function(output) {
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
  # Digital silence: integrated loudness measured as -inf (input_i = "-inf").
  # Keyed on input_i alone -- the canonical silence marker. Near-silence yields
  # a finite (if very negative) input_i and is handled as a normal measurement.
  # is.infinite(NA) is FALSE, so a missing input_i falls through to unparseable.
  if (isTRUE(is.infinite(num[["i"]]) && num[["i"]] < 0)) {
    return(list(status = "silent"))
  }
  bad <- is.na(num) | !is.finite(num)
  if (any(bad)) {
    return(list(status = "unparseable", bad = names(keys)[bad]))
  }
  list(status = "ok",
       measured = list(i = num[["i"]], tp = num[["tp"]], lra = num[["lra"]],
                       thresh = num[["thresh"]], offset = num[["offset"]]))
}

# parse_loudnorm_measurements() ------------------------------------------------

# Scalar-facing wrapper over classify_loudnorm_output(): return the five measured
# values, or abort. Silence gets its own clear message (distinct from the generic
# parse failure); any other missing/non-finite block keeps the "could not parse"
# abort -- there is no correction to build without a full, finite measurement.
parse_loudnorm_measurements <- function(output, call = rlang::caller_env()) {
  cls <- classify_loudnorm_output(output)
  if (identical(cls$status, "silent")) {
    cli::cli_abort(c(
      "The input appears to be silent.",
      "x" = "FFmpeg's {.code loudnorm} analysis measured the integrated \\
             loudness as {.val -inf} (digital silence).",
      "i" = "Loudness normalization to a target is undefined for silent \\
             audio; check the input."
    ), call = call)
  }
  if (identical(cls$status, "unparseable")) {
    cli::cli_abort(c(
      "Could not parse the {.code loudnorm} measurement from FFmpeg's output.",
      "x" = "Missing or non-finite value{?s}: {.field {cls$bad}}.",
      "i" = "The analysis pass must print a JSON measurement block \\
             ({.code print_format=json})."
    ), call = call)
  }
  cls$measured
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
  # Phase 1 runs before Phase 2's ffm_batch() furrr guard, so guard furrr here
  # too -- otherwise a furrr-less machine crashes with a raw "future_pmap not
  # found" instead of the package's friendly install prompt. The sequential-plan
  # warning stays with the Phase 2 ffm_batch() call so it still fires once.
  if (parallel) {
    rlang::check_installed("furrr", reason = "for parallel batch processing.")
  }
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
# M16's classifier per row. Returns a list of `measured` (a tibble with one row
# per input and columns measured_I/measured_TP/measured_LRA/measured_thresh/
# offset -- FFmpeg-arg spellings -- ready to bind onto the jobs table) and
# `silent` (a per-row logical). Silent rows (input_i = -inf, M18) are set aside,
# not corrected: their measured columns are NA and `silent` marks them, so the
# batch can continue-and-mark rather than abort (D011 idiom). Fail-fast is
# reserved for genuine failures: a row whose analysis pass exited non-zero
# (`status` attr) or whose stderr holds no parseable finite measurement block
# (for a non-silence reason) is collected, and the function aborts naming every
# offending row before any correction command is built.
assemble_measured <- function(outputs, call = rlang::caller_env()) {
  cls <- lapply(outputs, function(out) {
    if (!is.null(attr(out, "status"))) return(list(status = "error"))
    classify_loudnorm_output(out)
  })
  status <- vapply(cls, `[[`, character(1), "status")
  bad <- which(status %in% c("error", "unparseable"))
  if (length(bad) > 0) {
    cli::cli_abort(c(
      "The {.code loudnorm} analysis pass did not yield usable measurements.",
      "x" = "Job{cli::qty(bad)}{?s} {.val {bad}} failed to run or printed no \\
             parseable measurement block.",
      "i" = "Every row must produce a finite JSON measurement block (or be \\
             silent) before the correction pass can be built."
    ), call = call)
  }
  silent <- status == "silent"
  # Silent rows have no measured values; pull them from the classifier's
  # `measured` list for the rest and leave NA where silent.
  pick <- function(field) {
    vapply(seq_along(cls), function(i) {
      if (silent[[i]]) NA_real_ else cls[[i]]$measured[[field]]
    }, numeric(1))
  }
  list(
    silent = silent,
    measured = tibble::tibble(
      measured_I = pick("i"), measured_TP = pick("tp"),
      measured_LRA = pick("lra"), measured_thresh = pick("thresh"),
      offset = pick("offset")
    )
  )
}

# bind_two_pass_result() -------------------------------------------------------

# Reassemble the batch two-pass result in original row order. `jobs` is the full
# jobs table already augmented with the five measured columns (NA for silent
# rows); `ok_res` is the correction fan-out's result over the non-silent rows
# (its added columns -- command, success, and any verified -- in the same order
# as jobs[!silent]), or NULL when every row is silent. Silent rows are marked
# (silent = TRUE, success = FALSE, no command/output); non-silent rows carry
# ok_res's added columns threaded back into their positions. Pure (no binary),
# so the interleaving is unit-testable independent of ffmpeg. A manifest
# attribute on ok_res (opt-in provenance for the rows that ran) is carried over.
bind_two_pass_result <- function(jobs, silent, ok_res, run) {
  result <- tibble::as_tibble(jobs)
  result$silent <- silent
  if (is.null(ok_res)) {
    # Every row silent: no correction fan-out ran.
    result$command <- NA_character_
    if (run) result$success <- FALSE
    return(result)
  }
  # ok_res's columns beyond the jobs table are the run outputs to thread back;
  # silent rows get a type-matched fill (no command, not a success).
  added <- setdiff(names(ok_res), names(jobs))
  for (nm in added) {
    fill <- switch(nm, command = NA_character_, success = FALSE, NA)
    col <- rep(fill, nrow(result))
    col[!silent] <- ok_res[[nm]]
    result[[nm]] <- col
  }
  if (!is.null(attr(ok_res, "manifest"))) {
    attr(result, "manifest") <- attr(ok_res, "manifest")
  }
  result
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
