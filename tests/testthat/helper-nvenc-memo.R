# Counting stub for the encoder probe (M67).
#
# Counts at ffmpeg_encoders() rather than at find_ffmpeg(): ffmpeg() shells out
# through system(), not run_program(), so ffmpeg_encoders() is the seam where
# the binary would actually be consulted (the test-nvenc-docs.R pattern).
#
# Starts the caller's test from a cold memo, unsets the option seam so the probe
# is forced, and discards the memo again on exit. Returns a zero-argument
# function reading the cumulative count.
local_encoder_probe_counter <- function(
    names = c("h264_nvenc", "hevc_nvenc", "av1_nvenc"),
    env = parent.frame()) {
  count <- 0L
  local_mocked_bindings(
    ffmpeg_encoders = function(...) {
      count <<- count + 1L
      tibble::tibble(name = names)
    },
    .env = env
  )
  withr::local_options(tidymedia.hardware_encoders = NULL, .local_envir = env)
  forget_ffmpeg_capabilities()
  withr::defer(forget_ffmpeg_capabilities(), envir = env)
  function() count
}

# The nvenc grid (M67 AC2/AC3) ------------------------------------------------

# H: every exported VERB taking a `hardware` argument. Read from the namespace
# at test time, never hand-listed -- a seventeenth verb gaining `hardware`
# joins the grid without anyone remembering to add it.
#
# The two capability helpers are excluded by name (M100). They grew a
# `hardware` argument too, so the enumeration catches them, but they were never
# in this domain: the grid asserts one probe per cell (hardware_encoder() is
# pure and probes zero times), the AC1 sweep reads each member's accepted set
# off a `hardware` default the helpers deliberately do not have, and the
# probe-blame sweep crosses members with wrong `codec` forms. The helpers are
# tested on their own terms in test-nvenc.R.
nvenc_hardware_helpers <- function() {
  c("hardware_encoder", "has_hardware_encoder")
}

nvenc_hardware_exports <- function() {
  ns <- asNamespace("tidymedia")
  sort(setdiff(
    Filter(
      function(nm) {
        obj <- get(nm, envir = ns)
        is.function(obj) && "hardware" %in% names(formals(obj))
      },
      getNamespaceExports("tidymedia")
    ),
    nvenc_hardware_helpers()
  ))
}

# Which of a function's formals the caller MUST supply.
nvenc_required_formals <- function(fmls) {
  names(fmls)[vapply(fmls, function(d) identical(d, quote(expr = )), logical(1))]
}

# Two boxes, one for the pipeline to blur and one to keep the region argument
# from being the reason a cell aborts.
nvenc_grid_regions <- function() {
  tibble::tibble(x = 0, y = 0, width = 16, height = 16)
}

# nvenc_grid_args(): build one verb's `hardware = "nvenc"` call from its OWN
# formals, so no per-verb call is hand-written here.
#
# Scalar verbs: every formal with no default gets a value from the name-keyed
# table below. Batch verbs: the same required set is read off the SCALAR
# SIBLING's formals and split -- an argument the batch verb carries itself
# (crop's width/height) is passed as an argument, and everything else becomes a
# `jobs` column. Outputs are distinct per row so the duplicate-output guard is
# never what a cell measures.
nvenc_grid_args <- function(fname, input, rows = 3L) {
  ns <- asNamespace("tidymedia")
  fmls <- formals(get(fname, envir = ns))

  scalar_value <- list(
    infile = input,
    infiles = c(input, input),
    main = input,
    overlay = input,
    outfile = "out.mp4",
    outfiles = "out.mp4",
    audiofile = "out.m4a",
    videofile = "outv.mp4",
    regions = nvenc_grid_regions(),
    width = 32,
    height = 32,
    start = 0,
    end = 1
  )
  column_of <- c(
    infile = "input", infiles = "inputs", main = "main", overlay = "overlay",
    outfile = "output", outfiles = "output", audiofile = "audiofile",
    videofile = "videofile", regions = "regions", start = "start", end = "end"
  )
  column_value <- list(
    input = rep(input, rows),
    inputs = rep(list(c(input, input)), rows),
    main = rep(input, rows),
    overlay = rep(input, rows),
    output = sprintf("out%d.mp4", seq_len(rows)),
    audiofile = sprintf("out%d.m4a", seq_len(rows)),
    videofile = sprintf("outv%d.mp4", seq_len(rows)),
    regions = rep(list(nvenc_grid_regions()), rows),
    start = rep(0, rows),
    end = rep(1, rows)
  )

  args <- list()
  if ("jobs" %in% names(fmls)) {
    sf <- formals(get(sub("_batch$", "", fname), envir = ns))
    needed <- nvenc_required_formals(sf)
    cols <- unname(column_of[setdiff(needed, names(fmls))])
    # A derived-output verb (segment's `outfiles` defaults to NULL) still needs
    # distinct outputs, or three identical rows collide before anything probes.
    if (any(c("outfile", "outfiles") %in% names(sf))) cols <- union(cols, "output")
    args$jobs <- tibble::as_tibble(column_value[cols])
    for (a in intersect(needed, names(fmls))) args[[a]] <- scalar_value[[a]]
  } else {
    for (a in nvenc_required_formals(fmls)) args[[a]] <- scalar_value[[a]]
  }

  # A `"copy"` default aborts under nvenc on the codec conflict, never reaching
  # the probe (test-nvenc-docs.R's stream-copy exception), so those verbs are
  # asked to re-encode.
  if ("video_codec" %in% names(fmls) && identical(fmls$video_codec, "copy")) {
    args$video_codec <- "libx264"
  }
  if ("parallel" %in% names(fmls)) args$parallel <- FALSE
  if ("run" %in% names(fmls)) args$run <- FALSE
  args$hardware <- "nvenc"
  args
}
