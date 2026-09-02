# The backend vocabulary: two backends, one per-backend codec-family table, and
# one availability probe reached by two routes.
#
# Everything below the executing test at the bottom is binary-free: availability
# is simulated, so a runner with neither an NVIDIA GPU nor a Mac still decides
# every compile, refusal and fallback assertion here.

# A software codec name per family, so the table can be iterated without
# hand-writing a call per (family, backend) cell.
hw_software_codec <- function(family) {
  c(h264 = "libx264", hevc = "libx265", av1 = "libaom-av1",
    prores = "prores")[[family]]
}

# AC1 -- the vocabulary, over the domain the suite computes ---------------------
#
# The sweep AC1 names. The domain is `nvenc_hardware_exports()` -- every export
# carrying a `hardware` formal, less the two capability helpers, whose
# `hardware` is a different argument over a narrower set (RR07 R8) -- so a
# seventeenth verb gaining the argument joins this sweep by existing.
#
# Each verb's ACCEPTED set is its `hardware` formal's own default, and that is
# true only because every one of them calls bare `rlang::arg_match(hardware)`
# with no `values=`. Both halves are asserted: a verb that supplied its own
# vocabulary would carry the widened default and still refuse the backend, and
# the default alone would not catch it.

# Every `arg_match()` call in a body whose first argument is the symbol
# `hardware`, returned as calls so the caller can count them and read their
# arguments.
hw_arg_match_calls <- function(fn) {
  found <- list()
  walk <- function(node) {
    if (is.call(node)) {
      fun <- node[[1]]
      nm <- if (is.name(fun)) as.character(fun) else if (
        is.call(fun) && identical(as.character(fun[[1]]), "::")
      ) as.character(fun[[3]]) else ""
      if (identical(nm, "arg_match") && length(node) >= 2L &&
          identical(node[[2]], quote(hardware))) {
        found[[length(found) + 1L]] <<- node
      }
      # `for (el in as.list(node))` rather than indexing: a missing argument in
      # a call (`x[i, ]`) is the empty symbol, which is not a call and so ends
      # the recursion on its own.
      for (el in as.list(node)) walk(el)
    }
  }
  walk(body(fn))
  found
}

test_that("every export carrying `hardware` accepts both backends, and nothing else decides that", {
  members <- nvenc_hardware_exports()
  # Non-empty and the size the criterion names: a domain that silently emptied
  # would make every expectation below vacuously true.
  expect_equal(length(members), 16L)

  ns <- asNamespace("tidymedia")
  # The resolver is swept with them. Widening only the exported verbs leaves
  # every `hardware = "videotoolbox"` call aborting inside it, so it carries the
  # same default and is checked the same way.
  for (nm in c(members, "resolve_hw_encoder")) {
    fn <- get(nm, envir = ns)
    default <- eval(formals(fn)$hardware)
    expect_equal(default, c("none", "nvenc", "videotoolbox"), info = nm)
    # "none" FIRST, not merely present: `arg_match()` takes the first element
    # when the argument is left at its default, so a reordering would make GPU
    # encoding the default at all 16 verbs.
    expect_identical(default[[1]], "none", info = nm)

    calls <- hw_arg_match_calls(fn)
    expect_length(calls, 1L)
    # Bare: one argument and no `values=`, so the formal's default above IS the
    # accepted set. A verb naming its own vocabulary fails here.
    expect_length(calls[[1]], 2L)
    expect_null(names(calls[[1]]))
  }
})

test_that("the vocabularies spelled literally match the tables they mirror", {
  # The verbs, the resolver and the two capability helpers spell their defaults
  # out rather than calling the table, because an Rd usage line publishes a
  # default verbatim and a reader cannot evaluate an unexported call there. That
  # buys a drift risk, and this is the check that pays for it.
  expect_equal(
    setdiff(eval(formals(tidymedia:::resolve_hw_encoder)$hardware), "none"),
    tidymedia:::hardware_backends()
  )
  for (nm in nvenc_hardware_helpers()) {
    expect_equal(eval(formals(get(nm, envir = asNamespace("tidymedia")))$codec),
                 tidymedia:::hardware_codec_families(), info = nm)
  }
})

# AC2 -- the table decides what is emitted --------------------------------------

test_that("each backend compiles its own encoder for every family it declares", {
  table <- tidymedia:::hardware_backend_families()
  # Iterated, never hand-listed: a family added to a table without a builder
  # case fails here rather than shipping.
  for (backend in names(table)) {
    for (family in table[[backend]]) {
      encoder <- paste0(family, "_", backend)
      withr::local_options(tidymedia.hardware_encoders = encoder)
      f <- make_input()
      cmd <- standardize_video(f, "out.mp4",
                               video_codec = hw_software_codec(family),
                               hardware = backend, run = FALSE)
      expect_match(cmd, paste0("-codec:v ", encoder), fixed = TRUE,
                   info = encoder)
      expect_no_match(cmd, hw_software_codec(family), fixed = TRUE,
                      info = encoder)
    }
  }
})

test_that("both backends declare the two families they share", {
  # The iteration above is only as strong as the table it reads, so the two
  # families both backends cover are asserted by name as well.
  table <- tidymedia:::hardware_backend_families()
  expect_true(all(c("h264", "hevc") %in% table$nvenc))
  expect_true(all(c("h264", "hevc") %in% table$videotoolbox))
})

test_that("a family outside a backend's table is refused at the verb", {
  # Each refusal names the backend the caller asked for and the family, and
  # neither names the other backend.
  withr::local_options(
    tidymedia.hardware_encoders = c("h264_nvenc", "hevc_nvenc", "av1_nvenc",
                                    "h264_videotoolbox", "hevc_videotoolbox")
  )
  f <- make_input()

  vt <- rlang::catch_cnd(
    standardize_video(f, "out.mp4", video_codec = "libaom-av1",
                      hardware = "videotoolbox", run = FALSE)
  )
  expect_s3_class(vt, "rlang_error")
  expect_match(conditionMessage(vt), "videotoolbox", fixed = TRUE)
  expect_match(conditionMessage(vt), "av1", fixed = TRUE)
  expect_no_match(conditionMessage(vt), "nvenc", fixed = TRUE)
  # "at the verb" is the claim in this test's name, so the blamed frame is
  # asserted and not only the message: at master this class of refusal came
  # from `codec_family(video_codec, call = call)` and named the verb, and a
  # refusal that named `hardware_encoder()` instead would be M094 F2 / D074's
  # defect one layer down.
  expect_match(rlang::expr_deparse(conditionCall(vt))[[1]],
               "^standardize_video\\(")

  nv <- rlang::catch_cnd(
    standardize_video(f, "out.mp4", video_codec = "prores",
                      hardware = "nvenc", run = FALSE)
  )
  expect_s3_class(nv, "rlang_error")
  expect_match(conditionMessage(nv), "nvenc", fixed = TRUE)
  expect_match(conditionMessage(nv), "prores", fixed = TRUE)
  expect_no_match(conditionMessage(nv), "videotoolbox", fixed = TRUE)
  expect_match(rlang::expr_deparse(conditionCall(nv))[[1]],
               "^standardize_video\\(")
})

test_that("the exported predicate is blamed for its own out-of-table pair", {
  # The refusal reaches the predicate too, and there it must name the predicate:
  # the mapper's frame has neither `video_codec` nor `fallback`, so blaming it
  # would point a reader at arguments the call does not have. `@return` says
  # this pair raises rather than returning FALSE.
  cnd <- rlang::catch_cnd(has_hardware_encoder("av1", "videotoolbox"))
  expect_s3_class(cnd, "rlang_error")
  expect_match(conditionMessage(cnd), "videotoolbox", fixed = TRUE)
  expect_match(rlang::expr_deparse(conditionCall(cnd))[[1]],
               "^has_hardware_encoder\\(")
})

# AC3 -- the two routes to the probe's answer -----------------------------------
#
# The option seam is read before the session memo, so a test that mocks the memo
# without unsetting the option measures the option (D044). The two routes are
# asserted separately for that reason, and because the option is the seam
# carried into `parallel = TRUE` workers.

# One (backend available, backend missing) pair, against whatever pool the
# calling test has already put in place.
hw_expect_pool_decides <- function(available, missing) {
  f <- make_input()
  cmd <- standardize_video(f, "out.mp4", hardware = available, run = FALSE)
  expect_match(cmd, paste0("-codec:v h264_", available), fixed = TRUE,
               info = available)

  cnd <- rlang::catch_cnd(
    standardize_video(f, "out.mp4", hardware = missing, run = FALSE)
  )
  expect_s3_class(cnd, "rlang_error")
  expect_match(conditionMessage(cnd), paste0("h264_", missing), fixed = TRUE,
               info = missing)
}

test_that("the memo route decides which backend proceeds and which aborts", {
  # The option MUST be unset, or the mock below is inert: the option seam is
  # read first and the memo is only the fall-through.
  withr::local_options(tidymedia.hardware_encoders = NULL)
  forget_ffmpeg_capabilities()
  withr::defer(forget_ffmpeg_capabilities())

  local_mocked_bindings(cached_encoder_names = function() "h264_videotoolbox",
                        .package = "tidymedia")
  hw_expect_pool_decides(available = "videotoolbox", missing = "nvenc")
})

test_that("the reverse memo pool inverts both answers", {
  withr::local_options(tidymedia.hardware_encoders = NULL)
  forget_ffmpeg_capabilities()
  withr::defer(forget_ffmpeg_capabilities())

  local_mocked_bindings(cached_encoder_names = function() "h264_nvenc",
                        .package = "tidymedia")
  hw_expect_pool_decides(available = "nvenc", missing = "videotoolbox")
})

test_that("the option route decides the same outcomes with no mock", {
  withr::local_options(tidymedia.hardware_encoders = "h264_videotoolbox")
  hw_expect_pool_decides(available = "videotoolbox", missing = "nvenc")
})

test_that("the reverse option pool inverts both answers", {
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
  hw_expect_pool_decides(available = "nvenc", missing = "videotoolbox")
})

test_that("fallback re-encodes in software and names the backend it left", {
  f <- make_input()
  for (backend in tidymedia:::hardware_backends()) {
    withr::local_options(tidymedia.hardware_encoders = character(0))
    msg <- rlang::catch_cnd(
      standardize_video(f, "out.mp4", hardware = backend, fallback = TRUE,
                        run = FALSE),
      classes = "message"
    )
    expect_s3_class(msg, "message")
    # `backend` alone matches inside the interpolated encoder name
    # ("h264_videotoolbox"), so deleting the leading token would leave this
    # green; the token is matched with the word that follows it.
    expect_match(conditionMessage(msg), paste0(backend, " encoder"),
                 fixed = TRUE, info = backend)
    expect_match(conditionMessage(msg), "libx264", fixed = TRUE, info = backend)

    cmd <- withCallingHandlers(
      standardize_video(f, "out.mp4", hardware = backend, fallback = TRUE,
                        run = FALSE),
      message = function(m) invokeRestart("muffleMessage")
    )
    expect_match(cmd, "-codec:v libx264", fixed = TRUE, info = backend)
  }
})

test_that("a missing backend in a _batch call is blamed on the verb", {
  # The front-door gate is membership in the backend set, never a test against
  # one backend's name: under the old gate a videotoolbox batch call returned
  # early and the abort surfaced from purrr::pmap() instead of the verb.
  withr::local_options(tidymedia.hardware_encoders = character(0))
  f <- make_input()
  jobs <- tibble::tibble(input = f, output = "out.mp4")
  for (backend in tidymedia:::hardware_backends()) {
    cnd <- rlang::catch_cnd(
      standardize_video_batch(jobs, hardware = backend, run = FALSE)
    )
    expect_s3_class(cnd, "rlang_error")
    expect_identical(deparse(conditionCall(cnd))[[1]],
                     "standardize_video_batch(jobs, hardware = backend, run = FALSE)",
                     info = backend)
  }
})

# AC5 -- the exported helper answers for either backend --------------------------

test_that("has_hardware_encoder() answers per backend under either pool", {
  withr::local_options(tidymedia.hardware_encoders = "h264_videotoolbox")
  expect_true(has_hardware_encoder("h264", "videotoolbox"))
  expect_false(has_hardware_encoder("h264", "nvenc"))

  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
  expect_true(has_hardware_encoder("h264", "nvenc"))
  expect_false(has_hardware_encoder("h264", "videotoolbox"))
})

# AC4 -- the videotoolbox path executed, not only compiled -----------------------

test_that("videotoolbox encodes a real file", {
  skip_if_no_videotoolbox()
  infile <- withr::local_tempfile(fileext = ".mp4")
  outfile <- withr::local_tempfile(fileext = ".mp4")
  # A synthetic input, so the test carries no fixture and no provenance debt.
  system2("ffmpeg",
          c("-hide_banner", "-loglevel", "error", "-y", "-f", "lavfi",
            "-i", "testsrc=s=320x240:d=1", "-c:v", "libx264",
            shQuote(infile)),
          stdout = FALSE, stderr = FALSE)
  skip_if_not(file.exists(infile) && file.size(infile) > 0,
              message = "could not build a test input")

  standardize_video(infile, outfile, width = 160, height = 120,
                    hardware = "videotoolbox")

  expect_true(file.exists(outfile))
  expect_gt(file.size(outfile), 0)
  expect_equal(get_width(outfile), 160)
})
