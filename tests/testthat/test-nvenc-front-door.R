# The nvenc availability guard at the front door of every fan-out verb (M57).
#
# A verb that fans out through ffm_batch() -> purrr::pmap() cannot get useful
# blame from threading `call` into its pipeline: the pipeline's caller_env()
# lands on the anonymous closure, so an unavailable nvenc encoder was reported
# as "Error in `purrr::pmap(jobs, .f, ...)`" (LESSONS M47/M48-F1). D035
# licenses re-running the availability check at each such verb's front door.
#
# Availability is simulated with the `tidymedia.hardware_encoders` option seam, so
# these tests need no GPU and no nvenc-capable FFmpeg build.

# --- AC2: one abort site -----------------------------------------------------
#
# The front door and the pipeline must reach the same worded abort. Comparing
# the two functions directly is the sharpest form of that check: a copy of the
# wording in resolve_hw_encoder() would pass every verb-level test in this file
# right up until someone edited one of the two.

test_that("the front-door guard and resolve_hw_encoder() word the abort identically", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
  front <- tryCatch(
    tidymedia:::check_hardware_available("libx264", "nvenc", FALSE),
    condition = function(cnd) cnd
  )
  pipeline <- tryCatch(
    tidymedia:::resolve_hw_encoder("libx264", "nvenc", FALSE),
    condition = function(cnd) cnd
  )
  expect_s3_class(front, "rlang_error")
  expect_s3_class(pipeline, "rlang_error")
  expect_identical(conditionMessage(front), conditionMessage(pipeline))
  expect_match(conditionMessage(front), "h264_nvenc", fixed = TRUE)
})

# --- the fan-out family -----------------------------------------------------
#
# The nine verbs that take `hardware` AND fan out through ffm_batch(). The list
# is fixed rather than derived, so a verb that gains `hardware` later fails the
# completeness test below instead of silently dropping out of the sweep.
nvenc_fanout_verbs <- function() {
  c("segment_video", "anonymize_video_batch", "segment_video_batch",
    "standardize_video_batch", "crop_video_batch", "format_for_web_batch",
    "separate_audio_video_batch", "compare_videos_batch",
    "picture_in_picture_batch")
}

# The seven that take `hardware` and call their pipelines DIRECTLY, where `call`
# already resolves to the verb, so M57 adds them no guard (M47 F8). Named here
# only so the two lists together can be checked against the package's exports:
# a new hardware-bearing verb has to be classified, not quietly omitted.
nvenc_direct_verbs <- function() {
  c("separate_audio_video", "crop_video", "format_for_web", "standardize_video",
    "anonymize_video", "compare_videos", "picture_in_picture")
}

# Call templates. Eight come from helper-codec-family.R, which already models
# each verb's real column names -- picture_in_picture_batch takes main/overlay
# columns (D015) and compare_videos_batch an `inputs` list-column, and getting
# either wrong records a schema error in place of the abort under test (M54).
# format_for_web_batch has no codec argument at all (its recipe fixes both
# codecs by identity), so it is not in that helper and gets a template here.
nvenc_fanout_call <- function(verb, input, out) {
  if (identical(verb, "format_for_web_batch")) {
    return(list(jobs = tibble::tibble(input = input, output = out)))
  }
  args <- codec_family_call(verb, input, out)
  # separate_audio_video*'s video_codec defaults to "copy", which codec_family()
  # refuses outright -- a DIFFERENT abort, and one that would satisfy a blame
  # assertion while measuring nothing about availability (M54). Name an
  # h264-family codec so the cell measures what it claims to.
  if (identical(verb, "separate_audio_video_batch")) {
    args$video_codec <- "libx264"
  }
  args
}

# Which function the condition blames, rendered from the call's head alone: the
# full call deparses a whole jobs tibble inline.
nvenc_blamed <- function(cnd) {
  cl <- conditionCall(cnd)
  if (is.null(cl)) return(NA_character_)
  paste(deparse(cl[[1]]), collapse = "")
}

nvenc_fanout_catch <- function(verb, input, out = "out.mp4", parallel = FALSE,
                               extra = list()) {
  f <- get(verb, envir = asNamespace("tidymedia"))
  args <- nvenc_fanout_call(verb, input, out)
  args$hardware <- "nvenc"
  args$fallback <- FALSE
  args$run <- FALSE
  if ("parallel" %in% names(formals(f))) args$parallel <- parallel
  # Plain replacement, never modifyList(): a tibble IS a list, so modifyList
  # merges `extra$jobs` column-wise into the template's jobs instead of
  # replacing it, and it DELETES any element whose value is NULL rather than
  # setting it -- which would silently drop a `video_codec = NULL` case.
  for (nm in names(extra)) args[nm] <- list(extra[[nm]])
  # Catch the ERROR, never any condition: at parallel = TRUE the sequential-plan
  # warning arrives first and a `condition =` handler would return that instead
  # (measured while recording the master readings).
  withCallingHandlers(
    tryCatch({
      do.call(verb, args, envir = asNamespace("tidymedia"))
      NULL
    }, error = function(e) e),
    warning = function(w) invokeRestart("muffleWarning")
  )
}

test_that("the two lists together are every hardware-bearing exported verb", {
  exported <- getNamespaceExports("tidymedia")
  hw <- Filter(function(nm) {
    # `inherits = TRUE` (the default): a re-export such as `.data` is listed in
    # NAMESPACE but bound in the imports env, and an inherits = FALSE lookup
    # errors on it outright (M55).
    obj <- get(nm, envir = asNamespace("tidymedia"))
    is.function(obj) && "hardware" %in% names(formals(obj))
  }, exported)
  # The two capability helpers carry `hardware` but are not verbs and have no
  # front door to mirror; nvenc_hardware_exports() excludes them for the same
  # reason.
  expect_setequal(setdiff(hw, nvenc_hardware_helpers()),
                  c(nvenc_fanout_verbs(), nvenc_direct_verbs()))
})

# --- AC1: the abort names the verb ------------------------------------------

test_that("an unavailable nvenc encoder blames the fan-out verb, not purrr::pmap()", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
  input <- make_input()
  for (verb in nvenc_fanout_verbs()) {
    cnd <- nvenc_fanout_catch(verb, input)
    expect_s3_class(cnd, "rlang_error")
    # Match the MESSAGE first: a jobs-schema error blames the same front door
    # and would satisfy a bare conditionCall() assertion with the guard
    # deleted -- three M54 review rounds were paid for exactly that (M54).
    expect_match(conditionMessage(cnd), "nvenc encoder .* is not available",
                 info = verb)
    expect_identical(nvenc_blamed(cnd), verb, info = verb)
  }
})

test_that("the abort names the verb at parallel = TRUE too", {
  skip_if_not_installed("furrr")
  withr::local_options(tidymedia.hardware_encoders = character(0))
  old <- future::plan(future::sequential)
  withr::defer(future::plan(old))
  input <- make_input()
  cnd <- nvenc_fanout_catch("standardize_video_batch", input, parallel = TRUE)
  expect_s3_class(cnd, "rlang_error")
  expect_match(conditionMessage(cnd), "nvenc encoder .* is not available")
  expect_identical(nvenc_blamed(cnd), "standardize_video_batch")
})

# --- AC3: the guard sweeps the whole video_codec column ---------------------
#
# A _batch call is one `hardware` choice over many rows, but the FAMILY is
# per row: seven of the eight _batch verbs let a `video_codec` column override
# the argument, and h264_nvenc being listed says nothing about av1_nvenc. A
# guard reading only the argument would pass a table whose second row cannot
# encode.

test_that("the guard checks every family a video_codec column spells", {
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
  input <- make_input()
  jobs <- tibble::tibble(input = c(input, input), output = c("a.mp4", "b.mp4"),
                         video_codec = c("libx264", "libaom-av1"))
  cnd <- nvenc_fanout_catch("standardize_video_batch", input,
                            extra = list(jobs = jobs))
  expect_s3_class(cnd, "rlang_error")
  # The AV1 row is the one with no encoder; naming h264_nvenc here would mean
  # the guard had read the argument and stopped.
  expect_match(conditionMessage(cnd), "av1_nvenc", fixed = TRUE)
  expect_identical(nvenc_blamed(cnd), "standardize_video_batch")
})

test_that("the same column compiles when every family is available", {
  withr::local_options(tidymedia.hardware_encoders = c("h264_nvenc", "av1_nvenc"))
  input <- make_input()
  jobs <- tibble::tibble(input = c(input, input), output = c("a.mp4", "b.mp4"),
                         video_codec = c("libx264", "libaom-av1"))
  expect_null(nvenc_fanout_catch("standardize_video_batch", input,
                                 extra = list(jobs = jobs)))
})

test_that("an NA cell and an absent column both read as the h264 family", {
  input <- make_input()
  # NA is the column form of the NULL sentinel (D022), which
  # resolve_hw_encoder() resolves to h264 -- so a seam holding only av1_nvenc
  # must refuse both shapes, naming h264_nvenc.
  withr::local_options(tidymedia.hardware_encoders = "av1_nvenc")
  cnd <- nvenc_fanout_catch("segment_video_batch", input,
                            extra = list(jobs = tibble::tibble(
                              input = input, output = "a.mp4",
                              start = 0, end = 1, video_codec = NA)))
  expect_match(conditionMessage(cnd), "h264_nvenc", fixed = TRUE)
  expect_identical(nvenc_blamed(cnd), "segment_video_batch")

  # And with no column at all, the argument's own NULL default.
  bare <- nvenc_fanout_catch("segment_video_batch", input)
  expect_match(conditionMessage(bare), "h264_nvenc", fixed = TRUE)
  expect_identical(nvenc_blamed(bare), "segment_video_batch")
})

test_that("format_for_web_batch checks h264, the codec its recipe fixes", {
  withr::local_options(tidymedia.hardware_encoders = "av1_nvenc")
  input <- make_input()
  cnd <- nvenc_fanout_catch("format_for_web_batch", input)
  expect_match(conditionMessage(cnd), "h264_nvenc", fixed = TRUE)
  expect_identical(nvenc_blamed(cnd), "format_for_web_batch")
})

# --- AC4: fallback = TRUE reaches no front-door guard -----------------------

test_that("fallback = TRUE still falls back, once per row", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
  input <- make_input()
  jobs <- tibble::tibble(input = c(input, input), output = c("a.mp4", "b.mp4"))
  n <- 0
  out <- withCallingHandlers(
    standardize_video_batch(jobs, hardware = "nvenc", fallback = TRUE,
                            run = FALSE),
    message = function(m) {
      if (grepl("falling back", conditionMessage(m))) n <<- n + 1
      invokeRestart("muffleMessage")
    }
  )
  # Two, not one: the message belongs to the per-row resolution, and a guard
  # that hoisted or suppressed it would show up here as 1 or 0. Measured at 2
  # on master for the same table, so this is a count carried across, not a
  # count invented here.
  expect_identical(n, 2)
  expect_s3_class(out, "data.frame")
})

test_that("fallback = TRUE never lets the front door refuse an unmappable codec", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
  input <- make_input()
  # A prores codec under nvenc is refused regardless of `fallback` -- the
  # (family, backend) pair is not in the table -- so a front-door column sweep
  # would refuse this call. The guard's early return is what keeps the failure
  # where it was: inside the fan-out, unchanged from master.
  jobs <- tibble::tibble(input = input, output = "a.mp4",
                         video_codec = "prores")
  cnd <- nvenc_fanout_catch("standardize_video_batch", input,
                            extra = list(jobs = jobs, fallback = TRUE))
  expect_s3_class(cnd, "rlang_error")
  expect_match(conditionMessage(cnd), 'nvenc has no "prores" encoder',
               fixed = TRUE)
  expect_false(identical(nvenc_blamed(cnd), "standardize_video_batch"))
})

# --- the preconditions each front door mirrors ------------------------------
#
# Two pipelines abort BEFORE they ever reach resolve_hw_encoder(), so on those
# verbs the front-door guard must not fire: doing so would replace a specific
# message with an availability one that is not why the call failed (D035's
# second condition). These tests are what catches the guard's gate being
# dropped -- without them, widening it reads as green everywhere else.

test_that("a non-re-encoding cut still reports the cut, not availability", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
  input <- make_input()
  for (verb in c("segment_video", "segment_video_batch")) {
    cnd <- nvenc_fanout_catch(verb, input,
                              extra = list(reencode = FALSE, video_codec = NULL))
    expect_s3_class(cnd, "rlang_error")
    expect_match(conditionMessage(cnd), "need a re-encoding cut", info = verb)
    expect_no_match(conditionMessage(cnd), "is not available", info = verb)
  }
})

test_that("an all-FALSE reencode column reaches no front-door guard", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
  input <- make_input()
  # The column form of the scalar case above: every row copies, so no row has an
  # encoder to check and segment_pipeline()'s own cut error must survive.
  jobs <- tibble::tibble(input = c(input, input), output = c("a.mp4", "b.mp4"),
                         start = c(0, 0), end = c(1, 1),
                         reencode = c(FALSE, FALSE))
  cnd <- nvenc_fanout_catch("segment_video_batch", input,
                            extra = list(jobs = jobs))
  expect_s3_class(cnd, "rlang_error")
  expect_match(conditionMessage(cnd), "need a re-encoding cut")
  expect_no_match(conditionMessage(cnd), "is not available")
})

test_that("a MIXED reencode column is refused at the front door", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
  input <- make_input()
  # `reencode` is per row, and M57's all-or-nothing gate skipped the availability
  # guard entirely here, sending the re-encoding row back to blaming
  # purrr::pmap() (M57 review F4). Both guards are row-swept now, and since M58
  # the CUT contradiction reports: row 2 copies while `hardware = "nvenc"` names
  # an encoder, which is a contradiction whatever this machine has installed.
  jobs <- tibble::tibble(input = c(input, input), output = c("a.mp4", "b.mp4"),
                         start = c(0, 0), end = c(1, 1),
                         reencode = c(TRUE, FALSE))
  cnd <- nvenc_fanout_catch("segment_video_batch", input,
                            extra = list(jobs = jobs))
  expect_s3_class(cnd, "rlang_error")
  expect_match(conditionMessage(cnd), "need a re-encoding cut")
  expect_identical(nvenc_blamed(cnd), "segment_video_batch")
})

test_that("a copying row's own error reports rather than its family", {
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
  input <- make_input()
  # The AV1 row copies, so it names no encoder and the h264-only seam must not
  # refuse the table: a guard reading the whole column would abort on av1_nvenc.
  # Its own cut error is what this call fails on instead -- since M58 at the
  # front door rather than inside the fan-out, which is why the blame assertion
  # below is new.
  jobs <- tibble::tibble(input = c(input, input), output = c("a.mp4", "b.mp4"),
                         start = c(0, 0), end = c(1, 1),
                         reencode = c(TRUE, FALSE),
                         video_codec = c("libx264", "libaom-av1"))
  cnd <- nvenc_fanout_catch("segment_video_batch", input,
                            extra = list(jobs = jobs))
  expect_s3_class(cnd, "rlang_error")
  expect_no_match(conditionMessage(cnd), "av1_nvenc")
  expect_match(conditionMessage(cnd), "need a re-encoding cut")
  expect_identical(nvenc_blamed(cnd), "segment_video_batch")
})

# The precedence M58 reassigns, replacing the pair M57 pinned the other way up.
# A mixed-`reencode` call fails either way; what changes is that it now fails
# the SAME way on every machine, because a contradiction between two arguments
# is decided without consulting the local FFmpeg build while availability is
# not (the M54 lesson).

test_that("on a mixed column the cut contradiction reports before availability", {
  input <- make_input()
  jobs <- tibble::tibble(input = c(input, input), output = c("a.mp4", "b.mp4"),
                         start = c(0, 0), end = c(1, 1),
                         reencode = c(TRUE, FALSE))
  # Encoder present: row 2's cut error, at the verb -- where M57 left it inside
  # the fan-out, blaming purrr::pmap().
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
  present <- nvenc_fanout_catch("segment_video_batch", input,
                                extra = list(jobs = jobs))
  expect_match(conditionMessage(present), "need a re-encoding cut")
  expect_identical(nvenc_blamed(present), "segment_video_batch")
  # Encoder absent: the SAME error, not the availability one M57 raised here.
  # The set of failing calls is unchanged; what moves is which error reports.
  withr::local_options(tidymedia.hardware_encoders = character(0))
  absent <- nvenc_fanout_catch("segment_video_batch", input,
                               extra = list(jobs = jobs))
  expect_match(conditionMessage(absent), "need a re-encoding cut")
  expect_no_match(conditionMessage(absent), "is not available")
  expect_identical(nvenc_blamed(absent), "segment_video_batch")
  expect_identical(conditionMessage(present), conditionMessage(absent))
})

test_that("a copy video_codec still reports the copy, not availability", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
  input <- make_input()
  cnd <- nvenc_fanout_catch("separate_audio_video_batch", input,
                            extra = list(video_codec = "copy"))
  expect_s3_class(cnd, "rlang_error")
  expect_match(conditionMessage(cnd), "needs a re-encoding")
  expect_no_match(conditionMessage(cnd), "is not available")
})

test_that("a within-row output collision still reports the collision", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
  input <- make_input()
  # separate_audio_video_batch reshapes N rows -> 2N before pooling the duplicate
  # check, so audiofile == videofile is a collision only the reshaped table can
  # see (M26). Above the reshape the availability guard preempted it and this
  # caller was told the encoder was missing (review F3); below it, the guard is
  # last in the front-door block on this verb as on the other seven.
  jobs <- tibble::tibble(input = input, audiofile = "same.mkv",
                         videofile = "same.mkv")
  cnd <- nvenc_fanout_catch("separate_audio_video_batch", input,
                            extra = list(jobs = jobs))
  expect_s3_class(cnd, "rlang_error")
  expect_match(conditionMessage(cnd), "same output path")
  expect_no_match(conditionMessage(cnd), "is not available")
  expect_identical(nvenc_blamed(cnd), "separate_audio_video_batch")
})

test_that("a mixed copy column reports the copy conflict before availability", {
  input <- make_input()
  # The copy cell has no encoder to check and the libx264 cell does, so both
  # errors are live on this table (M57 review F2). M57 let the seam decide which
  # one reported; M58 fixes it on the copy conflict, so the same wrong call is
  # diagnosed identically on a machine with the encoder and one without.
  jobs <- tibble::tibble(input = c(input, input),
                         audiofile = c("a1.aac", "a2.aac"),
                         videofile = c("v1.mp4", "v2.mp4"),
                         video_codec = c("copy", "libx264"))
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
  present <- nvenc_fanout_catch("separate_audio_video_batch", input,
                                extra = list(jobs = jobs))
  expect_match(conditionMessage(present), "needs a re-encoding")
  expect_identical(nvenc_blamed(present), "separate_audio_video_batch")
  withr::local_options(tidymedia.hardware_encoders = character(0))
  absent <- nvenc_fanout_catch("separate_audio_video_batch", input,
                               extra = list(jobs = jobs))
  expect_match(conditionMessage(absent), "needs a re-encoding")
  expect_no_match(conditionMessage(absent), "is not available")
  expect_identical(nvenc_blamed(absent), "separate_audio_video_batch")
  expect_identical(conditionMessage(present), conditionMessage(absent))
})

# --- the precedence the guard reassigns -------------------------------------
#
# D035's second condition admits that a front-door guard reports before checks
# that live in the pipeline, and requires that this be tested rather than
# assumed away. AC6's grep enumerated the `cli_abort()` sites inside the nine
# verbs' `*_pipeline()` functions; it does NOT reach an abort a pipeline raises
# through a helper, nor a per-row check living in the batch closure. Those are
# pinned here instead of left unmeasured.

# The three cases this section pinned at M57 have REVERSED, and the pin moved
# with them. M57 measured a regions cell, a crop dimension and an inset margin
# all reporting the availability error first once the encoder was absent, and
# recorded that as a precedence the guard had taken. M59 moved all three checks
# to their verbs' front doors, ahead of check_nvenc_available(), so each now
# reports its own value error on a machine with the encoder and on a machine
# without it alike -- which is what makes the diagnosis machine-independent, the
# property M54 named and D036 ordered for. The new pin, with a control proving
# the availability abort is still live on the same verb, is
# test-value-check-front-door.R's AC5(a) test; asserting it here as well would
# be a second site for one claim.
#
# compare_videos_batch's three-input `resize` case left this sweep at M58 for
# the same reason, one milestone earlier.

# The other three verbs carrying both guards. Their contradictions are decided
# without reference to any encoder, so unlike the two mixed-column cases above
# these need no mixed table: a plain uniform call has both errors live at once.

test_that("a contradiction reports before availability on the fan-in verbs", {
  input <- make_input()
  cases <- list(
    # Condition 4: an audio encoder named with no audio mapped.
    list(verb = "compare_videos_batch", own = "needs an audio stream to encode",
         args = list(jobs = tibble::tibble(inputs = list(c(input, input)),
                                           output = "o.mp4"),
                     audio_codec = "aac")),
    # Condition 6: the same contradiction on the overlay verb.
    list(verb = "picture_in_picture_batch",
         own = "needs an audio stream to encode",
         args = list(jobs = tibble::tibble(main = input, overlay = input,
                                           output = "o.mp4"),
                     audio_codec = "aac")),
    # Condition 5: resize across three inputs. Encoder-independent too, which
    # is why it left the availability-first sweep above at M58.
    list(verb = "compare_videos_batch", own = "exactly two inputs",
         args = list(jobs = tibble::tibble(
           inputs = list(c(input, input, input)), output = "o.mp4"),
           resize = TRUE))
  )
  for (case in cases) {
    call_it <- function() {
      args <- case$args
      args$hardware <- "nvenc"
      args$run <- FALSE
      tryCatch(do.call(case$verb, args, envir = asNamespace("tidymedia")),
               error = function(e) e)
    }
    # Both seams, and the assertion is that they agree: a contradiction between
    # two arguments is settled without consulting the local FFmpeg build, so the
    # same wrong call must be diagnosed identically on any machine (M54).
    withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
    present <- call_it()
    withr::local_options(tidymedia.hardware_encoders = character(0))
    absent <- call_it()
    expect_match(conditionMessage(present), case$own, info = case$verb)
    expect_match(conditionMessage(absent), case$own, info = case$verb)
    expect_no_match(conditionMessage(absent), "is not available",
                    info = case$verb)
    expect_identical(conditionMessage(present), conditionMessage(absent))
    expect_identical(nvenc_blamed(absent), case$verb, info = case$verb)
  }
})

# --- the guard validates what it reads --------------------------------------

test_that("a malformed fallback reports its own type error, on any machine", {
  input <- make_input()
  jobs <- tibble::tibble(input = input, output = "a.mp4")
  # The guard used to test isTRUE(fallback), under which NA read as FALSE and
  # the call was told the encoder was missing -- but only where it WAS missing,
  # so one wrong call had two diagnoses depending on the machine (review F1).
  # Both seams must now give the type error resolve_hw_encoder() would give.
  for (seam in list("h264_nvenc", character(0))) {
    withr::local_options(tidymedia.hardware_encoders = seam)
    cnd <- tryCatch(
      standardize_video_batch(jobs, hardware = "nvenc", fallback = NA,
                              run = FALSE),
      error = function(e) e
    )
    expect_s3_class(cnd, "rlang_error")
    expect_match(conditionMessage(cnd), "must be `TRUE` or `FALSE`")
    expect_no_match(conditionMessage(cnd), "is not available")
    expect_identical(nvenc_blamed(cnd), "standardize_video_batch")
  }
})

test_that("hardware = \"none\" reaches no fallback check at the front door", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
  input <- make_input()
  jobs <- tibble::tibble(input = input, output = "a.mp4")
  # The validation sits after the hardware test on purpose: a hardware = "none"
  # call never consults `fallback` at the front door, so refusing one there
  # would be a refusal master does not make. It still fails, from the pipeline.
  cnd <- tryCatch(
    standardize_video_batch(jobs, hardware = "none", fallback = NA,
                            run = FALSE),
    error = function(e) e
  )
  expect_s3_class(cnd, "rlang_error")
  expect_match(conditionMessage(cnd), "must be `TRUE` or `FALSE`")
  expect_false(identical(nvenc_blamed(cnd), "standardize_video_batch"))
})

test_that("resolve_hw_encoder() reaches the abort by calling the shared guard", {
  # Read the function object, never the source tree: a test that opens R/ under
  # the package root SKIPS under R CMD check, which runs against an installed
  # package with no R/ dir, and so looks healthy while never running (M51).
  src <- deparse(body(tidymedia:::resolve_hw_encoder))
  expect_true(any(grepl("check_hardware_available", src, fixed = TRUE)))
  # cli_inform() stays -- that is the fallback message, not the abort.
  expect_false(any(grepl("cli_abort", src, fixed = TRUE)))
})
