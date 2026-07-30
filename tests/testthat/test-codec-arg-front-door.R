# Front-door validation parity for the codec arguments (M41).
#
# Every task verb and `_batch` sibling whose `video_codec` or `audio_codec`
# argument *sets* a codec must refuse a non-string value at its own front door:
# naming its own argument (never Layer-1's `video`/`audio`), blaming itself
# (never a `*_pipeline()` helper or `purrr::pmap()`), and firing before the
# fan-out (no `In index: <n>` at parallel = FALSE).
#
# `verify_media()` is excluded by design: its same-named arguments are expected
# probe values, not codec settings.
#
# The pair list is fixed here rather than derived at run time, so a verb that
# gains a codec argument without a guard fails the completeness test below
# instead of silently dropping out of the sweep.

codec_front_door_pairs <- function() {
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
codec_front_door_call <- function(verb, input, out) {
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
    # codec argument this file exists to test (review A1).
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

# A valid codec for the `col = "present"` runs below. It must be a value the
# per-row column guards accept, so the column genuinely wins the internal
# `pick()` and the scalar argument is the only thing under test.
codec_front_door_col_value <- function(arg) {
  if (arg == "video_codec") "libx264" else "aac"
}

# Which column states a verb can be in: a scalar verb has no `jobs` table, so
# "present" is not a state it can reach.
codec_front_door_cols <- function(args) {
  if ("jobs" %in% names(args)) c("absent", "present") else "absent"
}

# Call `verb` with `arg` set to `value`, returning the condition it throws (or
# NULL if it did not throw). `parallel = FALSE` is passed explicitly where the
# verb accepts it, because AC3 is a claim about exactly that path.
#
# `col = "present"` gives `jobs` a column of the same name as `arg`. Internally
# the batch verbs prefer that column over the scalar argument, so before M41 the
# scalar was never read on this path and a bad value in it was discarded in
# silence. M41-D2 adopts refusing it, and this is the ONLY place the executed
# suite measures that: without these runs, making a guard conditional on the
# column's absence reverts M41-D2 with the whole suite still green (measured at
# the round-2 review).
codec_front_door_catch <- function(verb, arg, value, input, out = "out.mp4",
                                   col = "absent") {
  f <- get(verb, envir = asNamespace("tidymedia"))
  args <- codec_front_door_call(verb, input, out)
  args$run <- FALSE
  if ("parallel" %in% names(formals(f))) args$parallel <- FALSE
  # Single-bracket assignment of list(value) so a NULL value would be STORED
  # rather than deleting the element (`args[[arg]] <- NULL` removes it).
  args[arg] <- list(value)
  if (identical(col, "present")) {
    args$jobs[[arg]] <- codec_front_door_col_value(arg)
  }
  tryCatch({
    do.call(verb, args, envir = asNamespace("tidymedia"))
    NULL
  }, condition = function(cnd) cnd)
}

# The three non-string shapes AC2 names.
codec_front_door_bad <- list(
  `NA` = NA,
  number = 1,
  `length-2 vector` = c("aac", "mp3")
)

test_that("every codec argument refuses a non-string at its own front door", {
  input <- make_input()
  for (pair in codec_front_door_pairs()) {
    verb <- pair$verb
    for (arg in pair$args) {
      cols <- codec_front_door_cols(codec_front_door_call(verb, input, "out.mp4"))
      for (col in cols) {
      for (shape in names(codec_front_door_bad)) {
        label <- paste0(verb, "(", arg, " = ", shape, ", col = ", col, ")")
        cnd <- codec_front_door_catch(verb, arg, codec_front_door_bad[[shape]],
                                      input, col = col)

        # It must abort at all -- the M41 regression was a silent compile.
        # Fail SOFT past this point: without the `next`, a pair that does not
        # abort sends NULL into conditionMessage() below, which throws and takes
        # the whole test_that() down with it -- silently dropping every later
        # verb in the sweep, so one broken guard hides the state of nineteen
        # others (review F8). The mutation sweep blanked one guard at a time and
        # so never produced the two-failure case that exposes this.
        aborted <- inherits(cnd, "error")
        expect_true(aborted, label = paste(label, "aborts"))
        if (!aborted) next
        msg <- cli::ansi_strip(conditionMessage(cnd))

        # AC2: the message names the caller's own argument ...
        expect_match(msg, arg, fixed = TRUE, label = paste(label, "names arg"))
        # ... and never Layer-1's parameter name.
        engine <- if (arg == "video_codec") "`video` must be" else "`audio` must be"
        expect_no_match(msg, engine, fixed = TRUE,
                        label = paste(label, "hides engine arg"))

        # AC2: the condition blames the Layer-2 verb, not a helper or pmap.
        call_txt <- paste(deparse(conditionCall(cnd)), collapse = " ")
        expect_match(call_txt, paste0("^", verb, "\\("),
                     label = paste(label, "blames the verb"))

        # AC3: it fired before the fan-out, not inside purrr::pmap().
        expect_no_match(msg, "In index:", fixed = TRUE,
                        label = paste(label, "is not mid-fan-out"))
      }
      }
    }
  }
})

test_that("every batch template is a jobs shape its own verb accepts", {
  # A template whose `jobs` has the wrong columns aborts on the SHAPE before the
  # codec argument is ever read. The sweep above still passes -- the verb did
  # refuse the value -- but it refused it for an unrelated reason, and the
  # matching cell in data-raw/codec-guard-baseline.R goes vacuous the same way.
  # That is exactly how picture_in_picture_batch sat in both files with an
  # `inputs` list-column it does not accept (review A1), which made AC4's
  # "no default/null row changed" a claim about nothing on that verb.
  #
  # Checked cheaply and without media: call each batch verb with no codec
  # argument and require that whatever it says next is not a jobs-shape
  # complaint. A later failure for want of a real input file is fine, and is why
  # this asserts the absence of one message rather than success.
  input <- make_input()
  for (pair in codec_front_door_pairs()) {
    verb <- pair$verb
    args <- codec_front_door_call(verb, input, "out.mp4")
    if (!"jobs" %in% names(args)) next
    args$run <- FALSE
    args$parallel <- FALSE
    cnd <- tryCatch({
      do.call(verb, args, envir = asNamespace("tidymedia"))
      NULL
    }, condition = function(cnd) cnd)
    msg <- if (is.null(cnd)) "" else cli::ansi_strip(conditionMessage(cnd))
    shape <- grepl("Missing column", msg, fixed = TRUE) ||
      grepl("`jobs` must", msg, fixed = TRUE)
    expect_false(
      shape,
      label = paste0(verb, " accepts its template's jobs shape",
                     if (shape) paste0(" [got: ", msg, "]"))
    )
  }
})

# Which complaint a batch verb makes when a call is wrong about BOTH `jobs` and
# the codec argument. Measured against the pre-M41 tree, not chosen: the split
# below is inherited, and ten of these seventeen pairs answered "codec" long
# before this milestone existed. Deliberately NOT normalized here -- making them
# agree would change error text on verbs M41 never touched, which is precisely
# the unasked-for behaviour change this table exists to catch.
#
# The table's job is to freeze the answers so a guard cannot silently move
# across the `jobs` check again. Two of M41's did, flipping
# standardize_video_batch and anonymize_video_batch from "jobs" to "codec"
# (review A6); both are back where they were, and this is what says so.
codec_front_door_precedence <- function() {
  c(
    "anonymize_video_batch video_codec"       = "jobs",
    "anonymize_video_batch audio_codec"       = "codec",
    "compare_videos_batch video_codec"        = "codec",
    "compare_videos_batch audio_codec"        = "codec",
    "convert_audio_batch audio_codec"         = "jobs",
    "crop_video_batch video_codec"            = "codec",
    "crop_video_batch audio_codec"            = "codec",
    "extract_audio_batch audio_codec"         = "jobs",
    "normalize_audio_batch audio_codec"       = "jobs",
    "picture_in_picture_batch video_codec"    = "codec",
    "picture_in_picture_batch audio_codec"    = "codec",
    "segment_video_batch video_codec"         = "codec",
    "segment_video_batch audio_codec"         = "codec",
    "separate_audio_video_batch video_codec"  = "jobs",
    "separate_audio_video_batch audio_codec"  = "jobs",
    "standardize_video_batch video_codec"     = "jobs",
    "standardize_video_batch audio_codec"     = "codec"
  )
}

test_that("a bad codec argument does not change which error an invalid jobs gets", {
  expected <- codec_front_door_precedence()
  observed <- character()
  for (pair in codec_front_door_pairs()) {
    verb <- pair$verb
    if (!"jobs" %in% names(codec_front_door_call(verb, "in.mp4", "out.mp4"))) next
    for (arg in pair$args) {
      args <- list(jobs = "oops", run = FALSE, parallel = FALSE)
      args[arg] <- list(NA)
      cnd <- tryCatch({
        do.call(verb, args, envir = asNamespace("tidymedia"))
        NULL
      }, condition = function(cnd) cnd)
      msg <- if (is.null(cnd)) "" else cli::ansi_strip(conditionMessage(cnd))
      observed[paste(verb, arg)] <-
        if (grepl("`jobs`", msg, fixed = TRUE)) "jobs" else "codec"
    }
  }
  # setequal on names first, so a verb added or dropped reports as that rather
  # than as a confusing value mismatch.
  expect_setequal(names(observed), names(expected))
  expect_identical(observed[names(expected)], expected)
})

test_that("a codec guard does not preempt a verb's other front-door checks", {
  # The jobs-shape probe above covers only the FIRST tier of jobs validation.
  # Each batch verb has a second tier -- override-column types, the "copy"
  # refusal, the derived-output duplicate check -- and the new guards sat above
  # all of it, so a call wrong about the jobs CONTENTS reported the codec
  # instead (review A1r3). The same held for normalize_audio's two_pass block,
  # which type-checks this argument itself (A3r3). Every guard now sits at the
  # end of its verb's front-door validation, and these pin it there: each call
  # is wrong about two things, and must still report the non-codec one.
  input <- make_input()
  r <- data.frame(x = 0, y = 0, width = 32, height = 32)
  cases <- list(
    list(lbl = "standardize_video_batch / duplicate input",
         f = function() standardize_video_batch(
           tibble::tibble(input = c(input, input)), video_codec = NA,
           run = FALSE, parallel = FALSE),
         want = "duplicated"),
    list(lbl = "standardize_video_batch / bad pixel_format column",
         f = function() standardize_video_batch(
           tibble::tibble(input = input, output = "o.mp4", pixel_format = 1),
           video_codec = NA, run = FALSE, parallel = FALSE),
         want = "pixel_format"),
    list(lbl = "anonymize_video_batch / duplicate input",
         f = function() anonymize_video_batch(
           tibble::tibble(input = c(input, input), regions = list(r, r)),
           video_codec = NA, run = FALSE, parallel = FALSE),
         want = "duplicated"),
    list(lbl = "normalize_audio_batch / duplicate input",
         f = function() normalize_audio_batch(
           tibble::tibble(input = c(input, input)), audio_codec = NA,
           run = FALSE, parallel = FALSE),
         want = "duplicated"),
    list(lbl = "normalize_audio_batch / copy in the column",
         f = function() normalize_audio_batch(
           tibble::tibble(input = input, output = "o.mp4", audio_codec = "copy"),
           audio_codec = NA, run = FALSE, parallel = FALSE),
         want = "copy"),
    list(lbl = "normalize_audio / two_pass channels",
         f = function() normalize_audio(
           input, "o.mp3", audio_codec = NA, channels = 0, two_pass = TRUE,
           run = FALSE),
         want = "channels")
  )
  for (case in cases) {
    cnd <- tryCatch({ case$f(); NULL }, error = function(e) e)
    expect_true(inherits(cnd, "error"), label = paste(case$lbl, "aborts"))
    if (!inherits(cnd, "error")) next
    msg <- cli::ansi_strip(conditionMessage(cnd))
    expect_match(msg, case$want, fixed = TRUE,
                 label = paste0(case$lbl, " reports the non-codec problem"))
  }
})

test_that("the front-door sweep covers every codec argument the package exports", {
  # Completeness: a verb that gains a video_codec/audio_codec argument later must
  # either join the list above or be excluded on the record, rather than quietly
  # escaping the sweep.
  exported <- getNamespaceExports(asNamespace("tidymedia"))
  found <- list()
  for (nm in sort(exported)) {
    f <- get(nm, envir = asNamespace("tidymedia"))
    if (!is.function(f)) next
    for (a in intersect(c("video_codec", "audio_codec"), names(formals(f)))) {
      found[[length(found) + 1]] <- paste(nm, a)
    }
  }
  found <- sort(unlist(found))

  covered <- sort(unlist(lapply(codec_front_door_pairs(), function(p) {
    paste(p$verb, p$args)
  })))
  # verify_media()'s same-named arguments are expected probe VALUES, not codec
  # settings, so guarding them would change the contract rather than validate it.
  excluded <- c("verify_media video_codec", "verify_media audio_codec")

  expect_setequal(found, c(covered, excluded))
})

test_that("NULL keeps its existing per-verb meaning (M41 is contract-neutral)", {
  input <- make_input()
  # M41 changed only which values are REFUSED. Every guard takes
  # allow_null = TRUE, so NULL reaches exactly the path it reached before, and
  # these verbs still disagree about what it means there -- deliberately, with
  # the reconciliation left to M42.

  # Compiles, dropping the codec flag entirely.
  expect_no_match(
    as.character(extract_audio_batch(
      tibble::tibble(input = input, output = "a.aac"),
      audio_codec = NULL, run = FALSE)$command[[1]]),
    "-codec:a", fixed = TRUE
  )
  # ... while the scalar sibling refuses NULL outright.
  expect_error(
    extract_audio(input, "a.aac", audio_codec = NULL, run = FALSE),
    "audio_codec"
  )
  # convert_audio's NULL selects -q:a 0 rather than emitting nothing (D021).
  expect_match(
    as.character(convert_audio(input, "a.mp3", audio_codec = NULL, run = FALSE)),
    "-q:a 0", fixed = TRUE
  )
  # standardize_video's NULL drops -codec:v and keeps everything else.
  expect_no_match(
    as.character(standardize_video(input, "out.mp4", video_codec = NULL,
                                   run = FALSE)),
    "-codec:v", fixed = TRUE
  )
  # normalize_audio's NULL is D019's emit-nothing sentinel.
  expect_no_match(
    as.character(normalize_audio(input, "out.mp4", audio_codec = NULL,
                                 run = FALSE)),
    "-codec:a", fixed = TRUE
  )
})
