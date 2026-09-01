# What `NULL` and a column `NA` mean across the codec family (M42, D022).
#
# The rule: `video_codec` / `audio_codec` = NULL emits no `-codec:v` /
# `-codec:a` at all, deferring to the output container's default encoder, and a
# jobs-table column `NA` is the column form of that same NULL. The one departure
# is recorded here rather than skipped -- convert_audio's NULL and column NA
# select `-q:a 0` (D021, reaffirmed by D022).
#
# Every "the flag is absent" assertion is paired with a NON-VACUITY assertion
# that the same call with a real encoder DOES show the flag. Without it, "no
# -codec:v in this command" passes on a verb that emits one under no setting at
# all, and the table measures nothing -- the failure mode
# data-raw/codec-guard-baseline.R's codec_guard_vacuous() exists to catch on the
# probe side.

# Set `arg` to `value` in a call template. Single-bracket assignment of
# list(value) so a NULL is STORED rather than deleting the element, which would
# silently turn every NULL case back into the default case.
codec_with <- function(args, arg, value) {
  args[arg] <- list(value)
  args
}

# Compile a verb to one string. A fan-out verb emits several commands and a
# `_batch` verb returns a tibble carrying them in a column; both are collapsed,
# because every assertion below asks whether a flag appears anywhere in what the
# call compiles.
codec_compiled <- function(verb, args) {
  f <- get(verb, envir = asNamespace("tidymedia"))
  args$run <- FALSE
  if ("parallel" %in% names(formals(f))) args$parallel <- FALSE
  out <- do.call(verb, args, envir = asNamespace("tidymedia"))
  if (is.data.frame(out)) out <- out$command
  paste(as.character(out), collapse = " ||| ")
}

# Fail-soft form for the parameterized sweeps below: register a failure naming
# the pair and return NULL, rather than letting the error propagate.
#
# This mirrors the `if (!aborted) next` in test-codec-arg-front-door.R, which
# M41's review added as its finding F8: a sweep whose body throws inside
# expect_match()'s argument evaluation takes the whole test_that() down with it,
# so ONE unexpectedly-broken verb hides the state of every pair after it in the
# loop. A mutation harness that breaks one guard at a time never produces the
# two-failure case that exposes this, which is how it survived M41 round 1.
codec_compiled_soft <- function(verb, args, label) {
  out <- tryCatch(codec_compiled(verb, args), error = function(e) e)
  if (inherits(out, "error")) {
    testthat::fail(paste0(
      label, " — expected this to compile, got an error: ",
      sub("\n.*", "", cli::ansi_strip(conditionMessage(out)))
    ))
    return(NULL)
  }
  out
}

# The resolved meaning of "unset" per verb x argument (D022). The family rule is
# "none" -- emit no -codec:v / -codec:a at all -- and only departures are listed.
# A departure appears here as an EXPECTED outcome rather than a skipped case, so
# it stays a recorded choice instead of decaying back into the accident this
# milestone found three of.
#
#   q0  NULL and a column NA select `-q:a 0`, highest-quality VBR (D021,
#       reaffirmed at M42's gate: the rename corrected the argument's name, and
#       transferring the sentinel would silently change every existing default
#       call's output).
codec_family_unset_meaning <- function() {
  c(
    "convert_audio audio_codec"       = "q0",
    "convert_audio_batch audio_codec" = "q0"
  )
}

# Assert one compiled command against the meaning the table records for it.
codec_expect_unset <- function(cmd, arg, want, label) {
  flag <- if (arg == "video_codec") "-codec:v" else "-codec:a"
  if (identical(want, "q0")) {
    expect_match(cmd, "-q:a 0", fixed = TRUE,
                 label = paste(label, "selects -q:a 0"))
  } else {
    expect_no_match(cmd, flag, fixed = TRUE,
                    label = paste(label, "emits no", flag))
  }
}

test_that("NULL and a column NA mean the same thing on every codec argument (D022)", {
  # The family table. Completeness against the package's exports is asserted by
  # test-codec-arg-front-door.R, which sweeps the same codec_family_pairs()
  # list, so a verb that gains a codec argument cannot escape this table either.
  input <- make_input()
  departures <- codec_family_unset_meaning()
  flag <- c(video_codec = "-codec:v", audio_codec = "-codec:a")

  for (pair in codec_family_pairs()) {
    verb <- pair$verb
    for (arg in pair$args) {
      key <- paste(verb, arg)
      want <- if (key %in% names(departures)) departures[[key]] else "none"
      base <- c(codec_family_call(verb, input, "out.mp4"),
                codec_family_extra(verb, arg))
      lbl <- paste0(key, " [", want, "]")

      # Non-vacuity, per pair: a named encoder reaches the compiled command, so
      # an absent flag below is the sentinel's doing and not a verb that emits
      # no such flag under any setting at all.
      named <- codec_family_col_value(arg)
      got <- codec_compiled_soft(verb, codec_with(base, arg, named),
                                 paste(lbl, "named encoder"))
      if (is.null(got)) next
      expect_match(
        got, paste(flag[[arg]], named), fixed = TRUE,
        label = paste(lbl, "emits the flag for a named encoder")
      )

      got <- codec_compiled_soft(verb, codec_with(base, arg, NULL),
                                 paste(lbl, "scalar NULL"))
      if (is.null(got)) next
      codec_expect_unset(got, arg, want, paste(lbl, "scalar NULL"))

      # The column form, wherever there is a column to carry it.
      if ("jobs" %in% names(base)) {
        na_args <- base
        na_args$jobs[[arg]] <- NA
        got <- codec_compiled_soft(verb, na_args, paste(lbl, "column NA"))
        if (is.null(got)) next
        codec_expect_unset(got, arg, want, paste(lbl, "column NA"))
      }
    }
  }
})

test_that("the departure table names only pairs that exist", {
  # A departure left behind for a renamed or retired verb would sit here reading
  # as a live exception while asserting nothing, which is how a table of
  # exceptions rots.
  keys <- unlist(lapply(codec_family_pairs(),
                        function(p) paste(p$verb, p$args)))
  expect_true(all(names(codec_family_unset_meaning()) %in% keys))
})

test_that("the four standardize/anonymize verbs agree on video_codec = NULL (D022)", {
  input <- make_input()
  regions <- data.frame(x = 0, y = 0, width = 32, height = 32)

  # Before M42 these four disagreed three ways: standardize_video and its batch
  # sibling compiled NULL, anonymize_video aborted at anonymize_pipeline()'s
  # unconditional check_token(), and anonymize_video_batch aborted INSIDE
  # purrr::pmap() carrying `In index: 1` (measured at M42 T1).
  calls <- list(
    standardize_video       = list(infile = input, outfile = "o.mp4"),
    standardize_video_batch = list(jobs = tibble::tibble(input = input,
                                                         output = "o.mp4")),
    anonymize_video         = list(infile = input, outfile = "o.mp4",
                                   regions = regions),
    anonymize_video_batch   = list(jobs = tibble::tibble(
                                     input = input, output = "o.mp4",
                                     regions = list(regions)))
  )

  for (verb in names(calls)) {
    args <- calls[[verb]]

    # Non-vacuity: this verb emits -codec:v at all, so the absence asserted
    # below is the sentinel's doing and not the verb's silence.
    expect_match(
      codec_compiled(verb, codec_with(args, "video_codec", "libx265")),
      "-codec:v libx265", fixed = TRUE,
      label = paste(verb, "emits -codec:v for a named encoder")
    )

    # The rule: NULL drops the flag, on all four.
    expect_no_match(
      codec_compiled(verb, codec_with(args, "video_codec", NULL)),
      "-codec:v", fixed = TRUE,
      label = paste(verb, "drops -codec:v under NULL")
    )
  }
})

test_that("the NULL sentinel changes nothing else about the compiled command", {
  # The two verbs ship a documented standard profile, so `NULL` must remove the
  # encoder and nothing else: dropping the codec is not licence to drop the
  # pixel format, the faststart flag, or the audio stream copy alongside it.
  input <- make_input()
  regions <- data.frame(x = 0, y = 0, width = 32, height = 32)

  std <- codec_compiled("standardize_video",
                        list(infile = input, outfile = "o.mp4",
                             video_codec = NULL))
  expect_match(std, "-pix_fmt yuv420p", fixed = TRUE)
  expect_match(std, "-movflags +faststart", fixed = TRUE)
  expect_match(std, "-codec:a copy", fixed = TRUE)

  anon <- codec_compiled("anonymize_video",
                         list(infile = input, outfile = "o.mp4",
                              regions = regions, video_codec = NULL))
  expect_match(anon, "-pix_fmt yuv420p", fixed = TRUE)
  expect_match(anon, "-codec:a copy", fixed = TRUE)
  expect_match(anon, "drawbox", fixed = TRUE)
})

test_that("extract_audio and its batch sibling agree on audio_codec = NULL (D022)", {
  input <- make_input()
  calls <- list(
    extract_audio       = list(infile = input, outfile = "a.aac"),
    extract_audio_batch = list(jobs = tibble::tibble(input = input,
                                                     output = "a.aac"))
  )
  for (verb in names(calls)) {
    args <- calls[[verb]]
    expect_match(
      codec_compiled(verb, codec_with(args, "audio_codec", "aac")),
      "-codec:a aac", fixed = TRUE,
      label = paste(verb, "emits -codec:a for a named encoder")
    )
    # The scalar verb aborted here until M42 while the batch sibling compiled;
    # D021 recorded that as "defensible" without noticing the disagreement.
    expect_no_match(
      codec_compiled(verb, codec_with(args, "audio_codec", NULL)),
      "-codec:a", fixed = TRUE,
      label = paste(verb, "drops -codec:a under NULL")
    )
    # Dropping the codec is not licence to drop the video-stream removal that
    # is this verb's actual job.
    expect_match(
      codec_compiled(verb, codec_with(args, "audio_codec", NULL)),
      "-vn", fixed = TRUE,
      label = paste(verb, "still drops video under NULL")
    )
  }
})

test_that("each remaining verb's NULL keeps the meaning D022 records for it", {
  # Inherited from test-codec-arg-front-door.R, where M41 pinned these as the
  # per-verb meanings it left untouched. They belong here now: D022 makes them
  # one family rule with one departure, and a second file asserting NULL
  # semantics is how the two drift apart.
  input <- make_input()

  # convert_audio is the departure: NULL selects -q:a 0, not "emit nothing"
  # (D021, reaffirmed at M42's gate).
  expect_match(
    as.character(convert_audio(input, "a.mp3", audio_codec = NULL, run = FALSE)),
    "-q:a 0", fixed = TRUE
  )
  # normalize_audio is D019's emit-nothing sentinel.
  expect_no_match(
    as.character(normalize_audio(input, "out.mp4", audio_codec = NULL,
                                 run = FALSE)),
    "-codec:a", fixed = TRUE
  )
})

test_that("a codec column NA is the column form of NULL (D022)", {
  # Three columns could not spell "unset" while their own argument could:
  # standardize_video_batch and anonymize_video_batch's video_codec (an inline
  # str_cols no-NA loop) and extract_audio_batch's audio_codec
  # (check_batch_string_col). Each aborted on an NA cell that every other codec
  # column accepts.
  input <- make_input()
  regions <- data.frame(x = 0, y = 0, width = 32, height = 32)
  flag <- c(video_codec = "-codec:v", audio_codec = "-codec:a")

  cases <- list(
    list(verb = "standardize_video_batch", arg = "video_codec",
         jobs = tibble::tibble(input = input, output = "o.mp4")),
    list(verb = "anonymize_video_batch", arg = "video_codec",
         jobs = tibble::tibble(input = input, output = "o.mp4",
                               regions = list(regions))),
    list(verb = "extract_audio_batch", arg = "audio_codec",
         jobs = tibble::tibble(input = input, output = "a.aac"))
  )

  for (case in cases) {
    lbl <- paste0(case$verb, "$", case$arg)

    # Non-vacuity: a real codec in the column reaches the command, so the
    # absence asserted next is the NA cell's doing.
    named <- case$jobs
    named[[case$arg]] <- if (case$arg == "video_codec") "libx265" else "aac"
    got <- codec_compiled_soft(case$verb, list(jobs = named),
                               paste(lbl, "named encoder in the column"))
    if (is.null(got)) next
    expect_match(
      got, paste(flag[[case$arg]], named[[case$arg]]), fixed = TRUE,
      label = paste(lbl, "carries a named encoder from the column")
    )

    # An NA cell compiles and drops the flag, exactly as the scalar NULL does.
    na_jobs <- case$jobs
    na_jobs[[case$arg]] <- NA
    got <- codec_compiled_soft(case$verb, list(jobs = na_jobs),
                               paste(lbl, "column NA"))
    if (is.null(got)) next
    expect_no_match(
      got, flag[[case$arg]], fixed = TRUE,
      label = paste(lbl, "reads an NA cell as unset")
    )

    # The all-NA column R types as LOGICAL, not character -- the shape
    # check_batch_codec_col() admits on purpose and a plain is.character()
    # test rejects (the M34 lesson).
    expect_true(is.logical(na_jobs[[case$arg]]), label = paste(lbl, "NA is logical"))

    # A *scalar* NA is still refused, and the present column does not excuse it
    # (M41-D2). Widening the column must not widen the argument.
    expect_error(
      codec_compiled(case$verb, codec_with(list(jobs = named), case$arg, NA)),
      case$arg
    )
  }
})

test_that("a mixed codec column leaves the named rows alone (D022)", {
  # The point of NA-as-unset is per-ROW opt-out: one row unset while another
  # names an encoder. A column-wide guard cannot express that, which is why
  # check_batch_string_col() was the wrong tool for a codec column.
  input <- make_input()
  cmds <- standardize_video_batch(
    tibble::tibble(input = c(input, input), output = c("a.mp4", "b.mp4"),
                   video_codec = c("libx265", NA)),
    run = FALSE, parallel = FALSE
  )$command
  expect_match(as.character(cmds[[1]]), "-codec:v libx265", fixed = TRUE)
  expect_no_match(as.character(cmds[[2]]), "-codec:v", fixed = TRUE)
})

test_that("each widened codec column still refuses a non-character column", {
  # The move to check_batch_codec_col() widened these columns to accept NA. It
  # must not have widened them to accept anything else. `standardize_video_batch`
  # and `extract_audio_batch` already had a test of their own
  # (test-standardize-video-batch.R, test-extract-audio-batch.R);
  # `anonymize_video_batch` had none, so deleting its new guard outright left the
  # whole suite green (M42 review F4). Without the guard a numeric column reaches
  # check_token() per row and aborts inside purrr::pmap() with `In index: 1` --
  # the defect shape M41 exists to have removed.
  input <- make_input()
  r <- data.frame(x = 0, y = 0, width = 32, height = 32)
  cases <- list(
    list(lbl = "anonymize_video_batch video_codec",
         f = function() anonymize_video_batch(
           tibble::tibble(input = input, output = "o.mp4",
                          regions = list(r), video_codec = 5),
           run = FALSE, parallel = FALSE),
         want = "video_codec"),
    list(lbl = "standardize_video_batch video_codec",
         f = function() standardize_video_batch(
           tibble::tibble(input = input, output = "o.mp4", video_codec = 5),
           run = FALSE, parallel = FALSE),
         want = "video_codec"),
    list(lbl = "extract_audio_batch audio_codec",
         f = function() extract_audio_batch(
           tibble::tibble(input = input, output = "a.aac", audio_codec = 1),
           run = FALSE, parallel = FALSE),
         want = "audio_codec")
  )
  for (case in cases) {
    cnd <- tryCatch({ case$f(); NULL }, error = function(e) e)
    expect_true(inherits(cnd, "error"), label = paste(case$lbl, "aborts"))
    if (!inherits(cnd, "error")) next
    msg <- cli::ansi_strip(conditionMessage(cnd))
    expect_match(msg, case$want, fixed = TRUE,
                 label = paste(case$lbl, "names the column"))
    # At the front door, not mid-fan-out.
    expect_no_match(msg, "In index:", fixed = TRUE,
                    label = paste(case$lbl, "is not mid-fan-out"))
  }
})

test_that("a bad codec column no longer preempts a bad non-codec column", {
  # Moving video_codec out of the str_cols loop into check_batch_codec_col()
  # changed which complaint a jobs table bad in BOTH columns receives:
  # `video_codec` reported first before M42, `pixel_format` reports first now.
  # M41's review caught an unpinned precedence move twice, so this freezes the
  # new order rather than leaving it to a comment (M42 review F3).
  input <- make_input()
  r <- data.frame(x = 0, y = 0, width = 32, height = 32)
  expect_match(
    tryCatch(standardize_video_batch(
      tibble::tibble(input = input, output = "o.mp4",
                     video_codec = 5, pixel_format = 1),
      run = FALSE, parallel = FALSE), error = conditionMessage),
    "pixel_format", fixed = TRUE
  )
  expect_match(
    tryCatch(anonymize_video_batch(
      tibble::tibble(input = input, output = "o.mp4", regions = list(r),
                     video_codec = 5, pixel_format = 1),
      run = FALSE, parallel = FALSE), error = conditionMessage),
    "pixel_format", fixed = TRUE
  )
  # `color` came before `video_codec` in anonymize's str_cols already, so that
  # pair's order is UNCHANGED by M42 -- pinned so a later reader does not read
  # the flip above as covering it too.
  expect_match(
    tryCatch(anonymize_video_batch(
      tibble::tibble(input = input, output = "o.mp4", regions = list(r),
                     video_codec = 5, color = 1),
      run = FALSE, parallel = FALSE), error = conditionMessage),
    "color", fixed = TRUE
  )
})

test_that("a widened codec argument's refusal says NULL is legal", {
  # allow_null = TRUE over `if (!is.null(x)) check_string(x)`: both accept the
  # same values, but only the former's message names NULL. M41 chose the latter
  # deliberately, when NULL really was illegal here; D022 made it legal and the
  # message had to follow (M42 review F1).
  input <- make_input()
  r <- data.frame(x = 0, y = 0, width = 32, height = 32)
  expect_match(
    tryCatch(anonymize_video(input, "o.mp4", regions = r, video_codec = NA,
                             run = FALSE), error = conditionMessage),
    "or `NULL`", fixed = TRUE
  )
  expect_match(
    tryCatch(anonymize_video_batch(
      tibble::tibble(input = input, output = "o.mp4", regions = list(r)),
      video_codec = NA, run = FALSE, parallel = FALSE),
      error = conditionMessage),
    "or `NULL`", fixed = TRUE
  )
  expect_match(
    tryCatch(extract_audio(input, "a.aac", audio_codec = NA, run = FALSE),
             error = conditionMessage),
    "or `NULL`", fixed = TRUE
  )
})

test_that("pixel_format and color columns still reject NA (D022)", {
  # str_cols keeps the non-codec columns, which have no sentinel: nothing about
  # a codec column learning to spell "unset" says an unset pixel format means
  # anything.
  input <- make_input()
  expect_error(
    standardize_video_batch(
      tibble::tibble(input = input, output = "o.mp4", pixel_format = NA),
      run = FALSE, parallel = FALSE),
    "pixel_format"
  )
  expect_error(
    anonymize_video_batch(
      tibble::tibble(input = input, output = "o.mp4",
                     regions = list(data.frame(x = 0, y = 0, width = 32,
                                               height = 32)),
                     color = NA),
      run = FALSE, parallel = FALSE),
    "color"
  )
})

test_that("a NULL video_codec still resolves to the nvenc H.264 family (D016)", {
  # D016 puts the sentinel INSIDE resolve_hw_encoder(), before family inference:
  # under hardware = "nvenc" a NULL assumes H.264 rather than emitting nothing.
  # anonymize_video reached that branch only for non-NULL values before M42,
  # because check_token() refused NULL first, so this pins the branch it now
  # reaches. Availability is simulated through the option seam has_hardware_encoder()
  # consults, so this stays a binary-free compile test (test-video-codec.R).
  withr::local_options(tidymedia.hardware_encoders = "h264_nvenc")
  input <- make_input()
  regions <- data.frame(x = 0, y = 0, width = 32, height = 32)
  expect_match(
    codec_compiled("anonymize_video",
                   list(infile = input, outfile = "o.mp4", regions = regions,
                        video_codec = NULL, hardware = "nvenc")),
    "-codec:v h264_nvenc", fixed = TRUE
  )
})
