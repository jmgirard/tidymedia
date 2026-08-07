# Front-door validation parity for the codec arguments (M41).
#
# Every task verb and `_batch` sibling whose `video_codec` or `audio_codec`
# argument *sets* a codec must refuse a bad value at its own front door:
# naming its own argument (never Layer-1's `video`/`audio`), blaming itself
# (never a `*_pipeline()` helper or `purrr::pmap()`), and firing before the
# fan-out (no `In index: <n>` at parallel = FALSE).
#
# "Bad" covered only non-string shapes until M56. A malformed but perfectly
# string-shaped TOKEN -- "aac -evil" -- took a different route: it passed every
# front-door check_string() and was refused deeper in, by ffm_codec(), which
# names Layer-1's `audio`/`video` and blames itself, or by a pipeline seam
# inside purrr::pmap(), which arrives as "In index: 1". Measured on the
# pre-M56 tree, 11 of the 51 cells this sweep runs held; the token value is in
# the set below so that stays true rather than being re-derived.
#
# `verify_media()` is excluded by design: its same-named arguments are expected
# probe values, not codec settings.
#
# The verb/argument list and the call templates live in
# helper-codec-family.R, shared with test-codec-null-na-semantics.R: two files
# sweep this family, and a copy in each is how the two lists drift apart. The
# list is fixed rather than derived at run time, so a verb that gains a codec
# argument without a guard fails the completeness test below instead of
# silently dropping out of the sweep.

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
  args <- codec_family_call(verb, input, out)
  args$run <- FALSE
  if ("parallel" %in% names(formals(f))) args$parallel <- FALSE
  # Single-bracket assignment of list(value) so a NULL value would be STORED
  # rather than deleting the element (`args[[arg]] <- NULL` removes it).
  args[arg] <- list(value)
  if (identical(col, "present")) {
    args$jobs[[arg]] <- codec_family_col_value(arg)
  }
  tryCatch({
    do.call(verb, args, envir = asNamespace("tidymedia"))
    NULL
  }, condition = function(cnd) cnd)
}

# The three non-string shapes AC2 names, plus M56's malformed token. The token
# is the only value here that a front-door check_string() lets through, so it is
# what distinguishes a verb guarded by check_token() (or routed through a
# Layer-2 codec seam with `call` threaded) from one that merely type-checks.
codec_front_door_bad <- list(
  `NA` = NA,
  number = 1,
  `length-2 vector` = c("aac", "mp3"),
  `malformed token` = "aac -evil"
)

test_that("every codec argument refuses a bad value at its own front door", {
  input <- make_input()
  for (pair in codec_family_pairs()) {
    verb <- pair$verb
    for (arg in pair$args) {
      cols <- codec_front_door_cols(codec_family_call(verb, input, "out.mp4"))
      for (col in cols) {
      for (shape in names(codec_front_door_bad)) {
        label <- paste0(verb, "(", arg, " = ", shape, ", col = ", col, ")")
        cnd <- codec_front_door_catch(verb, arg, codec_front_door_bad[[shape]],
                                      input, col = col)

        # It must abort at all -- the M41 regression was a silent compile, and
        # M56's `col = present` cells were silent compiles too: the malformed
        # scalar was discarded outright whenever a same-named jobs column won.
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
  for (pair in codec_family_pairs()) {
    verb <- pair$verb
    args <- codec_family_call(verb, input, "out.mp4")
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

# Which complaint a doubly-invalid call gets, at one bad value.
codec_front_door_precedence_at <- function(value) {
  observed <- character()
  for (pair in codec_family_pairs()) {
    verb <- pair$verb
    if (!"jobs" %in% names(codec_family_call(verb, "in.mp4", "out.mp4"))) next
    for (arg in pair$args) {
      args <- list(jobs = "oops", run = FALSE, parallel = FALSE)
      args[arg] <- list(value)
      cnd <- tryCatch({
        do.call(verb, args, envir = asNamespace("tidymedia"))
        NULL
      }, condition = function(cnd) cnd)
      msg <- if (is.null(cnd)) "" else cli::ansi_strip(conditionMessage(cnd))
      observed[paste(verb, arg)] <-
        if (grepl("`jobs`", msg, fixed = TRUE)) "jobs" else "codec"
    }
  }
  observed
}

test_that("a bad codec argument does not change which error an invalid jobs gets", {
  expected <- codec_front_door_precedence()
  observed <- codec_front_door_precedence_at(NA)
  # setequal on names first, so a verb added or dropped reports as that rather
  # than as a confusing value mismatch.
  expect_setequal(names(observed), names(expected))
  expect_identical(observed[names(expected)], expected)
})

test_that("a malformed token gets the same precedence a non-string does", {
  # M56 moved the token guard onto M41's site, so a doubly-invalid call answers
  # identically whichever kind of bad codec value it carries. Ten of these pairs
  # answered "jobs" for a token before M56 and "codec" for an NA, because the
  # token was refused deep in the pipeline where the jobs check had already
  # spoken; measured on both refs via data-raw/codec-guard-baseline.R.
  #
  # Asserted against the SAME frozen table rather than against a fresh NA run:
  # a table-free "these two agree" would stay green if both drifted together,
  # which is the whole failure the table above exists to catch.
  expected <- codec_front_door_precedence()
  observed <- codec_front_door_precedence_at("aac -evil")
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

test_that("a malformed token in a jobs COLUMN blames the batch verb", {
  # The column form of the sweep above. A batch verb reaches its pipeline
  # through ffm_batch() -> purrr::pmap(), so a per-row abort names `.f()` and
  # carries "In index: 1" -- which is what every batch verb did with a malformed
  # codec CELL until M56 moved the check into check_batch_codec_col(), at the
  # verb's own front door. Without this test the column path is unmeasured:
  # codec_family_col_value() deliberately puts a VALID codec in the column, so
  # the sweep above can never see a malformed one there.
  input <- make_input()
  for (pair in codec_family_pairs()) {
    verb <- pair$verb
    args0 <- codec_family_call(verb, input, "out.mp4")
    if (!"jobs" %in% names(args0)) next
    for (arg in pair$args) {
      label <- paste0(verb, "(jobs$", arg, " = malformed token)")
      args <- codec_family_call(verb, input, "out.mp4")
      args$run <- FALSE
      args$parallel <- FALSE
      args$jobs[[arg]] <- "aac -evil"
      args <- c(args, codec_family_extra(verb, arg))
      cnd <- tryCatch({
        do.call(verb, args, envir = asNamespace("tidymedia"))
        NULL
      }, condition = function(cnd) cnd)

      aborted <- inherits(cnd, "error")
      expect_true(aborted, label = paste(label, "aborts"))
      if (!aborted) next
      msg <- cli::ansi_strip(conditionMessage(cnd))
      expect_match(msg, arg, fixed = TRUE, label = paste(label, "names arg"))
      call_txt <- paste(deparse(conditionCall(cnd)), collapse = " ")
      expect_match(call_txt, paste0("^", verb, "\\("),
                   label = paste(label, "blames the verb"))
      expect_no_match(msg, "In index:", fixed = TRUE,
                      label = paste(label, "is not mid-fan-out"))
      expect_no_match(msg, ".f()", fixed = TRUE,
                      label = paste(label, "does not name the pmap closure"))
    }
  }
})

test_that("the nvenc path refuses a malformed token the same as the software path", {
  # resolve_hw_encoder() REWRITES video_codec before the pipeline's seam sees
  # it: codec_family() reads "libx264 -evil" as h264 and yields "h264_nvenc",
  # a perfectly clean token. So a verb that resolves before it checks accepts a
  # malformed value under hardware = "nvenc" while refusing it under "none".
  # standardize_video() did exactly that -- on master too -- until M56 gave the
  # seam `hardware` and let it check first, the shape crop_video() already had.
  # The encoder pool is pinned so the cell does not depend on this machine.
  input <- make_input()
  withr::local_options(tidymedia.nvenc_encoders = "h264_nvenc")
  cases <- list(
    list(lbl = "standardize_video", f = function() standardize_video(
      input, "o.mp4", video_codec = "libx264 -evil", hardware = "nvenc",
      run = FALSE)),
    list(lbl = "standardize_video_batch", f = function() standardize_video_batch(
      tibble::tibble(input = input, output = "o.mp4"),
      video_codec = "libx264 -evil", hardware = "nvenc", run = FALSE,
      parallel = FALSE)),
    # The sibling that was already right, kept as the control: it passes for the
    # same reason the two above now do, and would go red with them.
    list(lbl = "crop_video", f = function() crop_video(
      input, "o.mp4", 32, 32, video_codec = "libx264 -evil",
      hardware = "nvenc", run = FALSE))
  )
  for (case in cases) {
    cnd <- tryCatch({ case$f(); NULL }, error = function(e) e)
    expect_true(inherits(cnd, "error"), label = paste(case$lbl, "aborts"))
    if (!inherits(cnd, "error")) next
    msg <- cli::ansi_strip(conditionMessage(cnd))
    expect_match(msg, "video_codec", fixed = TRUE,
                 label = paste(case$lbl, "names video_codec"))
    expect_match(msg, "single clean token", fixed = TRUE,
                 label = paste(case$lbl, "is the token complaint"))
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

  covered <- sort(unlist(lapply(codec_family_pairs(), function(p) {
    paste(p$verb, p$args)
  })))
  # verify_media()'s same-named arguments are expected probe VALUES, not codec
  # settings, so guarding them would change the contract rather than validate it.
  excluded <- c("verify_media video_codec", "verify_media audio_codec")

  expect_setequal(found, c(covered, excluded))
})

test_that("M41's guards still refuse a bad value when NULL is legal beside it", {
  # M41 pinned each verb's per-verb NULL meaning here, when the family had no
  # single answer. D022 gave it one, and those pins moved whole to
  # test-codec-null-na-semantics.R -- two files asserting NULL semantics is how
  # the two drift apart.
  #
  # What stays here is the interaction this file owns: `allow_null = TRUE` is a
  # widening, and a guard written as "accept NULL" is one typo away from
  # "accept anything nullish". NA is the value that distinguishes them, on the
  # two verbs M42 widened.
  input <- make_input()
  expect_error(
    extract_audio(input, "a.aac", audio_codec = NA, run = FALSE),
    "audio_codec"
  )
  expect_error(
    anonymize_video(input, "out.mp4",
                    regions = data.frame(x = 0, y = 0, width = 32, height = 32),
                    video_codec = NA, run = FALSE),
    "video_codec"
  )
})
