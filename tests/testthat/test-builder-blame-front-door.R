# A value the verb hands to a Layer-1 builder is refused at the verb's front
# door, so the abort names the verb the caller typed (M64).
#
# Before this milestone `crop_video(f, "o.mp4", width = 0)` printed
# "Error in `ffm_crop()`" -- a builder the caller never called -- because
# check_dim()'s `call = rlang::caller_env()` default resolves to the builder's
# own frame and no `ffm_*` builder takes a `call` argument. The batch siblings
# leaked differently: their values are read inside ffm_batch() -> purrr::pmap(),
# so the abort read "Error in `purrr::pmap(jobs, .f, ...)` / In index: 1".
#
# The fix is the shape M59 gave crop_video_batch(): the verb calls the SAME
# shared checker at its own front door, so caller_env() lands on the verb.
# Threading `call` through the exported builders was rejected at M59-D1 and
# again here (see the milestone's Decisions).
#
# The cells come from blame_specs() in helper-blame-specs.R -- one declaration
# shared with data-raw/blame-baseline.R, the list's one other consumer (the
# other data-raw scripts declare no cell list of their own).
# blamed_verb() and catch_call() come from helper-blame.R.
#
# Nothing here needs FFmpeg: every probe runs at `run = FALSE` and every cell
# aborts before a command is built. The encoder seam is held EMPTY so a message
# mentioning nvenc availability is a failure rather than a coincidence.

test_that("the spec list names only arguments its verbs actually have", {
  # The completeness reader (AC1). It cannot prove the list complete -- it
  # proves no cell probes an argument that does not exist, which is the failure
  # that would let a cell pass while measuring nothing.
  expect_identical(blame_spec_defects(blame_specs(make_input())), character(0))
})

test_that("the completeness reader detects the defects it exists for", {
  # AC5's harness mutates the reader and requires a red. Without this test a
  # neutered reader -- one returning character(0) unconditionally -- still
  # passes the empty check above, since the real list HAS no defects; only a
  # planted defect can tell a clean list from a reader that stopped looking.
  specs <- blame_specs(make_input())
  foreign <- specs[[1]]
  foreign$id <- "planted/foreign-argument"
  foreign$argument <- "no_such_formal"
  no_col <- NULL
  for (cell in specs) {
    if (identical(cell$delivery, "column")) { no_col <- cell; break }
  }
  no_col$id <- "planted/missing-column"
  no_col$args$jobs[[no_col$argument]] <- NULL
  defects <- blame_spec_defects(c(specs, list(foreign, no_col)))
  expect_match(defects, "planted/foreign-argument", fixed = TRUE, all = FALSE)
  expect_match(defects, "planted/missing-column", fixed = TRUE, all = FALSE)
})

test_that("a builder-bound value blames the verb the user called", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
  input <- make_input()
  outdir <- withr::local_tempdir()
  for (cell in blame_specs(input, outdir)) {
    cnd <- catch_call(cell$verb, cell$args)
    expect_s3_class(cnd, "rlang_error")
    # Which failure, never that one occurred: a schema error or a missing
    # column would otherwise satisfy a bare expectation with the sweep deleted.
    expect_match(conditionMessage(cnd), cell$own, info = cell$id)
    if (!is.null(cell$absent)) {
      expect_no_match(conditionMessage(cnd), cell$absent, fixed = TRUE,
                      info = cell$id)
    }
    expect_identical(blamed_verb(cnd), cell$verb, info = cell$id)
    # The blame lives in conditionCall() -- what the console prints as
    # "Error in `<verb>()`". Deparsing the whole call catches an internal's
    # name leaking there even when the function part looks right.
    deparsed <- paste(deparse(conditionCall(cnd)), collapse = "")
    for (leak in c("pmap", "_pipeline(", "ffm_")) {
      expect_no_match(deparsed, leak, fixed = TRUE, info = cell$id)
      expect_no_match(conditionMessage(cnd), leak, fixed = TRUE, info = cell$id)
    }
    expect_no_match(conditionMessage(cnd), "In index:", fixed = TRUE,
                    info = cell$id)
  }
})

test_that("a bad batch value reports before a missing nvenc encoder", {
  # The one reporting order M64 reassigns (M64-D2, D036): on the two `_batch`
  # verbs whose sweep is new, a value wrong on every machine now outranks an
  # encoder missing on this one -- the answer crop_video_batch() has given its
  # width/height since M59. The encoder pool is EMPTY, so the nvenc abort is
  # live on every one of these calls and losing is the finding.
  withr::local_options(tidymedia.hardware_encoders = character(0))
  input <- make_input()
  jobs <- tibble::tibble(input = input, output = "o.mp4")

  cnd <- catch_call("crop_video_batch",
                    list(jobs = jobs, width = 160, height = 120, x = -1,
                         hardware = "nvenc"))
  expect_match(conditionMessage(cnd), "`x` must be a single FFmpeg expression")
  cnd <- catch_call("standardize_video_batch",
                    list(jobs = jobs, width = 0, hardware = "nvenc"))
  expect_match(conditionMessage(cnd),
               "`width` must be a single FFmpeg expression")
  cnd <- catch_call("standardize_video_batch",
                    list(jobs = jobs, pixel_format = "yuv 420p",
                         hardware = "nvenc"))
  expect_match(conditionMessage(cnd),
               "`pixel_format` must be a single clean token")
})

# --- The M65 extension: region, overlay-scale and loudness cells -------------
# Same grid, next family (blame_specs_m65() in helper-blame-specs-m65.R): the
# values anonymize_video, picture_in_picture and normalize_audio hand to
# ffm_drawbox() / ffm_overlay() / ffm_loudnorm().

test_that("the M65 spec list names only arguments its verbs actually have", {
  expect_identical(blame_spec_defects_m65(blame_specs_m65(make_input())),
                   character(0))
})

test_that("the M65 completeness reader detects the defects it exists for", {
  # AC6 mutates the reader and requires a red; only planted defects can tell a
  # clean list from a reader that stopped looking. The planted trio covers the
  # reader's third clause too -- the region-field check M64's reader has no
  # equivalent of.
  specs <- blame_specs_m65(make_input())
  foreign <- NULL
  for (cell in specs) {
    if (!identical(cell$argument, "regions")) { foreign <- cell; break }
  }
  foreign$id <- "planted/foreign-argument"
  foreign$argument <- "no_such_formal"
  no_col <- NULL
  for (cell in specs) {
    if (identical(cell$delivery, "column") &&
        !identical(cell$argument, "regions")) { no_col <- cell; break }
  }
  no_col$id <- "planted/missing-column"
  no_col$args$jobs[[no_col$argument]] <- NULL
  bad_field <- NULL
  for (cell in specs) {
    if (identical(cell$argument, "regions")) { bad_field <- cell; break }
  }
  bad_field$id <- "planted/foreign-region-field"
  bad_field$field <- "no_such_field"
  defects <- blame_spec_defects_m65(c(specs, list(foreign, no_col, bad_field)))
  expect_match(defects, "planted/foreign-argument", fixed = TRUE, all = FALSE)
  expect_match(defects, "planted/missing-column", fixed = TRUE, all = FALSE)
  expect_match(defects, "planted/foreign-region-field", fixed = TRUE,
               all = FALSE)
})

# The shared cell assertions: which failure (never that one occurred), whose
# blame, and no internal's name leaking through the deparsed call.
expect_blames_verb_m65 <- function(cell) {
  cnd <- catch_call(cell$verb, cell$args)
  expect_s3_class(cnd, "rlang_error")
  expect_match(conditionMessage(cnd), cell$own, info = cell$id)
  if (!is.null(cell$absent)) {
    expect_no_match(conditionMessage(cnd), cell$absent, fixed = TRUE,
                    info = cell$id)
  }
  expect_identical(blamed_verb(cnd), cell$verb, info = cell$id)
  deparsed <- paste(deparse(conditionCall(cnd)), collapse = "")
  for (leak in c("pmap", "_pipeline(", "ffm_")) {
    expect_no_match(deparsed, leak, fixed = TRUE, info = cell$id)
    expect_no_match(conditionMessage(cnd), leak, fixed = TRUE, info = cell$id)
  }
  expect_no_match(conditionMessage(cnd), "In index:", fixed = TRUE,
                  info = cell$id)
}

test_that("a region, overlay or loudness value blames the verb the user called", {
  withr::local_options(tidymedia.hardware_encoders = character(0))
  for (cell in blame_specs_m65(make_input())) {
    if (isTRUE(cell$needs_ffmpeg)) next  # the two-pass block below
    expect_blames_verb_m65(cell)
  }
})

test_that("a two-pass loudness value blames the verb, before the analysis pass", {
  # The `two_pass = TRUE` cells: the sweep aborts before
  # run_loudnorm_analysis() spawns FFmpeg, but with the sweep deleted these
  # calls DO reach it (D034's shape), so they run only where the binary exists
  # and their evidence is local-only (the milestone's evidence note; T5).
  skip_if_no_ffmpeg()
  withr::local_options(tidymedia.hardware_encoders = character(0))
  for (cell in blame_specs_m65(make_input())) {
    if (!isTRUE(cell$needs_ffmpeg)) next
    expect_blames_verb_m65(cell)
  }
})

test_that("a bad region or scale value reports before a missing nvenc encoder", {
  # M65's slice of the M64-D2 reordering (D036): on the two `_batch` verbs
  # whose new sweep sits above check_nvenc_available(), a value wrong on every
  # machine outranks an encoder missing on this one. The encoder pool is EMPTY,
  # so the nvenc abort is live on both calls and losing is the finding.
  # (normalize_audio_batch() has no hardware argument, so it has no such cell.)
  withr::local_options(tidymedia.hardware_encoders = character(0))
  input <- make_input()

  cnd <- catch_call("anonymize_video_batch", list(
    jobs = tibble::tibble(
      input = input, output = "o.mp4",
      regions = list(data.frame(x = 0, y = 0, width = 0, height = 10))),
    hardware = "nvenc"))
  expect_match(conditionMessage(cnd),
               "`width` must be a single FFmpeg expression")

  cnd <- catch_call("picture_in_picture_batch", list(
    jobs = tibble::tibble(main = input, overlay = input, output = "o.mp4"),
    scale = 2, hardware = "nvenc", video_codec = "libx264"))
  expect_match(conditionMessage(cnd),
               "`scale` must be greater than 0 and at most 1")
})

test_that("both forms refuse the same value with the same guard", {
  # AC2: compared cell-for-cell rather than asserted independently, so a fix
  # landing on one form only is red here even when both forms abort.
  withr::local_options(tidymedia.hardware_encoders = character(0))
  input <- make_input()
  outdir <- withr::local_tempdir()
  specs <- blame_specs(input, outdir)
  by_arg <- split(specs, vapply(specs, function(s) {
    paste(sub("_batch$", "", s$verb), s$argument, sep = "/")
  }, character(1)))

  for (key in names(by_arg)) {
    group <- by_arg[[key]]
    # Every argument in the grid must be probed in both forms; a family that
    # lost its scalar or its batch cell would otherwise compare nothing.
    forms <- vapply(group, function(s) s$form, character(1))
    expect_setequal(unique(forms), c("scalar", "batch"))
    msgs <- vapply(group, function(cell) {
      conditionMessage(catch_call(cell$verb, cell$args))
    }, character(1))
    # The messages carry no verb name, so the WHOLE message must match across
    # forms -- bullets included, which is what catches an `inclusive =`
    # divergence between a scalar sweep and its batch sibling. The previous
    # spelling normalized every message to one identical byte (base sub() has
    # no lazy quantifier), comparing nothing (M64 review F12). Since M66 the
    # batch COLUMN cells append the row locator the scalar form must not
    # carry, so the comparison runs after strip_row_locator() -- a remover
    # verified in both directions by test-check-batch-cell.R, not a
    # normalizer of the kind F12 warned about: a column cell must still
    # CARRY the locator (asserted here, so a neutered locator is red), and
    # everything outside the locator still compares byte-for-byte.
    for (k in seq_along(group)) {
      cell <- group[[k]]
      if (cell$form == "batch" && identical(cell$delivery, "column")) {
        expect_match(cli::ansi_strip(msgs[[k]]),
                     "First offending jobs row: [0-9]+\\.$",
                     info = cell$id)
      } else {
        expect_no_match(cli::ansi_strip(msgs[[k]]),
                        "First offending jobs row", info = cell$id)
      }
    }
    expect_length(unique(strip_row_locator(cli::ansi_strip(msgs))), 1L)
  }
})
