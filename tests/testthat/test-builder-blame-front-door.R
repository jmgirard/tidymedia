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
# shared with data-raw/blame-baseline.R and data-raw/blame-precedence.R.
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
  withr::local_options(tidymedia.nvenc_encoders = character(0))
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

test_that("both forms refuse the same value with the same guard", {
  # AC2: compared cell-for-cell rather than asserted independently, so a fix
  # landing on one form only is red here even when both forms abort.
  withr::local_options(tidymedia.nvenc_encoders = character(0))
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
    # The verb's own name legitimately differs between the forms; the guard's
    # sentence must not.
    normalized <- unique(sub("^.*?(must be [^\n]*).*$", "\\1", msgs))
    expect_length(normalized, 1L)
  }
})
