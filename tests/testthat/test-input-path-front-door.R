# M62: a missing input file is refused at the front door, in both forms.
#
# The literal expectations below were captured from merged master before the
# shared checker landed (AC2), so they fail if the one-path rendering moves by
# a single byte. They are written out rather than snapshotted deliberately: a
# testthat snapshot records itself on first run and so cannot witness a
# pre-change string.

test_that("the one-path rendering is unchanged from before the shared checker", {
  expect_error(
    check_file_exists("nope.mp4", arg = "infile"),
    "^`infile` does not exist: 'nope\\.mp4'\\.$"
  )
  expect_error(
    check_file_exists("nope.mp4", arg = "file"),
    "^`file` does not exist: 'nope\\.mp4'\\.$"
  )
})

test_that("a one-path check reports through the shared checker", {
  # check_file_exists() must delegate, not carry its own copy of the abort.
  expect_error(check_paths_exist("nope.mp4", arg = "infile"),
               "^`infile` does not exist: 'nope\\.mp4'\\.$")
})

test_that("a multi-path check leads with the count and names every path", {
  err <- rlang::catch_cnd(
    check_paths_exist(c("a.mp4", "b.mp4"), arg = "jobs$input")
  )
  msg <- conditionMessage(err)
  expect_match(msg, "`jobs$input` names 2 files that do not exist.", fixed = TRUE)
  expect_match(msg, "'a.mp4'", fixed = TRUE)
  expect_match(msg, "'b.mp4'", fixed = TRUE)
})

test_that("a vector with one missing path still uses the count form", {
  # The branch is on the ARGUMENT's arity, not the missing count: a jobs column
  # of five paths with one missing is not a scalar `infile`, and saying
  # "`jobs$input` does not exist" would misdescribe the column.
  present <- withr::local_tempfile(fileext = ".mp4")
  file.create(present)
  err <- rlang::catch_cnd(
    check_paths_exist(c(present, "gone.mp4"), arg = "jobs$input")
  )
  expect_match(conditionMessage(err),
               "`jobs$input` names 1 file that does not exist.", fixed = TRUE)
  expect_match(conditionMessage(err), "'gone.mp4'", fixed = TRUE)
})

test_that("the checker passes a readable path through invisibly", {
  present <- withr::local_tempfile(fileext = ".mp4")
  file.create(present)
  expect_silent(check_paths_exist(present, arg = "infile"))
  expect_identical(check_paths_exist(c(present, present), arg = "jobs$input"),
                   c(present, present))
})

test_that("the front door's missing-input abort is written at one site", {
  bodies <- tm_namespace_bodies()
  writes <- names(bodies)[grepl("does not exist", bodies, fixed = TRUE)]
  expect_identical(writes, "check_paths_exist")
})

test_that("ffm_files() is the only other place an input refusal is worded", {
  # M62 leaves ffm_files()' READABILITY refusal where it is -- its predicate is
  # file.access(mode = 4), not file.exists(), and unifying the two is M63's
  # scope. Pinning the residual here means M63 has something to delete, and
  # means a third wording cannot appear unnoticed in the meantime.
  bodies <- tm_namespace_bodies()
  writes <- names(bodies)[grepl("find or read", bodies, fixed = TRUE)]
  expect_setequal(writes, c("ffm", "ffm_files"))
})

test_that("the derived verb sets cover every spec and vice versa", {
  # The walk fixes membership; the specs supply only call SHAPE. A verb the walk
  # returns with no spec is a gap in the evidence, and a spec for a verb the
  # walk does not return is a spec that has stopped describing the package.
  verbs <- input_guard_verbs()
  specs <- input_guard_specs()
  expect_setequal(c(verbs$fanout, verbs$scalar), names(specs))
  # The walk must also be finding real verbs, not an empty set that would make
  # every criterion below vacuously true.
  expect_gt(length(verbs$fanout), 10)
  expect_gt(length(verbs$scalar), 10)
})

test_that("the walk excludes a name that only appears inside a message string", {
  # ffm_manifest()'s body mentions `ffm_batch(` inside a cli hint. A deparsed
  # substring search calls it a fan-out verb; a call-node walk does not.
  expect_false("ffm_manifest" %in% input_guard_verbs()$fanout)
  expect_true(grepl("ffm_batch(",
                    paste(deparse(body(ffm_manifest)), collapse = " "),
                    fixed = TRUE))
})

test_that("every fan-out verb refuses a missing input at its own front door", {
  verbs <- input_guard_verbs()
  specs <- input_guard_specs()
  for (verb in verbs$fanout) {
    err <- rlang::catch_cnd(specs[[verb]]("m62-absent-input.mp4"))
    # WHICH failure, not that one occurred: a malformed spec aborts for its own
    # reason and would otherwise satisfy the call check vacuously (M54/M58).
    expect_match(conditionMessage(err), "not exist", fixed = TRUE,
                 info = verb)
    expect_match(paste(deparse(conditionCall(err)), collapse = " "),
                 paste0(verb, "("), fixed = TRUE, info = verb)
  }
})

test_that("every scalar verb refuses a missing input at its own front door", {
  verbs <- input_guard_verbs()
  specs <- input_guard_specs()
  for (verb in verbs$scalar) {
    err <- rlang::catch_cnd(specs[[verb]]("m62-absent-input.mp4"))
    expect_match(conditionMessage(err), "not exist", fixed = TRUE,
                 info = verb)
    expect_match(paste(deparse(conditionCall(err)), collapse = " "),
                 paste0(verb, "("), fixed = TRUE, info = verb)
  }
})

test_that("no verb reports the missing input from inside the fan-out", {
  # The defect this milestone removes, asserted as an absence against the two
  # names that used to show: purrr's pmap and the Layer-1 builder.
  verbs <- input_guard_verbs()
  specs <- input_guard_specs()
  for (verb in c(verbs$fanout, verbs$scalar)) {
    err <- rlang::catch_cnd(specs[[verb]]("m62-absent-input.mp4"))
    shown <- paste(deparse(conditionCall(err)), collapse = " ")
    expect_false(grepl("pmap", shown, fixed = TRUE), info = verb)
    expect_false(grepl("ffm_files", shown, fixed = TRUE), info = verb)
  }
})

# The three defects the M62 review returned, each pinned where it was measured:
# at the shared checker AND at the verb whose front door showed it, because the
# checker being right is not the same claim as every carrier reaching it right.

test_that("one path repeated across rows is one missing file, not several", {
  # No unique(): a single typo shared by twenty rows read as twenty files, and
  # on the `inputs` list-column the flattening made it worse (review F3).
  err <- rlang::catch_cnd(
    check_paths_exist(rep("gone.mp4", 20), arg = "jobs$input")
  )
  expect_match(conditionMessage(err),
               "`jobs$input` names 1 file that does not exist.", fixed = TRUE)
  jobs <- tibble::tibble(
    input = rep("m62-gone.mp4", 3),
    output = file.path(tempdir(), sprintf("m62-dup-%d.mp4", 1:3)))
  err <- rlang::catch_cnd(
    crop_video_batch(jobs, width = 10, height = 10, run = FALSE))
  expect_match(conditionMessage(err),
               "names 1 file that does not exist", fixed = TRUE)
})

test_that("a verb with two input columns names both missing files", {
  # Two sweeps, one per column, aborted on the first and hid the second, so a
  # row missing both named only `main` (review F2).
  jobs <- tibble::tibble(main = "m62-gone-main.mp4",
                         overlay = "m62-gone-overlay.mp4",
                         output = file.path(tempdir(), "m62-pip.mp4"))
  err <- rlang::catch_cnd(picture_in_picture_batch(jobs, run = FALSE))
  msg <- conditionMessage(err)
  expect_match(msg, "`jobs$main` and `jobs$overlay` name 2 files", fixed = TRUE)
  expect_match(msg, "m62-gone-main.mp4", fixed = TRUE)
  expect_match(msg, "m62-gone-overlay.mp4", fixed = TRUE)
})

test_that("a factor path column keeps the abort attributed", {
  # A factor carries its paths as levels; handed to file.exists() raw it raised
  # the base error `invalid 'file' argument`, blamed on file.exists() -- worse
  # in both message and blame than what the pipeline said before the sweep
  # existed (review F1).
  expect_match(
    conditionMessage(rlang::catch_cnd(
      check_paths_exist(factor(c("m62-a.mp4", "m62-b.mp4")), arg = "jobs$input"))),
    "names 2 files that do not exist", fixed = TRUE)
  jobs <- data.frame(input = factor(c("m62-fa.mp4", "m62-fb.mp4")),
                     start = 0, end = 1)
  err <- rlang::catch_cnd(segment_video_batch(jobs, run = FALSE))
  expect_match(conditionMessage(err), "not exist", fixed = TRUE)
  expect_match(paste(deparse(conditionCall(err)), collapse = " "),
               "segment_video_batch(", fixed = TRUE)
})

test_that("the checker blames its caller, not itself", {
  caller <- function(p) check_paths_exist(p, arg = "infile")
  err <- rlang::catch_cnd(caller("nope.mp4"))
  expect_match(paste(deparse(conditionCall(err)), collapse = " "),
               "caller(", fixed = TRUE)
})
