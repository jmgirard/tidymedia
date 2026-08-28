# M62: a missing input file is refused at the front door, in both forms.
# M63: so is one that is there and cannot be read, at the same site.
#
# The literal expectations below are written out rather than snapshotted
# deliberately: a testthat snapshot records itself on first run, so it can
# witness neither a pre-change string nor a wording moved on purpose.

test_that("the one-path rendering is pinned byte-for-byte", {
  # M62's `does not exist` moved at M63: the predicate is now readability, and
  # a file that is there but unopenable is not absent. The pin stays, on the
  # new string.
  expect_error(
    check_file_readable("nope.mp4", arg = "infile"),
    "^`infile` can't be found or read: 'nope\\.mp4'\\.$"
  )
  expect_error(
    check_file_readable("nope.mp4", arg = "file"),
    "^`file` can't be found or read: 'nope\\.mp4'\\.$"
  )
})

test_that("the existence spelling keeps its own wording for its two callers", {
  # check_file_exists() is NOT the input front door and did not move: its two
  # callers (verify_media()'s `file`, write_mediainfo_template()'s
  # `templatefile`) have no downstream refusal to agree with, so they keep
  # M62's exact string. This is the byte-for-byte pin captured from merged
  # master before the shared checker landed.
  expect_error(
    check_file_exists("nope.mp4", arg = "file"),
    "^`file` does not exist: 'nope\\.mp4'\\.$"
  )
})

test_that("a one-path check reports through the shared checker", {
  # check_file_readable() must delegate, not carry its own copy of the abort.
  expect_error(check_paths_readable("nope.mp4", arg = "infile"),
               "^`infile` can't be found or read: 'nope\\.mp4'\\.$")
})

test_that("a multi-path check leads with the count and names every path", {
  err <- rlang::catch_cnd(
    check_paths_readable(c("a.mp4", "b.mp4"), arg = "jobs$input")
  )
  msg <- conditionMessage(err)
  expect_match(msg, "`jobs$input` names 2 files that can't be found or read.",
               fixed = TRUE)
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
    check_paths_readable(c(present, "gone.mp4"), arg = "jobs$input")
  )
  expect_match(conditionMessage(err),
               "`jobs$input` names 1 file that can't be found or read.",
               fixed = TRUE)
  expect_match(conditionMessage(err), "'gone.mp4'", fixed = TRUE)
})

test_that("the checker passes a readable path through invisibly", {
  present <- withr::local_tempfile(fileext = ".mp4")
  file.create(present)
  expect_silent(check_paths_readable(present, arg = "infile"))
  expect_identical(
    check_paths_readable(c(present, present), arg = "jobs$input"),
    c(present, present))
})

test_that("the front door's bad-input abort is written at one site", {
  bodies <- tm_namespace_bodies()
  writes <- names(bodies)[grepl("can't be found or read", bodies, fixed = TRUE)]
  expect_identical(writes, "check_paths_readable")
})

test_that("the existence abort is written at one site of its own", {
  bodies <- tm_namespace_bodies()
  writes <- names(bodies)[grepl("does not exist", bodies, fixed = TRUE)]
  expect_identical(writes, "check_file_exists")
})

test_that("no input refusal is worded outside the shared site", {
  # M62 left ffm_files() wording a second input refusal, with a predicate of its
  # own, and pinned that residual here so M63 would have something to delete.
  # This is that deletion, asserted: the retired wording is gone from the
  # namespace, and ffm_files() carries no refusal text at all.
  bodies <- tm_namespace_bodies()
  expect_identical(names(bodies)[grepl("find or read", bodies, fixed = TRUE)],
                   character(0))
  expect_identical(names(bodies)[grepl("Not readable", bodies, fixed = TRUE)],
                   character(0))
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
    expect_match(conditionMessage(err), "can't be found or read", fixed = TRUE,
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
    expect_match(conditionMessage(err), "can't be found or read", fixed = TRUE,
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
    check_paths_readable(rep("gone.mp4", 20), arg = "jobs$input")
  )
  expect_match(conditionMessage(err),
               "`jobs$input` names 1 file that can't be found or read.",
               fixed = TRUE)
  jobs <- tibble::tibble(
    input = rep("m62-gone.mp4", 3),
    output = file.path(tempdir(), sprintf("m62-dup-%d.mp4", 1:3)))
  err <- rlang::catch_cnd(
    crop_video_batch(jobs, width = 10, height = 10, run = FALSE))
  expect_match(conditionMessage(err),
               "names 1 file that can't be found or read", fixed = TRUE)
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
  # A factor carries its paths as levels; handed to the predicate raw it raised
  # an unattributed base error blamed on the predicate -- worse in both message
  # and blame than what the pipeline said before the sweep existed (review F1).
  expect_match(
    conditionMessage(rlang::catch_cnd(check_paths_readable(
      factor(c("m62-a.mp4", "m62-b.mp4")), arg = "jobs$input"))),
    "names 2 files that can't be found or read", fixed = TRUE)
  jobs <- data.frame(input = factor(c("m62-fa.mp4", "m62-fb.mp4")),
                     start = 0, end = 1)
  err <- rlang::catch_cnd(segment_video_batch(jobs, run = FALSE))
  expect_match(conditionMessage(err), "can't be found or read", fixed = TRUE)
  expect_match(paste(deparse(conditionCall(err)), collapse = " "),
               "segment_video_batch(", fixed = TRUE)
})

# M63 -- the front door and the pipeline refuse the same paths, and the wording
# no longer asserts absence of a file that is there.

# The corpus. A readable directory is in it because file.exists() and
# file.access(mode = 4) BOTH accept one (measured 2026-08-08), so it is the case
# where agreement means accepting rather than refusing -- a property test that
# only ever compared refusals could not see a guard that started refusing
# directories. An UNREADABLE directory is the case that splits them, and it is
# here because the readable one alone would have left the claim "a directory
# passes both predicates" reading as true of directories generally (M63 review).
tm_path_corpus <- function(dir) {
  present <- file.path(dir, "m63-present.mp4")
  file.create(present)
  subdir <- file.path(dir, "m63-subdir")
  dir.create(subdir)
  shut <- file.path(dir, "m63-shut-dir")
  dir.create(shut)
  Sys.chmod(shut, "000")
  list(present = present,
       absent = file.path(dir, "m63-absent.mp4"),
       unreadable = tm_unreadable_path(dir),
       directory = subdir,
       unreadable_directory = if (file.access(shut, mode = 4) != 0) shut)
}

# Did this call refuse the INPUT, as opposed to compiling or tripping on
# something else? Classified from the wording, so a spec that aborts for its own
# reason cannot read as a refusal (the failure-identity rule).
tm_refused_input <- function(expr) {
  cnd <- rlang::catch_cnd(expr)
  !is.null(cnd) && inherits(cnd, "error") &&
    grepl("can't be found or read", conditionMessage(cnd), fixed = TRUE)
}

test_that("the front door and ffm_files() refuse the same set of paths", {
  dir <- withr::local_tempdir()
  corpus <- tm_path_corpus(dir)
  tm_require_unreadable(corpus$unreadable)
  out <- file.path(dir, "m63-out.mp4")
  corpus <- corpus[!vapply(corpus, is.null, logical(1))]
  for (case in names(corpus)) {
    p <- corpus[[case]]
    door <- tm_refused_input(check_paths_readable(p, arg = "infile"))
    pipe <- tm_refused_input(ffm_files(p, out))
    verb <- tm_refused_input(standardize_video(p, out, run = FALSE))
    expect_identical(door, pipe, info = case)
    expect_identical(door, verb, info = case)
  }
  # The corpus must also SPLIT, or the identity above holds vacuously over a
  # corpus that is refused everywhere or nowhere.
  refused <- vapply(corpus, function(p)
    tm_refused_input(check_paths_readable(p, arg = "infile")), logical(1))
  expect_identical(refused[["absent"]], TRUE)
  expect_identical(refused[["unreadable"]], TRUE)
  expect_identical(refused[["present"]], FALSE)
  # A readable directory is accepted and an unreadable one is refused, which is
  # the predicate applied to a directory rather than a policy about directories:
  # whether an input slot should take one at all is open (M63's D-entry).
  expect_identical(refused[["directory"]], FALSE)
  if (!is.null(corpus$unreadable_directory)) {
    expect_identical(refused[["unreadable_directory"]], TRUE)
  }
})

test_that("the message does not assert absence of a file that is there", {
  dir <- withr::local_tempdir()
  p <- tm_unreadable_path(dir)
  tm_require_unreadable(p)
  msg <- conditionMessage(rlang::catch_cnd(
    check_paths_readable(p, arg = "infile")))
  expect_false(grepl("not exist", msg, fixed = TRUE))
  expect_match(msg, "`infile` can't be found or read:", fixed = TRUE)
  expect_match(msg, basename(p), fixed = TRUE)
  # And the column form, whose summary is the other of the two renderings.
  msg <- conditionMessage(rlang::catch_cnd(
    check_paths_readable(c(p, p), arg = "jobs$input")))
  expect_false(grepl("not exist", msg, fixed = TRUE))
  expect_match(msg, "`jobs$input` names 1 file that can't be found or read.",
               fixed = TRUE)
})

test_that("every verb refuses an unreadable input at its own front door", {
  # Both walk-derived sets, through M62's specs, so the walk still fixes
  # membership and this criterion cannot quietly cover fewer verbs than that
  # one does.
  dir <- withr::local_tempdir()
  p <- tm_unreadable_path(dir)
  tm_require_unreadable(p)
  verbs <- input_guard_verbs()
  specs <- input_guard_specs()
  for (verb in c(verbs$fanout, verbs$scalar)) {
    err <- rlang::catch_cnd(specs[[verb]](p))
    expect_match(conditionMessage(err), "can't be found or read", fixed = TRUE,
                 info = verb)
    shown <- paste(deparse(conditionCall(err)), collapse = " ")
    expect_match(shown, paste0(verb, "("), fixed = TRUE, info = verb)
    # The residual M62 disclosed, asserted gone: this refusal used to come from
    # inside the fan-out, blamed on ffm_files() or on purrr::pmap().
    expect_false(grepl("pmap", shown, fixed = TRUE), info = verb)
    expect_false(grepl("ffm_files", shown, fixed = TRUE), info = verb)
  }
})

test_that("the checker blames its caller, not itself", {
  caller <- function(p) check_paths_readable(p, arg = "infile")
  err <- rlang::catch_cnd(caller("nope.mp4"))
  expect_match(paste(deparse(conditionCall(err)), collapse = " "),
               "caller(", fixed = TRUE)
})

# M080 -------------------------------------------------------------------

test_that("the abort names only the carriers actually holding a bad path", {
  # `col` may name several carriers, swept in ONE call so a row missing both
  # names both (M62 review F2). The count was right and the blame was not:
  # `arg` was the whole `col` vector, so a row whose `main` is fine and whose
  # `overlay` is missing still read `` `jobs$main` and `jobs$overlay` `` (M62
  # N3).
  dir <- withr::local_tempdir()
  good <- file.path(dir, "good.mp4")
  file.create(good)
  out <- file.path(dir, "out.mp4")
  unreadable <- tm_unreadable_path(dir)
  tm_require_unreadable(unreadable)

  # Both halves of D041's predicate: a path that is not there, and one that is
  # there and cannot be opened. The blame must not depend on which it is.
  for (bad in list(absent = "gone.mp4", unreadable = unreadable)) {
    msg <- conditionMessage(rlang::catch_cnd(picture_in_picture_batch(
      tibble::tibble(main = good, overlay = bad, output = out), run = FALSE)))
    expect_match(msg,
                 "`jobs$overlay` names 1 file that can't be found or read.",
                 fixed = TRUE)
    expect_false(grepl("jobs$main", msg, fixed = TRUE))

    msg <- conditionMessage(rlang::catch_cnd(picture_in_picture_batch(
      tibble::tibble(main = bad, overlay = good, output = out), run = FALSE)))
    expect_match(msg, "`jobs$main` names 1 file that can't be found or read.",
                 fixed = TRUE)
    expect_false(grepl("jobs$overlay", msg, fixed = TRUE))

    # Both bad: the one call still names both carriers, which is what the
    # single-call sweep exists for.
    msg <- conditionMessage(rlang::catch_cnd(picture_in_picture_batch(
      tibble::tibble(main = bad, overlay = bad, output = out), run = FALSE)))
    expect_match(msg, "`jobs$main` and `jobs$overlay` name 1 file", fixed = TRUE)
  }
})

test_that("a duplicated absent input reports the path, not the duplication", {
  # With no `output` column a verb derives one name per input, so it rejects
  # duplicated inputs before deriving -- and that rejection ran ABOVE the path
  # sweep, so a table whose rows all name the same file that is not there was
  # told about the duplication, which names nothing the caller can fix (M62
  # N7). NEWS.md's "one path typed wrong the same way in twenty rows is one
  # missing file" was observable only off the explicit-output path.
  #
  # The verb set is the walk's, not a list: every verb reaching the shared
  # helper is exercised, and a verb the walk returns with no call shape here
  # fails rather than being skipped.
  graph <- tm_call_graph()
  exported <- sort(intersect(getNamespaceExports("tidymedia"), names(graph)))
  verbs <- exported[vapply(exported, function(v)
    tm_reaches(graph, v, "reject_duplicate_inputs"), logical(1))]
  expect_gt(length(verbs), 0)

  dir <- withr::local_tempdir()
  good <- file.path(dir, "good.mp4")
  file.create(good)
  boxes <- data.frame(x = 0, y = 0, width = 10, height = 10)
  shapes <- list(
    anonymize_video_batch = function(input) anonymize_video_batch(
      tibble::tibble(input = input,
                     regions = lapply(input, function(...) boxes)),
      run = FALSE),
    standardize_video_batch = function(input) standardize_video_batch(
      tibble::tibble(input = input), run = FALSE),
    normalize_audio_batch = function(input) normalize_audio_batch(
      tibble::tibble(input = input), run = FALSE)
  )
  expect_identical(sort(setdiff(verbs, names(shapes))), character(0))

  for (verb in verbs) {
    msg <- conditionMessage(rlang::catch_cnd(
      shapes[[verb]](rep("gone.mp4", 2))))
    expect_match(msg, "`jobs$input` names 1 file that can't be found or read.",
                 fixed = TRUE, info = verb)
    expect_false(grepl("duplicated", msg, fixed = TRUE), info = verb)

    # And the case where the new report must stay silent: readable inputs,
    # duplicated, still get the duplication message they always got.
    msg <- conditionMessage(rlang::catch_cnd(shapes[[verb]](rep(good, 2))))
    expect_match(msg, "duplicated input paths but no output column",
                 fixed = TRUE, info = verb)
  }
})

test_that("the duplicated-input abort is worded at one site", {
  # The three verbs carried a copy each. One site, so a fourth verb inherits
  # the wording and the order rather than restating them.
  bodies <- tm_namespace_bodies()
  hits <- names(bodies)[grepl("has duplicated", bodies, fixed = TRUE)]
  expect_identical(hits, "reject_duplicate_inputs")
})
