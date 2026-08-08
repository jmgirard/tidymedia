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

test_that("the checker blames its caller, not itself", {
  caller <- function(p) check_paths_exist(p, arg = "infile")
  err <- rlang::catch_cnd(caller("nope.mp4"))
  expect_match(paste(deparse(conditionCall(err)), collapse = " "),
               "caller(", fixed = TRUE)
})
