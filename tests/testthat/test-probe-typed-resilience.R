# M52 AC5: `typed` parity against the recorded pre-change baseline, and the
# resilience contract `probe_all()` documents.
#
# Both halves feed RECORDED FFprobe output to the real probe_all() through a
# mocked run_program(), rather than reprobing a freshly generated fixture. That
# keeps them binary-free and deterministic, and it exercises probe_all()'s own
# composition -- the `file` column, bind_rows, type_columns and the warning --
# instead of a test-side re-implementation of it.

# baseline(), exempt_fixtures() and scrub_paths() live in
# helper-probe-baseline.R, shared with the parser file.
baseline <- probe_baseline

# Answer every FFprobe call from `responses`, one per call in order. A single
# character vector answers every call. find_ffprobe() is mocked too so these
# tests run on a machine with no FFprobe at all.
local_recorded_ffprobe <- function(responses, env = parent.frame()) {
  if (!is.list(responses)) responses <- list(responses)
  i <- 0L
  testthat::local_mocked_bindings(
    find_ffprobe = function(...) "ffprobe",
    run_program = function(...) {
      i <<- i + 1L
      responses[[min(i, length(responses))]]
    },
    .env = env
  )
}

# -- AC5: typed parity -------------------------------------------------------

test_that("typed = TRUE reproduces the recorded pre-change output", {
  b <- baseline()
  for (nm in setdiff(names(b), exempt_fixtures())) {
    entry <- b[[nm]]
    local_recorded_ffprobe(entry$compact)
    out <- probe_all(entry$path, typed = TRUE)
    expect_equal(scrub_paths(out$container, entry$path, entry$token),
                 entry$typed$container, info = nm)
    expect_equal(scrub_paths(out$streams, entry$path, entry$token),
                 entry$typed$streams, info = nm)
  }
})

test_that("typed = FALSE reproduces the recorded pre-change output", {
  b <- baseline()
  for (nm in setdiff(names(b), exempt_fixtures())) {
    entry <- b[[nm]]
    local_recorded_ffprobe(entry$compact)
    out <- probe_all(entry$path, typed = FALSE)
    expect_equal(scrub_paths(out$container, entry$path, entry$token),
                 entry$untyped$container, info = nm)
    expect_equal(scrub_paths(out$streams, entry$path, entry$token),
                 entry$untyped$streams, info = nm)
    # The point of typed = FALSE: nothing was converted.
    expect_true(all(vapply(out$streams, is.character, logical(1))))
  }
})

test_that("side data reaches probe_all() under both typed values", {
  # The exempt fixtures are still checked here, on everything their recorded
  # baseline gets right: parity is claimed for the whole streams tibble minus
  # the columns the old writer's multi-line value corrupted.
  b <- baseline()
  entry <- b$rotated
  for (typed in c(TRUE, FALSE)) {
    local_recorded_ffprobe(entry$compact)
    out <- probe_all(entry$path, typed = typed)
    before <- if (typed) entry$typed$streams else entry$untyped$streams
    shared <- setdiff(names(before),
                      c(matrix_row_columns(names(before)), "displaymatrix"))
    expect_equal(scrub_paths(out$streams, entry$path, entry$token)[shared],
                 before[shared], info = paste("typed =", typed))
    expect_true("rotation" %in% names(out$streams))
  }
})

# -- AC5: the resilience contract --------------------------------------------

test_that("an unprobeable file yields an NA row and one warning, not an abort", {
  # FFprobe says nothing at all about a file it cannot open.
  local_recorded_ffprobe(character(0))
  expect_warning(p <- probe_all("no-such-file.mkv"))
  expect_equal(nrow(p$container), 1L)
  expect_equal(p$container$file, "no-such-file.mkv")
  expect_equal(names(p$container), "file")
  expect_equal(nrow(p$streams), 1L)
})

test_that("a file with no readable streams yields a single NA stream row", {
  # A container FFprobe reads but that reports no streams: a format line and
  # nothing else. This is the path the old code reached via `nb_streams < 1`.
  local_recorded_ffprobe("format|nb_streams=0|format_name=matroska,webm")
  p <- expect_no_warning(probe_all("empty.mkv"))
  # The container row is real, not NA.
  expect_equal(p$container$format_name, "matroska,webm")
  # The streams tibble carries one row keyed by file and no stream columns.
  expect_equal(nrow(p$streams), 1L)
  expect_equal(names(p$streams), "file")
})

test_that("a mixed vector warns once and keeps the good file's data", {
  b <- baseline()
  good <- b$plain
  local_recorded_ffprobe(list(good$compact, character(0)))
  expect_warning(p <- probe_all(c(good$path, "missing.mkv")),
                 "Could not probe 1 file")
  expect_equal(nrow(p$container), 2L)
  expect_true(any(!is.na(p$container$format_name)))
  expect_true(any(is.na(p$container$format_name)))
})

test_that("two unprobeable files warn once, not twice", {
  local_recorded_ffprobe(character(0))
  warnings <- character()
  withCallingHandlers(
    probe_all(c("a.mkv", "b.mkv")),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1L)
  expect_match(warnings, "2 files")
})

test_that("probe_video()/probe_audio() stay safe when every file failed", {
  local_recorded_ffprobe(character(0))
  expect_warning(v <- probe_video(infile = "gone.mkv"))
  expect_equal(nrow(v), 0L)
  local_recorded_ffprobe(character(0))
  expect_warning(a <- probe_audio(infile = "gone.mkv"))
  expect_equal(nrow(a), 0L)
})
