# Argument-validation and unreadable-file resilience run without the binary
# (a missing file short-circuits before MediaInfo is invoked). Execution tests
# are gated on the mediainfo binary and skip where it is absent.

test_that("mediainfo readers abort on malformed file arguments", {
  expect_error(mediainfo_parameter(123, "General", "Format"))
  expect_error(mediainfo_parameter(character(0), "General", "Format"))
  expect_error(mediainfo_query(123, "General", "Format"))
  expect_error(mediainfo_template(character(0), "brief"))
})

test_that("mediainfo_query() rejects mismatched names length", {
  expect_error(
    mediainfo_query("x.mp4", "General", c("Format", "Duration"), names = "one")
  )
})

test_that("mediainfo_template() requires a templatefile exactly for custom", {
  expect_error(mediainfo_template("x.mp4", "custom"))
  tf <- withr::local_tempfile(fileext = ".txt")
  file.create(tf)
  expect_error(mediainfo_template("x.mp4", "brief", templatefile = tf))
})

test_that("mediainfo readers are resilient to unreadable files", {
  missing <- file.path(tempdir(), "tm-missing-xyz.mp4")

  expect_warning(v <- mediainfo_parameter(missing, "General", "Format"))
  expect_true(is.na(v))

  expect_warning(q <- mediainfo_query(missing, "General", c("Format", "Duration")))
  expect_s3_class(q, "tbl_df")
  expect_identical(q$file, missing)

  expect_warning(t <- mediainfo_template(missing, "brief"))
  expect_s3_class(t, "tbl_df")
  expect_identical(t$file, missing)
})

test_that("built-in templates use snake_case headers matching their value counts", {
  for (tmpl in c("brief", "extended")) {
    path <- system.file(
      sprintf("extdata/mediainfo_template_%s.txt", tmpl), package = "tidymedia"
    )
    lines <- readLines(path, warn = FALSE)
    lines <- lines[nzchar(lines)]
    # Line 1 is "General;<header>\n<values...>"; later lines are "Section;<values>".
    parts <- strsplit(lines[[1]], "\\n", fixed = TRUE)[[1]]
    header <- sub("^General;", "", parts[[1]])
    cols <- trimws(strsplit(header, ",")[[1]])
    expect_true(
      all(grepl("^[a-z][a-z0-9_]*$", cols)),
      info = paste(tmpl, "->", paste(cols, collapse = ", "))
    )
    # Header column count must equal the number of %value% tokens across sections.
    values <- paste(c(parts[[2]], sub("^[^;]*;", "", lines[-1])), collapse = "")
    tokens <- trimws(strsplit(values, ",")[[1]])
    tokens <- tokens[nzchar(tokens)]
    expect_equal(length(tokens), length(cols), info = tmpl)
  }
})

# Parsing of MediaInfo's CSV output is verified without the binary by mocking
# the internal runner, so the read.csv/typing/file-column logic is covered even
# on images where mediainfo is absent.

test_that("mediainfo_query() parses CSV output: file column, typing, opt-out", {
  tmp <- withr::local_tempfile(fileext = ".mp4")
  file.create(tmp)
  local_mocked_bindings(
    find_mediainfo = function() "mediainfo",
    run_program = function(location, args, ...) c("Format, Duration", "AVI, 2000")
  )
  out <- mediainfo_query(tmp, "General", c("Format", "Duration"))
  expect_identical(names(out), c("file", "Format", "Duration"))
  expect_identical(out$Format, "AVI")
  expect_equal(out$Duration, 2000L)

  raw <- mediainfo_query(tmp, "General", c("Format", "Duration"), typed = FALSE)
  expect_type(raw$Duration, "character")
})

test_that("mediainfo readers are resilient to empty CLI output (existing file)", {
  tmp <- withr::local_tempfile(fileext = ".mp4")
  file.create(tmp)
  local_mocked_bindings(
    find_mediainfo = function() "mediainfo",
    run_program = function(location, args, ...) character(0)
  )
  expect_warning(q <- mediainfo_query(tmp, "General", "Format"))
  expect_s3_class(q, "tbl_df")
  expect_identical(q$file, tmp)
  expect_warning(t <- mediainfo_template(tmp, "brief"))
  expect_identical(t$file, tmp)
})

test_that("mediainfo_query() keeps user-supplied names verbatim", {
  tmp <- withr::local_tempfile(fileext = ".mp4")
  file.create(tmp)
  local_mocked_bindings(
    find_mediainfo = function() "mediainfo",
    run_program = function(location, args, ...) c("Frame Rate, My Col", "30, x")
  )
  out <- mediainfo_query(tmp, "Video", c("FrameRate", "Foo"),
                         names = c("Frame Rate", "My Col"))
  expect_identical(names(out), c("file", "Frame Rate", "My Col"))
})

test_that("mediainfo_parameter() returns a typed scalar", {
  tmp <- withr::local_tempfile(fileext = ".mp4")
  file.create(tmp)
  local_mocked_bindings(
    find_mediainfo = function() "mediainfo",
    run_program = function(location, args, ...) "64"
  )
  expect_equal(mediainfo_parameter(tmp, "Video", "Width"), 64L)
  expect_identical(mediainfo_parameter(tmp, "Video", "Width", typed = FALSE), "64")
})

test_that("get_duration() converts units", {
  skip_if_no_mediainfo()
  infile <- make_test_video()
  ms <- get_duration(infile, unit = "ms")
  sec <- get_duration(infile, unit = "sec")
  expect_equal(sec, ms / 1000, tolerance = 1e-6)
})

test_that("get_*() query the video/audio streams and vectorize over files", {
  skip_if_no_mediainfo()
  infile <- make_test_video()
  expect_equal(get_width(infile), 64)
  expect_equal(get_height(infile), 64)
  expect_gt(get_frame_rate(infile), 0)
  expect_length(get_duration(c(infile, infile)), 2)
})

test_that("mediainfo_query() returns a file-keyed tibble and stacks files", {
  skip_if_no_mediainfo()
  infile <- make_test_video()
  out <- mediainfo_query(infile, "General", c("Format", "Duration"))
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out)[[1]], "file")
  expect_equal(nrow(out), 1)

  stacked <- mediainfo_query(c(infile, infile), "General", "Format")
  expect_equal(nrow(stacked), 2)
})

test_that("mediainfo_template() returns a file-keyed tibble", {
  skip_if_no_mediainfo()
  infile <- make_test_video()
  out <- mediainfo_template(infile, "brief")
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out)[[1]], "file")
  expect_equal(nrow(out), 1)
})
