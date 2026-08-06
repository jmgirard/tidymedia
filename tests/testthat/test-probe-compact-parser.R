# M52: the `-of compact` line parser.
#
# These tests are binary-free. Every input is either a literal compact line or
# the text recorded in the T1 baseline, so a green run means the parser is
# right, never that this machine's FFprobe happened to agree with it.

baseline <- function() {
  readRDS(test_path("fixtures", "probe-baseline.rds"))
}

# The scrub data-raw/probe-baseline.R applied before recording: the fixture's
# own temp path becomes a stable token. Re-applied here to whatever is parsed
# out of the recorded text, so the comparison is not two temp directories.
scrub_paths <- function(x, path, token) {
  for (col in names(x)) {
    if (is.character(x[[col]])) {
      x[[col]] <- gsub(path, token, x[[col]], fixed = TRUE)
      x[[col]] <- gsub(basename(path), basename(token), x[[col]], fixed = TRUE)
    }
  }
  x
}

# -- field splitting ---------------------------------------------------------

test_that("compact_fields() splits on unescaped | and keeps escaped ones", {
  expect_equal(compact_fields("stream|a=1|b=2"), c("stream", "a=1", "b=2"))
  # `\|` is a literal pipe inside a value, not a separator.
  expect_equal(compact_fields("stream|a=x\\|y"), c("stream", "a=x\\|y"))
  # `\\` is a literal backslash, so the `|` after it IS a separator. This is
  # the case a naive "split on | not preceded by a backslash" rule gets wrong.
  expect_equal(compact_fields("stream|a=x\\\\|b=2"),
               c("stream", "a=x\\\\", "b=2"))
})

test_that("compact_fields() handles empty fields and a single field", {
  expect_equal(compact_fields("format"), "format")
  expect_equal(compact_fields("stream||a=1"), c("stream", "", "a=1"))
})

# -- unescaping --------------------------------------------------------------

test_that("compact_unescape() decodes the six escapes the writer emits", {
  expect_equal(compact_unescape("a\\|b"), "a|b")
  expect_equal(compact_unescape("a\\\\b"), "a\\b")
  expect_equal(compact_unescape("a\\nb"), "a\nb")
  expect_equal(compact_unescape("a\\rb"), "a\rb")
  expect_equal(compact_unescape("a\\bb"), "a\bb")
  expect_equal(compact_unescape("a\\fb"), "a\fb")
})

test_that("compact_unescape() leaves raw control characters alone", {
  # BEL, TAB and vertical tab were measured arriving unescaped, so they must
  # survive untouched rather than being re-interpreted.
  expect_equal(compact_unescape("a\ab\tc\vd"), "a\ab\tc\vd")
})

test_that("compact_unescape() decodes each pair exactly once", {
  # A literal backslash followed by `n` arrives as `\\n`. Decoding `\\` and then
  # `\n` in two passes would yield a newline; one pass yields backslash + n.
  expect_equal(compact_unescape("a\\\\nb"), "a\\nb")
  expect_equal(compact_unescape("\\\\\\|"), "\\|")
})

# -- section dispatch and prefix casing --------------------------------------

test_that("compact_section_case() restores the default writer's casing", {
  expect_equal(compact_section_case("tag:title"), "TAG:title")
  expect_equal(compact_section_case("disposition:default"),
               "DISPOSITION:default")
  expect_equal(compact_section_case("codec_name"), "codec_name")
})

test_that("parse_compact_probe() dispatches by section, format arriving last", {
  txt <- c("stream|index=0|codec_type=video",
           "stream|index=1|codec_type=audio",
           "format|nb_streams=2|format_name=matroska")
  out <- parse_compact_probe(txt)
  expect_equal(nrow(out$streams), 2L)
  expect_equal(out$streams$index, c("0", "1"))
  expect_equal(nrow(out$container), 1L)
  expect_equal(out$container$nb_streams, "2")
  # The keyless leading section field must not become a column (AC2: no extra
  # column), and no `file` column is added here -- probe_all() does that.
  expect_false("stream" %in% names(out$streams))
})

test_that("a field splits only on its first '='", {
  # Carried over from format_probe()'s test when probe_one() stopped using it:
  # a value containing `=` must survive, and the writer does not escape `=`.
  out <- parse_compact_probe("format|key=a=b=c|n=2")
  expect_equal(out$container$key, "a=b=c")
  expect_equal(out$container$n, "2")
})

test_that("parse_compact_probe() returns NULL on empty or format-less output", {
  expect_null(parse_compact_probe(character(0)))
  expect_null(parse_compact_probe(""))
  expect_null(parse_compact_probe("stream|index=0"))
})

test_that("parse_compact_probe() gives an empty streams tibble with no streams", {
  out <- parse_compact_probe("format|nb_streams=0|format_name=matroska")
  expect_equal(nrow(out$container), 1L)
  expect_equal(nrow(out$streams), 0L)
})

# -- AC2: the recorded baseline ----------------------------------------------

test_that("the parser rebuilds the pre-change tibbles from the recorded text", {
  b <- baseline()
  # The escape fixture is excluded: its recorded baseline is the CORRUPTION
  # AC4 removes, so asserting equality there would pin the bug in place.
  for (nm in setdiff(names(b), "hostile")) {
    entry <- b[[nm]]
    out <- parse_compact_probe(entry$compact)
    expect_equal(names(out$container), names(entry$one$container),
                 info = nm)
    expect_equal(names(out$streams), names(entry$one$streams), info = nm)
    expect_equal(nrow(out$streams), nrow(entry$one$streams), info = nm)
    # The recorded tibbles have the fixture's own path scrubbed to a token, so
    # scrub the freshly parsed ones the same way before comparing values.
    expect_equal(scrub_paths(out$container, entry$path, entry$token),
                 entry$one$container, info = nm)
    expect_equal(scrub_paths(out$streams, entry$path, entry$token),
                 entry$one$streams, info = nm)
  }
})

# -- AC4: the corruption the old parser had ----------------------------------

test_that("a newline-bearing tag was corrupt before and is one cell now", {
  b <- baseline()
  before <- b$hostile$one$streams
  # What the pre-change per-stream parse actually produced, recorded at T1: the
  # tag value truncated at the newline, and its remainder read as a further
  # `key=value` pair and emitted as its own column.
  expect_true("break" %in% names(before))
  expect_equal(before[["TAG:title"]][[2]], "line")

  after <- parse_compact_probe(b$hostile$compact)$streams
  expect_false("break" %in% names(after))
  expect_equal(after[["TAG:title"]][[2]], "line\nbreak")
  # No row was gained either: the corruption added a column, but a value
  # forging a line break under a different writer would have added a row.
  expect_equal(nrow(after), nrow(before))
})

# -- AC3: every escape round-trips -------------------------------------------

test_that("each escape the writer emits round-trips into one cell", {
  originals <- list(
    pipe = "a|b", backslash = "a\\b", newline = "a\nb",
    carriage = "a\rb", backspace = "a\bb", formfeed = "a\fb",
    # Not an escape -- a raw byte the writer passes through untouched.
    raw_tab = "a\tb"
  )
  encode <- function(s) {
    s <- gsub("\\", "\\\\", s, fixed = TRUE)
    s <- gsub("|", "\\|", s, fixed = TRUE)
    s <- gsub("\n", "\\n", s, fixed = TRUE)
    s <- gsub("\r", "\\r", s, fixed = TRUE)
    s <- gsub("\b", "\\b", s, fixed = TRUE)
    gsub("\f", "\\f", s, fixed = TRUE)
  }
  for (nm in names(originals)) {
    line <- paste0("stream|index=0|tag:title=", encode(originals[[nm]]))
    out <- parse_compact_probe(c(line, "format|nb_streams=1"))
    expect_equal(nrow(out$streams), 1L, info = nm)
    expect_equal(ncol(out$streams), 2L, info = nm)
    expect_equal(out$streams[["TAG:title"]], originals[[nm]], info = nm)
  }
})
