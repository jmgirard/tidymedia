# M52: the `-of compact` line parser.
#
# These tests are binary-free. Every input is either a literal compact line or
# the text recorded in the T1 baseline, so a green run means the parser is
# right, never that this machine's FFprobe happened to agree with it.

# baseline(), exempt_fixtures(), matrix_row_columns() and scrub_paths() live in
# helper-probe-baseline.R, shared with the typed/resilience file.
baseline <- probe_baseline

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

test_that("compact_key_name() gives nested keys their old-writer names", {
  expect_equal(compact_key_name("tag:title"), "TAG:title")
  expect_equal(compact_key_name("disposition:default"),
               "DISPOSITION:default")
  expect_equal(compact_key_name("codec_name"), "codec_name")
  # Side data: the old writer printed it with NO prefix, so the prefix is
  # dropped rather than uppercased. Uppercasing renamed `rotation` away.
  expect_equal(compact_key_name("side_datum/display_matrix:rotation"),
               "rotation")
  expect_equal(compact_key_name("side_datum/display_matrix:side_data_type"),
               "side_data_type")
  # The prefix FFprobe builds has moved across versions, so the stem is matched
  # with whatever follows it: a build omitting the type slug, and the older
  # `side_data` spelling, must strip to the same bare names.
  expect_equal(compact_key_name("side_datum:rotation"), "rotation")
  expect_equal(compact_key_name("side_data/display_matrix:rotation"), "rotation")
  expect_equal(compact_key_name("side_data:rotation"), "rotation")
  # A build that emits the key bare needs no stripping, and gets none.
  expect_equal(compact_key_name("rotation"), "rotation")
  # Only the first `:` is the prefix boundary, so a tag whose own name carries
  # one keeps it.
  expect_equal(compact_key_name("tag:com.apple:make"), "TAG:com.apple:make")
  # An unrecognized prefix is left alone rather than guessed at.
  expect_equal(compact_key_name("newsection:key"), "newsection:key")
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
  # Two fixtures are excluded, both because their recorded baseline is itself
  # CORRUPT -- a value the old writer printed across several lines, which the
  # old parser then read as further columns. Asserting equality on either would
  # pin the bug in place. `hostile` is AC4's; `rotated` is checked just below.
  for (nm in setdiff(names(b), exempt_fixtures())) {
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

# -- AC2: the side-data fixture, whose baseline is itself corrupt -------------

test_that("side-data columns keep the names probe_all() has always returned", {
  b <- baseline()
  before <- b$rotated$one$streams
  after <- parse_compact_probe(b$rotated$compact)$streams

  # The corruption in the recorded baseline: the display matrix's value runs
  # over four lines, and the old parser read three of them as columns.
  bogus <- matrix_row_columns(names(before))
  expect_length(bogus, 3L)

  # Every other column survives, in the same order, with no column and no row
  # added -- `rotation` above all, which review round 1 found renamed away.
  expect_equal(names(after), setdiff(names(before), bogus))
  expect_equal(nrow(after), nrow(before))
  expect_true("rotation" %in% names(after))
  expect_equal(after$rotation, before$rotation)
  expect_equal(after$side_data_type, before$side_data_type)

  # Same values everywhere except the cell the corruption truncated: the old
  # parser cut `displaymatrix` off at the first newline and spent the rest on
  # bogus columns. Its three lines are back in the one cell they belong to.
  shared <- setdiff(names(after), "displaymatrix")
  expect_equal(after[shared], before[shared])
  expect_equal(before$displaymatrix, "")
  for (row in bogus) expect_true(grepl(row, after$displaymatrix, fixed = TRUE))
})

# -- a line the session's locale cannot read ---------------------------------

test_that("a byte invalid in the session locale keeps its row", {
  # A character-wise split returns NA on such a line (warning only), which sent
  # the whole stream row to the section-dispatch floor and deleted it silently.
  # The parse is byte-based precisely so one unreadable metadata byte costs
  # nothing but its own legibility.
  odd <- rawToChar(as.raw(0xE9))
  line <- paste0("stream|index=0|codec_type=video|tag:title=caf", odd)
  out <- parse_compact_probe(c(line, "format|nb_streams=1"))
  expect_equal(nrow(out$streams), 1L)
  expect_equal(out$streams$index, "0")
  expect_equal(charToRaw(out$streams[["TAG:title"]]),
               charToRaw(paste0("caf", odd)))
})

test_that("field splitting and unescaping survive an unreadable byte", {
  odd <- rawToChar(as.raw(0xE9))
  expect_equal(compact_fields(paste0("stream|a=", odd, "|b=2")),
               c("stream", paste0("a=", odd), "b=2"))
  # The escaped pipe stays inside the value even beside the odd byte.
  expect_equal(compact_fields(paste0("stream|a=x\\|", odd)),
               c("stream", paste0("a=x\\|", odd)))
  expect_equal(charToRaw(compact_unescape(paste0("a\\n", odd))),
               charToRaw(paste0("a\n", odd)))
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
