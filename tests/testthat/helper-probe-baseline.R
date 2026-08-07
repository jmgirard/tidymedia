# M52: shared reading of the recorded pre-change probe baseline.
#
# The baseline pairs, per fixture, the raw `-of compact` text a single FFprobe
# call returns with the tibbles the PRE-CHANGE per-stream parser built from the
# same file. Regenerate it with data-raw/probe-baseline.R.

probe_baseline <- function() {
  readRDS(test_path("fixtures", "probe-baseline.rds"))
}

# Fixtures whose recorded pre-change output is itself corrupt, so an
# identical-output assertion against them would pin the corruption in place.
# Both are corrupt the same way: the old writer printed one value across
# several lines and the old parser read the continuation lines as further
# `key=value` pairs. `hostile` carries a newline inside a tag (AC4);
# `rotated` carries a display matrix, whose value is four lines by nature.
exempt_fixtures <- function() c("hostile", "rotated")

# The columns that split-value corruption produced in the `rotated` baseline:
# the display matrix's own rows, read as column names. Derived from the
# recorded names rather than listed, so the set cannot drift from the fixture.
matrix_row_columns <- function(nms) grep("^[0-9]{8}:", nms, value = TRUE)

# The scrub data-raw/probe-baseline.R applied before recording: the fixture's
# own temp path becomes a stable token. Re-applied to whatever a test parses
# out of the recorded text, so a comparison is not two temp directories.
scrub_paths <- function(x, path, token) {
  for (col in names(x)) {
    if (is.character(x[[col]])) {
      x[[col]] <- gsub(path, token, x[[col]], fixed = TRUE)
      x[[col]] <- gsub(basename(path), basename(token), x[[col]], fixed = TRUE)
    }
  }
  x
}
