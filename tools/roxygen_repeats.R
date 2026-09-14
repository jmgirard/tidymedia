#!/usr/bin/env Rscript
# Lists each roxygen paragraph found in two or more roxygen blocks.
#
#   Rscript tools/roxygen_repeats.R <files>
#
# Run from the package root. A developer tool, kept out of the build by
# `.Rbuildignore`'s `^tools$`.
#
# A block is a run of consecutive `#'` lines. A paragraph is the text of a run
# of `#'` lines that starts at a tag or after a blank `#'` line, and ends before
# the next tag or blank `#'` line. The leading tag and its argument name (the
# names after `@param`, the title before the colon after `@section`) are
# removed, and runs of white space become one space.
#
# Not paragraphs: lines under `@examples` or `@examplesIf` up to the next tag;
# lines holding only `@export`, `@examples`, `@family ...`, `@rdname ...` or
# `@inheritParams ...`; and a paragraph whose text is one inline `r ...` call.
#
# Output: one record per repeated paragraph, most blocks first:
#   == <n> blocks: <text>
#      <file>:<line> <block name>
# Exits 0 when nothing is listed, 1 when something is, and 3 on a usage error or
# a missing file.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  message("usage: Rscript tools/roxygen_repeats.R <files>")
  quit(status = 3)
}
missing <- args[!file.exists(args)]
if (length(missing) > 0) {
  message("no such file: ", paste(missing, collapse = ", "))
  quit(status = 3)
}

squish <- function(x) gsub("\\s+", " ", trimws(x), perl = TRUE)

# Tags whose first word (or, for @section, the text up to the first colon) is a
# name rather than prose.
NAMED_TAGS <- c("param", "field", "slot", "describeIn", "templateVar")
SKIP_LINE <- "^@(export|examples|family|rdname|inheritParams)\\b"
EXAMPLE_TAG <- "^@examples(If)?\\b"

paragraphs_in <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  is_rox <- grepl("^\\s*#'", lines)
  out <- list()
  i <- 1L
  n <- length(lines)
  while (i <= n) {
    if (!is_rox[[i]]) {
      i <- i + 1L
      next
    }
    start <- i
    while (i <= n && is_rox[[i]]) i <- i + 1L
    idx <- start:(i - 1L)
    # The block's name: its @rdname or @name, else the object defined after it.
    body <- sub("^\\s*#' ?", "", lines[idx])
    tag_name <- sub("^@(rdname|name)\\s+(\\S+).*$", "\\2",
                    grep("^@(rdname|name)\\s", body, value = TRUE))
    obj <- if (i <= n) sub("^\\s*`?([^`[:space:]]+)`?\\s*(<-|=).*$", "\\1", lines[[i]]) else ""
    name <- if (length(tag_name) > 0) tag_name[[1]] else obj
    block <- sprintf("%s:%d %s", path, start, name)

    buf <- character()
    buf_line <- NA_integer_
    in_examples <- FALSE
    flush <- function() {
      if (length(buf) > 0) {
        text <- squish(paste(buf, collapse = " "))
        if (nzchar(text) && !grepl("^`r [^`]*`$", text)) {
          out[[length(out) + 1L]] <<- list(text = text, block = block,
                                           line = buf_line)
        }
      }
      buf <<- character()
    }
    for (k in seq_along(idx)) {
      x <- body[[k]]
      if (!nzchar(trimws(x))) {
        flush()
        next
      }
      is_tag <- grepl("^\\s*@[A-Za-z]", x)
      if (is_tag) {
        flush()
        tx <- trimws(x)
        in_examples <- grepl(EXAMPLE_TAG, tx)
        if (in_examples || grepl(SKIP_LINE, tx)) next
        tag <- sub("^@([A-Za-z]+).*$", "\\1", tx)
        rest <- sub("^@[A-Za-z]+\\s*", "", tx)
        if (tag %in% NAMED_TAGS) {
          rest <- sub("^\\S+\\s*", "", rest)
        } else if (tag == "section") {
          rest <- sub("^[^:]*:\\s*", "", rest)
        }
        x <- rest
      } else if (in_examples) {
        next
      }
      if (length(buf) == 0) buf_line <- idx[[k]]
      buf <- c(buf, x)
    }
    flush()
  }
  out
}

paras <- unlist(lapply(args, paragraphs_in), recursive = FALSE)
if (length(paras) == 0) {
  message("no paragraphs read -- the parse is empty")
  quit(status = 2)
}
texts <- vapply(paras, `[[`, "", "text")
blocks <- vapply(paras, `[[`, "", "block")
lines <- vapply(paras, `[[`, 0L, "line")
message(sprintf("%d paragraphs in %d blocks", length(paras),
                length(unique(blocks))))

by_text <- split(seq_along(texts), texts)
n_blocks <- vapply(by_text, function(k) length(unique(blocks[k])), 0L)
rep_texts <- names(by_text)[n_blocks >= 2]
if (length(rep_texts) == 0) quit(status = 0)

rep_texts <- rep_texts[order(-n_blocks[rep_texts], rep_texts)]
for (t in rep_texts) {
  k <- by_text[[t]]
  cat(sprintf("== %d blocks: %s\n", n_blocks[[t]], t))
  first <- k[!duplicated(blocks[k])]
  cat(sprintf("   %s (line %d)\n", blocks[first], lines[first]), sep = "")
}
quit(status = 1)
