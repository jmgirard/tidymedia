#!/usr/bin/env Rscript
# Reports each sentence of user-facing prose that is over 25 words or that
# matches a maintainer term, and in `.Rmd` prose each sentence holding ` -- ` or
# `---` outside a code span.
#
#   Rscript tools/doc_prose_report.R <files>          report mode
#   Rscript tools/doc_prose_report.R --prose <files>  print the swept prose
#
# Run from the package root. Takes `.Rmd` and `.Rd` files. A developer tool,
# kept out of the build by `.Rbuildignore`'s `^tools$`. The rules it applies,
# and the reason for each, are in cairn/references/plain-docs.md.
#
# What counts as prose:
#   .Rmd -- everything except the YAML header, code chunks and HTML comments.
#           Link text is kept and link targets dropped; images are dropped.
#   .Rd  -- the page as `tools::Rd2txt()` renders it, less the Usage and
#           Examples sections, and less the names in each "Other ... functions:"
#           list (its header line stays). The source outside `\usage` and
#           `\examples` is also scanned for ` -- ` and `---`, which Rd renders
#           as dashes; each such line is reported.
#
# A heading, an argument item, a list item, a table cell and the end of a
# paragraph each end a sentence, as do `.`, `?` and `!` before a letter, a
# digit, a code span, an opening bracket, a double quote or an underscore,
# unless they end "e.g.", "i.e.", "vs.", "etc." or "cf.". A code span counts as
# one word.
# `--prose` prints one sentence per line with its line number, code spans and
# leading argument names removed; for an `.Rd` file the line number is the line
# of the rendered text, not of the source.
#
# The file count of sentences goes to stderr. Exit status: 0 when nothing is
# reported, 1 when something is, 2 when a file parses to no sentences (every
# file is still read and every finding printed), and 3 on a usage error or a
# missing file. So an empty parse cannot pass as a clean one.

MAX_WORDS <- 25

# The first pattern is case-sensitive; the others ignore case.
TERMS_CASE <- c("\\b(D|M|RR)[0-9]{2,3}\\b")
TERMS_NOCASE <- c(
  "\\bLayer [012]\\b", "escape hatch", "front door",
  "\\bseams?\\b", "\\bsentinel", "\\bmemo\\b", "\\bspawn", "best-effort",
  "\u2014"
)

args <- commandArgs(trailingOnly = TRUE)
prose_mode <- "--prose" %in% args
files <- args[args != "--prose"]
if (length(files) == 0) {
  message("usage: Rscript tools/doc_prose_report.R [--prose] <files>")
  quit(status = 3)
}
missing_files <- files[!file.exists(files)]
if (length(missing_files) > 0) {
  message("no such file: ", paste(missing_files, collapse = ", "))
  quit(status = 3)
}

# `tools::Rd2txt()` writes code spans as curly quotes only in a UTF-8 locale, and
# the word and term patterns read UTF-8 text. Run under a UTF-8 character
# locale whatever the caller's locale is, so every locale gives the same result.
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  for (loc in c("C.UTF-8", "en_US.UTF-8", "UTF-8")) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
  }
  if (!isTRUE(l10n_info()[["UTF-8"]])) {
    message("no UTF-8 locale is available")
    quit(status = 3)
  }
}

# Units --------------------------------------------------------------------
# A unit is a block of text that ends a sentence at its own end: a paragraph, a
# heading, a list item, a table cell or an argument item. Each carries the line
# it starts on and whether it opens with an argument name.

new_unit <- function(text, line, arg = FALSE) {
  list(text = text, line = line, arg = arg)
}

# Split lines into units at blank lines and at lines that `starts()` flags.
# `solo()` flags a line that is a unit on its own (a heading).
group_lines <- function(lines, first_line, starts, solo = function(x) FALSE,
                        arg_start = function(x) FALSE) {
  units <- list()
  buf <- character()
  buf_line <- NA_integer_
  buf_arg <- FALSE
  flush <- function() {
    if (length(buf) > 0) {
      units[[length(units) + 1]] <<- new_unit(paste(buf, collapse = " "),
                                              buf_line, buf_arg)
    }
    buf <<- character()
  }
  for (i in seq_along(lines)) {
    x <- lines[[i]]
    ln <- first_line + i - 1L
    if (!nzchar(trimws(x))) {
      flush()
      next
    }
    if (solo(x)) {
      flush()
      units[[length(units) + 1]] <- new_unit(x, ln)
      next
    }
    if (length(buf) > 0 && (starts(x) || arg_start(x))) flush()
    if (length(buf) == 0) {
      buf_line <- ln
      buf_arg <- arg_start(x)
    }
    buf <- c(buf, x)
  }
  flush()
  units
}

# .Rmd -------------------------------------------------------------------

rmd_units <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  n <- length(lines)
  keep <- rep(TRUE, n)

  # YAML header.
  if (n > 0 && grepl("^---\\s*$", lines[[1]])) {
    end <- which(grepl("^---\\s*$", lines))[2]
    if (!is.na(end)) keep[seq_len(end)] <- FALSE
  }
  # Code chunks, fenced with three or more backticks. A fence closes only on a
  # line of at least as many backticks as opened it, so a four-backtick fence
  # can show a three-backtick chunk.
  fence <- 0L
  for (i in seq_len(n)) {
    if (!keep[[i]]) next
    ticks <- nchar(sub("^\\s*(`*).*$", "\\1", lines[[i]]))
    if (fence == 0L && ticks >= 3L) {
      keep[[i]] <- FALSE
      fence <- ticks
    } else if (fence > 0L) {
      keep[[i]] <- FALSE
      if (ticks >= fence && grepl("^\\s*`+\\s*$", lines[[i]])) fence <- 0L
    }
  }
  # HTML comments, which may span lines.
  in_comment <- FALSE
  for (i in seq_len(n)) {
    if (!keep[[i]]) next
    x <- lines[[i]]
    if (in_comment) {
      if (grepl("-->", x)) {
        in_comment <- FALSE
        x <- sub("^.*?-->", "", x, perl = TRUE)
      } else {
        x <- ""
      }
    }
    x <- gsub("<!--.*?-->", "", x, perl = TRUE)
    if (grepl("<!--", x)) {
      in_comment <- TRUE
      x <- sub("<!--.*$", "", x)
    }
    lines[[i]] <- x
  }
  lines[!keep] <- ""

  # A `<br />` ends a unit, like a line break between two list steps.
  out <- list()
  pieces <- character()
  piece_line <- integer()
  for (i in seq_len(n)) {
    parts <- strsplit(lines[[i]], "<br\\s*/?>", perl = TRUE)[[1]]
    if (length(parts) == 0) parts <- ""
    if (length(parts) > 1 || grepl("<br\\s*/?>\\s*$", lines[[i]], perl = TRUE)) {
      parts <- c(parts[1], unlist(lapply(parts[-1], function(p) c("", p))))
      if (grepl("<br\\s*/?>\\s*$", lines[[i]], perl = TRUE)) parts <- c(parts, "")
    }
    pieces <- c(pieces, parts)
    piece_line <- c(piece_line, rep(i, length(parts)))
  }

  is_heading <- function(x) grepl("^#{1,6}\\s", x)
  is_item <- function(x) grepl("^\\s*([-*+]|[0-9]+[.)])\\s+", x)
  is_table <- function(x) grepl("^\\s*\\|", x)

  units <- list()
  buf <- character()
  buf_line <- NA_integer_
  flush <- function() {
    if (length(buf) > 0) {
      units[[length(units) + 1]] <<- new_unit(paste(buf, collapse = " "), buf_line)
    }
    buf <<- character()
  }
  for (k in seq_along(pieces)) {
    x <- pieces[[k]]
    ln <- piece_line[[k]]
    if (!nzchar(trimws(x))) {
      flush()
      next
    }
    if (is_heading(x)) {
      flush()
      units[[length(units) + 1]] <- new_unit(sub("^#+\\s*", "", x), ln)
      next
    }
    if (is_table(x)) {
      flush()
      if (grepl("^[\\s|:-]+$", x, perl = TRUE)) next
      # A `|` inside a code span or escaped as `\|` does not end a cell.
      p <- protect_code(x, rd = FALSE)
      row <- sub("^\\s*\\|", "", sub("\\|\\s*$", "", p$text))
      cells <- strsplit(row, "(?<!\\\\)\\|", perl = TRUE)[[1]]
      for (cell in cells) {
        cell <- restore_code(cell, p$spans, drop = FALSE)
        units[[length(units) + 1]] <- new_unit(cell, ln)
      }
      next
    }
    if (is_item(x)) {
      flush()
      x <- sub("^\\s*([-*+]|[0-9]+[.)])\\s+", "", x)
    }
    if (length(buf) == 0) buf_line <- ln
    buf <- c(buf, x)
  }
  flush()
  units
}

# .Rd --------------------------------------------------------------------

rd_units <- function(path) {
  txt <- tempfile(fileext = ".txt")
  on.exit(unlink(txt))
  old <- options(useFancyQuotes = TRUE)
  on.exit(options(old), add = TRUE)
  tools::Rd2txt(path, out = txt,
                options = list(underline_titles = FALSE, width = 80L,
                               itemBullet = "* "))
  lines <- readLines(txt, warn = FALSE, encoding = "UTF-8")

  is_section <- function(x) grepl("^[A-Z][^:]*:\\s*$", x)
  sections <- cumsum(is_section(lines))
  section_name <- c("", sub(":\\s*$", "", lines[is_section(lines)]))[sections + 1]

  drop <- section_name %in% c("Usage", "Examples")
  lines[drop & !is_section(lines)] <- ""

  # "Other ... functions:" lists keep their header and drop the names, which may
  # wrap onto following lines until the paragraph ends.
  in_list <- FALSE
  for (i in seq_along(lines)) {
    if (in_list) {
      if (!nzchar(trimws(lines[[i]]))) in_list <- FALSE else lines[[i]] <- ""
    }
    if (grepl("^\\s*Other .* functions:", lines[[i]])) {
      lines[[i]] <- sub("(functions:).*$", "\\1", lines[[i]])
      in_list <- TRUE
    }
  }

  # `Rd2txt()` right-aligns each argument name in eight columns before ": ", and
  # indents wrapped lines by ten or more spaces. Only a line laid out that way
  # starts an argument, so a wrapped line holding ": " stays in its argument.
  is_arg_line <- function(x) {
    m <- regmatches(x, regexec("^( *)(\\S.*?): ", x, perl = TRUE))[[1]]
    length(m) == 3L && nchar(m[[2]]) == max(0L, 8L - nchar(m[[3]]))
  }
  units <- list()
  runs <- rle(section_name)
  starts_at <- cumsum(c(1L, head(runs$lengths, -1L)))
  for (r in seq_along(runs$lengths)) {
    idx <- seq(starts_at[[r]], length.out = runs$lengths[[r]])
    seg <- lines[idx]
    arg_section <- runs$values[[r]] == "Arguments"
    units <- c(units, group_lines(
      seg, idx[[1]],
      starts = function(x) grepl("^\\s*(\\* |\u2022 |[0-9]+\\. )", x),
      solo = is_section,
      arg_start = function(x) {
        arg_section && is_arg_line(x) && !is_section(x)
      }
    ))
  }
  units
}

# Scan Rd source outside \usage and \examples for the two dash forms.
rd_dashes <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  depth <- 0L
  hits <- character()
  for (i in seq_along(lines)) {
    x <- lines[[i]]
    if (depth == 0L && grepl("^%", x)) next
    opening <- depth == 0L && grepl("^\\\\(usage|examples)\\{", x)
    if (depth == 0L && !opening && grepl(" -- |---", x)) {
      hits <- c(hits, sprintf("%s:%d: [dash in Rd source] %s", path, i, trimws(x)))
    }
    if (opening || depth > 0L) {
      bare <- gsub("\\\\[{}]", "", x)
      depth <- depth + lengths(regmatches(bare, gregexpr("\\{", bare))) -
        lengths(regmatches(bare, gregexpr("\\}", bare)))
      if (depth < 0L) depth <- 0L
    }
  }
  hits
}

# Sentences --------------------------------------------------------------

CODE_OPEN <- "\u0001"
CODE_CLOSE <- "\u0002"

# Replace code spans with numbered placeholders so their punctuation never ends
# a sentence and each counts as one word.
protect_code <- function(text, rd) {
  pattern <- if (rd) "\u2018[^\u2018\u2019]*\u2019" else "(`+)(?:(?!\\1).)+?\\1"
  m <- gregexpr(pattern, text, perl = TRUE)
  spans <- regmatches(text, m)[[1]]
  if (length(spans) == 0) return(list(text = text, spans = character()))
  keys <- paste0(CODE_OPEN, seq_along(spans), CODE_CLOSE)
  regmatches(text, m) <- list(keys)
  list(text = text, spans = spans)
}

restore_code <- function(text, spans, drop) {
  for (k in rev(seq_along(spans))) {
    key <- paste0(CODE_OPEN, k, CODE_CLOSE)
    text <- gsub(key, if (drop) "" else spans[[k]], text, fixed = TRUE)
  }
  text
}

clean_markup <- function(text) {
  text <- gsub("!\\[[^]]*\\]\\([^)]*\\)", "", text, perl = TRUE)
  text <- gsub("\\[([^]]*)\\]\\([^)]*\\)", "\\1", text, perl = TRUE)
  text <- gsub("<(https?://[^>]+)>", "\\1", text, perl = TRUE)
  # Only an HTML tag is dropped, so prose such as "a < b and c > d" stays.
  text <- gsub("</?[A-Za-z][A-Za-z0-9-]*(\\s[^<>]*)?/?>", "", text, perl = TRUE)
  text <- gsub("**", "", text, fixed = TRUE)
  text <- gsub("(?<![\\w*])\\*(?=\\S)(.+?)(?<=\\S)\\*(?![\\w*])", "\\1", text,
               perl = TRUE)
  text
}

ABBREV <- "(?<!\\be\\.g\\.)(?<!\\bi\\.e\\.)(?<!\\bvs\\.)(?<!\\betc\\.)(?<!\\bcf\\.)"

split_sentences <- function(text) {
  boundary <- paste0(ABBREV, "(?<=[.?!])([\"')\\]]*)\\s+(?=[A-Za-z0-9(\"",
                     CODE_OPEN, "_])")
  marked <- gsub(boundary, "\\1\u0003", text, perl = TRUE)
  trimws(strsplit(marked, "\u0003", fixed = TRUE)[[1]])
}

count_words <- function(sentence) {
  tokens <- strsplit(sentence, "\\s+", perl = TRUE)[[1]]
  sum(grepl(paste0("[[:alnum:]", CODE_OPEN, "]"), tokens))
}

squish <- function(x) gsub("\\s+", " ", trimws(x), perl = TRUE)

# Main -------------------------------------------------------------------

reports <- character()
empty <- character()
for (path in files) {
  rd <- grepl("[.]Rd$", path, ignore.case = TRUE)
  units <- if (rd) rd_units(path) else rmd_units(path)
  if (rd) reports <- c(reports, rd_dashes(path))

  n_sentences <- 0L
  prose_lines <- character()
  for (u in units) {
    p <- protect_code(squish(u$text), rd)
    body <- clean_markup(p$text)
    if (u$arg) body <- sub("^.*?:\\s+", "", body, perl = TRUE)
    if (rd) body <- sub("^(\\* |• |[0-9]+\\. )", "", body)
    for (s in split_sentences(body)) {
      if (!grepl("[[:alnum:]]", restore_code(s, p$spans, drop = FALSE))) next
      n_sentences <- n_sentences + 1L
      full <- squish(restore_code(s, p$spans, drop = FALSE))
      if (prose_mode) {
        prose_lines <- c(prose_lines, sprintf(
          "%d: %s", u$line, squish(restore_code(s, p$spans, drop = TRUE))
        ))
        next
      }
      # Markdown renders ` -- ` and `---` as dashes. Code spans are still
      # placeholders in `s`, so a dash inside one is not reported.
      if (!rd && grepl(" -- |---", squish(s), perl = TRUE)) {
        reports <- c(reports, sprintf("%s:%d: [dash in Rmd prose] %s", path,
                                      u$line, full))
      }
      words <- count_words(s)
      if (words > MAX_WORDS) {
        reports <- c(reports, sprintf("%s:%d: [%d words] %s", path, u$line,
                                      words, full))
      }
      hit <- c(
        TERMS_CASE[vapply(TERMS_CASE, grepl, logical(1), x = full, perl = TRUE)],
        TERMS_NOCASE[vapply(TERMS_NOCASE, grepl, logical(1), x = full,
                            perl = TRUE, ignore.case = TRUE)]
      )
      for (h in hit) {
        reports <- c(reports, sprintf("%s:%d: [term %s] %s", path, u$line, h, full))
      }
    }
  }
  message(sprintf("%s: %d sentences", path, n_sentences))
  if (n_sentences == 0L) {
    message("no sentences read from ", path, " -- the parse is empty")
    empty <- c(empty, path)
  }
  if (prose_mode) {
    if (length(files) > 1) cat("==> ", path, " <==\n", sep = "")
    if (length(prose_lines) > 0) cat(prose_lines, sep = "\n")
  }
}

# An empty parse wins over findings, but the findings are still printed.
if (!prose_mode && length(reports) > 0) cat(reports, sep = "\n")
if (length(empty) > 0) quit(status = 2)
if (!prose_mode && length(reports) > 0) quit(status = 1)
quit(status = 0)
