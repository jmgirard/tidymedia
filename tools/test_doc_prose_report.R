#!/usr/bin/env Rscript
# Plants each parse case of the prose sweep and checks the sweep reads it
# correctly. Exits 0 only when every plant passes, and 1 otherwise.
#
#   Rscript tools/test_doc_prose_report.R [<sweep script>]
#
# Run from the package root. The sweep script defaults to
# tools/doc_prose_report.R; pass an older copy to see which plants it fails.
# A developer tool, kept out of the build by `.Rbuildignore`'s `^tools$`.

args <- commandArgs(trailingOnly = TRUE)
sweep <- normalizePath(if (length(args) > 0) args[[1]] else "tools/doc_prose_report.R",
                       mustWork = TRUE)
dir <- tempfile("prose-plants-")
dir.create(dir)
on.exit(unlink(dir, recursive = TRUE))

plant <- function(name, lines) {
  path <- file.path(dir, name)
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  path
}

# Runs the sweep and returns its stdout lines with the exit status attached.
run <- function(files, prose = FALSE, env = character()) {
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c(shQuote(sweep), if (prose) "--prose",
                                          shQuote(files)),
    stdout = TRUE, stderr = FALSE, env = env
  ))
  status <- attr(out, "status")
  out <- as.character(out)
  attr(out, "status") <- if (is.null(status)) 0L else status
  out
}
prose_of <- function(file, env = character()) {
  sub("^[0-9]+: ", "", run(file, prose = TRUE, env = env))
}

results <- logical()
check <- function(name, ok) {
  ok <- isTRUE(ok)
  cat(sprintf("%s  %s\n", if (ok) "PASS" else "FAIL", name))
  results[[name]] <<- ok
}

yaml <- c("---", "title: plant", "---", "")

# 1. A sentence may start with a lowercase word. -------------------------------
f <- plant("lower.Rmd", c(yaml,
  "It runs fast. tidymedia reads the file.",
  "",
  "It runs slow. another word starts here.",
  "",
  "Use a tool, e.g. ffmpeg, to cut."))
p <- prose_of(f)
check("lowercase start: tidymedia", "tidymedia reads the file." %in% p)
check("lowercase start: another word", "another word starts here." %in% p)
check("lowercase start: e.g. stays one sentence (control)",
      "Use a tool, e.g. ffmpeg, to cut." %in% p)

# 2. Rd argument items. --------------------------------------------------------
f <- plant("args.Rd", c(
  "\\name{args}", "\\alias{args}", "\\title{Plant}", "\\usage{args(x)}",
  "\\arguments{",
  paste("\\item{x}{A string. The text wraps to a second line and holds a",
        "colon: here it is, with more words to wrap past eighty.}"),
  paste("\\item{long_argument_name}{A list. \\itemize{\\item one item that is",
        "long enough to wrap around the eighty column limit so: we see its",
        "indent.}}"),
  "\\item{a:b}{Colon name text.}",
  "\\item{y}{Plain text.}",
  "}"))
p <- prose_of(f)
check("argument: wrapped line at indent 10 keeps its text",
      any(grepl("^The text wraps to a second line and holds a colon: here it is",
                p)))
check("argument: wrapped line at indent 14 keeps its text",
      any(grepl("^one item that is long enough .* limit so: we see its indent[.]$",
                p)))
check("argument: a name holding a colon is removed", "Colon name text." %in% p)
check("argument: plain item (control)", "Plain text." %in% p)

# 3. The same result in a non-UTF-8 locale. -------------------------------------
f <- plant("locale.Rd", c(
  "\\name{locale}", "\\alias{locale}", "\\title{Plant}", "\\usage{locale(x)}",
  "\\description{Call \\code{foo(x)} to read it. Then stop.}"))
utf8 <- prose_of(f, env = "LC_ALL=en_US.UTF-8")
c_loc <- prose_of(f, env = "LC_ALL=C")
check("locale: C gives the UTF-8 result", identical(utf8, c_loc))
check("locale: code span removed under C", "Call to read it." %in% c_loc)

# 4. Exit status. --------------------------------------------------------------
empty <- plant("empty.Rmd", yaml)
finding <- plant("finding.Rmd", c(yaml, "A maintainer memo is here."))
clean <- plant("clean.Rmd", c(yaml, "A short sentence."))
orders <- list(first = c(empty, finding, clean), middle = c(finding, empty, clean),
               last = c(finding, clean, empty))
for (o in names(orders)) {
  out <- run(orders[[o]])
  check(sprintf("exit: empty file %s gives 2 and still prints the finding", o),
        attr(out, "status") == 2L && any(grepl("\\[term .*memo.*\\] A maintainer",
                                               out)))
}
check("exit: finding without empty file gives 1 (control)",
      attr(run(c(finding, clean)), "status") == 1L)
check("exit: clean file gives 0 (control)", attr(run(clean), "status") == 0L)
check("exit: no files gives 3", attr(run(character()), "status") == 3L)
check("exit: missing file gives 3",
      attr(run(c(clean, file.path(dir, "absent.Rmd"))), "status") == 3L)

# 5. Dashes in .Rmd prose. -----------------------------------------------------
dashes <- plant("dashes.Rmd", c(yaml, "One part -- another part.", "",
                                "One part---another part.", "",
                                "Below is a rule.", "", "---", ""))
out <- run(dashes)
check("dash: ` -- ` in prose is reported",
      any(grepl("\\[dash in Rmd prose\\] One part -- another", out)))
check("dash: `---` in prose is reported",
      any(grepl("\\[dash in Rmd prose\\] One part---another", out)))
code_dashes <- plant("code-dashes.Rmd", c(yaml, "Run `a -- b` now.", "",
                                          "Type `---` here.", "", "---", ""))
check("dash: dashes in code spans and a rule line are not reported (control)",
      attr(run(code_dashes), "status") == 0L)

# 6. Fences, table cells and angle brackets. -----------------------------------
f <- plant("fence.Rmd", c(yaml, "````markdown", "```{r}",
                          "Fenced words stay out.", "```",
                          "More fenced words.", "````", "",
                          "Outside words count."))
p <- prose_of(f)
check("fence: text inside a four-backtick fence is dropped",
      !any(c("Fenced words stay out.", "More fenced words.") %in% p))
check("fence: text after a four-backtick fence is read",
      "Outside words count." %in% p)
f <- plant("table.Rmd", c(yaml, "| Column | Other |", "|---|---|",
                          "| use `a|b` here | two |"))
p <- prose_of(f)
check("table: `|` inside a code span does not end the cell", "use here" %in% p)
check("table: the next cell is read (control)", "two" %in% p)
f <- plant("angle.Rmd", c(yaml, "Keep a < b and c > d in the text.", "",
                          "<span>Tag</span> words stay."))
p <- prose_of(f)
check("angle: prose between < and > is kept",
      "Keep a < b and c > d in the text." %in% p)
check("angle: an HTML tag is dropped (control)", "Tag words stay." %in% p)

cat(sprintf("%d of %d plants pass\n", sum(results), length(results)))
quit(status = if (all(results)) 0L else 1L)
