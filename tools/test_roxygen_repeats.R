#!/usr/bin/env Rscript
# Plants repeated and non-repeated roxygen paragraphs and checks that
# tools/roxygen_repeats.R lists exactly the repeats. Exits 0 only when every
# plant passes, and 1 otherwise.
#
#   Rscript tools/test_roxygen_repeats.R [<repeats script>]
#
# Run from the package root. A developer tool, kept out of the build by
# `.Rbuildignore`'s `^tools$`.

args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(if (length(args) > 0) args[[1]] else "tools/roxygen_repeats.R",
                        mustWork = TRUE)
dir <- tempfile("repeat-plants-")
dir.create(dir)
on.exit(unlink(dir, recursive = TRUE))

run <- function(files) {
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c(shQuote(script), shQuote(files)),
    stdout = TRUE, stderr = FALSE
  ))
  status <- attr(out, "status")
  out <- as.character(out)
  attr(out, "status") <- if (is.null(status)) 0L else status
  out
}
# The listed paragraphs, as a named vector of block counts.
listed <- function(out) {
  heads <- grep("^== [0-9]+ blocks: ", out, value = TRUE)
  stats::setNames(as.integer(sub("^== ([0-9]+) blocks: .*$", "\\1", heads)),
                  sub("^== [0-9]+ blocks: ", "", heads))
}

results <- logical()
check <- function(name, ok) {
  ok <- isTRUE(ok)
  cat(sprintf("%s  %s\n", if (ok) "PASS" else "FAIL", name))
  results[[name]] <<- ok
}

src <- file.path(dir, "plants.R")
writeLines(c(
  "#' Title one",
  "#'",
  "#' Shared description text.",
  "#'",
  "#' @param x Shared argument text wrapped",
  "#'   here, with \\code{a very",
  "#'   long} macro.",
  "#' @param y Same text under another name.",
  "#' @return Shared return text.",
  "#' @section Details one:",
  "#' Shared section text.",
  "#' @family task functions",
  "#' @inheritParams base::paste",
  "#' @examples",
  "#' repeated_example(1)",
  "#' @export",
  "one <- function(x, y) NULL",
  "",
  "#' Title two",
  "#'",
  "#' Shared description text.",
  "#'",
  "#' @param x Shared argument text wrapped here, with \\code{a",
  "#'   very long} macro.",
  "#' @param z Same text under another name.",
  "#' @return Shared return text.",
  "#'   ",
  "#' Ended by a spaces line.",
  "#'   ",
  "#' @section Details two:",
  "#' Shared section text.",
  "#' @family task functions",
  "#' @inheritParams base::paste",
  "#' @examplesIf interactive()",
  "#' repeated_example(1)",
  "#' @export",
  "#' @rdname one",
  "two <- function(x, z) NULL",
  "",
  "#' Title three",
  "#'",
  "#' @param w,v Same text under another name.",
  "#' @param u `r shared_call()`",
  "#' @return Ended by a spaces line.",
  "#' @rdname one",
  "#' @examples",
  "#' repeated_example(1)",
  "three <- function(w, v, u) NULL",
  "",
  "#' Title four",
  "#'",
  "#' @param u `r shared_call()`",
  "#' @return Only here.",
  "#' @rdname one",
  "four <- function(u) NULL"
), src)

out <- run(src)
# A list, so a paragraph that is not listed reads as NULL rather than an error.
l <- as.list(listed(out))
check("exit: repeats found gives 1", attr(out, "status") == 1L)
check("description paragraph, ended by a blank line, in 2 blocks",
      identical(l[["Shared description text."]], 2L))
check("@param wrapped at different points, inside an Rd macro, ended by a tag",
      identical(l[["Shared argument text wrapped here, with \\code{a very long} macro."]],
                2L))
check("@param under two names and under `w,v`, in 3 blocks",
      identical(l[["Same text under another name."]], 3L))
check("@return ended by a `#'` line of spaces",
      identical(l[["Shared return text."]], 2L))
check("paragraph ended by a spaces line, and a @return, in 2 blocks",
      identical(l[["Ended by a spaces line."]], 2L))
check("@section text with its title removed",
      identical(l[["Shared section text."]], 2L))
check("example code, @examplesIf, @export, @family, @rdname and @inheritParams not listed",
      !any(grepl("repeated_example|interactive|task functions|base::paste|^one$|^export",
                 names(l))))
check("inline r call not listed", !any(grepl("shared_call", names(l))))
check("paragraph in one block not listed", !("Only here." %in% names(l)))
check("titles not listed", !any(grepl("^Title", names(l))))
check("nothing else listed", length(l) == 6L)

clean <- file.path(dir, "clean.R")
writeLines(c("#' Title", "#' @param x Text one.", "a <- 1", "",
             "#' Other title", "#' @param x Text two.", "b <- 1"), clean)
check("exit: no repeats gives 0 (control)", attr(run(clean), "status") == 0L)
check("exit: missing file gives 3",
      attr(run(file.path(dir, "absent.R")), "status") == 3L)
check("exit: no files gives 3", attr(run(character()), "status") == 3L)

cat(sprintf("%d of %d plants pass\n", sum(results), length(results)))
quit(status = if (all(results)) 0L else 1L)
