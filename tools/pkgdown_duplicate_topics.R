#!/usr/bin/env Rscript
# Reports every help topic named more than once across the `contents:` entries
# of `_pkgdown.yml`'s reference index, and every entry that matches no topic.
# Exits 1 if either list is non-empty.
#
# An entry may be an alias (`set_ffmpeg`) rather than its topic's own name
# (`set_program`), so entries are resolved to their .Rd file before counting:
# two aliases of one topic are a repeat even though the two strings differ.
# Run from the package root: Rscript tools/pkgdown_duplicate_topics.R
# Requires the `yaml` package, which the package itself does not depend on:
# this is a developer tool, kept out of the build by `.Rbuildignore`'s `^tools$`.

yml <- yaml::read_yaml("_pkgdown.yml")
entries <- unlist(lapply(yml$reference, function(sec) sec$contents), use.names = FALSE)
entries <- entries[!grepl("[()]", entries)]  # skip pkgdown selector expressions
if (length(entries) == 0) stop("no contents entries found -- the parse is empty")

rd <- list.files("man", pattern = "[.]Rd$", full.names = TRUE)
if (length(rd) == 0) stop("no .Rd files found in man/")
alias_of <- unlist(lapply(rd, function(f) {
  lines <- grep("^\\\\alias\\{", readLines(f, warn = FALSE), value = TRUE)
  a <- sub("^\\\\alias\\{(.*)\\}$", "\\1", lines)
  stats::setNames(rep(basename(f), length(a)), a)
}))

topics <- unname(alias_of[entries])
unknown <- entries[is.na(topics)]
counts <- table(topics[!is.na(topics)])
dupes <- names(counts)[counts > 1]

cat("contents entries:", length(entries), "\n")
cat("man/ topics:", length(rd), "\n")
if (length(unknown) == 0) {
  cat("entries matching no topic: none\n")
} else {
  cat("entries matching no topic:", paste(unknown, collapse = ", "), "\n")
}
if (length(dupes) == 0) {
  cat("repeated topics: none\n")
} else {
  for (t in dupes) {
    cat("repeated topic:", t, "<-",
        paste(entries[!is.na(topics) & topics == t], collapse = ", "), "\n")
  }
}
quit(status = if (length(dupes) > 0 || length(unknown) > 0) 1L else 0L)
