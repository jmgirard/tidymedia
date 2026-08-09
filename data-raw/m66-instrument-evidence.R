# m66-instrument-evidence.R — M66 AC2/AC3 instrument evidence, re-derivable
# from the tree (committed at review after M66 review F13: the marker-loop
# check had no committed instrument). Baseline, both precedence grids, and
# the locator-vs-marker loop (AC2 marker check, AC3 baseline + flips).
suppressMessages(devtools::load_all(quiet = TRUE))

cat("== blame baseline (M64 grid) across master..tree ==\n")
source("data-raw/blame-baseline.R")
before <- blame_baseline("master")
after <- blame_baseline()
cat("vacuous(before):", nrow(blame_vacuous(before)),
    " vacuous(after):", nrow(blame_vacuous(after)), "\n")
mv <- blame_moves(before, after)
cat("blame moves:", nrow(mv), "\n")
if (nrow(mv)) print(mv)
dr <- blame_message_drift(before, after)
cat("message drift rows:", nrow(dr), "\n")
if (nrow(dr)) print(dr)

cat("\n== scalar-form byte identity (AC3) ==\n")
scalar <- !grepl("_batch", before$id)
same <- before$outcome[scalar] == after$outcome[scalar]
cat("scalar cells:", sum(scalar), " byte-identical:", sum(same), "\n")
if (any(!same)) print(before$id[scalar][!same])

cat("\n== precedence (M64) master..tree ==\n")
source("data-raw/blame-precedence.R")
pb <- blame_precedence("master")
pa <- blame_precedence()
cat("dead controls before/after:", nrow(precedence_dead_controls(pb)), "/",
    nrow(precedence_dead_controls(pa)), "\n")
cat("unresolved before/after:", nrow(precedence_unresolved(pb)), "/",
    nrow(precedence_unresolved(pa)), "\n")
fl <- precedence_flips(pb, pa)
cat("flips:", nrow(fl), "\n")
if (nrow(fl)) print(fl)

cat("\n== precedence (M65) master..tree ==\n")
source("data-raw/blame-precedence-m65.R")
qb <- blame_precedence_m65("master")
qa <- blame_precedence_m65()
cat("dead controls before/after:", nrow(precedence_dead_controls(qb)), "/",
    nrow(precedence_dead_controls(qa)), "\n")
cat("unresolved before/after:", nrow(precedence_unresolved(qb)), "/",
    nrow(precedence_unresolved(qa)), "\n")
fl2 <- precedence_flips(qb, qa)
cat("flips:", nrow(fl2), "\n")
if (nrow(fl2)) print(fl2)

cat("\n== AC2: the rendered locator matches no instrument marker ==\n")
locator <- "x First offending jobs row: 3."
sample <- normalizePath(file.path("inst", "extdata", "sample.mp4"),
                        mustWork = TRUE)
all_cells <- c(blame_precedence_cells(sample), blame_precedence_cells_m65(sample))
markers <- unique(na.omit(unlist(lapply(all_cells, function(cl)
  c(cl$sweep_marker, cl$crossed_marker)))))
cat("markers enumerated:", length(markers), "\n")
hits <- markers[vapply(markers, function(m) grepl(m, locator), logical(1))]
cat("markers matching the locator:", length(hits), "\n")
if (length(hits)) print(hits)
stopifnot(length(markers) > 0, length(hits) == 0)
cat("\nDONE\n")
