# M62 working checker: which verbs still fail the input-path front-door
# criteria. Not evidence -- the committed evidence is
# data-raw/input-guard-baseline.R and the tests. This exists so the wiring can
# be driven to completion mechanically.
#
# Run:  Rscript -e 'devtools::load_all(quiet=TRUE); source("data-raw/input-guard-progress.R")'

source("tests/testthat/helper-input-paths.R")

verbs <- input_guard_verbs()
specs <- input_guard_specs()
all_verbs <- c(verbs$fanout, verbs$scalar)

no_spec <- setdiff(all_verbs, names(specs))
orphan <- setdiff(names(specs), all_verbs)
if (length(no_spec)) cat("VERBS WITH NO SPEC:", paste(no_spec, collapse = ", "), "\n")
if (length(orphan)) cat("SPECS FOR NO VERB:", paste(orphan, collapse = ", "), "\n")

absent <- "m62-absent-input.mp4"
bad <- character()
for (v in all_verbs) {
  e <- tryCatch(specs[[v]](absent), error = function(e) e)
  call_txt <- paste(deparse(conditionCall(e)), collapse = " ")
  msg <- conditionMessage(e)
  # Assert WHICH failure, not that one occurred (M54): a malformed spec would
  # otherwise abort for its own reason and read as a pass.
  # "not exist" rather than "does not exist": check_paths_exist's plural
  # branch says "do not exist" for 2+ missing paths (pinned by
  # test-input-path-front-door.R), and the fan-in specs below legitimately
  # name the same absent path twice. Still distinct from ffm_files()'s
  # "Can't find or read" wording, so the discrimination this assertion exists
  # for (M54) is unaffected.
  ok <- grepl(paste0(v, "("), call_txt, fixed = TRUE) &&
    grepl("not exist", msg, fixed = TRUE)
  if (!ok) {
    bad <- c(bad, v)
    cat(sprintf("%-28s | %-32s | %s\n", v, substr(call_txt, 1, 32),
                substr(strsplit(msg, "\n")[[1]][1], 1, 44)))
  }
}
cat(sprintf("\n%d/%d verbs refuse at their own front door.\n",
            length(all_verbs) - length(bad), length(all_verbs)))
