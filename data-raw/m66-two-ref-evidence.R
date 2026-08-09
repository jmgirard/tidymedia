# m66-two-ref-evidence.R — M66 AC2 evidence, re-derivable from the tree
# (committed at review after M66 review F12: a one-off measurement in the
# work log is not reproducible). Per M66 grid cell, strip_row_locator(branch msg)
# must equal the SAME call's message at the merge-base (master), byte-for-byte;
# argument-delivered complements must be byte-identical unstripped.
suppressMessages(devtools::load_all(quiet = TRUE))
source("data-raw/codec-guard-baseline.R")
test_that <- function(...) invisible(NULL)  # neutralize the file's test block
source("tests/testthat/helper-blame.R")
source("tests/testthat/test-row-locator-grid.R")

env_master <- codec_guard_env("master")
run <- local({
  input3 <- make_locator_input(3)
  specs <- locator_specs(input3)
  msg_of <- function(verb, args, env = NULL) {
    if (is.null(args$run)) args$run <- FALSE
    f <- tryCatch(
      if (is.null(env)) get(verb, envir = asNamespace("tidymedia"))
      else get(verb, envir = env),
      error = function(e) NULL)
    if (is.null(f)) return(NA_character_)
    cnd <- tryCatch({ do.call(verb, args,
                              envir = if (is.null(env)) asNamespace("tidymedia") else env)
                      NULL },
                    condition = function(cnd) cnd)
    if (is.null(cnd)) NA_character_
    else paste(cli::ansi_strip(conditionMessage(cnd)), collapse = "\n")
  }
  rows <- lapply(specs, function(cell) {
    b <- msg_of(cell$verb, cell$args)
    m <- msg_of(cell$verb, cell$args, env_master)
    data.frame(id = cell$id,
               locator = !isFALSE(cell$locator),
               both_abort = !is.na(b) && !is.na(m),
               head_equal = identical(strip_row_locator(b), m),
               raw_equal = identical(b, m))
  })
  do.call(rbind, rows)
})
cat("cells:", nrow(run), "\n")
cat("both refs abort:", sum(run$both_abort), "\n")
loc <- run[run$locator, ]
cat("locator cells with strip(branch) == master:", sum(loc$head_equal),
    "of", nrow(loc), "\n")
if (any(!loc$head_equal)) print(loc[!loc$head_equal, ])
cmp <- run[!run$locator, ]
cat("complement cells byte-identical unstripped:", sum(cmp$raw_equal),
    "of", nrow(cmp), "\n")
if (any(!cmp$raw_equal)) print(cmp[!cmp$raw_equal, ])
stopifnot(all(run$both_abort), all(loc$head_equal), all(cmp$raw_equal))
cat("AC2 two-ref: OK\n")
