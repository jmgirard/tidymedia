#!/usr/bin/env Rscript
# Runs the steps of `.github/workflows/fedora-cran-model.yaml` in a local
# container, read from that file, so a local reading is a reading of the
# committed job and not of a hand-made copy of it.
#
# Why it exists. M138's review returned twice because the committed workflow
# and the script that produced the quoted green run were different files. Here
# there is only one: each `run:` block is taken from the YAML, in order, and run
# the way GitHub runs it (`bash --noprofile --norc -eo pipefail`), with the
# job's own container image and `env:`. `actions/checkout` is modelled by
# unpacking `git archive HEAD`, which is the committed tree and nothing else. A
# `uses:` step this script cannot run is named and skipped, and the
# `upload-artifact` step is replaced by copying the same files out.
#
# Run from the package root, with docker (colima works) and the R `yaml`
# package: Rscript tools/fedora_workflow_local.R --out=DIR
# Exits with the container's status: 0 only when every step passed.
# A developer tool, kept out of the build by `.Rbuildignore`'s `^tools$`.

args <- commandArgs(trailingOnly = TRUE)
out <- sub("^--out=", "", grep("^--out=", args, value = TRUE))
if (length(out) != 1L) stop("usage: Rscript tools/fedora_workflow_local.R --out=DIR")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(out)

workflow <- ".github/workflows/fedora-cran-model.yaml"
job <- yaml::read_yaml(workflow)$jobs[["fedora-check"]]
image <- job$container
if (!is.character(image) || length(image) != 1L) stop("the job names no container image")

stage <- tempfile("fedora-steps-")
dir.create(stage)
driver <- c(
  "#!/bin/bash",
  "set -u",
  "mkdir -p /work/tidymedia && cd /work/tidymedia"
)
for (i in seq_along(job$steps)) {
  step <- job$steps[[i]]
  label <- step$name %||% step$uses %||% paste("step", i)
  if (!is.null(step$run)) {
    file <- sprintf("step_%02d.sh", i)
    writeLines(step$run, file.path(stage, file))
    driver <- c(
      driver,
      sprintf("echo '=== step %d: %s'", i, gsub("'", "", label)),
      sprintf(
        "bash --noprofile --norc -eo pipefail /steps/%s || { echo '=== FAILED at step %d'; exit 1; }",
        file, i
      )
    )
  } else if (grepl("^actions/checkout@", step$uses %||% "")) {
    driver <- c(
      driver,
      sprintf("echo '=== step %d: %s (the committed tree, from git archive)'", i, label),
      "tar -xf /src.tar -C /work/tidymedia"
    )
  } else {
    driver <- c(driver, sprintf("echo '=== step %d: %s (skipped: GitHub-only)'", i, label))
  }
}
driver <- c(driver, "echo '=== every step passed'")
writeLines(driver, file.path(stage, "driver.sh"))

src <- file.path(stage, "src.tar")
if (system2("git", c("archive", "--format=tar", "-o", shQuote(src), "HEAD")) != 0L) {
  stop("git archive HEAD failed")
}

name <- paste0("tm-fedora-workflow-", Sys.getpid())
env <- unlist(lapply(names(job$env), function(k) c("-e", paste0(k, "=", job$env[[k]]))))
docker <- function(...) system2("docker", c(...))
if (docker("create", "--name", name, env, image, "bash", "/steps/driver.sh") != 0L) {
  stop("docker create failed")
}
on.exit(docker("rm", "-f", name, stdout = FALSE), add = TRUE)
docker("cp", shQuote(stage), paste0(name, ":/steps"))
docker("cp", shQuote(src), paste0(name, ":/src.tar"))

status <- system2(
  "docker", c("start", "-a", name),
  stdout = file.path(out, "run.log"), stderr = file.path(out, "run.log")
)
for (f in c("00check.log", "00install.out", "tests")) {
  docker(
    "cp", paste0(name, ":/work/tidymedia/tidymedia.Rcheck/", f),
    shQuote(file.path(out, f)), stdout = FALSE, stderr = FALSE
  )
}
cat("container exit status:", status, "\nlog:", file.path(out, "run.log"), "\n")
quit(status = status)
