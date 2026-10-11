#!/usr/bin/env Rscript
# Runs the steps of `.github/workflows/fedora-cran-model.yaml` in a local
# container, read from that file, so a local reading is a reading of the
# committed job and not of a hand-made copy of it.
#
# Why it exists. M138's review returned twice because the committed workflow
# and the script that produced the quoted green run were different files. Here
# there is only one: each `run:` block is taken from the YAML as committed at
# HEAD, in order, and run the way GitHub runs a step that sets no `shell:`
# (`bash -e`), with the job's own container image and `env:`.
# `actions/checkout` is modelled by unpacking `git archive HEAD`, which is the
# committed tree and nothing else. A `uses:` step this script cannot run is
# named and skipped, and the `upload-artifact` step is replaced by copying the
# same files out. `CI=true` and `HOME=/github/home` are set as GitHub sets them
# in a container job. A `run:` step that sets `shell:`, `if:`,
# `working-directory:`, `env:`, `continue-on-error:` or `timeout-minutes:`, or
# a job that sets `defaults:`, stops the script, because it does not model
# those keys.
#
# What it cannot model: a `pull_request` run checks out GitHub's merge of the
# branch into the base, where this unpacks the branch's own HEAD, and it runs
# on the host's architecture (aarch64 under colima), where GitHub runs x86_64.
#
# Run from the package root, with docker (colima works), R 4.4 or later, and
# the R `yaml` package: Rscript tools/fedora_workflow_local.R --out=DIR
# Exits with the container's status: 0 only when every step passed.
# A developer tool, kept out of the build by `.Rbuildignore`'s `^tools$`.

args <- commandArgs(trailingOnly = TRUE)
out <- sub("^--out=", "", grep("^--out=", args, value = TRUE))
if (length(out) != 1L) stop("usage: Rscript tools/fedora_workflow_local.R --out=DIR")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(out)

# The workflow as committed, not as it sits in the working tree, so the steps
# and the package source come from the same commit. HEAD is resolved once, so
# a commit made while this runs cannot split the two reads.
sha <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
workflow <- system2(
  "git", c("show", paste0(sha, ":.github/workflows/fedora-cran-model.yaml")),
  stdout = TRUE
)
job <- yaml::yaml.load(paste(workflow, collapse = "\n"))$jobs[["fedora-check"]]
image <- job$container
if (!is.character(image) || length(image) != 1L) stop("the job names no container image")
if (!is.null(job$defaults)) stop("the job sets `defaults:`, which this script does not model")

stage <- tempfile("fedora-steps-")
dir.create(stage)
driver <- c(
  "#!/bin/bash",
  "set -u",
  "mkdir -p /work/tidymedia \"$HOME\" && cd /work/tidymedia",
  sprintf("echo '=== commit %s'", sha)
)
for (i in seq_along(job$steps)) {
  step <- job$steps[[i]]
  label <- step$name %||% step$uses %||% paste("step", i)
  unmodelled <- intersect(
    names(step),
    c("shell", "if", "working-directory", "env", "continue-on-error",
      "timeout-minutes")
  )
  if (!is.null(step$run) && length(unmodelled)) {
    stop("step ", i, " sets ", paste(unmodelled, collapse = ", "),
         ", which this script does not model")
  }
  if (!is.null(step$run)) {
    file <- sprintf("step_%02d.sh", i)
    writeLines(step$run, file.path(stage, file))
    driver <- c(
      driver,
      sprintf("echo '=== step %d: %s'", i, gsub("'", "", label)),
      sprintf(
        "bash -e /steps/%s || { echo '=== FAILED at step %d'; exit 1; }",
        file, i
      )
    )
  } else if (grepl("^actions/checkout@", step$uses %||% "")) {
    driver <- c(
      driver,
      sprintf("echo '=== step %d: %s (the committed tree, from git archive)'", i, label),
      "tar -xf /src.tar -C /work/tidymedia || { echo '=== FAILED at the checkout'; exit 1; }"
    )
  } else {
    driver <- c(driver, sprintf("echo '=== step %d: %s (skipped: GitHub-only)'", i, label))
  }
}
driver <- c(driver, "echo '=== every step passed'")
writeLines(driver, file.path(stage, "driver.sh"))

src <- file.path(stage, "src.tar")
if (system2("git", c("archive", "--format=tar", "-o", shQuote(src), sha)) != 0L) {
  stop("git archive failed")
}

name <- paste0("tm-fedora-workflow-", Sys.getpid())
job_env <- c(list(CI = "true", HOME = "/github/home"), job$env)
env <- unlist(lapply(names(job_env), function(k) c("-e", paste0(k, "=", job_env[[k]]))))
docker <- function(args, ...) system2("docker", args, ...)
if (docker(c("create", "--name", name, env, image, "bash", "/steps/driver.sh")) != 0L) {
  stop("docker create failed")
}
docker(c("cp", shQuote(stage), paste0(name, ":/steps")))
docker(c("cp", shQuote(src), paste0(name, ":/src.tar")))

status <- system2(
  "docker", c("start", "-a", name),
  stdout = file.path(out, "run.log"), stderr = file.path(out, "run.log")
)
for (f in c("00check.log", "00install.out", "tests")) {
  docker(
    c("cp", paste0(name, ":/work/tidymedia/tidymedia.Rcheck/", f),
      shQuote(file.path(out, f))),
    stdout = FALSE, stderr = FALSE
  )
}
# Removed here rather than by `on.exit()`, which does not run at the top level
# of an Rscript file.
docker(c("rm", "-f", name), stdout = FALSE)
cat("container exit status:", status, "\nlog:", file.path(out, "run.log"), "\n")
quit(status = status)
