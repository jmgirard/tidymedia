# M136: the `quality` grid over the batch re-encoding verbs.
#
# The verb set is AC1's own filter, read off the namespace at test time: every
# export whose formals include `hardware` and `jobs`. `ifnotfound` because
# mget() over the exports hits rlang's reexported `.data` active binding
# (LESSONS M135).
quality_batch_verbs <- function() {
  ns <- asNamespace("tidymedia")
  fns <- mget(getNamespaceExports("tidymedia"), envir = ns,
              ifnotfound = list(NULL))
  keep <- Filter(function(f) {
    is.function(f) && all(c("hardware", "jobs") %in% names(formals(f)))
  }, fns)
  tm_sort_c(names(keep))
}

# One verb's two-job call from its own formals (nvenc_grid_args(),
# helper-nvenc-memo.R), reset to hardware = "none" and no codec override so
# each cell sets its own. Two rows: the first keeps the encoder default (NA),
# the second carries the value. Outputs are distinct per row already.
quality_batch_args <- function(fname, input) {
  args <- nvenc_grid_args(fname, input, rows = 2L)
  args$hardware <- "none"
  args$video_codec <- NULL
  args
}

# The rows a batch verb's `video_codec` surface and `hardware` can resolve to:
# the same rule as the scalar grid's quality_grid_rows().
quality_batch_rows <- function(fname) quality_grid_rows(fname)

# The compiled commands of ONE job, by its 1-based position in `jobs`. The
# separation verb returns an audio and a video command per job; every other
# verb returns one command per job.
quality_batch_job_commands <- function(out, job) {
  if ("stream" %in% names(out)) {
    out$command[seq(2L * job - 1L, 2L * job)]
  } else {
    out$command[[job]]
  }
}

# A job's VIDEO command: the one that carries `-codec:v` on the separation
# verb, and the job's only command elsewhere.
quality_batch_video_command <- function(out, job) {
  if ("stream" %in% names(out)) {
    out$command[out$stream == "video"][[job]]
  } else {
    out$command[[job]]
  }
}
