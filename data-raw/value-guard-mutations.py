#!/usr/bin/env python3
"""M59 AC2 mutation harness -- are the six front-door guards falsifiable?

A guard that cannot be deleted without a test going red is not covered, however
many tests appear to exercise it. This script deletes each guard in turn and
records which tests notice, so AC2's "verified by mutation" is a re-runnable
procedure rather than an implementation-time transcript.

For each of the six FRONT-DOOR calls: delete it, run
tests/testthat/test-value-check-front-door.R, require the AC1 blame test to go
RED. For each SHARED call a scalar verb also reaches (sites 1, 3, 5, 6): delete
it, require the scalar-siblings test to go RED. Sites 2 and 4 have no shared
call -- M59-D2 retired their fan-out closure copies -- so they have no second
half.

The edited file is restored in a `finally`, so an interrupted run still leaves
the working tree as it found it; `git status` after a run is the check.

Run from anywhere:  python3 data-raw/value-guard-mutations.py

Two anchors are line numbers rather than text: `check_dim(width)` /
`check_dim(height)` appear at four builders in R/ffm.R, and only ffm_crop()'s
pair is the site 1 mutation. Re-point them if that file moves.
"""
import subprocess, sys, os, shutil

ROOT = "/Users/jmgirard/GitHub/tidymedia"
os.chdir(ROOT)

FRONT = {
  "site1 crop_video_batch check_dim sweep": ("R/ffmpeg.R", """  for (dim in c("width", "height")) {
    for (value in batch_arg_rows(jobs, dim, get(dim))) {
      check_dim(value, arg = dim)
    }
  }
"""),
  "site2 pip margin sweep": ("R/ffmpeg.R", """  for (value in batch_arg_rows(jobs, "margin", margin)) {
    rlang::check_number_whole(value, min = 0, arg = "margin")
  }
"""),
  "site3 anonymize regions sweep": ("R/ffmpeg.R", """  for (cell in jobs$regions) {
    check_regions(cell)
  }
"""),
  "site4 compare audio index sweep": ("R/ffmpeg.R", """  for (i in seq_len(nrow(jobs))) {
    if (!is.null(audio_rows[[i]])) {
      rlang::check_number_whole(audio_rows[[i]], min = 0,
                                max = length(jobs$inputs[[i]]) - 1,
                                arg = "audio")
    }
  }
"""),
  "site5 compare direction sweep": ("R/ffmpeg.R",
    '  check_batch_vocab_col(jobs, "direction", direction, stack_directions())\n'),
  "site6 pip position sweep": ("R/ffmpeg.R",
    '  check_batch_vocab_col(jobs, "position", position, pip_positions())\n'),
}

SHARED = {
  "site1 ffm_crop check_dim": ("R/ffm.R", "LINES", (287, 288)),
  "site3 anonymize_pipeline check_regions": ("R/ffmpeg.R",
    "  check_regions(regions, call = call)\n"),
  "site5 compare_videos_pipeline check_vocab_arg": ("R/ffmpeg.R",
    """  direction <- check_vocab_arg(direction, stack_directions(), "direction",
                               call = call)
"""),
  "site6 picture_in_picture_pipeline check_vocab_arg": ("R/ffmpeg.R",
    """  position <- check_vocab_arg(position, pip_positions(), "position",
                              call = call)
"""),
}

TEST = ("devtools::load_all(quiet=TRUE); "
        "testthat::test_local(filter='value-check-front-door', "
        "reporter='silent', stop_on_failure=FALSE)")

def run_tests(which):
    """Return the set of failing test names."""
    code = ("suppressMessages({devtools::load_all(quiet=TRUE)}); "
            "r <- testthat::test_local(filter='value-check-front-door', "
            "reporter='silent', stop_on_failure=FALSE); "
            "d <- as.data.frame(r); "
            "cat(paste(unique(d$test[d$failed > 0 | d$error]), collapse='|||'))")
    p = subprocess.run(["Rscript", "-e", code], capture_output=True, text=True)
    return p.stdout.strip()

def mutate_text(path, needle):
    src = open(path).read()
    n = src.count(needle)
    assert n == 1, f"anchor matched {n} times in {path}"
    open(path, "w").write(src.replace(needle, ""))

def mutate_lines(path, lo, hi):
    lines = open(path).readlines()
    del lines[lo-1:hi]
    open(path, "w").writelines(lines)

results = []
for label, spec in list(FRONT.items()) + list(SHARED.items()):
    path = spec[0]
    backup = path + ".mutbak"
    shutil.copy(path, backup)
    try:
        if spec[1] == "LINES":
            mutate_lines(path, *spec[2])
        else:
            mutate_text(path, spec[1])
        failing = run_tests(label)
    finally:
        shutil.move(backup, path)
    results.append((label, failing))
    print(f"=== {label}\n    failing: {failing or '(NONE -- guard is not falsifiable)'}",
          flush=True)

print("\n--- summary ---")
for label, failing in results:
    print(f"{'RED ' if failing else 'GREEN'}  {label}: {failing}")
