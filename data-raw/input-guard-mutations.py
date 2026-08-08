#!/usr/bin/env python3
"""M62 AC7 mutation harness -- are the input-guard grid's own checkers falsifiable?

`data-raw/input-guard-baseline.R` is evidence only if its readers would notice
being wrong. Three of them carry the milestone's completeness and ordering
claims, and none is exercised by the grid running green -- a reader that always
returned zero rows would look exactly the same. So each is broken in turn and
required to report:

  1  a verb's CALL SHAPE deleted        -> input_guard_uncovered() must report
                                           every combination that verb owed
  2  a crossing's CONTROL re-pointed    -> input_guard_dead_controls() must
        at a different error               report that crossing's controls
  3  a CALL EDGE deleted from R/        -> input_guard_verbs() must return a
                                           different fan-out set

Mutation 3 reaches past the grid into the walk that supplies its domain: the
declarations are checked against `input_guard_verbs()`, so a walk that quietly
stopped seeing an edge would shrink the grid and every reader would stay green.

Each mutation is applied to a copy, and the file is restored in a `finally`, so
an interrupted run still leaves the working tree as it found it; `git status`
after a run is the check.

Run from anywhere:  python3 data-raw/input-guard-mutations.py
"""
import subprocess, os, shutil

ROOT = "/Users/jmgirard/GitHub/tidymedia"
os.chdir(ROOT)

GRID = "data-raw/input-guard-baseline.R"
SRC = "R/ffmpeg.R"

# The R probe each mutation is judged by. Each prints one line naming what the
# reader reported, so a GREEN reader is visible as an empty report rather than
# inferred from an exit code.
PROBE_GRID = """
suppressMessages(devtools::load_all(quiet = TRUE))
source("data-raw/input-guard-baseline.R")
after <- input_guard_baseline()
u <- input_guard_uncovered(after)
d <- input_guard_dead_controls(after)
cat("uncovered:", nrow(u), "\n")
if (nrow(u)) cat(paste(sprintf("  %s / %s / %s", u$verb, u$form, u$crossing),
                       collapse = "\n"), "\n")
cat("dead_controls:", nrow(d), "\n")
if (nrow(d)) cat(paste(sprintf("  %s / %s / %s -> reported %s",
                               d$verb, d$form, d$crossing, d$reported),
                       collapse = "\n"), "\n")
"""

PROBE_WALK = """
suppressMessages(devtools::load_all(quiet = TRUE))
source("tests/testthat/helper-input-paths.R")
v <- input_guard_verbs()
cat("fanout:", paste(v$fanout, collapse = ","), "\n")
cat("scalar:", paste(v$scalar, collapse = ","), "\n")
"""

# (label, file, needle, replacement, probe)
MUTATIONS = [
    (
        "1. crop_video_batch's call shape deleted",
        GRID,
        """  crop_video_batch = tm_shape_input(
    "crop_video_batch",
    extra = function(n) list(
      output = vapply(seq_len(n), tm_out, character(1))),
    args = list(width = 10, height = 10)),
""",
        "",
        PROBE_GRID,
    ),
    (
        # Re-pointed at a LIVE different error, not at no error: a control that
        # merely stopped aborting would also be caught, but by the vacuity
        # screen. What this mutation asks is whether dead_controls() checks the
        # control against its own crossing rather than against "some abort".
        "2. the audio_codec contradiction's control re-pointed at the run guard",
        GRID,
        '    "contradiction:audio_codec" = list(args = list(audio_codec = "aac")),',
        '    "contradiction:audio_codec" = list(args = list(run = "yes")),',
        PROBE_GRID,
    ),
    (
        # strip_metadata_batch's single ffm_batch() call: with the edge gone the
        # walk cannot reach ffm_batch from it, and the verb leaves the fan-out
        # set the criteria quantify over. `ffm_files` is still reachable through
        # the pipeline, so it lands in the scalar set instead -- a MOVE, which is
        # a sharper signal than a disappearance.
        "3. strip_metadata_batch's ffm_batch call edge deleted",
        SRC,
        "  ffm_batch(\n    jobs,\n    function(input, output, ...) strip_metadata_pipeline(input, output),",
        "  list(\n    jobs,\n    function(input, output, ...) strip_metadata_pipeline(input, output),",
        PROBE_WALK,
    ),
]


def run(probe):
    p = subprocess.run(["Rscript", "-e", probe], capture_output=True, text=True)
    return (p.stdout.strip() or "(no output)") + (
        "\n  stderr: " + p.stderr.strip()[-400:] if p.returncode else "")


print("=== baseline (unmutated) ===")
base_grid = run(PROBE_GRID)
base_walk = run(PROBE_WALK)
print(base_grid)
print(base_walk)

results = []
for label, path, needle, repl, probe in MUTATIONS:
    backup = path + ".mutbak"
    shutil.copy(path, backup)
    try:
        src = open(path).read()
        n = src.count(needle)
        assert n == 1, f"anchor matched {n} times in {path}: {label}"
        open(path, "w").write(src.replace(needle, repl))
        out = run(probe)
    finally:
        shutil.move(backup, path)
    base = base_grid if probe is PROBE_GRID else base_walk
    results.append((label, out, out != base))
    print(f"\n=== {label}\n{out}")

print("\n--- summary ---")
for label, out, moved in results:
    print(f"{'CAUGHT ' if moved else 'MISSED '} {label}")
