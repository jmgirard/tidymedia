#!/usr/bin/env python3
"""M64 AC5 mutation harness -- are the new blame sweeps falsifiable?

A sweep that can be deleted without a test going red is not covered, however
many cells appear to exercise it. This script re-derives the mutation list,
deletes each site in turn, and records which grid cells notice -- so "verified
by mutation" is a re-runnable procedure rather than an implementation-time
transcript.

THE MUTATION LIST IS DERIVED, NOT DECLARED: it is the added lines of
`git diff <merge-base> -- R/` that call one of the swept checkers
(check_dim / check_token / resolve_sample_fps), located by the diff's own
new-file line numbers. A hand-kept list would drift the first time a site
moved; deriving it means a checker call the branch adds later is a mutation
this harness picks up unasked (M64 AC5). Comment lines are excluded; each
site's line content is re-verified against the file before it is deleted.

For each site: delete that one line, run
tests/testthat/test-builder-blame-front-door.R, and require at least one
failing expectation whose `info` names a cell id OWNED by that site's verb --
the id prefix is derived from the function enclosing the site
(standardize_pipeline's checks belong to standardize_video, its only Layer-2
caller in the grid). "Some cell went red" is not enough: another sweep's red
would satisfy it while this site went unwatched (the AC5 wording).

Two further mutations aim at the instruments themselves:

  reader   neuter blame_spec_defects() (helper-blame-specs.R) to return no
           defects; the planted-defect test in the grid file must go RED.
  controls remove one crossed guard from R/ffmpeg.R (crop_video_batch's
           "`width` is required") and require blame_precedence()'s
           precedence_dead_controls() to report the dead control; then, with
           the guard still removed, neuter the control check in
           data-raw/blame-precedence.R (control_ok <- TRUE) and require that
           same report to VANISH -- the vanishing is the red that shows the
           control machinery, not the guard, was doing the detecting.

Every edited file is restored in a `finally`, so an interrupted run leaves the
working tree as it found it; `git status` after a run is the check.

Run from the package root:  python3 data-raw/blame-guard-mutations.py
Exit status 0 iff every mutation produced its required red.
"""
import re
import shutil
import subprocess
import sys

CHECKER_RE = re.compile(r"(check_dim|check_token|resolve_sample_fps)\s*\(")

# The grid's cell ids start with the verb that owns them ("crop_video/width",
# "standardize_video_batch/pixel_format/column"). A site's owner is the
# function it sits in, mapped to the verb whose cells watch it where the two
# differ. The trailing "/" keeps "crop_video/" from matching a
# "crop_video_batch/..." id.
OWNER_OF_FUNCTION = {"standardize_pipeline": "standardize_video"}

MERGE_BASE = subprocess.run(
    ["git", "merge-base", "master", "HEAD"],
    capture_output=True, text=True, check=True).stdout.strip()


def sh(args):
    return subprocess.run(args, capture_output=True, text=True).stdout


def derive_sites():
    """(path, new-file line number, line text) per added checker call."""
    sites = []
    path = None
    newline = None
    for line in sh(["git", "diff", "-U0", MERGE_BASE, "--", "R/"]).splitlines():
        if line.startswith("+++ b/"):
            path = line[6:]
            continue
        m = re.match(r"@@ -\S+ \+(\d+)", line)
        if m:
            newline = int(m.group(1))
            continue
        if line.startswith("+") and not line.startswith("+++"):
            text = line[1:]
            if CHECKER_RE.search(text) and not text.lstrip().startswith("#"):
                sites.append((path, newline, text))
            newline += 1
    return sites


def enclosing_function(path, lineno):
    lines = open(path).readlines()
    for i in range(lineno - 1, -1, -1):
        m = re.match(r"([A-Za-z0-9_.]+) <- function", lines[i])
        if m:
            return m.group(1)
    raise AssertionError(f"no enclosing function above {path}:{lineno}")


def delete_line(path, lineno, expect_text):
    lines = open(path).readlines()
    got = lines[lineno - 1].rstrip("\n")
    assert got == expect_text, (
        f"{path}:{lineno} is {got!r}, expected {expect_text!r} -- "
        "stale diff? re-run from a clean tree")
    del lines[lineno - 1]
    open(path, "w").writelines(lines)


def mutate_text(path, needle, replacement):
    src = open(path).read()
    n = src.count(needle)
    assert n == 1, f"anchor matched {n} times in {path}"
    open(path, "w").write(src.replace(needle, replacement))


def failing_expectations():
    """[(test title, failure message)] for the blame grid file."""
    code = (
        "suppressMessages(devtools::load_all(quiet=TRUE)); "
        "r <- testthat::test_local(filter='builder-blame', reporter='silent', "
        "stop_on_failure=FALSE); "
        "for (t in r) for (e in t$results) "
        "if (inherits(e, c('expectation_failure', 'expectation_error'))) "
        "cat('<<<TEST>>>', t$test, '<<<MSG>>>', conditionMessage(e), sep='')")
    out = sh(["Rscript", "-e", code])
    fails = []
    for chunk in out.split("<<<TEST>>>")[1:]:
        test, _, msg = chunk.partition("<<<MSG>>>")
        fails.append((test, msg))
    return fails


def dead_control_ids():
    code = (
        "source('data-raw/blame-precedence.R'); "
        "x <- blame_precedence(); "
        "cat(paste(precedence_dead_controls(x)$id, collapse='|'))")
    out = sh(["Rscript", "-e", code]).strip()
    return [i for i in out.split("|") if i]


class Restore:
    def __init__(self, *paths):
        self.pairs = [(p, p + ".mutbak") for p in paths]

    def __enter__(self):
        for path, bak in self.pairs:
            shutil.copy(path, bak)

    def __exit__(self, *exc):
        for path, bak in self.pairs:
            shutil.move(bak, path)


results = []


def record(label, ok, detail):
    results.append((label, ok, detail))
    print(f"{'RED ' if ok else 'FAIL'}  {label}: {detail}", flush=True)


# -- baseline: the unmutated grid must be green ------------------------------
baseline = failing_expectations()
if baseline:
    print("baseline is not green; refusing to attribute reds:")
    for test, msg in baseline:
        print(f"  [{test}] {msg.splitlines()[0]}")
    sys.exit(1)
print("baseline green", flush=True)

# -- the derived sweep mutations ---------------------------------------------
sites = derive_sites()
assert sites, "derived no sites -- wrong branch or merge-base?"
print(f"derived {len(sites)} sites from git diff {MERGE_BASE[:12]} -- R/")
for path, lineno, text in sites:
    fn = enclosing_function(path, lineno)
    owner = OWNER_OF_FUNCTION.get(fn, fn) + "/"
    label = f"{path}:{lineno} {text.strip()} [{fn}]"
    with Restore(path):
        delete_line(path, lineno, text)
        fails = failing_expectations()
    owned = sorted({m.group(0)
                    for _, msg in fails
                    for m in [re.search(re.escape(owner) + r"\S*", msg)]
                    if m})
    record(label, bool(owned),
           f"owned reds: {', '.join(owned) if owned else '(NONE)'}"
           f" ({len(fails)} failing expectations)")

# -- the reader mutation ------------------------------------------------------
HELPER = "tests/testthat/helper-blame-specs.R"
with Restore(HELPER):
    mutate_text(HELPER, "blame_spec_defects <- function(specs) {",
                "blame_spec_defects <- function(specs) {\n  return(character(0))")
    fails = failing_expectations()
reader_red = [t for t, _ in fails
              if t == "the completeness reader detects the defects it exists for"]
record("reader neutered (blame_spec_defects returns nothing)", bool(reader_red),
       "planted-defect test red" if reader_red else
       "planted-defect test DID NOT notice")

# -- the control mutations ----------------------------------------------------
GUARD = """  for (dim in c("width", "height")) {
    if (is.null(get(dim)) && !dim %in% names(jobs)) {
      cli::cli_abort(c(
        "{.arg {dim}} is required.",
        "i" = "Pass {.arg {dim}} (applied to every row) or add a {.field {dim}} column."
      ))
    }
  }
"""
PRECEDENCE = "data-raw/blame-precedence.R"
CONTROL_OK = """    control_ok <- control$kind == "abort" &&
      grepl(cell$crossed_marker, control$msg)"""

with Restore("R/ffmpeg.R"):
    mutate_text("R/ffmpeg.R", GUARD, "")
    dead = dead_control_ids()
    record("crossed guard removed (crop_video_batch width-required)",
           any("width-required" in i for i in dead),
           f"dead controls reported: {', '.join(dead) or '(NONE)'}")

    with Restore(PRECEDENCE):
        mutate_text(PRECEDENCE, CONTROL_OK, "    control_ok <- TRUE")
        dead_neutered = dead_control_ids()
    record("controls neutered (control_ok forced TRUE), guard still removed",
           not dead_neutered,
           "the dead-control report vanished -- the controls were the detector"
           if not dead_neutered else
           f"report SURVIVED the neutering: {', '.join(dead_neutered)}")

# -- summary ------------------------------------------------------------------
print("\n--- summary ---")
bad = [r for r in results if not r[1]]
for label, ok, detail in results:
    print(f"{'RED ' if ok else 'FAIL'}  {label}")
sys.exit(1 if bad else 0)
