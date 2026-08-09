#!/usr/bin/env python3
"""M65 AC6 mutation harness -- are the six new sweeps and both instruments
falsifiable?

The M65 sibling of data-raw/blame-guard-mutations.py (M64). THE MUTATION LIST
IS DERIVED, NOT DECLARED: the added lines of `git diff <merge-base> -- R/`
that call one of the M65 shared checkers (check_region_values /
check_overlay_scale / check_loudnorm_targets), located by the diff's own
new-file line numbers. The checker DEFINITIONS in R/utils.R do not match (the
name is followed by ` <- function`, not `(`), so the derived sites are
exactly the call sites the branch adds.

Each site's required red depends on WHICH layer it sits in (AC6):

  Layer 2 (a verb's or pipeline's sweep, in R/ffmpeg.R): at least one failing
  expectation in the M65 blame grid whose `info` cell id is OWNED by that
  site's verb (anonymize_pipeline's sweep belongs to anonymize_video, its
  only Layer-2 caller in the grid; picture_in_picture_pipeline's to
  picture_in_picture). Another verb's red does not count.

  Layer 1 (ffm_overlay's / ffm_loudnorm's read of the binding, in R/ffm.R):
  the grid CANNOT see it -- every grid cell aborts at a front door that is
  still standing -- so the required red is AC1's direct-builder probe in
  test-shared-range-bindings.R, matched by that builder's own test title.

Two further mutations aim at the instruments themselves:

  reader   neuter blame_spec_defects_m65() (helper-blame-specs-m65.R) to
           return no defects; the M65 planted-defect test must go RED.
  controls remove one crossed guard from R/ffmpeg.R (normalize_audio's
           `rlang::check_bool(two_pass)`) and require blame_precedence_m65()'s
           precedence_dead_controls() to report the dead control; then, with
           the guard still removed, neuter the control check in
           data-raw/blame-precedence.R (control_ok <- TRUE) and require that
           same report to VANISH -- the vanishing shows the control machinery,
           not the guard, was doing the detecting.

Every edited file is restored in a `finally`; `git status` after a run is the
check. Run from the package root:  python3 data-raw/blame-guard-mutations-m65.py
Exit status 0 iff every mutation produced its required red.
"""
import re
import shutil
import subprocess
import sys

CHECKER_RE = re.compile(
    r"(check_region_values|check_overlay_scale|check_loudnorm_targets)\s*\(")

# A Layer-2 site's owner: the enclosing function, mapped to the verb whose grid
# cells watch it where the two differ. The trailing "/" keeps
# "normalize_audio/" from matching a "normalize_audio_batch/..." id.
OWNER_OF_FUNCTION = {
    "anonymize_pipeline": "anonymize_video",
    "picture_in_picture_pipeline": "picture_in_picture",
}

# A Layer-1 site's required red: a failing expectation in this AC1 test.
LAYER1_TEST_OF_FUNCTION = {
    "ffm_overlay": "the overlay scale range is one binding read by both layers",
    "ffm_loudnorm":
        "the three loudness ranges are one binding each, read by both layers",
}

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
    """[(test title, failure message)] over the grid AND the AC1 probes."""
    code = (
        "suppressMessages(devtools::load_all(quiet=TRUE)); "
        "r <- testthat::test_local(filter='builder-blame|shared-range', "
        "reporter='silent', stop_on_failure=FALSE); "
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
        "source('data-raw/blame-precedence-m65.R'); "
        "x <- blame_precedence_m65(); "
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


# -- baseline: the unmutated suite must be green ------------------------------
baseline = failing_expectations()
if baseline:
    print("baseline is not green; refusing to attribute reds:")
    for test, msg in baseline:
        print(f"  [{test}] {msg.splitlines()[0]}")
    sys.exit(1)
print("baseline green", flush=True)

# -- the derived sweep mutations ----------------------------------------------
sites = derive_sites()
assert sites, "derived no sites -- wrong branch or merge-base?"
print(f"derived {len(sites)} sites from git diff {MERGE_BASE[:12]} -- R/")
for path, lineno, text in sites:
    fn = enclosing_function(path, lineno)
    label = f"{path}:{lineno} {text.strip()} [{fn}]"
    with Restore(path):
        delete_line(path, lineno, text)
        fails = failing_expectations()
    if fn in LAYER1_TEST_OF_FUNCTION:
        wanted = LAYER1_TEST_OF_FUNCTION[fn]
        hits = [t for t, _ in fails if t == wanted]
        record(label, bool(hits),
               f"AC1 probe red: {wanted!r}" if hits else
               f"AC1 probe {wanted!r} DID NOT go red"
               f" ({len(fails)} failing expectations)")
    else:
        owner = OWNER_OF_FUNCTION.get(fn, fn) + "/"
        owned = sorted({m.group(0)
                        for _, msg in fails
                        for m in [re.search(re.escape(owner) + r"\S*", msg)]
                        if m})
        record(label, bool(owned),
               f"owned reds: {', '.join(owned) if owned else '(NONE)'}"
               f" ({len(fails)} failing expectations)")

# -- the reader mutation ------------------------------------------------------
HELPER = "tests/testthat/helper-blame-specs-m65.R"
with Restore(HELPER):
    mutate_text(HELPER, "blame_spec_defects_m65 <- function(specs) {",
                "blame_spec_defects_m65 <- function(specs) {\n"
                "  return(character(0))")
    fails = failing_expectations()
reader_red = [
    t for t, _ in fails
    if t == "the M65 completeness reader detects the defects it exists for"]
record("reader neutered (blame_spec_defects_m65 returns nothing)",
       bool(reader_red),
       "planted-defect test red" if reader_red else
       "planted-defect test DID NOT notice")

# -- the control mutations ----------------------------------------------------
GUARD = """  rlang::check_bool(two_pass)
  # The three loudness targets, ABOVE the two_pass block deliberately (M65):"""
GUARD_REPLACEMENT = (
    "  # The three loudness targets, ABOVE the two_pass block deliberately"
    " (M65):")
PRECEDENCE = "data-raw/blame-precedence.R"
CONTROL_OK = """    control_ok <- control$kind == "abort" &&
      grepl(cell$crossed_marker, control$msg)"""

with Restore("R/ffmpeg.R"):
    mutate_text("R/ffmpeg.R", GUARD, GUARD_REPLACEMENT)
    dead = dead_control_ids()
    record("crossed guard removed (normalize_audio check_bool(two_pass))",
           any("two_pass-not-bool" in i for i in dead),
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
