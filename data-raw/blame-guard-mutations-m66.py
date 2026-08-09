#!/usr/bin/env python3
"""M66 AC4 mutation harness -- is every row locator falsifiable?

THE MUTATION LIST IS DERIVED, NOT DECLARED, and it cannot drift out of the
tree: the harness first re-runs data-raw/m66-derive-sites.py --check, which
re-derives AC1's grep + callee closure and fails on ANY difference from the
committed triage (data-raw/m66-site-triage.tsv). The mutable sites are then
the triage's `swept` rows whose token is check_batch_cell -- the wrapper
call sites, each of which PASSES a row index. The mutation rewrites that
site's first argument (the row-index pass: `i`, a conditional, or a
`which(...)[1]`) to the constant `1L`; the owning red is a failing
expectation in test-row-locator-grid.R whose `info` carries a cell id owned
by that site, every cell placing its bad value at a row other than 1.

Ownership maps (function, site-order-within-function) to cell-id patterns.
The SITE SET is derived; only ownership is a map, and a derived site with no
mapping fails the run loudly rather than passing unowned.

Every edited file is restored in a `finally`; `git status` after a run is
the check. Run from the package root:
  python3 data-raw/blame-guard-mutations-m66.py
Exit status 0 iff every mutation produced its owned red.
"""
import re
import shutil
import subprocess
import sys

TRIAGE = "data-raw/m66-site-triage.tsv"

# (function, 0-based order of its wrapper sites by line) -> cell-id regex
OWNER = {
    ("anonymize_video_batch", 0): r"anonymize/regions-structure",
    ("anonymize_video_batch", 1): r"anonymize/region-values",
    ("check_batch_codec_col", 0): r"codec-col/",
    ("check_batch_vocab_col", 0): r"compare/direction|pip/position",
    ("compare_videos_batch", 0): r"compare/needs-audio",
    ("compare_videos_batch", 1): r"compare/resize-three-inputs",
    ("compare_videos_batch", 2): r"compare/audio-bound",
    ("crop_video_batch", 0): r"crop/width/row3",
    ("crop_video_batch", 1): r"crop/x",
    ("extract_frame_batch", 0): r"extract_frame/timestamp-finite",
    ("extract_frame_batch", 1): r"extract_frame/frame-whole",
    # No index-2 two-pass-token entry: that wrapper is triaged
    # excluded-backstop (shadowed by check_batch_codec_col's earlier loop),
    # so it is not in the swept set this map serves.
    ("normalize_audio_batch", 0): r"normalize/copy-column",
    ("normalize_audio_batch", 1): r"normalize/target_loudness",
    ("normalize_audio_batch", 2): r"normalize/channels|normalize/sample_rate",
    ("picture_in_picture_batch", 0): r"pip/needs-audio",
    ("picture_in_picture_batch", 1): r"pip/margin",
    ("picture_in_picture_batch", 2): r"pip/audio",
    ("picture_in_picture_batch", 3): r"pip/scale",
    ("sample_frames_batch", 0): r"sample/fps|sample/interval",
    ("segment_video_batch", 0): r"segment/reencode-video",
    ("segment_video_batch", 1): r"segment/reencode-audio",
    ("separate_audio_video_batch", 0): r"separate/hardware-reshape",
    ("standardize_video_batch", 0): r"standardize/fps",
    ("standardize_video_batch", 1): r"standardize/pixel_format",
}


def sh(args):
    return subprocess.run(args, capture_output=True, text=True)


def derived_wrapper_sites():
    """[(function, order, file, line)] for swept check_batch_cell rows,
    after the anti-drift check has passed."""
    check = sh(["python3", "data-raw/m66-derive-sites.py", "--check"])
    if check.returncode != 0:
        print("triage out of sync with the tree -- refusing to mutate:")
        print(check.stdout or check.stderr)
        sys.exit(1)
    rows = []
    for line in open(TRIAGE).read().splitlines()[1:]:
        f = line.split("\t")
        if f[2] == "check_batch_cell" and f[6] == "swept":
            path, lineno = f[3].split(":")
            rows.append((f[0], "R/" + path if not path.startswith("R/") else path,
                         int(lineno)))
    rows.sort(key=lambda r: (r[0], r[2]))
    out = []
    order = {}
    for fun, path, lineno in rows:
        k = order.get(fun, 0)
        order[fun] = k + 1
        out.append((fun, k, path, lineno))
    return out


def mutate_row_pass(path, lineno):
    """Rewrite the first argument of the check_batch_cell( call that starts
    on `lineno` to `1L`, tracking parens and quotes across lines."""
    src = open(path).read()
    lines = src.splitlines(keepends=True)
    offset = sum(len(l) for l in lines[: lineno - 1])
    # Bounded to the recorded line: an unbounded search from a stale offset
    # would silently lock onto the NEXT wrapper after unrelated edits shift
    # line numbers, mutating the wrong site (M66 review F9).
    m = re.search(r"check_batch_cell\(", src[offset: offset + len(lines[lineno - 1])])
    assert m, (f"no check_batch_cell( on {path}:{lineno} -- line numbers "
               "drifted; regenerate the triage")
    start = offset + m.end()
    depth, i, quote = 1, start, None
    while i < len(src):
        ch = src[i]
        if quote:
            if ch == "\\":
                i += 2
                continue
            if ch == quote:
                quote = None
        elif ch in "\"'":
            quote = ch
        elif ch in "([{":
            depth += 1
        elif ch in ")]}":
            depth -= 1
        elif ch == "," and depth == 1:
            break
        i += 1
    assert i < len(src) and src[i] == ",", f"no first-arg comma at {path}:{lineno}"
    open(path, "w").write(src[:start] + "1L" + src[i:])


def failing_cells():
    """cell ids (info strings) carried by failing grid expectations."""
    code = (
        "suppressMessages(devtools::load_all(quiet=TRUE)); "
        "r <- testthat::test_local(filter='row-locator-grid', "
        "reporter='silent', stop_on_failure=FALSE); "
        "for (t in r) for (e in t$results) "
        "if (inherits(e, c('expectation_failure', 'expectation_error'))) "
        "cat('<<<MSG>>>', conditionMessage(e), sep='')")
    out = sh(["Rscript", "-e", code]).stdout
    return [chunk for chunk in out.split("<<<MSG>>>")[1:]]


class Restore:
    def __init__(self, *paths):
        self.pairs = [(p, p + ".mutbak") for p in paths]

    def __enter__(self):
        for path, bak in self.pairs:
            shutil.copy(path, bak)

    def __exit__(self, *exc):
        for path, bak in self.pairs:
            shutil.move(bak, path)


def main():
    baseline = failing_cells()
    if baseline:
        print("baseline is not green; refusing to attribute reds:")
        for msg in baseline:
            print("  " + msg.splitlines()[0])
        sys.exit(1)
    print("baseline green", flush=True)

    sites = derived_wrapper_sites()
    print(f"{len(sites)} wrapper sites derived (triage in sync)", flush=True)
    failures = []
    for fun, k, path, lineno in sites:
        label = f"{path}:{lineno} [{fun} #{k}]"
        owner = OWNER.get((fun, k))
        if owner is None:
            print(f"FAIL  {label}: no ownership mapping -- add it to OWNER")
            failures.append(label)
            continue
        with Restore(path):
            mutate_row_pass(path, lineno)
            fails = failing_cells()
        owned = sorted({m.group(0) for msg in fails
                        for m in [re.search(owner, msg)] if m})
        ok = bool(owned)
        print(f"{'RED ' if ok else 'FAIL'}  {label}: "
              f"{', '.join(owned) if owned else '(no owned red)'} "
              f"({len(fails)} failing expectations)", flush=True)
        if not ok:
            failures.append(label)

    print(f"\n{len(sites) - len(failures)}/{len(sites)} mutations owned")
    sys.exit(1 if failures else 0)


if __name__ == "__main__":
    main()
