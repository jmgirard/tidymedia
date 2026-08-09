#!/usr/bin/env python3
"""M66 site derivation — AC1's recorded grep + callee closure.

Enumerates every abort site reachable from each `_batch` verb's front door
before `ffm_batch()`, so the committed triage (data-raw/m66-site-triage.tsv)
is re-derivable rather than hand-kept. The mutation harness re-runs this at
mutation time and fails on any difference from the committed file (AC4), so
the triage cannot drift out of the tree the way a static list would.

Procedure (AC1, verbatim from the milestone file):
- the 15 `_batch` task verbs are read from NAMESPACE (`_batch` exports minus
  `ffm_batch`, the Layer-1 runner);
- each verb's FRONT DOOR is its body from the definition line to the first
  line calling `ffm_batch(` (exclusive) — text at or after that line,
  including an inline fan-out closure, is pipeline territory and out;
- the CALLEE CLOSURE expands called identifiers to fixpoint over the
  package's own functions, terminating at `ffm_batch()` and at exported
  `ffm_*` builders (Layer-1 blame by D042, excluded by rule);
- the recorded ABORT PATTERN is grepped over the front-door bodies and the
  closure functions' whole bodies.

Output: TSV rows keyed (function, occurrence-index, token) — stable across
line drift — with file:line informational. Keys join m66-site-triage.tsv,
which adds the disposition (swept / already-located / excluded) + reason.

Usage:
  python3 data-raw/m66-derive-sites.py            # print derived TSV
  python3 data-raw/m66-derive-sites.py --check    # compare against committed
                                                  # triage keys; exit 1 on any
                                                  # difference, listing it
"""

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
R_DIR = ROOT / "R"
TRIAGE = ROOT / "data-raw" / "m66-site-triage.tsv"

ABORT_PATTERN = re.compile(
    r"cli_abort\(|abort\(|stop\(|stopifnot\(|match\.arg\(|"
    r"check_[a-z_]+\(|arg_match|check_number_"
)
DEF_RE = re.compile(r"^([A-Za-z_.][A-Za-z0-9_.]*)\s*<-\s*function")
CALL_RE = re.compile(r"\b([A-Za-z_.][A-Za-z0-9_.]*)\(")


def namespace_exports():
    out = []
    for line in (ROOT / "NAMESPACE").read_text().splitlines():
        m = re.match(r"export\(([^)]+)\)", line)
        if m:
            out.append(m.group(1))
    return out


def parse_functions():
    """{name: (file, def_line_1idx, [body lines incl. def line])}"""
    funs = {}
    for path in sorted(R_DIR.glob("*.R")):
        lines = path.read_text().splitlines()
        defs = [(i, DEF_RE.match(l).group(1))
                for i, l in enumerate(lines) if DEF_RE.match(l)]
        for k, (start, name) in enumerate(defs):
            end = defs[k + 1][0] if k + 1 < len(defs) else len(lines)
            funs[name] = (path.name, start + 1, lines[start:end])
    return funs


def front_door(body):
    """Body lines strictly before the first `ffm_batch(` call."""
    for i, line in enumerate(body):
        if "ffm_batch(" in strip_code(line):
            return body[:i]
    return body


def strip_code(line):
    """Blank out string literals and comments so neither the closure walk nor
    the abort grep matches a function named in a message or a comment."""
    out, quote, i = [], None, 0
    while i < len(line):
        ch = line[i]
        if quote:
            if ch == "\\":
                out.append("  ")
                i += 2
                continue
            if ch == quote:
                quote = None
                out.append(ch)
            else:
                out.append(" ")
        elif ch in "\"'":
            quote = ch
            out.append(ch)
        elif ch == "#":
            break
        else:
            out.append(ch)
        i += 1
    return "".join(out)


def called_names(lines):
    names = set()
    for line in lines:
        for m in CALL_RE.finditer(strip_code(line)):
            names.add(m.group(1))
    return names


def closure(seed_lines, funs, terminators):
    """Fixpoint expansion of package callees, never expanding terminators."""
    seen, frontier = set(), called_names(seed_lines)
    while frontier:
        name = frontier.pop()
        if name in seen or name in terminators or name not in funs:
            continue
        seen.add(name)
        frontier |= called_names(funs[name][2])
    return seen


def derive():
    exports = namespace_exports()
    verbs = sorted(n for n in exports if n.endswith("_batch") and n != "ffm_batch")
    terminators = {"ffm_batch"} | {n for n in exports if n.startswith("ffm_")}
    funs = parse_functions()

    # region name -> (file, first line 1idx, lines, verbs reaching it)
    regions = {}
    for verb in verbs:
        fname, defline, body = funs[verb]
        fd = front_door(body)
        regions.setdefault(verb, (fname, defline, fd, set()))[3].add(verb)
        for callee in closure(fd, funs, terminators):
            cf, cl, cb = funs[callee]
            regions.setdefault(callee, (cf, cl, cb, set()))[3].add(verb)

    rows = []
    for name in sorted(regions):
        fname, defline, lines, via = regions[name]
        occ = 0
        for off, line in enumerate(lines):
            for m in ABORT_PATTERN.finditer(strip_code(line)):
                occ += 1
                rows.append((
                    name, occ, m.group(0).rstrip("("),
                    f"{fname}:{defline + off}",
                    ",".join(sorted(via)),
                    line.strip()[:80],
                ))
    return rows


def main():
    rows = derive()
    if "--check" in sys.argv:
        if not TRIAGE.exists():
            sys.exit(f"missing {TRIAGE}")
        committed = set()
        for line in TRIAGE.read_text().splitlines()[1:]:
            if line.strip():
                f = line.split("\t")
                committed.add((f[0], int(f[1]), f[2]))
        derived = {(r[0], r[1], r[2]) for r in rows}
        extra = sorted(derived - committed)
        gone = sorted(committed - derived)
        for key in extra:
            print(f"DERIVED-NOT-TRIAGED\t{key}")
        for key in gone:
            print(f"TRIAGED-NOT-DERIVED\t{key}")
        if extra or gone:
            sys.exit(1)
        print(f"triage in sync: {len(derived)} sites")
    else:
        print("function\tocc\ttoken\tsite\tvia\tcode")
        for r in rows:
            print("\t".join(str(x) for x in r))


if __name__ == "__main__":
    main()
