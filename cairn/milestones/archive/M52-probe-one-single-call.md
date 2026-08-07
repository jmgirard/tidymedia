# M52: Collapse `probe_one()`'s per-stream FFprobe loop into one call

**Status:** done (2026-08-06, PR #55 https://github.com/jmgirard/tidymedia/pull/55)

**Goal:** Cut `probe_one()` from N+1 FFprobe spawns per file to one, by reading the container
and every stream from a single compact-format call.

**Outcome:** `probe_one()` issues one call pinning `-of compact=print_section=1:nokey=0:escape=c`,
parsed by the new `parse_compact_probe()` family beside it; `format_probe()` deleted. Parsing is
byte-based, so a line invalid in the session locale keeps its row, and `compact_key_name()`
uppercases `tag:`/`disposition:` while stripping the side-data prefix in all its FFprobe spellings,
which keeps `rotation` a bare column. Multi-line values — newline tags, the display matrix — reach
one cell instead of spawning invented columns. `data-raw/probe-baseline.R` records six fixtures
against the pre-change ref, so the parity tests need no binary. Five processes to one on a
five-stream file; 1.7s to 0.46s over ten local probes.

**Decisions:** none milestone-local. Two gated amendments: AC2's exemption widened to any fixture
whose pre-change output is itself corrupt, and the Scope clause naming both nested-key repairs
rather than casing alone.

**Review:** Round 1 returned it — F1 (96), the side-data rename dropping `rotation`; F2 (88) and
F4 (85) fixed with it. Round 2 passed: 16 findings, none at or above 80, highest 55. F1 (35), a
version-pinned prefix pattern, hardened at the merge gate on the user's instruction.
