# M083: The ROADMAP gets back under its byte budget

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m083-roadmap-byte-budget` — https://github.com/jmgirard/tidymedia/pull/87

## Goal

Bring `cairn/ROADMAP.md` under its 24,000-byte budget by entombing the
instrument-findings rows, pruning the already-promoted ones, and compressing
what stays.

## Scope

Surface tier: **internal** — the deliverable is the repo's own `cairn/`
tracking records, which no external consumer of the R package reads.

**In:** `cairn/ROADMAP.md` is 41,509 bytes against a 24,000 budget (50 lines,
under the 60-line cap), and 39,760 of those bytes are its 32 candidate rows.
Apply the tracking-rules remedy in its stated order. (a) Move the five
instrument-findings rows (13,303 bytes) into a new
`cairn/references/instrument-findings.md`, leaving one grouped row pointing at
it. (b) Prune the three struck-through rows already fully promoted,
first folding the retired M44 row's `(a)`/`(b)` promotion conditions into the
M082-leftovers row, which cites them by reference rather than restating them. (c) Compress the widest surviving live rows to a
hook, a promote-on clause and a links trailer, moving measurement narrative to
the D-entry or milestone file each already cites. Refresh the hygiene stamp.

**Out:** `cairn/LESSONS.md` → M084. Any byte-budget checker: budgets are
judgment-checked at hygiene passes by rule, and adding one would be the
checker-regress shape. Retiring any live candidate on its merits — this
milestone changes how rows are stored and worded, never whether an idea stands.

## Acceptance criteria

- [x] AC1 On the merge commit, `wc -c cairn/ROADMAP.md` reports fewer than
      24,000 bytes and `wc -l` fewer than 60 lines.
- [x] AC2 Let the branch-point rows be what `awk '/^## Candidates/,0'
      cairn/ROADMAP.md | grep '^- '` enumerates at M083's branch point
      (`git merge-base <branch> master`), the struck set what the same pipe
      with `grep '^- ~~'` enumerates there, and the instrument set what the
      same pipe with `grep -i 'instrument'` enumerates there; those two sets
      are disjoint. After the merge: no line of `cairn/ROADMAP.md` or of
      `cairn/references/instrument-findings.md` is byte-identical to any
      struck-set row; every instrument-set row appears as a byte-identical
      line in `cairn/references/instrument-findings.md`, and no line of
      `cairn/ROADMAP.md` is byte-identical to it; and the first pipe
      enumerates exactly (branch-point rows) − (struck set) − (instrument
      set) + 1 lines, the one being the grouped row that replaces the
      entombed set. This count is a count only: T4 rewrites surviving rows,
      so no claim is made here that any individual surviving row is still
      present in its branch-point form (AC4 governs what the surviving rows
      must retain).
- [x] AC3 `cairn/references/instrument-findings.md` exists and is listed in
      `cairn/references/INDEX.md`, and each row the same pipe with
      `grep -i 'instrument'` enumerates at M083's branch point is present in it
      carrying that row's finding ids and its promote-on clause, and absent
      from `cairn/ROADMAP.md`.
- [x] AC4 Each row AC2 finds present in `cairn/ROADMAP.md` after the merge
      retains its `— added` trailer, and retains a promote-on clause wherever
      its branch-point text matched `grep 'Promote '`.
- [x] AC5 `python3 ~/.claude/skills/cairn/scripts/cairn_validate.py` reports
      all 16 checks PASS and all 7 advisories OK.

## Coverage

- AC1 → T2, T3, T4, T5
- AC2 → T2, T3, T4
- AC3 → T2
- AC4 → T4
- AC5 → T5

## Tasks

- [x] T1 Record the branch-point baseline: run AC2–AC4's four enumerations,
      save each row's bytes, its `— added` trailer and its `Promote ` match to
      the branch as the comparison surface AC2–AC4 read, and record the counts.
      (At bd4d545 these were 32 / 3 / 5 / 21; this plan's own commit adds one
      candidate row, so the baseline is re-measured, never carried over.)
- [x] T2 Create `cairn/references/instrument-findings.md` holding AC3's five
      rows verbatim under per-row headings; add its `INDEX.md` line
      under "Working artifacts"; replace the five rows with one grouped
      candidate row pointing at the page.
- [x] T3 Fold the retired M44 row's `(a)`/`(b)` promotion conditions into the
      M082-leftovers row (which says "read them there rather than here"), then
      delete AC2's three struck-through rows. Check the D055-gaps row's stated
      dependence on the retired floor-harness row before deleting that one.
- [x] T4 Compress the surviving rows over 1,000 bytes at the branch point
      (eleven at 8021df1) to hook + promote-on + trailer, each keeping the citations its
      narrative is moving to. Before dropping a sentence, confirm the entry or
      milestone file it cites actually holds it; anything with no home stays.
      Re-measure after each; stop when AC1 clears with headroom.
- [x] T5 Refresh the `Last hygiene check` stamp with the new byte figures; run
      `cairn_validate.py`.

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in REDUCED mode (internal tier, no RB-tripwire tags). Round 1 returned five findings on this milestone's criteria: AC3's "the five instrument-findings rows" was a recalled count (the case-sensitive grep returns four; `tolower` returns five), AC5 named a `cairn_validate.py` path with no target, and AC2/AC4 routed their domain through a work-log disposition ledger (recording instrument, D-120). Round 2 on the revised wording passed AC1/AC2/AC3/AC5 and returned one finding: AC4's `-- added` literal matches nothing (the file uses an em dash) and its promote-on sub-domain named no procedure. All fixed before writing.
- 2026-08-28: plan gate chose entombing the instrument rows into `cairn/references/` over pruning them outright, because a pruned finding surfaces at no later plan gate; falsified by a hygiene pass finding the page itself over budget with nothing having consulted it.
- 2026-08-28: T1 branch-point baseline at `8021df1`: `cairn/ROADMAP.md` 42,552 bytes / 53 lines; 33 candidate rows, 3 struck, 5 matching `instrument` (13,303 bytes), 22 carrying `Promote `. Frozen in `cairn/references/roadmap-candidates-baseline-M083.md` as a convenience record; the criteria read git and the post-merge files, not that page.
- 2026-08-28: minor amendment to T4 — the branch point has eleven surviving rows over 1,000 bytes, not the nine the plan recorded at `bd4d545`; T4's parenthetical corrected and its citation-check step (the implement gate's choice) written in.
- 2026-08-28: implement gate chose one shared promote-on clause for the grouped instrument row over restating all five (the five stay verbatim on the entombed page), checking each row's citations before dropping narrative over trusting them, and compressing all eleven wide rows rather than stopping at the first pass under 24,000.
- 2026-08-28: T2 — the five instrument rows moved verbatim into `cairn/references/instrument-findings.md` (15,878 bytes, indexed), replaced in `cairn/ROADMAP.md` by one grouped row with a shared promote-on clause; verified each of the five is byte-identical on the page and absent from the ROADMAP. ROADMAP 42,552 → 30,013 bytes, 53 → 49 lines, 33 → 29 candidate rows.
- 2026-08-28: T3 — the retired M44 row's (a)/(b) promotion conditions folded verbatim into the M082-leftovers row, then the three struck-through rows deleted. Checked the D055-gaps row first: it states its own gaps in full and says outright that retiring the carry row does not lose them, and the carry row agreed, so nothing was lost. ROADMAP 30,013 → 27,140 bytes, 49 → 46 lines, 29 → 26 candidate rows.
- 2026-08-28: T4 — an [S] subagent checked all eleven wide rows against the entries, archives, reviews and source lines each cites (row 17 it skipped; checked inline); every fact it could not find a home for stayed in the row, which is why the rows land near 1,000 bytes rather than the ~700 a pure hook-plus-trigger rewrite would give. Orphans kept: the container reproduce recipe and the macOS pass time; the two M077 plan-gate declines; the 42.0-42.1 s CI figure, the 10 s→60 s bound and the `?system` quote; the `multicore` copy-on-write parenthetical; the 53/32/22 domain counts; M67's rejection reasoning; `format_for_web()`'s exit 234; the "roughly doubling" speculative magnitude; and four review-finding scores. Eleven rows 16,706 → 11,124 bytes; ROADMAP 27,140 → 22,212 bytes, 46 lines. All 26 rows keep an `— added` trailer; the 17 surviving branch-point rows that carried `Promote ` still carry it.
- 2026-08-28: T5 — hygiene stamp replaced with M083's own figures (`cairn/ROADMAP.md` 22,102 bytes over 46 lines, from 42,552 over 53 at the branch point; `cairn/LESSONS.md` unchanged at 42,232 against its 20,000 cap); the stamp's self-referential byte figure was settled by iterating the rewrite until it matched `wc -c`. `cairn_validate.py`: 16 checks PASS, 7 advisories OK.
- 2026-08-28: AC2 reading surfaced before review. A byte-for-byte comparison against the branch point puts the eleven T4-compressed rows and the folded M082-leftovers row in AC2's "absent from both" state, which T4 exists to produce and the Coverage map (AC2 → T4) already anticipates; AC2's three states are about where each row's idea ended up, not about its bytes. Verified under that reading: 25 branch-point rows carried in `cairn/ROADMAP.md` (plus one new grouped row, 26 total), 5 on the entombed page byte-identical, 3 absent — and the 3 are exactly the struck-through set. Put to the user as a mini amendment gate rather than left for review to interpret.
- 2026-08-28: substantive amendment to AC2, accepted at a mini gate. The original wording put each branch-point row in one of three states — present in the ROADMAP, present on the entombed page, or absent from both — without fixing what "present" meant; compared byte-for-byte the eleven rows T4 rewrites read as absent, which is the opposite of what happens to them. Amended so the criterion claims only what a procedure decides.
- 2026-08-28: criteria audit on the amended AC2 ran in REDUCED mode (internal tier, no RB-tripwire tags), two fresh-context [O] readers, neither the author of what it read. Reader 1 returned one bounded-promise finding: "a row there proposes the same idea, however reworded" substituted a reader's judgment for a procedure, and the repair was to keep only the states a procedure decides plus a count identity. Reader 2, on that repaired text, returned two more: the count cannot carry a "no other row is dropped" promise (a deletion plus an unrelated addition satisfies the arithmetic), and "present"/"absent" were undecidable between a line and a substring reading — under the substring reading the criterion is false on T3's own fold, since the folded conditions still match `cairn/ROADMAP.md`. Reader 2's wording adopted: the unit is a byte-identical line, and the count is stated as a count only.
- 2026-08-28: all five tasks done; status → review. `cairn/ROADMAP.md` 22,097 bytes over 46 lines (from 42,552 over 53 at the branch point), 26 candidate rows. `cairn_validate.py`: 16 checks PASS, 7 advisories OK. No R sources changed, so the profile's verify slot has nothing to run.
- 2026-08-28: plan gate chose compressing live rows in place over deferring the whole cut to entombment and pruning, because the arithmetic does not reach 24,000 without it (41,509 less 13,303 less 3,429 leaves ~24,800, before this plan's own additions); falsified by a measured pass where entombment alone clears the budget with headroom.

## Review

PR: https://github.com/jmgirard/tidymedia/pull/87 (docs-only diff; internal
tier). Branch point `8021df1`; `master` was level with `origin/master` at
review time, so no merge-forward was needed. Driving RR: none, so no
projection-vs-outcome pairs apply.

### Acceptance-criteria evidence (2026-08-28, branch tip)

- **AC1 — pass.** `wc -c cairn/ROADMAP.md` = 22,097 bytes (< 24,000);
  `wc -l` = 46 lines (< 60). Measured on the branch tip, whose tree the
  squash-merge reproduces byte for byte.
- **AC2 — pass.** At `git merge-base m083-roadmap-byte-budget master`
  (`8021df1`) the three enumerations return 33 rows, 3 struck, 5 matching
  `instrument`; `comm -12` over the sorted struck and instrument sets returns
  0 lines, so they are disjoint. After the change: no struck-set row is
  byte-identical to any line of `cairn/ROADMAP.md` or of
  `cairn/references/instrument-findings.md` (`grep -Fxq` per row, 0 hits);
  each of the 5 instrument-set rows is a byte-identical line of
  `cairn/references/instrument-findings.md` and matches no line of
  `cairn/ROADMAP.md`. The post pipe enumerates 26 rows against the required
  33 − 3 − 5 + 1 = 26.
- **AC3 — pass.** `cairn/references/instrument-findings.md` exists (15,885
  bytes) and is listed at `cairn/references/INDEX.md:12` under Working
  artifacts. Each of the 5 branch-point instrument rows is present on the page
  as a byte-identical line — so each carries its own finding ids and its
  promote-on clause unchanged (all 5 matched `Promote ` at the branch point) —
  and none matches a line of `cairn/ROADMAP.md`.
- **AC4 — pass.** All 26 post rows carry a `— added` trailer (`grep -v
  '— added'` over the enumerated rows returns nothing). Of the 25 branch-point
  survivors, 17 matched `Promote `; removing the one new grouped instrument
  row from the 26 post rows leaves 25 that align in file order with the 25
  survivors, and the per-row present/absent pattern of `Promote ` is identical
  position by position. 18 post rows carry a promote-on clause: the 17
  survivors plus the grouped row.
- **AC5 — pass.** `python3 ~/.claude/skills/cairn/scripts/cairn_validate.py`
  exits 0 with 16 PASS and 7 OK.

### Consistency gate

Universal: `cairn_validate.py` as above, all 16 checks PASS (its `coverage
complete` and `scaffold present` checks among them) and all 7 advisories OK.
No `DESIGN.md` principle changed, so `cairn_impact.py` does not apply.

Toolchain (`r-package` profile `consistency-gate` slot):
`devtools::document()` exits 0 and leaves no diff in `NAMESPACE`, `man/` or
`data/`; `pkgdown::check_pkgdown()` reports no problems; `README.md` and
`README.Rmd` are unchanged by this branch and in sync; the diff makes no
user-visible change, so `NEWS.md` needs no entry; the two new files are under
`cairn/`, already covered by the `^cairn$` `.Rbuildignore` entry, so no new
top-level file was added; `devtools::check()` — see below.
`devtools::check()`: 0 errors, 0 warnings, 0 notes (7m23s, full suite).

### Independent review

Routing: internal tier, `git diff origin/master...HEAD --name-only` shows only
markdown under `cairn/`, so one fresh-context [O] diff-bug reviewer was spawned
and the other two lenses skipped. It re-derived all five criteria independently
and reached the same verdicts, and added that exactly 13 branch-point rows
survive byte-identical, so the 11 compressed rows plus the 1 folded row are the
only surviving rows touched.

Return floor: no finding demonstrates an acceptance criterion failing, and none
is a defect in what the R package does for its users, so none returns the
milestone. All twelve are triaged below; the ranking is the reviewer's.

1. **Fix now.** The pruned struck M080 row carried the reasoning for withdrawing
   its F10 — that `rlang::caller_arg()` resolves at the guard's own frame, so
   `check_bool(reencode, call = call)` already aborts naming `reencode` — and no
   `cairn/` file holds it (`grep -rn caller_arg cairn/` returns nothing).
   T3 checked only the D055-gaps row's dependence before deleting, so the struck
   rows' bodies went unchecked. Carried into M083's archive summary.
2. **Fix now.** `cairn/references/roadmap-candidates-baseline-M083.md` is neither
   of the two page types `references/` owns; its own header calls it a
   convenience record and disclaims being a verification surface, and it is
   re-derivable from `git show 8021df1:cairn/ROADMAP.md`. Deleted with its
   `INDEX.md` line.
3. **Fix now.** On `cairn/references/instrument-findings.md` the M62/M63 row
   says M080's findings "left it for the candidate row below"; that row is the
   struck M080 row T3 deleted, so the reference now points at nothing. Repaired
   by a note under the row's heading rather than in the row line, which AC3
   requires to stay byte-identical.
4. **Fix now.** The `(corrected M078: …)` marker was dropped from the
   `with_timeout()` row while its "minutes, not seconds" hook stayed, so the
   ROADMAP now reads as an uncorrected claim. Marker restored.
5. **Fix now.** `R/ffmpeg.R:882-891` is that verb's front-door `check_*` block;
   the audio-then-video sequencing the row claims is at `R/ffmpeg.R:911-912`.
   Loose at the branch point, precise-looking and wrong after the rewrite.
   Citation corrected.
6. **Fix now.** The `tm_timeout_domain()` row's `53 / 32 / 22` does not add up.
   Measured by running the helper: the domain is 53, of which 31 take `run =`
   and 22 do not. Pre-existing at the branch point, one character to fix.
   Corrected to 31.
7. **Reject.** The Scope says the tracking-rules remedy was applied "in its
   stated order" when M083 ran entomb → prune → compress rather than
   compress-first. Real, but Scope is plan-owned text in a file the archive
   replaces at merge; the archive summary states the order actually run.
8. **Reject.** T1's "the comparison surface AC2–AC4 read" contradicts the
   settled position that the criteria read git; superseded by the T1 work-log
   line, and the same transient-text reason as 7.
9. **Reject.** The Scope's opening figures (41,509 / 50 / 32) are plan-time and
   were re-measured by T1 to 42,552 / 53 / 33, which T1's parenthetical says
   outright. Same reason as 7.
10. **Reject.** The T2 work-log line's 15,878 bytes was accurate when written; a
    later T2 commit grew the page to 15,885. Work logs are append-only history,
    never edited; the archive summary carries the shipped figure.
11. **Reject.** The unticked criterion boxes in `HEAD` are this review session's
    own in-flight work, landing in its checkpoint commit.
12. **Reject.** Double blank lines between rows on the entombed page — a style
    nitpick.
