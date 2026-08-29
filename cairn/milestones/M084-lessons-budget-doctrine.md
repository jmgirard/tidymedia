# M084: LESSONS gets back under its budget, and its biggest family graduates

- **Status:** review
- **Priority:** normal
- **Depends on:** M083
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m084-lessons-budget-doctrine` — https://github.com/jmgirard/tidymedia/pull/88

## Goal

Bring `cairn/LESSONS.md` under its 20,000-byte budget by graduating the
false-green family whole into a doctrine module and trimming or retiring the
rest.

## Scope

Surface tier: **internal** — the deliverable is the repo's own `cairn/`
tracking records, which no external consumer of the R package reads.

**In:** `cairn/LESSONS.md` is 42,232 bytes against a 20,000 budget and 49 lines
against a 50-line cap, so one more lesson makes it line-illegal too. Its 44 entries at bd4d545 are dominated by one family — "a test or control that reads green for
the wrong reason" — running through roughly fifteen entries and ~20,100 bytes,
already consolidated across M074/M076/M077/M079/M081 inside a single 4,851-byte
line. That family meets the maturation exit (transferable craft, extended well
more than twice, neither other exit applying), so it graduates whole into
`cairn/references/false-greens.md`, whose header states its own line and byte
budget set from the graduated size plus headroom. Classify every remaining entry
against the three exits — enforcement, ownership, maturation — trimming a
partly-covered lesson to its remainder and leaving no line behind for a retired
one. Refresh the hygiene stamp.

**Out:** `cairn/ROADMAP.md` → M083. A second doctrine module for the
front-door-ordering family (lines 21, 33, 41) → candidate row; it has been
extended twice at most, which is the edge of the maturation bar. Any lesson
merely disputed rather than redundant, which is corrected in place, never
retired.

## Acceptance criteria

- [x] AC1 On the merge commit, `wc -c cairn/LESSONS.md` reports fewer than
      20,000 bytes and `wc -l` fewer than 50 lines.
- [x] AC2 Each entry `grep '^- ' cairn/LESSONS.md` enumerates at M084's branch
      point (`git merge-base <branch> master`) is, after the merge, in exactly
      one observable state: present in `cairn/LESSONS.md` whole or trimmed, present in
      `cairn/references/false-greens.md`, or absent from both.
- [x] AC3 `cairn/references/false-greens.md` exists, is listed in
      `cairn/references/INDEX.md`, states a line budget and a byte budget in
      its own header, and `wc -l -c` reports it inside both figures.
- [x] AC4 No entry AC2 finds in `cairn/references/false-greens.md` is also in
      `cairn/LESSONS.md`: for each, `grep -F` of the first 120 bytes of its
      branch-point text (`cut -c1-120`) over `cairn/LESSONS.md` returns no match;
      T1 records that the key is unique across the enumeration.

## Coverage

- AC1 → T2, T3, T4
- AC2 → T1, T2, T3
- AC3 → T2
- AC4 → T1, T2

## Tasks

- [x] T1 Record the branch-point baseline: `grep '^- ' cairn/LESSONS.md` with
      each entry's bytes and its `cut -c1-120` key, saved to the branch as the
      comparison surface AC2 and AC4 read, and assert the key is unique across
      the enumeration (it is across the 44 entries at bd4d545). Classify every
      entry against the three exits and write the classification into the
      milestone's Decisions section.
- [x] T2 Author `cairn/references/false-greens.md`: the graduated family under
      themed headings, a header stating its line and byte budget set from the
      graduated size plus stated headroom, and an `INDEX.md` line. Delete every
      graduated entry from `LESSONS.md`, leaving no line behind.
- [x] T3 Apply the remaining exits: trim each partly-covered lesson to its
      remainder; retire under enforcement or ownership only where the exit's
      test or owning slot is named in T1's classification. Re-measure; stop when
      AC1 clears with headroom.
- [x] T4 Refresh the `Last hygiene check` stamp with the new byte and line
      figures for both files and the module's own budget.

## Decisions

- 2026-08-28 (T1): the 44 entries at the branch point (`31a8e4f`) classify as 17 `graduate`, 6 `trim`, 21 `keep`. The 17 graduating entries are one family — a test or control that reads green for the wrong reason: a fixture or grid that measures nothing, a skip or gate that never runs, a mock or control the call site swallows, a coverage or non-vacuity floor blind to the loss it exists to catch. They hold 21,171 bytes of entry text and were extended or consolidated across M52, M31/M63/M68, M35, M39, M41/M61/M62/M64, M42, M43, M44/M63/M67, M47, M50, M51/M59, M54/M58, M70, M071, M072/M073/M078 and M074/M076/M077/M079/M081, well past the maturation exit's twice; neither other exit applies, since no test fails on the mistake and no other tracking file's slot owns test-discrimination craft. The 6 `trim` entries each carry one half now covered elsewhere: the FFmpeg-version entry's `.aac`-muxer clause, the error-precedence entry's otherwise-valid-grid clause and the two-pass entry's half-domain clause belong to the graduating family; the front-door-ordering entry's front-door-guard half is enforced by `tests/testthat/test-builder-blame-front-door.R`, which reddens on the verb whose guard is removed; the line-endings entry's blame-config half is owned by `CLAUDE.md`'s development conventions; the timeout-wait entry's limit + 40 s measurement is owned by D056. The remaining 21 keep FFmpeg, R and package-shape facts no exit reaches. Per-entry rows with byte lengths and keys: `cairn/references/lessons-baseline-M084.md`.

## Review

_PR: https://github.com/jmgirard/tidymedia/pull/88 — reviewed 2026-08-28 on `m084-lessons-budget-doctrine` at branch point `31a8e4f`._

### Acceptance criteria — fresh evidence

- **AC1 — pass.** `wc -c cairn/LESSONS.md` reports 19,860 bytes and `wc -l` reports
  33 lines, under the 20,000-byte budget and well inside the 50-line cap. Measured
  on the branch head after the gate-directed fixes, the tree the squash-merge
  commit carries — those fixes restored 488 bytes of trimmed content, so the room
  is 140 bytes, and that is what the F4/F5 candidate row records. Pre-fix the file
  was 19,372 over 32 lines.
- **AC2 — pass.** `git show 31a8e4f:cairn/LESSONS.md | grep '^- '` enumerates 44
  entries; `cut -c1-120` over them yields 44 keys with `sort | uniq -d` empty.
  Testing each key with `grep -F` against both files partitions the enumeration
  27 present in `cairn/LESSONS.md` / 17 present in `cairn/references/false-greens.md`
  / 0 in both / 0 in neither — every entry in exactly one observable state.
- **AC3 — pass.** `cairn/references/false-greens.md` exists; `cairn/references/INDEX.md:14`
  lists it; its header states "Budget: fewer than 26,000 bytes and fewer than 60
  lines"; `wc -l -c` reports 53 lines / 24,959 bytes, inside both figures (52 /
  24,846 before the F11 header correction).
- **AC4 — pass.** The same partition run records 0 entries present in both files:
  for each of the 17 keys AC2 finds in the module, `grep -F` over
  `cairn/LESSONS.md` returns no match. Key uniqueness across the 44-entry
  enumeration is recorded by T1 in `cairn/references/lessons-baseline-M084.md` and
  re-verified here (`sort | uniq -d` returns nothing).

_No `Driving RR:` on this milestone, so the projection-vs-outcome record no-ops._

### Consistency gate

- `cairn_validate.py` — exit 0; all 16 checks PASS, all 7 advisories OK
  (`release window` OK, so step 10's displacement clause does not fire).
- `cairn_impact.py` — skipped; the diff touches no `cairn/DESIGN.md` principle
  (`git diff master..HEAD -- cairn/DESIGN.md` is empty).
- Toolchain checks (`r-package` profile `consistency-gate` slot): the diff is six
  `cairn/` markdown files and touches no `R/`, `man/`, `NAMESPACE`, `README*`,
  `NEWS.md`, `_pkgdown.yml`, `DESCRIPTION`, `tests/` or `data-raw/` path, and adds
  no new top-level file. Run anyway:
  `devtools::document()` exits 0 and leaves `git status --porcelain` empty (no
  diff, so `NAMESPACE`/`man/` are not hand-edited); `pkgdown::check_pkgdown()`
  reports "No problems found"; `devtools::check()` is clean — 0 errors,
  0 warnings, 0 notes, with `testthat.R` OK in 110s. `NEWS.md` needs no entry:
  the milestone changes only the repo's own `cairn/` tracking records and ships
  no user-visible change. No new top-level file, so `.Rbuildignore` is unchanged
  (`cairn/` is already covered by `^cairn$`).

### Independent fresh-context review

Routing: the declared surface tier is **internal** and
`git diff master..HEAD --name-only` shows six `cairn/` markdown files and no
executable surface, so one fresh-context reviewer was spawned — the [O] diff-bug
lens — and the blame-history and prior-PR-comments lenses were skipped per the
docs-only route.

The reviewer independently re-derived every criterion rather than reading the
implementer's figures: it recomputed the 44 branch-point keys, re-ran the
partition (27 / 17 / 0 / 0), string-compared all 17 graduated entries against
their branch-point text (byte-identical), recomputed all 44 byte lengths in
`lessons-baseline-M084.md`, and confirmed each named trim owner actually holds
the content (`D056`, `CLAUDE.md`, `test-builder-blame-front-door.R`). It also
re-verified every figure in the ROADMAP stamp. Fourteen findings, ranked:

1. **Truncated, ungrammatical residual on the M075 entry** — `cairn/LESSONS.md:32`. The trim cut mid-sentence and left a comma splice: "…signalled two warnings for one drop on the two-pass path, The two ways M075's suite failed to see it are in `references/false-greens.md`." The original clause "and the test could not see it:" was removed but its comma was not. Severity: it is the only outright broken sentence in a durable record, and it reads as an unfinished edit.
2. **The M47 entry's trim removed content no exit covers** — `cairn/LESSONS.md:25`. Dropped: the M48-F1 correction ("the converse M47 drew — 'if the pipeline threads `call`, the front-door copy buys no blame, so omit it' — holds ONLY where the verb calls its pipeline DIRECTLY"), and M45's reshaped-index observation (a 2-row table blamed "In index: 3"). The named owner is a test file, i.e. the *enforcement* exit — but a test cannot fail on a lost historical correction, and the reshaped-index fact is nowhere in `test-builder-blame-front-door.R`. The entry's own attribution line still reads "absorbs M45's reshaped-index line" while the absorbed line is gone.
3. **Dangling ordinal on the FFmpeg-version entry** — `cairn/LESSONS.md:9`. The trim correctly changed "Two instances paid for" to "One instance paid for", but the sentence three clauses later still begins "A third platform failure is not the package at all". With one instance enumerated, "a third" has no referent.
4. **The module's budget has less headroom than one member** — `cairn/references/false-greens.md:7`. Header: "Budget: fewer than 26,000 bytes… Set from the graduated size plus room for a few more members." Actual 24,846 bytes leaves 1,154, while the module's 20 entries average 1,242 bytes — the next single graduate overflows it.
5. **`LESSONS.md` clears its byte budget by 628 bytes, under one average entry.** T3's stop rule was "when AC1 clears with headroom"; 628 bytes is 3.1%, and the 27 surviving entries average 717 bytes, so the next lesson captured re-breaks the budget.
6. **The module is not discoverable from the file it was cut out of** — `cairn/LESSONS.md:1-5`. Its header still says only "surfaced at plan time… Capped at 50 lines (D-015)"; it does not point at `references/false-greens.md`, and `CLAUDE.md` mentions it zero times. The module's own line "Read at plan time alongside `LESSONS.md`" only reaches a reader who already found it.
7. **The M41 entry's M080 clause was rewritten past the trim** — `cairn/LESSONS.md:17`. Beyond moving the otherwise-valid-grid half to the module, the trim also deleted the record that M080's NEWS paragraph "was returned twice, round 1 understating the reorder and round 2 overstating it" plus the quoted bad draft, replacing it with "falsified four earlier drafts, two of them mis-stating how far the reorder reached". No exit in the Decisions section covers that deletion.
8. **The hygiene stamp is written pre-merge and pins a figure the merge invalidates** — `cairn/ROADMAP.md:5`. It opens "M084 branch:" (M083's opened "M083 post-merge:") and states `cairn/ROADMAP.md` "22,360 bytes over 45 lines" — a number that changes the moment review flips the row to `done` and repoints it at `milestones/archive/`.
9. **Borderline keep: the M47 `-map` entry retains a false-green clause** — branch-point entry 27, classified `keep`. It contains "The suite caught only the video-only half, via one test that happened to use a silent fixture" — a fixture-coincidence false green whose near-twin (branch-point entry 29) graduated. Under the milestone's own partly-covered rule this was a trim candidate.
10. **Enforcement pointer silently reassigned** — `cairn/LESSONS.md:25`. The original credited "M57's nine-verb sweep"; the trim credits `tests/testthat/test-builder-blame-front-door.R`, whose header comments date it to M64/M65. The `purrr::pmap` / `caller_env` mechanism and the explicit "LESSONS M47/M48-F1" citation actually live in `tests/testthat/test-value-check-front-door.R:11-15` (the M59 sweep). The named file does redden, so the exit holds, but the pointer moved without a note.
11. **Module header overstates verbatimness** — `cairn/references/false-greens.md:3-4`: "Every entry below left `LESSONS.md` verbatim". Three of the 20 lines (`moved M084`) were rewritten to stand alone. The provenance block eight lines down says so correctly; the flat claim above it does not.
12. **Stale candidate row** — `cairn/ROADMAP.md:20`. The second-doctrine-module candidate cites "`cairn/LESSONS.md` lines 21, 33 and 41 at bd4d545"; line 41 ("a blame-reading test passing for the wrong reason") is one of the 17 M084 just graduated, so the candidate now proposes a module overlapping the one that shipped.
13. **Work-log figure off by 5 bytes** — T4's line reads "`ROADMAP.md` 22,365 bytes"; actual and the stamp both say 22,360.
14. **Scope estimate never reconciled** — Scope says the family is "roughly fifteen entries and ~20,100 bytes"; it turned out 17 entries / 21,171 bytes. Recorded accurately in Decisions, but Scope reads as if unamended.

**Return floor.** No finding demonstrates an acceptance criterion failing — the
reviewer confirms all four hold as written — and none is a load-bearing defect in
what this repo ships to its users (the R package's verbs and their behavior);
every finding is a defect in the `cairn/` record. So none returns the milestone,
and each takes ordinary triage at the gate.

**Triage** (maintainer at the approval gate, 2026-08-28): fix now 1, 2, 3, 6, 7,
10, 11, 12, 13; follow-up 4 and 5; reject 9 and 14; 8 discharged by this pass.

- **Fixed on the branch, before the approval marker.**
  **1** — the comma became a full stop, so the sentence closes.
  **2 and 10 together** (one clause, `cairn/LESSONS.md:25`) — the M48-F1 converse
  limit and M45's reshaped-index observation are restored in compressed form, and
  the clause now names `test-value-check-front-door.R` (M59) as the file carrying
  the M47/M48-F1 citation alongside `test-builder-blame-front-door.R` as the file
  that reddens, so the attribution line "absorbs M45's reshaped-index line" is
  true again.
  **3** — "A third platform failure" → "A separate platform failure".
  **6** — the `LESSONS.md` header now points at `references/false-greens.md`.
  **7** — the record that M080's NEWS paragraph was returned twice, with the
  round-2 draft quoted, is restored.
  **11** — the module header's verbatim claim now excepts the three `moved M084`
  lines, agreeing with the provenance block below it.
  **12** — the second-doctrine-module candidate row is narrowed: it now cites the
  two entries that remain and records that line 41 was graduated by M084.
  **13** — 22,365 → 22,360.
  Re-verified after the fixes: AC1 33 lines / 19,860 bytes; AC2 and AC4 partition
  unchanged at 27 / 17 / 0 / 0 (no key disturbed); AC3 53 lines / 24,959 bytes;
  `cairn_validate.py` exit 0.
- **Follow-up — 4 and 5**, filed as one candidate row in `cairn/ROADMAP.md`:
  both files clear their budgets by less than one average entry (`LESSONS.md`
  140 bytes of room against a 735-byte average entry; the module 1,041 against
  1,248), so the next lesson captured re-breaks one of them. The row carries the
  merge-commit figures and the open call — compress, raise a budget with a stated
  reason, or graduate a second family.
- **Rejected — 9**, the borderline `keep` on the M47 `-map` entry: the clause the
  reviewer points at is one sentence of evidence inside an entry whose subject is
  `-map`'s stream selection, not test discrimination, so it is a defensible
  classification call rather than a defect; recorded here rather than acted on.
- **Rejected — 14**, the unreconciled Scope estimate: Scope is plan-owned and may
  not be edited at review (the never-reinterpret rule); Decisions carries the
  measured 17 / 21,171 accurately.
- **Discharged — 8**: the stamp is rewritten in the post-merge hygiene pass with
  the merge-commit figures and a `post-merge` opening, which is what the finding
  asks for.

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in REDUCED mode (internal tier, no RB-tripwire tags). Round 1 returned four findings on this milestone's criteria: AC2 said "the pre-branch commit" without a SHA, AC2/AC4 routed their domain through a work-log disposition ledger (recording instrument, D-120), AC4's "no line of it remains" named no procedure, and AC5 mandated a run showing a test red under a planted defect — a plant matrix plus a recording act, and a demonstration crossing into R sources and testthat inside a milestone whose deliverable is `cairn/` markdown. Round 2 on the revised wording passed AC1/AC2/AC3 and returned two: AC4's "dated opening clause" was not procedurally bounded (two entry pairs share their first 20 characters), and the revised AC5 still bound archive-summary prose rather than the files. AC5 was dropped — AC2 already carries the deliverable-side fact — and AC4's key fixed to `cut -c1-120`, verified unique across all 44 entries.
- 2026-08-28: plan gate chose `cairn/references/false-greens.md` over a new `cairn/doctrine/` directory, because references/ exists with an INDEX and this repo has no doctrine-directory convention; falsified by a second family graduating and the two modules reading as source summaries in the index. Round 2's reader flagged the boundary: the module is doctrine-shaped and the plugin sites its own under `skills/shared/`, so the choice is a repo-local convention, not the plugin's.
- 2026-08-28: plan gate chose one module holding the family whole over splitting it into false-greens plus front-door-ordering, because the maturation exit requires a family to graduate whole and the second group has been extended twice at most; falsified by AC1 failing to clear with the one module out.
- 2026-08-28: T1 — branch cut; ledger `cairn/references/lessons-baseline-M084.md` committed with all 44 entries, their byte lengths and `cut -c1-120` keys (unique: `sort | uniq -d` returns nothing); classification in Decisions.
- 2026-08-28: T2 — `cairn/references/false-greens.md` authored (46 lines, 22,780 bytes; budget < 60 lines / < 26,000 bytes, chosen at the implement gate), holding the 17 graduated entries verbatim under five themed headings; the same 17 deleted from `LESSONS.md`, which is now 32 lines / 21,044 bytes. Partition checked against the branch point: 27 in `LESSONS.md` only, 17 in the module only, 0 in both, 0 in neither.
- 2026-08-28: T3 — six partly-covered entries trimmed to their remainders; the FFmpeg-version, error-precedence and two-pass halves moved into the module as three new lines, the front-door-guard half dropped to `tests/testthat/test-builder-blame-front-door.R`, the blame-config half to `CLAUDE.md` and the timeout escalation figures to D056. `LESSONS.md` 19,372 bytes / 32 lines (under 20,000 / 50, 628 bytes of room); module 24,124 bytes / 49 lines (under its own 26,000 / 60). Partition re-checked: 27 / 17 / 0 both / 0 neither. `devtools::test()`: 0 failures, 8223 pass, 5 skip.
- 2026-08-28: T4 — `Last hygiene check` stamp replaced with M084's figures: `LESSONS.md` 19,372 bytes / 32 lines, `false-greens.md` 24,846 bytes / 52 lines against its own < 26,000 / < 60 budget, `ROADMAP.md` 22,360 bytes / 45 lines. Both new `references/` pages carry provenance blocks; all 16 `cairn_validate` checks PASS, all 7 advisories OK.
- 2026-08-28: review in progress — AC1–AC4 verified with fresh evidence and ticked; universal cairn gate clean (16/16 PASS, 7/7 advisories OK). `devtools::check()` and the fresh-context diff-bug reviewer still running; their results and the triage fill the two placeholders in the Review section.
- 2026-08-28: review checkpoint — toolchain consistency gate clean (`devtools::check()` 0/0/0, `document()` no diff, `pkgdown::check_pkgdown()` no problems). Fresh-context diff-bug reviewer still running; its findings and triage remain the one open placeholder.
- 2026-08-28: review — one fresh-context reviewer (docs-only internal route) returned 14 findings, none a criterion failure and none a defect in shipped behavior, so no return floor fired. Maintainer triage at the gate: nine fixed on the branch, 4 and 5 filed as one candidate row, 9 and 14 rejected with reason, 8 discharged by the hygiene pass. Every criterion re-verified after the fixes.
