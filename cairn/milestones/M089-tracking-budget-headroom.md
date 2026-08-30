# M089: The guard-ordering family graduates and the tracking files get headroom

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m089-tracking-budget-headroom` — https://github.com/jmgirard/tidymedia/pull/93

## Goal

`cairn/LESSONS.md` and `cairn/ROADMAP.md` each clear their byte budget by more
than one average entry, reached by graduating the front-door guard-ordering
family into its own doctrine module rather than by raising a figure this repo
does not own.

## Scope

**Surface tier: internal.** Every deliverable is a file under `cairn/`; no
external consumer of the package relies on one.

**In:** the call the standing budget candidate row forces, made and recorded.
The front-door guard-ordering and precedence family — the two `LESSONS.md`
entries dated 2026-07-29 (M41) and 2026-07-30 (M47), 3,219 bytes over 2 lines —
graduates whole into `cairn/references/guard-ordering.md` under the maturation
exit. The ROADMAP absorbs the two candidate rows this milestone promotes and is
compressed to a stated figure. A D-entry records which budgets this repo owns
and what remedy each of the three tracked files got.

**Out:**
- Raising `ROADMAP.md`'s or `LESSONS.md`'s byte budget. Those figures are the
  cairn plugin's (`skills/shared/tracking-rules.md`, "Weight caps": line cap ×
  400); a D-entry in this repo cannot supersede plugin doctrine. Changing them
  is the user's call against the plugin → not a milestone here.
- Raising `references/false-greens.md`'s budget. That one IS repo-owned, set by
  M084 in the module's own header, but the header states the remedy over budget
  is compressing or retiring content, never growth. Superseding that → its own
  milestone, promotable if the module ever breaches 26,000 bytes.
- Any third doctrine module. The remaining `LESSONS.md` families have not met
  the maturation exit's twice-extended test → stays unfiled; propose a row only
  when one does.
- The five M088 findings, the unclassed-abort naming pass, and every other
  candidate row → their existing ROADMAP rows, compressed but not promoted.

## Acceptance criteria

- [x] AC1 `cairn/references/guard-ordering.md` exists, carries exactly the two
      graduated entries one per line, and its header states a line budget and a
      byte budget set from the graduated size plus headroom the header itself
      states (the maturation exit's rule).
- [x] AC2 Each of the module's two entry lines is byte-identical to the
      `cairn/LESSONS.md` line it left: `diff` between the module's entry lines
      and those same lines extracted from `cairn/LESSONS.md` at this
      milestone's base commit reports no difference.
- [x] AC3 `wc -c cairn/LESSONS.md` reports fewer than 17,500 bytes and
      `wc -l` reports 28 or fewer, and `grep -F` for a distinctive phrase from
      each graduated entry finds neither in the file.
- [x] AC4 `wc -c cairn/ROADMAP.md` reports fewer than 22,000 bytes and `wc -l`
      reports 44 or fewer, measured after this milestone's own row and its
      replacement hygiene stamp are in the file.
- [x] AC5 `wc -l -c cairn/references/false-greens.md` reports fewer than 60
      lines and fewer than 26,000 bytes, and the figures in the module's own
      header are unchanged from the base commit.
- [x] AC6 `cairn/DECISIONS.md` gains one entry naming, for each of the three
      tracked files, whether its budget is repo-owned or the plugin's and which
      remedy it got here.
- [x] AC7 Every ROADMAP candidate row this milestone's diff shows as edited
      (enumerated by `git diff --unified=0` against the base commit) still
      names a promote trigger in its post-change text.
- [x] AC8 `python3 .../cairn_validate.py` exits clean, and every plan-owned
      section of this file is under the 150-line cap it reports.

## Coverage

- AC1 → T1, T2
- AC2 → T2, T3
- AC3 → T3
- AC4 → T5, T6
- AC5 → T7
- AC6 → T8
- AC7 → T6
- AC8 → T9

## Tasks

- [x] T1 Confirm the maturation exit for the family: read `cairn/LESSONS.md`
      lines 16 and 23, count the extension/consolidation/correction marks each
      carries, and confirm neither the enforcement nor the ownership exit
      applies. Record the count.
- [x] T2 Create `cairn/references/guard-ordering.md` — header modelled on
      `false-greens.md`'s (provenance naming M089 and the originating
      milestones, the stated budget with its headroom, the one-entry-per-line
      rule), then the two entries pasted verbatim.
- [x] T3 Delete those two lines from `cairn/LESSONS.md`; verify byte-identity
      of the moved text by `diff` against the base commit; check `wc -l -c`.
- [x] T4 Add the module to `cairn/references/INDEX.md` under "Working
      artifacts", in the form the other five rows use.
- [x] T5 In `cairn/ROADMAP.md`: delete the two candidate rows this milestone
      absorbs (the second-doctrine-module row and the budget row), add the M089
      row, replace the hygiene stamp.
- [x] T6 Compress the widest remaining candidate rows until `wc -c` is under
      22,000 and `wc -l` is 44 or fewer, checking each edited row after the
      edit that its promote trigger survived.
- [x] T7 Confirm `cairn/references/false-greens.md` is untouched: `git diff`
      empty for that path, `wc -l -c` under its header's figures.
- [x] T8 Append the D-entry (next free id) recording the call and the
      repo-owned/plugin-owned split.
- [x] T9 Run `cairn_validate.py`; fix what it reports; confirm clean.

## Work log

- 2026-08-29: created by /milestone-plan.
- 2026-08-29: criteria audit ran in REDUCED mode (surface tier internal, no RB-tripwire tag on any criterion or task). Deviation from the skill's fresh-context [O] reader: this session's standing instruction forbids spawning subagents unrequested, so the audit was run inline by the plan author. Returned two findings, both fixed here. (1) Bounded-promise: a draft AC3 read "no lesson is lost in the graduation" — universal over "lessons" as content, which no procedure enumerates; narrowed to byte-identity of two named lines plus a `grep -F` absence check. (2) Proportionality: a draft AC4 read "the ROADMAP never needs compressing at a milestone commit again" — quantified over all future commits, unenumerable and disproportionate to an internal-tier deliverable; narrowed to a byte figure with stated headroom. AC6's D-entry was weighed against D-118/D-120's instrument question and kept as deliverable-bound: this milestone's deliverable IS the call, and the D-entry is the call's durable form, not a record of the milestone's own verification.
- 2026-08-29: plan gate chose graduating the family plus compressing over raising the budgets, because `ROADMAP.md`'s 24,000 and `LESSONS.md`'s 20,000 live in the cairn plugin's `skills/shared/tracking-rules.md` as line cap × 400 and no repo D-entry supersedes plugin doctrine, and because `false-greens.md`'s repo-owned budget is stated by its own header to be met by compressing, never growth; falsified by the user changing the plugin's Weight caps section, or by M084's module header being superseded.
- 2026-08-29: plan gate chose the maturation exit over pruning the stalest `LESSONS.md` entries, because tracking-rules names prune-the-stalest "the last resort" and the family clears the twice-extended test (6 marks measured across the two lines against a requirement of 2); falsified by T1 finding fewer than two extension marks, or by the enforcement or ownership exit turning out to apply.
- 2026-08-29: split tripwires weighed and none fired decisively — 8 criteria and 9 tasks sit at the >~7 / >~10 edges without crossing, and a second milestone would add a ROADMAP row consuming the headroom the first one makes, so this stays one milestone.

- 2026-08-29: T1 — maturation exit confirmed for the family: 6 extension/consolidation/correction marks across the two lines (L16 `Extended M080`; L23 `extends M41`, `absorbs M45`, `corrected M48`, `Extended M57`, `Extended M56`) against a requirement of 2. Ownership exit does not apply. Enforcement exit found PARTIALLY met and ruled not to apply: `test-codec-arg-front-door.R:166` freezes a 17-cell codec-family precedence table that reddens when a codec guard crosses the `jobs` check, but nothing fails on the lesson's rule (guard placement, pipeline-validated arguments, reshaped-index blame, derived prose) — L16 states as much itself. Gate: user chose proceed-under-maturation over trim-to-remainder.
- 2026-08-29: gate — module budget set to fewer than 8,000 bytes and fewer than 6 lines (graduated 3,219 bytes over 2 lines, plus room for ~3 more members at this family's ~1,600-byte average); ROADMAP line reduction takes two groupings, the engine-extension trio and the memoization pair, landing at 43 lines.

- 2026-08-29: T2 — `cairn/references/guard-ordering.md` created (24 lines, 5,033 bytes); the two entries pasted verbatim and `diff`-verified byte-identical against `056fd63:cairn/LESSONS.md` lines 16 and 23. Budget restated on the whole-file basis `false-greens.md` uses (`wc -l -c` over the file, one command per module at a hygiene pass) rather than the per-entry basis the gate's option was phrased in: fewer than 10,000 bytes and fewer than 27 lines, carrying the same headroom the gate chose — room for about three more members at this family's ~1,600-byte average.
- 2026-08-29: repaired a plan-authoring defect — the file had no `## Work log` header, so the dated lines sat under `## Tasks` and counted against the 150-line plan-owned cap. Header added; no line moved.

- 2026-08-29: T3 — both lines deleted from `cairn/LESSONS.md`; the header's pointer now names `references/guard-ordering.md` alongside `references/false-greens.md`. `wc -l -c` reports 28 lines / 16,835 bytes (AC3's 28 / 17,500); `grep -F` for a distinctive phrase from each graduated entry returns 0 hits; `diff` against `056fd63` re-confirms the moved text byte-identical.

- 2026-08-29: T4 — `guard-ordering.md` listed in `cairn/references/INDEX.md` under "Working artifacts", in the same `citekey — short title — produced/graduated by M<NN>` form the other five rows use.

- 2026-08-29: T5 — the second-doctrine-module row and the budget row deleted from `cairn/ROADMAP.md` (both absorbed by this milestone); the M089 row was already present from planning; the hygiene stamp replaced and resolved to its own byte count by fixed point.
- 2026-08-29: T6 — grouped the memoization pair (`find_ffmpeg()` unmemoized; M67's per-process memo) into one row and the M31 encoder-surface trio (quality/rate-control knob; GPU decode + `-hwaccel`; other hardware backends) into another, and compressed the instrument-findings row. `wc -l -c` reports 43 lines / 21,773 bytes (AC4's 44 / 22,000). The trio's three source rows named blockers but no promote trigger; the grouped row supplies one. AC7 checked by `git diff --unified=0` against `056fd63`: three rows show as edited, each names a promote trigger.

- 2026-08-29: T7 — `cairn/references/false-greens.md` untouched: `git diff 056fd63` for that path is empty, so its header figures are unchanged; `wc -l -c` reports 55 lines / 25,810 bytes, under the header's 60 / 26,000.
- 2026-08-29: T8 — D067 appended, naming for each of the three tracked files whether its budget is repo-owned or the plugin's and which remedy it got here, plus the maturation-exit ruling and the new module's repo-owned budget. Shown verbatim at the durable-record preview before it was written.

- 2026-08-29: T9 — `cairn_validate.py` reports all checks passed with one advisory, the >7-criteria sizing tripwire the plan already weighed. It first raised a `references staleness` advisory (`guard-ordering.md` provenance recording no extraction status); fixed by adding the `Extraction:` clause `false-greens.md` carries. That block pushed the module to 27 lines, past the 27-line budget it had just stated, so the budget was reset to fewer than 11,000 bytes and fewer than 31 lines — same headroom the gate chose (about three more members), now measured over the shipped file rather than the graduated text alone. D067's budget paragraph and the ROADMAP hygiene stamp were corrected to the new figures before either shipped.

- 2026-08-29: completion — `devtools::test()` clean: FAIL 0 | WARN 12 | SKIP 5 | PASS 8434 (no R source, test, man, NAMESPACE or DESCRIPTION file is in this milestone's diff; the warnings and skips are the suite's standing FFmpeg-banner and nvenc-absent set). `cairn_validate.py` all checks passed, one advisory (the >7-criteria sizing tripwire weighed at plan time). Status → review.

## Decisions

## Review

_Reviewed 2026-08-29 on branch `m089-tracking-budget-headroom`, PR #93, against base `056fd63`. Default branch `master` is at `056fd63` and the branch contains it, so no merge was needed and no evidence is stale._

### Acceptance-criteria evidence
- **AC1 — pass.** `cairn/references/guard-ordering.md` exists at 27 lines / 5,451 bytes. Its `## Entries` section holds exactly two lines (file lines 26 and 27), one graduated entry each. The header states both figures — "fewer than 11,000 bytes and fewer than 31 lines" — and states the headroom they were set from: the shipped size (3,219 graduated bytes over 2 entry lines, plus the header) plus room for about three more members at this family's ~1,600-byte average.
- **AC2 — pass.** `git show 056fd63:cairn/LESSONS.md | sed -n '16p;23p'` (3,219 bytes) diffed against `sed -n '26,27p' cairn/references/guard-ordering.md`: no difference, exit 0.
- **AC3 — pass.** `wc -l -c cairn/LESSONS.md` reports 28 lines / 16,835 bytes, against the criterion's 28 and 17,500. `grep -c -F 'silently reassigns error PRECEDENCE'` and `grep -c -F '"last in the front door" is not "last overall"'` each return 0.
- **AC4 — pass.** `wc -l -c cairn/ROADMAP.md` reports 43 lines / 21,766 bytes, against the criterion's 44 and 22,000, measured with the M089 row and the replacement hygiene stamp both in the file.
- **AC5 — pass.** `wc -l -c cairn/references/false-greens.md` reports 55 lines / 25,810 bytes, against its header's 60 and 26,000. `git diff 056fd63 -- cairn/references/false-greens.md` is empty, so the header figures are unchanged from the base commit.
- **AC6 — pass.** `cairn/DECISIONS.md` gains exactly one entry, D067, whose "Which budget belongs to whom" list names all three tracked files: `ROADMAP.md` (plugin-owned, remedy absorb/group/compress), `LESSONS.md` (plugin-owned, remedy retire via the maturation exit), `references/false-greens.md` (repo-owned, remedy none needed and growth unavailable).
- **AC7 — pass.** `git diff --unified=0 056fd63 -- cairn/ROADMAP.md` shows three candidate rows edited rather than deleted: the instrument-findings row, the grouped memoization pair, and the grouped M31 encoder-surface trio. Each post-change row contains a promote clause — "Promote on a bug reaching a user through one of these gaps…", "Promote (a) on a measured batch stall…; promote (b) on a measured parallel batch…", "Promote any part on the first request naming that part's knob, platform, or backend…".
- **AC8 — pass.** `python3 cairn_validate.py` exits 0, "all checks passed", with one advisory (the >7-criteria sizing tripwire the plan already weighed). The plan-owned body — everything above `## Work log` — is 109 lines, under the 150-line cap the validator's weight-caps check reports on.

### Independent review

Routing: surface tier internal and `git diff master...HEAD --name-only` lists six paths, all markdown under `cairn/` — no script, hook or other executable surface — so the skill's single-reviewer route applies and only the diff-bug [O] lens was in scope. Deviation from the fresh-context spawn, the same one this milestone's plan phase logged and the user accepted at the plan gate: this session's standing instruction forbids spawning subagents unrequested, so the [O] lens was run inline by this session against `git diff 056fd63..HEAD`, the acceptance criteria, `DESIGN.md` and `DECISIONS.md`. Nothing was filtered before reporting; both candidate findings are below, most severe first.

- **F1 — D067's ROADMAP after-figure is 7 bytes stale.** D067 records the ROADMAP going "48 lines / 23,989 bytes → 43 / 21,773". The base figures check out (`git show 056fd63:cairn/ROADMAP.md | wc -l -c` reports 48 / 23,989), but the shipped file is 43 lines / 21,766 bytes. The T9 work-log line says the module's budget change was carried into "D067's budget paragraph and the ROADMAP hygiene stamp"; the stamp was corrected to 21,766 and this figure was not. No acceptance criterion asks for the figure — AC6 asks only that the entry name each file's owner and remedy, which it does — so this is not a floor return. **Disposition: TBD at the gate.**
- **F2 — the deleted budget row's one live condition has no successor row.** The absorbed candidate row watched all three tracked files. Two are now clear by wide margins (ROADMAP 2,234 bytes, LESSONS 3,165). `references/false-greens.md` is not: 25,810 of its own header's 26,000, 190 bytes of headroom, and this milestone deliberately left it untouched. This file's Scope/Out block says superseding that budget is "its own milestone, promotable if the module ever breaches 26,000 bytes" — but with the budget row deleted there is no candidate row for that condition to promote from. The figure survives only on the ROADMAP hygiene stamp, which every pass overwrites. No criterion covers it (AC5 asks only that the file be under budget and its header unchanged), so this is not a floor return either. **Disposition: TBD at the gate.**

### Consistency gate

**Universal cairn-file checks.** `cairn_validate.py` exits 0 — all checks passed, one advisory (`sizing`: M089's 8 acceptance criteria against the >7 tripwire, weighed and dismissed at plan time; the `release window` advisory did not fire). No `DESIGN.md` principle is in the diff, so `cairn_impact.py --changed` was skipped.

**Toolchain checks (`r-package` profile's `consistency-gate` slot).**
- `devtools::check()` — `Status: OK`: 0 errors, 0 warnings, 0 notes.
- `devtools::document()` — exit 0, no diff in `man/`, `NAMESPACE` or any generated file. No roxygen source is in the diff.
- `pkgdown::check_pkgdown()` — "No problems found."
- `devtools::build_readme()` — re-knits cleanly. It reports a diff, but only in two `temp_libpath` strings inside knitted `ffm_compile()` output: `system.file()` resolves to a per-session temporary library, so `README.md` is non-deterministic across machines and sessions in exactly those two lines. Substantively in sync; the churn was reverted rather than committed. Pre-existing and unrelated to this diff.
- Changelog (`NEWS.md`) — no entry required: no user-visible change. Every path in the diff is markdown under `cairn/`.
- `.Rbuildignore` — the one new file, `cairn/references/guard-ordering.md`, is covered by the existing `^cairn$` entry; no new top-level file.

**CI.** Draft PR #93 opened at review start so CI ran alongside; state re-derived before merge.

