# M109: The codec seam's four instrument gaps are triaged under D072, and the two with a path to a user are closed

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** internal — the deliverable is two test instruments, a data-generation script, and a tracking ledger
- **Branch/PR:** `m109-codec-seam-gap-triage` / https://github.com/jmgirard/tidymedia/pull/113

## Goal

Apply D072's rule to the four codec-seam instrument gaps M106 and M107 left as a
candidate row: close the two whose gap lets a defect in shipped behaviour reach a
user, and prune the other two with their reasons recorded.

## Scope

**In:** Cross a jobs-column form of `video_codec` into the out-of-table sweep
(`tests/testthat/test-hardware-out-of-table-blame.R`), so `check_hardware_available()`'s
`is.list()` arm (`R/ffmpeg.R:3348`) is swept rather than bound by one hand-written
cell — gap (d). Restore wrong-form `video_codec` cells to the probe-order grid
(`data-raw/nvenc-probe-order-baseline.R`) without disturbing the `caller`/`sentinel`
cross that displaced them, so a malformed token's compiled bytes are recorded and not
only its blamed frame — gap (a). Demonstrate each close by planting the defect it
claims to catch. Record the four dispositions in one ledger.

**Out:** Making `tm_hw_encoder_checked_before()` dataflow-aware — gap (c), pruned in
this milestone's ledger under D072 and the checker-regress shape; its three sites are
straight-line, so positional and dataflow readings agree on every member of the domain
today. Removing `apply_video_codec()`'s second `check_video_codec()` call — gap (b),
pruned in the same ledger; `R/ffmpeg.R:3406-3409` already records the duplication as
deliberate and idempotent, and it reads correctly against the code. No `R/` change of
any kind: a change there would move this milestone's surface tier.

## Acceptance criteria

- [x] AC1: For every member `nvenc_order_members()` returns that carries a
      `video_codec` formal, `data-raw/nvenc-probe-order-baseline.R`'s cell set
      contains one cell per form `tm_nvenc_wrong_forms()` returns, and the runner
      records each such cell's compiled command bytes or refusal at every
      `(hardware, fallback, pool)` combination it crosses.
- [x] AC2: Running the grid against a working tree with `check_video_codec()`'s
      token refusal removed changes at least one row against the recorded baseline;
      running it against the unmutated tree changes none.
- [x] AC3: For every member `oot_members()$reachable` whose `formals()` include
      `jobs`, `tests/testthat/test-hardware-out-of-table-blame.R`'s out-of-table
      sweep runs each `(member, omitted pair, fallback)` cell it enumerates in both
      a scalar `video_codec` form and a jobs-column form whose `video_codec` column
      spells an in-table family of the pair's backend before the pair's omitted family.
- [x] AC4: Running the suite against a working tree where `check_hardware_available()`'s
      family loop (`R/ffmpeg.R:3360`) is narrowed to `families[1]` reddens
      `test-hardware-out-of-table-blame.R`; the unmutated tree leaves it green.
- [x] AC5: The milestone file carries a ledger with one row per gap in the four the
      plan commit's absorbed candidate row named, each classed `close` or `prune`
      under D072 with its own recorded reason.
- [ ] AC6: `devtools::check()` reports 0 errors and 0 warnings and `devtools::test()`
      passes.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T6
- AC6 → T6

## Tasks

- [x] T1: Add a wrong-form `video_codec` cell class to `nvenc_order_cells()`
      (`data-raw/nvenc-probe-order-baseline.R:161-184`) that carries the form as the
      cell's own `video_codec` value instead of returning the argument to the
      `setdiff()` at `:173`, so the `caller`/`sentinel` cross at `:186-204` and
      `:259-270` stays whole and the new cells take that cross's `absent` level.
      Set every value with `args["video_codec"] <- list(form)`, never `$<-` (M106).
- [x] T2: Re-record `data-raw/nvenc-probe-order-merge-base.rds` forward through
      `nvenc_order_baseline(attr(x, "ref"))` at the ref the object already carries,
      and update the header's row count and provenance (`:68-96`, `:81-85`) and the
      note at `:349-352` that says the grid the 27 was measured over no longer exists.
- [x] T3: Plant AC2's defect — remove `check_video_codec()`'s `check_token()` call
      (`R/ffmpeg.R:3416`) in a scratch tree — run the grid against it, and record in
      the work log how many rows differ from the baseline and which cells they are;
      then confirm the unmutated tree differs in none. Revert the scratch tree.
- [x] T4: Give `oot_args()` (`tests/testthat/test-hardware-out-of-table-blame.R:59-70`)
      a jobs-column form for members whose `formals()` include `jobs`, building the
      column from `hardware_backend_families()[[pair$hardware]][[1]]`'s codec followed
      by the pair's omitted codec, and wrap the sweep's loops (`:124-147`) in a form
      loop. Extend the non-emptiness control at `:73-80` to cover the new partition,
      so a form set that silently emptied cannot pass vacuously.
- [x] T5: Plant AC4's defect — narrow `R/ffmpeg.R:3360` to `families[1]` in a scratch
      tree — run `test-hardware-out-of-table-blame.R` against it, record which cells
      redden and confirm the scalar-only cells stay green, then confirm the unmutated
      tree is green. Revert the scratch tree.
- [x] T6: Write the four-row D072 ledger into `## Decisions` as one table, one row per
      gap (class, reason), never one sub-heading per gap (M092's line-cap lesson);
      run `devtools::document()`, `devtools::check()` and `devtools::test()`.

## Work log

- 2026-09-05: created by /milestone-plan.
- 2026-09-05: plan gate chose the D072 triage shape (close (a) and (d), prune (b) and (c)) over hardening all four instruments because D072's "reaches a user" test is the standing rule and the artifacts (b) and (c) grade are correct today; falsified by a defect reaching a user through the gap (b) or (c) leaves open.
- 2026-09-05: plan gate chose a separate non-crossed wrong-form cell class for gap (a) over recording compiled bytes in `tm_nvenc_condition()` because that helper compares the `none` and `nvenc` arms for parity and a compiled command differs between them by the codec name, so recording bytes there would break parity at every cell; falsified by a measurement showing the two arms' compiled bytes agree wherever both compile.
- 2026-09-05: plan gate chose a two-family jobs column with the omitted family in a non-first position for gap (d) over a one-element list because a one-element column passes a `families[1]` regression unchanged; falsified by a mutation the two-family cell misses that a one-element cell catches.
- 2026-09-05: AC5 lost its "the ROADMAP candidate row is deleted in the same commit" clause after the gate: the plan commit absorbs a promoted candidate row, so that half was satisfied before implementation began. The ledger clause is unchanged.
- 2026-09-05: criteria audit ran in reduced mode (internal tier). One finding: AC3's restriction "fans out through a jobs table" was decided by the author's judgment rather than a procedure. Fixed at the gate — rewritten to `oot_members()$reachable` whose `formals()` include `jobs` — and re-checked clean by the same reader.
- 2026-09-05: T1: `nvenc_order_cells()` gained a `video_codec/<form>` class beside the caller/sentinel cross, and the spec flag the runner reads was renamed `has_vc` -> `cross_vc` because it now means "the runner sets this cell's `video_codec`", which the new class makes false for a member that does have the formal. Grid on the working tree: 7,200 rows (was 6,840), 360 of them the new class, 0 vacuous, all 360 aborts.

- 2026-09-05: T2: `nvenc-probe-order-merge-base.rds` re-recorded at the ref it already carried (`96e973b`): 7,200 rows, 0 vacuous. All 6,840 rows the previous object held reproduced with the same kind, outcome and blamed frame, and the 360 rows of difference are exactly the new `video_codec/<form>` class. Header provenance, the usage block's stale M095 figure, and the note on why M095's grid is no longer re-derivable all updated.

- 2026-09-05: T3: the defect was planted in a COPY of `R/`, `NAMESPACE` and the sample under the scratch dir and run through `nvenc_order_baseline(root =)`, rather than by editing and reverting the working tree, so the repo was never dirty. Mutant (`check_video_codec()`'s `check_token()` call removed): 18 rows differ from the baseline, all of them `standardize_video`'s `video_codec/token` cell at all 18 of its (hardware, fallback, pool) combinations — 6 reblamed from `standardize_video` to `ffm_codec`, 12 still blamed on the verb but now carrying the codec-family message instead of the token one. Every differing row is in the cell class M109 added, so the pre-M109 grid would have reported none. Control: the unmutated working tree differs from the baseline in 0 rows on both the contract and the wide comparison, 0 vacuous.

- 2026-09-05: T4: `oot_forms()` reads the form set off `formals()` and the out-of-table sweep loops over it, so the seven reachable members that take `jobs` now run each (omitted pair, fallback) cell in a two-row column form as well as the scalar one — 42 new cells. Discovered sub-task, not in the plan: the duplicated row needed distinct OUTPUT paths, told from inputs by what `tm_timeout_call_specs()` created on disk; without it four of the seven refused for colliding outputs before reading the codec (measured 2026-09-05). The non-emptiness control gained the form partition and a check that `batch_video_codecs()` really reads each built column as two families with the in-table one first. Suite: 0 failed, 0 errors, 12,440 passed.

- 2026-09-05: T5: the defect was planted in a full package copy under the scratch dir (`R/ffmpeg.R:3360` narrowed to `families[1]`) and the file run there, so the repo was never dirty. `test-hardware-out-of-table-blame.R` reddens: 21 of its 126 out-of-table cells fail, every one of them a jobs-form cell on the `fallback = TRUE` arm, each reblamed from the verb onto `purrr::pmap` — D035's own defect. All 84 scalar cells stay green, so the scalar form cannot see this mutation at all. Unmutated tree: 0 failed, 0 errors, 209 passed.

- 2026-09-05: T6: the four-gap D072 ledger written into `## Decisions` as one table — (a) and (d) closed, (b) and (c) pruned with reasons. `devtools::document()` produced no diff; `devtools::check()` 0 errors, 0 warnings, 0 notes; `devtools::test()` 0 failed, 0 errors, 12,440 passed, 18 skipped.
- 2026-09-05: status to review; all six tasks checked.
- 2026-09-05: review: draft PR #113 opened; AC1-AC5 verified with fresh evidence and their boxes ticked; `cairn_validate` 16/16 with no advisories, `pkgdown::check_pkgdown()` clean, `devtools::document()` no diff, `devtools::check()` 0 errors / 0 warnings / 0 notes. AC6's `devtools::test()` count re-run and the diff-bug review lens still outstanding.

## Decisions

### The four codec-seam instrument gaps, triaged under D072 (2026-09-05)

D072's test is whether the gap lets a defect in shipped behaviour reach a user. Two of the four have that path and are closed here; the other two grade an artifact that is correct, so they are pruned with their reasons, not deferred again. Gap letters are the absorbed candidate row's own (M106 review F1/F2/F3; M107 review F5).

| Gap | Class | Reason |
|---|---|---|
| (a) Crossing `video_codec` forced it out of `nvenc_order_cells()`'s wrong-form set, so a malformed token's compiled bytes went unrecorded and only its blamed frame was kept | close | A path to a user: `check_video_codec()`'s token refusal is what makes a malformed `video_codec` name the caller's own argument rather than the machine. With that refusal removed, 18 grid rows change — six reblame the error from `standardize_video` onto `ffm_codec`, twelve keep the verb's name and change what it says (T3). The grid as M106 left it reported none of them. |
| (b) `apply_video_codec()`'s `check_video_codec()` call is redundant with the emit half's | prune | The artifact it grades is correct. `R/ffmpeg.R:3403-3409` already records the duplication as deliberate: the emit half checks the token itself, so no pipeline can reach the resolver with an unchecked one, and `check_video_codec()` reads its argument and returns, so the second pass cannot change what a caller is told. Nothing a user observes turns on the redundancy. |
| (c) `tm_hw_encoder_checked_before()` reads position, not dataflow | prune | The artifact it grades is correct. The ledger's computed domain is three sites; the one literal site has no token to check, and both symbol sites hold their check at the top of a straight-line body above the resolver — measured 2026-09-05, neither reassigns `video_codec` between the two, the only assignment above each resolver call being that call's own. A dataflow reading returns what the positional one returns on every member of the domain today. A weaker instrument, not a defect. |
| (d) M107's AC1 sweep reached `check_hardware_available()` only through the scalar `video_codec`, leaving the `is.list()` arm on one hand-written cell | close | A path to a user: narrowing the family loop above that arm to its first family leaves a `_batch` caller's out-of-table row blamed on `purrr::pmap` instead of the verb the caller typed — D035's own defect, and what M107 exists to prevent. 21 of the sweep's 126 cells redden under that mutation and all 84 scalar cells stay green (T5), so the scalar-only sweep could not see it. |

## Review

Evidence gathered 2026-09-05 by command on the branch head (`806595a`), against
the working tree; every mutant was built as a COPY under the session scratch
directory and run from there, so the repo was never dirty.

**AC1 — the wrong-form `video_codec` cells and their coverage.** `nvenc_order_members()`
returns 6 members, 4 of them carrying a `video_codec` formal (`anonymize_video`,
`anonymize_video_batch`, `standardize_video`, `standardize_video_batch`);
`tm_nvenc_wrong_forms()` returns 5 forms. `nvenc_order_cells()` holds exactly the
20 `video_codec/<form>` cells that cross names — 0 missing, 0 extra — and each
carries the form as its own value with `cross_vc = FALSE`. Running the grid on the
working tree: 7,200 rows over 18 `(hardware, fallback, pool)` combinations; the 360
wrong-form rows are 20 cells x 18 combinations, no cell short of a combination, every
row a recorded refusal with a non-empty outcome and the `absent` codec level.

**AC2 — the grid is shown able to fail.** Mutant (`check_video_codec()`'s
`check_token()` call removed, `R/ffmpeg.R:3416`): the wide diff against
`nvenc-probe-order-merge-base.rds` returns 18 rows, all of them
`standardize_video`'s `video_codec/token` cell at all 18 of its combinations — 6
reblamed from `standardize_video` onto `ffm_codec`, 12 keeping the verb's name and
changing the sentence from the token message to the codec-family one. Control: the
unmutated tree differs in 0 rows on both the contract and the wide comparison, 0
vacuous on both sides.

**AC3 — both forms in the out-of-table sweep.** `oot_members()$reachable` is 14
members, 7 of which take `jobs`; `oot_pairs(held = FALSE)` is 3 omitted pairs
(`nvenc/prores`, `videotoolbox/av1`, `videotoolbox/prores`). `oot_forms()` returns
`scalar` for every reachable member and adds `jobs` for exactly the 7, so the sweep
enumerates 126 cells: 84 scalar + 42 jobs. For all 42 jobs cells,
`batch_video_codecs()` reads the built column as two families with an in-table family
of the pair's backend first and the pair's omitted family second — 0 exceptions.

**AC4 — the sweep is shown able to fail.** Mutant (`R/ffmpeg.R:3360`'s family loop
narrowed to `families[1]`): `test-hardware-out-of-table-blame.R` reddens. Cell-level:
21 of the 126 cells fail, every one a jobs-form cell on the `fallback = TRUE` arm,
each reblamed from the verb onto `purrr::pmap` — D035's own defect. All 84 scalar
cells pass, so the scalar form cannot see this mutation. Unmutated tree: the file is
green (0 failures).

**AC5 — the ledger.** The absorbed candidate row (ROADMAP at `e5731f6`, from M106
review F1/F2/F3 and M107 review F5) named four gaps (a)-(d); the milestone's
`## Decisions` section carries one table with one row per gap, each classed `close`
(a, d) or `prune` (b, c) with its own reason. Spot-checked the (c) row's own claim by
command: `tm_hw_encoder_ledger()`'s computed domain is 3 sites — one literal
(`format_for_web_pipeline`, `"libx264"`) and two symbol sites
(`anonymize_pipeline`, `emit_video_codec`) whose only assignment to `video_codec`
at or above the resolver is that resolver call itself, so the positional and
dataflow readings agree on every member of the domain, as the row states.

**AC6 — pending.** `devtools::check()` and `devtools::test()` are running.

**Consistency gate (partial).** `cairn_validate.py` 16/16 PASS, no advisories fired.
No DESIGN principle changed (`Principles touched: —`), so `cairn_impact.py` does not
apply. Toolchain slot: `pkgdown::check_pkgdown()` clean; no new top-level files, so
no `.Rbuildignore` entry is owed; no user-visible change, so no `NEWS.md` entry is
owed. `devtools::document()` no-diff and `devtools::check()` still to run.
