# M107: A backend with no encoder for the codec's family is refused by the verb the caller typed, `fallback` or not

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Resolves:** —
- **Surface tier:** user-facing — it moves the blamed frame of an abort exported verbs raise
- **Branch/PR:** `m107-out-of-table-refusal-at-the-verb` / https://github.com/jmgirard/tidymedia/pull/111

## Goal

An out-of-table `(backend, codec family)` pair is refused by the verb the caller
typed on both `fallback` arms, and the encoder pools the codec seam's
instruments mock stop being nvenc-only literals.

## Scope

**In:** The blame fix in `check_hardware_available()` and its sweep over
`nvenc_hardware_exports()`; a no-regression pin on the fallback an absent
in-table encoder still gets; one derivation of the mocked encoder pools from
`hardware_backend_families()`, routed into the three sites that spell the
triple literally; the seam test's videotoolbox-present arm; a forward
videotoolbox baseline for the probe grid.

**Out:**
- Falling back to software for an out-of-table pair — the gate kept the abort;
  reopening it needs a new D-entry.
- Re-measuring M095's reorder contract under videotoolbox — unavailable in
  principle: the backend postdates the reorder, so no pre-reorder ref carries
  it. T5 records a forward baseline instead.
- Routing `format_for_web_pipeline()` and `anonymize_pipeline()`'s direct
  `resolve_hw_encoder()` calls through the codec seam → ROADMAP candidate
  (M106 Out).
- The `prores_videotoolbox` container guard and the other five hardware-surface
  leftovers → ROADMAP candidate (M100 Out).

## Acceptance criteria

- [x] AC1: For every export `nvenc_hardware_exports()` returns, a `video_codec`
      whose family the named backend has no encoder for aborts naming that
      export's own frame under `fallback = TRUE`, as it already does under
      `fallback = FALSE`, over every `(backend, family)` pair
      `hardware_backends()` × `hardware_codec_families()` holds and
      `hardware_backend_families()` omits.
- [x] AC2: An encoder `hardware_backend_families()` does hold but the build does
      not list still falls back to software with a message rather than aborting
      under `fallback = TRUE`, for every `(backend, family)` pair that table
      holds.
- [x] AC3: `devtools::test()` clean, `devtools::document()` produces no diff,
      and `devtools::check()` reports 0 errors, 0 warnings and no new NOTEs.

## Coverage

- AC1 → T1, T2, T6
- AC2 → T1, T2
- AC3 → T1, T2, T3, T4, T5, T6

## Tasks

- [x] T1: Write the sweep first, red. Build each export's valid argument cell
      from `tm_timeout_call_specs()` (`tests/testthat/helper-timeout-sweep.R`),
      never by hand — a hand-built cell aborts on a missing required argument
      and masks the case (measured 2026-09-04: 12 of 14 verbs did). Cross
      `nvenc_hardware_exports()` × the three omitted pairs × `fallback` in
      `{FALSE, TRUE}` for AC1, and × the five held pairs under an empty build
      for AC2. An export whose formals carry no `video_codec` is recorded
      unreachable by reading its formals, never skipped by name. Record the red
      set; do not predict it.
- [x] T2: Fix `check_hardware_available()` (`R/ffmpeg.R:3300-3320`) so the
      out-of-table refusal fires at the front door on both `fallback` arms while
      an absent in-table encoder still returns early under `fallback = TRUE`.
      Correct the early return's comment, whose premise ("`fallback = TRUE`
      returns above, so this call can only pass or abort") is false — the
      refusal reaches `hardware_encoder()` through the mapper first. T1 green.
- [x] T3: Derive the mocked encoder pools from `hardware_backend_families()` in
      one helper, at three levels (nvenc-present, videotoolbox-present, absent),
      and route the three literal spellings through it: `tm_nvenc_encoder_pools()`
      (`tests/testthat/helper-timeout-sweep.R:1390`), `seam_pools()`
      (`tests/testthat/test-codec-seam-bound.R:24`), `nvenc_order_pools`
      (`data-raw/nvenc-probe-order-baseline.R:182`). Keep the existing
      nvenc-pool-under-videotoolbox cell, which
      `test-codec-seam-bound.R:19-23` records as the harder half.
- [x] T4: Cross the third pool level into `test-codec-seam-bound.R`'s existing
      `hw` loop, keeping the zero-probe assertion and the discrimination control
      in every arm. Add a sixth wrong form to `tm_nvenc_wrong_forms()` — a
      well-formed clean token naming no codec — since the five held forms all
      vary malformedness and none reaches `codec_family()`. Amended
      2026-09-04: the sixth form is `tm_nvenc_unmappable_codec()`, a helper of
      its own rather than a sixth entry in `tm_nvenc_wrong_forms()` -- see the
      work log for the measurement. The seam test's `expect_setequal`/
      `expect_length` pins therefore stay at five, and gain a pair over
      `seam_pools()`.
- [x] T5: Add `"videotoolbox"` to the probe grid's `hw` loop
      (`data-raw/nvenc-probe-order-baseline.R:224`) and regenerate
      `data-raw/nvenc-probe-order-merge-base.rds` as a forward baseline.
      Measured cost ≈1.5× on a 25 s / 3040-row working-tree run (2026-09-04).
      No criterion binds this: it is an instrument property.
- [x] T6: Record D085 — an out-of-table pair is a wrong argument, not an absent
      encoder, so it is sited at the front door on both `fallback` arms.
      Annotate D035, D074 and D076 with no forward pointer (IP4). Update
      `NEWS.md`; confirm `@param fallback`'s existing wording still holds.

## Work log

- 2026-09-04: created by /milestone-plan. Absorbs the ROADMAP candidate row "The probe grid's mocked encoder pools are nvenc-only" (M106 Out; M106 review F5; D079).
- 2026-09-04: criteria audit ran in FULL mode (surface tier user-facing), two passes, fresh-context [O] reader both times. Pass 1 returned six findings and reshaped the milestone: AC3 as drafted (M095's reorder contract re-measured under videotoolbox at `b538e63`) was unsatisfiable, since that ref predates the backend, and it was dropped; the grid's member set was found to be a procedure seeded from a hand-list; AC1's stated gap was factually wrong (the seam test already loops both backends — the pool is what is nvenc-only); the drafted pool correlation would have deleted the harder cell; and AC2 re-promised ground `test-hardware-backends.R:127` already holds. The fifth finding surfaced a shipped defect and became the gate's first question. Pass 2 over the gate-changed wording returned seven more: AC1 carried an instrument sub-clause (moved to T1), AC2 bound an unexported function under a user-facing tier with hand-listed axes and a pool cross vacuous for its own clause (dropped as a criterion; T3/T4 keep the work), the five wrong forms vary malformedness only (T4 adds the sixth), T2's no-regression half was bound by no criterion (now AC2), T1's red-cell prediction was unverifiable (T1 now measures rather than predicts), and AC3 left NOTEs unbounded (now bounded).
- 2026-09-04: plan gate chose fixing the blame defect in this milestone over pinning it and routing the fix, and over `/hotfix`, because the fix needs a D-entry — `check_hardware_available()`'s early return encodes D035/D074's siting reasoning and its comment states a false premise — and `/hotfix` writes none; falsified by the fix landing with no rule worth recording.
- 2026-09-04: plan gate chose keeping the abort under `fallback = TRUE` over falling back to software, because it is current behaviour and what `@param fallback` documents at eleven blocks, so only the blamed frame changes; falsified by a report of a caller who set `fallback = TRUE` expecting a wrong backend/codec pair to be tolerated.
- 2026-09-04: plan gate chose bounding AC1's verb axis by `nvenc_hardware_exports()` over the 14 with a `video_codec` formal and over one exemplar pair, because a seventeenth verb then joins on its own; falsified by an export the NAMESPACE filter admits that cannot reach the check at all.
- 2026-09-04: implement gate chose fixing the unmappable-codec class alongside the out-of-table class, because the front door must infer the family before it can test the table, so both classes move together; falsified by a caller who wants an unrecognized `video_codec` tolerated under `fallback = TRUE`.
- 2026-09-04: implement gate chose adding the sixth wrong form to the shared `tm_nvenc_wrong_forms()` table over a seam-test-local one, accepting a re-measurement of `tm_nvenc_dropped_master()`, `tm_nvenc_mismatch_master()` and the two pinned sweep counts; falsified by the re-measurement proving unstable.
- 2026-09-04: T1 red, measured on the branch: 24 of the 84 AC1 cells (14 reachable members x 3 omitted pairs x 2 fallback arms) blamed `purrr::pmap` instead of the member -- all 24 under `fallback = TRUE`, at the 8 members that fan out (`anonymize_video_batch`, `compare_videos_batch`, `crop_video_batch`, `picture_in_picture_batch`, `segment_video`, `segment_video_batch`, `separate_audio_video_batch`, `standardize_video_batch`). AC2's 10 cells and the domain test were green already.
- 2026-09-04: T2 green. The family sweep moved above `check_hardware_available()`'s `fallback` early return and each family goes through `hardware_encoder()` there, so the table lookup runs on both arms while the availability probe below still returns early. All 24 red cells now name their own member. Both false comments corrected (the early return's, and `resolve_hw_encoder()`'s claim that `fallback = TRUE` always returns above). `devtools::test()`: 0 failures, 10 warnings, 18 skips, 12128 passes -- the M095/M096 argument-outranks-the-probe sweeps and their two pinned counts unchanged, so the new front-door refusals displaced no error a caller had already earned.
- 2026-09-04: `test-nvenc-front-door.R`'s AC4 test "fallback = TRUE never lets the front door refuse an unmappable codec" asserted the defect (blame NOT the verb) and was rewritten to assert the verb, plus a new sibling pinning that an in-table encoder the build lacks still reaches the per-row fallback. The section header narrowed from "reaches no front-door guard" to "reaches no AVAILABILITY guard".
- 2026-09-04: T3 done. `tm_hardware_encoder_pools()` (helper-timeout-sweep.R) derives three pool levels from `hardware_backend_families()`; `tm_nvenc_encoder_pools()`, `seam_pools()` and `nvenc_order_pools` all read it, and no encoder triple is spelled out in a test or generator any more. `test-nvenc-probe-blame.R` clean, so the derivation is behaviour-neutral for the AC1 sweep.
- 2026-09-04: T4 done, with a minor amendment to its own text. Adding the sixth form to the shared `tm_nvenc_wrong_forms()` was measured rather than estimated: 1535 cells to 1842, kept 1093 to 1235, dropped 442 to 607, and 26 new `member/arg -> <none>` entries in `tm_corrupt_dropped_master()`'s census -- entries recording that `"notacodec"` is a legal value for those arguments, not that a guard caught it. That table's contract is values wrong on the type / token-shape / missingness / length / container axes, which a well-formed legal string is not, and its census is asserted as a two-way difference from a fixed earlier ref, so honouring the design would mean re-measuring that ref with six forms. Second implement gate chose a separate `tm_nvenc_unmappable_codec()` helper instead, over re-measuring and over dropping the case; one definition, no recorded baseline moves. The seam test gained the unmappable-token cell over all six (pool x backend) arms at zero probes, and its discrimination control now runs in every backend arm against that backend's own pool rather than only nvenc's.
- 2026-09-04: correction to the first implement gate: the cost I put to the user for the shared-table option (two recorded tables, two counts) understated it -- four tables, five counts, across three files. The corrected figures were measured and re-put at a second gate before anything was written.
- 2026-09-04: T5 done, widened past its own text. The `hw` loop reads `hardware_backends()` rather than gaining one literal, and the pool axis gained a level per backend for the same reason: with only the nvenc and empty pools, every videotoolbox cell at `fallback = FALSE` would have been the availability abort, so the arm would have measured a build that never lists its encoders. `nvenc_order_vacuous()`'s carve-out re-keyed from `pool == "absent"` to the pool not naming the cell's backend. 3,040 rows to 6,840 (2.25x, not the 1.5x the task estimated for the `hw` axis alone), 62 s per ref.
- 2026-09-04: `data-raw/nvenc-probe-order-merge-base.rds` re-recorded at `96e973b` (master tip) as a forward baseline, replacing the `b538e63` M095-era grid the file had carried since M095: 6,840 rows, 0 vacuous on both refs, and 0 rows on both the contract and the wide diff against the working tree. Zero is the honest answer and not a silent instrument: the grid crosses only `libx264` and the NULL sentinel, both h264, and M107's subject is a family no backend covers -- so the grid says M107 disturbed nothing it watches, and AC1's own sweep is what measures the change. The script header now carries the fixture's provenance (source ref, generator call, no seed), which it had never recorded.
- 2026-09-04: T6 done. D085 recorded, its heading naming what it annotates (D035's front-door licence, D074 property 1's per-row class, D076's early return); the three older entries are not edited (IP4). `NEWS.md` gains a Bug fixes entry. `@param fallback`'s existing wording at eleven blocks already says an out-of-table pair aborts whatever `fallback` says and needed no change; `devtools::document()` produces no diff.
- 2026-09-04: the NEWS entry also claims the unmappable-codec class, which no test bound, so the AC1 sweep gained a third arm for it -- every reachable member x backend x fallback arm, each with a mapping-codec control asserted not refused in that same cell first, so a refusal is the token's and not the cell's.
- 2026-09-04: AC3 measured. `devtools::test()`: 0 failures, 10 warnings, 18 skips, 12,158 passes. `devtools::document()`: no diff. `devtools::check()`: Status OK, 0 errors / 0 warnings / 0 notes. The first check run had 1 NOTE -- `spelling.Rout` differing on "ProRes" and "behaviour" from the new NEWS entry -- fixed by rewording to the encoder-name spellings the wordlist already carries and to US "behavior", not by adding wordlist entries. Status to review.

- 2026-09-04: /milestone-review, in progress. PR #111 opened draft; `master` unmoved since the branch was cut. AC1 and AC2 verified and ticked against fresh evidence; AC3 has `devtools::test()` and `devtools::document()` recorded and `devtools::check()` still running. `cairn_validate` 16/16, no advisories; `pkgdown::check_pkgdown()` clean; NEWS carries the entry and names no milestone. Three review lenses spawned, none reported yet.

- 2026-09-04: /milestone-review checkpoint 2. AC3 verified and ticked (`devtools::check()` Status OK, 0/0/0); consistency gate passes in both halves; the blame-history and prior-review lenses reported no findings. The [O] diff-bug lens is still running and CI on PR #111 is pending.

- 2026-09-04: /milestone-review, [O] lens returned nine findings. Six fixed on the branch (F1/F3/F4 reworded the NEWS entry, whose "only the name in the error has changed" claim three measured consequences falsify; F2 rewrote T2's replacement comment, false in both its new claims; F6 `$`->`[[`; F8 derived a re-hard-coded count), two rejected (F7 pre-existing, F9 stale), one routed to a candidate row at hygiene (F5, the jobs-column list branch the sweep never reaches). No finding demonstrates a criterion failing, so no return floor fires. Affected tests and the full suite re-run green; `devtools::check()` re-running over the reworded NEWS.

- 2026-09-04: step-7 approval: PR #111 approved for merge.

## Decisions

## Review

Evidence gathered 2026-09-04 on `m107-out-of-table-refusal-at-the-verb` at
`a964042`, against `master` unmoved since the branch was cut (0 behind).
PR: https://github.com/jmgirard/tidymedia/pull/111

**AC1 — verified.** `test-hardware-out-of-table-blame.R`, 30 passing
expectations, 0 failures. Domain measured from the package rather than listed:
`nvenc_hardware_exports()` returns 16 exports, 14 of them reachable (the two
recorded unreachable by their own formals are `format_for_web` and
`format_for_web_batch`, neither carrying a `video_codec` argument);
`hardware_backends()` x `hardware_codec_families()` is 2 x 4 = 8 pairs, of
which `hardware_backend_families()` omits 3 -- `(videotoolbox, av1)`,
`(nvenc, prores)`, `(videotoolbox, prores)`. 14 x 3 x 2 fallback arms = **84
cells**, every one aborting with the calling export's own frame and the
sentence `<backend> has no "<family>" encoder.`; frame and message are both
asserted, so a cell that kept the frame but changed the sentence would fail.
The build is answered generously through the option seam (every encoder either
backend could have is listed), so no cell can be refused for an absent encoder
instead. The unmappable-codec class NEWS also claims carries its own arm: 14 x
2 backends x 2 arms = **56 cells**, each preceded by a mapping-codec control in
the same cell asserted NOT refused, so the refusal is the token's.

**AC2 — verified.** Same file, the in-table half: under an empty build
(`tidymedia.hardware_encoders = character()`) every one of the 5 held pairs at
each of the 14 reachable members = **70 cells** returns without aborting and
emits a message containing "falling back". No cell aborted.

**AC3 — verified.** `devtools::test()`: 0 failures, 10 warnings, 18 skips,
**12,181 passes**. `devtools::document()`: no diff (`git status` clean apart
from this milestone file). `devtools::check()`: **Status OK, 0 errors / 0
warnings / 0 notes**, 5m 37.6s -- so no new NOTE, the bound the criterion sets.
The `spelling.Rout` comparison the first implement-side run tripped is OK here.
Re-measured after the fix-now commit, since it reworded NEWS: **Status OK, 0
errors / 0 warnings / 0 notes**, 5m 23.2s, `spelling.Rout` comparison OK; the
suite and `document()` unchanged at 12,181 passes and no diff.

### Consistency gate

Universal cairn-file checks: `cairn_validate.py` **16/16 PASS**, all seven
advisories OK (including `release window`, so step 10's parking clause does not
fire). No `DESIGN.md` principle changed on this branch, so `cairn_impact.py
--changed` is skipped -- `Principles touched: IP1` records a principle the work
obeys, not one it edits.

Toolchain checks, from the `r-package` profile's `consistency-gate` slot:
`devtools::document()` no diff; `NAMESPACE`, `man/` and `data/*.rda` untouched
by the branch, so nothing generated was hand-edited; `README.Rmd` unchanged, so
no re-knit is owed; `pkgdown::check_pkgdown()` "No problems found"; `NEWS.md`
carries a Bug fixes entry for the user-visible change and names no milestone
number; the branch adds no top-level file, so no `.Rbuildignore` entry is owed;
`devtools::check()` Status OK.

### Independent review

Three lenses, fresh-context, distinct evidence bases, all three spawned (the
declared tier is user-facing and the diff touches R code, so the full fan-out
rather than the docs-only single lens).

- **[S] blame-history: no findings.** Traced the deleted `if (fallback)
  return()` line through M100 to the M095 reorder (D075) and reports the move as
  what D085 documents rather than a silent regression: only the
  machine-independent table lookup moved above the early return, and the
  build-dependent `hardware_encoder_available()` probe stays below it, which is
  what D075 requires. Checked D079's "the refusal lives in the mapper, once" and
  found no contradiction -- the branch adds a call site, not a second copy of
  the refusal. Read the M100-era pin `test-nvenc-front-door.R` inverts and found
  the inversion annotated in place.
- **[S] prior-review record: no findings.** Primary surface, archived `##
  Review` sections on the touched files (M094, M095, M096, M100, M106, M56,
  M61), plus `LESSONS.md`: the diff resolves the M106 review's mocked-pool
  candidate rather than regressing a prior lesson, and the M100 `call =`
  lesson's site is untouched here. Secondary surface probe
  `gh api repos/jmgirard/tidymedia/pulls/comments?per_page=1` returned `[]`, so
  no per-PR thread walk was paid for.
- **[O] diff-bug: nine findings**, verified individually against the
  implementation and recorded below with dispositions. It also cleared, by its
  own account: the committed `.rds` matches its stated 6,840 rows / `96e973b`
  ref / 3x3 cross, `nvenc_order_vacuous()`'s re-keying is correct for all nine
  cells, every front-door call site validates the token above
  `check_hardware_available()` so moving `codec_family()` onto the
  `fallback = TRUE` arm displaces no type error, `families` is safely empty for
  a zero-row jobs table, and `@param fallback` already covers the case.

#### Findings and dispositions

**F1 (fix now, fixed).** The new family sweep runs over every family before the
availability loop, so on `fallback = FALSE` a multi-family `video_codec` list
whose first family is in-table-but-unbuilt and whose second is out-of-table now
reports the out-of-table pair where master reported the availability failure.
Verified by evaluating master's `check_hardware_available()` beside the
branch's: `list("libx264", "prores_ks")`, `hardware = "nvenc"`,
`fallback = FALSE`, empty build -- master `nvenc encoder "h264_nvenc" is not
available.`, branch `nvenc has no "prores" encoder.` The new precedence is the
one D075 wants (an argument outranks the machine), so the behaviour stands; what
was wrong was the claim. NEWS said "only the name in the error has changed",
which this falsifies. NEWS reworded to state the precedence change.

**F2 (fix now, fixed).** T2's replacement comment in `resolve_hw_encoder()`
(`R/ffmpeg.R:3279`) was false in both new claims -- the exact failure class T2
existed to fix. "Reached on the `fallback = FALSE` arm only" is wrong: with
`fallback = TRUE` and the encoder present the `&&` is FALSE and control falls
through (measured -- `resolve_hw_encoder("libx264", "nvenc", TRUE)` against a
build listing `h264_nvenc` returns `"h264_nvenc"` through that line). "The
predicate above ... refuses it there, on either arm" is wrong on the FALSE arm,
where `&&` short-circuits and the predicate never runs. Comment rewritten to
both routes, with the measurement beside it.

**F3 (fix now, fixed).** The refusal no longer carries `In index: N`, so a
batch says which codec and backend are wrong but not which row named them --
the reach of ROADMAP candidate M100 Out (e), widened here and unnoted. Fixed as
prose: NEWS now states it. The candidate row is extended at hygiene.

**F4 (fix now, fixed).** Rows earlier in a mixed table no longer print their
"falling back" messages before the abort, since nothing is built before the
refusal. Also a user-visible consequence riding on the "only the frame moved"
claim; NEWS now states it.

**F5 (follow-up).** AC1's sweep reaches `check_hardware_available()` only
through the scalar `video_codec`: no `tm_timeout_call_specs()` cell carries a
`video_codec` jobs column, so the `is.list()` list branch -- the column shape
D085 describes -- is bound by one hand-written cell in
`test-nvenc-front-door.R:250`. A regression sweeping `families[1]` would pass
84 of the 86 AC1 cells. Real coverage gap, not an AC failure (AC1's text names
a `video_codec`, not a path). Routed to a candidate row at hygiene.

**F6 (fix now, fixed).** `tm_hardware_encoder_pools()$nvenc` degrades to `NULL`
on a renamed row, which would make both pool levels answer "no encoders" and
AC1's discrimination vacuous with nothing red. Changed to `[[`, which errors.

**F7 (reject).** `nvenc_order_pools <- tm_hardware_encoder_pools()` is
evaluated at source time against the currently loaded namespace, so a
historical ref is mocked with today's table. True, but not introduced here: the
literal it replaced was equally a snapshot of the table at writing time, and no
worse. Rejected as a pre-existing property the diff did not introduce.

**F8 (fix now, fixed).** `expect_length(seam_pools(), 3L)` re-hard-coded the
count the derivation removed. Changed to
`length(hardware_backends()) + 1L`, which keeps the duplicate-name guard the
literal gave while a third backend no longer fails it spuriously.

**F9 (reject).** AC3 unticked while the work log recorded it measured. Stale --
the reviewer read the branch before this review's own AC3 tick, which is the
verification mark AC fencing requires and which this session wrote.

#### Fix-now re-verification

After F1/F2/F3/F4/F6/F8: `test-codec-seam-bound.R`, `test-hardware-out-of-table-blame.R`,
`test-nvenc-front-door.R` and `test-nvenc-probe-blame.R` all green;
`devtools::test()` 0 failures / 12,181 passes unchanged; `devtools::document()`
no diff; `tests/spelling.R` "All Done!" on the reworded NEWS. `devtools::check()`
re-run recorded below.

#### Return floor

No actioned finding demonstrates an acceptance criterion failing: AC1 and AC2
hold as written under every fix, and F1's behaviour change falls outside every
criterion's domain. F1/F3/F4 were defects in what the branch CLAIMED, not in
what it does, and the claims are corrected on the branch. Status stays `review`;
no defect return, no amendment return.

### PR conversation

Read once immediately before the merge gate: `pulls/111/reviews` empty,
`issues/111/comments` empty, and the `reviewThreads` query returns no
unresolved thread (`hasNextPage` false). Nothing to triage; the blocking rule
does not fire.
