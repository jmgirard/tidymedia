# M095: A wrong argument outranks the nvenc availability probe

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m095-argument-error-outranks-nvenc-probe` / https://github.com/jmgirard/tidymedia/pull/99

## Goal

`standardize_video(hardware = "nvenc", video_codec = "libx264", pixel_format = "bad fmt!")` is told the nvenc encoder is missing instead of that its pixel format is malformed; the machine decides which error the caller sees, which is what D036 removed at the front door and never reached the builder.

## Scope

Surface tier: **user-facing** — the condition three exported verbs raise changes.

**In:** the three pipelines that resolve the nvenc encoder above a machine-independent argument check — `standardize_pipeline()` (`R/ffmpeg.R:1802`, below the probe: `audio_codec`, `pixel_format`, `audio_stream`), `format_for_web_pipeline()` (`:1507`, `audio_stream`), `anonymize_pipeline()` (`:1984`, `audio_codec`, `audio_stream`). The fix hoists `apply_video_codec()`'s token check and sinks the encoder resolution below every such check — the shape `crop_video_pipeline()`, `segment_pipeline()`, `compare_videos_pipeline()` and `picture_in_picture_pipeline()` already have. The compiled command is invariant to builder call order (measured 2026-08-31: `ffm_groups()` emits by group, and moving `ffm_codec()` below `ffm_map()`/`ffm_pixel_format()` compiles byte-identically), so this moves blame and nothing else. Also in: the disclosure's own example is wrong — `video_codec` is the one argument of the three verbs that already reports first, and it is what `NEWS.md` and `?tidymedia` name.

**Out:** the per-row fan-out class (`segment_video()`'s `outfiles`, a `_batch` job table's `output` column) and `ffmpeg_codecs(sort_by_type =)` → M096; the gate-boolean class (`run`, `parallel`, …, refused by `ffm_finish()`/`ffm_batch()`) and `nvenc_available()`'s unassertable defaultless `call` → two candidate rows. Any change to what a call compiles to or to which calls are refused → forbidden by AC2, not deferred.

## Acceptance criteria

- [x] AC1 A sweep enumerates its own domain: every member of `tm_timeout_domain()` whose `formals()` carry `hardware`, crossed with each of that member's other formals, corrupted in turn by each of five wrong forms spanning the axes an argument is free in — a number, a token-invalid string, `NA`, a length-2 vector, a list. A cell is kept where its `hardware = "none"` form with no limit set refuses **from the member's own frame**; a cell refused from a frame below the member (`ffm_finish()`, `ffm_batch()`) or not refused at all is dropped by that measurement, never by a list, and named in the evidence. For every kept cell the condition raised under `hardware = "nvenc"` is identical — blamed frame and message — to the one under `hardware = "none"`, with `cached_encoder_names()` mocked to list `h264_nvenc`/`hevc_nvenc`/`av1_nvenc` and mocked to list none. Evidence: kept-cell and dropped-cell counts, each dropped formal named with the frame that refused it, 0 mismatches under both mocks.
- [x] AC2 The change refuses no call it compiled and compiles no call it refused, over every member that reaches one of the three reordered pipelines — the members `git grep -n` finds calling `standardize_pipeline`, `format_for_web_pipeline` or `anonymize_pipeline`, the `_batch` siblings included — crossed with `hardware` ∈ {`none`, `nvenc`}, `fallback` ∈ {`TRUE`, `FALSE`}, that member's valid-argument cell, and each AC1 kept cell, with `cached_encoder_names()` mocked both ways. Every cell compiling at the merge-base compiles byte-identically at HEAD, and the refused set is the same at both refs. Evidence: the recorded merge-base table, its cell count, and 0 differences (D035's second condition).
- [x] AC3 With `cached_encoder_names()` mocked to list the nvenc encoders and `options(tidymedia.timeout =)` set to each form `tm_timeout_bad_forms()` holds, each AC1 kept cell reports its own argument error, identical to that cell's condition with no limit set. Evidence: cell count and 0 mismatches. (This is the class D074 disclosed as unfixed and M094 review H1/H3 measured.)
- [x] AC4 A `cairn/DECISIONS.md` entry states the builder-order rule, its lineage to D036/D039 (front door → builder) and to D074 (whose property-1 falsifier names exactly these verbs), and its own falsifier.
- [x] AC5 Every tracked occurrence of the disclosure that `git grep -n` finds by searching for the exported names it must mention — `has_nvenc`, `nvenc`, `resolve_timeout` — across `NEWS.md`, `man/`, `R/` and `cairn/DECISIONS.md` either describes the build-time-probe class as fixed or does not describe it at all, and none offers `video_codec` as its example (measured 2026-08-31: `video_codec` is the one argument of the three verbs that already reports first, so the shipped example is wrong today). Evidence: the sweep's full output quoted before and after.
- [x] AC6 `cairn/PROFILE.md`'s verify slot clean: `devtools::test()` green and `devtools::check()` 0 errors / 0 warnings.

## Coverage

- AC1 → T1, T3
- AC2 → T2, T3
- AC3 → T4
- AC4 → T5
- AC5 → T5
- AC6 → T3, T5

## Tasks

- [x] T1 Build the sweep of AC1 in `tests/testthat/helper-timeout-sweep.R` and record the baseline it reports on master — the kept cells, the dropped formals, and which cells mismatch today. Instrument before fix: M094's three review rounds each found a new instance of this class by hand because `tm_timeout_corrupt_specs()` corrupts only `args[[1]]` and never crosses the variant table.
- [x] T2 Record the AC2 merge-base table (compiled command and refusal per cell) into `data-raw/`, following `data-raw/contradiction-guard-baseline.R`'s shape from M58.
- [x] T3 Reorder the three pipelines: hoist `apply_video_codec()`'s `check_token()` to where the probe now sits, sink the encoder resolution and `ffm_codec()` emission below the last machine-independent check. Re-run T1's sweep and T2's table.
- [x] T4 Add the AC3 leg — the same kept cells under each invalid limit form, with `cached_encoder_names()` mocked present, which is the only way the nvenc-available branch executes anywhere (the M094 lesson).
- [x] T5 Write the D-entry; correct `NEWS.md`, `?tidymedia` (`R/tidymedia-package.R:95-107`) and `R/timeout.R`'s siting comment to name only the per-row fan-out class; `devtools::document()`.

## Work log

- 2026-08-31: created by /milestone-plan.
- 2026-08-31: plan gate chose sinking the encoder resolution below the machine-independent checks (hoisting only `apply_video_codec()`'s token check) over hoisting those checks above the probe, because hoisting would move `pixel_format` ahead of the codec seams -- the precedence M41's and M64's reviews each caught moving -- while sinking leaves every machine-independent check in its current relative order; falsified by a compiled command that changes under the reorder (measured 2026-08-31 as invariant: `ffm_groups()` emits by group, not call order).
- 2026-08-31: [O] criteria audit ran (FULL mode, user-facing tier); findings and disposition recorded in M096's work log, which the same audit covered.
- 2026-08-31: T1 -- AC1's sweep built (`helper-timeout-sweep.R`) with its master baseline recorded and asserted (`test-nvenc-probe-blame.R`): 730 cells over the 16 `hardware`-carrying domain members, 496 kept, 234 dropped (each named with its refusing frame), and 27 kept cells whose `hardware = "nvenc"` condition differs from its `hardware = "none"` reference -- only under the absent-encoder mock, only in the three pipelines Scope names, and none of them on `video_codec`. Spawns are intercepted at `run_program()`/`guard_timeout()`, so no cell's answer depends on the runner's FFmpeg.
- 2026-08-31: T2 -- AC2's merge-base table recorded (`data-raw/nvenc-probe-order-baseline.R`, regenerating from any git ref via `codec-guard-baseline.R`'s machinery; the table itself at `data-raw/nvenc-probe-order-merge-base.rds`, measured at b538e63). Six members derived rather than listed -- every export whose body names one of the three pipelines -- over 231 cells crossed with `hardware`, `fallback` and the two mocked encoder pools: 1,848 rows, 203 compiled, 1,645 refused, 0 vacuous.
- 2026-08-31: T3 -- the three pipelines reordered. `apply_video_codec()` split into `check_video_codec()` + `emit_video_codec()` (gate choice), the token check left where the combined call sat and the encoder resolution sunk below the last machine-independent check in each of `standardize_pipeline()`, `format_for_web_pipeline()` and `anonymize_pipeline()`. AC1's sweep: 0 mismatches under both encoder pools, kept/dropped counts unmoved at 496/234. AC2 against b538e63: 1,848 rows both refs, 203 compiled and 1,645 refused on each, 0 contract differences -- every compiled command byte-identical and the refused set unchanged; the 27 rows the widest diff still reports are the argument errors replacing the availability abort, blamed frame unmoved. `devtools::test()` 0 failures / 11,347 passes.
- 2026-08-31: T4 -- AC3's leg added: the same 496 kept cells under each of the five `tm_timeout_bad_forms()` values, with `cached_encoder_names()` mocked to list the nvenc encoders, compared against each cell's no-limit `hardware = "none"` condition. 0 mismatches at HEAD; 27 per form measured against b538e63's `R/ffmpeg.R` in the same harness, the same 27 cells AC1 records, so the leg is shown able to see the displacement it reports absent. `devtools::test()` 0 failures / 11,358 passes.
- 2026-08-31: T5 -- D075 appended (the build-time probe runs below every check whose answer cannot depend on it; supersedes D074 property 1's disposition of that class, leaves D074's per-row fan-out class in force). AC5's sweep corrected at three sites -- the `NEWS.md` development-version bug-fix entry (in place, per the gate), `?tidymedia` (`R/tidymedia-package.R`) and `R/timeout.R`'s siting comment -- and `man/tidymedia-package.Rd` regenerated. `devtools::document()` no diff; `devtools::test()` 0 failures / 11,358 passes; `devtools::check()` 0 errors / 0 warnings / 0 notes.
- 2026-08-31: AC5 disposition for its one remaining hit. `cairn/DECISIONS.md:3415-3416` still describes the build-time-probe class as disclosed-not-fixed, and is left unedited: DECISIONS.md is append-only history (IP4), so the criterion is met there by supersession -- D075 names the clause it supersedes, and the bounded-DECISIONS read back-references D074 to it. No amendment taken; recorded here so review judges the reading rather than inferring it.
- 2026-08-31: all five tasks done; `devtools::test()` 0 failures / 11,358 passes and `devtools::check()` 0 errors / 0 warnings / 0 notes; status to review.
- 2026-08-31: review -- PR #99 opened; all six criteria verified with fresh evidence (counts in the Review section); consistency gate clean; three fresh-context lenses, eight findings, five fixed at the gate (the overclaiming disclosure, the undisclosed `fallback` descent, two instrument gaps, one stale comment), two filed to a candidate row, one rejected; no finding met the return floor.

## Review

Reviewed 2026-08-31. PR https://github.com/jmgirard/tidymedia/pull/99 (draft at
review start). `origin/master` had not moved since the branch was cut, so no
merge was needed: `git merge-base origin/master HEAD` is `b538e63`, which is
`origin/master`'s own tip, and local `master` was level with it.

### Acceptance-criteria evidence (fresh, by command)

- **AC1 — verified.** `tm_nvenc_wrong_arg_cells()` produced 730 cells over the
  16 `hardware`-carrying members of `tm_timeout_domain()`; 496 kept, 234
  dropped, the dropped set reducing to 78 distinct `member/arg -> frame` pairs
  and matching `tm_nvenc_dropped_master()` element for element. The refusing
  frames are `<none>`, `ffm_finish`, `ffm_batch`, `if` and `purrr::pmap` — every
  drop by measurement, none by a list. 0 kept-cell mismatches under the present
  pool and 0 under the absent pool. Discrimination shown against the merge-base
  in the same harness: `b538e63` gives 0 mismatches under the present pool and
  27 under the absent pool, and that 27-cell set is identical to
  `tm_nvenc_mismatch_master()`.
- **AC2 — verified.** `nvenc-probe-order-baseline.R` regenerated at `b538e63`
  and at the working tree: 1,848 rows each over 231 cells and 6 derived members,
  203 compiled and 1,645 refused at both refs, 0 vacuous `valid` cells at both.
  `nvenc_order_contract_diff()` returns 0 rows — every compiled command
  byte-identical, the refused set unchanged. The recorded
  `nvenc-probe-order-merge-base.rds` regenerates identically from `b538e63`.
  The wide diff returns exactly 27 rows, all under the absent pool at
  `hardware = "nvenc"`, every one an availability abort replaced by the caller's
  own argument error with the blamed frame unmoved.
- **AC3 — verified.** The 496 kept cells under each of the five
  `tm_timeout_bad_forms()` values, encoders mocked present: 496 kept and 0
  mismatches for every form. Discrimination against `b538e63` in the same
  harness: 27 mismatching kept cells under every one of the five forms.
- **AC4 — verified.** D075 states the rule (a build-time capability probe runs
  below every check that cannot depend on its answer), its lineage to D036/D039
  and to D074 property 1, the two consequences and the one cost, its Scope, and
  its falsifier. Its heading names the clause it supersedes.
- **AC5 — verified, with one disposition recorded below.** The sweep returns 525
  hits. Three sites describe the build-time-probe class and now describe it as
  fixed: `NEWS.md`'s development-version entry, `?tidymedia`
  (`R/tidymedia-package.R`) and its generated `man/tidymedia-package.Rd`. None
  offers `video_codec` as the displaced example. The 16 per-verb
  `@param hardware` blurbs are about caching and the dry-run probe and describe
  no ordering, so they are out of the criterion's predicate. The one remaining
  hit that describes the class as unfixed is `cairn/DECISIONS.md:3415` — D074
  property 1, append-only history under IP4, corrected by supersession rather
  than by edit; D075's heading names the superseded clause, and the repo's own
  precedent for this is D050 superseding D047's "Disclosed, not fixed" bullet
  (M071). The three sites were re-swept after the gate fixes below.
- **AC6 — verified after the gate fixes.** `devtools::test()` 0 failures /
  11,362 passes; `devtools::check()` 0 errors / 0 warnings / 0 notes.

### Consistency gate

`cairn_validate.py` exit 0, all 16 checks PASS and all 7 advisories OK. No
DESIGN.md principle changed, so `cairn_impact.py` was not run. Toolchain slot:
`devtools::document()` produces no diff; `NAMESPACE`, `_pkgdown.yml` and
`README.Rmd` untouched (no new exports — `check_video_codec()` and
`emit_video_codec()` are internal); `pkgdown::check_pkgdown()` no problems;
`NEWS.md` carries the user-visible change in the unreleased development section;
no new top-level files (`data-raw/` is already `.Rbuildignore`d);
`devtools::check()` clean.

One tracking defect found and NOT introduced by this milestone: `ROADMAP.md` is
26,852 bytes against a 24,000-byte budget. The M094 hygiene stamp recorded
25,627 bytes as "inside budget", which was already over. Remedied in this
milestone's hygiene pass.

### Independent review

Three fresh-context lenses, distinct evidence bases, none having seen the
implementation. [S] blame-history: no defects — the M41/M64 precedence traps are
preserved by construction (the token check does not move), and the `fallback`
descent is named in D075 rather than silent. [S] prior-review record: no
regressions; the probe of GitHub inline review comments returned empty, so that
surface was skipped, and the archived `## Review` findings on the touched files
(M56 F3, M57, M67, M61/M64) are all consistent with the diff. [O] diff-bug:
eight findings, ranked.

**Findings and disposition.** Four fixed on the branch before the gate, four
otherwise disposed.

- **F1 (fixed).** "The user-facing disclosure now claims more than the code
  delivers." `NEWS.md`, `R/tidymedia-package.R` and `man/tidymedia-package.Rd`
  said, unqualified, that the encoder question "now happens after every check
  whose answer cannot depend on it" and that a bad `audio_codec`,
  `pixel_format` or `audio_stream` reports as itself "whether or not that build
  has nvenc". Reproduced: `anonymize_video_batch()` with a valid `regions`, a
  malformed `pixel_format` and `hardware = "nvenc"` on a build listing no nvenc
  reports the availability abort, where `hardware = "none"` reports the
  `pixel_format` error from `purrr::pmap`. The claim is true of every check the
  verb makes itself and false of checks the per-row fan-out makes. The three
  sites were rewritten to say that, naming `segment_video()`'s `outfiles`, a
  `_batch` job table's `output` column, and `anonymize_video_batch()`'s
  `pixel_format` and `color`; both halves of the replacement claim — the limit
  and the encoder question — were measured before being written.
- **F2 (fixed).** "A real precedence change master→HEAD that neither instrument
  measures and no user-facing text mentions." `resolve_hw_encoder()` carries
  `fallback`'s `check_bool()`, so sinking the resolution sinks that check too.
  Reproduced: `standardize_video(..., pixel_format = "bad fmt!", fallback = NA,
  hardware = "none")` reports the `fallback` error at `b538e63` and the
  `pixel_format` error at HEAD — a `hardware = "none"` path. The refusal set is
  intact (`fallback = NA` alone is still refused). Disclosed only in a code
  comment and D075's "one it costs" on a user-facing-tier milestone; a sentence
  was added to the same `NEWS.md` and `?tidymedia` paragraph.
- **F3 (fixed).** "`tm_nvenc_mismatch_master()` is decorative." The recorded
  27-cell table was asserted only by `expect_gt(length(...), 0)`, which no
  edit to it could fail. It is now bound: length 27, every name a cell the sweep
  drives, every one kept under the absent pool, and the members exactly the
  three reordered pipelines.
- **F5 (fixed).** "The dropped-cell assertion is de-duplicated, so a
  kept→dropped regression can hide." The comparison ran over `unique()` strings,
  and the 496/234 counts lived only in the work log. `sum(sweep$kept)` and
  `sum(!sweep$kept)` are now asserted.
- **F7 (fixed).** "Stale sentence left in the standardize comment."
  `R/ffmpeg.R` still described the abort as firing after `ffm_scale()`'s
  dimension checks, three checks short of where it now sits; the sentence was
  put in the past tense and points at the M095 paragraph that supersedes it.
- **F4 (follow-up).** "AC2's grid never exercises the `NULL` codec sentinel."
  Verified by the reviewer by hand across pool × fallback as byte-identical at
  both refs, so a coverage gap and not a defect. Filed to a candidate row.
- **F6 (follow-up).** "The new seam has no invariant binding its halves."
  Calling `emit_video_codec()` without `check_video_codec()` above it would
  reintroduce M56's hole; all three sites are correct today and nothing pins
  that. Filed to the same candidate row.
- **F8 (rejected, with reason).** The reviewer agreed with the AC5 disposition
  above and noted that a reader landing on D074 alone gets a stale statement.
  No forward pointer can be added: `DECISIONS.md` is append-only under IP4, and
  the bounded-`DECISIONS` read's back-reference step (search the matched entry's
  own id across the file) is the mechanism that surfaces D075. The nit about the
  uncommitted milestone-file edit was the PR URL being written, and is
  committed here.

No finding met the return floor: none demonstrated an acceptance criterion
failing, and the two user-visible ones (F1, F2) were prose defects repaired on
the branch rather than defects in what the verbs do.
