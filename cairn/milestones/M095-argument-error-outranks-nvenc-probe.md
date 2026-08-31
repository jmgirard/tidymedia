# M095: A wrong argument outranks the nvenc availability probe

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m095-argument-error-outranks-nvenc-probe`

## Goal

`standardize_video(hardware = "nvenc", video_codec = "libx264", pixel_format = "bad fmt!")` is told the nvenc encoder is missing instead of that its pixel format is malformed; the machine decides which error the caller sees, which is what D036 removed at the front door and never reached the builder.

## Scope

Surface tier: **user-facing** — the condition three exported verbs raise changes.

**In:** the three pipelines that resolve the nvenc encoder above a machine-independent argument check — `standardize_pipeline()` (`R/ffmpeg.R:1802`, below the probe: `audio_codec`, `pixel_format`, `audio_stream`), `format_for_web_pipeline()` (`:1507`, `audio_stream`), `anonymize_pipeline()` (`:1984`, `audio_codec`, `audio_stream`). The fix hoists `apply_video_codec()`'s token check and sinks the encoder resolution below every such check — the shape `crop_video_pipeline()`, `segment_pipeline()`, `compare_videos_pipeline()` and `picture_in_picture_pipeline()` already have. The compiled command is invariant to builder call order (measured 2026-08-31: `ffm_groups()` emits by group, and moving `ffm_codec()` below `ffm_map()`/`ffm_pixel_format()` compiles byte-identically), so this moves blame and nothing else. Also in: the disclosure's own example is wrong — `video_codec` is the one argument of the three verbs that already reports first, and it is what `NEWS.md` and `?tidymedia` name.

**Out:** the per-row fan-out class (`segment_video()`'s `outfiles`, a `_batch` job table's `output` column) and `ffmpeg_codecs(sort_by_type =)` → M096; the gate-boolean class (`run`, `parallel`, …, refused by `ffm_finish()`/`ffm_batch()`) and `nvenc_available()`'s unassertable defaultless `call` → two candidate rows. Any change to what a call compiles to or to which calls are refused → forbidden by AC2, not deferred.

## Acceptance criteria

- [ ] AC1 A sweep enumerates its own domain: every member of `tm_timeout_domain()` whose `formals()` carry `hardware`, crossed with each of that member's other formals, corrupted in turn by each of five wrong forms spanning the axes an argument is free in — a number, a token-invalid string, `NA`, a length-2 vector, a list. A cell is kept where its `hardware = "none"` form with no limit set refuses **from the member's own frame**; a cell refused from a frame below the member (`ffm_finish()`, `ffm_batch()`) or not refused at all is dropped by that measurement, never by a list, and named in the evidence. For every kept cell the condition raised under `hardware = "nvenc"` is identical — blamed frame and message — to the one under `hardware = "none"`, with `cached_encoder_names()` mocked to list `h264_nvenc`/`hevc_nvenc`/`av1_nvenc` and mocked to list none. Evidence: kept-cell and dropped-cell counts, each dropped formal named with the frame that refused it, 0 mismatches under both mocks.
- [ ] AC2 The change refuses no call it compiled and compiles no call it refused, over every member that reaches one of the three reordered pipelines — the members `git grep -n` finds calling `standardize_pipeline`, `format_for_web_pipeline` or `anonymize_pipeline`, the `_batch` siblings included — crossed with `hardware` ∈ {`none`, `nvenc`}, `fallback` ∈ {`TRUE`, `FALSE`}, that member's valid-argument cell, and each AC1 kept cell, with `cached_encoder_names()` mocked both ways. Every cell compiling at the merge-base compiles byte-identically at HEAD, and the refused set is the same at both refs. Evidence: the recorded merge-base table, its cell count, and 0 differences (D035's second condition).
- [ ] AC3 With `cached_encoder_names()` mocked to list the nvenc encoders and `options(tidymedia.timeout =)` set to each form `tm_timeout_bad_forms()` holds, each AC1 kept cell reports its own argument error, identical to that cell's condition with no limit set. Evidence: cell count and 0 mismatches. (This is the class D074 disclosed as unfixed and M094 review H1/H3 measured.)
- [ ] AC4 A `cairn/DECISIONS.md` entry states the builder-order rule, its lineage to D036/D039 (front door → builder) and to D074 (whose property-1 falsifier names exactly these verbs), and its own falsifier.
- [ ] AC5 Every tracked occurrence of the disclosure that `git grep -n` finds by searching for the exported names it must mention — `has_nvenc`, `nvenc`, `resolve_timeout` — across `NEWS.md`, `man/`, `R/` and `cairn/DECISIONS.md` either describes the build-time-probe class as fixed or does not describe it at all, and none offers `video_codec` as its example (measured 2026-08-31: `video_codec` is the one argument of the three verbs that already reports first, so the shipped example is wrong today). Evidence: the sweep's full output quoted before and after.
- [ ] AC6 `cairn/PROFILE.md`'s verify slot clean: `devtools::test()` green and `devtools::check()` 0 errors / 0 warnings.

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
- [ ] T4 Add the AC3 leg — the same kept cells under each invalid limit form, with `cached_encoder_names()` mocked present, which is the only way the nvenc-available branch executes anywhere (the M094 lesson).
- [ ] T5 Write the D-entry; correct `NEWS.md`, `?tidymedia` (`R/tidymedia-package.R:95-107`) and `R/timeout.R`'s siting comment to name only the per-row fan-out class; `devtools::document()`.

## Work log

- 2026-08-31: created by /milestone-plan.
- 2026-08-31: plan gate chose sinking the encoder resolution below the machine-independent checks (hoisting only `apply_video_codec()`'s token check) over hoisting those checks above the probe, because hoisting would move `pixel_format` ahead of the codec seams -- the precedence M41's and M64's reviews each caught moving -- while sinking leaves every machine-independent check in its current relative order; falsified by a compiled command that changes under the reorder (measured 2026-08-31 as invariant: `ffm_groups()` emits by group, not call order).
- 2026-08-31: [O] criteria audit ran (FULL mode, user-facing tier); findings and disposition recorded in M096's work log, which the same audit covered.
- 2026-08-31: T1 -- AC1's sweep built (`helper-timeout-sweep.R`) with its master baseline recorded and asserted (`test-nvenc-probe-blame.R`): 730 cells over the 16 `hardware`-carrying domain members, 496 kept, 234 dropped (each named with its refusing frame), and 27 kept cells whose `hardware = "nvenc"` condition differs from its `hardware = "none"` reference -- only under the absent-encoder mock, only in the three pipelines Scope names, and none of them on `video_codec`. Spawns are intercepted at `run_program()`/`guard_timeout()`, so no cell's answer depends on the runner's FFmpeg.
- 2026-08-31: T2 -- AC2's merge-base table recorded (`data-raw/nvenc-probe-order-baseline.R`, regenerating from any git ref via `codec-guard-baseline.R`'s machinery; the table itself at `data-raw/nvenc-probe-order-merge-base.rds`, measured at b538e63). Six members derived rather than listed -- every export whose body names one of the three pipelines -- over 231 cells crossed with `hardware`, `fallback` and the two mocked encoder pools: 1,848 rows, 203 compiled, 1,645 refused, 0 vacuous.
- 2026-08-31: T3 -- the three pipelines reordered. `apply_video_codec()` split into `check_video_codec()` + `emit_video_codec()` (gate choice), the token check left where the combined call sat and the encoder resolution sunk below the last machine-independent check in each of `standardize_pipeline()`, `format_for_web_pipeline()` and `anonymize_pipeline()`. AC1's sweep: 0 mismatches under both encoder pools, kept/dropped counts unmoved at 496/234. AC2 against b538e63: 1,848 rows both refs, 203 compiled and 1,645 refused on each, 0 contract differences -- every compiled command byte-identical and the refused set unchanged; the 27 rows the widest diff still reports are the argument errors replacing the availability abort, blamed frame unmoved. `devtools::test()` 0 failures / 11,347 passes.
