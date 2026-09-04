# M106: The codec seam's halves cannot be called apart, and the probe grid exercises the codec sentinel

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Resolves:** —
- **Surface tier:** user-facing — it changes package runtime code in `R/` that decides which refusal a caller sees
- **Branch/PR:** `m106-codec-seam-bound-sentinel-covered`

## Goal

Make `emit_video_codec()` validate its own token before it asks the FFmpeg build anything, so no future pipeline can emit a codec it never checked, and cross the `video_codec = NULL` sentinel into the probe-order grid so that promise is measured over the sentinel too.

## Scope

**In:** the token check moves inside `emit_video_codec()` (`R/ffmpeg.R:3384`); a committed ledger of every namespace function naming `resolve_hw_encoder()`, with each site's disposition; `video_codec` becomes a crossed dimension of `data-raw/nvenc-probe-order-baseline.R`; suite tests pinning the sentinel's `hardware = "nvenc"` behaviour, which nothing asserts today.

**Out:**
- Rerouting the two pipelines that call `resolve_hw_encoder()` directly — `format_for_web_pipeline()` (`R/ffmpeg.R:1541`) and `anonymize_pipeline()` (`:2126`) — through the seam. Declined at this gate: it moves where the codec is emitted relative to the surrounding filter calls, which is the one thing the grid exists to hold fixed. → candidate row.
- A standing static sweep asserting every emit site also checks. The construction fix removes the shape it would guard. → not planned; the reasoning is in the work log.
- Extending the grid's mocked encoder pools past nvenc to videotoolbox. → candidate row.

## Acceptance criteria

- [ ] AC1: `emit_video_codec()` (`R/ffmpeg.R:3384`) calls `check_video_codec()` on its own `video_codec` above its `resolve_hw_encoder()` call, so the seam's two halves cannot be called apart. A test calling `emit_video_codec()` directly, with no `check_video_codec()` above it, refuses each of `tm_nvenc_wrong_forms()`'s five wrong forms under `hardware = "nvenc"` and under `hardware = "videotoolbox"`, on both mocked encoder pools, with the token refusal rather than an availability abort; a `cached_encoder_names()` mock recording its own calls records none in any of those cells.
- [ ] AC2: Every namespace function whose own body names `resolve_hw_encoder` — the domain computed by `all.names(body(f))` over `asNamespace("tidymedia")`, the mechanism `tm_symbol_graph()` uses (`tests/testthat/helper-timeout-sweep.R:52`) — carries a stated disposition in a committed ledger: the codec it passes is a literal, or its token is checked above the call, or it is the emit half itself. A test recomputes that domain and fails on a member the ledger does not name.
- [ ] AC3: Over every cell `nvenc_order_baseline()` enumerates — the grid now crossing `video_codec` over the caller's value and the `NULL` sentinel on every member carrying that formal — `nvenc_order_contract_diff(before, after)` returns zero rows between the merge-base and the branch head, and `nvenc_order_vacuous()` is empty at both refs. `nvenc_order_align()` completes rather than stopping, and the two refs cover the same cell count.
- [ ] AC4: The sentinel's hardware behaviour is pinned by the suite. With `cached_encoder_names()` mocked to list no nvenc encoder, `standardize_video(video_codec = NULL, hardware = "nvenc", fallback = TRUE, run = FALSE)` compiles a command containing no `-codec:v` and emits the container-default-encoder message; the same call at `fallback = FALSE` aborts naming `h264_nvenc`; with the mock listing `h264_nvenc`, it compiles `-codec:v h264_nvenc`.
- [ ] AC5: The profile's verify slot is clean: `devtools::test()` green and `devtools::check()` at 0 errors / 0 warnings / 0 notes.

## Coverage

- AC1 → T3, T4
- AC2 → T5
- AC3 → T1, T2, T7
- AC4 → T6
- AC5 → T7

## Tasks

- [x] T1: Widen the grid in `data-raw/nvenc-probe-order-baseline.R` so `video_codec` is a crossed dimension beside `hardware`, `fallback` and `pool`: replace the `base$video_codec <- "libx264"` overwrite (`:132`) with the cross, add `video_codec` to `nvenc_order_cells()`'s excluded formals (`:137`) so a wrong-form cell cannot overwrite the crossed value, add the new column to the row frame and to `nvenc_order_align()`'s key (`:294`), and skip the cross for a member with no `video_codec` formal (`format_for_web()`, `format_for_web_batch()`). Pass the sentinel as `args["video_codec"] <- list(NULL)` — assigning `NULL` to a list element deletes it, which would silently run the member's own default instead.
- [x] T2: With the widened grid and `R/` untouched, record the merge-base baseline: `before <- nvenc_order_baseline("<merge-base sha>")`. Confirm `nvenc_order_vacuous(before)` is empty and note the row count and the sha in the work log.
- [x] T3: Move the token check into `emit_video_codec()` (`R/ffmpeg.R:3384`), above `resolve_hw_encoder()`. Leave `apply_video_codec()`'s own `check_video_codec()` call (`:3358`) in place: it is idempotent and it holds the reporting position `standardize_pipeline()` depends on. Rewrite the seam's comment block (`:3362`–`:3379`) against the changed code, not from recollection.
- [x] T4: Tests for AC1 in `tests/testthat/`: `emit_video_codec()` called alone over the five wrong forms × both backends × both mocked pools, asserting the token refusal by condition class and message; a recording `cached_encoder_names()` mock asserted to have zero calls in every cell. Show the test reddens with the check removed from the emit half (planted-defect control) and log that run.
- [x] T5: Build the `resolve_hw_encoder()` call-site ledger for AC2 — compute the domain by `all.names(body(f))` over the namespace, state each member's disposition in one table, and commit the recomputing test that fails on an unnamed member.
- [x] T6: Sentinel tests for AC4 against `standardize_video()` at `run = FALSE`, reading the compiled bytes for the absent and present `-codec:v`, and asserting the fallback message and the availability abort by class and text.
- [x] T7: Re-run the widened grid against the branch head, take `nvenc_order_contract_diff(before, after)` and `nvenc_order_vacuous(after)`, record the row counts and both refs; then the profile's verify slot and `devtools::check()`.

## Work log

- 2026-09-03: created by /milestone-plan; absorbs the ROADMAP candidate row for M095 review F4/F6 (the seam's unbound halves and the grid's overwritten `video_codec`). Its cited line numbers were stale — the seam is at `R/ffmpeg.R:3375`/`:3384`.
- 2026-09-03: plan gate chose binding the seam by construction over a static sweep asserting every emit site also checks, because the construction fix removes the shape the sweep would guard rather than watching for it; falsified by a site that must emit without checking (a caller holding a token already validated in a frame the seam cannot see, where the second check would change the reported error).
- 2026-09-03: plan gate chose crossing `video_codec` over every grid cell over adding the sentinel to each member's valid cell alone, for the reason the file already gives for crossing `fallback` — a partial cross leaves one arm unprobed; falsified by the widened grid's runtime making the before/after run impractical.
- 2026-09-03: plan gate chose enumerating and asserting the two direct `resolve_hw_encoder()` sites over rerouting them through the seam, because rerouting moves the emission point relative to the surrounding filter calls and risks the compiled bytes the grid holds fixed; falsified by a third direct site appearing whose token is neither literal nor checked above.
- 2026-09-03: question gate: the `resolve_hw_encoder()` call-site ledger gets its own `tests/testthat/helper-hw-encoder-ledger.R` + `test-hw-encoder-ledger.R` pair rather than folding into the 1,400-line timeout helper; and its test verifies each site's stated disposition against the site's own body, not only that the site is named — a strengthening past AC2's text, which asks only that an unnamed member fail.
- 2026-09-03: T1 done. `video_codec` is a fourth crossed dimension of `data-raw/nvenc-probe-order-baseline.R` (`caller` = "libx264", `sentinel` = NULL, `absent` for a member without the formal); it leaves `nvenc_order_cells()`'s wrong-form set, which drops that argument's 5 wrong-form cells per member. Working-tree run: 3,040 rows (was 1,848), 19.6 s, `nvenc_order_vacuous()` empty. Suite green before the change: 0 failures, 12,036 passes, 18 skips.
- 2026-09-03: T2 done. Merge-base baseline recorded at `9b7fbe4` with the widened grid and `R/` untouched: 3,040 rows over 6 members (`anonymize_video`, `anonymize_video_batch`, `format_for_web`, `format_for_web_batch`, `standardize_video`, `standardize_video_batch`), 1,352 rows each on the `caller` and `sentinel` arms and 336 `absent`; `nvenc_order_vacuous(before)` empty; 28 s. No code changed, so the verify slot's result from T1 stands.
- 2026-09-03: T3 done. `emit_video_codec()` calls `check_video_codec()` above its `resolve_hw_encoder()` call (`R/ffmpeg.R:3402`); `apply_video_codec()`'s own call stays, now idempotent with it. The seam's comment block was rewritten against the changed code. Verify slot clean: 0 failures, 12,036 passes, 18 skips.
- 2026-09-03: T4 done. `tests/testthat/test-codec-seam-bound.R`: 5 wrong forms x 2 backends x 2 mocked pools, each asserted on its own recorded refusal message (four are `check_string()`'s type refusal, one `check_token()`'s shape refusal; all five name `video_codec`), with a recording `cached_encoder_names()` mock at 0 calls in all 20 cells; plus a discrimination block where a valid `libx264` in those same cells does reach the mock and does get the availability abort. Planted-defect control: with the check removed from the emit half the first cell reddens on `Cannot use hardware with video_codec = 123 / No hardware encoder family maps to that codec` from `codec_family()` — not the token refusal — and `R/ffmpeg.R` was restored (`git diff` empty) before the suite run. Verify slot clean: 0 failures, 12,084 passes (48 new), 18 skips.
- 2026-09-03: T5 done. `tests/testthat/helper-hw-encoder-ledger.R` + `test-hw-encoder-ledger.R`. Domain recomputed by `all.names(body(f))` over the namespace: `format_for_web_pipeline` (literal `"libx264"`), `anonymize_pipeline` (checked above by `check_token(video_codec, allow_null = TRUE)`, not `check_video_codec()`), `emit_video_codec` (the emit half). Per the question gate the test also verifies each disposition against the site's own parsed body in source order, not only that the site is named. Two planted-defect controls, both red and both restored (`git diff` empty): an unlisted site added to `R/` fails the set comparison; the check removed from the emit half fails with `emit_video_codec: video_codec is not checked by check_video_codec above the resolver`. Verify slot clean: 0 failures, 12,094 passes (10 new), 18 skips.
- 2026-09-04: T6 done. `tests/testthat/test-codec-sentinel-hardware.R` pins all three sentinel behaviours against `standardize_video(run = FALSE)` with a mocked `cached_encoder_names()` and `check_tracks` off, so no cell needs a binary: `fallback = TRUE` on an empty pool emits no `-codec:v` and messages `falling back to the output container's default video encoder`; `fallback = FALSE` aborts naming `h264_nvenc` and `is not available`; a pool listing the encoder compiles `-codec:v h264_nvenc`. A fourth test holds `hardware = "none"` silent on the listing pool. Planted-defect control: making the sentinel's fallback return `"libx264"` reddens the first test on the `-codec:v` absence; `R/ffmpeg.R` restored (`git diff` empty). Verify slot clean: 0 failures, 12,101 passes (7 new), 18 skips.
- 2026-09-04: T7 done. Widened grid re-run over both refs in one session: `before` at `9b7fbe4` and `after` at the branch head, 3,040 rows each, identical member lists, `nvenc_order_align()` completed. `nvenc_order_contract_diff(before, after)` 0 rows; `nvenc_order_vacuous()` 0 rows at both refs; `nvenc_order_diff()` 0 rows too — the seam change moves no message at any grid cell, because the only member reaching `emit_video_codec()` is `standardize_pipeline()`, which already checked above it. The grid's discrimination on this milestone therefore comes from the widening, shown separately: with the sentinel's fallback planted to return `"libx264"`, the contract diff is 24 rows, every one on the `sentinel` arm (`standardize_video` 9, `standardize_video_batch` 8, `anonymize_video` 4, `anonymize_video_batch` 3) — 24 rows the pinned grid could not have seen. `R/ffmpeg.R` restored (`git diff` empty). `devtools::document()` produced no diff. `devtools::check()`: 0 errors, 0 warnings, 0 notes (8m 4s). No NEWS.md entry: no shipped call path changes behaviour, which the 0-row wide diff is the measurement of.
- 2026-09-03: criteria audit ran in full mode ([O] fresh-context reader, user-facing tier). Six findings, all fixed at the gate: AC1's universal quantified over callers of `resolve_hw_encoder()` that the emit half does not cover (became AC2's computed ledger); AC1's single malformed token stood in for the five-form family and one backend for two (probe widened); the grid widening broke `nvenc_order_align()`'s key and would have been overwritten by the wrong-form cells (stated operationally in T1, with the alignment precondition in AC3); a "recorded in the Review section" clause bound the record of verification rather than the deliverable (dropped to the review procedure); AC4 named no entry point (now `standardize_video()`). No principle conflict; proportionality passed on all.

## Decisions

## Review
