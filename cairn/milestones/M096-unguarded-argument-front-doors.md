# M096: A wrong argument is refused by the verb, not by `purrr::pmap()` or after FFmpeg runs

- **Status:** planned
- **Priority:** normal
- **Depends on:** M095
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

`segment_video(outfiles = 123)` is reported by `purrr::pmap()` and `ffmpeg_codecs(sort_by_type = "yes")` spawns FFmpeg before failing on an `if` — two arguments no front door guards, and the instrument that would have found them corrupts only each member's first argument.

## Scope

Surface tier: **user-facing** — the condition two exported verbs raise changes, and one of them stops spawning a binary on a call it should refuse.

**In:** widening `tm_timeout_corrupt_specs()` (`tests/testthat/helper-timeout-sweep.R:859`) from `args[[1]]` to every formal, and fixing what that returns. Three known instances: `segment_video()`'s `outfiles` has only a length check (`R/ffmpeg.R:3510`) and reaches `ffm_files()` inside the fan-out; `ffmpeg_codecs()` has no `sort_by_type` guard at all where `ffmpeg_encoders()` has `rlang::check_bool()` (`R/ffmpeg.R:2875`), so it alone of the pair spawns FFmpeg and then errors from `if`.

**Out:** the gate-boolean class — `run`, `parallel`, `progress`, `manifest`, `checksums`, `verify` are refused by `ffm_finish()`/`ffm_batch()` rather than by the verb, and D074 property 2 puts `resolve_timeout()` above the run gate by decision, so a bad limit outranks a bad `run` by design; AC1 drops those cells by measurement and names them, and a candidate row carries the class. `nvenc_available()`'s defaultless `call` (`R/ffmpeg.R:3004`) — measured 2026-08-31: R never forces the promise unless an abort is built, so a site that omits `call` returns silently under a valid limit and under a set `tidymedia.nvenc_encoders` alike, and no test can assert the missing-argument error M094's comment claims. A candidate row records it. `ffm_batch()`'s `output` column — `ffm_batch()`'s contract names no column, so guarding one would invent a contract; it stays disclosed and a candidate row carries the contract change. The build-time nvenc probe class → M095. Any front-door guard that would refuse a call the pipeline compiles today → forbidden by AC4.

## Acceptance criteria

- [ ] AC1 The corrupt-argument sweep runs over every formal of every member of `tm_timeout_domain()`, not the first alone, each corrupted in turn by the five wrong forms M095's sweep uses (a number, a token-invalid string, `NA`, a length-2 vector, a list). A cell is kept where the member's no-limit reference refuses **from the member's own frame**, and dropped by that measurement — never by a list — where the refusal comes from a frame below the member or does not come; every dropped formal is named in the evidence with the frame that refused it. Under each form in `tm_timeout_bad_forms()` the condition — blamed frame and message — is identical to that cell's no-limit reference. Evidence: kept-cell and dropped-cell counts, the dropped formals and frames named, 0 mismatches.
- [ ] AC2 `segment_video()` refuses every `outfiles` value that reaches `ffm_files()` inside the fan-out today — each of the five wrong forms above, and a character vector holding `NA` — from `segment_video`'s own frame, above its `resolve_timeout()` call (`R/ffmpeg.R:3555`) and above `ffm_batch()`. Evidence: a test asserting, per form, that the blamed frame is `segment_video` and matches neither `pmap` nor `In index:`.
- [ ] AC3 `ffmpeg_codecs(sort_by_type =)` gives, for each of the five wrong forms above, the message `ffmpeg_encoders()` gives for that same form, and spawns nothing on any of them. Evidence: a test comparing the two messages string-for-string per form and pinning the spawn count at 0.
- [ ] AC4 The change refuses no call either verb compiled before: over `segment_video()`'s valid-argument cell × `outfiles` ∈ {`NULL`, a supplied character vector} and `ffmpeg_codecs()` × `sort_by_type` ∈ {`TRUE`, `FALSE`}, every cell succeeding at the merge-base succeeds at HEAD with a byte-identical compiled command (`segment_video(run = FALSE)`) or an identical return value (`ffmpeg_codecs()`). Evidence: the recorded merge-base table and 0 differences.
- [ ] AC5 `cairn/PROFILE.md`'s verify slot clean: `devtools::test()` green and `devtools::check()` 0 errors / 0 warnings.

## Coverage

- AC1 → T1, T3
- AC2 → T3
- AC3 → T3
- AC4 → T2, T3
- AC5 → T3, T4

## Tasks

- [ ] T1 Widen `tm_timeout_corrupt_specs()` to the member × formal cross-product and record what it reports on master: kept cells, dropped formals, and every mismatch. The three named in Scope are what a hand read found; the sweep decides the rest.
- [ ] T2 Record the AC4 merge-base table for the members T1 reports as mismatching.
- [ ] T3 Add the missing front-door guards — `outfiles` on `segment_video()` above its `resolve_timeout()`, `rlang::check_bool(sort_by_type)` in `ffmpeg_codecs()` matching `ffmpeg_encoders()`, and every other mismatch T1 returned — then re-run T1's sweep and T2's table. AC1's 0-mismatch promise covers what the sweep returns, not only the two named: where T1's count makes a task exceed one working session, raise it through the amendment protocol rather than exempting cells (plan gate, 2026-08-31).
- [ ] T4 `NEWS.md` entries for the two changed refusals; re-disclose `ffm_batch()`'s `output` column as the residual in `?tidymedia` and `NEWS.md`; add the candidate row for the `ffm_batch()` contract change; `devtools::document()`.

## Work log

- 2026-08-31: created by /milestone-plan.
- 2026-08-31: plan gate chose widening the corrupt-argument sweep to member x formal x five wrong forms over M094's one-value-on-args[[1]] instrument because the narrow form let the same class through three review rounds; falsified by a defect of this class that a formal-level sweep cannot express.
- 2026-08-31: plan gate chose leaving `ffm_batch()`'s `output` column disclosed over guarding it because `ffm_batch()`'s contract names no column and a guard would invent one; falsified by a caller reporting the pmap blame on a batch `output` value.
- 2026-08-31: [O] criteria audit ran (FULL mode, user-facing tier) over both milestones and returned 11 findings; 8 fixed here (probe-form variation on M095 AC1 / M096 AC1-AC3, the lexical-spelling proxy on M095 AC5, the one-cell grids on M095 AC2 / M096 AC4, and M096 AC5's unsatisfiable missing-argument promise, cut to Scope Out); the run-gate reachability finding went to the question gate.
