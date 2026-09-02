<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M100: Hardware encoding is a backend vocabulary, and videotoolbox is the second member

- **Status:** review
- **Priority:** normal
- **Depends on:** M099
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m100-videotoolbox-backend`

## Goal

Generalize the nvenc-only hardware surface into a backend vocabulary and ship
videotoolbox as its second member, so hardware encoding works on Apple hardware.

## Scope

Surface tier: **user-facing** — it widens an exported argument's accepted values
at 16 verbs and changes which encoder a hardware request selects.

**In:** `hardware=`'s vocabulary at the 16 exported verbs carrying it and at the
internal `resolve_hw_encoder()`, which runs its own `arg_match()` against the
same default; a per-backend codec-family table (nvenc: h264/hevc/av1;
videotoolbox: h264/hevc); backend-aware availability probing over both routes
the probe already has; the abort and `fallback = TRUE` paths per backend; and a
`hardware` argument on the exported availability helper, which today can answer
only for nvenc — spelled `hardware =`, the word the 16 verbs already use for the
same values, never `backend =` (RR06 Q5, amended 2026-09-01). The helper's NAME is M099's (d) call; its SIGNATURE is this
milestone's, since adding an argument is additive and outside D014.

**Out:**
- **prores.** `prores_videotoolbox` is real and is the family nvenc has no
  equivalent for, but it cannot mux into `.mp4` — measured 2026-08-31, a 0-byte
  output and "Could not find tag for codec prores in stream #0" — while `.mov`
  and `.mkv` take it. Shipping it needs a container guard, a design call of its
  own → ROADMAP candidate row carrying that measurement.
- `qsv`, `vaapi`, `amf` → ROADMAP candidate row; each enters through the table
  this milestone builds, and none is testable on hardware this project reaches.
- GPU *decode* / `-hwaccel` input acceleration → the standing M31 candidate row.
- Renaming `has_nvenc()`, `nvenc_encoder()`, or `tidymedia.nvenc_encoders` →
  M099 candidate (d), decided 2026-09-01 (D077): this milestone builds against
  `has_hardware_encoder()`, `hardware_encoder()` and `tidymedia.hardware_encoders`.

## Acceptance criteria

- [ ] AC1 `hardware=` accepts `"videotoolbox"` wherever it accepts `"nvenc"`,
      with `"none"` still first so the default is unchanged. The exported domain
      is enumerated by the suite's existing `nvenc_hardware_exports()`
      (`tests/testthat/helper-nvenc-memo.R`) — 16 at HEAD — and each verb's
      accepted set is its `hardware` formal's own default, which is the accepted
      set only because every one of the 16 calls bare `rlang::arg_match(hardware)`
      with no `values=`; the sweep asserts that too, so a verb supplying its own
      vocabulary cannot pass while refusing the backend. The internal
      `resolve_hw_encoder()` (`R/ffmpeg.R:3095`) carries the same default and is
      widened with them: widening only the exported 16 leaves every
      `hardware = "videotoolbox"` call aborting inside the resolver.
- [ ] AC2 A backend's codec-family table decides both what it emits and what it
      refuses. Under `hardware = "videotoolbox"` the compiled command names
      `<family>_videotoolbox` for each family that table declares, and under
      `hardware = "nvenc"` names `<family>_nvenc` for its own; the test iterates
      the declared families and asserts the h264 and hevc cases both backends
      share, so a family added to a table without a builder case fails. A family
      outside a backend's table is refused naming the backend the caller asked
      for and that family — `"videotoolbox"` with an `av1` codec, `"nvenc"` with
      a `prores` codec — neither abort naming the other backend. These
      assertions set an available pool (AC3's mechanism); `run = FALSE` does not
      license them, since D034 runs the probe when the pipeline is built,
      `run` notwithstanding.
- [ ] AC3 An unavailable backend aborts, and `fallback = TRUE` falls back, and
      both routes to the probe's answer agree. The memo route: with
      `withr::local_options(tidymedia.hardware_encoders = NULL)` — required,
      because the option is read before the memo and leaves a mock inert — and
      `cached_encoder_names()` mocked to a pool holding `h264_videotoolbox` and
      not `h264_nvenc`, `hardware = "videotoolbox"` proceeds and
      `hardware = "nvenc"` aborts; the reverse pool inverts both. The option
      route: the same four outcomes with the option set directly and no mock,
      since that seam is read first, is documented in three help topics, and is
      the one carried into `parallel = TRUE` workers. Under `fallback = TRUE`
      each unavailable case instead falls back to the software encoder and says
      which backend it fell back from.
- [ ] AC4 The videotoolbox path is executed for real, not only compiled: one
      verb writes a file under `hardware = "videotoolbox"` that exists, is
      non-empty, and reports the requested width. Its skip runs a one-frame
      encode and skips on non-zero exit, never on the encoder list —
      `skip_if_no_nvenc()`'s shape, which exists because a build can list an
      encoder it cannot run, and VideoToolbox is listed on every macOS build
      including virtualized runners. This is the first hardware path the suite
      can execute at all; every nvenc behavior is decided by hardware no runner
      has (M094).
- [ ] AC5 The exported availability helper answers for either backend, not only
      nvenc: under AC3's pools it reports videotoolbox available and nvenc not,
      and the reverse, under whatever name M099 settled.
- [ ] AC6 The user-facing text describes a vocabulary, not one backend, over a
      domain wider than `man/`: the three topics
      `grep -rln "tidymedia.hardware_encoders" man/` returns, which are the same
      three at HEAD; the paragraph and chunk in `vignettes/workflow.Rmd` under
      `standardize_video_batch()` that today teach `hardware = "nvenc"` and
      `has_hardware_encoder("h264")` as the way to ask; the "FFmpeg
      capabilities" section prose in `_pkgdown.yml`, which reads "opt-in
      hardware (GPU) encoding" and which this milestone re-checks rather than
      rewrites; and a `NEWS.md` entry naming videotoolbox and the families each
      backend covers. `devtools::check()` reaches none of these, so AC7 does not
      backstop it.
- [ ] AC7 `devtools::test()` clean, `devtools::document()` produces no diff,
      `pkgdown::check_pkgdown()` passes, and `devtools::check()` reports 0
      errors and 0 warnings with every NOTE justified (PROFILE `verify` and
      `consistency-gate` slots).

## Tasks

1. [x] Take the helpers' names from D077 — `has_hardware_encoder()`,
   `hardware_encoder()`, `tidymedia.hardware_encoders`, argument `hardware =`.
2. [x] Add the per-backend codec-family table and the backend-aware
   encoder-name builder, replacing `hardware_encoder()`'s
   `paste0(codec, "_nvenc")`, and site the (family, backend)-not-in-table
   refusal there (Decisions, RR07 R4).
3. [x] Keep `codec_family()` backend-free; stop its abort naming nvenc for a
   family it cannot infer.
4. [x] Generalize the probe, its abort and both helpers over the backend —
   required `hardware =` over the backend set only (D079) — keeping
   `resolve_timeout()` above the memo (D074, M094 F5) and the option seam above
   both (D044); widen `check_nvenc_available()`'s gate so a videotoolbox
   `_batch` call is refused at the verb (Decisions, D035).
5. [x] Widen `hardware=` at the 16 exported verbs and at
   `resolve_hw_encoder()`; extend `nvenc_hardware_exports()` into AC1's sweep,
   excluding the two helpers from its domain (Decisions, RR07 R8).
6. [x] Write AC2's and AC3's tests — compiled-command, mocked-pool and
   option-seam assertions, plus the batch-verb blame assertion; no hardware
   needed.
7. [x] Write AC4's executing test with its one-frame-encode skip.
8. [x] Update the three help topics, the 16 verbs' `@param hardware` and
   `@seealso` text, the ~31 helper call sites the required argument reaches,
   `vignettes/workflow.Rmd`, `_pkgdown.yml` and `NEWS.md` (M099's "nvenc is the
   one hardware backend" sentence is edited, not contradicted).
9. [x] Run `devtools::document()`, `pkgdown::check_pkgdown()`,
   `devtools::test()`, `devtools::check()`.

## Coverage

- AC1 → T5
- AC2 → T2, T3, T6
- AC3 → T4, T6
- AC4 → T7
- AC5 → T4, T6
- AC6 → T8
- AC7 → T9

## Decisions
<!-- owner: implement/review -->

- 2026-09-01 (RR07 ingest, every disposition taken by the maintainer at the
  gate). **The helpers' signature.** Both `has_hardware_encoder()` and
  `hardware_encoder()` take a required `hardware =` over the backend set only;
  `"none"` is refused. Promoted to D079, which carries the reasoning.
- 2026-09-01 (RR07 R4). The (family, backend)-not-in-table refusal is sited in
  `hardware_encoder()`, once. `codec_family()` stays a backend-free inference
  from a software codec name, and only its abort text changes. This narrows T3,
  whose plan wording said to generalize `codec_family()` over the backend: the
  family question and the table question are different questions with different
  refusals.
- 2026-09-01 (RR07 R8). The two helpers are excluded from
  `nvenc_hardware_exports()`'s domain. That helper enumerates every export with
  a `hardware` formal, so adding the argument would pull the helpers into AC1's
  sweep, the memo grid (`test-nvenc-memo-grid.R:41-47`, where a mapper cell
  probes zero times) and the probe-blame sweep. The domain those three tests
  mean is the 16 verbs and the resolver; AC5 covers the helpers separately.
  This is the enumeration procedure catching two members it never meant, not a
  narrowing of AC1.
- 2026-09-01 (RR07 R9). `check_nvenc_available()`'s gate is
  `identical(hardware, "nvenc")`, so a `hardware = "videotoolbox"` call would
  return early from the front-door check and a missing encoder in a `_batch`
  call would be blamed on `purrr::pmap()` — the defect D035 sited that call to
  prevent. The gate is widened, and the batch-verb blame gets its own test
  assertion. AC3's wording is NOT amended for it: asserting where an abort is
  blamed is a property AC3 does not promise, and a criterion is a floor, not a
  ceiling on what the suite may assert.
- 2026-09-01 (RR07 R5, amendment gate, maintainer-accepted). AC6 amended: its
  grep, its vignette anchor and its `_pkgdown.yml` quote all named instruments
  M099 moved between this milestone's planning and its implementation. Cleared
  before writing by a fresh-context [O] reader that did not author the wording,
  asked the criteria audit's three questions in FULL mode plus the widening
  test and the joint-satisfiability question: ADMISSIBLE, not a widening —
  every clause respells a stale instrument or narrows, and one clause
  ("re-checks rather than rewrites") converts an implied rewrite into a
  verification. The reader required one clarifying edit, taken: the quoted
  `has_hardware_encoder("h264")` is marked as the anchor at HEAD, since D079
  removes that call's default and it would otherwise read as a frozen target
  state. The reader also confirmed what RR07 found — the 16 verbs' own roxygen
  is user-facing text outside every procedure AC6 names, in both versions —
  and that closing it inside AC6 would be the widening the test forbids; it
  went to T8 at the maintainer's gate instead.

- 2026-09-02 (implement, T2/T3). **`prores` is a family the package infers and
  no backend's table holds.** AC2 requires the `"nvenc"`-with-a-`prores`-codec
  refusal to name the backend and the family, and only the table lookup knows
  which backend was asked for. So `codec_family()` gained a `prores` branch and
  the family vocabulary `hardware_encoder()` accepts is
  `c("h264", "hevc", "av1", "prores")`, wider than either backend's row. Both
  backends refuse `prores`; that is the scope-out decision showing through as a
  refusal rather than as an inference failure, and `codec_family()`'s abort now
  fires only when no family matches at all and names no backend.
- 2026-09-02 (implement, T4; the question gate's second question, which the
  maintainer folded into `/milestone-review`). **The internals that now serve
  both backends are renamed:** `nvenc_available()` is
  `hardware_encoder_available()` and `check_nvenc_available()` is
  `check_hardware_available()`. The nvenc-named test files and test helpers
  (`test-nvenc*.R`, `helper-nvenc-memo.R`, `skip_if_no_nvenc()`,
  `nvenc_hardware_exports()`) keep their names, as the gate's recommended
  option said. Review adjudicates the split.

## Work log
<!-- owner: implement/review -->

- 2026-08-31 plan: criteria audit ran in FULL mode (surface tier user-facing), fresh-context [O] reader, over the final wording. It returned eight findings on this file and all are disposed here. Three were verified against the repo before acting and all three held. (1) AC2 licensed its assertions with `run = FALSE`, which is false against D034 — its title says a probe entering the compiled command runs when the pipeline is built, `run` notwithstanding — so every videotoolbox compile assertion would have aborted on a non-macOS runner; AC2 now sets a pool. (2) AC3's mock of `cached_encoder_names()` was inert, since `nvenc_available()` reads `getOption("tidymedia.nvenc_encoders")` first and only falls through to the memo; the option unset is now named, and the option seam gained its own assertions, being the branch read first and the one carried into workers. (3) `prores_videotoolbox` cannot mux into `.mp4` — measured, a 0-byte output and "Could not find tag for codec prores in stream #0", while `.mov` and `.mkv` take it — so the drafted table would have compiled a command that always dies at run time; prores left the scope at the user's gate. Also fixed: AC1's count was 17 from a grep including internal `resolve_hw_encoder()` (16 exported, and the resolver is now explicitly in scope, since widening only the exported verbs leaves every call aborting inside it); AC1 admitted a vocabulary reordering that would have made GPU encoding the default at 16 verbs; the drafted AC3 bound the test's iteration strategy rather than the deliverable (D-118) and folded into AC2; AC6's skip trusted the encoder list where `skip_if_no_nvenc()` already establishes a one-frame encode probe; and AC7's domain stopped at `man/`, missing `vignettes/workflow.Rmd:74-87` and `_pkgdown.yml:118`, which teach nvenc-only and which `devtools::check()` never reads. The suite's existing `nvenc_hardware_exports()` is now reused rather than a second sweep invented.
- 2026-08-31 plan: alternative rejected — shipping qsv/vaapi/amf alongside videotoolbox. Lost because none is testable on hardware this project can reach, the same gap M094 measured for nvenc, where a refusal bug survived three review rounds and 11,310 assertions on a path no runner executes. They became a candidate row entering through this milestone's own table. Falsified by CI gaining a runner with one of the three.
- 2026-08-31 plan: alternative rejected — adding `hardware = "videotoolbox"` as a special case beside nvenc with no backend abstraction. Lost at the question gate: it leaves a second special case for the next backend to trip over. Falsified by the abstraction costing more than the two special cases it replaces.
- 2026-08-31 plan: alternative rejected — shipping prores with a container guard in this milestone. Lost at the user's gate to shipping h264/hevc first: those two behave exactly as nvenc's families do, so the backend abstraction lands without introducing a new user-facing failure mode. Falsified by a caller needing ProRes output badly enough that the guard is cheaper than the wait.
- 2026-08-31 plan: the audit found a scope hole neither milestone owned — `has_nvenc()`/`nvenc_encoder()` are exported and structurally answer only for nvenc, so a user could request videotoolbox at 16 verbs with no exported way to ask whether it is available, while `vignettes/workflow.Rmd:79` teaches `has_nvenc("h264")` as the check. Resolved without a user gate: the helper's NAME stays M099's (d) call and its SIGNATURE becomes this milestone's, since adding a `backend` argument is additive and outside D014's rename policy. The consequence of declining (d) is recorded in M099's work log so its gate weighs it.
- 2026-09-01 implement: branch `m100-videotoolbox-backend` cut from `master` at 0436981; status set in-progress in ROADMAP and the header mirror.
- 2026-09-01 implement question gate, three questions, all three deferred by the user. (1) What `hardware =` defaults to on the two exported helpers -- nvenc, any-available, or no default -- escalated to `/milestone-brief`; the recommendation had been keeping nvenc, which changes no existing call and keeps `hardware_encoder()` a pure lookup (D077). (2) How far the backend-neutral naming carries into internal helpers still spelled for nvenc: the user folded this into `/milestone-review`, so implementation takes the recommended option -- rename the internals that now serve both backends, leave the nvenc-named test files and test helpers -- and review adjudicates it. (3) AC6's wording, whose two named instruments moved under M099 (`tidymedia.nvenc_encoders` now returns no `man/` topics; `_pkgdown.yml`'s section prose already reads "opt-in hardware (GPU) encoding"): the user folded the wording question into the same brief, so AC6 is NOT amended here and the amendment gate runs on the brief's answer.
- 2026-09-01 plan amendment (from M099's RR06 ingest, user-accepted at that gate): Scope In's helper argument is spelled `hardware =`, not `backend =` — RR06 Q5 found `backend =` would put one value set under two argument names where the verbs' `@param hardware` already reads "The encoder backend". Scope Out's declined-(d) contingency replaced by the D077 names this milestone now builds against. Plan-owned body re-checked against the 150-line cap after the edit.
- 2026-09-01 implement: blocked on RB07 — the two exported helpers' `hardware =` default, its accepted set, whether the pure mapper carries it, AC6's stale wording, and the second-escalation removal option. Brief at `cairn/reviews/RB07-hardware-helper-signature.md`.
- 2026-09-01 ingest RR07, advisory (no binding criteria requested), every disposition taken by the maintainer at the gate: R1-R9 applied, R10-R12 rejected with the reasons RR07 states, R13 to a candidate row. The five substantive answers are in this file's Decisions section; the cross-cutting one is promoted to D079. Status back to in-progress; RB07/RR07 archived.
- 2026-09-01 ingest audit over the amended AC6, FULL mode (surface tier user-facing), fresh-context [O] reader that did not author the wording, asked the criteria audit's three questions plus the widening test and joint satisfiability across AC1-AC7. Returned ADMISSIBLE and not a widening, with one required clarifying edit, taken. It confirmed the under-enumeration RR07 found (the 16 verbs' own roxygen is user-facing text outside every procedure AC6 names) is present identically in both versions, so closing it inside AC6 would be the widening the test forbids; it went to T8 at the maintainer's gate.
- 2026-09-01 plan amendment (substantive, mini gate, maintainer-accepted): AC6's wording. Amended text shown verbatim in chat at the gate and carried in the Decisions section's rationale; the criteria set is unchanged in size and in what it promises.
- 2026-09-01 minor amendments: T1 checked off (the helpers' names come from D077); T3 narrowed to keeping `codec_family()` backend-free; T4 gained the required-argument and front-door-gate work; T5 gained the sweep exclusion; T8 gained the 16 verbs' roxygen and the helper call sites. Tasks compressed in one pass to shed the two lines the amendments put over the 150-line plan-owned cap — Acceptance criteria is the heavier section but cannot be reworded outside the amendment gate, and Tasks is the section that grew.
- 2026-09-01 candidate row: RR07's backend-vector export (R13) absorbed as item (d) of the existing "What M100 leaves out of the hardware surface" row rather than added as a new row; that row and the `install_on_win()` row were compressed in the same pass to keep ROADMAP.md under its byte budget.
- 2026-09-02 plan amendment (substantive, mini gate, maintainer-accepted): AC3's option name, `tidymedia.nvenc_encoders` -> `tidymedia.hardware_encoders`, which M099 renamed between this plan and its implementation, leaving AC3's named procedure setting an option no code path reads. Amended text shown verbatim in chat at the gate. Cleared before writing by a fresh-context [O] reader that did not author it, FULL mode (surface tier user-facing), asked the criteria audit's three questions plus the probe, instrument, proportionality and widening tests and joint satisfiability across AC1-AC7: ADMISSIBLE, not a widening, no required edits. The criteria set is unchanged in size and in what it promises.
- 2026-09-02 implement T2-T5: `hardware_backend_families()` (nvenc h264/hevc/av1, videotoolbox h264/hevc), `hardware_backends()` read off it, `hardware_encoder(codec, hardware)` with the (family, backend) refusal sited there once, `codec_family()` backend-free, `hardware_encoder_available()`/`check_hardware_available()` generalized with the front-door gate widened to backend-set membership, and `hardware = c("none", "nvenc", "videotoolbox")` at the 16 verbs and the resolver. Verify slot clean.
- 2026-09-02 delegation [S], T8's roxygen half: the 16 verbs' `@param hardware`, `@param fallback` and `@seealso` text plus the shared helper topic, ~390 roxygen lines in `R/ffmpeg.R`. Diff verified by this session: code-line hunks were all this session's own, and one repair was needed -- the rewrite wrapped the probe sentence across a line break, which `test-nvenc-docs.R` matches as a contiguous clause over 16 topics.
- 2026-09-02 implement T6/T7: `tests/testthat/test-hardware-backends.R` -- the table-iterating compile assertions, the two (family, backend) refusals, the memo and option routes in both directions, the fallback message per backend, the `_batch` blame assertion, AC5's helper pools, and AC4's executing test behind `skip_if_no_videotoolbox()`. AC4 ran for real on this machine: `h264 (native) -> h264 (h264_videotoolbox)`, a 160x120 output of 19 KiB.
- 2026-09-02 enumeration correction, extending RR07 R8's disposition to the three further sweeps that quantify over "everything documenting or carrying `hardware`": `nvenc_hardware_exports()`, the probe-blame cell builder, `test-nvenc-front-door.R`'s completeness test, and `test-nvenc-docs.R`'s Rd-topic enumeration all exclude the two capability helpers by name, through one shared `nvenc_hardware_helpers()`. Same correction, same reason; AC1's and AC5's domains are unchanged.
- 2026-09-02 census: the corrupt-argument sweep's master census is a merge-base measurement and was left untouched (IP4). `has_hardware_encoder()`'s new required `hardware` formal adds five dropped cells refused inside `hardware_encoder()` -- the frame that already refuses a wrong `codec` -- so the live totals move 1530/437 to 1535/442 with `kept` unchanged, and the one new entry is named in the test's two-way difference rather than added to the frozen list.
- 2026-09-02 implement T8: `NEWS.md` gains a new-features entry for the vocabulary and a breaking-changes entry for the helpers' required `hardware`, and M099's "nvenc is the one hardware backend" sentence is edited rather than contradicted (RR07 point 4). `vignettes/workflow.Rmd` teaches naming a backend and checking under the same name. `_pkgdown.yml`'s "FFmpeg capabilities" prose was re-checked and needs no edit: it already reads "opt-in hardware (GPU) encoding", which is backend-neutral.
- 2026-09-02 implement T9: `devtools::document()` produces no diff, `pkgdown::check_pkgdown()` reports no problems, `devtools::test()` is 0 failures / 11271 passing / 18 skipped (the 10 warnings are the pre-existing dropped-audio-track warnings from execution tests, none in this milestone's files), and `devtools::check()` is Status: OK -- 0 errors, 0 warnings, 0 notes. One spelling NOTE fired first and was closed by `spelling::update_wordlist()` adding `backend's`, `hevc` and `videotoolbox` to `inst/WORDLIST`. `README.Rmd` names neither `hardware` nor nvenc, so it needs no re-knit.
