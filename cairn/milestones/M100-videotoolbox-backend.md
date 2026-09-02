<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M100: Hardware encoding is a backend vocabulary, and videotoolbox is the second member

- **Status:** blocked
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
      `withr::local_options(tidymedia.nvenc_encoders = NULL)` — required,
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
      `grep -rn "tidymedia.nvenc_encoders" man/` returns; `vignettes/workflow.Rmd`
      lines 74-87, which teach `hardware = "nvenc"` and `has_nvenc("h264")` as
      the way to ask; `_pkgdown.yml:118`, whose section prose says "opt-in
      NVIDIA nvenc GPU encoding"; and a `NEWS.md` entry naming videotoolbox and
      the families each backend covers. `devtools::check()` reaches none of
      these, so AC7 does not backstop it.
- [ ] AC7 `devtools::test()` clean, `devtools::document()` produces no diff,
      `pkgdown::check_pkgdown()` passes, and `devtools::check()` reports 0
      errors and 0 warnings with every NOTE justified (PROFILE `verify` and
      `consistency-gate` slots).

## Tasks

1. Read M099's (d) disposition; take the helper's name from it.
2. Add the per-backend codec-family table and the backend-aware encoder-name
   builder, replacing `nvenc_encoder()`'s `paste0(codec, "_nvenc")`.
3. Generalize `codec_family()`, which today recognizes only h264/hevc/av1 and
   aborts naming nvenc by hand, to name the backend it was asked about.
4. Generalize the availability probe, its abort, and the exported helper over
   the backend, keeping `resolve_timeout()` above the memo (D074, M094 F5) and
   the option seam above both (D044).
5. Widen `hardware=` at the 16 exported verbs and at `resolve_hw_encoder()`;
   extend `nvenc_hardware_exports()` into AC1's sweep.
6. Write AC2's and AC3's tests — compiled-command, mocked-pool and option-seam
   assertions, no hardware needed.
7. Write AC4's executing test with its one-frame-encode skip.
8. Update the three help topics, `vignettes/workflow.Rmd`, `_pkgdown.yml` and
   `NEWS.md`.
9. Run `devtools::document()`, `pkgdown::check_pkgdown()`, `devtools::test()`,
   `devtools::check()`.

## Coverage

- AC1 → T5
- AC2 → T2, T3, T6
- AC3 → T4, T6
- AC4 → T7
- AC5 → T4, T6
- AC6 → T8
- AC7 → T9

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
