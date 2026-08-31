<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M100: Hardware encoding is a backend vocabulary, and videotoolbox is the second member

- **Status:** planned
- **Priority:** normal
- **Depends on:** M099
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Generalize the nvenc-only hardware surface into a backend vocabulary and ship
videotoolbox as its second member, so hardware encoding works on Apple hardware.

## Scope

Surface tier: **user-facing** — it widens an exported argument's accepted values
at 16 verbs and changes which encoder a hardware request selects.

**In:** `hardware=`'s vocabulary at the 16 exported verbs carrying it; a
per-backend codec-family table (nvenc: h264/hevc/av1; videotoolbox:
h264/hevc/prores); backend-aware availability probing reusing the existing
session memo; the abort and `fallback = TRUE` paths per backend. The names this
milestone builds against are M099's (d) call, not this milestone's to choose.

**Out:**
- `qsv`, `vaapi`, `amf` → ROADMAP candidate rows. Each enters through the same
  vocabulary and table this milestone builds; none is testable on hardware this
  project can reach, which is why they are not shipped blind alongside it.
- GPU *decode* / `-hwaccel` input acceleration → the standing M31 candidate row;
  it needs an engine input-options slot that does not exist.
- Renaming `has_nvenc()`, `nvenc_encoder()`, or `tidymedia.nvenc_encoders` →
  M099 candidate (d), which this milestone depends on.

## Acceptance criteria

- [ ] AC1 `hardware=` accepts `"videotoolbox"` at every verb that accepts
      `"nvenc"`. The domain is enumerated, not recalled: the exported functions
      whose `formals()` carry `hardware`, computed by walking
      `getNamespaceExports("tidymedia")` (16 at HEAD; a 17th `hardware=` grep hit
      is the internal `resolve_hw_encoder()`, outside the exported domain). For each, the accepted
      vocabulary is read from the formal's own default and asserted to contain
      both backends — so a verb added later fails the sweep rather than silently
      missing a backend.
- [ ] AC2 A backend's codec-family table decides both what it emits and what it
      refuses. Under `hardware = "videotoolbox"` the compiled command names
      `<family>_videotoolbox` for each family that table declares (`h264`,
      `hevc`, `prores`), and under `hardware = "nvenc"` names `<family>_nvenc`
      for its own (`h264`, `hevc`, `av1`) — asserted at `run = FALSE`, so no
      hardware is needed. A family outside a backend's table is refused naming
      the backend the caller asked for and that family: `"videotoolbox"` with an
      `av1` codec, `"nvenc"` with a `prores` codec, neither abort naming the
      other backend.
- [ ] AC3 The domain of each table is the table itself, not a hand-list: the
      test iterates the declared families per backend, so adding a family
      without a case fails rather than passing unnoticed.
- [ ] AC4 The availability probe answers per backend from one session memo:
      with `cached_encoder_names()` mocked to a pool holding
      `h264_videotoolbox` and not `h264_nvenc`, `hardware = "videotoolbox"`
      proceeds and `hardware = "nvenc"` aborts, and the reverse pool inverts
      both — the M094 technique, since no runner has nvenc hardware.
- [ ] AC5 `fallback = TRUE` falls back to the software encoder for an
      unavailable backend and says which backend it fell back from, asserted for
      both backends under the mocked pools of AC4.
- [ ] AC6 The videotoolbox path is executed for real, not only compiled: on a
      machine whose FFmpeg lists `h264_videotoolbox`, one verb writes a playable
      file under `hardware = "videotoolbox"`, skipped by `skip_if` where the
      encoder is absent. This is the first hardware path the suite can actually
      run — every nvenc behavior is decided by hardware no runner has (M094).
- [ ] AC7 The user-facing text describes a vocabulary, not one backend:
      `?tidymedia`, `?nvenc_encoder` and `?refresh_ffmpeg_capabilities` — the
      three topics `grep -rn "tidymedia.nvenc_encoders" man/` returns at HEAD —
      under whatever names M099 settled, and a `NEWS.md` entry naming
      videotoolbox and the families each backend covers.
- [ ] AC8 `devtools::test()` clean, `devtools::document()` produces no diff,
      `devtools::check()` reports 0 errors and 0 warnings with every NOTE
      justified (PROFILE `verify` and `consistency-gate` slots).

## Tasks

1. Read M099's (d) disposition; take the names from it.
2. Add the per-backend codec-family table and the backend-aware encoder-name
   builder, replacing `nvenc_encoder()`'s `paste0(codec, "_nvenc")`.
3. Generalize `codec_family()`, which today recognizes only h264/hevc/av1 and
   aborts naming nvenc, to resolve prores and to name the backend it was asked
   about.
4. Generalize the availability probe and its abort over the backend, keeping the
   `resolve_timeout()` call above the memo (D074, M094 review F5).
5. Widen `hardware=` at the 16 exported verbs and at `resolve_hw_encoder()`; write AC1's sweep.
6. Write AC2, AC3, AC4, AC5 tests — compiled-command and mocked-pool assertions,
   no hardware needed.
7. Write AC6's executing test with its `skip_if`.
8. Update the three help topics and `NEWS.md`.
9. Run `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Coverage

- AC1 → T5
- AC2 → T2, T3, T6
- AC3 → T2, T6
- AC4 → T4, T6
- AC5 → T4, T6
- AC6 → T7
- AC7 → T8
- AC8 → T9

## Work log
<!-- owner: implement/review -->

- 2026-08-31 plan CHECKPOINT: milestone drafted and registered; its criteria audit ([O] fresh-context reader, FULL mode per the user-facing surface tier) was still running when this was committed, so the audit's mandated result line is NOT yet written and no finding has been disposed. M100 is not ready to implement until that line exists. Verified independently while drafting: the AC1 sweep mechanism works (each verb's `hardware` default is readable via `formals()`), and the count is 16 exported verbs — an earlier draft said 17, which was a grep count including the internal `resolve_hw_encoder()`.
- 2026-08-31 plan: alternative rejected — shipping qsv/vaapi/amf alongside videotoolbox. Lost because none is testable on hardware this project can reach, the same gap M094 measured for nvenc, where a refusal bug survived three review rounds and 11,310 assertions on a path no runner executes. They became a candidate row entering through this milestone's own table. Falsified by CI gaining a runner with one of the three.
- 2026-08-31 plan: alternative rejected — adding `hardware = "videotoolbox"` as a special case beside nvenc with no backend abstraction. Lost at the question gate: it leaves a second special case for the next backend to trip over. Falsified by the abstraction costing more than the two special cases it replaces.
