# M139: A re-encoding function refuses by name when this FFmpeg build has no encoder for what the caller asked

- **Status:** planned
- **Priority:** normal
- **Depends on:** M138
- **Driving RR:** —
- **Principles touched:** GP1
- **Resolves:** —
- **Surface tier:** user-facing. It adds a refusal a caller sees, and a condition class.
- **Branch/PR:** —

## Goal

When a call is about to run FFmpeg, refuse by name if this build has no encoder
for what the call will use. Today FFmpeg's own unknown-encoder exit reaches the
caller instead.

## Scope

**In:** the refusal, sited where a verb is about to spawn. The verb set is swept
rather than listed. Both the scalar argument and the jobs column are covered.
The name asked about is the encoder the call actually emits, so a hardware
backend is judged on its own encoder. One condition class covers the event, and
the hardware sibling's unclassed abort takes it too. The decision entries this
needs are written here.

**Out:** compilation stays binary-free, so a `run = FALSE` call still previews a
command this build cannot run. That is disclosed in `cairn/DESIGN.md`'s Known
issues rather than fixed. `verify_media()`'s codec arguments, which are
expectations read from a file's metadata rather than encoders to run. A refusal
for a muxer, filter or decoder this build lacks.

## Acceptance criteria

- [ ] AC1: The refusal fires where the call is about to spawn, and never where it compiles.
      Every `run = FALSE` call spawns exactly the programs it spawned before. The committed
      spawn-trace sweeps measure that, and so do the five purity tests at
      `tests/testthat/test-audio-stream-passthrough.R:161`, `test-audio-stream-crop-segment.R:290`,
      `test-audio-stream-format-web.R:131`, `test-audio-stream-normalize.R:306` and
      `test-audio-track-drop.R:63`, each still asserting zero spawns. A refusal planted above the
      `run` gate reddens at least one of them. `tm_outfiles_baseline()` is re-measured at the
      merge base in the same run rather than trusted.
- [ ] AC2: On the run path, a call that uses an encoder this build lacks refuses before it
      spawns. The refusal names the encoder, the argument or column it came from, and the verb
      the caller typed. The name asked about is `intended_encoder(video_codec, hardware)`, so
      `video_codec = "libx264"` with `hardware = "videotoolbox"` is judged on
      `h264_videotoolbox` and is not refused for lacking `libx264`. The refusal carries the class
      `tidymedia_encoder_unavailable` with `tm_` fields for the encoder and the verb, and
      `check_hardware_available()`'s availability abort at `R/ffmpeg.R:3432` carries it too, so
      one event is not classed at one site and unclassed at the other (D062, D063). The domain is
      the exported verbs whose codec argument reaches the codec emitters, enumerated by a sweep
      over the installed namespace's formals, whose output is quoted at review. `verify_media()`
      is excluded by name and by reason, because its codec arguments are expectations about a
      file. `format_for_web()` and `format_for_web_batch()` are included by hand, because they
      hardcode theirs. Cells carrying `"copy"`, `NULL` or a column `NA` are outside the promise,
      because none of the three names an encoder (D022).
- [ ] AC3: The batch forms refuse the same way for a codec read from a jobs column, and the
      message names the row. The cell where a column overrides the argument is asserted on its
      own. The verb-and-column pairs come from AC2's sweep, and the absent-column and `NA`-cell
      readings follow D022. This moves a refusal that D074 property 1, D076 and D085 all leave
      disclosed inside the per-row fan-out, so this milestone writes the entry superseding that
      disposition, trims `cairn/DESIGN.md`'s Known issues entry that carries it, and trims the
      ROADMAP candidate row that holds the remainder.
- [ ] AC4: The new refusal reports below every check that cannot depend on its answer (D075), and
      it displaces no limit error. It reads the session memo without re-reading
      `tidymedia.timeout`, so the verb's own re-call still refuses an unusable limit (D074
      property 1), and a caller's `pixel_format` or `regions` error still reports at
      `crop_video()`, `format_for_web()`, `standardize_video()` and `anonymize_video()`. A test
      fires each of those four with a bad limit and a bad argument together. The orderings are
      generated and never listed: a committed generator crosses the swept verb set with the
      argument contradictions, the cheap value guards, the hardware-availability refusals and the
      `run` guard, in both the scalar and the column form, each cell with a live control. A
      planted reordering reddens the affected cells.
- [ ] AC5: Where no FFmpeg location resolves, the new refusal stays silent. The run path then
      gives the existing not-found refusal from the spawn site, and the `run = FALSE` path still
      compiles with no new refusal. The silence is conditioned on a location resolving, and not on
      `has_encoder()` answering FALSE, which it also does on a machine that has no FFmpeg at all.
- [ ] AC6: `cairn/PROFILE.md`'s verify and consistency-gate slots are clean, the universal
      cairn-file checks included. `NEWS.md` carries an entry for the new refusal.
      `cairn/DESIGN.md`'s Known issues entry opening "Ten arguments are refused below the verb the
      caller typed" is corrected for what AC3 and AC4 change. Each cell's instrument is named:
      M138's `TIDYMEDIA_HIDE_ENCODERS` wrapper, a mocked `cached_encoder_names()`, or the
      `tidymedia.hardware_encoders` option seam for the hardware half, which `has_encoder()`
      deliberately does not read.

## Coverage

- AC1 → T1, T2
- AC2 → T2, T3
- AC3 → T4
- AC4 → T5, T6
- AC5 → T2
- AC6 → T7

## Tasks

- [ ] T1: Record the baseline. Re-measure the spawn-trace sweeps and
      `tm_outfiles_baseline()` at the merge base, and pin the five purity tests
      as the controls AC1 reads.
- [ ] T2: Write the refusal and site it at the spawn gate, above every spawn and
      below the `run` gate. Add the class, and the no-location silence.
- [ ] T3: Sweep the verb set from the installed namespace's formals. Apply the
      refusal across it, less `verify_media()`, plus the two `format_for_web`
      functions. Quote the sweep.
- [ ] T4: Carry the refusal into the batch forms over a jobs column, with the row
      in the message and the column-overrides-argument cell asserted.
- [ ] T5: Give `check_hardware_available()`'s availability abort the same class,
      and keep the limit re-read out of the probe's path.
- [ ] T6: Write the ordering generator and its live controls, and plant a
      reordering to see the affected cells redden.
- [ ] T7: Write the records. `NEWS.md`. The entry under D024's third exclusion,
      answering D035's three conditions for a new abort gate. The entry
      superseding the disclosed per-row disposition of D074 property 1, D076 and
      D085. The `cairn/DESIGN.md` Conventions and Known issues corrections, and
      the ROADMAP candidate-row trims.

## Work log

- 2026-10-10: created by /milestone-plan, from the user's answer at M138's question set asking for the refusal everywhere a caller names an encoder. Split out of M138 so the CRAN deadline of 2026-10-31 is not held by it.
- 2026-10-10: plan gate chose the spawn gate over the verb's front door as the refusal's site. A front-door probe spawns `ffmpeg -encoders` on a `run = FALSE` call. That runs against D024's pure-compilation surface, the `cairn/DESIGN.md` Conventions bullet, D034's two-member census, and five committed purity tests asserting zero spawns. The cost is that a dry run still previews a command this build cannot run. AC1 pins the purity, and Scope Out discloses the cost. Falsified by a caller harmed by previewing a command that cannot run.
- 2026-10-10: plan gate chose to ask about `intended_encoder(video_codec, hardware)` rather than the token the caller typed. Asking about the token refuses `video_codec = "libx264", hardware = "videotoolbox"`, which works today, against D035's no-new-refusal condition. Falsified by a caller who needs the refusal to name what they typed.
- 2026-10-10: criteria audit ran in full mode, in a fresh Opus reader, over a first draft that sited the refusal at the verb's front door. It found no criterion passing all six questions, and two of them mutually unsatisfiable: the front-door siting and the promise that no `run = FALSE` call spawns anything new. All findings were taken, and the siting choice above is what resolves the pair. Also taken: `verify_media()` excluded from the sweep by name and reason, the `"copy"`, `NULL` and `NA` cells carved out, the hardware axis routed through `intended_encoder()`, the class named and the hardware sibling's unclassed abort brought with it, the D075 wording restored to its own rule, the timeout displacement at the four D074 verbs closed, the crossings generated rather than listed per D039's precedent, the batch-column disposition given its superseding entry, the no-location silence conditioned on location resolution rather than on FALSE, and the per-cell instruments named because this build's missing encoder is not observable on this machine.
- 2026-10-10: the redrafted criteria have not been through a second audit round. The first round's findings are all answered above, and the siting choice is what the round asked to be settled before the rest were re-drafted.

## Decisions

## Review
