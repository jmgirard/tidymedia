# M40: `audio_codec` subsumes `format` on `convert_audio` (+ batch), closing the codec sweep

- **Status:** planned
- **Priority:** normal
- **Depends on:** M39
- **Driving RR:** —
- **Principles touched:** IP1, GP1
- **Branch/PR:** —

## Goal

Rename `convert_audio()`'s `format` argument to `audio_codec` as a clean break,
give its batch column a way to spell "unset", and record the decision that
closes the codec-argument sweep.

## Scope

**In:** `convert_audio(infile, outfile, audio_codec = NULL, run = TRUE)`;
`format` removed under D014's pre-0.2.0 clean-break policy, no `lifecycle`
shim. The argument was already an audio codec in all but name — its own docs
say "naming the output audio codec … passed to FFmpeg's `-c:a`"
(`R/ffmpeg.R:461`) — so D014's `audio_codec` spelling applies and M22's audit
simply missed it. `NULL` keeps compiling `-q:a 0` (`R/ffmpeg.R:444`), so every
existing default command is byte-identical; that is a deliberate departure from
D016's emit-nothing sentinel, settled at the 2026-07-26 plan gate and recorded
in the new D-entry. Batch: the argument renamed, its column guard swapped from
`check_batch_string_col` (`R/ffmpeg.R:3280`, rejects `NA`) to
`check_batch_codec_col(jobs, "audio_codec")` + `batch_codec_cell()`, so `NA`
spells "use the `-q:a 0` default"; a stale `format` argument or `format` jobs
column aborts naming the replacement, because `...` swallows a retired argument
in silence (M37 lesson). The scalar needs no such guard — with no `...`, R's own
`unused argument` covers it. New D-entry closing the sweep.

**Out:** a separate quality/rate-control argument to carry the VBR level →
the standing ROADMAP candidate row (M31 Q4). Codec arguments on
`concatenate_videos`, `strip_metadata`, `format_for_web` → refused; the
D-entry states why. `extract_audio`'s no-`NULL` asymmetry (`check_string`
scalar-side, no-`NA` column) → left standing, recorded in the D-entry, not
changed here.

## Acceptance criteria

- [ ] AC1 `convert_audio()` and `convert_audio_batch()` take `audio_codec`, and
      `format` is gone from both signatures; a test asserts the scalar errors on
      `format =` and the batch aborts naming `audio_codec`.
- [ ] AC2 A `format` column in the jobs table aborts naming `audio_codec`;
      tested.
- [ ] AC3 `audio_codec = NULL` still compiles `-q:a 0`, and a named codec
      compiles `-c:a <name>` with no `-q:a`; both byte-identical to what the
      equivalent pre-rename call produced. Tested.
- [ ] AC4 A per-row `audio_codec` column overrides the scalar argument; `NA`
      resolves to the `-q:a 0` default; an all-`NA` (logical) column is
      accepted; a numeric column aborts (M34 lesson). Both boundaries tested.
- [ ] AC5 `cairn/DECISIONS.md` carries the sweep-closing entry: the three
      deliberately codec-less verbs and why each is on D016's hidden-codec
      side, `convert_audio`'s `NULL` departure, and `extract_audio`'s asymmetry.
- [ ] AC6 No `format =` reference to this verb survives in roxygen
      `@examples` (`R/ffmpeg.R:476`), `vignettes/`, `README.Rmd`, or
      `_pkgdown.yml` — verified by grep (M23 lesson); `pkgdown::check_pkgdown()`
      passes.
- [ ] AC7 NEWS.md entry for the breaking rename; `devtools::document()` no diff;
      `devtools::test()` and `devtools::check()` clean (0 errors, 0 warnings).

## Coverage

- AC1 → T1, T2, T3, T4
- AC2 → T3, T4
- AC3 → T1, T2
- AC4 → T3, T4
- AC5 → T6
- AC6 → T5
- AC7 → T5, T7

## Tasks

- [ ] T1 Rename the formal in `convert_audio_pipeline()` (`R/ffmpeg.R:440`) and
      `convert_audio()` (`R/ffmpeg.R:478`); keep the `-q:a 0` NULL branch intact.
- [ ] T2 Scalar tests: `NULL` → `-q:a 0`, named → `-c:a` and no `-q:a`, parity
      against the recorded pre-rename commands, `format =` errors.
- [ ] T3 `convert_audio_batch()` (`R/ffmpeg.R:3275`): rename the argument, add
      the two retired-spelling guards (argument arriving via `...`, and a
      `format` jobs column), swap in `check_batch_codec_col(jobs,
      "audio_codec")` and `batch_codec_cell()`.
- [ ] T4 Batch tests: retired argument and retired column each abort naming the
      replacement; column overrides the scalar; `NA` → default; all-`NA`
      logical accepted; numeric aborts.
- [ ] T5 Docs sweep: roxygen `@param`/`@examples`, grep `vignettes/`,
      `README.Rmd`, `_pkgdown.yml`; NEWS.md entry; `devtools::document()`, and
      `devtools::build_readme()` only if README.Rmd actually changed (M24: revert
      path-only churn).
- [ ] T6 Author the sweep-closing entry in `cairn/DECISIONS.md`.
- [ ] T7 Full `devtools::check()` plus `pkgdown::check_pkgdown()`.

## Work log

- 2026-07-26: created by /milestone-plan.

## Decisions

## Review
