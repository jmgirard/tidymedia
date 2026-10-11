# M138: The shipped docs and the remembered-location checks stop assuming what the host's FFmpeg build and `which` provide

**Status:** done (2026-10-11, PR #142 https://github.com/jmgirard/tidymedia/pull/142)

**Goal:** `R CMD check` passes on a build without `libx264` and where `which` does not answer for a path.

**Outcome:** Exported `has_encoder()` (`R/ffmpeg.R`, over `cached_encoder_names()`). Every re-encoding example
and vignette chunk names its encoder and is guarded on it. `skip_if_no_encoder()` guards the tests and
fixtures that name one. `tm_program_state()` in `R/program_management.R` reads a path from the filesystem
and a bare name through `Sys.which()`. On POSIX it asks `file.access(path, 1L)`, and on Windows R's
`expandcmd()` rule. `find_program()`, `tm_set_program()` and `tm_usable_binary()` ask it. New warning class
`tidymedia_location_not_executable` (POSIX). A `~` location is expanded where it is saved or read.
`.github/workflows/fedora-cran-model.yaml` checks on `fedora:44` with `ffmpeg-free`.
`tools/fedora_workflow_local.R` runs that job's steps from the committed file. `tools/ffmpeg_build_model/ffmpeg`
models a build missing named encoders.

**Decisions:** D095 (doc sites gated on their encoder) and D096-D101 (the location predicate, `~` expansion,
the Windows rule, and corrections to earlier reasons).

**Review:** Five passes, each a three-lens fan-out, with three defect returns. Thrash rule (a) fired at the
third, and the maintainer chose a descope: AC2(c), AC4, AC6 and the Scope promise no run URL and a
POSIX-only diagnosis, in wording approved at the stop after two re-audits. Pass 5 passed with 9 fixes. All
11 CI checks green, including Fedora 44 and a Windows run of the predicate-against-`Sys.which()` test.
Follow-ups: the "Six M138 review leftovers" candidate row. The `Sys.which` lesson's (a) and (c) retired by
enforcement.
