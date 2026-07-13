# Lessons

_Durable, append-only repo lessons (build quirks, testing tricks) — captured at
milestone end, surfaced at plan time. Capped at 50 lines (D-015); prune the
least-useful when full. Not status, not decisions (a choice is a D-entry)._

- 2026-07-13 (M26): a `_batch` verb that auto-derives output paths from the
  input *basename* silently overwrites when two rows collide — a duplicated
  input, or same-basename inputs under one shared `outdir`. Guard at the
  resolved-path level (`anyDuplicated(patterns)`), not just the input level; the
  older `*_batch` verbs only rejected duplicated inputs.
- 2026-07-12 (M17): `devtools::check()` can print "0 notes" while raw `R CMD
  check` shows `Status: 1 NOTE` — the `tests/spelling.Rout` comparison NOTE for
  new technical terms is masked by the devtools summary. Run
  `spelling::update_wordlist()` and confirm `Status: OK` in `00check.log`.
- 2026-07-12 (M18): a cli `{?s}` governed by a `{.val {vector}}`/`{cli::qty(vec)}`
  throws `length(object) == 1` with 2+ items in a multi-line `cli_warn`/`abort`.
  Drive plurals off a scalar `{length(x)}` and list the vector without `{?s}`.
  Test cli count messages with 2+ items — a 1-item test hides the crash.
- 2026-07-12 (M19): a fast-path branch that skips the shared batch runner
  (`ffm_batch`) must re-synthesize that runner's *opt-in* outputs (`verified`
  column, manifest attr) or the return schema silently diverges from the normal
  path across calls. Trick: subsetting a canonical 0-row schema tibble pads to N
  all-NA rows for free (`col[rep(NA_integer_, n)]` gives n type-matched NAs).
  Test parity by comparing `names()`/types of the fast vs the normal path.
- 2026-07-13 (M23): a public-API rename/un-export must also sync `_pkgdown.yml`
  and live vignette/example chunks — neither is caught by `devtools::check()`
  (use `pkgdown::check_pkgdown()`; a chunk calling a now-internal fn fails only at
  vignette-build). Grep `vignettes/` + roxygen `@examples` before dropping `@export`.
- 2026-07-13 (M24): `devtools::build_readme()` always emits a spurious diff — README.Rmd
  examples print `system.file()` paths embedding the per-session temp libpath, which
  changes every build. Path-only churn, not a doc change; revert unless content changed.
- 2026-07-13 (M27): FFmpeg per-stream metadata (`-metadata:s:v:0 title=`) surfacing
  in mov stream tags is ffmpeg-version dependent (became `name` on 8.x macOS,
  absent on Ubuntu CI) — green locally + macOS, red on Ubuntu. Don't sanity-assert
  an injected per-stream tag's *presence*; assert only on the stripped *output*.
- 2026-07-13 (M28): extracting a shared helper *between* a documented function's
  `#'` roxygen block and its `fn <- function` line silently re-targets the roxygen
  to the helper and drops the original's `.Rd` — `document()` warns "Deleting
  <fn>.Rd". Put the extracted helper ABOVE the roxygen block, not between it and
  the function.
- 2026-07-12 (M30): a `*_batch` verb's jobs tibble keys on `input`/`output`
  *columns* (via `check_batch_jobs`), NOT the scalar verb's `infile`/`outfile`
  *argument* names — an easy mismatch in vignette/example chunks that errors only
  at build. Render vignettes with the ffmpeg/ffprobe/mediainfo binaries masked
  off PATH (`Sys.which()==""`) to reproduce the CI-absent build and catch it.
