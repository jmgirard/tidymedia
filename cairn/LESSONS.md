# Lessons

_Durable, append-only repo lessons (build quirks, testing tricks) — captured at
milestone end, surfaced at plan time. Capped at 50 lines (D-015); prune the
least-useful when full. Not status, not decisions (a choice is a D-entry)._
- 2026-07-13 (M26): a `_batch` verb that auto-derives output paths from the input *basename* silently overwrites when two rows collide — a duplicated input, or same-basename inputs under one shared `outdir`. Guard at the resolved-path level (`anyDuplicated(patterns)`), not just the input level; the older `*_batch` verbs only rejected duplicated inputs.
- 2026-07-12 (M17): `devtools::check()` prints "0 notes" while `R CMD check`
  shows `Status: 1 NOTE` — the `spelling.Rout` NOTE for new technical terms is
  masked. Run `spelling::update_wordlist()`; confirm `Status: OK` in `00check.log`.
- 2026-07-12 (M18): a cli `{?s}` governed by a `{.val {vector}}`/`{cli::qty(vec)}`
  throws `length(object) == 1` with 2+ items in a multi-line `cli_warn`/`abort`.
  Drive plurals off a scalar `{length(x)}` and list the vector without `{?s}`.
  Test cli count messages with 2+ items — a 1-item test hides the crash.
- 2026-07-12 (M19): a fast-path branch that skips the shared batch runner (`ffm_batch`) must re-synthesize that runner's *opt-in* outputs (`verified` column, manifest attr) or the return schema silently diverges from the normal path across calls. Trick: subsetting a canonical 0-row schema tibble pads to N all-NA rows for free (`col[rep(NA_integer_, n)]` gives n type-matched NAs). Test parity by comparing `names()`/types of the fast vs the normal path.
- 2026-07-13 (M23): a public-API rename/un-export must also sync `_pkgdown.yml`
  and live vignette/example chunks — neither is caught by `devtools::check()`
  (use `pkgdown::check_pkgdown()`; a chunk calling a now-internal fn fails only at
  vignette-build). Grep `vignettes/` + roxygen `@examples` before dropping `@export`.
- 2026-07-13 (M24): `devtools::build_readme()` emits a spurious README.Rmd diff
  (`system.file()` example paths embed the temp libpath); revert path-only churn.
- 2026-07-13 (M27): FFmpeg per-stream metadata (`-metadata:s:v:0 title=`) surfacing
  in mov stream tags is ffmpeg-version dependent (became `name` on 8.x macOS,
  absent on Ubuntu CI) — green locally + macOS, red on Ubuntu. Don't sanity-assert
  an injected per-stream tag's *presence*; assert only on the stripped *output*.
- 2026-07-13 (M28): extracting a shared helper *between* a documented function's
  `#'` roxygen block and its `fn <- function` line silently re-targets the roxygen
  to the helper and drops the original's `.Rd` — `document()` warns "Deleting
  <fn>.Rd". Put the extracted helper ABOVE the roxygen block, not between it and
  the function.
- 2026-07-26 (M31): skip a hardware-encoder execution test on run-time
  usability, not merely that the encoder is *listed* — CI lists `h264_nvenc`
  without a GPU, so probe a 1-frame lavfi encode and skip unless it exits 0.
- 2026-07-12 (M30): a `*_batch` verb's jobs tibble keys on `input`/`output`
  *columns* (via `check_batch_jobs`), NOT the scalar verb's `infile`/`outfile`
  *argument* names — an easy mismatch in vignette/example chunks that errors only
  at build. Render vignettes with the ffmpeg/ffprobe/mediainfo binaries masked
  off PATH (`Sys.which()==""`) to reproduce the CI-absent build and catch it.
- 2026-07-26 (M32): a `_batch` per-row override *column* skips the scalar's arg guards (`check_number_whole`/range) — re-validate each override column per row.
- 2026-07-26 (M34): guarding a `_batch` override column whose documented `NA` means "unset" — R types an all-NA column *logical*, so an `is.character`/`is.numeric`-only guard wrongly rejects it, while the usual patch `!is.character(x) && !all(is.na(x))` over-corrects and admits an all-NA numeric or Date. Spell it out: `is.character(x) || (is.logical(x) && all(is.na(x)))`, and test both boundaries.
- 2026-07-27 (M35): `R/ffmpeg.R` is the repo's only CRLF file, so editing it with anything that normalizes line endings (a Python `open(p, "w").write()` round-trip) silently rewrites all ~4000 lines — the diff reads 4172/3999 instead of the true 209/36 and `git blame` repoints the whole file at your milestone. Read and write it as bytes, restoring `\r\n`, and compare `grep -c $'\r'` against `git show <default-branch>:<file>` before committing.
- 2026-07-27 (M35): an execution test proving a stream copy survived must use a source codec that is NOT the output container's default, or copy and re-encode yield the same `codec_name` and the test passes either way. MP3-in-MP4 discriminates (copy keeps `mp3`, an unset codec yields `aac`); the AAC-in-MP4 `make_test_video()` fixture cannot.
- 2026-07-26 (M36): in a cli message `{.val NULL}` renders the *string* `"NULL"` — quoted, reading as a value to pass — and any argument validated by `check_token()` accepts `"NULL"` and forwards it to FFmpeg, so the hint causes a second failure. Write a NULL sentinel as `{.code NULL}`; every other NULL reference in the package already does.
- 2026-07-26 (M36): when a check is hoisted into a two-pass/batch prelude to fail before an expensive analysis phase, hoist *every* check on that argument — hoisting only the one in mind (here `"copy"`, leaving `check_token()` in the Phase-2 pipeline) still burns one analysis pass per row on a typo.
