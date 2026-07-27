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
- 2026-07-13 (M24): `devtools::build_readme()` emits a spurious README.Rmd diff (`system.file()` example paths embed the temp libpath); revert path-only churn.
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
- 2026-07-26 (M37): removing an argument from a `_batch` verb is only half a clean break — the verb's `...` swallows the retired *argument* silently and the N→2N reshape drops a retired *column* silently, so both spellings need a guard naming the replacement. The scalar sibling needs none: with no `...`, R's own `unused argument` covers it.
- 2026-07-26 (M37): resolving a scalar arg into a per-row column at the reshape *skips the arg's own type check*, because the column is then validated per row by whatever the pipeline runs (here `check_token`, which accepts `"TRUE"`). The bug is invisible until a jobs table happens to carry that column. Validate the argument at the front door, and test the same bad value with and without the column present.
- 2026-07-26 (M38): an `arg_match()` argument compared before it is resolved silently misreads its own default — `hardware = c("none","nvenc")` against `identical(hardware, "none")` is FALSE, firing a guard on every default call. Resolve at the front door, before any guard or pipeline reads the value.
- 2026-07-26 (M38): a `cli_abort()` remediation hint must be true under the condition that FIRED the guard, not in general — the M38 guard only fires under `hardware = "nvenc"`, where the `NULL` codec sentinel assumes H.264 rather than deferring to the container, so the general-case hint walked a `.webm` caller into an `h264_nvenc`-in-WebM command. Check a hint against the branch that reaches it.
- 2026-07-26 (M39): a `_batch` verb's batch-wide argument needs a test naming a NON-default value — asserting only the default passes even when the argument never reaches the fan-out, because the shared pipeline carries the same default. Prove the test discriminates by mutating the fan-out to ignore the argument: it must go red.
- 2026-07-27 (M40): moving a verb onto a SHARED column guard imports that guard's remediation hint, which can be false for the new caller even though it was true for every existing one — `check_batch_codec_col()` says "`NA` to leave the codec unset", but on `convert_audio_batch` `NA` selects `-q:a 0`. M38's rule is about the branch you wrote; this is the hint going stale because a caller was ADDED. Parameterize the wording (`na_means =`) and assert both the true string and the absence of the default one.
- 2026-07-26 (M39): adding an override column to a `_batch` verb also falsifies its `@param jobs` prose, which enumerates the honoured columns and closes "Any other columns are ignored" — a reader who believes that adds the column as a note-to-self and silently re-encodes every row. Update the enumeration, not just the new `@param`.
