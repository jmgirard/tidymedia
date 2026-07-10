# M04: Metadata layer polish (mediainfo/ffprobe tibbles)

- **Status:** planned <!-- mirror of ROADMAP.md; ROADMAP wins on conflict -->
- **Created:** 2026-07-10
- **Completed:** —

## Goal

Make the metadata layer deliver the D001 headline — *metadata as tibbles,
batch over many files* — the way M02/M03 made the pipeline engine real. Today
the readers are single-file, return all-character columns, disagree on their
identifier column (`File` vs `filename`), shell out through `glue()`+`system()`,
and carry live bugs (the `probe_*(infile=)` shortcuts return `NULL`). This
milestone reworks `R/mediainfo.R` and `R/ffprobe.R` into vectorized, typed,
schema-consistent tibble readers with safe shell-out, and fixes the bugs.

## Scope

**In:**
- Vectorize every reader over a vector of files → one stacked tibble (row per
  file; streams: row per stream) with a leading `file` column.
- Type-convert by **default** (numeric columns typed, `"N/A"`→`NA`), with a
  raw/character opt-out.
- Unify the output schema across both backends: leading `file` column,
  consistent casing.
- Fix bugs: `probe_container/streams/video/audio(infile=)` returning `NULL`;
  `type.convert()` without `as.is`; `eval(parse())` in `convert_fractions()`;
  `format_probe()` key/value split (superseded `separate`, `=`-in-value); the
  `1:nb_streams` / 0-stream loop guard.
- Safe shell-out: readers invoke the CLIs via argument vectors (`system2`), not
  `glue()` into a shell string, so paths/params with spaces/quotes/`$` work
  (resolves the metadata-layer half of the deferred M02 F6 / M03 F4).

**Out:**
- No new metadata backends (no exiftool, etc.) — still just mediainfo/ffprobe.
- No changes to the `ffm_*` builder, task verbs, or Layer 1/2.
- The Layer 0 string escape hatches `mediainfo(command)` / `ffprobe(command)`
  keep their raw-string signature (D002) — only internal readers move to
  arg vectors. Quoting for `ffmpeg()` and the builder stays deferred.
- No metadata caching, no pkgdown/vignettes/README prose (that is M05).

## Acceptance criteria

Each verifiable with evidence at review time.

- [ ] `probe_all(c(f1, f2))` returns `container`/`streams` tibbles with one row
      per file / per stream and a leading `file` column (test).
- [ ] `mediainfo_template()`, `mediainfo_query()`, `mediainfo_parameter()`
      accept a vector of files and stack results (test).
- [ ] `probe_container(infile = f)` (and `_streams/_video/_audio`) return the
      correct non-`NULL` tibble — regression test for the fixed bug.
- [ ] Default reader output is typed: a numeric column returns double/integer
      and `"N/A"`→`NA`; a `raw`/opt-out path returns character (test asserts
      column types both ways).
- [ ] Every reader's tibble leads with a `file` column; casing consistent
      across mediainfo + ffprobe (test asserts names).
- [ ] `convert_fractions("30000/1001")` ≈ 29.97 without `eval(parse())`, and
      malformed input errors via `cli::cli_abort` (test).
- [ ] Readers handle a file path containing a space and a quote without
      breaking (binary-gated test).
- [ ] `devtools::test()` clean; no `type.convert`/`separate` deprecation
      warnings; `devtools::check()` 0 errors / 0 warnings / 0 notes; NEWS
      updated under the development version.

## Plan

Tasks sized to one working session or less, ordered by dependency.

- [ ] T1: Internal safe runner — `system2`-based arg-vector shell-out helper for
      the readers; keep `mediainfo()`/`ffprobe()` string escape hatches intact.
      Quoting tests (space/quote/`$` in path).
- [ ] T2: ffprobe rework — fix `probe_*(infile=)`; robust key/value parse
      (`separate_wider_delim`, `too_many="merge"`); 0-stream guard; safe
      `convert_fractions()`; typed-by-default with opt-out; leading `file`
      column. Tests.
- [ ] T3: mediainfo rework — `check_file_exists` in `mediainfo_parameter()`;
      `type.convert(as.is=TRUE)`; typed-by-default with opt-out; leading `file`
      column. Tests.
- [ ] T4: Multi-file vectorization for both backends (stack row/rows per file)
      + shared schema-unification helper. Tests assert names + `nrow`.
- [ ] T5: roxygen updates + `devtools::document()`; NEWS under dev version;
      `devtools::check()` 0/0/0.

## Work log

Append-only; newest last. One line per session: date, what happened, next.

- 2026-07-10: Milestone planned (4 scope decisions taken at plan gate).

## Decisions

Milestone-local; promote cross-cutting ones to ../DECISIONS.md at review.

- D-M04-1 (plan gate): Vectorize readers over a file vector → stacked tibble
  (row per file), rather than a separate batch runner. Extends D007's
  tibble-in/out spirit to metadata.
- D-M04-2 (plan gate): Typed conversion is the default output (raw opt-out);
  breaking, acceptable pre-release.
- D-M04-3 (plan gate): Safe arg-vector shell-out (`system2`) for the metadata
  readers; Layer 0 string escape hatches unchanged. Resolves metadata half of
  deferred F6/F4; whole-layer quoting still deferred.
- D-M04-4 (plan gate): Unify output schema — leading `file` column, consistent
  casing across both backends.

## Review

Filled in by `/milestone review`.

- Criteria verification:
- check()/test()/coverage results:
- Follow-ups spawned:
