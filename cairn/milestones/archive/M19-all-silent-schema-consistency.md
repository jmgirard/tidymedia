# M19: Consistent schema for an all-silent two-pass batch

- **Status:** done · **Depends on:** M18 · **PR:** https://github.com/jmgirard/tidymedia/pull/21

## Goal

Make `normalize_audios(two_pass = TRUE)` return the same opt-in schema when
*every* row is silent as it does for a mixed batch, closing the D011 schema
inconsistency the M18 review surfaced (candidate, scored 78).

## Outcome

An all-silent two-pass batch now synthesizes the opt-in outputs a mixed batch
produces instead of dropping them: under `run = TRUE`, an all-`NA` logical
`verified` column when `verify=` was supplied, and a padded one-row-per-job
manifest when `manifest=` was supplied (input paths recorded, other columns
type-matched `NA`, `input_md5`/`output_md5` iff `checksums=TRUE`). New
`manifest_schema(checksums)` is the canonical column template `build_manifest()`
derives from (drift guard); the all-silent path pads it via the existing
`expand_manifest_rows()`. `bind_two_pass_result()` gained `verify`/`manifest`/
`checksums` params; `normalize_audios()` threads that intent from `...`.
Single-pass/scalar/mixed behavior unchanged; `run = FALSE` synthesizes nothing.

check 0/0/0 (`Status: OK`); CI 7/7; 6 new tests. Independent review clean; one
pre-existing note logged below threshold (invalid `verify=` unvalidated there).
