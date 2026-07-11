# M04: Metadata layer polish (mediainfo/ffprobe tibbles) (done 2026-07-10)

**Goal:** deliver the D001 headline (*metadata as tibbles, batch over many
files*) for the metadata layer and fix its live bugs.

**Outcome (PR #4, squash-merged as 9ca0ab3; CI green ×6):**
- `R/ffprobe.R` + `R/mediainfo.R` reworked into vectorized, typed,
  schema-consistent tibble readers: `probe_all()`/`probe_*()`,
  `mediainfo_query/template/parameter()`, `get_*()` accept a file vector and
  return one `file`-keyed stacked tibble.
- New internal `run_program()` (safe `system2()` arg vectors, resolves metadata
  half of deferred F6/F4) + shared hex-guarded `type_columns()`. Layer 0
  `mediainfo()`/`ffprobe()` string hatches untouched.
- Bugs fixed: `probe_*(infile=)` returned `NULL`; `convert_fractions()`
  `eval(parse())`; `format_probe()` `=`-split (dropped `tidyr::separate`);
  zero-stream guard. Opus review added 2 resilience fixes (`probe_video/audio`
  + `mediainfo_read` on unreadable input) + `nb_streams` guard.
- Breaking (pre-release): `typed=` replaces `probe_all(convert=)`;
  `filename`→leading `file`; dropped `-pretty`; both templates snake_cased;
  dropped unused `tidyr`. NEWS → 0.0.0.9003.
- Evidence: test 234 pass / 4 skip, check 0/0/0; mediainfo execution on CI
  (absent locally) + mocked-runner tests.
**Key decisions:** D-M04-1..7 — vectorize readers, typed default, safe
shell-out, `file`-led snake_case schema (verbatim user names), multi-file
resilience (D007-consistent). None promoted to DECISIONS.md (all layer-local).
