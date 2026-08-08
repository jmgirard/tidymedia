# M63: An unreadable input is refused where a missing one already is

**Status:** done (2026-08-08, PR #66 https://github.com/jmgirard/tidymedia/pull/66)

**Goal:** Make the front-door input guard refuse exactly what `ffm_files()`
refuses, so an input that exists but cannot be read reports against the verb
called rather than from inside the pipeline.

**Outcome:** `check_paths_readable()` (`R/utils.R`) replaces
`check_paths_exist()`, holding `ffm_files()`' own `file.access(mode = 4)`
predicate, and `ffm_files()` reaches that site rather than wording a second
refusal. `check_file_readable()` carries the thirteen scalar input verbs;
`check_file_exists()` stays existence-only for `verify_media()` and
`write_mediainfo_template()`. One message covers both conditions — "can't be
found or read" — moving the wording on verbs already refusing missing inputs.

**Decisions:** D041 (one predicate, both ends reach it; closes D040's disclosed
residual). AC4 amended twice at gates: the refusal set does not grow, and the
170 wording-only changes are intended.

**Review:** Three lenses, 32 findings, four at or above 80, none an AC failure —
stale roxygen on the two fan-in verbs (85), D041's directory claim false of an
unreadable directory (82), the tests skipping silently on root or Windows
(80/80); all fixed, 28 logged below. Evidence: 584 grid cells over two refs with
every reader empty, two mutations each reddening their own tests.
