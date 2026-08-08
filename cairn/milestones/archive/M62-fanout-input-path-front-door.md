# M62: A missing input file is refused at the front door, in both forms

**Status:** done (2026-08-08, PR #65 https://github.com/jmgirard/tidymedia/pull/65)

**Goal:** Make a call naming an input file that does not exist report against
the verb the user called, in both the table-driven and the scalar form.

**Outcome:** `check_paths_exist()` (`R/utils.R`) is the one site the abort is
written; `check_file_exists()` delegates its existence half, so a one-path call
renders byte-identically to before. `check_batch_inputs()` sweeps a jobs
table's carriers (`input`, D015's `inputs` list-column, `main`/`overlay` in one
call), coercing and deduplicating, at 16 fan-out verbs plus
`concatenate_videos()`/`compare_videos()`, which had none. It sits below each
verb's shape and column-type guards, above its M58 contradiction sweep;
`ffm_files()` keeps the readability refusal M63 unifies.

**Decisions:** D040 (licenses the front-door filesystem read under D024's third
exclusion; D035's shape, not its licence). M62-D1: the ordering claim is stated
over the after ref alone. M62-D2: "names every missing path" is about a carrier.

**Review:** Two rounds — round 1 returned three defects (F1 92, F2 88, F3 82),
round 2 nothing at threshold across 14. Evidence: 524 generated cells over two
refs, falsified against the pre-fix ref and by mutation.
