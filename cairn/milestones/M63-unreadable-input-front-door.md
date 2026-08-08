# M63: An unreadable input is refused where a missing one already is

- **Status:** planned
- **Priority:** normal
- **Depends on:** M62
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Make the front-door input guard refuse exactly what `ffm_files()` refuses, so an
input that exists but cannot be read reports against the verb the user called
rather than from inside the pipeline.

## Scope

**In:** Upgrading M62's shared front-door checker from `file.exists()` to
`ffm_files()`'s own `file.access(mode = 4)` predicate, at every site M62 wired
it into and at the thirteen scalar `check_file_exists(infile)` sites that
predate M62. The message change that follows, since "does not exist" is false of
a file that is there and unreadable. `ffm_files()` reaching the same shared site,
so the front door and the pipeline cannot drift.

**Out:** Any widening of the predicate beyond readability — a probe that opens
or decodes the file → no row; that is a decode gate, not an input check, and
D024's exclusions govern it separately. `check_file_exists()`'s two non-input
callers (`R/verify.R:53`, `R/mediainfo.R:203`, a template file) → they keep
existence semantics; the milestone states why rather than sweeping them.

## Acceptance criteria

- [ ] AC1 — The front-door guard and `ffm_files()` refuse the same set of paths,
      shown by a property test over a generated path corpus covering the four
      cases the two predicates distinguish: present-and-readable, absent,
      present-and-unreadable, and a directory in place of a file.
- [ ] AC2 — Every exported verb in M62's two walk-derived sets — those reaching
      `ffm_batch`, and those reaching `ffm_files` but not `ffm_batch` — refuses
      an existing-but-unreadable input at its front door with `conditionCall()`
      naming that verb, reusing M62's spec-required construction so the walk
      still fixes membership.
- [ ] AC3 — The message states what is wrong without asserting absence: a test
      asserts the unreadable-but-present case does not render "does not exist",
      and asserts the string it does render.
- [ ] AC4 — `data-raw/input-guard-baseline.R` is extended with the
      present-but-unreadable form as an additional declared axis, not as
      hand-added cells, and `input_guard_uncovered()` reports no uncovered
      combination. Measured over both refs: the set of refused calls grows by
      exactly the unreadable-but-present cases and by nothing else.
- [ ] AC5 — The M62 D-entry's recorded residual is closed by an appended entry
      naming it, rather than left standing.
- [ ] AC6 — `NEWS.md` records the message change for the thirteen scalar verbs
      whose wording moves, and a named test fails without it.
- [ ] AC7 — `devtools::document()` produces no diff, `devtools::test()` and
      `devtools::check()` are clean (0 errors, 0 warnings; NOTEs justified).

## Coverage

- AC1 → T1, T2
- AC2 → T2, T3
- AC3 → T1, T3
- AC4 → T4
- AC5 → T5
- AC6 → T5
- AC7 → T5

## Tasks

- [ ] T1 — Swap the shared checker's predicate to `file.access(mode = 4)` and
      re-word its message; keep the abort at one site and have `ffm_files()`
      reach it.
- [ ] T2 — Build the four-case path corpus and the property test pairing the
      front door against `ffm_files()`.
- [ ] T3 — Retarget the thirteen `check_file_exists(infile)` scalar sites; state
      in a comment why `R/verify.R:53` and `R/mediainfo.R:203` keep existence
      semantics.
- [ ] T4 — Extend the M62 grid's declaration with the unreadable-but-present
      axis; re-run against both refs.
- [ ] T5 — Appended D-entry closing M62's residual, `NEWS.md`, roxygen; then
      `document()` / `test()` / `check()`.

## Work log

- 2026-08-08: created by /milestone-plan, split from M62 at the plan gate.
- 2026-08-08: plan gate chose splitting the readability upgrade out of M62 over doing both in one milestone, because the upgrade changes an existing message on thirteen working verbs and roughly doubles the measurement grid; falsified by M62 shipping and the residual proving indistinguishable to callers, which would mean the split bought nothing.

## Decisions

## Review
