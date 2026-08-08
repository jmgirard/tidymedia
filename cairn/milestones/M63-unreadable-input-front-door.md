# M63: An unreadable input is refused where a missing one already is

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M62
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m63-unreadable-input-front-door`

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
      combination. Measured over both refs: no call's fate changes; every
      message that changes does so either by the one declared re-wording or
      at a cell whose blame moved; and the cells whose blame moved are
      exactly the unreadable ones.
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

- [x] T1 — Swap the shared checker's predicate to `file.access(mode = 4)` and
      re-word its message; keep the abort at one site and have `ffm_files()`
      reach it.
- [x] T2 — Build the four-case path corpus and the property test pairing the
      front door against `ffm_files()`.
- [x] T3 — Retarget the thirteen `check_file_exists(infile)` scalar sites; state
      in a comment why `R/verify.R:53` and `R/mediainfo.R:203` keep existence
      semantics.
- [x] T4 — Extend the M62 grid's declaration with the unreadable-but-present
      axis; re-run against both refs.
- [ ] T5 — Appended D-entry closing M62's residual, `NEWS.md`, roxygen; then
      `document()` / `test()` / `check()`.

## Work log

- 2026-08-08: created by /milestone-plan, split from M62 at the plan gate.
- 2026-08-08: in-progress on `m63-unreadable-input-front-door`, cut from origin/master at f4357e7.
- 2026-08-08: T4 — `unreadable` declared as a fifth form at the `none` crossing; 584 cells per ref, 484 live, 30 of them unreadable. Every reader empty over origin/master vs the branch; 200 messages changed, 170 by the declared re-wording and 30 at blame-moved cells, and the blame-moved set is exactly the 30 unreadable cells (reported `ffm_files` before, the verb after; no cell reports `ffm_files` after).
- 2026-08-08: T4 — two instrument defects found by running it: the form counted two paths on a one-slot verb whose call shape then dropped the second, so `input_guard_unnamed()` held twelve cells to naming a path never passed; and the re-wording comparison ran over raw text, which cli wraps at a different word per ref, so 170 intended changes read as regressions. Both fixed in the declaration rather than exempted.
- 2026-08-08: amendment (gated) — AC4 refined a second time, after measurement: the earlier text's "and nothing else" excluded the 170 wording-only changes the milestone intends, so the criterion now states three separately-measured claims (fate, message, blame).
- 2026-08-08: T1/T2/T3 — `check_paths_readable()` is the one site (`file.access(mode = 4)`, renamed from `check_paths_exist()` since the predicate moved); `check_file_readable()` carries the thirteen scalar input sites and `ffm_files()` reaches the same site, so its own refusal is deleted rather than duplicated. `check_file_exists()` keeps existence for its two non-input callers with the reason in a comment.
- 2026-08-08: T2 — the four-case corpus is present-readable / absent / present-unreadable / directory; the directory case measured as accepted by BOTH predicates, so the property test asserts agreement and pins which cases split, or the identity would hold vacuously. The unreadable fixture verifies itself with the guard's own predicate and skips where a mode-000 file is still readable.
- 2026-08-08: T3 — two sibling tests matched the retired wording: `test-ffmpeg.R:418` failed, and `test-normalize-audio.R:111` passed only because "does_not_exist.mp4" contains "exist"; both now match the wording.
- 2026-08-08: amendment (gated) — AC4's "the set of refused calls grows" is falsified by measurement: an existing-but-unreadable input already aborts inside the pipeline (`ffm_files()` for a scalar verb, `purrr::pmap()` for a batch one), so the criterion now reads that no call's fate changes and exactly those cases' blame and message move.
- 2026-08-08: implementation gate chose ONE wording covering both conditions ("can't be found or read") over one wording per condition, accepting that the thirteen scalar verbs' existing missing-file text moves; the per-condition option costs six renderings where the shared site has two, and the count form would need a third for a call carrying both.
- 2026-08-08: plan gate chose splitting the readability upgrade out of M62 over doing both in one milestone, because the upgrade changes an existing message on thirteen working verbs and roughly doubles the measurement grid; falsified by M62 shipping and the residual proving indistinguishable to callers, which would mean the split bought nothing.

## Decisions

## Review
