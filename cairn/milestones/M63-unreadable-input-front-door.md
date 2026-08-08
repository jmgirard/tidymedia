# M63: An unreadable input is refused where a missing one already is

- **Status:** review
- **Priority:** normal
- **Depends on:** M62
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m63-unreadable-input-front-door` · PR #66 https://github.com/jmgirard/tidymedia/pull/66

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
- [x] T5 — Appended D-entry closing M62's residual, `NEWS.md`, roxygen; then
      `document()` / `test()` / `check()`.

## Work log

- 2026-08-08: created by /milestone-plan, split from M62 at the plan gate.
- 2026-08-08: review — PR #66 opened; three fresh-context lenses returned 32 candidate findings, four scored at or above 80, none an acceptance-criterion failure. Fixed on the branch: stale roxygen on the two fan-in verbs (F1), D041's false directory claim plus the corpus case that would have caught it (F2), and the silent skip that let the whole readability axis vanish on a root or Windows runner (F3). Re-verified after: `test()` 0 failures / 4936 passes, `check()` 0/0/0, grid re-run with every reader empty.
- 2026-08-08: in-progress on `m63-unreadable-input-front-door`, cut from origin/master at f4357e7.
- 2026-08-08: T5 — D041 appended, closing the residual D040 disclosed; `NEWS.md`'s "not yet covered" paragraph replaced by the shipped behavior and the wording move, quoted against the byte-for-byte pin in `test-input-path-front-door.R`. `document()` produced no diff.
- 2026-08-08: T4 — `unreadable` declared as a fifth form at the `none` crossing; 584 cells per ref, 484 live, 30 of them unreadable. Every reader empty over origin/master vs the branch; 200 messages changed, 170 by the declared re-wording and 30 at blame-moved cells, and the blame-moved set is exactly the 30 unreadable cells (reported `ffm_files` before, the verb after; no cell reports `ffm_files` after).
- 2026-08-08: T4 — two instrument defects found by running it: the form counted two paths on a one-slot verb whose call shape then dropped the second, so `input_guard_unnamed()` held twelve cells to naming a path never passed; and the re-wording comparison ran over raw text, which cli wraps at a different word per ref, so 170 intended changes read as regressions. Both fixed in the declaration rather than exempted.
- 2026-08-08: amendment (gated) — AC4 refined a second time, after measurement: the earlier text's "and nothing else" excluded the 170 wording-only changes the milestone intends, so the criterion now states three separately-measured claims (fate, message, blame).
- 2026-08-08: T1/T2/T3 — `check_paths_readable()` is the one site (`file.access(mode = 4)`, renamed from `check_paths_exist()` since the predicate moved); `check_file_readable()` carries the thirteen scalar input sites and `ffm_files()` reaches the same site, so its own refusal is deleted rather than duplicated. `check_file_exists()` keeps existence for its two non-input callers with the reason in a comment.
- 2026-08-08: T2 — the four-case corpus is present-readable / absent / present-unreadable / directory; the directory case measured as accepted by BOTH predicates, so the property test asserts agreement and pins which cases split, or the identity would hold vacuously. The unreadable fixture verifies itself with the guard's own predicate and skips where a mode-000 file is still readable.
- 2026-08-08: T3 — two sibling tests matched the retired wording: `test-ffmpeg.R:418` failed, and `test-normalize-audio.R:111` passed only because "does_not_exist.mp4" contains "exist"; both now match the wording.
- 2026-08-08: amendment (gated) — AC4's "the set of refused calls grows" is falsified by measurement: an existing-but-unreadable input already aborts inside the pipeline (`ffm_files()` for a scalar verb, `purrr::pmap()` for a batch one), so the criterion now reads that no call's fate changes and exactly those cases' blame and message move.
- 2026-08-08: implementation gate chose ONE wording covering both conditions ("can't be found or read") over one wording per condition, accepting that the thirteen scalar verbs' existing missing-file text moves; the per-condition option costs six renderings where the shared site has two, and the count form would need a third for a call carrying both.
- 2026-08-08: T5 — `devtools::check()` clean (0 errors, 0 warnings, 0 notes, 3m 27s) and `devtools::test()` 0 failures / 4 pre-existing warnings / 5 nvenc skips; status to review.
- 2026-08-08: incidental — the M62 review's N1 (the site test matched a string only the one-path literal contained) no longer holds: both renderings now carry "can't be found or read", so the site test is red against either branch alone. The other three findings in that ROADMAP row are untouched.
- 2026-08-08: plan gate chose splitting the readability upgrade out of M62 over doing both in one milestone, because the upgrade changes an existing message on thirteen working verbs and roughly doubles the measurement grid; falsified by M62 shipping and the residual proving indistinguishable to callers, which would mean the split bought nothing.

## Decisions

## Review

Reviewed 2026-08-08 on `m63-unreadable-input-front-door` at PR #66. Evidence is
fresh: every command below was run in this session against the branch.

**Falsification, run twice at file level and reverted after each.** Reverting
the predicate to `file.exists()` (wording kept) reddens exactly the three tests
that own the readability claims — the property test, the absence-of-"does not
exist" test, and the per-verb unreadable sweep. Reverting the wording to M62's
(predicate kept) reddens thirteen, including the byte-for-byte pin and both
site-uniqueness tests. Neither mutation passes.

- [x] AC1 — `test_that("the front door and ffm_files() refuse the same set of
      paths")`: for each corpus case the three refusal answers (shared checker,
      `ffm_files()`, `standardize_video()`) are asserted identical, and the
      corpus is asserted to split (absent and unreadable refused; present and a
      readable directory accepted). Corpus extended at review to five cases —
      an unreadable directory was added, being the case the two predicates
      actually differ on. Red under the predicate mutation.
- [x] AC2 — `test_that("every verb refuses an unreadable input at its own front
      door")` quantifies over `input_guard_verbs()`'s two walk-derived sets
      through M62's `input_guard_specs()`; 30 verbs × 4 assertions (message,
      `conditionCall()` naming the verb, and the absence of `pmap` and
      `ffm_files` from the blame). Red under the predicate mutation.
- [x] AC3 — `test_that("the message does not assert absence of a file that is
      there")`: `expect_false(grepl("not exist", msg))` on a present-unreadable
      path, plus the rendered string asserted in both arities. Red under the
      wording mutation.
- [x] AC4 — `unreadable` is declared in `INPUT_GUARD_FORMS` +
      `INPUT_GUARD_FORM_CROSSINGS` and generated, not hand-added. Grid re-run
      over `origin/master` and the branch after the review fixes: 584 cells per
      ref, 484 live; `input_guard_uncovered()`, `input_guard_refusals()`,
      `input_guard_message_regressions()`, `input_guard_blame_unexpected()`,
      `input_guard_blame_regressions()`, `input_guard_missing_call()`,
      `input_guard_dead_controls()`, `input_guard_misordered()`,
      `input_guard_unreported()`, `input_guard_unnamed()` and both
      `input_guard_vacuous()` runs all empty. 200 messages changed: 170 by the
      declared re-wording, 30 at blame-moved cells, and the blame-moved set is
      exactly the 30 unreadable cells.
- [x] AC5 — D041 appended, quoting D040's residual paragraph and closing it;
      its directory claim was corrected at review (see F2 below).
- [x] AC6 — `NEWS.md`'s "not yet covered" paragraph replaced by the shipped
      behavior and the wording move, quoting both renderings; `ffm_files()`'
      own message change named. The named test is "the one-path rendering is
      pinned byte-for-byte", red under the wording mutation.
- [x] AC7 — `devtools::document()` no diff after the review's roxygen fix;
      `devtools::test()` 0 failures / 4 pre-existing warnings / 5 nvenc skips;
      `devtools::check()` 0 errors, 0 warnings, 0 notes. CI green on all 7
      checks at PR #66.

**Consistency gate.** `cairn_validate` all checks passed (exit 0);
`pkgdown::check_pkgdown()` no problems; `NAMESPACE`/`_pkgdown.yml` untouched
(no new exports); README unaffected; no new top-level files; changelog entry
present. No principle changed, so `cairn_impact` was not run.

**Independent review — three lenses, then a scorer.** 32 candidate findings
reported (20 diff-bug [O], 7 blame-history [S], 5 prior-review [S]); the
prior-PR-comments lens found the GitHub inline-comment probe empty and read the
archived `## Review` sections instead. Four scored ≥80, none of them an
acceptance-criterion failure, so no return; all three distinct defects were
fixed on the branch.

- F1 (85, fixed) — `concatenate_videos()` and `compare_videos()` roxygen still
  documented the guard as existence-only ("a path that does not exist"), and
  AC7's no-diff `document()` cannot catch prose that should have changed but
  was not edited. Both `@param infiles` blocks re-worded; `man/` regenerated.
- F2 (82, fixed) — D041 claimed "a directory passes both the old predicate and
  the new one", which is false of an unreadable directory: `file.exists()` is
  TRUE and `file.access(mode = 4)` is −1, so the new predicate refuses one the
  old accepted. The entry now says *readable* directory and states the
  unreadable case; the AC1 corpus gained an unreadable-directory case.
- F3 (80, fixed; reported twice, by the diff and history lenses) — all three
  M63 tests degraded to a silent `skip_if()` wherever a mode-000 file stayed
  readable, so the milestone's whole behavioural evidence could vanish on a
  root container with a green check. `tm_require_unreadable()` now skips only
  on Windows or as root and FAILS anywhere else, and `tm_unreadable_path()`
  fails rather than skips when `file.create()` itself fails.

**Below threshold, logged not actioned (28).** Highest first: C1 (70) the new
form inherits M62's N2 one-row-cell limit for `slots = 1L` verbs; A5 (68)
`tm_refused_input()` uses `catch_cnd()`'s default `classes = "condition"`, so a
condition signalled before the abort would misclassify; A8 (68) the
site-uniqueness test now asserts only that the retired wordings are gone and
fences no new third wording; A9 (68) `input_guard_reword()` applies its
substitutions to every message rather than keying on the `input` class; A11
(65) `input_guard_blame_unexpected()` mislabels a never-refused cell as a
blame problem; A4 (62) no cell mixes an absent and an unreadable path, the case
the merged wording exists for; B4 (58) N1's differentiator is bypassed rather
than restored; A16 (55) `file.access()` tests the real uid where `open()` uses
the effective one, and D041 does not say so; A2 (55) `ffm_files()`' direct
message shape is not pinned by a test; C2 (55) the ROADMAP row still lists N1
as open; A13 (48) the unreadable fixtures inherit the `absent` vector's
three-path ceiling; A7 (45) the corpus pins "a readable directory is accepted",
an open question; B1 (45) `multiple = TRUE` is hardcoded for `ffm_files()`; B5
(40) the wording exemption widens a strict M62 reader; A14 (35), A10 (32), A18
(28), A15 (28), A17 (25), A12 (22), A20 (20), A19 (15), and six findings the
lenses themselves reported as verified non-regressions (B2, B3, B7, C3, C4,
C5), each scored 8.
