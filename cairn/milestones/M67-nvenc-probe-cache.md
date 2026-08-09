# M67: The encoder probe answers once per session, not once per row

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP1
- **Branch/PR:** `m67-nvenc-probe-cache` · https://github.com/jmgirard/tidymedia/pull/70

## Goal

A `hardware = "nvenc"` call asks FFmpeg which encoders it has once per session
instead of once per row, without pinning a caller to a stale answer.

## Scope

**In:** a session-scoped memo of the encoder-name pool, sited inside
`has_nvenc()` (`R/ffmpeg.R:2519-2524`) *below* its `getOption()` seam, so the
N+1 `ffmpeg -encoders` spawns an N-row nvenc batch makes today become one. An
exported, documented call that discards the memo, plus a discard inside
`set_program()` (`R/program_management.R:141-159`). A `setup-` hook so the memo
never leaks between test files. A D-entry fixing the lifetime.

**Out:**
- Memoizing `find_ffmpeg()`/`Sys.which()`, so one `which` spawn per row
  survives → ROADMAP candidate row (it reaches every execution path in the
  package, not just the nvenc probe, and wants its own D-entry).
- Making the memo visible across `parallel = TRUE` workers — it is per-process,
  so a W-worker batch probes W times, not once. Disclosed in the D-entry and
  the docs; the seed-the-workers design → ROADMAP candidate row.
- Caching `ffmpeg_encoders()` or `ffmpeg_codecs()` themselves; AC5 pins the
  first as *uncached* on purpose, so a caller always keeps a fresh route.
- Caching the M31 *usability* answer (listed ≠ runnable,
  `tests/testthat/helper-skip.R:25-50`) → stays out; nothing here probes it.
- Any GPU-hardware evidence: this milestone counts probes, it runs no encodes.

## Acceptance criteria

- [x] AC1 With the memo cold, the option seam unset, and `ffmpeg_encoders()`
      replaced by a counting stub reporting the nvenc encoders present (the
      `tests/testthat/test-nvenc-docs.R:71-75` pattern), the sequence
      `has_nvenc("h264")`, `has_nvenc("hevc")`, `has_nvenc("h264")` invokes the
      stub exactly once in total — cumulative count `1L` after each call. Two
      codecs, so a per-codec memo fails this.
- [x] AC2 Let **H** be `intersect(getNamespaceExports("tidymedia"), <names in
      the namespace whose `formals()` include `hardware`>)`, read at test time,
      never hand-listed. For every member of H, one `hardware = "nvenc"` call
      with the memo cold and the counting stub in place invokes the stub
      exactly once. Each cell's call is built from that function's own
      `formals()` — `parallel = FALSE` and a 3-row `jobs` table supplied only
      where those formals exist, `video_codec` set to a non-`"copy"` value
      wherever its default is `"copy"` — and each cell carries a control
      asserting the call did not abort (M41: a cell that aborts measures
      nothing).
- [x] AC3 Running AC2's whole grid in one session *without* resetting the memo
      between cells invokes the stub exactly once in total across every member
      of H — the memo is shared across functions, not per-function.
- [x] AC4 After the exported discard call, and separately after
      `set_program("ffmpeg", <path>)`, the next `has_nvenc()` invokes the stub
      again. Both routes are documented on the discard call's own help topic.
- [x] AC5 `ffmpeg_encoders()` stays uncached: two consecutive calls reach the
      `ffmpeg()` execution seam (`R/ffmpeg.R:20-30`, which shells `system()` —
      **not** `run_program()`) twice, counted by mocking `ffmpeg` itself.
- [x] AC6 The `getOption("tidymedia.nvenc_encoders")` seam is read before the
      memo on every call, so setting it mid-session takes effect at once and
      never reads or populates the memo. Evidence: a test that warms the memo,
      then sets the option to `character(0)`, and observes `has_nvenc()` return
      `FALSE` with no further stub invocation — plus the existing suite green
      across every file that stubs the seam.
- [x] AC7 The three D034 probe-counting tests — in
      `test-audio-stream-passthrough.R`, `test-audio-stream-format-web.R`, and
      `test-audio-stream-crop-segment.R`, each identified by its
      `before <- probes; expect_gt(probes, before)` assertion — measure D034's
      claim by discarding the memo before each measured call, and each goes red
      when the `has_nvenc()` call inside `check_nvenc_available()`
      (`R/ffmpeg.R:2643`) is deleted from a committed baseline. All three red,
      recorded. (The call at `R/ffmpeg.R:2571` is *not* the mutant: it is
      short-circuited at these tests' default `fallback = FALSE`.)
- [x] AC8 A `tests/testthat/setup-*.R` file discards the memo before each test,
      so no file inherits another's warm memo; `devtools::test()` clean.
- [x] AC9 A `cairn/DECISIONS.md` entry records the lifetime (session, discarded
      only explicitly or via `set_program()`), the per-process behavior under
      `parallel = TRUE`, and why this does not trip D034's falsifier. The
      `@param hardware` probe sentence gets one re-decided wording, applied to
      every topic `test-nvenc-docs.R:19-31` enumerates as carrying it, and that
      test re-run.
- [x] AC10 Profile `verify` clean and the review-time full check:
      `devtools::test()` clean, `devtools::document()` no diff,
      `pkgdown::check_pkgdown()` passes (the new export needs a
      `_pkgdown.yml` row), and `devtools::check()` reports `Status: OK` — the
      final status line, not the devtools "0 notes" summary, which masks the
      spelling NOTE (M17).

## Coverage

- AC1 → T1, T2
- AC2 → T1, T4
- AC3 → T1, T4
- AC4 → T1, T3, T5
- AC5 → T2
- AC6 → T1, T2
- AC7 → T1, T5
- AC8 → T2
- AC9 → T6
- AC10 → T6

## Tasks

- [x] T1 Add the memo: a package-local environment (the package has none today
      — no `zzz.R`, no `.onLoad`, no `new.env()` in `R/`) holding the encoder-
      name pool, read and written inside `has_nvenc()` (`R/ffmpeg.R:2519-2524`)
      strictly *below* the `getOption()` line, plus an internal discard helper.
      Tests first (AC1, AC6).
- [x] T2 Test scaffolding: a counting stub helper on the
      `test-nvenc-docs.R:71-75` pattern, the `setup-` discard hook (AC8), and
      the `ffmpeg_encoders()`-stays-uncached test (AC5).
- [x] T3 Export the discard call — name settled against D014's scheme, sited
      with the capability family (`_pkgdown.yml:108-116`) — and call it from
      `set_program()` (`R/program_management.R:141-159`). Roxygen documents
      both routes and the mid-session install story.
      *(RB tripwire: irreversible-api — a new permanent export; GP1 trades on
      it, so the D-entry must state the trade.)*
- [x] T4 The AC2/AC3 generated grid: derive H from the namespace, build each
      cell's call from its own `formals()`, per-cell abort control, cold-memo
      and warm-memo totals.
- [x] T5 Rework the three D034 probe-counting tests to discard-then-measure,
      then run the AC7 mutation from a committed baseline (M44: commit first,
      or `git checkout` reverts the feature and every mutant reads red for the
      wrong reason) and record all three red.
- [x] T6 D-entry, NEWS `## Performance` entry, the re-decided `@param hardware`
      sentence across the enumerated topics, `document()`, `check()`.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: plan gate chose a session memo with no cache key over keying the memo on the resolved FFmpeg path, because a path key recomputes `find_ffmpeg()` → `Sys.which()` → `system("which ffmpeg")` on every row, so one shell spawn per row survives and the milestone's own headline goes unmet — and because `find_program()` prefers `Sys.which()` over the `set_program()` config (`R/program_management.R:23-27`), the key cannot see a `set_program()` change while ffmpeg is on `PATH`, which is the very staleness it was meant to catch. Falsified by a report of a stale pool surviving a mid-session FFmpeg change that neither the discard call nor `set_program()` was used for.
- 2026-08-08: plan gate chose disclosing the per-process memo under `parallel = TRUE` over seeding workers from the parent, because seeding means the package *writing* `tidymedia.nvenc_encoders`, and that option is read-only from the package's side today (`R/ffmpeg.R:2521` is the only `getOption()` in `R/`; nothing in `R/` calls `options()`), so it would change what the seam means. Falsified by a measured parallel batch whose W-worker probe count is itself the reported stall.
- 2026-08-08: plan chose siting the memo below the `getOption()` seam over above it, because ~80 existing test call sites set that option to control the answer; a memo above it would make them order-dependent. Falsified by a test needing the memo to override an explicitly-set option.
- 2026-08-08: `cairn_validate` sizing tripwire fired (10 ACs > 7) and was deliberately not split. The obvious seam — memo in M67, exported discard call in M68 — would ship a session-scoped cache with no user-facing escape from a stale answer, which is precisely the open question the candidate row carried ("a user installing FFmpeg or a GPU driver mid-session must not be pinned to a stale answer"), so the split makes the first half worse than not shipping it. The count is inflated by one mechanism measured six ways (AC1/2/3/5/6) after the criteria audit forced precision, plus two boilerplate criteria (AC9 record, AC10 profile verify); six tasks, each under a session. Falsified by implement finding T1+T4 alone fills a session, which would mean the measurements, not the mechanism, are the milestone.
- 2026-08-08: criteria audit ([O], fresh context) returned draft-AC2 and draft-AC6 UNSATISFIABLE, draft-AC7 UNBOUNDED-PROMISE, and draft-AC1/AC3/AC5 AMBIGUOUS. All fixed before writing, none left for the gate: AC2's procedure narrowed to `getNamespaceExports()` ∩ `hardware` formal (the drafted `mget(ls(asNamespace()))` enumerated 29, thirteen of them internal pipelines, and `parallel = FALSE` is an unused-argument error for 7 of the 16 exported, while `separate_audio_video*` abort at their `video_codec = "copy"` default); the mutation criterion's mutant retargeted from `R/ffmpeg.R:2571` to `:2643`, since the former is short-circuited at `fallback = FALSE` and so killed nothing; "updated wherever it is now false" replaced with one re-decided wording over the topic set `test-nvenc-docs.R:19-31` enumerates; AC1 given a second codec so a per-codec memo fails it; AC5's counter moved off `run_program` onto `ffmpeg`, which is the seam `ffmpeg_encoders()` actually reaches.

- 2026-08-09: branch `m67-nvenc-probe-cache` cut from `origin/master`; status in-progress.
- 2026-08-09: implement question gate — the export is named `refresh_ffmpeg_capabilities()` (broad over `refresh_ffmpeg_encoders()`/`reset_nvenc_cache()`, so the split-off `find_ffmpeg()` memo can join it without a second permanent export under D014's clean-break policy), and the `@param hardware` sentence is extended rather than rewritten (the existing matched clause stays live, so `test-nvenc-docs.R`'s fence needs no re-anchoring).
- 2026-08-09: minor amendment (reorder) — T2's `setup-` hook and T5's discard-then-measure rework were done alongside T1 rather than after it, because the memo makes the three D034 probe-counting tests and `test-nvenc-docs.R:80` red the moment T1 lands, and the profile's `verify` slot requires a clean `devtools::test()` before any task is checked off. T5's mutation half is unchanged and still follows a committed baseline.
- 2026-08-09: T1–T4 — memo in `R/cache.R` (a package-local `new.env(parent = emptyenv())`, the package's first), read via `cached_encoder_names()` strictly below `has_nvenc()`'s `getOption()` seam; `refresh_ffmpeg_capabilities()` exported and called from `set_program()`; `setup-nvenc-memo.R` discards via testthat's state inspector, which is its only per-test hook; the AC2/AC3 grid derives H from the namespace and builds each cell from its own `formals()`, taking a batch verb's `jobs` columns from its scalar sibling's required formals. `devtools::test()` clean (5984 pass, 0 fail).
- 2026-08-09: T6 part — the re-decided `@param hardware` wording applied to all 16 topics, and `test-nvenc-docs.R` gained two assertions fencing the new half (the "remembered for the rest of the R session" clause and the `refresh_ffmpeg_capabilities` pointer) over the same enumerated topic set.
- 2026-08-09: T5 mutation, run from committed baseline c0cde20 — `R/ffmpeg.R:2654`'s `if (!has_nvenc(family))` replaced by `if (FALSE)`, deleting the probe inside `check_nvenc_available()`. All three D034 probe-counting tests went red (8 failures), and the first failure in each is the probe assertion itself, not an abort: `test-audio-stream-passthrough.R:225` reports `Expected \`probes\` > 0L. Actual comparison: 0 <= 0`, with `:230`/`:234` following, `test-audio-stream-format-web.R:182`/`:186`, and `test-audio-stream-crop-segment.R:349`/`:353`/`:360`. Mutant reverted with `git checkout R/ffmpeg.R`.
- 2026-08-09: T6 — D044 appended (lifetime, the two discard routes, the per-process `parallel = TRUE` behavior, why D034's falsifier is untripped, and the GP1 trade the new export takes); NEWS `## Performance` entry added; `document()` no diff, `pkgdown::check_pkgdown()` clean.
- 2026-08-09: AC10 — the first `devtools::check()` reported `Status: 1 NOTE` (spelling: "repoints", NEWS.md:254) while devtools' own summary line read "0 notes ✔", which is exactly the mask M17 recorded; NEWS reworded and re-run reports `Status: OK` (0 errors, 0 warnings, 0 notes, 4m 14s), with the spelling comparison OK and `testthat.R` OK. `cairn_validate` all checks passed, one advisory (the 10-AC sizing tripwire the plan already logged as a deliberate non-split); `cairn_budget` plan-owned body 132/149. Status → review.

- 2026-08-09: review — PR #70 opened, CI green on all 9 checks. Three fresh-context lenses: blame-history and prior-review returned zero findings; the diff-bug lens returned 13, scored by a fourth agent. One actioned (F1, 85) and fixed on the branch; F2 (55) also fixed because AC2 requires a working per-cell abort control and AC fencing forbids ticking against one shown inert; the other 11 logged in the Review section. No status return — F1 is a test-instrument defect scored below 90, and AC7 names three tests of which it is not one.

## Decisions

## Review

Reviewed 2026-08-09 on branch `m67-nvenc-probe-cache`, PR #70.

### Acceptance criteria evidence

- **AC1** — `test-nvenc-memo.R` "the encoder pool is asked for once per session,
  across codecs": with the memo cold, the option seam unset and `ffmpeg_encoders()`
  replaced by a counting stub, `has_nvenc("h264")` / `("hevc")` / `("h264")` leave
  the cumulative count at `1L` after each call (6 assertions, 0 failures). Its
  control, "a cold memo still reaches FFmpeg", discards and re-probes to `2L`, so
  `1L` means "asked once" and not "the stub never bound".
- **AC2** — `test-nvenc-memo-grid.R` "each verb asks FFmpeg once, from a cold
  memo": H read from the namespace at test time is 16 members (the eight scalar
  verbs and their eight `_batch` siblings); every cell probes exactly `1L` from
  cold, 32 assertions, 0 failures. Each cell's call is built from that function's
  own `formals()`. The per-cell abort control was repaired at review (F2 below):
  `expect_cell_ran()` now catches the condition, so an aborting cell records one
  *labelled* failure and the loop finishes the remaining cells — verified on a
  synthetic three-cell fixture where the middle cell aborts (1 failure,
  `error = FALSE`, all three cells reached).
- **AC3** — same file, "one answer serves every verb in a session": the whole
  grid run without discarding between cells invokes the stub exactly `1L` times
  across all 16 members (17 assertions, 0 failures). A per-function memo would
  read 16 here.
- **AC4** — `test-nvenc-memo.R`: after `refresh_ffmpeg_capabilities()` the next
  `has_nvenc()` re-probes (count 1 → 1 → 2); separately, after
  `set_program("ffmpeg", <path>)` with the config dir redirected to a tempdir,
  the same (1 → 1 → 2). Neither test skipped in this run. Both routes are
  documented on `?refresh_ffmpeg_capabilities`.
- **AC5** — same file, "ffmpeg_encoders() itself stays uncached": two consecutive
  `ffmpeg_encoders()` calls reach the `ffmpeg()` execution seam twice (counter
  1 then 2), counted by mocking `ffmpeg` itself rather than `run_program()`.
- **AC6** — two tests: a warmed memo followed by `options(tidymedia.nvenc_encoders
  = character(0))` returns `FALSE` with no further stub invocation; and an option
  set before any warming leaves `ls(.tm_capabilities)` empty, so the option path
  never populates the memo. Full suite green across every file that stubs the seam.
- **AC7** — mutation re-run fresh at review from committed baseline: replacing
  `R/ffmpeg.R:2654`'s `if (!has_nvenc(family))` with `if (FALSE)` turns all three
  named tests red — `test-audio-stream-passthrough.R` 3 failures,
  `-format-web.R` 2, `-crop-segment.R` 3 — all with `error = FALSE`, i.e. by
  assertion rather than by abort. Mutant reverted; tree clean.
- **AC8** — `tests/testthat/setup-nvenc-memo.R` discards via testthat's state
  inspector, which runs immediately before and after every `test_that()` block
  (the only per-test hook testthat offers); `devtools::test()` clean.
- **AC9** — D044 appended, recording the session lifetime, the two discard
  routes, the per-process `parallel = TRUE` behavior, and why D034's falsifier is
  untripped. The `@param hardware` sentence carries one re-decided wording on all
  16 topics `test-nvenc-docs.R` enumerates, and that file gained two assertions
  fencing the new half; re-run green.
- **AC10** — see the closing check line below.

### Consistency gate

`cairn_validate` exit 0, all checks passed; one advisory (the 10-AC sizing
tripwire the plan already recorded as a deliberate non-split). No `DESIGN.md`
principle changed, so `cairn_impact` is a clean skip. Toolchain slot:
`document()` no diff, `pkgdown::check_pkgdown()` clean, `devtools::check()`
`Status: OK` read off the final status line — the first check run reported
`Status: 1 NOTE` (spelling) while devtools' own summary printed "0 notes ✔",
the M17 mask observed live. CI green on all 9 checks.

### Independent review

Three fresh-context lenses. The blame-history lens found no contradiction of
past intent (one low-confidence note on the `set_program()` side effect). The
prior-review lens found no regressions; its PR-comment existence probe returned
empty, so it worked from the archived `## Review` sections as primary evidence.
The diff-bug lens returned 13 findings, scored by a fourth agent that did not
generate them.

**Actioned (≥80): 1.**

- **F1 (85) — fixed.** `test-nvenc-docs.R`'s "a stream-copy nvenc call aborts
  without probing" was made vacuous by the memo: its own control call warmed the
  memo, so the trailing `expect_identical(probes, 0L)` could not fail whether or
  not the four stream-copy calls reached `has_nvenc()`. AC7 reworked the three
  probe-counting tests it names and this fourth one was not among them. Fixed by
  discarding the memo before the measured calls. Liveness proved by isolating the
  mechanism against a neutered stream-copy guard: memo cold → probes 1, assertion
  fails, regression caught; memo warm → probes 0, assertion passes, regression
  missed. (A first attempt to prove this by failure count was discarded as
  contaminated — neutering the guard also breaks the four `expect_error()`
  assertions, so the count did not isolate the probe assertion.)

**Fixed below threshold: 1.** F2 (55) — `expect_no_error(expr, message = <label>)`
passes `message` through as the matcher's *regexp*, not as a failure label.
Measured on testthat 3.3.2: a non-matching abort records **no expectation at all**
and propagates, so the run still goes red but the verb is never named and the
remaining cells of H go unmeasured. Repaired rather than logged because AC2
requires each cell to carry a working abort control, and AC fencing forbids
ticking a criterion against a control shown to be inert.

**Logged below threshold: 11** (surfaced, not actioned).

- F11 (78) — the two new doc assertions have no negative counterpart and grep the
  whole Rd text, so moving the pointer prose to a per-verb `@seealso` would keep
  them green while removing the `@param` wording AC9 decided.
- F5 (75) — `find_program()` prefers `Sys.which()` over the `set_program()`
  config, so `set_program()` cannot repoint while a binary is on `PATH`; D044 and
  NEWS present it unqualified as one of two escapes. The milestone's own work log
  records this limitation as the reason not to key the memo on the path.
- F9 (68) — `set_state_inspector()` is a single global slot with last-write-wins;
  using it as a resetter forecloses its leak-detection role and a later
  `setup-*.R` installing its own would silently disable AC8's discard.
- F4 (68) — `has_nvenc()`'s own help topic never discloses the memo, though it is
  the function whose behavior changed.
- F10 (60) — the new doc fence matches a phrase occupying a whole generated Rd
  line, so a roxygen rewrap could break it; the file's existing fence was
  deliberately a short clause for that reason.
- F3 (45) — under a forking `multicore` plan children inherit the parent's warm
  memo, so the "each worker asks once" wording in the docs, NEWS and D044 is not
  exact for that plan; the ROADMAP candidate row already carries the nuance.
- F6 (35) — eight of the 16 AC2 cells (the scalar verbs) probed once before this
  milestone too, so AC2 alone is partly met by pre-existing behavior; AC3 catches
  a full revert.
- F7 (32) — `character(0)` is cached stickily and a `NULL` `$name` would silently
  disable the memo; both are hypothetical future-parse regressions.
- F12 (27) — D034's call-chain parenthetical is now stale, but D034 itself tells
  readers to re-run the grep rather than trust it, and DECISIONS is append-only.
- F8 (20) — `set_program()`'s `@return` claims a logical and returns `NULL`
  invisibly; pre-existing, value and visibility unchanged by this branch.
- F13 (15) — duplicate `ffmpeg_encoders` link on the new topic via `@seealso` plus
  `@family`; cosmetic.

Return floor: F1 is the only actioned finding, scored 85 (not ≥90) and a defect in
a test instrument rather than in what the package does for users; the criterion it
touches, AC7, names three tests and this fourth is not among them. No status
return; fixed on the branch and re-verified.
