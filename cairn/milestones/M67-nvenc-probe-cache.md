# M67: The encoder probe answers once per session, not once per row

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP1
- **Branch/PR:** `m67-nvenc-probe-cache`

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

- [ ] AC1 With the memo cold, the option seam unset, and `ffmpeg_encoders()`
      replaced by a counting stub reporting the nvenc encoders present (the
      `tests/testthat/test-nvenc-docs.R:71-75` pattern), the sequence
      `has_nvenc("h264")`, `has_nvenc("hevc")`, `has_nvenc("h264")` invokes the
      stub exactly once in total — cumulative count `1L` after each call. Two
      codecs, so a per-codec memo fails this.
- [ ] AC2 Let **H** be `intersect(getNamespaceExports("tidymedia"), <names in
      the namespace whose `formals()` include `hardware`>)`, read at test time,
      never hand-listed. For every member of H, one `hardware = "nvenc"` call
      with the memo cold and the counting stub in place invokes the stub
      exactly once. Each cell's call is built from that function's own
      `formals()` — `parallel = FALSE` and a 3-row `jobs` table supplied only
      where those formals exist, `video_codec` set to a non-`"copy"` value
      wherever its default is `"copy"` — and each cell carries a control
      asserting the call did not abort (M41: a cell that aborts measures
      nothing).
- [ ] AC3 Running AC2's whole grid in one session *without* resetting the memo
      between cells invokes the stub exactly once in total across every member
      of H — the memo is shared across functions, not per-function.
- [ ] AC4 After the exported discard call, and separately after
      `set_program("ffmpeg", <path>)`, the next `has_nvenc()` invokes the stub
      again. Both routes are documented on the discard call's own help topic.
- [ ] AC5 `ffmpeg_encoders()` stays uncached: two consecutive calls reach the
      `ffmpeg()` execution seam (`R/ffmpeg.R:20-30`, which shells `system()` —
      **not** `run_program()`) twice, counted by mocking `ffmpeg` itself.
- [ ] AC6 The `getOption("tidymedia.nvenc_encoders")` seam is read before the
      memo on every call, so setting it mid-session takes effect at once and
      never reads or populates the memo. Evidence: a test that warms the memo,
      then sets the option to `character(0)`, and observes `has_nvenc()` return
      `FALSE` with no further stub invocation — plus the existing suite green
      across every file that stubs the seam.
- [ ] AC7 The three D034 probe-counting tests — in
      `test-audio-stream-passthrough.R`, `test-audio-stream-format-web.R`, and
      `test-audio-stream-crop-segment.R`, each identified by its
      `before <- probes; expect_gt(probes, before)` assertion — measure D034's
      claim by discarding the memo before each measured call, and each goes red
      when the `has_nvenc()` call inside `check_nvenc_available()`
      (`R/ffmpeg.R:2643`) is deleted from a committed baseline. All three red,
      recorded. (The call at `R/ffmpeg.R:2571` is *not* the mutant: it is
      short-circuited at these tests' default `fallback = FALSE`.)
- [ ] AC8 A `tests/testthat/setup-*.R` file discards the memo before each test,
      so no file inherits another's warm memo; `devtools::test()` clean.
- [ ] AC9 A `cairn/DECISIONS.md` entry records the lifetime (session, discarded
      only explicitly or via `set_program()`), the per-process behavior under
      `parallel = TRUE`, and why this does not trip D034's falsifier. The
      `@param hardware` probe sentence gets one re-decided wording, applied to
      every topic `test-nvenc-docs.R:19-31` enumerates as carrying it, and that
      test re-run.
- [ ] AC10 Profile `verify` clean and the review-time full check:
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
- [ ] T5 Rework the three D034 probe-counting tests to discard-then-measure,
      then run the AC7 mutation from a committed baseline (M44: commit first,
      or `git checkout` reverts the feature and every mutant reads red for the
      wrong reason) and record all three red.
- [ ] T6 D-entry, NEWS `## Performance` entry, the re-decided `@param hardware`
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

## Decisions

## Review
