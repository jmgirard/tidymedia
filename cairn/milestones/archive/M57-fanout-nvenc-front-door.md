# M57: A missing nvenc encoder is refused at the front door, on every verb that fans out

**Status:** done (2026-08-07, PR #60 https://github.com/jmgirard/tidymedia/pull/60)

**Goal:** Make an unavailable nvenc encoder abort at the fan-out verb the user
called, not inside `purrr::pmap()`.

**Outcome:** The abort moved into `check_nvenc_available()`, the one place its
wording lives; `resolve_hw_encoder()` calls it rather than carrying a copy, and
the nine fan-out verbs call it at their front doors, immediately before
`ffm_batch()` and after their own checks. `batch_video_codecs()` yields the
families a `video_codec` column spells, `NULL`/`NA` reading as h264 (D022).
Rows naming no encoder are skipped, and `fallback` is validated where it is
read. `@param hardware` on all nine; NEWS discloses the reassigned precedence.

**Decisions:** D035 (cross-cutting: licenses the construction-time abort gate
D024's third exclusion reserved). Milestone-local: the guard takes one codec or
a list; a per-row skip condition scopes it to the rows it applies to.

**Review:** Two rounds. Round 1 returned it — F4 showed AC1 failing on a mixed
`reencode` column; F3/F1 also actioned. Round 2 actioned D3 (83, a doc sentence
true on three of nine verbs) and D1 (80, four error classes AC6's grep never
reached, now pinned by test, remainder on the candidate row); 8 logged below.
Graduated: the M47/M48-F1 lesson's "a fan-out verb needs a guard" half, now
failed by the nine-verb sweep.
