<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M099: D014's pre-0.2.0 rename window is reviewed before it closes

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

## Goal

Decide, for each of the three candidate API changes D014's clean-break policy
holds open, whether it ships now or is declined permanently — while a rename is
still free of a deprecation cycle.

## Scope

Surface tier: **user-facing** — every candidate under review renames an argument
or adds one to exported verbs.

**In:** the three candidate rows D014's pre-0.2.0 window covers —
(a) renaming `audio_stream` / `audio`, or unifying their two `NULL` readings
(D023, D025, D026, D032);
(b) a per-verb `check_tracks =` argument (D047, M082);
(c) a per-call `timeout =` argument on the run-capable verbs (D044, D047, M071,
M072).
`(RB tripwire: irreversible-api)` — an accepted change is irreversible on the
exported surface, taken under D014's clean break with no `lifecycle` shim.

**Out:**
- Superseding D014 with a deprecation policy — declined at this plan's question
  gate; the clean break stands. Not deferred.
- Any rename outside the three rows above → stays a candidate row.
- The release itself → the standing `CRAN readiness` ROADMAP candidate row.

## Acceptance criteria

- [ ] AC1 Each candidate leaves the exported surface in one of exactly two
      states, and a sweep says which. The sweep: for a name `N`, the exported
      functions carrying `N` as a formal, computed by walking
      `getNamespaceExports("tidymedia")` and testing `names(formals())` — the
      procedure that enumerates the domain rather than recalling it (at HEAD it
      returns 18 verbs for `audio_stream`, matching D032's count). **Shipped:**
      the sweep for the new name returns the verb set the milestone's D-entry
      names, and the sweep for the old name returns empty — no `lifecycle`
      shim, per D014. **Unchanged:** the sweep for the candidate's names returns
      exactly what it returns at the branch point.
- [ ] AC2 Candidate (a) may also ship as a behavior change with no surface
      change — unifying the two `NULL` readings, which alters what
      `audio_stream = NULL` selects without touching any `formals()`. Where it
      ships in that form, a test asserts the unified reading at each verb the
      AC1 sweep returns for `audio_stream`, and D025 and D026 are superseded,
      since each states the split reading this would remove.
- [ ] AC3 For every candidate that ships in any form, its documentation matches
      the surface: `devtools::document()` produces no diff, `_pkgdown.yml` has a
      row for any newly exported object, and the vignettes and `README.Rmd`
      compile against the shipped names.
- [ ] AC4 `NEWS.md` names every change that shipped, in user-facing wording; a
      candidate that did not ship produces no entry.
- [ ] AC5 `devtools::test()` clean and `devtools::check()` reports 0 errors and
      0 warnings with every NOTE justified (PROFILE `verify` and
      `consistency-gate` slots).

## Tasks

1. Read the three candidate rows and every D-entry they cite; write up what each
   change would look like at the call site, and run the AC1 sweep at the branch
   point to record each candidate's starting verb set.
2. Question gate: put the three dispositions to the user, one option set each,
   stating that the choice is permanent once 0.2.0 reaches CRAN.
3. Write the D-entry recording the three dispositions, each naming the verb set
   the AC1 sweep must return and the evidence class that would reopen it; where
   a candidate is declined, state that D014's window closes at 0.2.0.
4. For each shipped disposition: write its tests first, then make the change.
   Supersede D025 and D026 if candidate (a) ships as a `NULL`-reading change.
5. Update `man/`, `_pkgdown.yml`, vignettes, `README.Rmd`, and `NEWS.md` for
   what shipped.
6. Update the ROADMAP rows: remove a row whose change shipped; on a declined
   row, replace the promotion condition that assumed the window was open.
7. Run the AC1 sweep, `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Coverage

- AC1 → T1, T2, T3, T4, T7
- AC2 → T4
- AC3 → T5, T7
- AC4 → T5
- AC5 → T7

## Work log
<!-- owner: implement/review -->

- 2026-08-31 plan: criteria audit ran in FULL mode (surface tier user-facing), fresh-context [O] reader. It returned five findings, all disposed here: AC1 and AC4 of the draft bound recording acts (a D-entry's existence, a ROADMAP row's wording) rather than deliverable properties under D-118/D-120 — the record is now a task and the criteria bind the exported surface; the draft's vacuity clause let a user-facing milestone complete with no surface change at all, and is gone; the draft's absence check was unsatisfiable for candidate (a), since `grep -rn "audio" R/ man/ vignettes/ README.Rmd` returns 2,193 hits, and ill-typed for (b) and (c), which add an argument and have no old name — replaced by the `formals()` sweep over `getNamespaceExports()`, which returns 18 verbs for `audio_stream` at HEAD and matches D032's count; and no branch fitted candidate (a)'s behavior-only `NULL`-reading form, now AC2.
- 2026-08-31 plan: alternative rejected — superseding D014 with a `lifecycle` deprecation policy, which would keep renames available after 0.2.0. Declined at the question gate in favor of reviewing the three candidates while the clean break is still free. Falsified by a rename becoming necessary after 0.2.0 ships, which is exactly the cost this milestone exists to price.
- 2026-08-31 plan: alternative rejected — leaving the three rows as candidates and letting the release close D014's window unexamined. Lost at the question gate; the maintainer chose to review them before any release.
