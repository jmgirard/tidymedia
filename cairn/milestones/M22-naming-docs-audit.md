<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. -->
# M22: Naming & docs audit + target-scheme decisions

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Principles touched:** —   <!-- owner: plan · create/amend-via-gate; the naming D-entry references IP1's layer-prefix convention but does not change any principle -->
- **Branch/PR:** m22-naming-docs-audit   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal

Assess the whole public API surface for naming and documentation quality and
decide the target conventions, producing an approved written audit + decision
record — with **no changes to `R/` source** — that a later execution milestone
implements against.

## Scope

**In:**
- A written audit inventorying all ~80 exports (NAMESPACE), grouped by family
  (Layer 0 / Layer 1 `ffm_*` / Layer 2 verbs / metadata / program management /
  tidy-eval reexports & utils), flagging every function-name inconsistency with
  a specific recommendation (keep / rename-to-X) and a `file:line` citation.
- An argument-name consistency table: the canonical arg vocabulary
  (`infile`/`outfile`/`infiles`/`run`/`overwrite`/…) and every deviation across
  Layer-2 verbs and metadata families, each with a recommended target.
- A docs gap inventory (targeted gap-fill scope): every export missing
  `@examples` or `@return`, plus `@seealso`/`@family` cross-link gaps, as a
  concrete checklist.
- A `DECISIONS.md` entry codifying the approved target naming scheme
  (`verb_object`; singular scalar / plural batch; the `ffm_*` layer prefix; the
  metadata `probe_*` vs `get_*` vs `mediainfo_*` family boundaries) and the
  **clean-break** rename policy (no `lifecycle` shims; API is pre-0.2.0 and
  still soaking — D001/CRAN-candidate framing).
- Tracking reconciliation: trim the CRAN-readiness candidate to pure release
  mechanics (its "API-surface cleanup" + "examples/vignette policy pass"
  portions move under this effort's execution follow-up), add the execution
  candidate row, and fix DESIGN.md's stale "M10" reference.

**Out:**
- Actually renaming any function or editing arg signatures in `R/` → execution
  follow-up (candidate "Apply M22 naming/docs recommendations")
  (RB tripwire: irreversible-api).
- Adding `@examples`/`@return`/`@seealso` to source or touching roxygen →
  same execution follow-up.
- Full docs polish / vignette rewrite → not requested (user chose targeted
  gap-fill); residual lives in the CRAN candidate.
- CRAN release mechanics (win-builder, R-hub, version bump) → stays in the
  trimmed CRAN candidate.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets -->

- [ ] AC1 — `cairn/references/naming-docs-audit-M22.md` exists and inventories
      every NAMESPACE export grouped by family, with each flagged naming
      inconsistency carrying a specific recommendation and a `file:line`
      citation. At minimum it dispositions the known tensions: `audio_as_mp3`
      (breaks `verb_object`), `extract_frame`/`extract_frames` (one-vs-many
      semantics colliding with the scalar/batch plural convention),
      `normalize_audios`/`standardize_videos` plurals, the three metadata
      families (`probe_*`/`get_*`/`mediainfo_*`), and the tidy-eval reexports +
      `pad_integers`/`convert_fractions` stray-util exports.
- [ ] AC2 — The audit contains an argument-name consistency table naming the
      canonical arg vocabulary and listing every deviation across Layer-2 verbs
      and metadata families (at minimum `picture_in_picture(main, overlay)` and
      `separate_audio_video(audiofile, videofile)`), each with a target.
- [ ] AC3 — The audit contains a docs gap-fill checklist: every export lacking
      `@examples` or `@return`, plus the `@seealso` cross-link gaps, enumerated
      by function name (verified against the actual roxygen, not guessed).
- [ ] AC4 — A new `DECISIONS.md` entry records the approved target naming scheme
      and the clean-break rename policy, written so the execution milestone has a
      standing rule to implement against. (Applied at review, after user sign-off
      on the audit.)
- [ ] AC5 — `cairn/DESIGN.md` is reconciled with reality: the stale "M10"
      references (Function families note + Known issues) are corrected to point
      at this effort, and the "Conventions"/"Function families" prose reflects
      the decided naming scheme. (ROADMAP rows — M22 + execution candidate +
      trimmed CRAN candidate — are handled at plan time, not counted here.)
- [ ] AC6 — No net change to `R/`, `man/`, `NAMESPACE`, or `tests/`
      (docs/decision milestone; verified by `git diff --stat` touching only
      `cairn/`). No automated tests — there is no runtime surface; verification
      is document review + user approval at review.

## Coverage
<!-- owner: plan · each acceptance criterion → task(s) by positional number -->

- AC1 → T1, T2, T5
- AC2 → T3, T5
- AC3 → T4, T5
- AC4 → T6
- AC5 → T7
- AC6 → T1–T7 (no-code discipline held across every task)

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] T1 — Inventory all NAMESPACE exports into a family map (Layer 0 / Layer 1
      `ffm_*` / Layer 2 verbs / metadata / program management / tidy-eval &
      utils), each with its defining `file:line`.
- [x] T2 — Function-name consistency analysis: flag each inconsistent export
      with a keep/rename recommendation and rationale (cover the AC1 known
      tensions plus anything the sweep surfaces).
- [x] T3 — Argument-name consistency analysis: derive the canonical arg
      vocabulary and build the deviation → target table across Layer-2 verbs and
      metadata families.
- [x] T4 — Docs gap inventory: grep the roxygen for missing `@examples`/`@return`
      and thin `@seealso`; produce the per-function gap checklist (targeted scope).
- [x] T5 — Write `cairn/references/naming-docs-audit-M22.md` consolidating
      T1–T4 with a ranked recommendation list; register it in
      `cairn/references/INDEX.md`.
- [ ] T6 — Draft the `DECISIONS.md` entry (target naming scheme + clean-break
      policy); hold application until review/user sign-off.
- [ ] T7 — Reconcile DESIGN.md: fix the stale "M10" references and update the
      Conventions/Function-families prose to the decided naming scheme.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates -->

- 2026-07-12: created by /milestone-plan.
- 2026-07-12: T1–T5 — wrote cairn/references/naming-docs-audit-M22.md (80-export
  inventory, 10 function-name findings incl. overloaded `get_*` prefix, arg
  vocabulary table, `@seealso` gap checklist, ranked recs, proposed scheme);
  indexed it. No R/ changes.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->
