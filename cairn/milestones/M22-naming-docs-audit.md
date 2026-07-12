# M22: Naming & docs audit + target-scheme decisions

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Principles touched:** —   <!-- owner: plan · create/amend-via-gate; the naming D-entry references IP1's layer-prefix convention but does not change any principle -->
- **Branch/PR:** m22-naming-docs-audit · https://github.com/jmgirard/tidymedia/pull/24   <!-- owner: implement (branch) / review (PR URL) · create -->

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

- [x] AC1 — `cairn/references/naming-docs-audit-M22.md` exists and inventories
      every NAMESPACE export grouped by family, with each flagged naming
      inconsistency carrying a specific recommendation and a `file:line`
      citation. At minimum it dispositions the known tensions: `audio_as_mp3`
      (breaks `verb_object`), `extract_frame`/`extract_frames` (one-vs-many
      semantics colliding with the scalar/batch plural convention),
      `normalize_audios`/`standardize_videos` plurals, the three metadata
      families (`probe_*`/`get_*`/`mediainfo_*`), and the tidy-eval reexports +
      `pad_integers`/`convert_fractions` stray-util exports.
- [x] AC2 — The audit contains an argument-name consistency table naming the
      canonical arg vocabulary and listing every deviation across Layer-2 verbs
      and metadata families (at minimum `picture_in_picture(main, overlay)` and
      `separate_audio_video(audiofile, videofile)`), each with a target.
- [x] AC3 — The audit contains a docs gap-fill checklist: every export lacking
      `@examples` or `@return`, plus the `@seealso` cross-link gaps, enumerated
      by function name (verified against the actual roxygen, not guessed).
- [x] AC4 — A new `DECISIONS.md` entry records the approved target naming scheme
      and the clean-break rename policy, written so the execution milestone has a
      standing rule to implement against. (Applied at review, after user sign-off
      on the audit.)
- [x] AC5 — `cairn/DESIGN.md` is reconciled with reality: the stale "M10"
      references (Function families note + Known issues) are corrected to point
      at this effort, and the "Conventions"/"Function families" prose reflects
      the decided naming scheme. (ROADMAP rows — M22 + execution candidate +
      trimmed CRAN candidate — are handled at plan time, not counted here.)
- [x] AC6 — No net change to `R/`, `man/`, `NAMESPACE`, or `tests/`
      (docs/decision milestone; verified by `git diff --stat` touching only
      `cairn/`). No automated tests — there is no runtime surface; verification
      is document review + user approval at review.

## Coverage

- AC1 → T1, T2, T5
- AC2 → T3, T5
- AC3 → T4, T5
- AC4 → T6 · AC5 → T7
- AC6 → T1–T7 (no-code discipline held across every task)

## Tasks

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
- [x] T6 — Draft the `DECISIONS.md` entry (target naming scheme + clean-break
      policy); hold application until review/user sign-off. (Drafted as DRAFT D014
      in the Decisions section below.)
- [x] T7 — Reconcile DESIGN.md: fix the stale "M10" references (Function families
      + Known issues) and point them at M22/D014. Conventions prose asserting the
      *ratified* scheme is deferred to review, promoted with D014 (see work log).

## Work log

- 2026-07-12: created by /milestone-plan.
- 2026-07-12: T1–T7 — wrote the audit (79-export inventory, 10 name findings,
  arg table, `@seealso` gaps, ranked recs); drafted DRAFT D014 (held for
  sign-off, AC4); fixed DESIGN.md's two "M10" refs. Minor amendment: AC5
  ratified-conventions prose deferred to review, promoted with D014.

## Decisions

**DRAFT D014 — pending M22 sign-off; promote to cairn/DECISIONS.md at review
(AC4).** Target scheme stated in full in
`cairn/references/naming-docs-audit-M22.md §6` (verb_object; `ffm_*` = Layer 1
only; `get_*` = file-metadata scalars not capability queries; backend-carrying
metadata prefixes; full-word args retiring `acodec`/`vcodec` and
`ts_start`/`ts_stop`); **rename policy: clean break** (no `lifecycle` shims —
API pre-0.2.0, D001). **Open decision point for review:** batch-sibling rule —
plural-noun `*_videos` (accepting `normalize_audios`) vs `*_batch`. On sign-off
the D-entry (audit §6 + this policy) is appended; execution carries
`RB tripwire: irreversible-api`.

## Review

**2026-07-12 — reviewed.** Evidence: AC1 audit exists, 79 exports by family
(3+23+18+15+12+8), 8 citations spot-checked exact, all tensions N1–N10
dispositioned; AC2 arg table with verified deviations (`acodec`/`vcodec`,
`ts_start`/`ts_stop`, `samplingrate`↔`sample_rate` 3v35, `picture_in_picture`,
`separate_audio_video`); AC3 `@seealso` gap list, count = 10 pages verified;
AC4 DRAFT D014 complete, promoted to DECISIONS.md on approval; AC5 DESIGN.md
M10 refs removed; AC6 `git diff` = cairn/ only, `cairn/` is `.Rbuildignore`d.
Consistency gate: `cairn_validate.py` exit 0 (12 checks incl. coverage);
document/README/NEWS/pkgdown N/A (no R/man/NAMESPACE/export change).
Independent review (1 Opus diff-bug lens; blame/prior-PR lenses no-op on a
new tracking-markdown diff): 2 findings, both ≥80, both fixed — (1) score 92:
"no internal use" was false for `.data` (used `ffprobe.R:191`) → corrected in
audit N6 + DESIGN.md (drop 4 quoting helpers, keep `.data`); (2) score 80:
`ffm_files` cited to alias line not definition → corrected. None below 80.
