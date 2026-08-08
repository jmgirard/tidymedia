# tidymedia

R interface to FFmpeg and MediaInfo for **reproducible media preprocessing in
research/data-science pipelines** — batch trimming, cropping, format
standardization, and metadata extraction as tibbles. It is deliberately not
"all of ffmpeg in R" (see cairn/DECISIONS.md D001).

## Architecture: three layers (DECISIONS.md D002–D003)

- **Layer 0 — escape hatch:** `ffmpeg()`, `ffprobe()`, `mediainfo()` pass raw
  arguments to the CLIs. This is the only "faithful wrapper" there will be.
- **Layer 1 — engine:** the `ffm_*` pipe builder (`R/ffm.R`, `R/ffm_oop.R`).
  All command assembly, quoting, and copy-vs-re-encode logic lives here,
  once. Deliberately linear: one input chain, sequential filters, one output,
  plus blessed multi-input verbs (stack/concat/overlay). No filtergraph DAGs.
- **Layer 2 — front door:** task verbs (`extract_audio()`, `segment_video()`,
  …) as thin wrappers over Layer 1. Must not glue their own command strings.

## Project tracking (cairn)

This repo uses the cairn plugin. All project state lives in markdown
under `cairn/`. Boundary rule: **Architecture → DESIGN · Status → ROADMAP ·
Tasks → milestone files · Decisions → DECISIONS · History → archive + git.**

- Start with `/milestone` — status snapshot, health audit, suggested next
  action. Never record status or TODOs in this file; anything time-varying
  rots here.
- All eight skills: `/milestone`, `/milestone-plan`, `/milestone-implement`,
  `/milestone-review`, `/milestone-brief` (Fable escalation), `/hotfix`,
  `/cairn-release`, `/cairn-init`.
- Work tiers: trivial edits (no runtime surface) commit directly to main;
  user-visible bugs go through `/hotfix`; everything else is a milestone
  (`/milestone-plan` → `/milestone-implement` → `/milestone-review`).
- Ideas: "add X to the candidates" appends a ROADMAP row — no ceremony.
- Nothing merges to main without the user's explicit approval at review.
- Claude's persistent memory never holds project state; `cairn/` files win
  any conflict.
- All skills read the plugin's `skills/shared/tracking-rules.md` first and
  obey it.

## Development conventions

- Check: `devtools::check()` (0 errors / 0 warnings expected)
- Test: `devtools::test()` — testthat 3e; execution tests must
  `skip_if` ffmpeg/mediainfo binaries are absent
- Errors/messages: `cli::cli_abort()` and rlang checks; assertthat is being
  retired — never add new assertthat calls
- Scaffolding via usethis; CI via GitHub Actions
- `man/` is generated: edit roxygen comments, then `devtools::document()`
- `README.md` is generated: edit `README.Rmd`, then `devtools::build_readme()`
- New non-package files at repo root must be added to `.Rbuildignore`
- Line endings are LF, pinned by `.gitattributes` (`* text=auto`); `git` normalizes
  on commit, so no editor or script needs to remember. One-time, per clone, so
  `git blame` skips the normalization commit:
  `git config blame.ignoreRevsFile .git-blame-ignore-revs`. Note this setting is
  fatal whenever the file is absent from the working tree — check out a pre-M60
  tag or branch and every `git blame` dies with `could not open object name
  list`. Skip it for one call with `git blame --no-ignore-revs-file <file>`
  (the `-c blame.ignoreRevsFile=` forms do not work — git accumulates the
  values rather than overriding), or undo it with
  `git config --unset blame.ignoreRevsFile`.
