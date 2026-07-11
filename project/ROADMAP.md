# tidymedia Roadmap

Single source of truth for milestone status. Update only via the `/milestone`
skill. If this file and a milestone file disagree, this file wins — fix the
milestone file immediately.

Statuses: `idea` → `planned` → `in-progress` → `blocked` → `review` → `done`

Rules: at most one milestone `in-progress` at a time; keep this file under 60
lines; all detail lives in `milestones/`, never here.

| ID  | Milestone                                          | Status  | File                              |
|-----|----------------------------------------------------|---------|-----------------------------------|
| M01 | Infrastructure modernization (testthat, CI, cli)   | done    | archive/M01-infrastructure.md     |
| M02 | ffm builder engine rework (real command model)     | done    | archive/M02-ffm-engine.md         |
| M03 | Task verbs rebuilt on the builder + batch support  | done    | archive/M03-task-verbs.md         |
| M04 | Metadata layer polish (mediainfo/ffprobe tibbles)  | done    | archive/M04-metadata-layer.md     |
| M05 | Docs, vignettes, pkgdown, release prep             | done    | archive/M05-docs-release.md       |
| M06 | Engine hardening & safe execution                  | planned | milestones/M06-engine-hardening.md |
| M07 | Complete the blessed multi-input verbs             | idea    | milestones/M07-multi-input-verbs.md |
| M08 | Verification & provenance                          | idea    | milestones/M08-verification-provenance.md |
| M09 | Research-workflow task verbs                       | idea    | milestones/M09-research-verbs.md  |
| M10 | CRAN readiness (prep, hold submission)             | idea    | milestones/M10-cran-readiness.md  |

Last hygiene check: 2026-07-10 (M05 merged as PR #5 / 4b04fad, archived; M06 planned, M07–M10 queued as ideas)
