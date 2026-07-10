# tidymedia Roadmap

Single source of truth for milestone status. Update only via the `/milestone`
skill. If this file and a milestone file disagree, this file wins — fix the
milestone file immediately.

Statuses: `idea` → `planned` → `in-progress` → `blocked` → `review` → `done`

Rules: at most one milestone `in-progress` at a time; keep this file under 60
lines; all detail lives in `milestones/`, never here.

| ID  | Milestone                                          | Status  | File                              |
|-----|----------------------------------------------------|---------|-----------------------------------|
| M01 | Infrastructure modernization (testthat, CI, cli)   | planned | milestones/M01-infrastructure.md  |
| M02 | ffm builder engine rework (real command model)     | idea    | —                                 |
| M03 | Task verbs rebuilt on the builder + batch support  | idea    | —                                 |
| M04 | Metadata layer polish (mediainfo/ffprobe tibbles)  | idea    | —                                 |
| M05 | Docs, vignettes, pkgdown, release prep             | idea    | —                                 |

Last hygiene check: 2026-07-10
