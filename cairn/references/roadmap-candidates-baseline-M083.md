# ROADMAP candidate-row baseline at M083's branch point

_Working artifact produced by M083. Freezes what `cairn/ROADMAP.md`'s
candidate rows were at the commit M083's branch was cut from, so a later
reader can see which row became which after the entombment, the pruning and
the compression this milestone applied._

_This page is a convenience record, not a verification surface: M083's
acceptance criteria read the branch point out of git
(`git show $(git merge-base m083-roadmap-byte-budget master):cairn/ROADMAP.md`)
and the post-merge files directly, never this table._

**Provenance.** Ingested 2026-08-28 by M083 from a first-hand enumeration of
`cairn/ROADMAP.md` at commit `8021df1`, the branch point of
`m083-roadmap-byte-budget`. The four columns are the four enumerations M083's
criteria name, run over that blob: `grep '^- '` (row identity and byte count),
`grep '^- ~~'` (struck), `grep -i 'instrument'`, and `grep 'Promote '`.
No external source. Pagination: —.
Extraction: machine-generated from the frozen blob and re-derivable from it at
any time, so there is nothing to re-verify against a moving source — observed
2026-08-28.

## Counts at the branch point

`cairn/ROADMAP.md` was 42,552 bytes over 53 lines. Of its 33 candidate rows,
3 were struck through, 5 matched `instrument` case-insensitively (13,303
bytes between them), and 22 carried a `Promote ` clause.

## Rows

| # | Bytes | Struck | Instrument | Promote-on | `— added` trailer | Opening words |
|---|---|---|---|---|---|---|
| 1 | 770 | — | — | yes | — added 2026-08-28 | A second doctrine module for the front-door guard ordering and p… |
| 2 | 468 | — | — | — | — added 2026-08-28 | The two shapes M082 left behind when it shipped the check's off … |
| 3 | 706 | — | — | yes | — added 2026-08-28 | ?tidymedia's closing See vignette(…) navigation paragraph is swa… |
| 4 | 1290 | — | yes | yes | — added 2026-08-28 | M081's three deferred review findings on the flag-guard INSTRUME… |
| 5 | 1091 | yes | — | — | — added 2026-08-28 | The four SHIPPED-predicate findings M080's review left behind. P… |
| 6 | 2537 | — | — | yes | — added 2026-08-28, split 2026-08-28, measured 2026-08-28 | with_timeout() and local_timeout() may take minutes, not seconds… |
| 7 | 750 | — | — | yes | — added 2026-08-27 | A permanent CI job that installs DESCRIPTION's declared Imports … |
| 8 | 1076 | — | — | yes | — added 2026-08-28 | The two gaps D055 discloses and does not close: no floor run alo… |
| 9 | 2215 | — | yes | yes | — added 2026-08-28 | M079's ten deferred review findings on the floor harness itself,… |
| 10 | 854 | yes | — | — | — added 2026-08-27 as the M074/M076 carry row, promoted 2026-08-28 | Harden the three data-raw floor-measurement scripts, and commit … |
| 11 | 1960 | — | yes | yes | — added 2026-08-27 | M071's five review findings on its own carry harness, grouped be… |
| 12 | 2718 | — | yes | yes | — added 2026-08-26 | The M70 review's eight guard-strength findings, grouped because … |
| 13 | 2051 | — | — | yes | — added 2026-08-26, measured 2026-08-28 | A tighter kill than base R's timeout= gives. Base R escalates SI… |
| 14 | 396 | — | — | — | — added 2026-07-10, split 2026-07-12, reconciled 2026-07-13 | Fixed-region region blur (no face tracking): split→crop→boxblur→… |
| 15 | 217 | — | — | — | — added 2026-07-13 | burn_timecode / drawtext text-and-timecode burn-in for coders & … |
| 16 | 236 | — | — | — | — added 2026-07-13 | Minor in-scope convenience verbs (grouped): split multi-view→per… |
| 17 | 1336 | — | — | yes | — added 2026-07-30, extended 2026-07-30 | Two separate_audio_video() failure-path leftovers M45 measured b… |
| 18 | 1040 | — | — | yes | — added 2026-07-30 | The multi-track separation abort is blind to WHY FFmpeg failed, … |
| 19 | 1926 | — | — | yes | — added 2026-08-09, extended 2026-08-26, split 2026-08-26, narrowed 2026-08-27 | Per-call timeout = arguments on the 60 run-capable verbs and on … |
| 20 | 1106 | — | — | yes | — added 2026-08-08 | find_ffmpeg() is not memoized either, so one Sys.which() — a sys… |
| 21 | 1466 | — | — | yes | — added 2026-08-08 | M67's memo is per-process, so it is invisible to parallel = TRUE… |
| 22 | 1484 | yes | — | — | — added 2026-07-30, reviewed 2026-07-31, promoted 2026-08-28 | The dropped-audio-track check has no opt-out and its cost is all… |
| 23 | 1977 | — | — | yes | — added 2026-07-31, grouped 2026-08-06 | Two normalize_audio() extensions the linear builder blocks (grou… |
| 24 | 886 | — | — | yes | — added 2026-07-31 | The compiled command's remaining bare token classes: codec names… |
| 25 | 632 | — | — | yes | — added 2026-07-31 | Renaming audio_stream or audio, or unifying the two NULL reading… |
| 26 | 1106 | — | — | yes | — added 2026-08-06 | probe_all()'s sequential (default) path now materializes every f… |
| 27 | 1131 | — | — | yes | — added 2026-08-08 | A wrongly-typed value still answers differently depending on its… |
| 28 | 5120 | — | yes | yes | — added 2026-08-08, corrected + extended 2026-08-08 | The M62 and M63 review findings logged below the action threshol… |
| 29 | 579 | — | — | yes | — added 2026-08-08, regroomed 2026-08-08 | The normalize pair's copy/audio_stream form divergence (this row… |
| 30 | 248 | — | — | — | — added 2026-07-26 | Video quality / rate-control knob (CRF↔CQ, -preset p1–p7, bitrat… |
| 31 | 231 | — | — | — | — added 2026-07-26 | GPU decode / -hwaccel cuda input acceleration + GPU filter pipel… |
| 32 | 200 | — | — | — | — added 2026-07-26 | Other hardware encode backends (videotoolbox/qsv/vaapi/amf) gene… |
| 33 | 344 | — | — | — | — added 2026-07-10, trimmed 2026-07-12, reconciled 2026-07-12 (M30) | CRAN readiness (release mechanics only): win-builder + R-hub, cr… |
