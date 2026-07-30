# M42: What `NULL` and column `NA` mean, settled across the codec family

- **Status:** review
- **Priority:** normal
- **Depends on:** M41
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m42-codec-null-na-semantics` · PR #45 https://github.com/jmgirard/tidymedia/pull/45

## Goal

Give the codec family one recorded answer to "what does `NULL` mean, and what
does a column `NA` mean", with every deliberate departure named.

## Scope

**In:** the three contract splits M41 deliberately left standing, each measured
against the working tree:

1. `standardize_video(video_codec = NULL)` compiles (drops `-codec:v`), while
   `anonymize_video(video_codec = NULL)` aborts.
2. `extract_audio(audio_codec = NULL)` aborts, while
   `extract_audio_batch(audio_codec = NULL)` compiles (`-vn`, no `-codec:a`).
3. Three `_batch` codec **columns** reject `NA` while the matching argument
   accepts `NULL`, so the column cannot spell what the argument can:
   `standardize_video_batch` and `anonymize_video_batch`'s `video_codec` via an
   inline `str_cols` guard, and `extract_audio_batch`'s `audio_codec` via
   `check_batch_string_col()`. The `str_cols` comments justify the guard by
   calling `video_codec` "a literal `libx264` default with no sentinel", a
   premise the probe falsifies — the argument does accept `NULL`.

D021 left this open on purpose: "It is deliberately **not** closed on semantics
… Anyone treating the family as uniform in what `NA` *means* will be wrong on two
of the verbs." This milestone closes it, keeping the departures that earn their
keep and recording why.

**Out:** adding codec arguments to verbs that lack them — D021's three
deliberately codec-less verbs (`format_for_web`, `strip_metadata`,
`concatenate_videos`) stay codec-less, and nothing here reopens that boundary.
Front-door type guards → M41 (this milestone assumes them).

## Acceptance criteria

- [x] AC1: A `cairn/DECISIONS.md` entry extending D016/D017/D019/D021 records,
      for each codec argument on each task verb and `_batch` sibling, what `NULL`
      means (emit nothing / abort / a specific encoding) and what a column `NA`
      means, plus the rationale for every verb that departs from the family
      default. It resolves splits 1–3 in Scope by name.
- [x] AC2: `standardize_video`/`_batch` and `anonymize_video`/`_batch` agree on
      `video_codec = NULL` — all four compile the same way, or all four abort
      with the same message shape. Which, and why, is AC1's entry.
- [x] AC3: `extract_audio` and `extract_audio_batch` agree on
      `audio_codec = NULL`, replacing the split M41 preserved on purpose; M41's
      code comment pointing here is removed in the same commit.
- [x] AC4: Every `_batch` codec column and its matching argument agree on
      whether "unset" is expressible. The three columns that reject `NA`
      (`standardize_video_batch`/`video_codec`,
      `anonymize_video_batch`/`video_codec`,
      `extract_audio_batch`/`audio_codec`) either move to
      `check_batch_codec_col()` + `batch_codec_cell()` like every other codec
      column, or their arguments stop accepting `NULL`, per AC1's entry. The
      falsified `str_cols` comments are corrected either way.
- [x] AC5: A test table asserts the resolved meaning of `NULL` and of a column
      `NA` for every codec argument AC1's entry covers, so uniformity — and each
      recorded departure, `convert_audio`'s `-q:a 0` included — is enforced
      rather than only documented. A departure appears in the table as an
      expected departure, never as a skipped case.
- [x] AC6: Every behavior change has a NEWS entry naming the verb and the old
      and new outcome; `@param` prose and each `@param jobs` column enumeration
      updated (M39 lesson); `devtools::document()` no-diff, `devtools::test()`
      and `devtools::check()` clean — 0 errors, 0 warnings.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4
- AC4 → T5
- AC5 → T6
- AC6 → T6

## Measurement (T1)

`codec_guard_semantics(codec_guard_baseline())` over all 34 codec
verb × argument pairs, working tree at b15f949. Regenerate rather than re-read.

**Family default, 27 of 34 pairs:** `NULL` emits no `-codec:v` / `-codec:a` at
all (D016's sentinel); a column `NA` resolves to `NULL` via `batch_codec_cell()`.

| verb | arg | `NULL` | column `NA` |
|---|---|---|---|
| `convert_audio` | `audio_codec` | `-q:a 0` | — scalar |
| `convert_audio_batch` | `audio_codec` | `-q:a 0` | `-q:a 0` |
| `extract_audio` | `audio_codec` | **abort** | — scalar |
| `extract_audio_batch` | `audio_codec` | emits nothing | **abort**, no `NA` |
| `anonymize_video` | `video_codec` | **abort** | — scalar |
| `anonymize_video_batch` | `video_codec` | **abort**, `In index: 1` | **abort**, no `NA` |
| `standardize_video_batch` | `video_codec` | emits nothing | **abort**, no `NA` |

Two findings the plan did not have. **Three** codec columns reject `NA`, not the
one Scope 3 names — `anonymize_video_batch` and `extract_audio_batch` carry the
same split. And `anonymize_video_batch(video_codec = NULL)` aborts *inside*
`purrr::pmap()` (`In index: 1`, blaming pmap): M41's defect shape, surviving on
the `NULL` path M41's guards deliberately waved through.

## Tasks

- [x] T1: Extend M41's `data-raw/` baseline script to also emit each codec
      argument's current column-`NA` outcome, and record the resulting
      argument × {`NULL`, column `NA`} table in this file — measured, not
      re-derived by hand.
- [x] T2: From T1's table, choose the family default and each departure; draft
      the D-entry and surface it at the implement question gate before any code
      lands. *(RB tripwire: irreversible-api)*
- [x] T3: Land the `video_codec = NULL` resolution across `standardize_video`,
      `standardize_video_batch` ([ffmpeg.R:2547](../../R/ffmpeg.R#L2547)),
      `anonymize_video`, and `anonymize_video_batch`
      ([ffmpeg.R:1145](../../R/ffmpeg.R#L1145)).
- [x] T4: Land the `extract_audio` / `extract_audio_batch` resolution
      ([ffmpeg.R:283](../../R/ffmpeg.R#L283),
      [ffmpeg.R:3295](../../R/ffmpeg.R#L3295)); remove M41's pointer comment.
- [x] T5: Land the codec-column resolution on all three columns that reject
      `NA` — `standardize_video_batch` and `anonymize_video_batch`'s
      `video_codec` (`str_cols`), `extract_audio_batch`'s `audio_codec`
      (`check_batch_string_col`) — and correct the falsified `str_cols`
      comments.
- [x] T6: Write the AC5 table test; update `@param` prose and every `@param
      jobs` column enumeration for the changed verbs; NEWS entries;
      `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Work log

- 2026-07-29: created by /milestone-plan.
- 2026-07-29: plan gate chose planning this now as its own milestone over a ROADMAP candidate row, because the probe evidence is fresh and a second deferral would leave the family non-uniform in what `NA` means — the exact reading D021 warns against; falsified by the three splits turning out to need one decision each with no shared code, which would make three hotfixes cheaper than a milestone.
- 2026-07-29: plan chose criteria that fix *agreement* between each verb pair rather than naming which way each split resolves, because the direction is T2's gated decision; falsified by a split whose two directions need materially different tasks, which would force the decision back into planning.
- 2026-07-29: T1 — added a `col = "na"` cell to the M41 probe grid plus `codec_guard_semantics()`/`codec_guard_flag()`; `codec_guard_vacuous()` deliberately excludes the new cell (there a non-compiling default is M42's finding, not M41's broken instrumentation). Minor amendment: new `## Measurement (T1)` section holds the 34-pair result.
- 2026-07-29: T1 measurement falsifies Scope 3's "unlike every other codec column" — three codec columns reject `NA` (`standardize_video_batch`/`video_codec`, `anonymize_video_batch`/`video_codec`, `extract_audio_batch`/`audio_codec`) — and finds `anonymize_video_batch(video_codec = NULL)` aborting inside `purrr::pmap()`. Both go to the T2 gate.
- 2026-07-29: T2 gate — user chose "`NULL` = unset everywhere" over a verb-class split and over resolving toward abort (the escalation option was offered on the `irreversible-api` tripwire and declined), kept `convert_audio`'s `-q:a 0` departure, and accepted the scope amendment. Landed as D022.
- 2026-07-29: amendment (substantive, gated above) — Scope 3, AC4 and T5 now name all three no-`NA` codec columns instead of `standardize_video_batch`'s alone, because the T1 probe found three where the plan asserted one.
- 2026-07-29: T3 — one line: `anonymize_pipeline()`'s unconditional `check_token(video_codec)` was the sole cause of split 1, and skipping it for `NULL` fixes both anonymize verbs (the batch one's `In index:` abort included). Check left at its original position so it keeps reporting before `pixel_format`/drawbox. New `test-codec-null-na-semantics.R`; every absence assertion paired with a named-encoder non-vacuity assertion. Suite 2461 passing.
- 2026-07-29: T4 — `extract_audio()` takes `allow_null = TRUE`; M41's 14-line pointer comment replaced by a 4-line statement of the settled rule. M41's "NULL keeps its existing per-verb meaning" test moved whole into the new file rather than edited in place, because two files asserting NULL semantics is how they drift; what stayed behind is the NA-still-aborts half, which is that file's own concern. Suite 2466 passing.
- 2026-07-29: T5 — all three columns moved to `check_batch_codec_col()` + `batch_codec_cell()`; `color`/`pixel_format` stay in `str_cols` (no sentinel). `codec_guard_diff(origin/master, HEAD)`: 21 changed cells, 8 abort→compiled (the widening) and 13 abort→abort where only the message moved; `codec_guard_semantics()` now shows `convert_audio`/`_batch` as the sole departure. Re-probed the newly reachable `col = na` aborts: each still names its own argument, blames the verb, carries no `In index:` (M41's contract). Suite 2482 passing.
- 2026-07-29: T6 — AC5 table over all 34 pairs, each with a per-pair non-vacuity assertion; the `convert_audio` departure is a table entry, and a second test rejects a departure naming a pair that no longer exists. Verb list + call templates extracted to `helper-codec-family.R` so the front-door sweep and the semantics sweep cannot drift; M41's completeness test now fences both. Falsifiability checked by running the new file against `origin/master`'s `R/ffmpeg.R`: 7 of 10 blocks fail. `@param`s and both `@param jobs` enumerations updated; NEWS entry under New features. `document()` idempotent; `check()` 0/0/0; suite 2568 passing.

## Decisions

## Review

_2026-07-29, PR #45. Every line below is a command run in this session against
the branch tip, never recall._

### Acceptance-criteria evidence

- **AC1** — `cairn/DECISIONS.md` gains D022, heading naming what it closes and
  supersedes. It states the rule universally (`NULL` = emit no `-codec:v` /
  `-codec:a`; a column `NA` is the column form of that `NULL`), so it records a
  meaning for all 34 pairs by construction, then names the one departure with
  its rationale (`convert_audio`/`_batch`, `-q:a 0`, D021 reaffirmed). Splits
  1–3 are resolved by name — grepping the entry for the verbs each split names:
  `anonymize_video` ×3, `anonymize_pipeline`, `standardize_pipeline`,
  `extract_audio` ×5, `extract_audio_batch` ×2, `standardize_video_batch`,
  `anonymize_video_batch`, `check_batch_string_col` ×2, `check_batch_codec_col`
  ×2, all present.
- **AC2** — all four compile, and identically. Compiled each at `run = FALSE`
  with `video_codec = NULL`: no `-codec:v` in any of the four; the
  `standardize_video` scalar command is string-identical to
  `standardize_video_batch`'s, and `anonymize_video`'s to
  `anonymize_video_batch`'s. Both batch verbs' column `NA` compiles
  string-identically to the matching scalar `NULL`. AC2's alternative branch
  ("or all four abort with the same message shape") is not the branch taken.
- **AC3** — `extract_audio(audio_codec = NULL)` and
  `extract_audio_batch(audio_codec = NULL)` compile string-identical commands
  (no `-codec:a`, `-vn` retained), and the batch verb's column `NA` matches
  both. M41's pointer comment is gone: grepping `R/` for its text
  ("Reconciling the two is M42's job", "is M42's question") returns nothing, and
  `git log -S` over the branch places its removal in `0be559f` — the same commit
  that added `allow_null = TRUE` to `extract_audio()`.
- **AC4** — the resolution took the first branch: all three columns moved to
  `check_batch_codec_col()` + `batch_codec_cell()`, no argument stopped
  accepting `NULL`. Each column's `NA` cell now compiles (AC2/AC3 lines above),
  and a mixed column compiles per row. The widening did not reach the argument:
  a *scalar* `NA` still aborts on all three
  (`standardize_video_batch`, `anonymize_video_batch`, `extract_audio_batch`),
  and the non-codec columns still reject `NA` — `pixel_format` and `color` both
  abort with the `str_cols` message. Both falsified `str_cols` comments are
  corrected: neither now asserts "a literal `libx264` default with no
  sentinel" — each quotes the old wording and marks it false when written
  ([ffmpeg.R:1259](../../R/ffmpeg.R#L1259),
  [ffmpeg.R:2699](../../R/ffmpeg.R#L2699)). `str_cols` itself is now
  `c("color", "pixel_format")` and `c("pixel_format")`, with no codec column
  left in either.
- **AC5** — `tests/testthat/test-codec-null-na-semantics.R` sweeps
  `codec_family_pairs()`: **34** verb × argument pairs, against **36** codec
  arguments the package exports; the two-pair gap is `verify_media`'s expected
  probe values, excluded on the record. Uncovered set and covered-but-
  nonexistent set are both empty. Each pair gets a non-vacuity assertion (a
  named encoder must reach the command) before its absence assertion, so no
  cell can pass by measuring nothing. `convert_audio`/`_batch` appear in
  `codec_family_unset_meaning()` as `"q0"` — asserted to compile `-q:a 0`, not
  skipped — and a second test fails if that table ever names a pair that no
  longer exists. Falsifiability checked by running the file against
  `origin/master`'s `R/ffmpeg.R`: 7 of its 10 blocks fail, the table among
  them.
- **AC6** — NEWS.md gains an entry under New features naming each changed verb
  with its old and new outcome ("refused `video_codec = NULL` … Both now accept
  it"; "rejected `NA` … All three now accept it"), plus the unchanged-command
  claim and the `convert_audio` exception. `@param` prose updated on all six
  changed verbs and both `@param jobs` column enumerations rewritten to
  distinguish codec columns (`NA` legal) from `width`/`height`/`fps`/
  `pixel_format`/`color` (`NA` an error) — the M39 lesson.
  `devtools::document()` rewrites nothing on a second run (`git status` clean
  for `man/` and `NAMESPACE`). `devtools::test()`: 0 failures, 0 warnings, 15
  skips, 2568 passing. `devtools::check()`: **0 errors, 0 warnings, 0 notes**.

### Consistency gate

`cairn_validate`: all 16 checks PASS, all 8 advisories OK. Profile
`r-package` `consistency-gate`: `document()` no-diff ✓ · generated files
unedited (no `man/`/`NAMESPACE` drift) ✓ · `pkgdown::check_pkgdown()` "No
problems found" ✓ · NEWS entry present ✓ · no new top-level files (both new
files sit under `tests/testthat/`, already covered) ✓ · `check()` 0/0/0 ✓.
README.Rmd and the vignettes make no claim about codec `NULL`/`NA` semantics
(grepped), so neither needed resyncing. No `DESIGN.md` principle changed, so
`cairn_impact` does not apply. First review round — no prior returns.

CI on PR #45: all five `R CMD check` platform jobs, `pkgdown` and
`test-coverage` pass. `codecov/patch` passes; `codecov/project` fails on a
94.75% → 94.60% total-coverage move (+5 lines, +4 misses). The active profile
declares coverage diagnostic-only and never a merge gate, so this is reported,
not treated as a red gate — but the miss it points at is real and is in the
findings below.

### Independent review (in progress)

Three fresh-context reviewers spawned against `master..HEAD`; the diff-bug
[O] lens has not yet reported, so triage and scoring are not yet done.