# M42: What `NULL` and column `NA` mean, settled across the codec family

- **Status:** planned
- **Priority:** normal
- **Depends on:** M41
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** —

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
3. `standardize_video_batch`'s `video_codec` **column** rejects `NA` via an
   inline `str_cols` guard ([ffmpeg.R:2590](../../R/ffmpeg.R#L2590)), unlike
   every other codec column, on a premise the probe falsifies — the comment at
   [ffmpeg.R:2594](../../R/ffmpeg.R#L2594) calls `video_codec` "a literal
   `libx264` default with no sentinel", but the argument does accept `NULL`.

D021 left this open on purpose: "It is deliberately **not** closed on semantics
… Anyone treating the family as uniform in what `NA` *means* will be wrong on two
of the verbs." This milestone closes it, keeping the departures that earn their
keep and recording why.

**Out:** adding codec arguments to verbs that lack them — D021's three
deliberately codec-less verbs (`format_for_web`, `strip_metadata`,
`concatenate_videos`) stay codec-less, and nothing here reopens that boundary.
Front-door type guards → M41 (this milestone assumes them).

## Acceptance criteria

- [ ] AC1: A `cairn/DECISIONS.md` entry extending D016/D017/D019/D021 records,
      for each codec argument on each task verb and `_batch` sibling, what `NULL`
      means (emit nothing / abort / a specific encoding) and what a column `NA`
      means, plus the rationale for every verb that departs from the family
      default. It resolves splits 1–3 in Scope by name.
- [ ] AC2: `standardize_video`/`_batch` and `anonymize_video`/`_batch` agree on
      `video_codec = NULL` — all four compile the same way, or all four abort
      with the same message shape. Which, and why, is AC1's entry.
- [ ] AC3: `extract_audio` and `extract_audio_batch` agree on
      `audio_codec = NULL`, replacing the split M41 preserved on purpose; M41's
      code comment pointing here is removed in the same commit.
- [ ] AC4: `standardize_video_batch`'s `video_codec` column and its
      `video_codec` argument agree on whether "unset" is expressible — either
      the column moves from the inline `str_cols` no-`NA` guard to
      `check_batch_codec_col()` + `batch_codec_cell()` like every other codec
      column, or the argument stops accepting `NULL`, per AC1's entry. The
      falsified comment at [ffmpeg.R:2594](../../R/ffmpeg.R#L2594) is corrected
      either way.
- [ ] AC5: A test table asserts the resolved meaning of `NULL` and of a column
      `NA` for every codec argument AC1's entry covers, so uniformity — and each
      recorded departure, `convert_audio`'s `-q:a 0` included — is enforced
      rather than only documented. A departure appears in the table as an
      expected departure, never as a skipped case.
- [ ] AC6: Every behavior change has a NEWS entry naming the verb and the old
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

## Tasks

- [ ] T1: Extend M41's `data-raw/` baseline script to also emit each codec
      argument's current column-`NA` outcome, and record the resulting
      argument × {`NULL`, column `NA`} table in this file — measured, not
      re-derived by hand.
- [ ] T2: From T1's table, choose the family default and each departure; draft
      the D-entry and surface it at the implement question gate before any code
      lands. *(RB tripwire: irreversible-api)*
- [ ] T3: Land the `video_codec = NULL` resolution across `standardize_video`,
      `standardize_video_batch` ([ffmpeg.R:2547](../../R/ffmpeg.R#L2547)),
      `anonymize_video`, and `anonymize_video_batch`
      ([ffmpeg.R:1145](../../R/ffmpeg.R#L1145)).
- [ ] T4: Land the `extract_audio` / `extract_audio_batch` resolution
      ([ffmpeg.R:283](../../R/ffmpeg.R#L283),
      [ffmpeg.R:3295](../../R/ffmpeg.R#L3295)); remove M41's pointer comment.
- [ ] T5: Land the `standardize_video_batch` `video_codec` column resolution and
      correct the falsified comment ([ffmpeg.R:2590](../../R/ffmpeg.R#L2590)).
- [ ] T6: Write the AC5 table test; update `@param` prose and every `@param
      jobs` column enumeration for the changed verbs; NEWS entries;
      `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Work log

- 2026-07-29: created by /milestone-plan.
- 2026-07-29: plan gate chose planning this now as its own milestone over a ROADMAP candidate row, because the probe evidence is fresh and a second deferral would leave the family non-uniform in what `NA` means — the exact reading D021 warns against; falsified by the three splits turning out to need one decision each with no shared code, which would make three hotfixes cheaper than a milestone.
- 2026-07-29: plan chose criteria that fix *agreement* between each verb pair rather than naming which way each split resolves, because the direction is T2's gated decision; falsified by a split whose two directions need materially different tasks, which would force the decision back into planning.

## Decisions

## Review
