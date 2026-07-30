# M42: What `NULL` and column `NA` mean, settled across the codec family

**Status:** done (2026-07-30, PR #45 https://github.com/jmgirard/tidymedia/pull/45)

**Goal:** Give the codec family one recorded answer to "what does `NULL` mean,
and what does a column `NA` mean", with every deliberate departure named.

**Outcome:** `NULL` emits no `-codec:v`/`-codec:a` across all 34 codec verb ×
argument pairs; a column `NA` is its per-row form. Three splits closed:
`anonymize_pipeline()`'s unconditional `check_token()` (sole cause of
`anonymize_video`/`_batch` refusing `NULL`, the batch one inside `pmap`),
`extract_audio()` gaining `allow_null`, and the last three codec columns moving
off `str_cols`/`check_batch_string_col()` onto `check_batch_codec_col()` +
`batch_codec_cell()`. `check_token()` gained `allow_null`; the probe gained a
`col = "na"` cell and `codec_guard_semantics()`; `helper-codec-family.R` shares
the verb list with M41's sweep. 30 changed cells, zero compiled→compiled.

**Decisions:** D022 — the family rule, `convert_audio`/`_batch`'s `-q:a 0` the
sole recorded departure; supersedes D021's `extract_audio` bullet. A gated
amendment widened Scope 3/AC4/T5 from one no-`NA` column to the three measured.

**Review:** 16 findings, three lenses; 5 scored ≥80 and fixed (F12 93 wrong
work-log split, F6 90 broken `.webm` doc example, F1 87 refusal message denying
`NULL`, F4 85 untested guard, F3 80 NEWS gaps + unpinned precedence); F8 78 and
F2 68 fixed at the maintainer's direction; 8 logged. F5 76 → candidate row.
