# M47: Stop `standardize_video()` and `anonymize_video()` picking an audio track by disposition

**Status:** done (2026-07-30, PR #50 https://github.com/jmgirard/tidymedia/pull/50)

**Goal:** Give both verbs (+ `_batch`) an explicit audio map on every call and an
`audio_stream` selector, so which tracks survive stops being a property of the input's flags.

**Outcome:** `pass_through_maps()` (`R/ffmpeg.R:281`) reuses `audio_stream_map(null_map = "0:a?")`
and prepends `0:v?`, emitted in one `ffm_map()` call per pipeline: `-map 0:v? -map 0:a?` unset,
`-map 0:v? -map 0:a:<n>` named. `audio_stream` on both scalars and both `_batch` siblings (argument
+ per-row column, `check_batch_audio_col(na_means = "keep every audio track")`, `batch_stream_cell()`);
no front-door guard on the scalars, deliberately (F8). Breaking: multi-track inputs keep every track
(was one, by DEFAULT disposition — measured 3 in, `spa` out); subtitles no longer carried into `.mkv`;
argument precedes `run`. New fixtures `make_multitrack_video(default_track =)`, `make_silent_video()`,
`audio_languages()`; `test-ffm.R`'s map invariant went from `<= 1` to an exact per-verb count table.

**Decisions:** D026 (pass-through rule; `NULL` = every track; the `?` asymmetry; `-map 0` rejected on
measurement). M47-D1: no diagnostic probe — the default no longer narrows, so M44's warning has no
occasion here and D024's licence is untouched.

**Review:** 3 lenses + scorer; blame-history and prior-review both returned zero. 18 findings, 5
actioned: F2 (92) and F3 (90) were criterion failures — AC8 had been ticked on a grep blind to
per-block coverage — plus F7 (82) and F8 (80, M41's precedence trap in a second shape); F4 (85)
deferred to a row (117 literals). 13 logged; F1 and F4 became rows. CI 9/9 green first pass.
