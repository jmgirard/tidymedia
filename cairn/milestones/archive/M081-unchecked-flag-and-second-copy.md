# M081: The unchecked flag, and the second copy of the one predicate

**Status:** done (2026-08-28, PR #85 https://github.com/jmgirard/tidymedia/pull/85)

**Goal:** Close the three of M080's four residual shipped-predicate findings that are real, and bound the first one's class by a walk rather than a hand-list.

**Outcome:** `check_audio_codec_needs_reencode()` and `check_resize_needs_two_inputs()` call `rlang::check_bool()` on the flag they branch on, so all four `na_values()` types signal an `rlang_error` naming the flag where the merge-base signalled a bare `simpleError`. Membership is `unchecked_flag_guards()` in `tests/testthat/helper-na-guards.R` — a walk over the namespace's parsed `check_*` bodies for a required formal made the direct operand of `!`, `&&` or `||` with no prior `check_bool()` — which flags 0 of 30 here and exactly those two on the merge-base. `unreadable_paths()` in `R/utils.R` is the package's one `file.access` site, non-aborting, reached by both `check_paths_readable()` and `check_batch_inputs()`; the merge-base had two. `flag_guard_verbs()`/`flag_guard_specs()` sweep the four exported verbs reaching either guard over six delivery forms × six scalar values. The comment over `reject_duplicate_inputs()` separates the order a later verb inherits from the wording it does not; the function is byte-identical. `NEWS.md` unchanged deliberately — no caller-observable change.

**Decisions:** D059.

**Review:** Three-lens fan-out. Blame-history and prior-review: no findings. Diff-bug: eight — four fixed here (a section header placed between another function's docs and its definition; three false branch-added prose claims), three deferred to a candidate row as instrument limits outside their criteria's stated domains, one a disclosure the milestone already makes. Nothing met the return floor; nothing retired.
