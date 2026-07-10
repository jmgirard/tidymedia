# M01: Infrastructure modernization (done 2026-07-10)

**Goal:** current R dev standards so later milestones have automated gates.

**Outcome (PR #1, merged as b768c18):**
- testthat 3e suite: pure `ffm_*`/`ffm_compile()` string tests (incl.
  characterization of known bugs) + binary-gated exec tests; 70 pass / 4 skip.
- GHA: R-CMD-check (macOS/Windows/Linux ×5) + test-coverage; Linux installs
  ffmpeg+mediainfo so exec tests run. All 6 checks green.
- assertthat fully removed → rlang `check_*` + `cli::cli_abort()`/`cli_warn()`
  (helpers `check_ffm`/`check_dim`/`check_file_exists`); DESCRIPTION drops
  assertthat, adds cli/dplyr/tidyr/purrr (+tools/utils, withr Suggests).
- `devtools::check()` = 0 errors / 0 warnings / 0 notes.

**Key decisions:** rlang check_* + cli mix; NOTE-free check (documented 7
exports, namespaced ffprobe); `match.arg`→`arg_match` (exact, noted in NEWS);
CI runs ffmpeg+mediainfo; `new_ffm()` keeps stopifnot (internal invariants).

**Bugs fixed en route:** `mediainfo_parameter()` `--Inform` shell-quoting
(broke get_* on POSIX); `get_volume` `out`→`output` typo.

**Deferred:** builder bugs (setpts, `-filter_complex:v`, pixel_format space,
drop-order) → M02; `probe_*` logic bugs → M04. **Follow-up:** covr reports 0%
locally and on CI (green but meaningless) — instrumentation issue to probe.
