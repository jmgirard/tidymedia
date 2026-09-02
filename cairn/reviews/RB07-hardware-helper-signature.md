# RB07: The hardware-capability helpers' signature under a two-backend vocabulary (M100)

- **Date:** 2026-09-01
- **Output required:** write findings to `cairn/reviews/RR07-hardware-helper-signature.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

**The package.** `tidymedia` is an R interface to FFmpeg and MediaInfo for
reproducible media preprocessing — batch trimming, cropping, format
standardization, metadata extraction. It is pre-release: version 0.1.x, not yet
on CRAN, and the maintainer is approximately the only caller. It is built in
three layers: a Layer 0 escape hatch (`ffmpeg()`, `ffprobe()`), a Layer 1 pipe
builder (`ffm_*`), and Layer 2 task verbs (`standardize_video()`,
`crop_video()`, …) that are thin wrappers over Layer 1.

**The hardware-encoding surface as it stands at HEAD.** Sixteen exported Layer 2
verbs carry a `hardware = c("none", "nvenc")` argument. `"nvenc"` swaps the
software video encoder for NVIDIA's (`libx264` → `h264_nvenc`). Two exported
helpers back that toggle:

- `hardware_encoder(codec = c("h264", "hevc", "av1"))` — a **pure** lookup
  mapping a codec family to its hardware encoder name. `hardware_encoder("h264")`
  is `"h264_nvenc"`. It asks the machine nothing.
- `has_hardware_encoder(codec = c("h264", "hevc", "av1"))` — a **capability
  probe**: does this FFmpeg build list that encoder? It reads
  `getOption("tidymedia.hardware_encoders")` first (an override seam, also the
  one carried into parallel workers), falling through to a memoized
  `ffmpeg -encoders` parse.

Both helpers were named `has_nvenc()` / `nvenc_encoder()` until 2026-09-01, when
milestone M099 renamed them to the backend-neutral names above on the reasoning
of a prior review in this same series (RB06/RR06, archived at
`cairn/reviews/archive/RR06-d014-rename-window.md`). At that point nvenc was
still the only backend, so the neutral names describe an intent, not yet a
capability.

**What M100 is doing.** Milestone M100 (`cairn/milestones/M100-videotoolbox-backend.md`)
generalizes the nvenc-only surface into a backend *vocabulary* and ships Apple's
`videotoolbox` as its second member. After M100:

- `hardware =` at the 16 verbs accepts `c("none", "nvenc", "videotoolbox")`.
- A per-backend codec-family table decides what each backend emits and what it
  refuses: nvenc covers `h264`/`hevc`/`av1`; videotoolbox covers `h264`/`hevc`
  (its third family, `prores`, is deliberately out of scope — `prores_videotoolbox`
  writes a 0-byte file into `.mp4` and needs a container guard M100 has no slot
  for).
- The two exported helpers must be able to answer *for a named backend*, since
  today they structurally answer only for nvenc. M100's scope states they gain a
  `hardware =` argument (spelled `hardware`, not `backend`, per RR06 Q5 — the
  16 verbs already use that word for the same values).

**Why this needs independent review.** The maintainer escalated one question at
M100's implementation gate: **what that new argument should default to**. The
default of an argument on an exported function is expensive to change once 0.2.0
ships (it would then need a deprecation cycle), and the three candidate answers
trade off differently against each other:

- Default `"nvenc"` — no existing call changes meaning, but on Apple hardware the
  natural, documented call `has_hardware_encoder("h264")` answers `FALSE` while
  the machine can in fact encode H.264 in hardware.
- Default to "any available backend" — the predicate becomes "can this machine do
  hardware H.264", which is the question a caller usually means; but the pure
  mapper cannot have that default (it must return one string, and picking one by
  probing the machine would destroy its purity), so the pair would then default
  differently from each other.
- No default (required argument) — every existing call site, example, the
  vignette, and a test skip helper must be edited, and a removed default is
  itself a breaking change.

Two further design questions ride along, plus one wording question about an
acceptance criterion whose named instruments went stale, and one option this
brief is required to put to you because it is the second escalation naming this
mechanism.

## Materials

Read these, in this order. Paths are relative to the repository root.

**The helpers and the machinery under them** — `R/ffmpeg.R`:

- lines 2973–3031: the roxygen block and both exported helpers
  (`hardware_encoder()` at 3020, `has_hardware_encoder()` at 3027).
- lines 3033–3068: `nvenc_available()`, the internal probe that
  `has_hardware_encoder()` delegates to with a `call` threaded. Note the
  option-before-memo read order and the `resolve_timeout()` call sited inside the
  fall-through and above the memo.
- lines 3069–3086: `codec_family()`, which infers a family from a software codec
  name (`"libx264"` → `"h264"`) and aborts naming nvenc by hand.
- lines 3087–3135: `resolve_hw_encoder()`, the internal resolver. It carries its
  own `hardware = c("none", "nvenc")` default and runs its own
  `rlang::arg_match()`.
- lines 3136–3194: `check_nvenc_available()`, the single site where the
  availability abort is worded.

**The 16 exported verbs.** `grep -n 'hardware = c(' R/ffmpeg.R` returns 17 sites:
16 exported verbs and `resolve_hw_encoder()`. You do not need to read all of
them; read two for shape, e.g. `standardize_video()` and its `_batch` sibling.

**The user-facing text that teaches the helpers:**

- `vignettes/workflow.Rmd` lines 74–87 — teaches `hardware = "nvenc"` and
  `has_hardware_encoder("h264")` as the way to ask.
- `NEWS.md` lines 307–313 — M099's rename entry, which states the helpers still
  answer for nvenc because nvenc is the one backend.
- `man/hardware_encoder.Rd`, `man/tidymedia-package.Rd`,
  `man/refresh_ffmpeg_capabilities.Rd` — the three generated topics naming the
  `tidymedia.hardware_encoders` option. `man/` is generated from roxygen; do not
  propose edits there, only in the roxygen source.
- `_pkgdown.yml`, the "FFmpeg capabilities" section (around line 115) — its
  `desc:` prose reads "opt-in hardware (GPU) encoding" at HEAD.

**The call sites of `has_hardware_encoder()` outside `man/`:**
`grep -rn 'has_hardware_encoder(' R/ tests/ vignettes/`. The one that matters for
question 1's cost is `tests/testthat/helper-skip.R:35`, inside
`skip_if_no_nvenc()`.

**The milestone and its record:**

- `cairn/milestones/M100-videotoolbox-backend.md` — the whole file. Its Scope,
  its seven acceptance criteria (AC1–AC7), and its work log, which records the
  criteria audit that ran over the plan and the gate at which this brief was
  convened.
- `cairn/DECISIONS.md`, entry `D077` (search for `## D077`) — the decision that
  gave the helpers their current names. Its section "Why the hardware names move"
  and its `(d)` dispositions bear directly on questions 3 and 5.
- `cairn/reviews/archive/RR06-d014-rename-window.md`, question 5 and its
  recommendation R11 — the prior review in this series, which settled the
  argument's spelling (`hardware =`, not `backend =`) and raised a generic
  `has_encoder(name)` as a separate additive idea.

**Running things.** The package is installed and FFmpeg is on `PATH` on the
authoring machine (an Apple Silicon Mac whose FFmpeg lists
`h264_videotoolbox`, `hevc_videotoolbox`, `prores_videotoolbox` and no nvenc
encoder). `Rscript -e 'devtools::load_all(); has_hardware_encoder("h264")'`
returns `FALSE` there today. You need not run anything to answer; say so if a
measurement would change your answer.

## Questions

1. **What should the `hardware =` argument on `has_hardware_encoder()` default
   to?** Weigh at least: (a) `"nvenc"`, preserving every existing call's meaning
   at the cost of a documented call that answers `FALSE` on a machine that can
   encode in hardware; (b) an "any available backend" default, making the
   predicate mean "can this machine encode this family in hardware" — note this
   changes an existing call's answer on Apple hardware, and consider whether that
   is a fix or a silent behavior change; (c) no default, forcing every caller to
   name a backend. State which you recommend and what evidence would falsify the
   recommendation. If you would choose differently for a package that had already
   shipped 0.2.0, say so and say why the pre-release position changes it.

2. **Should the helpers' accepted set include `"none"`?** The 16 verbs accept
   `c("none", "nvenc", "videotoolbox")`, and RR06 Q5 chose the spelling `hardware =`
   precisely because it is "the word the 16 verbs already use for the same
   values". But `has_hardware_encoder("h264", hardware = "none")` has no evident
   meaning. Is the right answer that the helpers take a narrower set than the
   verbs (and if so, what does the mismatch cost a reader), or that `"none"` is
   accepted with a defined answer (and if so, what answer)?

3. **Should `hardware_encoder()` — the pure mapper — take the same argument, and
   the same default, as the predicate?** D077 says the mapper is "the pure half,
   the way to set the option without hand-typing FFmpeg names, and after M100 the
   only exported view of the per-backend family table", which implies it must
   take a backend. Confirm or contest that. If your answer to question 1 is the
   "any available" default, address directly what the mapper should then do: it
   cannot both stay pure and resolve "any" against the machine.

4. **Is this amendment to acceptance criterion AC6 correct, and does it hold the
   criterion's promise without widening it?** AC6 as written names two
   instruments that moved when M099 shipped between this milestone's planning and
   its implementation: it greps `man/` for `tidymedia.nvenc_encoders`, an option
   name that no longer exists and that now returns no files, and it quotes
   `_pkgdown.yml` prose reading "opt-in NVIDIA nvenc GPU encoding", which now
   reads "opt-in hardware (GPU) encoding". The current text is in
   `cairn/milestones/M100-videotoolbox-backend.md` under `## Acceptance criteria`.
   The proposed replacement, verbatim:

   ```
   - [ ] AC6 The user-facing text describes a vocabulary, not one backend, over a
         domain wider than `man/`: the three topics
         `grep -rln "tidymedia.hardware_encoders" man/` returns — the same three,
         under the option name M099 gave it after this plan was written;
         `vignettes/workflow.Rmd` lines 74-87, which teach `hardware = "nvenc"`
         and `has_hardware_encoder("h264")` as the way to ask; the "FFmpeg
         capabilities" section prose in `_pkgdown.yml`, which M099 already
         generalized to "opt-in hardware (GPU) encoding" and which this milestone
         re-checks rather than rewrites; and a `NEWS.md` entry naming videotoolbox
         and the families each backend covers. `devtools::check()` reaches none of
         these, so AC7 does not backstop it.
   ```

   Ask of it the three questions this project's criteria audit asks: what state
   of the world satisfies it exactly as written; does any standing decision make
   that state unreachable; and does it make a universal claim over a domain no
   procedure it names enumerates. Say explicitly whether the replacement widens
   what the criterion promises relative to the original, since a widening is not
   an admissible amendment here.

5. **Should either exported helper be removed instead of widened?** This project's
   escalation protocol requires that a mechanism reaching its second review lists
   removal among the options. `has_hardware_encoder()` and `hardware_encoder()`
   were reviewed at RB06/RR06 six weeks-equivalent ago in project time; RR06's
   `(d) removal` disposition declined removal — both stay exported — and D077
   records that decline as permanent, reopened only by "a measured report that
   `hardware_encoder()` is used only through the predicate". The new fact since
   that decision is that both helpers are about to grow an argument. Does that
   change the calculus for either one? Answer for each helper separately. If you
   recommend removal, say what supersedes D077's decline and what the deprecation
   path is for a package that has not yet shipped 0.2.0.

## Constraints

Fixed; do not relitigate. Flag disagreement explicitly rather than working
around a constraint silently — a recommendation that contradicts a standing
decision is welcome, but it must say which decision it contradicts.

- **D077** settles the helpers' *names* (`has_hardware_encoder()`,
  `hardware_encoder()`, `tidymedia.hardware_encoders`) and, via RR06 Q5, the new
  argument's *spelling* (`hardware =`, never `backend =`). Questions here concern
  the argument's default, its accepted set, and which functions carry it.
- **D077 also declines permanently**, with stated reopening conditions, three
  changes you may otherwise be tempted to propose: a per-verb `check_tracks =`
  argument, a scalar `timeout =` argument, and renaming `audio_stream`.
- **D014** governs naming: no vendor or member name inside an export's or
  argument's name; full words, no abbreviations (`has_hw_encoder` was rejected on
  this ground). D014's free-rename window closes when 0.2.0 reaches CRAN, which
  has not happened.
- **M100's scope is fixed.** In scope: the backend vocabulary, the per-backend
  table, backend-aware probing, and the helpers' signature. Out of scope and not
  to be reopened: `prores` under videotoolbox (needs a container guard); the
  `qsv`, `vaapi` and `amf` backends (none testable on hardware this project
  reaches); GPU *decode* / `-hwaccel`.
- **The option seam `tidymedia.hardware_encoders` holds a flat character vector
  of encoder names** and is read before the memo (D044). It is backend-neutral
  already — `"h264_videotoolbox"` is a legal member — and this brief does not ask
  you to reshape it.
- **`hardware_encoder()`'s purity is a property D077 relies on**, not an accident:
  it is what lets a caller compute the option's value without hand-typing FFmpeg
  encoder names. A recommendation that makes it probe the machine must say so
  plainly and supersede that reasoning.
- This project's records style: state facts plainly, no hype adjectives, and
  every claim about the code's behavior derived from the code or an execution
  rather than recalled.

## Output format

In `RR07-hardware-helper-signature.md`: answer each question by number with your
reasoning and evidence; list any additional findings separately under "Beyond the
brief"; end with concrete recommendations, each marked apply / consider /
reject-with-reason. Your report is advisory: this brief's header slot says
`not requested`, so do **not** emit a `## Binding criteria` section.
