# RR02: May a verb run a binary on its executing path purely to emit a diagnostic? (M44)

- **Date:** 2026-07-30
- **Reviews:** `cairn/reviews/RB02-binary-on-executing-path.md`
- **Verdict in one line:** Write the entry, but as a *clarification* of the
  convention's boundary, not an extension of D013's carve-out; tighten the
  discriminator from "result not in the command" to "no effect but a
  diagnostic"; probe up front in the batch verbs; put the helper in
  `R/ffprobe.R`; keep fail-open but fix two traps the draft does not see
  (the locator's own warning, and an overclaim about `run = FALSE`).

## Answers

### Q1 — Is a carve-out needed at all?

The null hypothesis is substantially correct about the convention's letter, and
the evidence is stronger than the brief states. Three facts:

1. The convention as written (`cairn/DESIGN.md:48-49`) constrains
   **compilation** and says nothing about a verb's executing path. `ffm_compile()`
   and the builders run no binary; that is a property of functions, not of a
   time window during a verb call.

2. The executing path has never been binary-pure, and not only because
   `run = TRUE` runs FFmpeg by definition. D011 (`cairn/DECISIONS.md:113-123`)
   deliberately wired a **probe-backed** verifier into execution:
   `ffm_run(verify=)` and `ffm_batch(verify=)` run FFprobe on the executing
   path of any verb that reaches them — including `extract_audio_batch(...,
   verify = ...)` today, via `...` forwarding (`R/ffm_batch.R:122,142-152`).
   Nobody wrote a purity carve-out for D011, because none was needed. M44's
   probe differs from `verify=` in being default-on and pre-run, but not in the
   property the convention protects.

3. The metadata family (`probe_all()`, `get_*()`) runs FFprobe from user-facing
   functions as its entire purpose and has never been read as a violation —
   DESIGN.md lists it as its own function family (`cairn/DESIGN.md:34-35`),
   outside the Layer-2 transform verbs, which is why it never pressed on the
   question. It is weak evidence on its own; D011 is the strong evidence,
   because `verify=` sits *inside* the transform verbs' execution.

So what was D013 actually for? Two things, both real: (a) its analysis pass
runs a binary **under `run = FALSE`** — the one place a binary-free guarantee
did exist and was given up (`cairn/DECISIONS.md:141-144`); (b) it keeps the
analyze step out of `ffm_compile()` even though its output feeds a command.
D013 was not over-cautious about (a); it *was* over-broad in implying the verb
path generally needed an exception.

**Verdict:** D024 should be written — the milestone system demonstrably reads
the convention as forbidding M44's probe (M44's own Scope section does,
`cairn/milestones/M44-implicit-track-drop-warning.md:20-25`), and three queued
pieces of work will face the same question — but as a **clarification** that
draws the line the convention always implied, with D013 recorded as the sole
surviving `run = FALSE` exception. Not as a second carve-out: carving a second
exception ratifies the wrong reading and guarantees a third entry the next time
anyone probes anything. The draft's first bullet already argues exactly this
("the boundary is compilation, not the verb") — the entry just needs to commit
to that framing instead of also calling itself a widened carve-out (see Q7).

### Q2 — Is "the result is not in the command" the right discriminator?

It is the right axis but the wrong cut. "Not in the command" is necessary, not
sufficient, and the brief's three probes show where it leaks:

- **A probe that decides whether to abort.** Its result is not in the command,
  so the draft's operative sentence licenses it — but it is not a diagnostic,
  it is a gate. It changes *whether the command runs*, which is behavior; and
  the draft's own third bullet (diagnostics fail silently) cannot govern it,
  because an abort-gate that fails silently silently stops gating. Outside the
  licence; needs its own entry. (M44's constraints already reject
  abort-on-multi-track for these verbs; the rule should reject the *pattern*.)

- **A probe whose result changes a default the caller did not set.** The result
  is in the command (the resolved default is compiled), so the discriminator
  catches it — and D023 independently forbids it ("a heuristic consulted only
  sometimes is still a heuristic", `cairn/DECISIONS.md` D023 second bullet).
  Correctly stopped.

- **A probe that decides between two commands.** Result-in-command in effect if
  not in bytes; worse, it breaks `run = FALSE` (which command would a dry run
  return?). D013-class at best; needs its own entry.

The durable cut is by **effect**, not by destination of the bytes: *a probe may
run on the executing path iff its outcome — run, skip, succeed, fail — changes
nothing observable except whether a diagnostic condition is signalled. The
compiled command, every default, whether execution proceeds, and which pipeline
executes must be identical under all outcomes.* The draft already contains this
test as evidence ("the compiled command is byte-identical whether the probe
runs, succeeds, fails, or never happens") — promote that from evidence to the
operative rule, and the three leak cases above fall out as corollaries:
anything else (command-feeding, gating, default-resolving, command-choosing) is
D013-class or forbidden, one decision entry each.

This formulation also makes the third bullet a theorem rather than a policy:
a probe whose only possible effect is a diagnostic *must* fail open, because
failing closed would give it a second effect.

### Q3 — The batch ordering problem

The collapse is only apparent, and dissolving it is the same move as Q1:
"executing path" must be defined **modally** (the call has `run = TRUE`), not
**temporally** (after `ffm_compile()` has returned). The temporal reading is
incoherent on its own terms: compilation is pure
(`R/ffm.R:1360-1370` compiles before running; `R/ffm_batch.R:100-116` builds
every pipeline before any run), and a pure function cannot observe whether a
probe ran before or after it. Any `run = TRUE` call could reorder probe and
compile with no observable difference — so the ordering carries no meaning, and
a rule that hangs on it protects nothing. What the convention protects is:
(i) `ffm_compile()` and the builders never invoke a binary from any path, and
(ii) a `run = FALSE` **call** never invokes a binary (D013 excepted). A probe
at the top of a `run = TRUE` batch call violates neither. Note the scalar path
has the same shape anyway: a probe gated on `run` inside `extract_audio()` also
precedes `ffm_finish()`'s compile — nobody would call that "during
compilation".

**Structure:** probe up front in the Layer-2 batch verb, before `ffm_batch()`,
gated on `run = TRUE` and skipped for rows that name `audio_stream` (AC4's
no-probe case is then trivially checkable at the verb). Reject the engine hook:
`ffm_batch()`'s contract is generic (any verb, any pipeline), and a
track-narrowing diagnostic is specific to two verbs' semantics — D011 already
settled that verb-agnostic verification may live in the runner (`verify=`) but
verb-specific meaning does not, and changing a shared engine signature for one
diagnostic inverts the thin-verb economy (IP1). Up-front also happens to be the
better diagnostic: the aggregated warning fires *before* a potentially long
batch spends hours encoding, while the caller can still stop and add
`audio_stream`, and before FFmpeg's console output buries it.

The cost profile is acceptable and mostly not a choice: `ffm_batch()` already
front-loads all building, and the total probe cost is identical wherever the
probes sit. Two cheap mitigations the implementation should take or note:
probe only rows lacking `audio_stream` (already settled), and probe each
**unique** input once — jobs tables legitimately repeat an input (the package's
own examples do, `c(video, video)`), and the answer is per-file, not per-row.

### Q4 — Where does the probe helper live under IP1?

IP1's sentence ("Layer 2 verbs never glue their own command strings",
`cairn/DESIGN.md:57-59`) is written about FFmpeg command assembly, which is
Layer 1's monopoly (D002). FFprobe assembly has never gone through the `ffm_*`
builder and must not start now (IP2: the builder models FFmpeg pipelines).
But IP1's *spirit* — token assembly lives once, in a place whose job it is —
still binds. The package already has that place: `R/ffprobe.R`, where
`probe_one()` (`R/ffprobe.R:110-134`) builds FFprobe token vectors and hands
them to `run_program()`.

**Required placement:** one internal helper (e.g. `count_audio_streams(input)`)
in `R/ffprobe.R` beside `probe_one()`, building the settled narrow token vector
(`-select_streams a -show_entries stream=index ...`) and returning a count or
`NA`; all four verbs call it. No FFprobe token literal appears in any Layer-2
verb body. A separate file à la `R/loudnorm_two_pass.R` is not warranted —
D013's orchestrator is a 339-line analyze-parse-assemble subsystem; this is a
~20-line counter, and scattering one-function files costs more than it buys.
The *warning* logic (message text, count, index-offset wording, batch
aggregation) is verb-domain, not probe-domain: put it in one shared helper
beside the verbs so the scalar and batch messages cannot drift (the M19/M35
scalar-batch divergence lesson M44's own plan cites).

### Q5 — Does the silent-failure disposition have a hidden cost?

Fail-**open** is right and should be kept absolutely: a diagnostic must never
block the job it annotates, and per Q2 this is forced, not chosen. But
"silent" conflates two failure classes with different right answers, and the
draft misses an implementation trap that makes true silence *harder* than the
alternative:

- **Unprobeable input** (per-file): silence is correct. FFprobe and FFmpeg can
  disagree about readability; warning "could not check" on an input FFmpeg then
  processes fine is noise, and `probe_all()`'s failure contract
  (`R/ffprobe.R:89-94`) already covers the caller who wants to investigate.

- **Absent FFprobe** (per-machine): here the draft's cost analysis is
  incomplete. First, the trap: `find_ffprobe()` → `find_program()` **already
  warns** when the binary is missing ("Failed to find ffprobe...",
  `R/program_management.R:44-50`), and `run_program()` **aborts** on a
  NULL location (`R/program_management.R:112-114`). A helper that naively calls
  `find_ffprobe()` on an FFprobe-less machine either warns (not silent) or
  aborts (violates AC3). Silence must be built: short-circuit on the locator
  *before* its warning path (e.g. a quiet locate that returns `NA` without
  signalling), with the whole probe additionally wrapped fail-open. Second, the
  reliance concern is real but the fix is documentation plus an honest
  contract, not runtime chatter: the four verbs' roxygen must state the warning
  is best-effort — emitted *when FFprobe is available and the input can be
  probed*, silently skipped otherwise — so the documented contract never
  promises more than the code delivers. A once-per-session notice
  (`rlang::inform(.frequency = "once")`) when the check is skipped for a
  missing binary is a reasonable middle course — machines with FFmpeg but not
  FFprobe are rare (they ship together), so the message would almost never
  fire, and when it does fire it is telling the truth exactly once. I mark it
  *consider*, not required.

On the falsifier: M44's plan note ("a warning present and still missed") is
about a warning that fired and drowned; a skipped check is a different failure
mode and is bounded by how rare FFprobe-less-but-FFmpeg-ful machines are. The
approach survives — provided the docs say "best-effort" (BC6) and the warning
is classed so pipelines that *do* rely on it can test for it (BC5).

### Q6 — Scope of the licence

Neither a pure enumeration nor a bare predicate. Write it as a **general rule
stated by its conditions, with the occasioning verbs named as instances**:

> A verb may run a binary on its `run = TRUE` path when (i) the outcome affects
> nothing but a diagnostic condition, (ii) it fails open, (iii) it never runs on
> the `run = FALSE` path, and (iv) it never runs from `ffm_compile()` or any
> builder. First instances: the track-drop warning on `extract_audio()`,
> `convert_audio()`, and their `_batch` siblings. A new verb adopting a probe
> under these same conditions records the adoption in its own milestone's
> decision log; a probe that stretches any condition needs a new D-entry.

Why this beats the alternatives, tested against the three queued readers:

- **Enumeration** ("these four verbs") forces a content-free D025 the moment
  candidate (a) carries `audio_stream` — and the warning — to the pass-through
  verbs, though nothing about the rule changes. Ceremony without decision.
- **The predicate** ("verbs that narrow a multi-track input to one track")
  invites the worst misreading: it states the *occasion* for this warning but
  reads as the *licence condition*, so M45 (b) — where `NULL` means every
  track, nothing narrows by default, and no drop-warning applies — would
  wrongly conclude that any diagnostic probe is closed to it, while candidate
  (a) would wrongly conclude that gaining "narrowing" status auto-licenses any
  probe, including behavioral ones.
- The conditions formulation gives each reader the right answer directly:
  (a) reuses the warning under the same conditions, one work-log line;
  (b) gets no drop-warning (nothing dropped) and, if M45's escape wants a probe
  that *decides* anything, condition (i) sends it to its own entry — correct,
  because that is a D013-class question; (c) is cleanup, not probing — out of
  scope on its face.

GP1's probe-creep worry is answered by conditions (i)–(iv) plus the
record-in-milestone requirement, not by making four verb names load-bearing.

### Q7 — The drafted text as a durable record

Reject the draft as written; keep most of its material under the clarification
framing. Four specific defects, worst first:

1. **A factual overclaim that contradicts D013 on its face.** Bullet 1:
   "`run = FALSE` on every verb in the package returns a compiled command with
   no binary having run." False — D013's recorded consequence is precisely
   that `normalize_audio(two_pass = TRUE, run = FALSE)` **does** run the
   analysis pass (`cairn/DECISIONS.md:141-144`; `run_loudnorm_analysis()`,
   `R/loudnorm_two_pass.R:105-124`, runs unconditionally before `run` is
   consulted). In an append-only decisions file this sentence would stand as a
   wrong statement of the package's one existing exception. Correct to:
   "`run = FALSE` on every verb returns a compiled command with no binary
   having run — with `normalize_audio(two_pass = TRUE)` (D013) the sole
   exception, which this entry leaves as the only one."

2. **The entry argues against its own framing.** Bullet 1 establishes that the
   convention never protected the executing path (no carve-out exists to
   extend); bullet 2 then says "the carve-out widens on two axes at once —
   from one verb to four and from command-building to diagnostics." Both
   cannot be true, and the widening language is the misreading Q1 rejects (it
   also miscounts — with `normalize_audio` still licensed it is five verbs,
   not four). Delete the widening sentence; state instead that D013 becomes
   the sole `run = FALSE` exception and M44's probe is the first *diagnostic*
   instance of the clarified rule.

3. **The title misdescribes the rule.** "Which paths may run a binary **while
   building a command**" — M44's probe builds nothing; its whole point is that
   no command depends on it. Retitle to name the boundary, e.g. "D024 — The
   pure surface is compilation and `run = FALSE`; diagnostics may probe the
   executing path".

4. **The operative rule is buried and understated.** The byte-identity test —
   the entry's strongest sentence — appears as supporting evidence inside
   bullet 2 while the headline rule ("its result is not in the command")
   under-constrains (Q2's abort-gate leaks through). Lead with the
   effect-based rule from Q2 and let "not in the command" be its corollary.

Also: bullet 3 ("a diagnostic probe fails silently; a command-building probe
aborts") is correct and worth keeping, recast per Q2 as a consequence rather
than a stipulation; the exclusions bullet should be rewritten per Q6 (drop the
"verbs that narrow" predicate as licence language); and the companion
DESIGN.md line should name the boundary, not just cite entries — e.g.:
"Command **compilation** is pure and CI-safe (no binaries), and `run = FALSE`
runs no binary (sole exception: D013's analysis pass); the `run = TRUE` path
may run helper binaries — D013's analysis pass, D024's diagnostic probes."
The draft's proposed line ("D013/D024 qualify what the *executing* path may
run before the command") repeats the temporal framing Q3 rejects.

## Beyond the brief

- **B1 — M44's AC4 contradicts the settled implementation choice.** AC4 reads
  "`_batch` siblings warn per row"
  (`cairn/milestones/M44-implicit-track-drop-warning.md:50`) while the
  work log's implement-gate settlement (line 95) fixes *one aggregated warning
  naming every affected row*. AC4's text needs updating to the aggregated form
  before evidence is collected against it, or the review gate will fail a
  correct implementation.
- **B2 — AC5's "extends D013" phrasing.** If the clarification framing is
  adopted, AC5's requirement that the entry "extends D013" is satisfied in
  substance (the entry restates and re-scopes D013) but not in the word;
  record the reframe in M44's "Deviations from RR02" table or touch AC5's
  wording at the same time as B1.
- **B3 — Testing the absent-FFprobe path.** AC2's `PATH`-masking trick masks
  ffmpeg and ffprobe together, so it cannot produce the "FFprobe absent,
  FFmpeg present" machine AC3 describes. That path needs
  `testthat::local_mocked_bindings()` on the locator (or equivalent), not PATH
  games — worth knowing before T4 is written.
- **B4 — Duplicate-input probing.** See Q3: probing unique inputs once is a
  two-line `!duplicated()` guard and caps the up-front cost on fan-out-shaped
  jobs tables.

## Recommendations

1. **Apply:** Rewrite D024 as a clarification per Q1/Q7: boundary = compile
   purity + `run = FALSE` binary-freedom; D013 recorded as the sole
   `run = FALSE` exception; effect-based operative rule (Q2); conditions-based
   scope with named first instances (Q6); corrected `run = FALSE` sentence;
   new title; recast DESIGN.md line.
2. **Apply:** Probe up front in the batch verbs before `ffm_batch()`, gated on
   `run`; no engine hook (Q3).
3. **Apply:** Single probe/count helper in `R/ffprobe.R`; single shared
   warning-builder helper for scalar/batch message parity (Q4).
4. **Apply:** Build the absent-binary short-circuit so neither
   `find_program()`'s warning nor `run_program()`'s abort can escape the probe
   path (Q5), and document the warning as best-effort in all four verbs'
   roxygen.
5. **Apply:** Give the warning a condition class (repo precedent:
   `tidymedia_sequential_plan`, `R/ffm_batch.R:182`) so a caller who has
   legitimately settled on the first track can suppress it surgically.
6. **Apply:** Fix M44 AC4's per-row wording to the settled aggregated warning
   (B1) and reconcile AC5's "extends" wording (B2).
7. **Consider:** Once-per-session notice when the check is skipped because
   FFprobe is absent (Q5) — honest, near-zero-frequency; omit if it complicates
   AC3's test.
8. **Consider:** Probe unique inputs once in the batch verbs (B4).
9. **Reject (with reason):** An `ffm_batch()` probe hook — verb-specific
   semantics in a generic runner, an engine-contract change for one diagnostic
   (Q3).
10. **Reject (with reason):** Making FFprobe a hard requirement of the audio
    verbs or letting the probe abort — a diagnostic with a second effect is no
    longer a diagnostic (Q2, Q5).

## Binding criteria

- BC1: The ratified D024 entry is framed as a clarification and asserts all
  three of: (i) `ffm_compile()` and every `ffm_*` builder run no binary from
  any path; (ii) every verb's `run = FALSE` call runs no binary, with
  `normalize_audio(two_pass = TRUE)` (D013) named as the sole exception;
  (iii) a `run = TRUE` call may run a binary before or after compilation
  provided the probe's outcome changes nothing observable except a diagnostic
  condition. It contains no sentence claiming `run = FALSE` is binary-free on
  *every* verb without the D013 exception attached.
- BC2: The entry's operative rule is effect-based: it licenses only probes
  whose outcome affects nothing but a diagnostic condition, and it states that
  a probe whose result changes the compiled command, resolves a default,
  decides whether execution proceeds, or selects between pipelines is outside
  the licence and requires its own decision entry.
- BC3: The batch probe runs in the Layer-2 batch verbs before `ffm_batch()` is
  called, only when `run = TRUE`; `ffm_batch()`'s signature and behavior are
  unchanged by M44 (its formals are identical before and after the milestone
  diff).
- BC4: Exactly one internal helper performs the stream-count probe, it lives in
  `R/ffprobe.R`, and no FFprobe token vector is assembled in any Layer-2 verb
  body (grep for `-select_streams` outside `R/ffprobe.R` returns no R-code
  hits).
- BC5: The track-drop warning carries a documented condition class, and a test
  asserts the class; a test with the FFprobe locator mocked absent shows the
  probe path emits no error and no warning (a once-per-session
  `rlang::inform(.frequency = "once")` message is permitted).
- BC6: The roxygen for all four verbs states the warning is best-effort:
  emitted when FFprobe is available and the input can be probed, silently
  skipped otherwise.
