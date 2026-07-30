# RR03: Restoring `ffm_copy()` / `ffm_concat()` idempotence (M48)

- **Date:** 2026-07-30
- **Reviews:** `cairn/reviews/RB03-ffm-copy-idempotence.md`
- **Reviewer:** independent (Fable), no session context

All compile-level claims in the brief were re-measured in this repo before
answering. `ffm_copy() |> ffm_copy()` compiles `-map 0 -map 0`;
`ffm_concat() |> ffm_copy()` likewise; `ffm_map("0:v") |> ffm_copy()` compiles
`-map 0:v -map 0`. All three reproduce exactly as stated.

## Answers

### 1. Which spelling should restore idempotence?

**Option A, hardened with a guard** (the guard is Q4's subject): `ffm_copy(streams
= TRUE)` sets its map through `ffm_map(object, "0", replace = TRUE)`, and aborts
first if the pipeline already carries a map other than `"0"` itself.

Option A is right rather than merely workable for three reasons:

- **`"0"` is an absolute statement, and appending an absolute statement is never
  correct.** The specifier `"0"` subsumes every other specifier the linear
  builder can address (one input chain, IP2/D003; the concat demuxer collapses
  its inputs into one). So `c(existing_map, "0")` has no composition in which it
  is what the caller wanted: if the existing map is `"0"` it duplicates every
  stream, and if it is anything narrower (`"0:v"`) it duplicates the narrower
  selection's streams. An operation whose right-hand side subsumes any possible
  left-hand side is an assignment, not an increment. `ffm_copy()` executing an
  assignment as an increment is the defect; A fixes the operator, not an
  instance.
- **A restores `ffm_copy()` to the builder's family norm.** Measured: every
  other `ffm_*` setter assigns — `ffm_codec()` overwrites (`R/ffm.R:520-523`;
  two calls compile the last codec only), as do `ffm_pixel_format()` and the
  rest. `object$map` is the *only* accumulating field
  (`R/ffm.R:590` is the sole append in the package), and D023's fourth bullet
  earns that exception for `ffm_map()` specifically, because its arguments are
  partial selections that genuinely compose. `ffm_copy()`'s `"0"` is not a
  partial selection. A puts the one total-selection verb back on the
  assign side of that line and leaves the one genuinely compositional verb on
  the append side.
- **A is the only option that fixes the class (see Q2) and it is the shape M48
  needs anyway.** `segment_video(reencode = FALSE)` must narrow `ffm_copy()`'s
  map to D026's pass-through pair via `ffm_map(replace = TRUE)`
  (`R/ffmpeg.R:2659` + `pass_through_maps()`, `R/ffmpeg.R:326-329`); A leaves
  that mechanism exactly as D023 shipped it.

**Option B — reject.** It changes the contract D023's fourth bullet states, for
a fix that does not even cover the brief's own second measurement:
`ffm_map("0:v") |> ffm_copy()` still compiles `-map 0:v -map 0` under
`unique()`, because the duplication there is by subsumption, not by repetition.
A contract change that leaves half the reported defect standing buys nothing.
It also silently collapses a deliberately repeated specifier inside a single
`ffm_map(c("0:a:0", "0:a:0"))` call, which is a behavior change to a call D023
explicitly legalized (vector `mapping`).

**Option C — reject.** It is B's weakness without B's breadth: it fixes only
the literal `"0"`-twice instance, leaves `ffm_map("0:v") |> ffm_copy()`
duplicating the video stream, and hard-codes a special case ("append unless the
string is exactly this one") into a verb whose semantics should be statable in
one sentence. The next subsumption pair (`"0:a"` then `"0"`) reopens the bug.

No fourth option is better. "ffm_copy stops touching maps entirely" would break
both in-package callers and the vignette's taught composition
(`vignettes/tidymedia.Rmd:117-123`); a tri-state `streams` argument adds API
surface to dodge a semantic question that A answers directly.

### 2. Is "idempotence" the right frame?

The frame is **narrower than the defect**. Idempotence (`copy ∘ copy = copy`)
is one symptom; the brief's own `ffm_map("0:v") |> ffm_copy()` measurement is a
second symptom that no idempotence test catches. The underlying invariant is:

> After `ffm_copy(streams = TRUE)`, the pipeline's map is exactly `"0"` — the
> all-streams selection it documents — regardless of what preceded it; if the
> pipeline already stated a different selection, that conflict is surfaced, not
> silently resolved in either direction.

Of the three candidates only A establishes this: B and C fix repetition but not
subsumption, so under both the compiled command can still map a stream twice
after a chain no user would read as asking for it.

The brief's wider phrasing — "the compiled command never maps the same stream
twice unless the caller asked for it" — is the true north but is **not
establishable at Layer 1**, and no option should pretend to establish it.
Deciding whether `"0:a:1"` duplicates `"0:a"` requires implementing FFmpeg's
stream-specifier algebra in R; that is exactly the full-coverage growth GP1/D001
refuses. `"0"` is the only subsuming specifier the package itself ever writes,
so guarding it at its single source (`ffm_copy()`) covers the whole in-package
class, and the per-verb map-count invariant (`tests/testthat/test-ffm.R:438`)
is the correct guard for everything Layer 2 compiles. User-composed `ffm_map()`
chains that overlap remain the user's stated selection — D023 made appending the
contract precisely so that what the user writes is what compiles.

### 3. Does deliberate double-mapping have a legitimate use here?

Not one that Layer 1 can finish. FFmpeg's map-twice technique exists to produce
two *differently encoded* copies of one stream (two audio codecs, a proxy
video), which requires per-output-stream codec options (`-c:a:0` / `-c:a:1`).
The builder cannot express those: `ffm_codec()` sets one codec per type
(`R/ffm.R:520-523`), so a Layer-1 duplicate can only ever be two identical
copies of the same stream — a half-feature with no research-preprocessing use
case. Under GP1 that belongs at Layer 0, and `ffmpeg()` reaches it fine.

But note the question is moot under the recommended fix: **A does not remove
it.** A single `ffm_map(c("0:a:0", "0:a:0"))` call still compiles two maps, and
chained `ffm_map()` calls still append. Only B removes it, and B is rejected on
other grounds. So: no, stream duplication does not need to be a supported
Layer-1 capability; yes, it may incidentally remain reachable; do not adopt B
to remove it and do not add anything to preserve it.

### 4. Silent discard versus silent duplication

**Neither. The fix should abort, and the package has already decided this
question once.** `segment_pipeline()` (`R/ffmpeg.R:2641-2656`, M35/D017)
confronts the identical shape — a caller-stated value (`audio_codec`) that
`ffm_copy()` would silently overwrite — and aborts rather than letting either
silent outcome stand: "Anything else … would be silently overwritten by
ffm_copy(), so it aborts." A stated map that `ffm_copy()` would silently
overwrite is the same case one field over. D023 itself condemns the pre-M43
overwrite in exactly these terms ("a second call silently discarded the
first"); shipping A without a guard would re-introduce, for this one verb, the
precise flaw D023 was written to remove.

The guard: `ffm_copy(streams = TRUE)` on a pipeline whose map is non-empty
**and not identical to `"0"`** aborts with a classed `cli::cli_abort()` naming
both legal spellings — `ffm_copy(streams = FALSE)` to keep the existing map
(verified working: it compiles the copy codecs and no map), or calling
`ffm_copy()` first and narrowing afterwards with `ffm_map(replace = TRUE)`.
The `identical-to-"0"` carve-out is what makes the guard compatible with
idempotence: `ffm_copy() |> ffm_copy()` and `ffm_concat() |> ffm_copy()` are
no-op restatements (map already `"0"`), compile one `-map 0`, and stay silent —
AC4 exactly.

Why abort over warn: a warning that fires while compiling a command the caller
did not state is half a fix under this package's doctrine — the compiled
command is the product (D001), and D023/D026 rest on selection being *stated*.
There is also an asymmetry of regret: relaxing an abort to a warning later is
backward-compatible; promoting a warning to an abort breaks running code. On a
pre-0.2.0 clean break (D014) this is the cheap moment to be strict.

One wording constraint: because `ffm_concat()` calls `ffm_copy()` internally
(`R/ffm.R:943`), a user chain `ffm_map(…) |> ffm_concat()` will trip the guard
from a frame the user never called. The message must therefore describe the
*pipeline state* ("this pipeline already sets a stream mapping (`0:v`)"), not
presume the caller typed `ffm_copy()`. That chain was silently buggy before
(`-map 0:v -map 0`), so converting it to an error is a fix, not a regression.

### 5. Complex mode

No bad interaction from A; one theoretical edge in B (moot, rejected).

- The synthesized `-map "[vout]"` lives outside `object$map`
  (`R/ffm.R:1216-1219`), so neither `replace = TRUE` nor the new guard can
  discard or observe it. A cannot alter any blessed verb's auto map.
- hstack/vstack/overlay never call `ffm_copy()` or set `object$map` themselves;
  their task verbs append `<n>:a` via plain `ffm_map()` (`R/ffmpeg.R:4918`,
  `:5063`), untouched by A. The complex-mode explicit-map test
  (`test-ffm.R:980`) passes unchanged.
- `ffm_concat()` sets `concat`/`concat_list` then calls `ffm_copy()` on a map
  that is empty at that point, so its own compiled command is byte-identical
  under A. `ffm_concat() |> ffm_copy()` drops from two `-map 0` to one — the
  intended fix. The existing test at `test-ffm.R:965-976` asserts only the
  leading `-f concat -safe 0 -i` arguments and continues to pass; it should
  gain the map-count assertion (Q7).
- Under B, `unique()` in complex mode would additionally collapse a repeated
  explicit map (`ffm_map("0:a")` twice beside `[vout]`) — a second, unrequested
  behavior change. Rejected with B.

So: no blessed multi-input verb emits a map set it did not emit before, except
the doubled composition being repaired.

### 6. Blast radius and deprecation

**No deprecation affordance. NEWS plus rewritten roxygen is sufficient,** and
D014 says so directly (pre-0.2.0, clean break, no `lifecycle` shims). The
change also qualifies on its merits: the appending behavior being removed
shipped in M43 (D023 is dated 2026-07-30 — it has never been in a release),
produces commands nobody wants, and the
brief establishes no in-package pipeline performs any doubling composition.

Complete list of what the fix touches, from a full-repo survey:

- `R/ffm.R:610-613` — `@param streams` documents appending as the contract;
  must be rewritten (AC4 already requires this). The M43 comment at
  `R/ffm.R:586-590` ("narrowing ffm_copy()'s all-streams `0`") stays true but
  should note `ffm_copy()` now does its own narrowing.
- `R/ffm.R:541-545` — `ffm_map()`'s "Chaining appends" roxygen paragraph
  remains accurate under A (it describes `ffm_map()`, whose contract is
  untouched); re-read at documentation time, no rewrite forced.
- In-package callers: `strip_metadata_pipeline()` (`R/ffmpeg.R:1189`) and
  `segment_pipeline()` (`R/ffmpeg.R:2659`) both call `ffm_copy()` on an empty
  map — compiled commands byte-identical under A. `ffm_concat()`
  (`R/ffm.R:943`) likewise. No Layer-2 command changes except the ones M48's
  AC1/AC2 introduce on the segment copy path deliberately.
- Tests: `test-ffm.R:377` (asserts `p$map == "0"`, passes), `:417` (the
  `replace = TRUE` narrowing test, passes unchanged — it is now also the
  regression guard for the mechanism `ffm_copy()` uses), `:965` (passes;
  under-asserts, see Q7), `:438` (M48 rewrites per AC8).
- `vignettes/tidymedia.Rmd:117-123` — teaches `ffm_seek() |> ffm_copy()` on an
  empty map; unaffected.
- **New user-visible behavior:** the Q4 abort. A pre-M43 script written
  against the *overwrite* era (`ffm_map(…) |> ffm_copy()` compiling one
  `-map 0`) now errors instead of silently changing meaning a second time.
  That is the right failure mode for a clean break and must be in NEWS.

Nothing else in the repo breaks. The fix does not reopen D023's first three
bullets and leaves its fourth bullet standing verbatim; what it adds is a new
contract for `ffm_copy()`, which should be recorded as a decision entry
("`ffm_copy()` assigns; a conflicting prior map aborts; `ffm_map()` still
appends") rather than as an edit to D023.

### 7. What must the tests assert?

AC4 as written is necessary but under-specified in four ways: it pins only the
repetition symptom (not subsumption), asserts via compiled strings only (the
suite's stronger pattern is `ffm_args()` token counting, e.g.
`test-ffm.R:398-405`, which is immune to `-map "[vout]"`-style substring
accidents), says nothing about the guard, and says nothing about the
`streams = FALSE` escape. Required set, absorbing AC4:

- Compile-level, via `ffm_args()`: doubled `ffm_copy()` yields exactly one
  `-map` token followed by `"0"`; `ffm_concat() |> ffm_copy()` likewise
  (extend the test at `test-ffm.R:965`, which currently stops at the input
  arguments and passed throughout the bug's lifetime).
- The guard: `ffm_map("0:v") |> ffm_copy()` aborts (classed condition;
  `expect_error` on class, not message regex alone), and the message names
  `streams = FALSE`. `ffm_map("0:v") |> ffm_copy(streams = FALSE)` compiles
  copy codecs with exactly one `-map 0:v` — the escape works and is pinned.
- Contract non-regression for `ffm_map()`: the append test (`:407`) and the
  `replace = TRUE` test (`:417`) pass **unmodified** — evidence the fix landed
  in `ffm_copy()`, not in `ffm_map()`.
- `strip_metadata()`'s compiled command byte-identical to master (it is the
  untouched `ffm_copy()` caller; a committed-literal assertion per the
  `baseline_pair()` pattern M48 T1 already uses).
- Execution (skip_if no ffmpeg): AC4's 5-stream doubled-copy case, asserting
  the output stream count **exactly equals** the input's (tolerance zero), via
  ffprobe. The concat composition needs no separate execution test — after the
  compile-level fix its map state is identical to the single-copy case.
- The per-verb invariant at `:438` (AC8) stays exact-count per verb; it is the
  Layer-2 guard for the class-level invariant Q2 shows Layer 1 cannot fully
  own.

## Beyond the brief

- **`object$map` is the builder's only accumulating field.** Every other
  setter assigns. This asymmetry is worth one sentence in `ffm_map()`'s roxygen
  ("the only builder verb that accumulates"), because it is now the load-bearing
  distinction between `ffm_map()` and `ffm_copy()`.
- **`ffm_concat()` maps all streams unconditionally**, so concatenating
  subtitle-bearing `.mkv` inputs into `.mp4` will fail the same way D026
  measured for `-map 0` (exit 8, no default mp4 subtitle encoder). Same class
  as the standing subtitle/data-carriage candidate row; do not fold into M48,
  but the row should mention `ffm_concat()`/`concatenate_videos()` when next
  touched.
- **The brief's line references all check out** against the working tree
  (`ffm_map()` at `R/ffm.R:574`, `ffm_copy()` at `:626`, `ffm_concat()`'s copy
  call at `:943`, map emission at `:1217-1234`, `pass_through_maps()` at
  `R/ffmpeg.R:326`, callers at `:1189` and `:2659`).

## Recommendations

1. **Apply** — Option A: `ffm_copy(streams = TRUE)` sets its map via
   `ffm_map(object, "0", replace = TRUE)`; `ffm_map()`'s append contract
   (D023 fourth bullet) is untouched.
2. **Apply** — the conflict guard: abort (classed `cli::cli_abort()`) when
   `streams = TRUE` meets a non-empty map not identical to `"0"`; message is
   pipeline-state-worded (not caller-presuming, because `ffm_concat()` calls
   `ffm_copy()` internally) and names `streams = FALSE` and
   `ffm_map(replace = TRUE)` as the two legal spellings.
3. **Apply** — record the new `ffm_copy()` contract as a decision entry
   (assign + abort-on-conflict), leaving D023 unedited; NEWS entry covering
   both the idempotence fix and the new abort; rewrite `@param streams`
   (`R/ffm.R:610-613`) and touch the comment at `:586-590`.
4. **Apply** — the Q7 test set, including the byte-identical `strip_metadata()`
   baseline and the unmodified-`ffm_map()`-tests evidence.
5. **Consider** — one roxygen sentence noting `ffm_map()` is the builder's
   only accumulating verb.
6. **Consider** — when the subtitle-carriage candidate row is next groomed,
   extend it to name `ffm_concat()`/`concatenate_videos()`.
7. **Reject** — Option B (`unique()` in `ffm_map()`): changes a documented
   Layer-1 contract yet leaves the subsumption half of the defect
   (`ffm_map("0:v") |> ffm_copy()`) unfixed, and silently alters vector
   `mapping` behavior D023 legalized.
8. **Reject** — Option C (append `"0"` only if absent): fixes the instance,
   not the class; the next subsuming pair reopens the bug.
9. **Reject** — any deprecation/`lifecycle` affordance: D014 forbids it and
   nothing in the repo needs it.
10. **Reject** — a warning instead of the abort: the package's own precedent
    for "a stated value `ffm_copy()` would silently overwrite" is an abort
    (M35/D017, `R/ffmpeg.R:2641-2656`), and an abort is the reversible choice
    pre-0.2.0.

## Binding criteria

- BC1: `ffm_copy(streams = TRUE)` sets the pipeline map by assignment through
  `ffm_map(object, "0", replace = TRUE)`; after `ffm_copy() |> ffm_copy()` and
  after `ffm_concat() |> ffm_copy()`, `ffm_args()` contains exactly one
  `"-map"` token (tolerance: exact) and the token following it is `"0"`.
- BC2: `ffm_map()`'s contract is unchanged: no de-duplication is added to
  `ffm_map()`, and the tests at `tests/testthat/test-ffm.R:407` (append) and
  `:417` (`replace = TRUE` narrows) pass without modification.
- BC3: `ffm_copy(streams = TRUE)` on a pipeline whose map is non-empty and not
  identical to `"0"` aborts with a classed `cli` condition whose message names
  `streams = FALSE`; the pinned failing case is
  `ffm_map("0:v") |> ffm_copy()`, and the message is worded around the
  pipeline's existing map rather than presuming the user called `ffm_copy()`
  directly.
- BC4: `ffm_map("0:v") |> ffm_copy(streams = FALSE)` compiles `-codec:v copy
  -codec:a copy` and exactly one `-map` token, `"0:v"` (tolerance: exact).
- BC5: With ffmpeg present, a doubled `ffm_copy()` remux of a multi-stream
  `.mkv` fixture writes an output whose ffprobe stream count exactly equals
  the input's (tolerance: 0); the test `skip_if`s when ffmpeg is absent.
- BC6: `strip_metadata()`'s compiled command is byte-identical to its
  pre-M48 master baseline, asserted as a committed literal.
- BC7: `R/ffm.R`'s `@param streams` prose no longer documents appending; it
  states the assignment and the abort; `NEWS.md` records both the idempotence
  fix and the new abort; `devtools::document()` produces no diff.
- BC8: A decision entry records the `ffm_copy()` contract (assigns;
  conflicting prior map aborts; `ffm_map()` still appends) without editing
  D023's existing bullets.
