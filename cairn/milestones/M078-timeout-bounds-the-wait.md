# M078: The limit bounds the wait, not the process

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m078-timeout-bounds-the-wait` / https://github.com/jmgirard/tidymedia/pull/82

## Goal

Measure what `options(tidymedia.timeout = )` actually does on the platform
where it was seen to fail, in a container the repo can rebuild, and correct
every place the package tells a reader the limit is a bound.

## Scope

Surface tier: **user-facing** — the deliverable is the documented promise
readers act on (`?with_timeout`, `?local_timeout`, `NEWS.md`) plus the
DECISIONS record behind it. The container and the measurement script are
instruments serving it, and no criterion below binds a property of them.

**In:** commit a Dockerfile that rebuilds `tidymedia-floors:r443` (Ubuntu
noble / aarch64, R 4.4.3, ffmpeg) into `data-raw/`; commit a measurement
script that spawns a signal-ignoring child under a set limit and reports
observed elapsed time; run it and record what it found; correct the reader-
facing promise wherever the sweep in AC1 finds it; append a D-entry stating
the measurement and naming M69 return 2's falsified premise.

**Out:**
- Changing the spawn mechanism (processx or equivalent direct process
  control) → the candidate row this milestone's D-entry promotes; not
  plannable until the measurement says whether base R's SIGKILL at +40 s
  fires at all, or R is blocked reading a pipe a dead child's descendants
  still hold.
- Superseding M69 return 2's rejection of `processx` → the fix milestone,
  which is the one with standing to overturn it. This milestone records the
  premise as falsified; it does not decide the mechanism.
- A regression test for the hang → the fix milestone. There is nothing yet
  to regress against, and a test that reproduces the wedge in CI is the
  problem the fix has to solve, not this one.
- The other thirteen shapes in the floor-script hardening candidate row →
  that row, which keeps them. Only its F21 (commit the container) is
  absorbed here.
- Re-enabling the three fixture files M077 excluded from its measurement →
  the fix milestone; AC3 only names them.

## Acceptance criteria

- [x] AC1: Every hit of `grep -rni timeout` over `R/`, `man/`, `README.Rmd`,
      `NEWS.md` and `vignettes/` that states or implies the limit terminates
      the spawned program is corrected to say the limit bounds R's wait and
      may be exceeded; the evidence lists the total hit count reviewed and
      each corrected hit by `file:line`. Corrections are made in roxygen and
      `man/` regenerated, never hand-edited.
- [x] AC2: `NEWS.md` carries a development-version entry stating that the
      timeout may be exceeded, in user-facing words and with no milestone
      number.
- [x] AC3: A new `cairn/DECISIONS.md` entry states, for the platform triple
      it names, the set limit and the observed elapsed time the committed
      script reported, and whether the spawned program was still running when
      R returned; states what that number does to M69 return 2's premise
      ("bounded 42 s exactly as by a bounded 2 s") — falsifying or confirming
      it, as measured — without deciding a replacement mechanism; and names
      the three fixture files M077 excluded, what makes them block, and what
      they did when run in the committed container.
- [x] AC4: `Rscript -e 'devtools::test()'` clean and
      `Rscript -e 'devtools::check()'` clean (0 errors, 0 warnings; NOTEs
      justified).
- [x] AC5: `Rscript -e 'devtools::document()'` produces no diff.

## Coverage

- AC1 → T4, T5
- AC2 → T5
- AC3 → T2, T3, T6
- AC4 → T7
- AC5 → T5, T7

## Tasks

- [x] T1: Write `data-raw/Dockerfile.floors` reproducing the image D055 names
      (Ubuntu noble, R 4.4.3, ffmpeg, the floors harness's install deps).
      Build it and confirm it comes up. `data-raw/` is already
      `.Rbuildignore`d (`^data-raw$`), so no new entry is needed.
- [x] T2: Write `data-raw/timeout-bound.R`: spawn a child that ignores SIGINT
      and SIGTERM (`sh -c 'trap "" INT TERM; …'`) under a set
      `tidymedia.timeout`, and separately the `mkfifo` + ffmpeg case the
      candidate row describes; report set limit, observed elapsed, exit
      status, and whether the child was still alive afterwards. The script
      prints its numbers in a form the D-entry can quote rather than
      transcribe by hand.
- [x] T3: Run T2 inside T1's container and capture the transcript. Record
      whether base R's +40 s SIGKILL fires, and if the excess is larger,
      whether R is waiting on the process or on the output pipe.
- [x] T4: Run the AC1 sweep; triage all hits into corrected / not a promise,
      and record the count.
- [x] T5: Correct the reader-facing promises in roxygen
      (`R/timeout.R:38-100` `@description`/`@param seconds`, the file's
      opening comment at `R/timeout.R:1-10`, `R/tidymedia-package.R`),
      regenerate `man/`, and add the `NEWS.md` entry.
- [x] T6: Append the D-entry (AC3), and update the Linux-escalation
      candidate ROADMAP row so it carries what T3 found and states what the
      fix milestone now has to decide.
- [x] T7: `devtools::document()`, `devtools::test()`, `devtools::check()`.

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in **full mode** (declared surface tier is user-facing). It ran in-session rather than through a fresh-context `[O]` reader, because subagent delegation is disabled for this session; this line records that limitation rather than claiming a reader that did not run. It returned four findings. Three were fixed here: a draft AC1 promising "no topic that promises a bound is left uncorrected" quantified over a domain no named procedure enumerates, and now claims only what `grep -rni timeout` over the five named paths returns; a draft clause requiring the D-entry's numbers be "quoted from the script's own output rather than transcribed by hand" bound a mandated-evidence-quotation property (D-118/D-120 instrument genus) and moved to T2; a draft AC binding "the Dockerfile is committed and rebuilds" bound an instrument property and moved to T1. The fourth, a reachability finding on whether the container must reproduce an excess over limit + 40 s, went to the question gate.
- 2026-08-28: plan gate chose measure-and-document now with the fix as a promoted candidate over measuring and fixing in one milestone, because the mechanism choice is not answerable until T3 says where the 191.8 s goes, and ACs committing to a mechanism ahead of that evidence is the shape that cost M114 gated amendments; falsified by T3 returning a cause that only one mechanism can address, which would make the split pure overhead.
- 2026-08-28: plan gate chose to record M69 return 2's premise as falsified without superseding its rejection over superseding it in this milestone's D-entry, because this milestone changes no mechanism and so has no standing to overturn a mechanism rejection; falsified by the fix milestone finding the un-superseded rejection blocks its own gate.
- 2026-08-28: plan gate chose committing `tidymedia-floors:r443`'s Dockerfile here, absorbing F21 from the floor-script hardening candidate row, over a separate minimal repro container, because one image serves both needs and D055 already names a runner the repo cannot rebuild; falsified by the floors harness's install machinery making the image too slow or too fragile to iterate a timeout measurement in.
- 2026-08-28: implement question gate chose pinning `data-raw/Dockerfile.floors`'s repository to a dated P3M snapshot (`ARG CRAN_SNAPSHOT=2025-04-10`, the base image's own date) over reproducing the ad-hoc image's live-CRAN override verbatim, because the ad-hoc image's `options(repos = c(CRAN = "https://cloud.r-project.org"))` makes two rebuilds free to resolve different harness versions, which is the half of F21 that matters for a floors measurement; the file says in its own header that it is therefore not byte-identical to the runner D055 names. The user declined the third option (rebuild before measuring), so T3 measures in the existing `tidymedia-floors:r443`.
- 2026-08-28: implement question gate chose to run the T2 grid on the host as well as in the container, as explicitly-labelled context in the D-entry rather than as a second platform under AC3, because the fix milestone needs to know whether the overrun is a Linux/container property or a base-R one, and adding evidence widens no criterion.
- 2026-08-28: T1 — `data-raw/Dockerfile.floors` written from `docker image history tidymedia-floors:r443` and built as `tidymedia-floors:r443-rebuild`; it comes up with R 4.4.3 / aarch64-unknown-linux-gnu, ffmpeg 6.1.1-3ubuntu5, MediaInfoLib v24.01 and all seventeen harness packages present.
- 2026-08-28: T2 — `data-raw/timeout-bound.R` written: seven cases (signal-ignoring child and FIFO-blocked FFmpeg, each through `system2(stdout = TRUE)` and `system2(stdout = "")`, plus `input = ""`, plus the Layer 0 `system(intern = TRUE)`, plus the package's own `with_timeout(ffmpeg())`), each in its own capped child process. Two defects in the instrument were found and fixed before any number was quoted: `pgrep -f <marker>` matched the shell R runs the `pgrep` in, so the liveness probe reported a live child in every case (now a bracketed pattern that cannot match itself, with a control that spawns a known-live process, asserts the probe finds it, kills it and asserts the probe clears — a case's liveness line is only readable when both halves say ok); and the driver read each child's stdout through a PIPE, which every descendant inherits, so a case the cap had already SIGKILLed went on being waited for — the script's own subject matter met in its own driver — now redirected to a file.
- 2026-08-28: plan gate chose an AC3 that states whatever the measurement found over one asserting an observed excess above limit + 40 s, because the latter is unsatisfiable if the rebuilt image behaves differently from the ad-hoc one and the only remedy would be an amendment; falsified by a measurement so equivocal that "what was found" states nothing actionable.
- 2026-08-28: T3 — grid run in `tidymedia-floors:r443` and, as context, on the host. Container (Ubuntu noble / aarch64, R 4.4.3, ffmpeg 6.1.1-3ubuntu5), 2 s limit: A1 42.00 s child dead, A2 22.01 s **child alive** (pid 102, the signal-ignoring `sh`), A3 42.02 s dead, A4 42.02 s dead, B1 42.03 s dead, B2 22.01 s **FFmpeg alive** (pid 276), C1 42.41 s dead, aborting `tidymedia_timeout`. Host (macOS 26.6.2 / aarch64, R 4.6.1, ffmpeg 9.0.1): A1 42.03 s, A2 22.02 s child alive, A3 42.03 s, A4 42.01 s, B1 2.01 s, B2 ~2 s, C1 ~2.4 s — all dead but A2. Base R's +40 s SIGKILL fires; the excess is never larger. R waits limit + 40 s when it reads the child's stdout PIPE and limit + 20 s when it does not, and in the second case the program survives — the package uses only the first form. The macOS/Linux split is the FFmpeg build, not R: ffmpeg 9.0.1 dies on the first signal, ffmpeg 6.1.1 blocked on a FIFO does not.
- 2026-08-28: T3 — D055 item 3 does not reproduce in a rebuild of the runner it names. Its 191.8 s under a 2 s limit was not observed in any of 14 cases (largest overrun 40.41 s), and its "six full-suite runs never returned" did not recur: `test-with-timeout.R` 45.61 s, `test-runtime-timeout.R` 267.30 s, `test-timeout-silence.R` 52.11 s, each exit 0, and the full suite pass=6477 fail=0 skip=22 in 445.9 s with `NOT_CRAN=true`. 267 s is about six bounded calls at ~42 s each, which is the shape a "191.8 s isolated run" most plausibly had. Recorded as unreproduced, not disproven: D055's run had the nine floors pinned and testthat/furrr held back, which this run did not reproduce.
- 2026-08-28: **amendment (substantive), AC3.** The shipped AC3 required the D-entry to record M69 return 2's premise as *falsified* by the observed number; the measurement confirms it. Amended at a mini gate to state what the number does to the premise rather than assert which, to say whether the program was still running when R returned (the finding that survived), and to ask what the three files *did* in the container rather than what makes them *hang*. This restores the plan gate's own stated intent — its last work-log line chose "an AC3 that states whatever the measurement found" over one asserting an outcome — from which the shipped wording had drifted. The criteria audit was re-run on the amended clause in **full mode** (declared surface tier is user-facing), in-session rather than through a fresh-context `[O]` reader, because subagent delegation is disabled for this session; this line records that limitation rather than claiming a reader that did not run. It returned no finding: the clause quantifies over three named files and one named platform triple, binds no instrument property, and is satisfiable whichever way the premise falls.
- 2026-08-28: T4 — AC1 sweep run: `grep -rni timeout R/ man/ README.Rmd NEWS.md vignettes/` returns 195 hits (R/ 138, man/ 39, NEWS.md 18, README.Rmd 0, vignettes/ 0).
- 2026-08-28: T5 — corrected, in roxygen with `man/` regenerated: `R/timeout.R` file comment (the seam now says it stops the CALL, not the program, with the measured lag), `with_timeout()` and `local_timeout()` `@description` (each said a program "is bounded by `seconds`" with no lag anywhere in its own topic) and `@details` (a new paragraph giving the arithmetic and the 42.0 s measurement; "bounds each row at `seconds`" → "waits `seconds` on each row"), `R/tidymedia-package.R:36` and `:85` (same "bounded at" shape), and its disclosure paragraph, which kept its accurate "up to 40 seconds" and gained the measured number, the survivor case and the FFmpeg-build dependence. Internal comments saying the limit "killed" a child/probe/call corrected in `R/timeout.R`, `R/ffprobe.R` and `R/ffm_manifest.R` — R stops waiting either way, and whether the program died is a separate question. `NEWS.md`: the batch bullet's "how many jobs the limit killed", the `with_timeout()` and `local_timeout()` bullets' "is bounded by `seconds`", and the disclosure sentence's numbers. AC1 evidence: 195 hits reviewed; corrected by `file:line` are R/timeout.R:5-6, :62, :152, :277, :334; R/ffm_manifest.R:116, :150; R/ffprobe.R:243, :248, :253; NEWS.md:35, :48, :84, :96. The falsified sentence ITSELF contains no occurrence of "timeout" and so is not a sweep hit — corrected anyway, and reported separately rather than counted as a hit: R/tidymedia-package.R:36, :85, :94-99 and NEWS.md:63-66, :86, :98, plus their `man/` mirrors.
- 2026-08-28: T5 — AC2 entry added to `NEWS.md`'s development version, in user-facing words and naming no milestone.
- 2026-08-28: T5 — `tests/testthat/test-runtime-timeout.R` gains a guard on the MEASURED number. The standing guard asserts both docs contain "40 seconds", and it was green throughout the period `?with_timeout` and `?local_timeout` promised a bound with no lag mentioned in their own topics — it reads `?tidymedia`'s Rd and cannot tell which topic satisfied it. The new one fences "42.0 seconds", so dropping the arithmetic back to a bare "may be exceeded" reddens.

- 2026-08-28: T6 — D056 appended to `cairn/DECISIONS.md` (the grid, both platforms, liveness per case; M69 return 2's premise recorded as CONFIRMED; D055 item 3 recorded as unreproduced rather than disproven, with D055 left unamended). The Linux-escalation ROADMAP row's "the 2026-08-28 measurement (191.8 s ...) contradicts the 'bounded 42 s' premise" sentence replaced with what was measured; the row stays open on its original promote-on trigger, per the mini gate.

- 2026-08-28: T7 — `devtools::document()` produces no diff (AC5, run after the last roxygen edit and `git status` on `man/`/`NAMESPACE` clean). `devtools::test()` on the host: **FAIL 0 | WARN 12 | SKIP 5 | PASS 6692**; the 12 warnings are the pre-existing dropped-audio-track messages and the 5 skips are the nvenc and video-codec hardware gates, none touched here. `devtools::check()`: **0 errors, 0 warnings, 0 notes**, 2m 42s.

## Decisions

## Review

Fresh evidence, 2026-08-28, on the branch at `18ec465` (PR #82). `master` had
not moved since the branch was cut (`origin/master` = `6bcf477`, an ancestor of
HEAD), so nothing was merged in and no evidence is stale.

- **AC1 — verified.** `grep -rni timeout R/ man/ README.Rmd NEWS.md vignettes/`
  re-run at review: **202 hits** (R/ 140, man/ 40, NEWS.md 22, README.Rmd 0,
  vignettes/ 0). The count is above T4's pre-correction 195 because the
  corrections themselves add occurrences. Triage re-run at review over the whole
  202: no surviving hit states or implies the limit terminates the spawned
  program. The remaining termination words are all correct as written — the
  three-signal escalation (`R/timeout.R:73`, `R/tidymedia-package.R:97`, their
  `man/` mirrors, `NEWS.md:65`), the explicit denial that termination is
  promised (`R/timeout.R:298`, `R/tidymedia-package.R:105`), and the
  partial-output paragraph's "the killed run"
  (`R/tidymedia-package.R:74,:77`), which describes the case where the program
  did die. The surviving "bound"/"bounded" hits are titles, the per-call vs
  session framing, and the sequential-vs-parallel parity claim — none a
  termination promise. Corrections are in roxygen with `man/` regenerated:
  `devtools::document()` leaves `man/` and `NAMESPACE` clean (AC5), which no
  hand-edited `.Rd` would survive.
- **AC2 — verified.** `NEWS.md:86-101`, inside the `# tidymedia (development
  version)` heading at line 1 (the next `# ` heading is `tidymedia 0.1.0` at
  line 1382). It opens "A timeout limit can be exceeded, and by how much is now
  measured", says the limit is how long tidymedia waits and not how long the
  program may run, gives the escalation and the up-to-40-seconds figure with the
  42.0 s measurement, and says R does not promise the program dies.
  `grep -nE 'M[0-9]{2,3}\b' NEWS.md` returns nothing, so no milestone number.
- **AC4 — verified.** `NOT_CRAN=true Rscript -e 'devtools::test()'`:
  **FAIL 0 | WARN 12 | SKIP 5 | PASS 6692**. The 12 warnings are the
  pre-existing dropped-audio-track messages, the 5 skips the nvenc and
  video-codec hardware gates; neither is touched here.
  `Rscript -e 'devtools::check()'`: **Status OK — 0 errors, 0 warnings, 0
  notes**, 3m 22.1s. No NOTE to justify.
- **AC5 — verified.** `Rscript -e 'devtools::document()'` exit 0, and
  `git status --porcelain` afterwards shows only this milestone file (edited by
  review). `man/` and `NAMESPACE` are unchanged, so no `.Rd` was hand-edited.
- **AC3 — verified, with the measurement re-run at review rather than read off
  the implementation's record.** The entry is `cairn/DECISIONS.md` D056. Taking
  its four required clauses in turn:
  - *Platform triple, set limit, observed elapsed.* D056 names
    `tidymedia-floors:r443` — Ubuntu noble, aarch64, R 4.4.3, ffmpeg
    6.1.1-3ubuntu5 — and gives a seven-case table at a 2 s limit. The whole grid
    was re-run at review in that image
    (`docker run --rm -v "$PWD":/pkg -w /pkg tidymedia-floors:r443 Rscript
    data-raw/timeout-bound.R`), and the container header printed exactly that
    triple. Re-measured against D056: A1 42.01 (42.00), A3 42.02 (42.02), A4
    42.01 (42.02), B1 42.01 (42.03), B2 22.01 (22.01), C1 42.61 (42.41). A2 came
    back 29.20 against D056's 22.01 on the grid run, which overlapped the host's
    `devtools::check()`; re-run alone it gave **22.02**. Six of seven within
    0.2 s first time, the seventh within 0.01 s once the host was quiet.
  - *Whether the spawned program was still running when R returned.* Reproduced
    case for case: dead in A1, A3, A4, B1, C1; **alive** in A2 (pid 33,
    `sh -c trap '' INT TERM; …`) and B2 (pid 276, the blocked `ffmpeg`). The
    script's own liveness control printed `finds-a-live-process=ok
    clear-after-kill=ok` in all eight runs, so each verdict is readable.
  - *What the number does to M69 return 2's premise.* D056 records it
    **confirmed, not falsified** — the premise being that the Goal is "met by a
    bounded 42 s exactly as by a bounded 2 s" — and states no replacement
    mechanism, saying in terms that the rejection is untouched and belongs to
    the milestone that swaps the mechanism. Consistent with the re-run: every
    overrunning case lands on limit + 40 or limit + 20.
  - *The three fixture files M077 excluded.* D056 names `test-with-timeout.R`,
    `test-runtime-timeout.R` and `test-timeout-silence.R`, says what makes them
    block (`local_blocking_input()` at `helper-timeout-sweep.R:308` builds a
    `mkfifo` FIFO nobody writes to — verified in the file), and reports what
    they did in the container. Re-run at review in the same image with
    `NOT_CRAN=true`: **45.82 s / 267.82 s / 52.96 s, fail 0 error 0 skip 0**
    (D056: 45.61 / 267.30 / 52.11, exit 0). None wedged.

### Consistency gate

Universal cairn-file checks: `cairn_validate.py` exit 0, all 16 PASS and 7
advisory OK — including `coverage complete`, `binding criteria` and
`release window` (not fired, so nothing to park at the close). No `DESIGN.md`
principle changed (`Principles touched: —`), so `cairn_impact.py` is skipped.

Toolchain checks, from the `r-package` profile's `consistency-gate` slot:
`devtools::document()` no diff; `NAMESPACE`/`man/`/`data/*.rda` regenerate
clean, so nothing generated was hand-edited; `README.Rmd` is untouched by the
diff, so no re-knit is due; `pkgdown::check_pkgdown()` — "No problems found";
`NEWS.md` carries the user-visible entry with no milestone number; no new
top-level file (both new files are under the already-`.Rbuildignore`d
`data-raw/`, `^data-raw$` at `.Rbuildignore:15`), and `check()` reports 0
notes; `devtools::check()` clean.

### Independent review

Delegation to fresh-context subagents is disabled for this session, so the
three lenses were run in-session against the diff, `git log`/`blame`, and the
archived review record. This line states that limitation rather than claiming
readers that did not run; the implement phase records the same limitation for
its criteria audit. Five findings, ranked.

**F1 [O] — `?tidymedia` still says the limit killed the batch jobs, and its
NEWS twin was corrected in this same diff.** `R/tidymedia-package.R:53-54`
(mirror `man/tidymedia-package.Rd:64-65`) reads "warn once at the end of the
run saying how many jobs the limit killed". The identical sentence at
`NEWS.md:47-48` was changed by this branch to "how many jobs the limit gave up
waiting for". It also misreports the code: the warning at `R/ffm_batch.R:192`
says "{ran_out} job{?s} timed out and did not run to completion". So the topic
this milestone's Goal names still tells the reader the limit killed the jobs,
forty lines above the new paragraph saying the limit promises no such thing.
Not an AC1 failure: neither line carries the string `timeout`
(`:51` has "timed out"), so neither is a hit of the sweep AC1 quantifies over,
and the return floor is not reached.

**F2 [O] — D056's falsifier clause is met by D056's own data.** The entry ends
"Falsified by an overrun above limit + 40 s on any platform", and its own C1 row
is 42.41 s under a 2 s limit — an overrun of 40.41 s. The review re-run gave
42.61 s. The excess is the package's own frame around the wait (C1 goes through
`with_timeout(ffmpeg())`), not a different escalation, which is plainly what the
clause means; as written it records the entry as falsified on arrival. D056 is
on this branch and unmerged, so tightening the clause before the squash is not
an edit to history.

**F3 [S/blame] — the measurement script's summary table is headed with a number
it does not hold.** `data-raw/timeout-bound.R:353` prints the column as
`elapsed(s)` and fills it with `wall`, the driver's wall clock including the
child R process's startup — the review run's summary reads A1 44.20 where the
case block reads `observed elapsed: 42.01`. A future reader quoting the summary
gets numbers ~2.2 s above the ones D056 quotes. Instrument-only: the Scope
section states that no criterion binds a property of the script.

**F4 [O] — ragged rewrap in `NEWS.md`.** The edits left a three-word line at
`NEWS.md:48-49` ("The dropped-track / check behind") and a line past the
paragraph's width at `:127`. Renders identically; a formatter's business.

**F5 [O] — two comment lines pushed past 80 columns.** `R/ffm_manifest.R:116`
(84) and `R/ffprobe.R:248` (83), both from the wording change. The repo already
carries such lines on unmodified rows in the same files, so no stated convention
is broken.

**[S/prior-review] — no regression of a prior review's lesson.** The GitHub
thread surface is empty (`gh api repos/jmgirard/tidymedia/pulls/comments` →
0 comments), as M91 measured for this repo, so the evidence is the archived
`## Review` sections on the touched files: M46, M55, M69, M70, M071, M072,
M073, M074, M077. The nearest lessons are M46's (never key a guard on R's
translated warning text) and M70's O7 (a guard keyed on cli-FORMATTED output):
`is_timeout()` still keys on `status == 124L` and the new guard at
`test-runtime-timeout.R:556-562` greps `.Rd` and `NEWS.md` SOURCE text, so
neither is reintroduced. M69's disclosure retirement (D049) is not undone. The
new guard does inherit the standing guard's topic-blindness — it reads
`?tidymedia`'s Rd and cannot tell which topic satisfied it — which the test's
own comment states rather than hides. No finding.
