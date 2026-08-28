# M078: The limit bounds the wait, not the process

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m078-timeout-bounds-the-wait`

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

- [ ] AC1: Every hit of `grep -rni timeout` over `R/`, `man/`, `README.Rmd`,
      `NEWS.md` and `vignettes/` that states or implies the limit terminates
      the spawned program is corrected to say the limit bounds R's wait and
      may be exceeded; the evidence lists the total hit count reviewed and
      each corrected hit by `file:line`. Corrections are made in roxygen and
      `man/` regenerated, never hand-edited.
- [ ] AC2: `NEWS.md` carries a development-version entry stating that the
      timeout may be exceeded, in user-facing words and with no milestone
      number.
- [ ] AC3: A new `cairn/DECISIONS.md` entry states, for the platform triple
      it names, the set limit and the observed elapsed time the committed
      script reported; records that M69 return 2's premise ("bounded 42 s
      exactly as by a bounded 2 s") is falsified by that number, without
      deciding a replacement mechanism; and names the three fixture files
      M077 excluded and what makes them hang.
- [ ] AC4: `Rscript -e 'devtools::test()'` clean and
      `Rscript -e 'devtools::check()'` clean (0 errors, 0 warnings; NOTEs
      justified).
- [ ] AC5: `Rscript -e 'devtools::document()'` produces no diff.

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
- [ ] T3: Run T2 inside T1's container and capture the transcript. Record
      whether base R's +40 s SIGKILL fires, and if the excess is larger,
      whether R is waiting on the process or on the output pipe.
- [ ] T4: Run the AC1 sweep; triage all hits into corrected / not a promise,
      and record the count.
- [ ] T5: Correct the reader-facing promises in roxygen
      (`R/timeout.R:38-100` `@description`/`@param seconds`, the file's
      opening comment at `R/timeout.R:1-10`, `R/tidymedia-package.R`),
      regenerate `man/`, and add the `NEWS.md` entry.
- [ ] T6: Append the D-entry (AC3), and update the Linux-escalation
      candidate ROADMAP row so it carries what T3 found and states what the
      fix milestone now has to decide.
- [ ] T7: `devtools::document()`, `devtools::test()`, `devtools::check()`.

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

## Decisions

## Review
