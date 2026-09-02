# M102: `install_on_win()` verifies the archive and the unpacked programs before it registers anything

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Branch/PR:** `m102-install-verify-before-register` / https://github.com/jmgirard/tidymedia/pull/106

## Goal

`install_on_win()` verifies the archive it downloaded against the digest its
source publishes and confirms the programs the package needs were unpacked
before it writes any remembered location, and every failure on that path past
the front-door argument checks returns `FALSE` or carries a `tidymedia_*`
condition class.

## Scope

Surface tier: **user-facing** — the deliverable is an exported function's
failure behavior and its documented return contract.

**In:** SHA-256 verification of the downloaded archive — against
`<download_url>.sha256` for the package's own default source, against a
caller-supplied `archive_checksum =` for any source; `digest` added to `Imports`
(D081); classed refusals for the download, checksum, extraction and
missing-program failures; registration gated on the files the extraction
actually produced; the consent prompt extended to name the sidecar fetch;
roxygen, `NEWS.md`, `README.Rmd`.

**Out:** Authenticity against a compromised source — the sidecar rides the same
host and TLS session as the archive, so this buys integrity, not provenance; the
2026-09-02 ROADMAP candidate row carries the remainder and the pinned-digest
decline. `set_program()`'s own unconfirmed config write, and classing aborts
originating inside `set_program()` — each to its existing candidate row.
Consent-gate changes beyond naming the second fetch → M101 shipped it.

## Acceptance criteria

- [x] AC1: With `download_url` at its default and `archive_checksum` at `NULL`,
      `install_on_win()` fetches `<download_url>.sha256` before it unpacks, and
      the consent prompt names that fetch beside the archive download. It reads
      a sidecar body that is a bare 64-hex digest, a `sha256sum` two-field
      line, or a `SHA256(<file>)= <hex>` line, case-insensitively, and aborts
      `tidymedia_checksum_unavailable` on a body matching none of those and on
      either shape of a failed fetch — `utils::download.file()` signalling its
      own error, or returning a non-zero status. A fetched digest differing
      from the downloaded file's SHA-256 aborts `tidymedia_checksum_mismatch`
      naming both digests and leaves `tm_config_dir()`'s contents unchanged; a
      matching one reaches the extraction step.
- [x] AC2: `install_on_win(archive_checksum = <64-hex string>)` verifies the
      download against that digest, case-insensitively, and fetches no sidecar
      on any source, the package's own default included; the consent prompt
      names a sidecar fetch only where one happens, and a mismatch aborts with
      AC1's `tidymedia_checksum_mismatch` class. `archive_checksum` defaults to
      `NULL`; where the resolved `download_url` is any string but the package's
      own default URL and `archive_checksum` is `NULL`, the call fetches no
      sidecar, verifies nothing, and emits one message saying the archive was
      not verified. A value that is not a single 64-character hex string is
      refused at the front door with the argument named — asserted over a
      wrong-length, a non-hex, an `NA` and a length-2 value — and that refusal,
      like the other front-door checks, carries no `tidymedia_*` class. (RB
      tripwire: irreversible-api)
- [x] AC3: A failure inside `archive::archive_extract()` aborts
      `tidymedia_archive_unreadable` naming the archive path and the install
      directory, and neither that condition's message nor any `parent` it
      carries contains `archive_extract.cpp` — libarchive's text is replaced by
      the abort's own two facts, not retained. Both of libarchive's failure
      shapes are covered — a body it does not recognize as an archive at all,
      and a well-formed 7z header over a corrupt payload — and the downloaded
      temporary file is removed on the failing path as on the succeeding one.
- [x] AC4: No remembered location is written for a program the extraction did
      not produce; `ffmpeg` and `ffprobe` are required, `ffplay` optional. An
      extraction yielding all three writes three config files and returns
      `TRUE`; one yielding `ffmpeg` and `ffprobe` only writes those two, leaves
      no `ffplay` config file, emits one message naming `ffplay`, and returns
      `TRUE`; one yielding no `ffmpeg`, and one yielding no `ffprobe`, each
      write no config file at all and abort `tidymedia_program_not_extracted`
      naming the absent program.
- [x] AC5: A `utils::download.file()` call that does not deliver the archive
      aborts `tidymedia_download_unavailable` naming the URL, for both shapes
      its signalling contract allows — its own error, and a non-zero return
      status — retaining the base condition as the abort's `parent`.
- [x] AC6: Every `return()` and every `cli::cli_abort()` in
      `install_on_win()`'s own body returns `TRUE`/`FALSE` or passes a `class
      =` beginning `tidymedia_`. The domain is derived: a test walks the
      function's `body()`, collects those two node types, and asserts the
      property of each; the claim is exactly that set, which, as a floor
      against a collector that under-reads, holds at least the five abort sites
      AC1, AC3, AC4 and AC5 name. Exits the walk cannot reach are named, not
      claimed: the front-door `rlang` checks, the `archive_checksum` shape
      check, and aborts inside `tm_confirm()` and `set_program()`; T2 sites
      every classed abort those four criteria name in the body itself, its
      helpers returning values instead. Separately, `@return` names five
      outcomes: a declined confirmation, a download that did not deliver, a
      checksum that did not match, an archive that could not be unpacked, and a
      required program that was not produced.
- [x] AC7: `digest` appears in `DESCRIPTION` `Imports` with a floor version the
      `--only digest` leg of `data-raw/imports-floors.R` passes at, `NEWS.md`
      and `README.Rmd`'s installer paragraph name the verification and the new
      argument, and the profile's checks are clean: `devtools::document()`
      produces no diff, `devtools::test()` passes, `devtools::check()` reports
      0 errors and 0 warnings.

## Coverage

- AC1 → T2, T3, T6
- AC2 → T2, T6, T10
- AC3 → T4, T6, T9
- AC4 → T5, T6, T10
- AC5 → T4, T6, T10
- AC6 → T7, T8, T10
- AC7 → T1, T8

## Tasks

- [x] T1: Add `digest` to `DESCRIPTION` `Imports` (D081 is already appended);
      the floor measurement moves to T8, which has something to exercise.
- [x] T2: Add the sidecar-URL, sidecar-parse and archive-digest helpers, each
      returning a value rather than aborting so AC6's census holds; add the
      `archive_checksum` shape check to `R/utils.R`'s front-door family and
      `archive_checksum = NULL` last in the signature; wire the sidecar fetch
      above the archive download. (RB tripwire: irreversible-api)
- [x] T3: Extend `tm_install_details()` so the prompt names the sidecar fetch,
      keeping M101's property that it names every fetch and write it makes.
- [x] T4: Wrap `utils::download.file()` in a classed refusal retaining the base
      condition as `parent`, `archive::archive_extract()` in one that does not
      (AC3 excludes libarchive's text); temp files to `on.exit()`.
- [x] T5: Replace the blanket `set_program()` loop with a per-program check.
- [x] T6: Commit the two corrupt-archive fixtures and the `data-raw/` generator
      carrying their provenance; write the AC1–AC5 tests, mocking the sidecar
      fetch so a fetch and its absence are told apart, exercising all three
      body shapes, and asserting failures by class, not message text.
- [x] T7: Write the derived-exit test for AC6, proven able to fail by planting
      an unclassed `cli::cli_abort()` and a `return()` of a non-literal.
- [x] T8: Roxygen, `NEWS.md`, `README.Rmd`; measure the declared `digest` floor
      with `--only digest`; then `document()`, `test()`, `check()`.
- [x] T9: Give `tm_unpack()` ownership of the connection
      `archive::archive_extract()` leaks on a mid-read failure, which left the
      archive unlinkable on Windows; assert the leak in the AC3 test.
- [x] T10: Register off the file list `tm_unpack()` returns rather than off the
      install directory, with the repeat-install probe AC4 needs; make
      `tm_fetch()` require the file to have arrived and guard
      `tm_archive_digest()`, so a status-0 fetch delivering nothing refuses as
      AC5's download failure, not a bare `simpleError`; move the unverified
      notice above the consent prompt; harden the AC6 census on a bare
      `return()`, a vector `class =`; correct `@param confirm`.

## Work log

- 2026-09-02: created by /milestone-plan.
- 2026-09-02: criteria audit ran in FULL mode (user-facing tier), fresh-context [O] reader; returned findings on all six drafted criteria — wrong premise inherited from the ROADMAP row (`set_program()` already refuses a missing binary, so the defect is partial registration, not dead locations), two class names naming a category and a severity, an unbounded "six failure branches" hand-list, single-exemplar probes on extraction failure and on registration, an unspecified sidecar parse, a collision with M101's prompt property, and a goal left false by the unclassed `cannot open URL` path. All fixed at the gate; none deferred.
- 2026-09-02: plan gate chose fetching the source's published sidecar digest over pinning a digest in the package because a pinned digest goes stale on every upstream refresh and would refuse a good archive; falsified by an upstream that stops publishing a sidecar, or by a report of a substituted archive passing the sidecar check.
- 2026-09-02: plan gate chose `digest` over `tools::sha256sum()` and over `openssl` because `tools::sha256sum()` first ships in R 4.5.0 and would raise the measured 4.1.0 floor four releases, while `openssl` links a system SSL library; measured 2026-09-02, `digest` declares `R (>= 3.3.0)` and its `algo = "sha256"` matches `tools::sha256sum()` byte for byte. Falsified by the R floor rising to 4.5.0 for an unrelated reason, which makes the base function free.
- 2026-09-02: plan gate chose adding an optional `sha256 =` argument over verifying only the default source because a caller-supplied URL otherwise has no verification path at all; falsified by no outside caller of `install_on_win()` appearing before 0.2.0, which would say the added public argument bought nothing.
- 2026-09-02: plan gate chose requiring `ffmpeg` and `ffprobe` while treating `ffplay` as optional over all-three-required and over register-whatever-unpacked, because nothing in the package calls `ffplay` — it is reachable only through `find_ffplay()`/`set_ffplay()` — while an unpack with no `ffmpeg` must not report success; falsified by a package verb taking a dependency on `ffplay`.

- 2026-09-02: the audit's disposal check returned four repairs that traded one finding for another and three new ones — AC6's derived census still overclaimed (a two-node-type walk cannot see `rlang::check_bool()`'s abort, so the goal stayed false), AC7 had demoted the floor measurement out of the criterion and back into a task, AC3's no-`archive_extract.cpp` rule contradicted T4's parent retention, and AC5's headline was broader than its two shapes. All applied; AC4's non-executable probe dropped as unreachable on Windows.
- 2026-09-02: implement question gate settled three items — the new exported argument's name and shape; whether the published-digest fetch keys on the resolved URL or on the argument being omitted (resolved URL, so a caller who types the default address out still gets a verified install); and the fetch order (published digest before the archive, so an unverifiable source refuses before a long download).
- 2026-09-02: amendment — the new exported argument is `archive_checksum`, not `sha256`. The gate first chose `checksum`; a fresh-context [O] criteria audit of that wording found `checksums` already shipped as an exported logical on the eight `_batch` verbs (the md5 manifest toggle, `R/ffm_batch.R:73`), one letter away with a different type and a different algorithm, and the gate re-decided on `archive_checksum`, which satisfies D078's category rule and its compound-name rule together. AC2, AC6's exit list, the Scope In sentence, T2 and T6 amended.
- 2026-09-02: the amended AC2/AC6 wording re-entered the full criteria audit with a second fresh-context [O] reader that authored none of it. Six findings, four fixed at the gate — AC1 and AC2 both claimed the (default URL, digest supplied) case with opposite requirements, so AC1 gained its `archive_checksum` antecedent and AC2 a precedence sentence; AC6's closed exit list rested on an unstated siting rule, now stated in AC6 and carried out by T2; AC2's `check_*()` siting sentence was an implementation constraint, moved to T2; AC2's mock-observability sentence bound an instrument rather than the deliverable, moved to T6. Declined: renaming the two checksum condition classes (a vocabulary tidy that would drag AC1 along) and a fifth rejected-value probe of a non-character type (`rlang::check_string()` owns that branch, and AC2's domain says "string").

- 2026-09-02: the amendment pushed the plan-owned body over the 150-line cap, so the Acceptance criteria section was compressed in one pass, with the Scope and Tasks sections tightened alongside it when the criteria alone could not carry the cut (159 -> 148 lines). A third fresh-context [O] reader diffed the compressed sections against the pre-amendment file and returned promise-preserving; the instrument clauses the compression lifted out of AC1, AC3 and AC5 landed in T6, and five hyphenated words the re-wrap had broken were rejoined.
- 2026-09-02: T1 — `digest (>= 0.6.29)` declared in `Imports`, and `data-raw/corrupt-archive-fixtures.R` written for T6. Suite clean at 11435 passing, matching M101's baseline. Minor amendment: T1's floor measurement moved to T8, since `--only digest` pins the declared version and runs the suite twice, and until T2 lands nothing in the package calls `digest` for that run to exercise.
- 2026-09-02: T2–T5 land together, because each one alone leaves the suite red: T2's second fetch is only legal once T3's prompt names it (M101's property), T4's classed refusals are what T2's verification path aborts through, and T5's registration gate is what the extraction mock T2 needed had to start satisfying. `check_sha256()` joins the front-door family in `R/utils.R`; `tm_sidecar_url()`, `tm_parse_sidecar()`, `tm_archive_digest()`, `tm_fetch()` and `tm_unpack()` all return values rather than aborting, which is AC6's siting rule; `install_on_win()` takes `archive_checksum = NULL` last, fetches the digest above the archive, puts both temporary files on `on.exit()`, and registers only what the extraction left on disk. `tm_mock_install()` gained an `unpack =` argument and a sidecar-aware download mock; the M098 download test now asserts the class rather than base R's "cannot open URL". Suite clean at 11443 passing.
- 2026-09-02: T6 — the two corrupt-archive fixtures and `data-raw/corrupt-archive-fixtures.R` committed; the generator refuses to write a fixture it has not just watched fail, and refuses to run at all unless the undamaged control archive extracts. The two fixtures reach libarchive's two distinct failure routes, `archive_read_open1()` and `archive_read_data_block()`. AC1–AC5 tests written (208 in the file, suite 11532). Four planted defects each went red: retaining libarchive's condition as `parent` (4 failures), registering all three programs unconditionally (1), giving the front-door check a `tidymedia_*` class (2), and fetching the archive before the digest (2). One portability defect caught while writing the mock: `%||%` first ships in base R 4.4.0 and the package declares 4.1.0, so it was replaced rather than introduced as the repo's first use.
- 2026-09-02: T7 — the derived-exit census walks `body(install_on_win)` for `return()` and `cli_abort()` nodes, seeing through `pkg::fn`, and asserts of each that it hands back a literal `TRUE`/`FALSE` or passes a `class =` beginning `tidymedia_`; the floor asserts the collected classes hold all five AC1, AC3, AC4 and AC5 name, so a walk that found nothing cannot pass vacuously. Both planted defects went red in the real function — an unclassed `cli::cli_abort()` and a `return()` of a non-literal, one failure each — and both are also kept as permanent negative controls beside a compliant one, so a later reader can re-run the discrimination instead of trusting this one-time check. Suite 11547.
- 2026-09-02: T8 — roxygen documents `archive_checksum` and names all five aborting outcomes in `@return`; `NEWS.md` gains the verification entry, the classed-failures entry and a Requirements line for `digest`; `README.Rmd`'s Windows step names the digest fetch and the new argument, and `README.md` was rebuilt. The declared floor was WRONG as first guessed: `digest (>= 0.6.29)` does not compile on R 4.6 — `Calloc`/`Free` were removed from R's headers, and 0.6.37 (2024-08) is the first release using the `R_`-prefixed allocators — so the floor is `digest (>= 0.6.37)`, measured. `--only digest` then passed: no floor moved, and the pinned run's skip count matched the baseline's 18, so it exercised what the baseline did. `devtools::check()` first returned one NOTE, the spelling check flagging `SHA` and `libarchive`; `spelling::update_wordlist()` added both and the re-run is Status OK, 0 errors / 0 warnings / 0 notes. `devtools::document()` produces no diff; no new object is exported, so `_pkgdown.yml` is unchanged.

- 2026-09-02: review checkpoint — PR #106 opened as a draft; `master` had not moved since the branch was cut. `devtools::test()` fresh: FAIL 0, WARN 10, SKIP 18, PASS 11547 (the 10 warnings pre-existing, none on a touched file). AC1–AC6 verified and ticked. `cairn_validate.py` passes all 16 checks; `document()` no diff; `pkgdown::check_pkgdown()` clean; README.md in sync (a rebuild changes only the embedded tempdir paths). AC7 still open: the `--only digest` floor run and `devtools::check()` are still running, and the diff-bug review lens has not yet reported.

- 2026-09-02: review gate fixes — the three-lens fan-out returned 18 findings; 11 fixed on the branch, 6 rejected with reason, 3 deferred to candidate rows (one finding split across two dispositions). The load-bearing one was an unguarded `readLines()` on the sidecar: a `download.file()` reporting status 0 without leaving a readable file left `install_on_win()` through a bare `cannot open connection`, unclassed and invisible to AC6's census. Guarded, with a test proven red without the guard. Guarding it also exposed a latent bug in the AC6 census walker — the inline `function(cnd)` handlers put an empty symbol in a formals pairlist, and binding one in a `for` made the variable missing; the walker now traverses by index. AC4's on-disk claim is now asserted through the real `set_program()` (`tm_mock_install(real_set = TRUE)`), and `tm_archive_digest()` is pinned to a known answer. No finding demonstrated an acceptance criterion failing, so the return floor is not met and status stays `review`.

- 2026-09-02: review returns M102 to `in-progress` under the return floor. AC3 fails on Windows CI: `windows-latest (release)` on 9ae0bfe red at `test-program-management.R:796:5`, `expect_false(file.exists(rec$destfile))` FALSE-expected but TRUE for the `corrupt-payload.7z` fixture — the downloaded temporary file survives the failing path, which is exactly what AC3's last clause forbids, on the only platform `install_on_win()` runs on. Green everywhere else (macOS, four Ubuntu legs, pkgdown, coverage) and green locally, so the defect is Windows-specific; `not-an-archive.7z` passes and the payload fixture does not, which points at libarchive holding the archive open after a mid-read failure. Everything else this review found stands and is committed on the branch: 18 findings from the three-lens fan-out, 11 fixed, 6 rejected, 3 deferred. AC1, AC2, AC4, AC5 and AC6 stay ticked on their own evidence; AC3 is unticked; AC7 was never reached (local `devtools::test()` 11567 pass and `devtools::check()` Status OK, but the `--only digest` floor leg was not run to completion and CI is red regardless). First defect return on this milestone.

- 2026-09-02: T9 — AC3's Windows failure MEASURED rather than inferred: `archive::archive_extract()` opens `file(archive, "rb")` in its own R body and closes it only on the paths reaching the end of the read, so a libarchive failure inside `archive_read_data_block()` leaves it open. Counted on macOS, one leaked open connection for `corrupt-payload.7z`, zero for `not-an-archive.7z`, zero on the succeeding path — the same split Windows CI showed, since POSIX unlinks an open file and Windows refuses to. `tm_unpack()` now opens the connection itself and closes it through `on.exit(tm_close(con))`; extraction through a connection produces a file list identical to the path form (`ffmpeg.exe`, `ffplay.exe`, `ffprobe.exe` from a three-program control). The AC3 test gained a before/after `showConnections()` assertion on both fixtures and on the succeeding control, per the implement gate: `file.exists(destfile)` can only go red on Windows, so the mechanism is asserted where a local run can see it. Planting the old `tm_unpack()` back turned it red on macOS, one failure, on `corrupt-payload.7z` only. `tm_mock_install()`'s extract recorder reads the download linkage off the connection's description now that it is handed one.

- 2026-09-02: T9 verified where the defect lived. CI on 59dc788 is green on all ten checks, `windows-latest (release)` included (14m20s) — the leg that was red on 9ae0bfe at `expect_false(file.exists(rec$destfile))` for `corrupt-payload.7z`. macOS, the four Ubuntu legs, pkgdown and coverage are green alongside it. AC3 ticked on that run.
- 2026-09-02: AC7's evidence re-produced end to end, the floor leg this time run to completion: `Rscript data-raw/imports-floors.R --only digest` resolved `digest 0.6.37` from `~/floor-libs/digest` — the version and the DIRECTORY, which is the script's load-bearing control against a failed install falling through to the user library — and reported `pinned pass=11206 fail=0 err=0 skip=18` against an identical baseline, so no floor moved and the pinned run skipped no more than the baseline. `devtools::document()` produces no diff; `devtools::test()` FAIL 0, WARN 10, SKIP 18, PASS 11570 (the 10 warnings pre-existing, none on a touched file); `devtools::check()` Status OK, 0 errors / 0 warnings / 0 notes. AC7 ticked.

- 2026-09-02: review pass 2 returns M102 to `in-progress`. Two gate failures. (1) The consistency gate: `cairn_validate` `weight caps` FAILs at 153 plan-owned lines against the <150 cap — T9's addition crossed it after pass 1 recorded all 16 checks passing — and the remedy, compressing the heaviest plan-owned section, is amend-via-gate, not review-side. (2) The return floor: the [O] lens demonstrated AC4 failing, `unpacked` being computed by `file.exists()` over the install dir after extraction rather than from what the extraction produced, so on a re-install into the stable default `install_dir` the previous run's binaries satisfy the check and a build missing `ffmpeg` returns `TRUE` — reproduced, and confirmed against `R/program_management.R:649`. AC4 unticked; AC7 unticked for carrying a tick with no evidence line. AC3 verified and ticked on CI at `59dc788`, all ten checks green including `windows-latest (release)`. Fan-out returned 8 findings across three lenses: 5 actioned and returned ([O]1 the unguarded `tm_archive_digest()` escaping unclassed, [O]2, [O]4, [O]5, [P]1 the stale `@param confirm` "will overwrite"), 1 actioned as a question for the fix pass ([O]3), 2 rejected with reason. Second defect return on this milestone.

- 2026-09-02: implement question gate for the fix pass settled two items — the unverified-source notice moves ABOVE the consent prompt, so what a caller approves includes knowing nothing will check the download (AC2 fixes no position for it, so this is not an amendment); and the status-0-with-no-file download failure lands on AC5's existing `tidymedia_download_unavailable` site by making `tm_fetch()` return TRUE only where the file arrived, rather than by adding a class and widening a criterion.

- 2026-09-02: amendment — the Acceptance criteria section was compressed in one pass to clear the weight cap (68 -> 67 lines; "aborts with class X" -> "aborts X", four rationale words dropped), and the Tasks section tightened alongside it when the criteria alone could not carry the cut (45 -> 34 with T10 added). No criterion was added, removed, reordered or widened; the criteria set is exactly the seven it was. A fresh-context [O] reader that authored none of it diffed the compressed section against the current text and returned promise-preserving with one real finding — AC3's "libarchive's text is replaced by the abort's own two facts, not retained" had been dropped — plus two cosmetic ones; all three restored before the text was written. `cairn_validate` weight caps now PASS at 149.

- 2026-09-02: T10 — the AC4 defect fixed at its root: `tm_unpack()` returns the file list `archive::archive_extract()` produces (measured 2026-09-02 against a three-program control archive: `bin/ffmpeg.exe`, `bin/ffprobe.exe`, `bin/ffplay.exe`, relative to the directory and after `strip_components`), and `tm_extracted_programs()` intersects that list with the three registers, so a previous install's binaries in the stable default `install_dir` can no longer stand in for this build's. Planting the old `file.exists()` reading back turned the new repeat-install test red — the reviewer's own reproduction, `bin/` pre-seeded with `ffmpeg.exe` and `ffplay.exe` and the extraction producing only `ffprobe.exe`, which now aborts `tidymedia_program_not_extracted` naming `ffmpeg` and registers nothing. [O]1 fixed where the promise is: `tm_fetch()` returns TRUE only where the file arrived and `tm_archive_digest()` returns NULL rather than a bare `simpleError`, both routing to AC5's existing download refusal; planted back, the new test went red. The unverified notice now fires above the prompt, asserted as a two-event sequence rather than by where the message appears, and red when the notice is moved back below. Also [O]4 (a fresh install dir per loop iteration in the AC1/AC2 tests), [O]5 (the census reads a bare `return()` and a vector `class =`, both kept as controls) and [P]1 (`@param confirm`'s "will overwrite" -> "may overwrite", regenerated into `man/`). Suite FAIL 0, WARN 10, SKIP 18, PASS 11583.

- 2026-09-02: T10 verified on CI and the coverage gate closed. All eight R-CMD-check legs green on `d7600f6`, `windows-latest (release)` included, as are `pkgdown` and `test-coverage`; `codecov/patch` came back 95.89% against a 98.19% target, on three new branches no test reached — `tm_archive_digest()`'s error handler, the `is.null(found)` refusal behind it, and `tm_unpack()`'s unopenable-archive return. Each now has a test: the digest helper returns NULL for a missing file, an `install_on_win()` run whose digest cannot be computed refuses `tidymedia_download_unavailable` and extracts nothing, and `tm_unpack()` on a path that is not there returns NULL. Suite FAIL 0, WARN 10, SKIP 18, PASS 11588. One earlier full run reported FAIL 1 amid `ffmpeg` "Interrupted system call" noise from the timeout tests and did not reproduce on two subsequent full runs; recorded rather than attributed. AC7's evidence: `devtools::document()` no diff, `devtools::check()` Status OK 0/0/0, and `Rscript data-raw/imports-floors.R --only digest` pinned at `digest 0.6.37` reporting `pinned pass=11219 fail=0 err=0 skip=18` against an identical baseline — no floor moved, and the pinned run skipped no more than the baseline.

- 2026-09-02: status `review`. CI on `b624f12` is green on all ten checks — the eight build legs, `windows-latest (release)` among them, plus `codecov/patch` and `codecov/project`, both of which the previous head failed on the three uncovered branches. `devtools::check()` re-run against the final tree: Status OK, 0 errors / 0 warnings / 0 notes. AC4 and AC7 ticked on the evidence above; every criterion is now ticked.

## Decisions

- 2026-09-02: `archive_checksum` takes `NULL` as its default. D079 forbids a
  default that is one member of the set the argument ranges over; `NULL` here
  is the package's absence sentinel (D016, D022) meaning "no digest was
  supplied", not a digest, so D079's antecedent is not met — the same reading
  under which `download_url = NULL` and `install_dir = NULL` already stand on
  this function. D080's safe-position rule is not traded: the safe position
  would be a real digest, and the package cannot know a caller-supplied URL's
  digest, so the unverified path pays for it with a message rather than a
  silent skip. Recorded here rather than promoted because it applies existing
  entries rather than deciding anything new. Falsified by D079 being read at a
  later gate to cover absence sentinels, which would put every optional
  argument on this function outside it too.

## Review

Reviewed 2026-09-02 on branch `m102-install-verify-before-register`, PR #106,
against `origin/master` at 9ae0bfe's merge base (master had not moved since the
branch was cut, so no merge was needed). Suite run fresh:
`devtools::test()` — FAIL 0, WARN 10, SKIP 18, PASS 11547. The 10 warnings are
pre-existing, all in `test-audio-stream*.R` and `test-ffmpeg.R`; none is on a
file this milestone touched.

### Acceptance criteria

- AC1 — verified. `test-program-management.R:563` runs the default source
  through all three sidecar body shapes (bare, `sha256sum`, `SHA256(file)=`,
  the last upper-cased) and asserts for each that exactly one sidecar fetch
  happened, at `<archive url>.sha256`, and that the extraction step was
  reached. `:592` asserts the prompt names the sidecar URL. `:626` drives the
  three unreadable routes — an HTML 404 body, a signalling fetch, a non-zero
  status — each aborting `tidymedia_checksum_unavailable`, with the archive
  download never attempted and the redirected config root left empty. `:670`
  asserts a mismatch aborts `tidymedia_checksum_mismatch`, that the message
  carries both the published and the computed digest, and that a before/after
  snapshot of `tm_config_dir()` is identical.
- AC2 — verified. `:693` runs a caller-supplied digest in both cases, asserts
  no sidecar was fetched on the default source, and asserts a wrong supplied
  digest still aborts `tidymedia_checksum_mismatch` with nothing registered.
  `:592` covers the two no-fetch paths' prompts and the one message on the
  unverified caller-named path (observed live in the targeted run). `:720`
  drives all four rejected shapes — 63 chars, a trailing `z`, `NA_character_`,
  length 2 — with `utils::download.file()` mocked to abort if reached; each is
  an `rlang_error` naming `archive_checksum` and carrying no `tidymedia_*`
  class.
- AC3 — **FAILED.** Verified on macOS (`test-program-management.R:758`: both
  fixtures abort `tidymedia_archive_unreadable`, the message names the archive
  path and the install directory, the whole `$parent` chain is free of
  `archive_extract.cpp`, and `file.exists(destfile)` is FALSE on both failing
  paths and on the succeeding control) — but red on Windows CI, which is the
  only platform `install_on_win()` runs on. `windows-latest (release)` on
  9ae0bfe:
  `Failure ('test-program-management.R:796:5') ... Expected corrupt-payload.7z
  to be FALSE`, i.e. `expect_false(file.exists(rec$destfile))`. AC3's last
  clause — "the downloaded temporary file is removed on the failing path as on
  the succeeding one" — does not hold on Windows for the corrupt-payload
  fixture. `not-an-archive.7z` passes, which points at the difference between
  libarchive's two failure routes: the payload fixture fails inside
  `archive_read_data_block()`, after the file was opened, so a handle is
  plausibly still open when `on.exit(unlink(tf))` runs and Windows refuses to
  delete an open file. That is a hypothesis, not a measurement — the
  measurement is the assertion above.

- AC4 — verified. `:806` drives four builds. All three unpacked registers
  `ffmpeg`, `ffprobe`, `ffplay` and returns `TRUE`; `ffmpeg` + `ffprobe` only
  registers those two, emits a message naming `ffplay`, returns `TRUE`; each
  of the two missing-required builds aborts `tidymedia_program_not_extracted`
  naming the absent program with `rec$set` empty.
- AC5 — verified. `test-program-management.R:~845` covers both signalling
  shapes: `download.file()` raising (asserted `tidymedia_download_unavailable`,
  URL in the message, `$parent` a condition carrying base R's
  `cannot open URL`) and returning `1L` (same class), with nothing registered
  in either case.
- AC6 — verified. `:939` collects every `return()` and `cli_abort()` node from
  `body(install_on_win)` — seeing through `pkg::fn` — and asserts of each that
  it hands back a literal `TRUE`/`FALSE` or passes a `class =` beginning
  `tidymedia_`. The anti-vacuity floor asserts the collected classes contain
  all five AC1/AC3/AC4/AC5 name. `:968` keeps the discrimination re-runnable:
  a planted unclassed `cli_abort()` and a planted `return(x)` each read FALSE,
  a compliant control reads TRUE, and each is asserted to have a non-empty
  collected domain. `@return` in `man/install_on_win.Rd` names all five
  aborting outcomes plus the declined confirmation.

### Consistency gate

- `cairn_validate.py`: all 16 checks PASS, exit 0; the advisories are clean.
- No `DESIGN.md` principle changed, so `cairn_impact.py` was not run.
- Profile (`r-package`) toolchain checks: `devtools::document()` produces no
  diff; `NAMESPACE`, `man/` and `data/*.rda` are unedited by hand;
  `README.md` rebuilt from `README.Rmd` and committed; `pkgdown::check_pkgdown()`
  reports no problems; `NEWS.md` carries three entries for this milestone's
  user-visible changes, none naming a milestone number; no new top-level file
  was added, so no `.Rbuildignore` entry was needed;
  `devtools::check()` — recorded under AC7.

### Independent review

Full three-lens fan-out (the diff touches `R/`, `tests/` and `data-raw/`, so the
docs-only single-lens route does not apply). Findings and disposition, verbatim
where reported; nothing dropped.

**[O] diff-bug lens — 15 findings.**

1. *"`readLines(sidecar_file, warn = FALSE)` is unguarded — a real hole in the
   milestone Goal."* A `download.file()` returning status 0 without leaving a
   readable file sends the read into a bare `cannot open connection`, unclassed
   and invisible to AC6's census. **Fixed at the gate:** the read is wrapped the
   way `tm_fetch()`/`tm_unpack()` wrap theirs, `tm_parse_sidecar()` takes `NULL`,
   and a new test drives a status-0 fetch that delivers nothing. Proven red
   without the guard (`cannot open the connection`).
2. *"AC4's config-file claims are never asserted on disk."* Every AC4 build
   mocks `set_program()`, so the criterion's claim about config FILES was
   verified against recorded calls. **Fixed at the gate:** `tm_mock_install()`
   gained `real_set =`, which leaves the real `set_program()` in place (and
   makes the extracted stubs executable so `Sys.which()` accepts them); a new
   test reads the three written files back, asserts no `ffplay` file on the
   optional-absent build, and snapshots the config root across both
   required-missing builds.
3. *"A failed digest fetch leaves a newly created empty install directory
   behind."* **Rejected, with reason:** `dir.create()` sits above the fetch in
   `master` too, so the diff did not introduce it, and moving it below would
   trade fail-fast on an unwritable `install_dir` for a several-hundred-megabyte
   download before the same refusal. Filed as a candidate row instead.
4. *"Nothing pins the digest to a known-answer SHA-256."* Both sides of every
   comparison come from `tm_archive_digest()`. **Fixed at the gate:** a test
   pins `"abc"` to its published SHA-256.
5. *"AC1's 'case-insensitively' is only partly implemented for the OpenSSL
   shape."* The `SHA256` tag was matched case-sensitively. **Fixed at the
   gate** (`[Ss][Hh][Aa]256`). Not read as an AC1 failure: the criterion writes
   the tag in that case and the hex, which is what varies in the wild, was
   already case-insensitive — so this is a widening, not a repair.
6. *"Four pre-M102 tests now leak an uncaptured `will not be verified`
   message."* **Fixed at the gate:** the four calls are wrapped in
   `suppressMessages()` with a comment pointing at the tests that do assert the
   message.
7. *"The AC1 three-shapes test's ordering comment overclaims."* **Fixed at the
   gate:** the comment now says where the ordering IS asserted.
8. *"A 109-character comment line, from an unre-wrapped edit."* **Fixed at the
   gate**, along with the two other new lines over 80; every remaining long line
   in the file predates the branch.
9. *"The `tidymedia_archive_unreadable` message names a file `on.exit()` has
   already deleted."* **Rejected:** AC3 requires the message name the archive
   path, and it is the path the failure was about.
10. *"`tm_unpack()` leaves partial extraction debris."* **Deferred to a
    candidate row** — no AC covers it and the reviewer filed it as an
    observation.
11. *"Dead term in the optional-program computation."* **Fixed at the gate:**
    `setdiff(tm_install_registers, unpacked)`.
12. *"`tm_parse_sidecar()` takes the first digest on any line."* **Deferred to a
    candidate row** — out of scope for gyan.dev's single-digest sidecar.
13. *"AC6's `@return` clause has no test."* **Rejected:** the criterion asks
    that `@return` name five outcomes; that was verified by reading
    `man/install_on_win.Rd`, and it does (a superset).
14. *"Minor style/hygiene."* The stray double blank line in `R/utils.R` is
    **fixed**; `check_sha256()`'s `allow_null &&` is **rejected** — it is
    verbatim the `check_token()` idiom two functions above it; the `class`
    binding and the probe-then-remock pattern are test-local and **rejected**.
15. *"Uppercase hex is exercised on only one of the three sidecar shapes."*
    **Rejected** — the reviewer's own note: coverage, not a defect.

**[S] blame-history lens — 2 findings.**

1. *"The consent prompt now can promise a write that never happens"* — M101's
   property was that the prompt names every fetch AND write the call makes, and
   `ffplay`'s write is now conditional while the prompt still names all three.
   **Partly fixed at the gate.** The prompt itself is left alone: which programs
   the archive contains is not knowable before it is unpacked, so naming all
   three and saying afterwards which was skipped is the honest forecast. What
   was wrong was the wording around it — `tm_install_details()`'s docblock now
   states the may-write reading explicitly instead of dropping M101's clause,
   `NEWS.md`'s M101 entry says "the remembered program locations it may
   overwrite", and `README.Rmd`/`README.md` say "may overwrite".
2. *"Stale documentation"* — the same root cause; **fixed** by the same edit.

**[S] prior-review-record lens — 1 finding, 1 informational.**

1. *"Tests re-derive an expected value using the same construction logic the
   code under test uses, echoing M101's P1 finding."* **Rejected, with reason:**
   the expected sidecar URL is built from `rec$download[[1]]$url` — the
   recorder's observation of the archive URL — not from `tm_sidecar_url()`, so
   the two sides do not share a helper; hard-coding the full URL instead would
   re-pin what M097 F1 already moved off hand-lists.
2. The `tm_archive_digest()` observation was the reviewer's own non-finding;
   it is answered anyway by [O]4's known-answer test.

The lens also reports `gh api .../pulls/comments` returning `[]` — this repo has
no inline PR review threads, so the secondary surface contributed nothing, as in
M091 and M097.

**Return floor.** No actioned finding demonstrates an acceptance criterion
failing, and none is a load-bearing defect in what `install_on_win()` does for
its callers: [O]1 is a hole against the Goal's wording rather than any
criterion's, and it is closed. Status stays `review`.

### Second pass — 2026-09-02, at `fc5c2df`

Re-entered under the session-start resume route: PR #106 open, no recorded
step-7 approval. `origin/master` had still not moved (0 commits ahead of the
branch), so no merge was needed. Suite run fresh at `fc5c2df`:
`devtools::test()` — FAIL 0, WARN 10, SKIP 18, PASS 11570; the 10 warnings are
the same pre-existing ones, none on a touched file. That run re-covers the AC1,
AC2, AC5 and AC6 tests whose first-pass evidence was recorded at `9ae0bfe`.

- AC3 — verified, ticked. The Windows failure the first pass recorded is fixed
  by T9 and verified where it lived: `gh api repos/jmgirard/tidymedia/commits/
  59dc788/check-runs` reports all ten checks `success`, `windows-latest
  (release)` included — the leg red at `9ae0bfe` on
  `expect_false(file.exists(rec$destfile))` for `corrupt-payload.7z`. `fc5c2df`
  is docs-only on top of that commit. Locally the AC3 test also asserts the
  mechanism directly (`showConnections()` before/after on both fixtures and the
  succeeding control), which is what makes the fix visible off Windows.
- AC4 — **FAILED**, unticked. See [O]2 below: `unpacked` is computed by
  `file.exists()` over `<install_dir>/bin/*.exe` after extraction
  (`R/program_management.R:649`), not from what the extraction produced, and
  `archive_extract()` does not clear the directory. `install_dir` defaults to
  the stable `tm_install_dir()`, so on any re-install the previous run's
  binaries satisfy the check. Reproduced by the reviewer with `bin/`
  pre-seeded with `ffmpeg.exe` and `ffplay.exe` and the extraction mock
  producing only `ffprobe.exe`: the call returned `TRUE` and registered all
  three. AC4's first clause — "No remembered location is written for a program
  the extraction did not produce" — is false in the ordinary repeat-install
  case. Confirmed against the implementation at `R/program_management.R:
  649-652` and the `install_dir = NULL` default at `:514`, `:525`.
- AC7 — unticked. It carried a tick with no evidence line in this section,
  which AC fencing treats as unverified; the AC4 fix changes the registration
  path, so `devtools::check()` and the `--only digest` floor leg have to be
  re-run against the corrected tree regardless.

### Consistency gate — second pass

- `cairn_validate.py`: **FAIL**, exit 1. `weight caps (1)` —
  `cairn/milestones/M102-install-verify-before-register.md: 153 plan-owned
  lines (cap <150; shed >=4)`; heaviest first: Acceptance criteria 68 · Tasks
  37 · Scope 20 · Coverage 10 · Goal 8. T9's addition after the first pass
  (which recorded all 16 checks passing) is what crossed the line. The remedy —
  compressing the single heaviest plan-owned section in one rewrite — is
  plan-owned and amend-via-gate, so it cannot be done review-side; it returns
  to `/milestone-implement` step 6 with the AC4 fix.
- The other 15 checks PASS; advisories clean. No `DESIGN.md` principle changed,
  so `cairn_impact.py` was not run.
- Profile toolchain checks not re-run past `devtools::test()`: the gate had
  already failed and the code is about to change.

### Independent review — second pass

Full three-lens fan-out over `git diff origin/master..HEAD`. The first pass ran
at `9ae0bfe`; `59dc788` (T9) is code no lens had seen. Findings verbatim where
reported; nothing dropped.

**[O] diff-bug lens — 7 findings.**

1. *"`tm_archive_digest(tf)` is unguarded — an unclassed `simpleError` escapes
   `install_on_win()`"* (`R/program_management.R:622`). `digest::digest(path,
   file = TRUE)` aborts with a bare `simpleError` when the file is not there,
   and `tm_fetch()` returns `TRUE` on `download.file()` status 0, which is not
   a promise a readable file arrived — the same premise the first pass accepted
   when it closed the identical hole on the sidecar `readLines()`. Reproduced
   with `download.file` mocked to return `0L` and write nothing: classes
   `simpleError, error, condition`. Falsifies the Goal's "every failure ...
   returns `FALSE` or carries a `tidymedia_*` class"; invisible to AC6's census
   because it is neither a `return()` nor a `cli_abort()` node. **Actioned —
   returned with the AC4 fix**, same shape as the first pass's [O]1.
2. *"Registration reads the disk, not the extraction — a previous install's
   leftovers count as 'produced'"* — see AC4 above. **Return-floor finding.**
   The reviewer notes the information is already free: `archive_extract()`
   returns the extracted file list and `tm_unpack()` discards it
   (`R/program_management.R:396-399`); returning it and intersecting with
   `tm_install_registers` makes the implementation match AC4's wording.
3. *"The 'will not be verified' notice comes after the consent prompt"*
   (`:562-567`) — the user consents before being told this install verifies
   nothing. **Actioned as a follow-up question for the fix pass**; no AC covers
   it (AC2 asks only for "one message"), so it is not a floor finding.
4. *"Test-side: `install_dir` is reused across loop iterations, weakening
   AC1/AC2 evidence."* The three-sidecar-shapes and case-insensitive-digest
   tests bind `d` once outside the loop; combined with [O]2 the on-disk half of
   later iterations is contaminated. **Actioned — returned**: one
   `local_tempdir()` per iteration.
5. *"`tm_collect_exits()`/`tm_exit_ok()` are brittle on two node shapes."* A
   bare `return()` would throw *subscript out of bounds* rather than fail the
   assertion, and `class = c("tidymedia_x", "y")` would read as non-compliant.
   Test-only, neither shape exists today. **Actioned — returned** as a small
   hardening with the AC6 test.
6. *"`readLines()`'s warning handler turns any warning into
   `tidymedia_checksum_unavailable`."* `warn = FALSE` already covers the
   incomplete-final-line case, so what remains (embedded nul, encoding)
   converts a partly-read body into a refusal. **Rejected:** the net is wider
   than the comment describes but errs safe, and AC1 requires exactly this
   class on an unreadable body.
7. *"`check_sha256()` has no direct unit test"* (`R/utils.R:227-259`); the
   `allow_null = FALSE` branch is unreached. **Rejected:** the four-shape
   front-door test covers the criterion, and the repo tests helpers indirectly
   by doctrine.

The lens also reports trying and failing to break T9: `archive_extract()`'s
path form had the identical leak, the connection form is the same read
mechanism, and after a successful extraction the connection is destroyed, which
`tm_close()`'s `tryCatch` handles. No defect found in T9.

**[S] blame-history lens — 0 findings.** Every change traces to a recorded
decision or a prior finding; the `cli_warn()`/`return(FALSE)` → classed
`cli_abort()` move is documented in `NEWS.md` and `@return` and follows
`DESIGN.md`'s own convention; the conditional registration fixes a latent bug
on `master` rather than removing intended behavior; D080/D081 are cited
accurately; T9 repairs the AC3 regression without resurrecting anything; and
`tm_unpack()` has no other call site.

**[S] prior-review-record lens — 1 finding.**

1. *"The 'will overwrite' → 'may overwrite' fix from the first pass's [S]1 was
   applied inconsistently."* `install_on_win()`'s own `@param confirm` block
   (`R/program_management.R:483-488`) still reads "the remembered program
   locations it **will** overwrite" — the exact wording that pass called wrong
   — and it is regenerated into `man/install_on_win.Rd:24-27`, which this diff
   touches. The shipped help page therefore contradicts the corrected
   `tm_install_details()` docblock a few lines below it. **Actioned —
   returned.** No archived `## Review` section touches these files, and
   `gh api .../pulls/comments` returned `[]` again, so the live first-pass
   record was the only prior-review surface.

**Return floor.** Met. [O]2 demonstrates AC4 failing inside its own domain, on
the ordinary repeat-install path, and the repair is a procedure over that
domain (use the extraction's file list) rather than a widening of an
author-recalled enumeration — so this is a defect return, not an amendment
return. Second defect return on M102; the thrash rule's third-return threshold
is not reached.
