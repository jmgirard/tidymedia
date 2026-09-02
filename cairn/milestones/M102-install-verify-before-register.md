# M102: `install_on_win()` verifies the archive and the unpacked programs before it registers anything

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Branch/PR:** `m102-install-verify-before-register`

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

- [ ] AC1: With `download_url` at its default and `archive_checksum` at `NULL`,
      `install_on_win()` fetches `<download_url>.sha256` before it unpacks, and
      the consent prompt names that fetch beside the archive download. It reads
      a sidecar body that is a bare 64-hex digest, a `sha256sum` two-field line,
      or a `SHA256(<file>)= <hex>` line, case-insensitively, and aborts with
      class `tidymedia_checksum_unavailable` on a body matching none of those
      and on either shape of a failed fetch — `utils::download.file()`
      signalling its own error, or returning a non-zero status. A fetched digest
      differing from the downloaded file's SHA-256 aborts with class
      `tidymedia_checksum_mismatch` naming both digests and leaves
      `tm_config_dir()`'s contents unchanged; a matching one reaches the
      extraction step.
- [ ] AC2: `install_on_win(archive_checksum = <64-hex string>)` verifies the
      download against that digest, case-insensitively, and fetches no sidecar
      on any source, the package's own default included; the consent prompt
      names a sidecar fetch only where one happens, and a mismatch aborts with
      the `tidymedia_checksum_mismatch` class AC1 names. `archive_checksum`
      defaults to `NULL`; where the resolved `download_url` is any string but
      the package's own default URL and `archive_checksum` is `NULL`, the call
      fetches no sidecar, verifies nothing, and emits one message saying the
      archive was not verified. A value that is not a single 64-character hex
      string is refused at the front door with the argument named — asserted
      over a wrong-length, a non-hex, an `NA` and a length-2 value — and like
      the other front-door checks that refusal carries no `tidymedia_*` class.
      (RB tripwire: irreversible-api)
- [ ] AC3: A failure inside `archive::archive_extract()` aborts with class
      `tidymedia_archive_unreadable` naming the archive path and the install
      directory, and neither that condition's message nor any `parent` it
      carries contains `archive_extract.cpp` — libarchive's text is replaced by
      the abort's own two facts, not retained. Both of libarchive's failure
      shapes are covered — a body it does not recognize as an archive at all,
      and a well-formed 7z header over a corrupt payload — and the downloaded
      temporary file is removed on the failing path as on the succeeding one.
- [ ] AC4: No remembered location is written for a program the extraction did
      not produce; `ffmpeg` and `ffprobe` are required, `ffplay` optional. An
      extraction yielding all three writes three config files and returns
      `TRUE`; one yielding `ffmpeg` and `ffprobe` only writes those two, leaves
      no `ffplay` config file, emits one message naming `ffplay`, and returns
      `TRUE`; one yielding no `ffmpeg`, and one yielding no `ffprobe`, each
      write no config file at all and abort with class
      `tidymedia_program_not_extracted` naming the absent program.
- [ ] AC5: A `utils::download.file()` call that does not deliver the archive
      aborts with class `tidymedia_download_unavailable` naming the URL, for
      both shapes its signalling contract allows — its own error, and a non-zero
      return status — retaining the base condition as the abort's `parent`.
- [ ] AC6: Every `return()` and every `cli::cli_abort()` call in
      `install_on_win()`'s own body returns `TRUE`/`FALSE` or passes a `class =`
      beginning `tidymedia_`. The domain is derived, not listed: a test walks
      the function's `body()`, collects those two node types, and asserts the
      property of each; the claim is exactly that set, which — as a floor
      against a collector that under-reads — holds at least the five abort sites
      AC1, AC3, AC4 and AC5 name. Exits the walk cannot reach are named, not
      claimed: the front-door `rlang` checks, the `archive_checksum` shape
      check, and aborts inside `tm_confirm()` and `set_program()`; T2 sites
      every classed abort those four criteria name in the body itself, its
      helpers returning values instead. Separately, `@return` names five
      outcomes: a declined confirmation, a download that did not deliver, a
      checksum that did not match, an archive that could not be unpacked, and a
      required program that was not produced.
- [ ] AC7: `digest` appears in `DESCRIPTION` `Imports` with a floor version the
      `--only digest` leg of `data-raw/imports-floors.R` passes at, `NEWS.md`
      and `README.Rmd`'s installer paragraph name the verification and the new
      argument, and the profile's checks are clean: `devtools::document()`
      produces no diff, `devtools::test()` passes, `devtools::check()` reports 0
      errors and 0 warnings.

## Coverage

- AC1 → T2, T3, T6
- AC2 → T2, T6
- AC3 → T4, T6
- AC4 → T5, T6
- AC5 → T4, T6
- AC6 → T7, T8
- AC7 → T1, T8

## Tasks

- [x] T1: Add `digest` to `DESCRIPTION` `Imports` (D081 is already appended).
      The floor measurement moves to T8, where the package actually calls
      `digest` and the run has something to exercise.
- [x] T2: Add the sidecar-URL, sidecar-parse and archive-digest helpers, each
      returning a value rather than aborting so AC6's census holds; add the
      `archive_checksum` shape check as a `check_*()` helper in `R/utils.R`; add
      `archive_checksum = NULL` last in the signature; wire the sidecar fetch
      above the archive download and the comparison between download and
      extraction (`R/program_management.R:410-424`). (RB tripwire:
      irreversible-api)
- [x] T3: Extend `tm_install_details()` so the prompt names the sidecar fetch
      beside the archive download, keeping M101's property that the prompt names
      every fetch and write the call makes.
- [x] T4: Wrap `utils::download.file()` in a classed refusal retaining the base
      condition as `parent`, and `archive::archive_extract()` in one that does
      not — libarchive's text is what AC3 excludes; move temp-file cleanup to
      `on.exit()`.
- [x] T5: Replace the unconditional `set_program()` loop
      (`R/program_management.R:427-429`) with a per-program existence check.
- [x] T6: Commit the two corrupt-archive fixtures and their `data-raw/`
      generator, which carries their provenance; write the AC1–AC5 tests. Mock
      the sidecar fetch so a fetch and its absence are told apart, on AC2's two
      no-fetch paths as on AC1's fetching one; exercise all three sidecar body
      shapes; assert failures by class, never by message text.
- [x] T7: Write the derived-exit test for AC6; prove it can fail by planting,
      one at a time, an unclassed `cli::cli_abort()` and a `return()` of a
      non-literal, and seeing each red.
- [ ] T8: Roxygen, `NEWS.md`, `README.Rmd`; measure the declared `digest` floor
      with `Rscript data-raw/imports-floors.R --only digest`; then
      `devtools::document()`, `devtools::test()`, `devtools::check()`.

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
