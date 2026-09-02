# M102: `install_on_win()` verifies the archive and the unpacked programs before it registers anything

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Branch/PR:** —

## Goal

`install_on_win()` verifies the archive it downloaded against the digest its
source publishes and confirms the programs the package needs were unpacked
before it writes any remembered location, and every way the call can fail
returns `FALSE` or carries a `tidymedia_*` condition class, for every failure
on that path after the front-door argument checks.

## Scope

Surface tier: **user-facing** — the deliverable is an exported function's
failure behavior and its documented return contract.

**In:** SHA-256 verification of the downloaded archive, against
`<download_url>.sha256` for the package's own default source and against a
caller-supplied `sha256 =` for any source; `digest` added to `Imports`; classed
refusals for the download, checksum, extraction and missing-program failures,
which today escape as base R and libarchive text; registration gated on the
files the extraction actually produced; the consent prompt extended to name the
sidecar fetch; roxygen, `NEWS.md`, `README.Rmd`.

**Out:** Authenticity against a compromised source — the digest rides the same
host and TLS session as the archive, so this milestone buys integrity against
corruption and truncation, not provenance; a digest pinned in the package would
reach it and goes stale on every upstream refresh → candidate row.
`set_program()`'s own unconfirmed config write → its existing candidate row.
Any change to the consent gate beyond naming the second fetch → M101 shipped it.
Classing aborts that originate inside `set_program()` rather than inside
`install_on_win()` → the existing unclassed-aborts candidate row.

## Acceptance criteria

- [ ] AC1: With `download_url` at its default, `install_on_win()` fetches
      `<download_url>.sha256` before it unpacks, and the consent prompt names
      that fetch alongside the archive download. It accepts a sidecar body that
      is a bare 64-hex digest, a `sha256sum` two-field line, or a
      `SHA256(<file>)= <hex>` line, matching case-insensitively, and a test
      exercises all three. It aborts with class
      `tidymedia_checksum_unavailable` where the body matches none of the
      three, and for both shapes of a failed fetch — `utils::download.file()`
      signalling its own error, and returning a non-zero status. Where the fetched digest differs from the
      SHA-256 of the downloaded file it aborts with class
      `tidymedia_checksum_mismatch` naming both digests, and a test comparing
      the contents of `tm_config_dir()` before and after the call asserts they
      are unchanged. The matching-digest control reaches the extraction step.
- [ ] AC2: `install_on_win(sha256 = <64-hex string>)` verifies the download
      against that digest, case-insensitively, and fetches no sidecar.
      `sha256` defaults to `NULL`; where `download_url` is anything other than
      the package's own default and `sha256` is `NULL`, the call fetches no
      sidecar, verifies nothing, and emits one message saying the archive was
      not verified. A value that is not a single 64-character hex string is
      refused at the front door with the argument named, asserted over a
      wrong-length, a non-hex, an `NA` and a length-2 value; like the other
      front-door argument checks that refusal carries no `tidymedia_*` class.
      The sidecar fetch is mocked so an attempted fetch is
      observable in both directions. (RB tripwire: irreversible-api)
- [ ] AC3: A failure inside `archive::archive_extract()` aborts with class
      `tidymedia_archive_unreadable` naming the archive path and the install
      directory, and neither the top-level condition message nor any `parent`
      condition it carries contains `archive_extract.cpp` — libarchive's text
      is not retained, the abort's own two facts replacing it. Tests cover both of
      libarchive's failure shapes — a body it does not recognize as an archive
      at all, and a well-formed 7z header followed by a corrupt payload — from
      committed fixtures whose `data-raw/` generator carries their provenance,
      and assert the downloaded temporary file is removed on the failing path
      as well as on the succeeding one.
- [ ] AC4: No remembered location is written for a program the extraction did
      not produce; `ffmpeg` and `ffprobe` are required and `ffplay` is
      optional. A run whose extraction yields all three writes three config
      files and returns `TRUE`; one yielding `ffmpeg` and `ffprobe` only writes
      those two, leaves no `ffplay` config file, emits one message naming
      `ffplay`, and returns `TRUE`; one yielding no `ffmpeg`, and one yielding
      no `ffprobe`, each write no config file at all and abort with class
      `tidymedia_program_not_extracted` naming the absent program.
- [ ] AC5: A `utils::download.file()` call that does not deliver the archive
      aborts with class `tidymedia_download_unavailable` naming the URL, for
      both shapes its signalling contract allows: signalling its own error, and
      returning a non-zero status. The base condition
      is retained as the abort's `parent`, and the tests assert the class,
      never the message text.
- [ ] AC6: Every `return()` call and every `cli::cli_abort()` call in
      `install_on_win()`'s own body returns `TRUE`/`FALSE` or passes a
      `class =` beginning `tidymedia_`. That domain is derived, not listed: a
      test walks the function's `body()` and collects those two node types,
      asserting of each collected member that it has the property. The claim is
      exactly the collected set — the exits the walk cannot reach are named
      rather than claimed: the front-door `rlang` argument checks, and aborts
      originating inside `tm_confirm()` and `set_program()`. As a floor against
      a collector that silently under-reads the body, the test asserts the
      collected set holds at least the five abort sites AC1, AC3, AC4 and AC5
      name. Separately, `@return` documents five outcomes by name: a declined
      confirmation, a download that did not deliver, a checksum that did not
      match, an archive that could not be unpacked, and a required program that
      was not produced.
- [ ] AC7: `digest` appears in `DESCRIPTION` `Imports` with a floor version
      the `--only digest` leg of `data-raw/imports-floors.R` passes at,
      `NEWS.md` and `README.Rmd`'s installer paragraph name the verification
      and the new argument, and the profile's checks are clean:
      `devtools::document()` produces no diff, `devtools::test()` passes,
      `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T2, T3, T6
- AC2 → T2, T6
- AC3 → T4, T6
- AC4 → T5, T6
- AC5 → T4, T6
- AC6 → T7, T8
- AC7 → T1, T8

## Tasks

- [ ] T1: Add `digest` to `DESCRIPTION` `Imports`; measure the floor with
      `Rscript data-raw/imports-floors.R --only digest`; append D081.
- [ ] T2: Add the sidecar-URL, sidecar-parse and archive-digest helpers; add
      `sha256 = NULL` last in `install_on_win()`'s signature with its
      front-door shape check; wire verification between the download and the
      extraction (`R/program_management.R:410-424`).
      (RB tripwire: irreversible-api)
- [ ] T3: Extend `tm_install_details()` so the prompt names the sidecar fetch
      alongside the archive download, keeping M101's property that the prompt
      names every fetch and write the call makes.
- [ ] T4: Wrap `utils::download.file()` in a classed refusal retaining the
      base condition as `parent`, and `archive::archive_extract()` in one that
      does not retain it — libarchive's text is what AC3 excludes; move the
      temp-file cleanup to `on.exit()` so both paths clean up.
- [ ] T5: Replace the unconditional `set_program()` loop
      (`R/program_management.R:427-429`) with a per-program existence check.
- [ ] T6: Commit the two corrupt-archive fixtures and their `data-raw/`
      generator; write the tests for AC1–AC5.
- [ ] T7: Write the derived-exit test for AC6; prove it can fail by planting,
      one at a time, an unclassed `cli::cli_abort()` and a `return()` of a
      non-literal, and seeing each red.
- [ ] T8: Roxygen, `NEWS.md`, `README.Rmd`; then `devtools::document()`,
      `devtools::test()`, `devtools::check()`.

## Work log

- 2026-09-02: created by /milestone-plan.
- 2026-09-02: criteria audit ran in FULL mode (user-facing tier), fresh-context [O] reader; returned findings on all six drafted criteria — wrong premise inherited from the ROADMAP row (`set_program()` already refuses a missing binary, so the defect is partial registration, not dead locations), two class names naming a category and a severity, an unbounded "six failure branches" hand-list, single-exemplar probes on extraction failure and on registration, an unspecified sidecar parse, a collision with M101's prompt property, and a goal left false by the unclassed `cannot open URL` path. All fixed at the gate; none deferred.
- 2026-09-02: plan gate chose fetching the source's published sidecar digest over pinning a digest in the package because a pinned digest goes stale on every upstream refresh and would refuse a good archive; falsified by an upstream that stops publishing a sidecar, or by a report of a substituted archive passing the sidecar check.
- 2026-09-02: plan gate chose `digest` over `tools::sha256sum()` and over `openssl` because `tools::sha256sum()` first ships in R 4.5.0 and would raise the measured 4.1.0 floor four releases, while `openssl` links a system SSL library; measured 2026-09-02, `digest` declares `R (>= 3.3.0)` and its `algo = "sha256"` matches `tools::sha256sum()` byte for byte. Falsified by the R floor rising to 4.5.0 for an unrelated reason, which makes the base function free.
- 2026-09-02: plan gate chose adding an optional `sha256 =` argument over verifying only the default source because a caller-supplied URL otherwise has no verification path at all; falsified by no outside caller of `install_on_win()` appearing before 0.2.0, which would say the added public argument bought nothing.
- 2026-09-02: plan gate chose requiring `ffmpeg` and `ffprobe` while treating `ffplay` as optional over all-three-required and over register-whatever-unpacked, because nothing in the package calls `ffplay` — it is reachable only through `find_ffplay()`/`set_ffplay()` — while an unpack with no `ffmpeg` must not report success; falsified by a package verb taking a dependency on `ffplay`.

- 2026-09-02: the audit's disposal check returned four repairs that traded one finding for another and three new ones — AC6's derived census still overclaimed (a two-node-type walk cannot see `rlang::check_bool()`'s abort, so the goal stayed false), AC7 had demoted the floor measurement out of the criterion and back into a task, AC3's no-`archive_extract.cpp` rule contradicted T4's parent retention, and AC5's headline was broader than its two shapes. All applied; AC4's non-executable probe dropped as unreachable on Windows.

## Decisions

## Review
