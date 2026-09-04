# Changelog

## tidymedia (development version)

### Requirements

- tidymedia now imports `digest`, which is what computes the SHA-256 of
  a downloaded FFmpeg archive. Base R gained
  [`tools::sha256sum()`](https://rdrr.io/r/tools/sha256sum.html) in
  4.5.0, four releases above the `R (>= 4.1.0)` this package declares;
  taking a small, pure-C dependency rather than asking everyone below
  4.5.0 to give up the package was the trade made.

- The dependency versions tidymedia declares are now measured rather
  than assumed: the package’s test suite has been run against the exact
  version of each package `Imports` names. One of them was wrong.
  `rlang` is now `(>= 1.2.0)`, up from `1.1.0`: tidymedia checks its
  arguments with
  [`rlang::check_string()`](https://rlang.r-lib.org/reference/check_type_scalar.html),
  `check_bool()` and their siblings in 132 places, and rlang first
  exports those functions in 1.2.0 — so on an earlier rlang the
  package’s verbs failed at their own front doors. The other eight
  declared floors were exercised at the version they name and stand
  unchanged.

- tidymedia now declares the external tools it interfaces. `DESCRIPTION`
  names FFmpeg and MediaInfo in `SystemRequirements`, each with its
  project URL, so the tools the package shells out to are visible to
  anyone reading the package’s metadata rather than only to someone who
  runs it and gets an error. Nothing about how the package finds those
  tools has changed.

- tidymedia now states the R version it needs: `R (>= 4.1.0)`. An
  installer on an older R refuses the package and says why, instead of
  installing something whose help-page examples will not run. That
  version is what those examples require — they use the native pipe
  `|>`, a form R gained in 4.1.0 — and it sits above the highest R
  version any declared dependency floor asks for.

### Configuration

- [`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
  now decides what an archive produced by looking at the install
  directory as well as at the archive’s own file list. A path the
  archive listed and did not leave behind – an unpacked program an
  antivirus quarantined between the extraction and the check, which is
  how this happens – used to be refused as a program that “cannot be
  used”, which is not true of a path with no file on it, and the same
  refusal could tell you the unpacked files were still in a directory
  that held none of them. A required program that is not at its path now
  raises `tidymedia_program_not_extracted` rather than
  `tidymedia_program_unusable`; the error says the extraction reported
  writing that file and it is not there; and an install directory the
  call created and then found empty is removed again.

- [`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
  now registers every program the archive produced, or none of them. It
  used to register them one program at a time, so a build it could not
  use was registered in pieces: a truncated `ffprobe.exe` was remembered
  as a working program, and a build missing `ffprobe.exe` altogether
  registered `ffmpeg` and then failed — overwriting, in both cases,
  whatever location an earlier install had left. The install now looks
  at every produced program before it writes anything: where a required
  one cannot be used, the call refuses without changing a single
  remembered location and names each failed program and its full path;
  where an optional one cannot be used, the install completes and tells
  you which program it skipped and why. The check does not run the
  programs, so a build that unpacks and then cannot execute — the wrong
  architecture, say — still gets registered.

- A refused
  [`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
  now leaves the install directory as it found it. Files a failed
  extraction wrote are removed and a directory the call created is
  removed again, so a refusal no longer leaves debris or an empty
  directory behind. What was already there is kept — including inside a
  directory the extraction wrote into — with one exception: a file of
  yours that the failed extraction wrote over is removed along with the
  debris, because what it holds afterwards is nothing you put there —
  and the error names that file, so a refusal never reports the
  directory as untouched when it took something of yours out of it.
  Removal is best-effort, and on Windows a partly-written file is one it
  cannot make: the extraction library is still holding that file open,
  and Windows will not delete a file something holds. So on Windows the
  error names the leftovers by full path instead of removing them, and a
  refusal that happens before anything is unpacked — a download that did
  not arrive, a digest that did not match — still takes back the
  directory it created. The one exception to the rule is a build that
  unpacked successfully but did not contain a required program, where
  the error already tells you the unpacked files are still there; where
  such a build unpacked no files at all, there is nothing to leave you
  and the directory comes back like any other refusal.

- [`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
  now checks what it downloaded before it changes anything, and
  remembers a program’s location only if the archive actually contained
  that program. On the build tidymedia fetches by default, it downloads
  the SHA-256 digest gyan.dev publishes beside the archive — before the
  archive itself, so a source that cannot produce one refuses in a
  second rather than after a long download — and refuses to unpack
  anything whose digest does not match. For a build you name yourself,
  pass its digest as the new `archive_checksum`; without one, the call
  installs as before but says the archive was not verified. Note that
  the digest travels from the same host over the same connection as the
  archive, so this catches a corrupted or truncated download, not a
  substituted one.

- [`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)’s
  failures now carry conditions you can catch by class, where they used
  to escape as base R and libarchive text: a download that did not
  deliver, a digest that could not be fetched or read, a digest that did
  not match, an archive that could not be unpacked, and a required
  program the archive did not contain. `ffmpeg` and `ffprobe` are
  required — a build missing either leaves every remembered location
  untouched — while `ffplay` is optional, and an install without it
  succeeds and says so. The temporary download is removed whether the
  install succeeds or fails.

- [`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
  now asks before it downloads or installs anything. The prompt names
  the archive it will fetch, the directory it will unpack into, and the
  remembered program locations it may overwrite; declining returns
  `FALSE` and leaves every one of them untouched — no directory is
  created, nothing is downloaded, and no remembered location changes. In
  a session with no one to ask, the call now aborts rather than proceed
  as if consent had been given, and the abort names the same archive,
  directory and locations the prompt would have; pass the new
  `confirm = FALSE` to install without being asked, which is what an
  unattended script wants.

- [`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
  now installs FFmpeg under `tools::R_user_dir("tidymedia", "data")`,
  the user data directory CRAN policy sanctions, in an `ffmpeg`
  subdirectory — replacing the old `rappdirs` location. This is the
  default only: an `install_dir` you pass yourself is used as before. An
  FFmpeg installed by an earlier version keeps working and is not moved;
  its location was recorded when it was installed, and that record is
  what
  [`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_program.md)
  reads. Running
  [`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
  again does install a second copy, in the new location, and leaves the
  old one on disk; the old directory is yours to delete once nothing
  points at it.

- A binary location remembered with
  [`set_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/set_program.md),
  [`set_ffprobe()`](https://jmgirard.github.io/tidymedia/reference/set_program.md),
  [`set_ffplay()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  or
  [`set_mediainfo()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  now lives under `tools::R_user_dir("tidymedia", "config")`, the user
  configuration directory CRAN policy sanctions, in a file named
  `<program>_location.txt`. A location set before this change is still
  found:
  [`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_program.md)
  and its siblings read the new directory first and, only when no file
  exists there, the old one; nothing is moved or copied. Calling
  [`set_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  again writes the new file, after which the old one is no longer read,
  even if the new file names a binary that has since gone.

### New features

- Hardware encoding is a vocabulary of backends rather than one vendor.
  `hardware =` accepts `"videotoolbox"` wherever it accepted `"nvenc"`,
  on all sixteen verbs that carry the argument, so a Mac encodes on the
  hardware it has. Each backend covers the codec families it has
  encoders for: nvenc covers h264, hevc and av1; videotoolbox covers
  h264 and hevc. The encoder is named from the family and the backend,
  so `video_codec = "libx264"` resolves to `h264_nvenc` under one and
  `h264_videotoolbox` under the other. Asking a backend for a family it
  has no encoder for — av1 under videotoolbox — is an error naming the
  backend and the family. `hardware = "none"` is still the default, so a
  call that does not ask for hardware is unchanged, and
  `fallback = TRUE` still re-encodes in software when the requested
  backend is missing, now saying which backend it fell back from.

- A failed FFmpeg run is now something you can catch. When FFmpeg exits
  non-zero,
  [`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md)
  aborts with a condition of class `tidymedia_ffmpeg_exit`, carrying the
  exit status as a length-one integer in its `tm_status` field:

  ``` r

  tryCatch(
    ffm_run(pipeline),
    tidymedia_ffmpeg_exit = function(cnd) cnd$tm_status
  )
  ```

  The `loudnorm` analysis pass behind `normalize_audio(two_pass = TRUE)`
  raises the same class and carries the same field when FFmpeg exits
  non-zero, so one handler covers both of those runs. The status is
  whatever [`system2()`](https://rdrr.io/r/base/system2.html) reported:
  for a signal-terminated FFmpeg that is the shell’s 128-plus-signal
  number, passed through unchanged, rather than a value FFmpeg chose.
  Three paths still do not signal it: the
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  family records `success = FALSE` for a failed row instead of aborting,
  and the analysis pass raises `tidymedia_loudnorm_no_measurement`
  instead both when `normalize_audio(two_pass = TRUE)` exits zero and
  prints nothing parseable and whenever a
  `normalize_audio_batch(two_pass = TRUE)` row yields no usable
  measurement, for the reason the entry after next gives. Internally the
  package now reads the number off that field; it used to recover it by
  matching a regular expression against the error message, which could
  not tell the wording of one abort from the wording of another and gave
  callers nothing to catch.

- Two more failures now say what they are, so a handler can tell them
  apart.
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)’s
  multi-track diagnostic — the error that reports how many audio tracks
  the input carries when FFmpeg refuses the audio output and no
  `audio_stream` was named — is now classed `tidymedia_ffmpeg_exit` as
  well as `tidymedia_multitrack_separation`, and carries the exit number
  on `tm_status`. An exit-status handler catches it like any other
  refused run; a handler written for the multi-track case still catches
  only that.

  ``` r

  tryCatch(
    separate_audio_video("three-tracks.mkv", "audio.mp3", "video.mp4"),
    tidymedia_ffmpeg_exit = function(cnd) cnd$tm_status
  )
  ```

  `normalize_audio_batch(two_pass = TRUE)`’s analysis phase does not
  answer to the exit class — the next entry says why — but its condition
  now carries `tm_rows`, the 1-indexed offending rows the message names,
  and `tm_row_status`, their exit statuses aligned to it — `NA` where
  the row exited zero. Those numbers used to be discarded, so the only
  account of why a row failed was the prose.

- One handler now covers the `loudnorm` analysis pass in both of its
  forms. `tidymedia_loudnorm_no_measurement` means the analysis pass
  yielded no usable measurement, so no correction could be built — and
  `normalize_audio(two_pass = TRUE)` raises it as well as
  `normalize_audio_batch(two_pass = TRUE)`, so a handler written from
  either help page fires on the other:

  ``` r

  tryCatch(
    normalize_audio("input.wav", "out.m4a", two_pass = TRUE),
    tidymedia_loudnorm_no_measurement = function(cnd) NA_character_
  )
  ```

  One path that raises it could not be caught by any name before: a
  scalar analysis pass that exits zero and prints no parseable
  measurement block. The class rides alongside `tidymedia_ffmpeg_exit`
  where FFmpeg exited non-zero, and alone where it did not, so an
  exit-status handler still sees exactly the runs FFmpeg refused. A
  silent input is deliberately not this event — it was measured, at
  `-inf` — and keeps its own abort.

  Both `_batch` verbs now say why their diagnostic carries no exit
  status.
  [`?normalize_audio_batch`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md)
  explains that its abort fires for rows that exited zero too, so there
  is no one number to report;
  [`?separate_audio_video_batch`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md)
  explains that the batch runner records whether a row succeeded, not
  how FFmpeg exited, so the number is gone by the time the warning is
  assembled.

- The dropped-track check now has an off switch, and every verb that
  runs it says what it costs. `options(tidymedia.check_tracks = FALSE)`
  stops the check — the warning
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  and their `_batch` siblings signal when an input carries audio tracks
  the output will not — for the rest of the session; it defaults to
  TRUE, so nothing changes until you set it. What you get back is the
  check’s only cost: one FFprobe call per distinct input, run before the
  work starts and, on the `_batch` verbs, serially at the front door
  before the fan-out. That is worth declining on a large batch whose
  inputs you already know the tracks of, where the warning has nothing
  to tell you; a row that names an `audio_stream` is never probed, so a
  table whose rows all name one costs nothing either way. Use
  `withr::local_options(tidymedia.check_tracks = FALSE)` to switch it
  off for the rest of one function instead of the session. The option is
  carried into `parallel = TRUE` workers alongside the other two, and a
  value that is not `TRUE` or `FALSE` is refused, naming the option,
  rather than read as one or the other. The six verbs’ help pages now
  state the cost, the switch, and — on the `_batch` verbs — that those
  probes run serially before the fan-out; a batch sweep long enough to
  look like a hang now reports its progress.
  [`?tidymedia`](https://jmgirard.github.io/tidymedia/reference/tidymedia-package.md)
  gained a *Session options* section covering all three session options
  in one place.

- [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  and
  [`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md)
  now warn when the file they read carries audio tracks the file they
  write will not — the same warning
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  and
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  have carried, with the same `tidymedia_dropped_audio` class and the
  same wording. Naming a track with `audio_stream` silences it, and the
  batch form warns once for the whole table, naming every affected row.
  The check costs one FFprobe call per distinct input, runs only on a
  `run = TRUE` call that named no track, and lands before the two-pass
  analysis pass — so on a multi-track input the warning arrives while
  adding `audio_stream` can still save that pass. A wrong
  `target_loudness` or an `audio_codec` of `"copy"` still refuses the
  call before any of this runs.

- A hung media program no longer blocks the R session indefinitely.
  Setting `options(tidymedia.timeout = 600)` gives every FFmpeg, FFprobe
  and MediaInfo process tidymedia starts a wall-clock limit in whole
  seconds. A reached limit is never silent: every call that can start
  one of those programs either aborts or warns. The task verbs,
  [`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md)
  and the raw
  [`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)/[`ffprobe()`](https://jmgirard.github.io/tidymedia/reference/ffprobe.md)/[`mediainfo()`](https://jmgirard.github.io/tidymedia/reference/mediainfo.md)
  hatches abort, naming the program and the limit;
  [`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md)
  aborts too, since a probe that never answered is not an answer.
  Everywhere one hung file must not discard the rest of the work, it
  warns instead. The metadata readers —
  [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
  and the `probe_*()` accessors,
  [`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md),
  [`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md),
  [`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md)
  and the `get_*()` helpers — give an `NA` row and one warning saying
  how many files timed out, so a single hung file does not discard a
  whole corpus.
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  and the `_batch` verbs mark the row `success = FALSE`, as they do for
  any failed job, and warn once at the end of the run saying how many
  jobs the limit gave up waiting for. The dropped-track check behind
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  and their `_batch` siblings warns that it could not check, and the
  provenance manifest warns that it could not read a version. Those two
  lists are not written from memory: a test derives the calls that can
  start one of these programs from the package’s own call graph and
  drives a timeout through each of them. Where the call knows its own
  output — the task verbs and
  [`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md)
  — any partial file the killed run had written is removed just as it is
  after any other failed run; the raw
  [`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
  escape hatch does not parse the argument string it is given, so it
  leaves the partial file in place. The default is `0`, meaning no
  limit, so existing code is unaffected — a legitimate multi-hour encode
  still runs to completion. The limit applies to each spawned program
  rather than to a batch as a whole, and tidymedia’s own
  `parallel = TRUE` paths are bounded by the same limit as their
  sequential ones. The limit bounds the wait rather than promising the
  program dies at the second: R asks, insists after 20 seconds and kills
  after 40, so a program that does not answer is waited for up to 40
  seconds past its limit — measured at 42.0 seconds under a 2-second
  limit — and R does not guarantee termination at all. See
  [`?tidymedia`](https://jmgirard.github.io/tidymedia/reference/tidymedia-package.md).

- A `parallel = TRUE` call now runs its workers under the tidymedia
  settings you set in your own session. Previously each worker started
  from its own empty option list, so `options(tidymedia.timeout = )`
  bounded a sequential batch and left the parallel one unbounded, and
  `options(tidymedia.nvenc_encoders = )` steered a sequential build
  while each worker ignored it and asked FFmpeg for its own encoder
  list. Both values are now carried into each worker for the duration of
  the call, and whatever that worker had set for itself is put back
  afterwards — including when the call fails.
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  also refuses a limit the underlying `timeout=` could not use — a
  fraction of a second, a negative number, `NA`, a string — before it
  dispatches any job, on both paths and whether or not it is going to
  run anything; that refusal used to arrive as an unexplained
  `success = FALSE` per row, or not at all. What is still not carried is
  the remembered answer about your FFmpeg build itself: a worker with no
  `tidymedia.nvenc_encoders` override still asks its own binary once.
  See
  [`?tidymedia`](https://jmgirard.github.io/tidymedia/reference/tidymedia-package.md)
  and
  [`?refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md).

- **A timeout limit can be exceeded, and by how much is now measured.**
  The limit you set — with `options(tidymedia.timeout = )`,
  [`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
  or
  [`local_timeout()`](https://jmgirard.github.io/tidymedia/reference/local_timeout.md)
  — says how long tidymedia waits for a media program, not how long that
  program is allowed to run. When the limit is reached R asks the
  program to stop, insists 20 seconds later, and kills it 20 seconds
  after that, so a program that answers none of the three is waited for
  up to 40 seconds longer than you asked. Under a 2-second limit, an
  FFmpeg blocked reading a pipe nobody writes to returned at 42.0
  seconds on Linux, and a shell child that ignores both signals returned
  at 42.0 seconds on Linux and macOS alike. Plan for it when you pick a
  limit: a 1-second limit across five hung files is three and a half
  minutes of waiting, not five seconds. How much of the lag you see
  depends on your FFmpeg — the same blocked input took 42.0 seconds
  against FFmpeg 6.1.1 and 2.0 seconds against 9.0.1, which answers the
  first signal — and R does not promise the program dies at all: one can
  be written to survive every signal R sends. The documentation for
  [`?with_timeout`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md),
  [`?local_timeout`](https://jmgirard.github.io/tidymedia/reference/local_timeout.md)
  and
  [`?tidymedia`](https://jmgirard.github.io/tidymedia/reference/tidymedia-package.md)
  now says this where it previously said a program was “bounded by” the
  limit.

- `with_timeout(expr, seconds)` puts a wall-clock limit on one call
  without changing the limit the rest of your session runs under. Every
  FFmpeg, FFprobe and MediaInfo program started while `expr` is being
  evaluated is waited for at most `seconds`, plus the escalation lag
  [`?tidymedia`](https://jmgirard.github.io/tidymedia/reference/tidymedia-package.md)
  describes, and when the call ends — by any route, a failure or a
  reached limit included — whatever the session had set before is back,
  an unset option included. It reaches a `parallel = TRUE` fan-out too,
  because the worker is handed the limit in force when the fan-out
  starts. `0` means no limit, so `with_timeout(expr, 0)` lifts a
  session-wide limit for one call; a value the underlying limit could
  not use — a fraction of a second, a negative number, `NA`, a string —
  is refused before `expr` runs, naming `seconds`. See
  [`?with_timeout`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md).

- `local_timeout(seconds)` is the statement form of the same limit: it
  bounds the rest of the function you call it from, rather than an
  expression you wrap. Every FFmpeg, FFprobe and MediaInfo program
  started between the call and the end of that function is waited for at
  most `seconds`, plus the same escalation lag, and when the function
  ends — by any route — whatever the caller had set before is back, an
  unset option included, unless that function discards the undo by
  writing an [`on.exit()`](https://rdrr.io/r/base/on.exit.html) of its
  own without `add = TRUE`. Two calls in one function stack the way any
  pair of `local_*()` calls does, and `seconds` is refused by the same
  rule
  [`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
  uses. Reach for it when the thing to bound is the rest of a function
  body, or several calls that would be awkward to wrap together. See
  [`?local_timeout`](https://jmgirard.github.io/tidymedia/reference/local_timeout.md).

- tidymedia now imports **withr**, which
  [`local_timeout()`](https://jmgirard.github.io/tidymedia/reference/local_timeout.md)
  uses to register its undo on the calling frame. It was already a
  suggested package; installing tidymedia now installs it too. withr
  itself depends on nothing outside base R. The declared minimum is
  withr 2.5.0, and that is the version it was tested against rather than
  merely the one written down. On withr 2.5.0 and on 3.0.3 — the oldest
  this package accepts and the release current on 2026-08-27, when this
  was measured — all 35 `test_that()` blocks of `test-local-timeout.R`
  and `test-with-timeout.R` pass on each, and the four things
  [`?local_timeout`](https://jmgirard.github.io/tidymedia/reference/local_timeout.md)
  says about when the undo runs, the two ways it can be lost included,
  read the same on each. Two of the top levels the call can be written
  at were measured on each too: at the top level of a file run by
  `Rscript` the limit is still set when the script’s own exit hooks
  look, and at the top level of a file passed to
  [`source()`](https://rdrr.io/r/base/source.html) the caller’s value is
  back once [`source()`](https://rdrr.io/r/base/source.html) returns —
  identical on both versions. The versions were seen to part in one
  place: inside `source(file, local = TRUE)` called from a function, the
  line after `local_timeout(30)` still reads the limit on withr 2.5.0
  and already reads the caller’s value on 3.0.3 — either way the
  caller’s value is back once the enclosing function returns. That line
  is the only point inside the sourced file the measurement looks at, so
  it fixes the direction of the split and not how long 2.5.0 holds on.
  Of the two things
  [`?local_timeout`](https://jmgirard.github.io/tidymedia/reference/local_timeout.md)
  says that are not about frames, neither was run on 2.5.0 as that page
  states it: the claim that the limit reaches a `parallel = TRUE`
  fan-out is mentioned in neither file above, and the claim that the
  limit applies per spawned program is stated there of a
  [`local_timeout()`](https://jmgirard.github.io/tidymedia/reference/local_timeout.md)
  above a batch, which no test writes — though four of the blocks above
  drive the same per-spawn machinery through
  [`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
  and all four passed on 2.5.0. Also unmeasured: the `knitr` target
  environment the undo can be registered on, and every withr between
  2.5.0 and 3.0.3. So an installation that resolves withr 2.5.0 rather
  than a later release is running the frame behavior that page
  describes.

- [`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
  now refuses an omitted `expr` itself, saying which argument is
  missing, instead of letting R report a missing parameter of the
  function’s own definition. Both of its arguments are now checked the
  same way, and neither refusal disturbs the session-wide limit.

### Breaking changes

- [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
  and their `_batch` siblings now call the argument that picks whose
  sound to keep `audio_input`, not `audio`. It is the same argument: the
  0-based index of the *input* whose audio is carried, `NULL` for a
  silent output, and a `jobs` column of the same name overriding it row
  by row. Only the name changes, so that it says what it counts the way
  `audio_stream` says it counts one input’s tracks.
  `ffm_codec(audio = )` and `ffm_copy(audio = )` are unchanged. No alias
  is kept: a call still spelling `audio =` on these four verbs is an
  error, which R reports as the argument matching more than one formal
  (`audio` is a prefix of both `audio_input` and `audio_codec`). A
  `jobs` table still carrying an `audio` column is not refused: the
  column is unread, so those rows fall back to the verb’s `audio_input`
  default and write a silent output. Rename the column.

- [`hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
  and
  [`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
  take a second argument naming which backend to answer for, and it has
  no default: `has_hardware_encoder("h264", "nvenc")`,
  `hardware_encoder("h264", "videotoolbox")`. With two backends a helper
  that silently answered for one of them reports on a machine you did
  not ask about — on a Mac,
  `options(tidymedia.hardware_encoders = hardware_encoder("h264"))`
  would have declared the NVIDIA encoder available. The argument accepts
  `"nvenc"` and `"videotoolbox"` only: `"none"` is the verbs’ off
  position, and neither helper has an answer for it. An argument with no
  default can be given one later; one with a default cannot lose it.

- The hardware-encoder helpers no longer name one vendor. `has_nvenc()`
  is now
  [`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md),
  `nvenc_encoder()` is now
  [`hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md),
  and the option that overrides detection is now
  `tidymedia.hardware_encoders`. Each still answers the question it
  answered, under a name that survives the second backend arriving — see
  the entry above for the argument both helpers now take. The old names
  are gone, and an `options(tidymedia.nvenc_encoders = )` you set is no
  longer read.

- [`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md)
  and
  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  (and their `_batch` siblings) take a new `audio_stream` argument
  naming which audio track to work on, and now state that selection on
  every call. Both previously emitted no stream mapping at all, so
  FFmpeg picked for them — one stream of each type, preferring whichever
  audio track carries the container’s “default” flag. On a three-track
  test file whose default flag sat on the third track, both verbs kept
  only that third track, in silence.

  The two verbs read an unset `audio_stream` differently, and the
  difference is deliberate.
  [`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md)
  now keeps **every** audio track, matching
  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  and
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md).
  If you re-encode multi-track sources for the web, your outputs will
  gain tracks they used to lose, and grow accordingly.

  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  keeps the **first** audio track, matching
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  and
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md).
  That is not a narrowing: the verb already produced a single audio
  track, just an unpredictable one. It reads `NULL` this way because
  measuring loudness produces one measurement per audio track while the
  correction applies a single set of values, so normalizing several
  tracks at once would silently apply the first track’s measurements to
  all of them. Under `two_pass = TRUE` the measurement pass now measures
  exactly the track the correction pass normalizes. Normalizing every
  track independently would need per-track filter settings the pipeline
  builder does not have, and is not offered.

  Naming a track the input does not have remains an FFmpeg error rather
  than an R one, on both verbs. Each argument’s documentation says which
  family it belongs to.

  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  now writes **one audio stream and no video**, whatever container you
  name for the output. It has become an audio-producing verb like
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  and
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
  rather than one that passes a video stream through. Two consequences
  worth reading before you upgrade:

  - **Normalizing a recording’s loudness while keeping its picture is no
    longer possible in one call.** If you relied on
    `normalize_audio("clip.mp4", "clip_norm.mp4")` returning a playable
    video, it now returns an audio-only `.mp4`. Normalize to an audio
    file and mux it back with the
    [`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
    escape hatch; a first-class way to do this is on the roadmap.
  - **An input with no audio is now an error** rather than a silent copy
    of the video. A silent screen recording stops with FFmpeg’s “Stream
    map ’’ matches no streams” instead of quietly producing a file with
    no normalized audio in it.

  What you gain is that the output container no longer decides whether
  the call works. `.wav`, `.mp3`, `.aac`, `.flac`, `.opus`, `.m4a`,
  `.mka`, `.oga`, `.w64` and the video containers all behave the same
  way now, where before the choice of extension could decide whether the
  call succeeded at all. (Anything FFmpeg itself cannot encode for is
  still an FFmpeg error — `.wma`, for one, which failed before this
  change too.)

- Two argument-surface changes on the four verbs that gained
  `audio_stream`, worth knowing if you call them tersely. The new
  argument sits before `run`, so **positional** calls that supplied
  `run` (or `parallel`) by position now bind it to the wrong argument —
  name your arguments, or move `run` along by one. And on
  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  /
  [`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
  abbreviating `audio_codec` to `audio` no longer works: with
  `audio_stream` beside it, any prefix shorter than `audio_c` is
  ambiguous. Spell `audio_codec` out.

- [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md)
  and
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  (and their `_batch` siblings) take a new `audio_stream` argument
  naming which audio track to carry, and now state that selection on
  every call. What that changes depends on which of them you use,
  because the two verbs did not start from the same place.

  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md)
  and `segment_video(reencode = FALSE)` mapped *every* stream before, so
  they already kept every audio track — that part is unchanged for them
  — but they dragged subtitle and data streams along and offered no way
  to pick a track. Subtitles are no longer carried: writing to `.mkv`
  used to pass one through and now passes none. That also fixes a real
  failure, since
  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md)
  writing a subtitle-bearing input to `.mp4` used to abort outright
  (FFmpeg has no default subtitle encoder for that container) and now
  succeeds.

  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  with its **default** `reencode = TRUE` is the bigger change: it
  emitted no stream mapping at all, so FFmpeg picked for it — one stream
  of each type, preferring whichever audio track carries the container’s
  “default” flag. On a three-track test file whose default flag sat on
  the second track, cutting a segment kept only that second track and
  discarded the other two in silence. That branch now keeps all three.
  If you cut segments from multi-track sources, your outputs will gain
  tracks they used to lose, and grow accordingly.

  `NULL`, the default, keeps every audio track, matching
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  and
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md).
  Note that
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  and
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  read `NULL` the other way — they take the first track — because their
  output *is* an audio stream and has to be one track. Each argument’s
  documentation says which family it belongs to.

- [`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
  now **sets** the all-streams mapping rather than adding to it, so
  calling it twice no longer duplicates every output stream. Since the
  mapping builder began appending, `ffm_copy() |> ffm_copy()` compiled
  `-map 0` twice and a one-video/one-audio input came out with four
  streams; `ffm_concat() |> ffm_copy()` did the same, because
  concatenation copies internally. No pipeline built by a task verb was
  affected — this only reached you if you composed the builder yourself.

  If the pipeline already states a *different* mapping,
  [`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
  now stops with an error rather than discarding it silently. Pass
  `streams = FALSE` to keep the mapping you set, or call
  [`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
  first and narrow afterwards with `ffm_map(replace = TRUE)`.
  [`ffm_map()`](https://jmgirard.github.io/tidymedia/reference/ffm_map.md)
  itself is unchanged and still appends.

- [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
  and
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  (and their `_batch` siblings) now keep **every** audio track from the
  input instead of letting FFmpeg pick one. Neither verb emitted a
  stream mapping before, so FFmpeg applied its own rules: one stream of
  each type, preferring whichever audio track carries the container’s
  “default” flag. On a three-track file that meant two tracks were
  discarded in silence, and *which* one survived depended on the input’s
  flags rather than on anything you wrote — on a test file whose default
  flag sat on the second track, the second track is what came out. Both
  verbs now state their selection on every call, and a new
  `audio_stream` argument names a single track when you want one.

  Two consequences worth knowing about. Output files from multi-track
  inputs will be larger, because they now carry tracks that were
  previously dropped. And subtitle streams are no longer carried: into a
  container that accepts them (`.mkv`) these verbs used to pass one
  subtitle through, and now pass none. Writing to `.mp4`, the common
  case, is unaffected — that container was already dropping them.

- [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  now names the audio track it takes instead of leaving the choice to
  FFmpeg. It previously emitted no stream mapping at all, so FFmpeg
  picked a track by its own rules — which prefer whichever track carries
  the container’s “default” flag. On a multi-track file that could be
  any track, and it could differ between FFmpeg versions on the same
  file, which is exactly the kind of invisible variation this package
  exists to remove. The verb now maps the input’s **first** audio track
  unless you say otherwise with the new `audio_stream` argument.

  Single-track inputs are unaffected. On a multi-track input whose
  *second* track is flagged as the default, the extracted audio changes
  — you would have got that second track before and get the first one
  now. Pass `audio_stream = 1` to keep the old result on such a file.

  Extracting to a container that can hold subtitles (`.mkv`, say) also
  stops carrying a subtitle track through. The old command named no
  streams at all, so FFmpeg carried one stream of *each* type and `-vn`
  removed only the video; naming the audio stream takes audio alone.
  Extracting to an audio-only container such as `.aac`, `.m4a` or `.mka`
  is unaffected, because those never carried the subtitle track in the
  first place.

- `audio_stream` is inserted **before** `run` on
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
  [`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md)
  and
  [`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md),
  so the arguments after it have all shifted one position: **calls that
  pass `run` (or `parallel` on the batch verbs) by position rather than
  by name must be updated.**
  `extract_audio(video, "audio.aac", "copy", FALSE)` now reads `FALSE`
  as the audio-stream index rather than as `run` — an error rather than
  a silent misread, since the index must be a whole number. In line with
  this package’s pre-1.0 clean-break policy the argument is placed where
  it belongs rather than appended for compatibility; naming your
  arguments avoids the problem entirely.

- `audio_stream` is likewise inserted **before** `run` on
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  and
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
  so `run` (and `parallel` on the batch verb) shifts one position there
  too: **calls passing them by position rather than by name must be
  updated.** As above, the argument is placed where it belongs rather
  than appended, in line with this package’s pre-1.0 clean-break policy;
  naming your arguments avoids the problem.

- [`ffm_map()`](https://jmgirard.github.io/tidymedia/reference/ffm_map.md)
  appends instead of overwriting. Calling it twice on the same pipeline
  used to discard the first mapping; it now keeps both, emitting one
  `-map` per mapping in the order given, which is what lets a pipeline
  keep the video and then name one audio track. `mapping` may now be a
  character vector for the same reason. Pass `replace = TRUE` to get the
  old discard-what-came-before behavior, which is how you narrow the
  all-streams mapping that
  [`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
  sets. No task verb’s compiled command changes *as a result of this*:
  each sets its mapping once. Note that composing Layer-1 builders that
  each set a mapping now accumulates them —
  [`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
  maps every stream, so calling it twice, or calling it after
  [`ffm_concat()`](https://jmgirard.github.io/tidymedia/reference/ffm_concat.md)
  (which calls it internally), emits `-map 0` twice and duplicates every
  stream in the output. That was a harmless no-op before; use
  `ffm_map(replace = TRUE)` to narrow instead.

- [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and
  [`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md)
  rename the `format` argument to `audio_codec`. The argument was always
  an audio codec — its own documentation said so, and its value has only
  ever been passed to FFmpeg’s `-c:a` — so this brings the last of the
  codec arguments onto the package’s `audio_codec` / `video_codec`
  naming, and every codec argument in the package is now spelled the
  same way.

  Only the name changes: `audio_codec = NULL` is still the default and
  still compiles `-q:a 0`, letting the output extension pick the codec
  at highest VBR quality, so existing default calls produce
  byte-identical commands. Note that `NULL` means something different
  here than on the other transform verbs, where it leaves the codec
  unset — on this verb it selects `-q:a 0`.

  `format` is removed rather than deprecated, in line with this
  package’s pre-1.0 clean-break policy. Calls passing it to
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  get R’s usual `unused argument` error;
  [`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md),
  whose `...` would otherwise ignore it in silence, aborts and names the
  replacement, as it does for a stale `format` column in a jobs table.

  In a jobs table, the per-row column is likewise now `audio_codec`, and
  it gains the ability to spell “unset”: `NA` in a cell keeps that row
  on the `-q:a 0` default, which the old `format` column could not
  express.

- [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  and
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md)
  replace the `reencode` argument with per-stream `audio_codec` and
  `video_codec` arguments, so you can name the encoder for each output
  file instead of choosing between “copy everything” and “let the
  container decide everything”. Both default to `"copy"`, which compiles
  exactly the commands `reencode = FALSE` compiled before;
  `audio_codec = NULL, video_codec = NULL` reproduces `reencode = TRUE`;
  and a codec name (`audio_codec = "libmp3lame"`) transcodes that stream
  alone. Each argument governs only its own output file.

  `reencode` is removed rather than deprecated, in line with this
  package’s pre-1.0 clean-break policy. Calls passing it to
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  get R’s usual `unused argument` error;
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
  whose `...` would otherwise ignore it in silence, aborts and names the
  replacement.

  In a jobs table, `audio_codec` and `video_codec` may be per-row
  columns where `NA` means “leave that stream’s codec unset”. They
  replace the per-row `reencode` column. Because each input row fans out
  into an audio row and a video row, the returned table collapses the
  two into one `codec` column carrying each row’s encoder for its own
  stream.

- [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
  and
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
  (and their `_batch` siblings) no longer re-encode the audio they pass
  through. They now stream-copy it, matching what
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
  and
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  have always done: previously these four left the audio codec unset, so
  whatever encoder your FFmpeg build defaults to for the output
  container silently re-encoded the audio — a quality loss, and a result
  that depended on the machine. Their compiled commands therefore gain
  `-codec:a copy`.

  The new `audio_codec` argument controls this. `"copy"` is the default;
  name an encoder (e.g. `audio_codec = "aac"`) to transcode instead, or
  pass `audio_codec = NULL` for the old behavior of leaving the codec
  unset. Note that a stream copy fails if the output container cannot
  hold the source audio codec (FLAC in `.mp4`, say) — name an encoder in
  that case. In a jobs table, `audio_codec` may be a per-row column,
  where `NA` means “leave it unset”.

  Cutting with `segment_video(reencode = FALSE)` copies every stream by
  definition, so any `audio_codec` other than `"copy"` is an error
  there, as is naming an audio encoder on a composite that carries no
  audio at all.

- [`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md)
  now rejects a wrongly typed `audio` column up front with a clear
  message instead of failing partway through the batch, and
  [`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md)’s
  equivalent check no longer accepts an all-`NA` column of the wrong
  type.

### Performance

- `hardware = "nvenc"` asks FFmpeg which encoders it has once per R
  session instead of once per call. Every such call previously started a
  separate FFmpeg process to re-read the same encoder list, so a 500-row
  nvenc batch paid 500 of them before encoding anything; now it pays
  one. The compiled commands are unchanged.

  The answer is remembered for the rest of the session, which matters if
  the build changes underneath you — a fresh FFmpeg install, a new GPU
  driver, a different binary. Two calls discard it: the new
  [`refresh_ffmpeg_capabilities()`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md),
  and
  [`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  (or
  [`set_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)),
  which discards it for you since it points tidymedia at a different
  binary. Setting `options(tidymedia.nvenc_encoders = )` still overrides
  the answer outright and is read before anything remembered, so it
  takes effect at once.

  [`ffmpeg_encoders()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)
  and
  [`ffmpeg_codecs()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_codecs.md)
  are never remembered: they query FFmpeg on every call and always
  report the build as it is now.

  What is remembered is per R process, so under `parallel = TRUE` each
  worker asks once rather than sharing the parent’s answer. That is
  bounded by the worker count, not the row count.

- [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
  and the `probe_*()` shortcuts take a new `parallel` argument (default
  `FALSE`). With `parallel = TRUE` the per-file probes are spread across
  workers with the optional **furrr** package, following whatever
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  is active — the same mechanism
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  already uses, so one plan configures both. The output is unchanged
  either way: the same tibbles, the same types, and rows in the order
  the input vector gave them. Files that cannot be probed still produce
  one warning at the end of the call naming all of them, not one per
  worker.

  Two things to know. `furrr` is looked for only when `parallel = TRUE`,
  so it stays an optional dependency for everyone else. And because the
  default `future` plan is sequential, `parallel = TRUE` on its own
  gives no speedup — it now says so with a warning rather than quietly
  doing nothing. Set a plan first,
  e.g. `future::plan(future::multisession)`.

- [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
  and the `probe_*()` shortcuts now read each file with a **single**
  FFprobe process instead of one per stream plus one more for the
  container. A five-stream file needed six processes and needs one. The
  saving grows with stream count and with the number of files, so it is
  largest on exactly the batch work these functions exist for — locally,
  probing ten copies of a four-stream file went from 1.7 seconds to
  0.46. The returned tibbles keep the same columns, in the same order,
  with the same values and types — except for the invented columns
  described under Bug fixes below, which were never data in the first
  place.

### Bug fixes

- Asking for hardware encoding with a codec the backend cannot encode is
  now refused by the function you called, whatever `fallback` is set to.
  NVIDIA nvenc has no `prores` encoder and Apple videotoolbox has
  neither `prores` nor `av1`, so a call naming one of those was always
  an error — but with `fallback = TRUE` the eight verbs that fan out
  over rows or segments reported it as
  `` Error in `purrr::pmap(jobs, .f, ...)` `` with `In index: 1` beneath
  it: a dependency’s name and an internal row number in place of the
  function you typed. With `fallback = FALSE` the same call already
  named the verb. A `video_codec` that matches no codec family at all —
  a typo, say — moved the same way. `fallback` is for a machine whose
  FFmpeg was built without an encoder, and that behavior is unchanged: a
  codec the backend does cover but this build does not list still falls
  back to software with a message.

  No call that used to succeed is refused now. Three things about the
  refusal itself do change, all of them because it now happens before
  any row is built. It no longer carries the `In index:` line, so a
  batch of many rows says which codec and backend are wrong but not
  which row named them. Rows earlier in the table no longer print their
  “falling back” messages first, since nothing is built before the
  refusal. And when one call names codecs from several families — a
  `video_codec` column mixing them — the wrong-codec complaint now comes
  first, where before you could see a “not available on this machine”
  complaint about an earlier, valid codec instead.

- [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  now refuses an `outfiles` value it cannot use — a number, `NA`, a list
  holding one, or a character vector with a missing value in it — and
  names itself when it does. The value used to travel into the
  per-segment fan-out, so the failure came back as
  `` Error in `purrr::pmap(jobs, .f, ...)` `` with `In index: 1` beneath
  it: a dependency’s name and an internal row number in place of the
  function you called. The check is on each element, so an output name
  with a space in it, and a list of output names, both still compile
  exactly the command they compiled before. It also runs before your
  FFmpeg build is asked whether it has an nvenc encoder, so a wrong
  `outfiles` is reported as a wrong `outfiles` on a machine with nvenc
  and on one without.

- `ffmpeg_codecs(sort_by_type = )` now refuses a value that is not
  `TRUE` or `FALSE`, with the message
  [`ffmpeg_encoders()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)
  has always given for it, and without running FFmpeg first. What it did
  before depended on the value. A string, `NA`, or more than one value
  ran the binary, parsed the whole codec list, and only then failed on
  the internal `if` that does the sorting — so a call it could have
  refused outright cost a process, and the failure named no argument. A
  number, though, never failed at all: `if (123)` is `TRUE` in R, so
  `ffmpeg_codecs(sort_by_type = 1)` returned the sorted table. **That
  call is now an error**, matching what
  [`ffmpeg_encoders()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)
  has always done with it. Pass `TRUE` or `FALSE`.

- A `tidymedia.timeout` the underlying limit could not use — a fraction
  of a second, a negative number, `NA`, a string, more than one number —
  is now refused by the function you called. It was refused by whatever
  read the option first, so
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  reported the failure as
  [`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
  [`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md)
  as
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  followed by the whole deparsed builder it had been handed, and
  [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
  as `purrr::map(infile, probe_one)`, wrapped in an indexed error from
  `purrr` — none of them a function you had typed. The message itself is
  unchanged, and every call now gives the same one. The refusal also
  arrives on a `run = FALSE` call, which used to compile a command under
  a limit it could never have used; the `_batch` verbs already behaved
  this way. An argument your call got wrong is reported first wherever
  the verb itself can see it is wrong: the limit is checked after the
  verb’s own guards and after the command has been assembled, so a bad
  `regions`, `pixel_format` or `video_codec` reports as itself whether
  or not a limit is set. Asking for `hardware = "nvenc"` no longer
  changes that: your FFmpeg build is asked what encoders it has after
  every check the verb itself makes, so a bad `audio_codec`,
  `pixel_format` or `audio_stream` reports as itself there too — whether
  or not a limit is set, and whether or not that build has nvenc.
  `fallback` is checked where that question is asked and so moved down
  with it: a call wrong about both `fallback` and `pixel_format` now
  hears about the pixel format. Where the check runs somewhere the verb
  reaches only later, it loses to both the limit and the encoder
  question: a `_batch` job table’s `output` column and
  [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md)’s
  `pixel_format` and `color` are validated inside the per-row fan-out,
  so a set limit is reported instead of them, and so is a missing nvenc
  encoder under `hardware = "nvenc"` on a build without one. Two calls
  refuse nothing, because neither reads a limit: `has_nvenc()` answering
  from a `tidymedia.nvenc_encoders` you set, and a `probe_*()` shortcut
  handed a `probe` object instead of an `infile`, which reprobes
  nothing.

- The advice
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  gives when an audio output fails no longer arrives when you are
  already following it. Writing a multi-track input’s audio into a
  container that holds only one stream makes FFmpeg fail, and the error
  then reports how many tracks the input carries and offers two ways
  out: name one track with `audio_stream`, or write a container that
  holds several. That report was attached to any audio command FFmpeg
  ended at a non-zero exit status on a multi-track input — including one
  whose output was already `.mka`, `.m4a`, `.mp4`, `.mov`, `.mkv`,
  `.webm`, `.ogg`, `.opus` or `.ts`, every one of which holds three
  audio tracks (`.webm`, `.ogg` and `.opus` under an encoder they
  accept, such as `audio_codec = "libopus"`; none has room for AAC). On
  those the container is not what FFmpeg objected to, so the report
  named a cause that was not the cause while telling you to do the thing
  you had already done. Writing to one of those nine, the error you get
  is now the one the run itself raised — same class, same exit status,
  same message, but for the line saying the video output was written,
  which a failing audio half carries when the video command wrote its
  file and the failure is an rlang condition. Those nine are the
  containers the package knows about, not every one FFmpeg can write
  several audio streams into (`.avi` and `.nut` take three too), so on
  an output outside the list the report still appears.
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md)
  does the same: such a row is dropped from the post-fan-out warning
  rather than listed in it, the headline count follows the rows actually
  listed, and a batch whose failed audio rows all write to those
  containers warns not at all. The extension is read without regard to
  case, so `OUT.MKA` counts. What the report says when it does appear is
  unchanged, and it still tells you what the call did rather than why
  FFmpeg refused — a stream copy into a container that will not hold the
  source codec still looks the same from there.

- A failed audio output no longer costs you the video in
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md).
  The verb compiles two commands and runs the audio one first; when that
  command failed, the call aborted before the video command ran at all,
  so a caller whose multi-track input would not fit the requested audio
  container lost the video half too and had to run the whole separation
  again. The video command now runs either way. The audio failure is
  still what aborts the call, and its error gains one line —
  `The video output was written to 'video.mp4'.` — so it is clear the
  video half survived. That line is shown when the video command
  succeeded and that run actually wrote `videofile`, decided by
  comparing the file before the video command against the file after it
  rather than by that command’s exit status alone: a command that
  returns zero having left a file already at that path untouched does
  not claim to have written it. If the video command fails as well, the
  line is not shown and the audio failure is still the error you get —
  and that error carries the video command’s own condition on its
  `tm_video_error` field, so the second failure is available to a
  handler instead of only to a human reading FFmpeg’s console output.
  The field is `NULL` when the video command succeeded. What each failed
  command leaves at its own output path is unchanged: a partial file
  that run wrote is removed, a file it never wrote to is left as it was;
  the audio failure’s error says which of the two happened to
  `audiofile`. A wall-clock limit set with
  [`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
  bounds each spawned program, so an audio half that reaches the limit
  still lets the video command run on a fresh limit of its own, and such
  a call can wait up to two limits rather than one.
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md)
  is unchanged: it already ran both rows.

- A missing value in a size, position or rate argument is now refused
  instead of reaching FFmpeg.
  `crop_video(f, o, width = NA_real_, height = 100)` used to fail with
  R’s own `missing value where TRUE/FALSE needed`, which names neither
  the argument nor the function you called; `width = NA_character_` was
  worse, because it was accepted and compiled `crop=w=NA:h=100` into the
  command — `-vf "crop=w=NA:h=100:x=(in_w-out_w)/2:y=(in_h-out_h)/2"` —
  so a `run = FALSE` call returned a command string FFmpeg would have
  rejected later. Both now abort with
  `` `width` must be a single FFmpeg expression or number. `` against
  the verb you called. The same refusal covers the size and position
  arguments of
  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md)
  and
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  the region values
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  takes, the same values passed as arguments to their `_batch` siblings,
  and the
  [`ffm_crop()`](https://jmgirard.github.io/tidymedia/reference/ffm_crop.md)
  /
  [`ffm_scale()`](https://jmgirard.github.io/tidymedia/reference/ffm_scale.md)
  /
  [`ffm_fps()`](https://jmgirard.github.io/tidymedia/reference/ffm_fps.md)
  /
  [`ffm_overlay()`](https://jmgirard.github.io/tidymedia/reference/ffm_overlay.md)
  /
  [`ffm_drawbox()`](https://jmgirard.github.io/tidymedia/reference/ffm_drawbox.md)
  builders. A missing value in a `jobs` column is refused as before, by
  the column’s own guard, which names the column.

- [`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md)
  names only the column that actually holds a bad path. A row whose
  `main` is fine and whose `overlay` is missing used to read
  `` `jobs$main` and `jobs$overlay` name 1 file that can't be found or read. ``,
  sending you to a column with nothing wrong in it. It now reads
  `` `jobs$overlay` names 1 file that can't be found or read. ``, and
  still names both when both are bad.

- [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
  [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
  and
  [`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md)
  report a missing input file before they report duplicated inputs.
  Called without an `output` column, these verbs derive one output name
  per input, so two rows naming the same input would collide and are
  refused — but that refusal ran first, so a table whose twenty rows all
  carried one path typed wrong was told its inputs were duplicated and
  never told which file was not there. The path is what you can act on,
  so it now reports first; a table of duplicated inputs that all exist
  still gets the duplication message. One further order changes with it,
  on
  [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md)
  and
  [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md):
  a duplicated table that also carries a bad `video_codec` or
  `audio_stream` argument now reports that argument, where it used to
  report the duplication. On those two verbs the `video_codec` and
  `audio_stream` arguments are checked above the missing-file sweep; the
  duplication refusal now sits below it.

- [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  and
  [`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md)
  work again when the output is a FLAC (`.flac`) or Ogg Vorbis (`.oga`)
  file. On FFmpeg 9 these failed with “Could not open encoder before
  EOF” and left a zero-byte file: the loudness filter hands its output
  on in very long frames, which most encoders are re-framed out of but
  FLAC and Vorbis are not, and the frame was longer than FLAC will
  encode. Loudness normalization now re-chunks its output, so every
  audio container works. Commands built with
  [`ffm_loudnorm()`](https://jmgirard.github.io/tidymedia/reference/ffm_loudnorm.md)
  carry the extra `asetnsamples` filter, which is visible in the
  compiled command string.

- A run that fails no longer leaves a broken output file behind. FFmpeg
  creates its output before it knows the command will work, so a refused
  encode left a zero-byte file sitting where a result should be — and if
  you were writing over an existing file, FFmpeg had already truncated
  that to zero on its way to failing. Every verb, and every row of a
  `_batch` verb, now deletes what the failed run wrote, and the error
  says so and names it.

  Only what the run wrote. Some failures — an unknown encoder, an
  unknown filter, a bad option value — are refused before FFmpeg opens
  the output at all, and a file already sitting at that path is then
  untouched. tidymedia checks the output’s size and timestamp before the
  run and again after the failure, leaves such a file exactly as it was,
  and says that instead. A file whose name contains `*`, `?` or `[` is
  deleted as the name it is, never as a pattern, so a neighboring file
  is never taken with it.

  `overwrite = FALSE` against a file that was already there keeps its
  own guarantee: FFmpeg was told not to replace it, so neither will
  tidymedia. A failed run that created its output still has it cleaned
  up whatever `overwrite` says. If the file cannot be deleted — a
  read-only directory, say — the error tells you it is still there
  rather than claiming a cleanup that did not happen.

  [`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md)
  writes a numbered image sequence from one command, and a failed run
  there deletes the frames that run wrote, in that directory, leaving an
  earlier run’s frames alone.

  This does not reach
  [`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md),
  the raw escape hatch, which runs a command string it cannot parse for
  an output path.

- A `_batch` verb that refuses a bad value carried in a `jobs` column
  now says which row carries it. The refusal message gains one final
  bullet — `First offending jobs row: 7.` — on the front-door value,
  vocabulary, codec-token and contradiction sweeps of the batch verbs,
  so a bad cell in a 50-row table no longer has to be found by hand. The
  rest of the message is unchanged byte-for-byte, and the same value
  passed as the verb’s own argument (which applies to every row) still
  refuses without naming one. On
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
  whose jobs table is reshaped internally, the row named is the row of
  *your* table, not the reshaped one.

- A bad crop, scale, rate or pixel-format value is now refused by the
  function you called.
  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md)’s
  `width`, `height`, `x` and `y`,
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)’s
  `width`, `height`, `fps` and `pixel_format`, and
  [`sample_frames_batch()`](https://jmgirard.github.io/tidymedia/reference/sample_frames_batch.md)’s
  per-row rate used to be reported against an internal builder the
  caller never called —
  [`ffm_crop()`](https://jmgirard.github.io/tidymedia/reference/ffm_crop.md),
  [`ffm_scale()`](https://jmgirard.github.io/tidymedia/reference/ffm_scale.md),
  [`ffm_fps()`](https://jmgirard.github.io/tidymedia/reference/ffm_fps.md),
  [`ffm_pixel_format()`](https://jmgirard.github.io/tidymedia/reference/ffm_pixel_format.md)
  — or, on the `_batch` verbs, against
  [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html)
  with an `In index:` prefix. Each `_batch` sibling refuses the value
  whether it is passed as the argument or carried in a `jobs` column,
  and before any row runs. One message changes: a malformed
  `pixel_format` used to be reported against `format`, an argument name
  these verbs do not have, and now names `pixel_format`. On the two
  `_batch` verbs gaining a sweep, a call that is also wrong about
  `hardware = "nvenc"` — the machine lacks the encoder — is now told
  about the bad value first, the same answer
  [`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md)
  gives for its `width` and `height`.

- A bad region, inset-scale or loudness value is now refused by the
  function you called.
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)’s
  per-region `x`, `y`, `width` and `height` values,
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)’s
  out-of-range `scale`, and
  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)’s
  `target_loudness`, `true_peak` and `loudness_range` used to be
  reported against an internal builder the caller never called —
  [`ffm_drawbox()`](https://jmgirard.github.io/tidymedia/reference/ffm_drawbox.md),
  [`ffm_overlay()`](https://jmgirard.github.io/tidymedia/reference/ffm_overlay.md),
  [`ffm_loudnorm()`](https://jmgirard.github.io/tidymedia/reference/ffm_loudnorm.md)
  — or, on the `_batch` siblings, against
  [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html)
  with an `In index:` prefix. Each `_batch` sibling refuses the value
  whether it is passed as the argument or carried in a `jobs` column
  (`regions`, which exists only as a column on the batch verb, in its
  column form), and before any row runs. Under `two_pass = TRUE`, a bad
  loudness target is refused before the analysis pass measures the
  input, instead of after that measurement was already spent.
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)’s
  existing complaint about a non-numeric `scale` is unchanged; the new
  refusal covers a numeric `scale` outside `0 < scale <= 1`. On the two
  `_batch` verbs that take `hardware`, a call also wrong about
  `hardware = "nvenc"` on a machine without the encoder is now told
  about the bad value first, the same answer the crop and standardize
  verbs give. The documented loudness ranges and the checks that enforce
  them now read from one shared definition per range, so the
  documentation and the refusal can no longer drift apart.

- An input file that does not exist is now reported against the verb you
  called. Every `_batch` verb used to accept a `jobs` table naming a
  missing path and only discover it once the batch was under way, so the
  error arrived as `In index: 3` against
  [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html),
  naming a row number in a table you may have built programmatically
  rather than the file that was not there.
  [`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md)
  and
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  had no check of their own at all and reported
  `Error in ffm_files(infiles, outfile)`. All of them now refuse the
  call up front, name the function you called, and account for **every**
  missing path in one message rather than stopping at the first row — so
  one run tells you about all four typos in a fifty-row table instead of
  four runs. One path typed wrong the same way in twenty rows is one
  missing file, not twenty.

  The check reads the same way whichever shape carries the paths: the
  `input` column, the `inputs` list-column of the many-in/one-out verbs,
  and
  [`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md)’s
  `main`/`overlay` pair. A call that is wrong about a path *and* about
  something else — contradictory codec arguments, an unavailable
  hardware encoder, an out-of-range per-row value — is now told about
  the path first, on the reasoning that a path typed wrong is the more
  likely mistake and is the one you can act on without reading further.
  Malformed table shapes and wrong column types still report before it,
  since a column whose type has not been checked yet cannot usefully be
  swept for paths. Where a verb’s checks on its own *arguments* fall
  relative to the sweep is not uniform and is not a promise:
  [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
  reports a bad `video_codec` before the sweep and a bad `width` after
  it. The refusal of duplicated inputs on a verb deriving its own output
  names reports *after* it, so that promise about twenty rows sharing
  one typo holds whether or not you supply an `output` column.

  A file that *exists* but cannot be opened for reading is refused the
  same way, and by the same test the pipeline has always applied: there
  is now one readability test, reached both from the verb you call and
  from the pipeline underneath it, so the two cannot disagree about
  which paths are acceptable. Such a file was previously refused only
  once the pipeline reached it, reported against
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md)
  or, from a `_batch` verb, against
  [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html)
  with an `In index:` prefix.

  Because one message now covers both cases, the wording changed. Where
  these verbs said `` `infile` does not exist: 'clip.mp4'. `` they now
  say `` `infile` can't be found or read: 'clip.mp4'. ``, and the
  many-path form reads `names 2 files that can't be found or read`.
  Which calls are refused is unchanged.
  [`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md)
  and `write_mediainfo_template()` keep the existence wording, their
  file arguments not being pipeline inputs.

- A malformed codec value — a string carrying whitespace or shell
  characters, such as `"aac -evil"` — is now reported against the
  argument and the function you called. Every verb whose `video_codec`
  or `audio_codec` argument *sets* a codec used to accept such a value
  at its front door and refuse it deeper in, reporting it against the
  pipeline’s internal `audio` / `video` setting, against an internal
  helper, or — on the verbs that fan out, meaning every `_batch` sibling
  and the scalar
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  — against the fan-out, prefixed `In index:`. Non-string values were
  already reported this way.
  ([`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md)
  carries same-named arguments that are expected probe *values* rather
  than codec settings; it is unaffected.)

  A `_batch` verb reads the same value three ways, and all three now
  answer alike. A malformed value in the scalar argument used to be
  discarded in silence whenever the `jobs` table carried a column of the
  same name, since the column wins; it is now refused. A malformed value
  in the **column** used to be reported from inside the fan-out, naming
  an internal closure; it is now refused at the verb’s own front door,
  before any row runs.

  Under `hardware = "nvenc"`,
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
  and
  [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
  used to accept a malformed `video_codec` outright — the encoder name
  was rewritten to the nvenc equivalent before anything checked it, and
  the rewritten name is well-formed. They now refuse it, as
  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md)
  already did. One consequence for callers who pass both
  `hardware = "nvenc"` and bad dimensions:
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
  now reports the dimensions first, where it used to report the missing
  nvenc encoder first.

  No compiled command changes: every legal codec value compiles exactly
  the command it did before.

- When `hardware = "nvenc"` is requested on a machine whose FFmpeg does
  not list the encoder, every verb taking `hardware` now reports the
  error against the function you called rather than against an internal
  helper. This last covers the verbs that fan out over several commands
  — every `_batch` verb, and the scalar
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
  which fans out over its segments — which previously reported the error
  against
  [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html)
  with an internal row index, or against a `furrr` closure under
  `parallel = TRUE`.

  On those verbs the check now also runs before any row does, so a large
  jobs table fails immediately instead of after building the first row’s
  command. Where a `video_codec` column names several codec families in
  one call, each family is checked: a build listing `h264_nvenc` but not
  `av1_nvenc` refuses the table rather than failing partway through it.

  Only the encoders a call actually needs are checked, so a row that
  copies rather than re-encodes is not held to an encoder it never asks
  for.

  Because the check now runs first, it reports ahead of anything still
  raised from inside the fan-out. It does not report ahead of the other
  checks that also moved to the front door in this development cycle. A
  call that names an unavailable encoder and is *also* wrong about a
  per-row value — a malformed `regions` table, an out-of-range `width`,
  `height`, `margin` or `audio` index, a misspelled `direction` or
  `position` — is told about the value, and a call whose *arguments*
  contradict each other is told about the contradiction. Both of those
  answers are the same on every machine, which is why they come first;
  see the two entries below. Such calls failed before and fail now; what
  changes is which of the errors you see.

  `fallback = TRUE` behaves exactly as before, and no call that used to
  succeed now fails.

- Arguments that contradict each other are now refused by the function
  you called. Six such contradictions used to be caught only while each
  command was being built, which on a verb that processes many files at
  once meant the error was reported against
  [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html)
  with an internal row index instead of against your call:

  - a video stream copy asked to encode on the GPU
    ([`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md));
  - a `reencode = FALSE` cut that names a `video_codec` or `hardware`
    ([`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
    [`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md));
  - a `reencode = FALSE` cut that names an `audio_codec` other than
    `"copy"` (same two verbs);
  - an `audio_codec` with no audio carried into the output
    ([`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md),
    [`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md));
  - `resize = TRUE` across other than two inputs
    ([`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md)).

  Where any of these values can arrive as a `jobs` column, the check is
  made per row: a table with one offending row is refused for that row,
  and a table with none compiles as before. Large tables now fail
  immediately rather than after building the first command.

  Exactly the same calls are refused as before, verified cell by cell
  across a grid of every combination of the arguments involved. What
  moves is which function the error names, and when.

  Because the check now runs before any row is built, it also reports
  before errors that used to surface from inside the fan-out. A call
  that is wrong in more than one way — a contradiction *plus* an
  out-of-range `audio` index, a misspelled `direction`, an out-of-range
  `margin`, or a bad `run`/`parallel` value — is now told about the
  contradiction. Such calls failed before and fail now; which of the
  errors you see is what changes.

  On a machine lacking an nvenc encoder, a call that both contradicts
  itself and asks for GPU encoding is told about the contradiction
  rather than about the encoder. A contradiction between two arguments
  is the same mistake on every machine, so it is not reported
  differently depending on which FFmpeg build you happen to have.

  The single-file verbs —
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
  — build one command each, so three of the four contradictions they can
  raise already named the verb and are unchanged. The fourth,
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)’s
  two-input `resize` error, reported against an internal function name
  and now names
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md).

- Six per-row value checks are now made by the function you called,
  before any row runs. Each was previously reached only while a row’s
  command was being built, so on a verb that processes many files at
  once the error was reported against
  [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html) —
  or against a `furrr` closure under `parallel = TRUE` — with an
  internal row index instead of against your call:

  - a `width` or `height` that is neither a positive number nor an
    FFmpeg expression
    ([`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md));
  - a negative `margin`
    ([`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md));
  - a `regions` table missing a required column, or carrying one of the
    wrong type
    ([`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md));
  - an `audio` index past the number of inputs in that row
    ([`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md));
  - a `direction` outside `"horizontal"` and `"vertical"`
    ([`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md));
  - a `position` outside the five inset positions
    ([`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md)).

  The last two were previously checked only for the *argument*. A `jobs`
  column of the same name had its type checked but never its values, so
  a misspelled cell reached the fan-out; both columns are now checked
  against the same list of values the argument is checked against.

  Where any of these values can arrive as a `jobs` column, the check is
  made per row: a table with one offending row is refused for that row,
  and a table with none compiles as before. Large tables now fail
  immediately rather than after building the first command.

  Exactly the same calls are refused as before, verified cell by cell
  across a grid that varies each value in and out of range, as an
  argument, as a column, and as a column whose rows disagree. What moves
  is which function the error names, and when.

  Because these checks now run before any row is built, they also report
  before errors that used to surface first. A call wrong in one of these
  ways *and* asking for an nvenc encoder this machine does not have is
  now told about the value — the reverse of the order shipped earlier in
  this development cycle, so that the diagnosis no longer depends on
  which FFmpeg build you happen to have. A call wrong in one of these
  ways *and* in an argument that
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  alone guards — `run`, `parallel`, `progress`, `manifest`, `checksums`,
  `verify` — is likewise told about the value. (The `jobs` table’s own
  shape is not in that list: all four verbs check it themselves before
  reaching
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  so nothing displaces it.)

  On
  [`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md)
  and
  [`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md),
  a call can be wrong in **both** a per-row value and one of the
  contradictions above. A value error and a contradiction resolve the
  same way whether the value arrived as an argument or in a `jobs`
  column; the contradiction reports first. Four checks moved to make
  that true — `direction`, `position`, `margin`, and the `audio` index —
  so a call passing one of these as an **argument** alongside a
  contradiction is now told about the contradiction, where it used to be
  told about the value. If you match on the text of an error from such a
  call, that is the message that changed.

  Two consequences worth knowing if you match on error text. First,
  these four checks now also report **after** every argument check that
  runs before them, not only after the contradiction: a call wrong in
  both one of these values and in a malformed `video_codec` or
  `audio_codec` token, an unrecognized `hardware`, a `resize` that is
  not `TRUE` or `FALSE`
  ([`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md)
  only), a non-numeric `scale`
  ([`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md)
  only), or a `jobs` table of the wrong shape is now told about that
  other check. Second, the same reordering reaches the single-call
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  and
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md),
  which check `direction` and `position` inside the pipeline they share
  with the batch verbs — so
  `compare_videos(files, out, direction = "sideways", audio_codec = "aac")`
  now reports the `audio_codec` contradiction too. Exactly the same
  calls are refused as before in every case; only which error you are
  shown moves.

  [`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md)
  gains a front-door check on its `audio` index as part of this. An
  out-of-range index in a `jobs` `audio` column was previously caught
  only while a row’s command was being built, so it was reported against
  [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html) and
  named an internal variable (`aud`); it now aborts naming the verb you
  called, before any row runs. Two errors that used to report ahead of
  it — an unavailable nvenc encoder, and
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)’s
  own argument checks — now report after it, matching the other value
  checks above.

  One class of `audio` value behaves differently from the rest, on both
  verbs. Passing `audio = NA` (or `NaN`) asks to drop the audio, so
  these are the `audio` arguments that *create* the “`audio_codec` needs
  an audio stream to encode” contradiction rather than removing it —
  `picture_in_picture_batch(jobs, audio = NA, audio_codec = "aac")` now
  reports that contradiction where it used to report the `audio` value.
  An index carries audio, so it never creates that contradiction: out of
  range it reports the `audio` value, and in range the call compiles.

  Two smaller corrections come with this.
  [`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md)’s
  out-of-range `audio` message named an internal variable (`aud`) rather
  than the argument, and now names `audio`.
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  and
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
  reported a misspelled `direction` or `position` against their internal
  pipeline function, and now name themselves.

- Metadata values containing a newline no longer corrupt the probe
  output.
  [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
  and the `probe_*()` shortcuts read FFprobe’s per-stream output as one
  `key=value` pair per line, so a tag whose value spanned lines — a
  multi-line description or comment, most often — was truncated at the
  first line break and its remainder was read as further `key=value`
  pairs, adding invented columns to the `streams` tibble. Such a value
  now arrives whole, in one cell. Values containing `|` or a backslash
  are likewise returned unchanged. If you worked around this by dropping
  unexpected columns, that workaround is no longer needed.

  The commonest case in practice is a rotated video. FFprobe prints a
  stream’s display matrix across four lines, so `streams` gained three
  columns named after the matrix’s own rows while its `displaymatrix`
  cell sat empty. The matrix now arrives whole in that cell, and the
  `rotation` column beside it is unchanged.

- The compiled command string that every verb returns under
  `run = FALSE` — and that
  [`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
  produces — now wraps each stream map in double quotes: `-map "0:a:0"`
  where it used to print `-map 0:a:0`. Since the verbs began stating
  their stream selection explicitly, that string could carry a `?` (as
  in `-map 0:v?`, “this stream if the input has one”), and pasting it
  into a shell failed there rather than running: zsh reads a bare `?` as
  a filename pattern and answers `no matches found`. The command
  tidymedia itself runs is unchanged — it never goes through a shell —
  so this affects only what you read, log, and paste. If you compare
  compiled commands against saved strings, those strings need updating.

- [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and
  [`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md)
  no longer fail on an input with more than one audio track. They mapped
  *every* audio stream into the output, so a file carrying several
  tracks — a recording with separate per-speaker or per-language audio,
  say — handed several streams to a format that accepts only one. FFmpeg
  aborted (`Exactly one MP3 audio stream is required`) and left a
  zero-byte file behind. Both verbs now take the input’s first audio
  track, which is what their documentation always described and what a
  single-track file always did. Single-track inputs are unaffected. To
  choose a track other than the first, see the new `audio_stream`
  argument below.

### New features

- [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  and their `_batch` siblings gain an `audio_stream` argument: the
  0-based index of the audio track to carry, counted among the input’s
  audio streams, so `0` is the first audio track whatever its position
  among the file’s streams. Leaving it unset keeps every track. The
  `_batch` verbs also accept an `audio_stream` column in `jobs` to
  choose per row, where `NA` in a cell keeps that row’s tracks all.

  Note that `NULL` does not mean the same thing across the package:
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  and
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  take the **first** track when you leave `audio_stream` unset, because
  they write exactly one audio stream and have to pick. The verbs that
  pass audio through —
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  and now these two — keep them all. Each function’s documentation says
  which it does and names the ones that do the other.

  Naming a track the input does not have is an FFmpeg error rather than
  an R one, unchanged from the other verbs that take this argument. An
  input with no audio at all is fine, and so is a video-only or
  audio-only file.

- [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  and
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  (and their `_batch` siblings) now warn when the file they read carries
  audio tracks the file they write will not. Each of these verbs takes
  exactly one track, so feeding a three-track recording to
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  without saying which track you want quietly discarded two of them. It
  now says so, tells you how many went, and points at `audio_stream` for
  choosing a different one. Name a track and the warning stops; suppress
  it by class with
  `suppressWarnings(classes = "tidymedia_dropped_audio")`.

  The batch verbs warn **once** for the whole table, naming every
  affected row, rather than once per row.

  The message also spells out a trap worth knowing about:
  [`probe_audio()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)’s
  `index` column counts *all* of a file’s streams, while `audio_stream`
  counts only its audio streams. On a video file with three audio tracks
  those read `1, 2, 3` and `0, 1, 2` respectively, so reading a number
  off
  [`probe_audio()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)
  and passing it straight to `audio_stream` lands you one track off.

  Counting the tracks means running FFprobe, so the check is
  **best-effort**: it is made when FFprobe is available and the input
  can be probed, and skipped silently otherwise. It never runs under
  `run = FALSE` — compiling a command still touches no binary — and it
  never changes the command that gets compiled.

- [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and their `_batch` siblings gain an `audio_stream` argument for
  choosing which audio track to take from a file that carries several —
  a recording with separate per-speaker or per-language tracks, say. It
  is a 0-based index counted among the input’s audio streams, so
  `audio_stream = 1` takes the second audio track whatever its position
  among the file’s streams; the default takes the first.

  In a jobs table, `audio_stream` may be a per-row column, which
  overrides the argument row by row. `NA` in a cell keeps that row on
  the first audio track, the per-row form of leaving the argument unset.

  Asking for a track the input does not have is an FFmpeg error, not an
  R one: the compiled command is still what you asked for, and FFmpeg
  reports that the stream map matches no streams.

- [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  and
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md)
  gain an `audio_stream` argument for writing one audio track instead of
  all of them. Like the argument of the same name on
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  and
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
  it is a 0-based index counted among the input’s audio streams, so
  `audio_stream = 1` writes the second audio track whatever its position
  among the file’s streams. Only the audio output is affected — the
  video file always takes the input’s video.

  **The default is different on these two verbs, deliberately.** Leaving
  `audio_stream` unset keeps **every** audio track, which is what they
  have always done, rather than the first track
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  and
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  take. An audio container that holds several streams — Matroska
  (`.mka`) or `.m4a` — therefore still receives all of them, and no
  working call changes. The two families differ because they answer
  different questions: an extraction verb writes one track by
  construction, while a separation verb writes whatever your container
  can hold.

  In a jobs table, `audio_stream` may be a per-row column, which
  overrides the argument row by row; `NA` in a cell keeps that row on
  every audio track, the per-row form of leaving the argument unset.

- [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  now explains itself when FFmpeg refuses your audio file because the
  input carries several audio tracks. Most audio containers (`.aac`,
  `.mp3`, `.wav`) hold exactly one stream, so separating a three-track
  recording into one of them failed with FFmpeg’s own message and a
  zero-byte file — with nothing to say that the track count was the
  problem, or that there was any way around it. The error now states how
  many tracks the input carries and names both ways out: `audio_stream`
  to write one of them, or a container such as `.mka` or `.m4a` to keep
  them all.

  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md)
  cannot abort one row without abandoning the rest of the table, so it
  still records that row as `success = FALSE` and warns **once** when
  the batch finishes, naming every affected input row. Suppress it with
  `suppressWarnings(classes = "tidymedia_multitrack_separation")`.

  Counting the tracks means running FFprobe, so the explanation is
  **best-effort**: you get it when FFprobe is available and the input
  can be probed, and FFmpeg’s own error otherwise. The probe runs only
  after FFmpeg has already failed, only on a real run (never under
  `run = FALSE`), and never changes the command that gets compiled.
  Naming a track skips it entirely — with one track mapped, a failure is
  something else and a track count would not explain it.

- `NULL` now means the same thing on every codec argument in the
  package, and `NA` means the same thing in every per-row codec column.
  `audio_codec = NULL` or `video_codec = NULL` emits no `-codec:a` /
  `-codec:v` at all, leaving the encoder to the output container; `NA`
  in a jobs-table codec column is the per-row form of that same `NULL`.
  Three places disagreed:

  - [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
    and
    [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md)
    refused `video_codec = NULL`, while
    [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
    next door accepted it. Both now accept it — it is how you opt out of
    the `"libx264"` default when the output container is not an H.264
    one, such as `.webm`.
  - [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
    refused `audio_codec = NULL`, while
    [`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md)
    has always accepted the same call. The scalar verb now accepts it
    too.
  - The `video_codec` columns of
    [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
    and
    [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
    and the `audio_codec` column of
    [`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
    rejected `NA` — so a jobs table could not leave one row’s codec
    unset the way every other codec column already could. All three now
    accept it, including in a mixed column where some rows name an
    encoder and others do not.

  No existing command changes. A call passing neither `NULL` nor a
  column `NA` compiles exactly what it compiled before; the calls that
  changed are ones that used to abort and now compile. A *scalar* `NA`
  is still an error everywhere: `NA` spells “unset” only as a column
  cell, where a per-row table has no other way to say it.

  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and
  [`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md)
  stay the deliberate exception — `NULL` and a column `NA` there select
  `-q:a 0`, highest VBR quality, as they always have and as their
  documentation says. `pixel_format` and `color` columns still reject
  `NA`, having no unset state to spell.

  Three error messages changed along the way, all on calls that aborted
  before and still abort:

  - A non-character `video_codec` / `audio_codec` column now reports
    “must be character (`NA` to leave the codec unset)” instead of “must
    be character (no `NA`)”, on
    [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
    [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md)
    and
    [`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md)
    — the message every other codec column already gave.
  - A bad `video_codec` value passed to
    [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
    [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md)
    or
    [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
    now says it “must be a single string or `NULL`”, where it used to
    say only “a single string”. `NULL` is legal on those arguments as of
    this release, so the old wording had become untrue.
  - On
    [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
    and
    [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
    a jobs table invalid in *both* its `video_codec` column and its
    `pixel_format` column now reports the `pixel_format` problem first;
    it reported `video_codec` first before. Only the reporting order
    changed — both columns are still rejected.
    ([`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md)’s
    `color`-before-`video_codec` order is unchanged.)

- [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
  and
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  (and their `_batch` siblings) gain an `audio_codec` argument. Both
  verbs re-encode video and stream-copy audio, but the copy was fixed in
  place, so there was no way to say otherwise — which mattered because a
  stream copy fails outright when the output container cannot hold the
  source audio codec, and the documented remedy for that (“name an
  encoder”) had no argument to name one. `audio_codec = "copy"` is the
  default and compiles exactly the commands these verbs compiled before,
  so calls that name their arguments (or take the defaults) produce
  identical output; `audio_codec = "aac"` transcodes the audio instead;
  `audio_codec = NULL` emits no audio codec at all and lets the output
  container choose.

  The new argument sits beside `video_codec` rather than at the end, so
  the arguments after it have all shifted one position: **calls that
  pass `pixel_format`, `hardware`, `fallback`, or `run` by position
  rather than by name must be updated.**
  `standardize_video(f, out, 1280, 720, 30, "libx264", "yuv420p")` now
  reads `"yuv420p"` as the audio codec, not the pixel format. In line
  with this package’s pre-1.0 clean-break policy the argument is placed
  where it belongs rather than appended for compatibility; naming your
  arguments avoids the problem entirely.

  In a jobs table, `audio_codec` may be a per-row column where `NA`
  means “leave that row’s codec unset”. `hardware` remains batch-wide
  and applies to video only — audio is never hardware-accelerated.

- [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  and
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md)
  gain the `hardware` and `fallback` arguments the other re-encoding
  verbs already carry, so a video stream that is being re-encoded on the
  way out can be encoded on an NVIDIA GPU: `hardware = "nvenc"`. Only
  the video output is affected — nvenc encodes video, so the audio file
  is byte-for-byte what it would have been otherwise, whatever you pass.

  Because this verb copies the video by default, and a copy runs no
  encoder at all, `hardware = "nvenc"` on its own is an error rather
  than a silent switch from a lossless copy to a GPU re-encode. Pair it
  with `video_codec = NULL`, which assumes the H.264 family, or name a
  codec (`video_codec = "libx265"`) to pin a different one — a non-H.264
  container such as `.webm` needs that explicit name. As on the other
  verbs, `hardware` applies to a whole batch rather than row by row, so
  a jobs table mixing copied and re-encoded video must be split into
  separate calls.

- [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  and
  [`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md)
  gain an `audio_codec` argument naming the output audio encoder.
  Loudness normalization filters the audio, so it must be re-encoded —
  and until now it was re-encoded to whatever encoder your FFmpeg build
  defaults to for the output container, which made the result depend on
  the machine. `audio_codec = "aac"` (say) pins it. The default `NULL`
  leaves the codec unset, so existing calls compile exactly the commands
  they did before, and `"copy"` is an error, since a filtered stream
  cannot be copied. In a jobs table, `audio_codec` may be a per-row
  column (`NA` means “leave it unset”), and it applies to the two-pass
  path as well.

- [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
  and
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
  (and their `_batch` siblings) gain a `video_codec` argument, alongside
  the `hardware`/`fallback` GPU toggle. The default `video_codec = NULL`
  leaves the codec unset, so these verbs compile exactly the commands
  they did before and each output keeps its container’s default encoder.
  In a jobs table, `video_codec` may be a per-row column (`NA` means
  “leave it unset”); `hardware` and `fallback` apply to the whole batch.
  Naming a codec (or a hardware backend) while cutting with
  `segment_video(reencode = FALSE)` is an error — a stream copy runs no
  encoder.

- Opt-in NVIDIA GPU (nvenc) video encoding.
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
  and
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  (and their `_batch` siblings) gain a `hardware` argument:
  `hardware = "nvenc"` re-encodes on the GPU, choosing the nvenc encoder
  for the codec family (e.g. `h264_nvenc`). By default an unavailable
  GPU is an error (so output stays reproducible); `fallback = TRUE`
  re-encodes in software with a message instead. `has_nvenc()` reports
  whether an nvenc encoder is available in your FFmpeg build and
  `nvenc_encoder()` names it. Hardware *decoding* and GPU filter
  pipelines remain out of scope — use
  [`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
  for those.

- [`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md)
  samples a video at a fixed rate (`fps`) or interval (`interval`,
  seconds between frames) into a numbered image sequence — the front
  door to per-frame coding and computer-vision feature pipelines.
  [`sample_frames_batch()`](https://jmgirard.github.io/tidymedia/reference/sample_frames_batch.md)
  does the same across many videos from a jobs table.

- Batch (`_batch`) siblings for the remaining single-input transform
  verbs:
  [`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
  [`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md),
  [`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md),
  and
  [`format_for_web_batch()`](https://jmgirard.github.io/tidymedia/reference/format_for_web_batch.md)
  process many files from one jobs table, each a thin wrapper over
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).
  The audio verbs require an `output` column; the video verbs auto-name
  outputs (`_cropped`, `_web.mp4`) when it is absent, and all four
  reject two rows that resolve to the same output path.

### Standardized function and argument names

The public API was renamed to a single, predictable scheme. These are
breaking changes with no deprecation shims (the package is still pre-1.0
and soaking).

- **Batch verbs now use a `_batch` suffix** instead of a plural noun:
  `segment_videos()` →
  [`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
  `standardize_videos()` →
  [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
  `normalize_audios()` →
  [`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
  `anonymize_videos()` →
  [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
  and `extract_frames()` →
  [`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md)
  (which also removes the confusion with grabbing “many frames” from one
  video).
- **FFmpeg capability queries moved out of the `get_*` namespace:**
  `get_codecs()` →
  [`ffmpeg_codecs()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_codecs.md)
  and `get_encoders()` →
  [`ffmpeg_encoders()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md).
  `get_*` is now reserved for per-file metadata getters.
- **`audio_as_mp3()` is now
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)**,
  with a new `format` argument. The default (`format = NULL`) reproduces
  the old behavior exactly (the output format follows the file
  extension); pass `format` to pin the audio codec.
- **Metadata getters renamed** to match the argument vocabulary:
  `get_samplingrate()` →
  [`get_sample_rate()`](https://jmgirard.github.io/tidymedia/reference/get_sample_rate.md)
  and `get_framerate()` →
  [`get_frame_rate()`](https://jmgirard.github.io/tidymedia/reference/get_frame_rate.md).
- **Codec and time-bound arguments harmonized:** `acodec`/`vcodec` (and
  the matching jobs-table columns) are now `audio_codec`/`video_codec`,
  and
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)’s
  `ts_start`/`ts_stop` are now `start`/`end` (matching the batch
  columns).
- **Removed unintended exports:** the unused tidy-eval reexports
  (`enquo()`, `enquos()`, `as_label()`, `as_name()`, `:=`) and two
  internal helpers (`pad_integers()`, `convert_fractions()`) are no
  longer exported. `.data` remains reexported.

### Documentation

- The package has a landing help topic:
  [`?tidymedia`](https://jmgirard.github.io/tidymedia/reference/tidymedia-package.md)
  now resolves to an overview of the three layers and the vignettes, and
  the topic is listed by
  [`help(package = "tidymedia")`](https://jmgirard.github.io/tidymedia/reference)
  and on the reference index. Previously neither reached anything.

- The batch vignette’s account of `parallel = TRUE` now names the
  functions that actually take it —
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  every `*_batch` verb,
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
  and the five `probe_*()` readers — and says that the scalar verbs do
  not. It previously said “the fan-out verbs”, which read as covering
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  (only its `_batch` sibling takes the argument) while omitting the
  metadata readers and
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  entirely. The metadata vignette’s batching section, which was silent
  about the argument, now covers it too.

- Every verb taking `hardware` now says that asking for `"nvenc"`
  queries your FFmpeg build for the encoder while the command is being
  assembled, so a call that re-encodes the video runs the binary even
  with `run = FALSE`. Asking for `"nvenc"` alongside a stream copy is an
  error those pages already describe —
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  at its default `video_codec = "copy"`,
  `segment_video(reencode = FALSE)`, and both `_batch` siblings — and it
  is caught first, so such a call aborts without probing. This was
  always true; only the documentation is new. `run = FALSE` promises you
  the command that would run, not a call that touches nothing.

- New
  [`?audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md)
  help page explains the two 0-based audio arguments the package exposes
  and how they differ: `audio_stream` counts one input’s audio tracks,
  while `audio` on
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  and
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
  counts the verb’s inputs, so neither index can be read off the other.
  It also covers what leaving each unset means (the extraction verbs
  take the first track, the pass-through verbs keep every track, and an
  unset `audio` drops audio altogether), what an `NA` cell means in a
  `_batch` jobs column, and the two unrelated things `audio` names on
  [`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md)
  and
  [`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md).
  Every verb taking either argument now links to it, and the
  getting-started vignette gains a section on choosing an audio track.

- Corrected several `audio_stream` help pages whose descriptions still
  listed only some of the verbs that keep every audio track, omitting
  ones added later. Those lists are now generated from a single source,
  so they cannot fall behind the code again.

- Help pages now cross-reference each other: every task verb links to
  the `ffm_*` pipeline builders it is built on (and each builder back to
  the verbs that use it), and the three metadata reader families
  (`probe_*()`, `mediainfo_*()`, `get_*()`) link to one another so you
  can find the alternative backend.

- Each metadata help page now states its backend (FFprobe or MediaInfo)
  and what it returns (a tibble, a value, or a single scalar per file),
  and the “Media metadata as tibbles” vignette gains a table comparing
  the reader families at a glance.

- New “A research preprocessing workflow” vignette walks an end-to-end
  pipeline — standardizing recordings, normalizing and extracting audio,
  sampling frames, de-identifying, and packaging for sharing —
  demonstrating the task verbs on a realistic dyadic-interaction study.
  The “Get started” vignette now leads with the task verbs (the front
  door most users need) before descending to the builder, and every
  vignette cross-links to the others.

### Fixed-region anonymization

- New
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  covers one or more fixed rectangular regions of a video with opaque
  filled boxes — for redacting a face, a name badge, or any area that
  stays in one place for the whole clip (there is no motion tracking).
  Regions are given as a data frame of `x`, `y`, `width`, `height`
  (numbers or FFmpeg expressions), with an optional per-row `color`. The
  video is re-encoded reproducibly (H.264 / `yuv420p` by default) and
  audio is stream-copied unchanged.
- New `anonymize_videos()` applies the same box-fill redaction across
  many videos from one jobs tibble — each row names an `input` and
  carries its own `regions` (a list-column of boxes data frames), with
  optional per-row `output`, `color`, `vcodec`, and `pixel_format`
  columns. Like the other table-driven verbs it is a thin wrapper over
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  returning one reproducible command per input and supporting `verify`,
  `manifest`, and parallel execution.

### Graceful handling of silent input in two-pass loudnorm

- Two-pass normalization now handles digitally silent input honestly.
  Silence measures as `-inf` loudness, which cannot be normalized to a
  target. Previously this surfaced as a misleading “could not parse the
  loudnorm measurement” error. Now `normalize_audio(two_pass = TRUE)`
  aborts with a clear message that names silence as the cause, and
  `normalize_audios(two_pass = TRUE)` no longer lets one silent row
  abort the whole batch: the non-silent rows are normalized, the silent
  rows are marked in a new logical `silent` column (with
  `success = FALSE` and no output written), and a warning names them.
  Genuine analysis failures still abort fail-fast. (Near-silent but
  non-empty audio is unaffected.)
- The two-pass batch’s result schema no longer depends on how many rows
  are silent: when `verify` or `manifest` is requested, the `verified`
  column and the provenance manifest are now returned even when *every*
  row is silent (silent rows carry `NA` for those outputs), matching a
  batch with some non-silent rows.

### Accurate two-pass loudness normalization

- [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  gained `two_pass`. With `two_pass = TRUE` it runs an analysis pass to
  measure the input’s loudness, then a linear correction pass that feeds
  those measurements back, hitting the EBU R128 target far more
  precisely than the single-pass default on material with a wide
  loudness range. Because it must measure the input, two-pass always
  calls FFmpeg — even under `run = FALSE`, where the analysis still runs
  and the returned value is the exact correction command, left
  unexecuted. The single-pass default is unchanged and stays binary-free
  under `run = FALSE`.
- `normalize_audios()` gained `two_pass` too, applying the same accurate
  measured/linear normalization across a whole jobs table. With
  `two_pass = TRUE` it measures every input (honoring `parallel` and
  each row’s targets), then builds and runs one linear correction per
  row, surfacing the five measured values as
  `measured_I`/`measured_TP`/`measured_LRA`/ `measured_thresh`/`offset`
  columns. As with the scalar verb the analysis pass always runs — even
  under `run = FALSE`, which then gates only the correction pass — and a
  row whose analysis yields no usable measurement aborts the call,
  naming the offending row. `two_pass` is a whole-table switch, not a
  per-row column. The single-pass default is unchanged.

### Audio loudness normalization

- Added
  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md),
  a task verb that normalizes a file’s perceived loudness to an EBU R128
  target using FFmpeg’s single-pass `loudnorm` filter. By default it
  targets -23 LUFS integrated loudness with a -1 dBTP true-peak ceiling
  (EBU R128, measured per ITU-R BS.1770-4), copies the video stream
  unchanged, and preserves the source channel layout, so the same input
  always yields one reproducible command. Supply `target_loudness`,
  `true_peak`, and `loudness_range` to retarget, and
  `channels`/`sample_rate` to downmix or resample the audio. Note that
  single-pass `loudnorm` resamples its output (up to 192 kHz,
  encoder-capped), so set `sample_rate` to pin the output rate.
- Added
  [`ffm_loudnorm()`](https://jmgirard.github.io/tidymedia/reference/ffm_loudnorm.md),
  a builder that appends FFmpeg’s EBU R128 `loudnorm` audio filter to a
  pipeline — the first builder to write the audio filter chain (`-af`).

### Batch audio normalization across files

- Added `normalize_audios()`, a table-driven companion to
  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md).
  Pass a jobs tibble with one row per input (only an `input` column is
  required) to loudness-normalize many files in one call, each to an EBU
  R128 target. It returns the tibble plus one reproducible `command` per
  row. The five loudness knobs — `target_loudness`, `true_peak`,
  `loudness_range`, `channels`, and `sample_rate` — may each appear as a
  column to vary per row, and outputs are auto-named
  `<base>_normalized.<ext>` when no `output` column is given. It is a
  thin wrapper over
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  so `...` forwards batch options such as `verify`, `manifest`,
  `checksums`, `progress`, and `parallel`.

### Video standardization

- Added
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  a task verb that re-encodes a video to a reproducible,
  analysis-friendly format in one call. By default it produces H.264
  video with `yuv420p` and `+faststart`, stream-copies the audio
  unchanged, and keeps the source resolution and frame rate (rounding
  odd dimensions down to the nearest even value so the codec can
  encode), so the same input always yields a byte-identical command.
  Supply `width`/`height` to set the output size (giving only one
  preserves the aspect ratio with an even output dimension), `fps` to
  resample the frame rate, and `vcodec`/`pixel_format` to override the
  codec or pixel format.
- Added
  [`ffm_fps()`](https://jmgirard.github.io/tidymedia/reference/ffm_fps.md),
  a builder that appends an `fps` filter to a pipeline, accepting either
  a number of frames per second or an FFmpeg framerate expression such
  as `"30000/1001"`.

### Batch standardization across files

- Added `standardize_videos()`, a table-driven companion to
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md).
  Pass a jobs tibble with an `input` column — one row per video — to
  re-encode many files to a reproducible format in one call. It is a
  thin wrapper over
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  so `...` forwards batch options such as `verify`, `manifest`,
  `checksums`, and `progress`, and each row compiles to a command
  byte-identical to the equivalent
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
  call. Any of `width`, `height`, `fps`, `vcodec`, and `pixel_format`
  may appear as a column to override that setting per row, otherwise the
  function argument applies to every row. The `output` column is
  optional: when absent, names are derived per input as
  `<basename>_standardized.<ext>` (keeping the source extension), and a
  duplicated `input` with no `output` column is rejected rather than
  silently overwritten.

### Frame extraction across files

- Added `extract_frames()`, a table-driven companion to
  [`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md).
  Pass a jobs tibble with an `input` column and exactly one of a
  `timestamp` or `frame` column — one row per frame — to grab still
  images spanning many input files in one call. It is a thin wrapper
  over
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  so `...` forwards batch options such as `verify`, `manifest`,
  `checksums`, and `progress`. The `output` column is optional: when
  absent, names are derived per input file as `<basename>_<n>.<format>`
  (default `format = "png"`), the frame number restarting for each
  input.

### Bug fixes

- `normalize_audio_batch(audio_codec = NA)` now aborts instead of
  quietly compiling the default command. A scalar `NA` was resolved the
  same way as an `NA` cell in a jobs-table column — where it
  legitimately means “leave this row’s codec unset” — so an accidental
  `NA` argument produced a command with no `-codec:a` and no indication
  that anything had been ignored.

- Every `video_codec` and `audio_codec` argument now reports a bad value
  against the argument and the verb you actually called. Several
  previously blamed an internal helper, named FFmpeg’s own `video` /
  `audio` parameter instead of the argument you passed, or — on the
  `_batch` verbs — surfaced the complaint from inside the row loop with
  an `In index: 1` prefix, as though one row’s data were at fault rather
  than a whole-table argument. Affected
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
  [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
  [`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
  and
  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md).

- A bad `video_codec` / `audio_codec` **argument** on a `_batch` verb is
  now refused even when `jobs` carries a column of the same name. The
  column takes precedence over the argument, so a non-string value
  passed as the argument used to be discarded in silence;
  [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
  [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
  [`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md)
  and
  [`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md)
  now report it, matching
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
  which already refused it. Values these verbs *accept* are unchanged —
  a codec string, and `NULL` where it was already legal, behave exactly
  as before.

- One knock-on for
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md):
  a call that passes both a bad `video_codec` and an invalid `width` /
  `height` / `fps` now reports the codec problem first, where it
  previously reported the dimension problem. Both complaints are real
  and fixing the codec argument reveals the other; no value that was
  accepted before is refused now. The other verbs keep their previous
  ordering.

- [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  (and the `parallel = TRUE` path of
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  / `segment_videos()`) now warns when parallel processing is requested
  but no parallel
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  is active. Previously such calls ran one job at a time with no speedup
  and no indication; the warning points to
  `future::plan(future::multisession)`.

### Batch segmentation across files

- Added `segment_videos()`, a table-driven companion to
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md).
  Pass a jobs tibble with `input`, `output`, `start`, and `end` columns
  — one row per segment — to cut segments spanning many input files in
  one call. It is a thin wrapper over
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  so `...` forwards batch options such as `verify`, `manifest`,
  `checksums`, and `progress`; `reencode` selects accurate re-encoding
  (default) or the fast keyframe-snapping copy path, as in
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md).
- `segment_videos()` now reaches full parity with
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md):
  the `output` column is optional (when absent, names are derived per
  input file as `<basename>_<n>.<ext>`, numbering restarting for each
  input), a per-row `reencode` column overrides the scalar `reencode`
  argument, and non-numeric/character `start`/`end` (or a non-logical
  `reencode`) columns are rejected with a clear error instead of an
  opaque FFmpeg failure.

### Verification & provenance

- Added
  [`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md),
  a probe-backed checker that confirms an output really has the
  properties you asked for. It returns a tidy tibble with one row per
  check (`file`, `check`, `expected`, `actual`, `pass`) covering
  `duration`, `width`, `height`, `video_codec`, `audio_codec`, and
  `sample_rate`, plus any other FFprobe field passed by name through
  `...`. Numeric checks use an absolute `tolerance` (default `0.1`, so
  integer dimensions match exactly while duration gets a little slack);
  codec checks match exactly.
- Verification is wired into execution. `ffm_run(verify = <named list>)`
  probes the output after a successful run and aborts, listing the
  failed checks, if any assertion fails.
  `ffm_batch(verify = <list or function>)` instead records the outcome
  in a logical `verified` column (one spec for all jobs, or a
  `pmap`-style function of the job columns) without aborting.
- Added a batch provenance manifest. `ffm_batch(manifest = TRUE)`
  attaches a per-job record — command, FFmpeg/FFprobe versions,
  timestamp, and output size — read back with
  [`ffm_manifest()`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md),
  which can also write it to CSV via `path =`. `checksums = TRUE`
  additionally records input/output md5 checksums.
- `ffm_batch(progress = TRUE)` shows a `cli` progress bar as the jobs
  run (following the `future` plan on the parallel path).

### Multi-input verbs

- Completed the blessed multi-input builder set with
  [`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md)
  (stack videos top to bottom, the vertical companion to
  [`ffm_hstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md))
  and
  [`ffm_overlay()`](https://jmgirard.github.io/tidymedia/reference/ffm_overlay.md)
  (composite one video over another at an `x`/`y` position given as
  pixels or an FFmpeg expression).
  [`ffm_overlay()`](https://jmgirard.github.io/tidymedia/reference/ffm_overlay.md)
  also takes an optional `scale` to resize the overlay to a fraction of
  the main video’s width.
- Added two research task verbs built on these:
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  for a side-by-side or stacked comparison video, and
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
  for an inset overlay (corner or center `position`, `scale`, `margin`).
  Both drop audio by default; pass `audio =` an input index to carry
  that track.

### Safe execution

- Pipelines are now executed as argument vectors (via
  [`system2()`](https://rdrr.io/r/base/system2.html)), never through a
  shell string, so input and output paths containing spaces, quotes,
  `$`, or backticks are handled correctly. This applies to
  [`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  and every task verb;
  [`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
  still returns the same reproducible command string. The Layer 0 escape
  hatches
  ([`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md),
  [`ffprobe()`](https://jmgirard.github.io/tidymedia/reference/ffprobe.md),
  [`mediainfo()`](https://jmgirard.github.io/tidymedia/reference/mediainfo.md))
  keep their raw-string interface.
- Raw output options added with
  [`ffm_output_options()`](https://jmgirard.github.io/tidymedia/reference/ffm_output_options.md)
  are tokenized on whitespace at execution time; option values
  themselves must not contain spaces (they never worked reliably
  before).

### Breaking changes

- [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  now stream-copies by default — separation is lossless and fast, but
  each output container must support the source codec. Use the new
  `reencode = TRUE` argument for the previous re-encoding behavior.
- [`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md)
  and
  [`ffm_pixel_format()`](https://jmgirard.github.io/tidymedia/reference/ffm_pixel_format.md)
  now reject values that are not a single clean token (no whitespace or
  shell metacharacters, and starting with a letter or digit).
- [`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md)
  — and every task verb built on it — now aborts with FFmpeg’s exit
  status when an encode fails, instead of returning silently (the old
  shell path only emitted a warning).
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  still records failures in its `success` column without aborting.
- [`ffm_output_options()`](https://jmgirard.github.io/tidymedia/reference/ffm_output_options.md)
  now rejects option groups containing quote characters: options are
  split on whitespace into arguments at execution, so quoting cannot
  group tokens (previously such commands executed with a different
  meaning than printed).

### Bug fixes

- An explicit
  [`ffm_map()`](https://jmgirard.github.io/tidymedia/reference/ffm_map.md)
  on a multi-input pipeline
  (e.g. [`ffm_hstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md))
  is now emitted alongside the automatic `-map "[vout]"` instead of
  being silently ignored, so e.g. `ffm_map(p, "0:a")` keeps the first
  input’s audio next to the stacked video.
- Test coverage is measured again: an empty `R/zzz.R` triggered a `covr`
  bug that silently reported 0% package coverage.

## tidymedia 0.1.0

First tagged release, bringing the metadata, builder, and task-verb work
of the 0.0.0.900x development series to a documented, release-ready
state.

### Documentation

- Every exported function now carries a worked example and an
  architecture-layer `@family` tag, and there is a [pkgdown
  site](https://jmgirard.github.io/tidymedia/) whose reference index is
  grouped by the three layers (escape hatch, builder, task verbs).
- Added three vignettes: *Get started* (building pipelines,
  [`vignette("tidymedia")`](https://jmgirard.github.io/tidymedia/articles/tidymedia.md)),
  *Media metadata as tibbles*
  ([`vignette("metadata")`](https://jmgirard.github.io/tidymedia/articles/metadata.md)),
  and *Batch processing*
  ([`vignette("batch")`](https://jmgirard.github.io/tidymedia/articles/batch.md)).
- A small sample clip now ships in `inst/extdata/sample.mp4` so examples
  and vignettes are runnable.

### Metadata layer

- The MediaInfo and FFprobe readers now **accept a vector of files** and
  return one stacked tibble keyed by a leading `file` column, so
  metadata for a whole batch is ready for `dplyr` joins and filters.
  This covers
  [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md),
  the `probe_*()` shortcuts,
  [`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md),
  [`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md),
  [`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md),
  and the `get_*()` convenience helpers.
- **Typed output is now the default.** Every reader gains a `typed`
  argument (default `TRUE`) that converts numeric columns to
  integers/doubles and turns missing markers (FFprobe’s `"N/A"`,
  MediaInfo’s empty values) into `NA`; fractions, ratios, hex
  identifiers, and text stay as strings. Pass `typed = FALSE` for the
  previous all-character behavior. This replaces
  [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)’s
  `convert` argument.
- Readers are **resilient to unreadable files**: a missing file, or one
  that cannot be probed, among several yields an all-`NA` row (or `NA`
  value) plus a warning, instead of aborting the whole call. Malformed
  *arguments* still abort.
- Arguments are now passed to the CLIs through argument vectors
  ([`system2()`](https://rdrr.io/r/base/system2.html)) rather than
  interpolated into a shell string, so file paths and MediaInfo
  `--Inform` templates containing spaces, quotes, `;`, `%`, or `$` work
  correctly. The Layer 0 escape hatches
  [`mediainfo()`](https://jmgirard.github.io/tidymedia/reference/mediainfo.md)
  /
  [`ffprobe()`](https://jmgirard.github.io/tidymedia/reference/ffprobe.md)
  keep their raw-string signatures.
- Output column schemas are unified: readers lead with a `file` column
  and the two built-in MediaInfo templates now emit snake_case column
  names. User-supplied names (`mediainfo_query(names =)`, custom
  template headers) are kept verbatim.

### Bug fixes

- [`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
  no longer lets FFmpeg read the calling process’s standard input, so
  running a pipeline (e.g. via
  [`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md))
  inside a script that is itself fed through `stdin` no longer swallows
  the rest of that input. Equivalent to FFmpeg’s `-nostdin`.
- [`probe_container()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
  [`probe_streams()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
  [`probe_video()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md),
  and
  [`probe_audio()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)
  now return the requested tibble when called with `infile =`; they
  previously returned `NULL`.
- `convert_fractions()` parses fractions directly instead of via
  `eval(parse())`, passes `NA` through, and errors on values that are
  neither a number nor a fraction.
- FFprobe’s `key=value` output is split on the first `=` only, so values
  that contain `=` are no longer truncated; the superseded
  `tidyr::separate()` call is gone. Files with zero streams no longer
  trip the stream loop.

## tidymedia 0.0.0.9002

### Batch processing

- Added
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  the tidymedia batch entry point: it maps a pipeline-building function
  over every row of a jobs data frame (columns are passed to the
  function by name,
  [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html)-style),
  compiles one reproducible command per job, and optionally runs them.
  It returns the jobs as a tibble with an added `command` column and,
  when run, a `success` column. Set `parallel = TRUE` to map with
  `furrr` following the active `future` plan.

### Task verbs rebuilt on the builder

- Every task verb is now a thin wrapper over the Layer 1 `ffm_*` builder
  and no longer assembles its own FFmpeg command string. Each gains a
  `run` argument and returns its compiled, reproducible command
  (invisibly when run):
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  `audio_as_mp3()`,
  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
  [`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md),
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md),
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
  and
  [`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md).
- [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  is now built on
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md):
  it returns the job tibble (one row per segment with its command and
  run status) and gains `reencode` and `parallel` arguments.
- **Cutting is frame-accurate by default.**
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  and the new
  [`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md)
  default to `reencode = TRUE`, which re-encodes so cuts land on the
  exact requested frames. `reencode = FALSE` selects a fast, lossless
  copy that snaps to the nearest keyframes (so the output duration may
  differ by up to one group-of-pictures). The previous copy-based
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  cut at the wrong point and shifted timestamps; that behavior is gone.
- Breaking:
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)’s
  free-form `options` string is replaced by an `acodec` argument;
  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md)
  drops its `arg` argument and now centers the crop by default;
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  returns a named vector of two commands;
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  returns a tibble.

### Pipeline engine

- Added
  [`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md),
  a seek-based cut using `-ss`/`-to` (distinct from the `trim` filter of
  [`ffm_trim()`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md)),
  so cuts can stream-copy. Accurate seeks output-seek and re-encode;
  fast copy seeks input-seek and add `-avoid_negative_ts`.
- Added
  [`ffm_concat()`](https://jmgirard.github.io/tidymedia/reference/ffm_concat.md),
  a blessed multi-input verb that concatenates the pipeline’s inputs via
  FFmpeg’s concat demuxer (fast, lossless, same-format).
- Added
  [`ffm_output_options()`](https://jmgirard.github.io/tidymedia/reference/ffm_output_options.md),
  a controlled passthrough for raw output options that
  [`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
  still positions and quotes.

## tidymedia 0.0.0.9001

### Pipeline engine

- Reworked the Layer 1 `ffm_*` builder onto a structured command model:
  [`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
  is now the single place that assembles, positions, and quotes every
  option. Single-input filter chains compile to `-vf`/`-af`; multi-input
  stacking compiles to a valid `-filter_complex` graph with explicit
  stream labels and an automatic `-map`.
- Fixed four builder bugs: `ffm_trim(setpts = FALSE)` no longer forces a
  `setpts` filter;
  [`ffm_drop()`](https://jmgirard.github.io/tidymedia/reference/ffm_drop.md)
  flags are now output options placed after the input (not before `-i`);
  [`ffm_pixel_format()`](https://jmgirard.github.io/tidymedia/reference/ffm_pixel_format.md)
  no longer runs into the output filename; and the previously invalid
  `-filter_complex:v` output is gone.
- [`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
  now errors early when a stream is set to codec `copy` while a filter
  targets that same stream, instead of failing cryptically in ffmpeg.
- [`ffm_hstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md)
  must be applied before other video filters and now produces a runnable
  command (verified end-to-end against ffmpeg).

### Infrastructure

- Added a testthat (3rd edition) test suite covering the `ffm_*`
  pipeline builder and
  [`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
  output, plus binary-gated tests for the ffmpeg/ffprobe/mediainfo task
  functions.
- Added GitHub Actions workflows for `R CMD check` (macOS, Windows,
  Linux) and test coverage; the Linux jobs install ffmpeg and mediainfo
  so execution tests run in CI.
- All input validation and user-facing messages now use rlang’s
  `check_*` helpers and cli
  ([`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  /
  [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html));
  the assertthat dependency has been removed. Added `dplyr`, `tidyr`,
  `purrr`, and `cli` to Imports (the first three were already used but
  undeclared).
- Enumerated arguments (e.g. `units`, `unit`, `section`, `template`,
  `program`) are now matched exactly via
  [`rlang::arg_match()`](https://rlang.r-lib.org/reference/arg_match.html)
  instead of the partial matching of
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html); pass the full
  value.

### Bug fixes

- [`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md)
  (and the helpers built on it:
  [`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md),
  `get_framerate()`,
  [`get_width()`](https://jmgirard.github.io/tidymedia/reference/get_width.md),
  [`get_height()`](https://jmgirard.github.io/tidymedia/reference/get_height.md),
  `get_samplingrate()`) now shell-quote the `--Inform` argument, so they
  work on POSIX shells where the `;` was previously parsed as a command
  separator.

## tidymedia 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
