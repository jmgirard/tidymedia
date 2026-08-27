# tidymedia (development version)

## New features

* A hung media program no longer blocks the R session indefinitely. Setting
  `options(tidymedia.timeout = 600)` gives every FFmpeg, FFprobe and MediaInfo
  process tidymedia starts a wall-clock limit in whole seconds. A reached limit
  is never silent: every call that can start one of those programs either
  aborts or warns. The task verbs, `ffm_run()` and the raw
  `ffmpeg()`/`ffprobe()`/`mediainfo()` hatches abort, naming the program and
  the limit; `verify_media()` aborts too, since a probe that never answered is
  not an answer. Everywhere one hung file must not discard the rest of the
  work, it warns instead. The metadata readers — `probe_all()` and the
  `probe_*()` accessors, `mediainfo_parameter()`, `mediainfo_query()`,
  `mediainfo_template()` and the `get_*()` helpers — give an `NA` row and one
  warning saying how many files timed out, so a single hung file does not
  discard a whole corpus. `ffm_batch()` and the `_batch` verbs mark the row
  `success = FALSE`, as they do for any failed job, and warn once at the end of
  the run saying how many jobs the limit killed. The dropped-track check behind
  `extract_audio()`, `convert_audio()`, `separate_audio_video()` and their
  `_batch` siblings warns that it could not check, and the provenance manifest
  warns that it could not read a version. Those two lists are not written from
  memory: a test derives the calls that can start one of these programs from
  the package's own call graph and drives a timeout through each of them.
  Where the call knows its own output — the task verbs and `ffm_run()` — any
  partial file the killed run had written is removed just as it is after any
  other failed run; the raw `ffmpeg()` escape hatch does not parse the argument
  string it is given, so it leaves the partial file in place. The default is
  `0`, meaning no limit, so existing code is unaffected —
  a legitimate multi-hour encode still runs to completion. The limit applies to
  each spawned program rather than to a batch as a whole, and tidymedia's own
  `parallel = TRUE` paths are bounded by the same limit as their sequential
  ones. The limit
  bounds the wait rather than promising the program dies at the second: R asks,
  insists after 20 seconds and kills after 40, so on Unix a program that does
  not answer can outlive its limit by up to 40 seconds, and R does not
  guarantee termination at all. See `?tidymedia`.

* A `parallel = TRUE` call now runs its workers under the tidymedia settings you
  set in your own session. Previously each worker started from its own empty
  option list, so `options(tidymedia.timeout = )` bounded a sequential batch and
  left the parallel one unbounded, and `options(tidymedia.nvenc_encoders = )`
  steered a sequential build while each worker ignored it and asked FFmpeg for
  its own encoder list. Both values are now carried into each worker for the
  duration of the call, and whatever that worker had set for itself is put back
  afterwards — including when the call fails. `ffm_batch()` also refuses a limit
  the underlying `timeout=` could not use — a fraction of a second, a negative
  number, `NA`, a string — before it dispatches any job, on both paths and
  whether or not it is going to run anything; that refusal used to arrive as an
  unexplained `success = FALSE` per row, or not at all. What is still not
  carried is the remembered answer about your FFmpeg build itself: a worker with
  no `tidymedia.nvenc_encoders` override still asks its own binary once. See
  `?tidymedia` and `?refresh_ffmpeg_capabilities`.

* `with_timeout(expr, seconds)` puts a wall-clock limit on one call without
  changing the limit the rest of your session runs under. Every FFmpeg, FFprobe
  and MediaInfo program started while `expr` is being evaluated is bounded by
  `seconds`, and when the call ends — by any route, a failure or a reached limit
  included — whatever the session had set before is back, an unset option
  included. It reaches a `parallel = TRUE` fan-out too, because the worker is
  handed the limit in force when the fan-out starts. `0` means no limit, so
  `with_timeout(expr, 0)` lifts a session-wide limit for one call; a value the
  underlying limit could not use — a fraction of a second, a negative number,
  `NA`, a string — is refused before `expr` runs, naming `seconds`. See
  `?with_timeout`.

* `local_timeout(seconds)` is the statement form of the same limit: it bounds
  the rest of the function you call it from, rather than an expression you wrap.
  Every FFmpeg, FFprobe and MediaInfo program started between the call and the
  end of that function is bounded by `seconds`, and when the function ends — by
  any route — whatever the caller had set before is back, an unset option
  included, unless that function discards the undo by writing an `on.exit()` of
  its own without `add = TRUE`. Two calls in one function stack the way any pair
  of `local_*()` calls does, and `seconds` is refused by the same rule
  `with_timeout()` uses. Reach for it when the thing to bound is the rest of a
  function body, or several calls that would be awkward to wrap together. See
  `?local_timeout`.

* tidymedia now imports **withr**, which `local_timeout()` uses to register its
  undo on the calling frame. It was already a suggested package; installing
  tidymedia now installs it too. withr itself depends on nothing outside base R.
  The declared minimum is withr 2.5.0, and that is the version it was tested
  against rather than merely the one written down: the whole `with_timeout()`
  and `local_timeout()` test suite passes on withr 2.5.0 and on 3.0.3 alike,
  and each behavior their documentation describes was re-measured on both and
  agrees. So an installation that resolves an older withr is running the
  behavior these pages describe.

* `with_timeout()` now refuses an omitted `expr` itself, saying which argument
  is missing, instead of letting R report a missing parameter of the function's
  own definition. Both of its arguments are now checked the same way, and
  neither refusal disturbs the session-wide limit.

## Breaking changes

* `format_for_web()` and `normalize_audio()` (and their `_batch` siblings) take
  a new `audio_stream` argument naming which audio track to work on, and now
  state that selection on every call. Both previously emitted no stream mapping
  at all, so FFmpeg picked for them — one stream of each type, preferring
  whichever audio track carries the container's "default" flag. On a
  three-track test file whose default flag sat on the third track, both verbs
  kept only that third track, in silence.

  The two verbs read an unset `audio_stream` differently, and the difference is
  deliberate. `format_for_web()` now keeps **every** audio track, matching
  `crop_video()`, `segment_video()`, `standardize_video()`,
  `anonymize_video()` and `separate_audio_video()`. If you re-encode multi-track
  sources for the web, your outputs will gain tracks they used to lose, and grow
  accordingly.

  `normalize_audio()` keeps the **first** audio track, matching
  `extract_audio()` and `convert_audio()`. That is not a narrowing: the verb
  already produced a single audio track, just an unpredictable one. It reads
  `NULL` this way because measuring loudness produces one measurement per audio
  track while the correction applies a single set of values, so normalizing
  several tracks at once would silently apply the first track's measurements to
  all of them. Under `two_pass = TRUE` the measurement pass now measures exactly
  the track the correction pass normalizes. Normalizing every track
  independently would need per-track filter settings the pipeline builder does
  not have, and is not offered.

  Naming a track the input does not have remains an FFmpeg error rather than an
  R one, on both verbs. Each argument's documentation says which family it
  belongs to.

  `normalize_audio()` now writes **one audio stream and no video**, whatever
  container you name for the output. It has become an audio-producing verb like
  `extract_audio()` and `convert_audio()`, rather than one that passes a video
  stream through. Two consequences worth reading before you upgrade:

  - **Normalizing a recording's loudness while keeping its picture is no longer
    possible in one call.** If you relied on `normalize_audio("clip.mp4",
    "clip_norm.mp4")` returning a playable video, it now returns an audio-only
    `.mp4`. Normalize to an audio file and mux it back with the `ffmpeg()`
    escape hatch; a first-class way to do this is on the roadmap.
  - **An input with no audio is now an error** rather than a silent copy of the
    video. A silent screen recording stops with FFmpeg's "Stream map '' matches
    no streams" instead of quietly producing a file with no normalized audio in
    it.

  What you gain is that the output container no longer decides whether the call
  works. `.wav`, `.mp3`, `.aac`, `.flac`, `.opus`, `.m4a`, `.mka`, `.oga`,
  `.w64` and the video containers all behave the same way now, where before the
  choice of extension could decide whether the call succeeded at all. (Anything
  FFmpeg itself cannot encode for is still an FFmpeg error — `.wma`, for one,
  which failed before this change too.)

* Two argument-surface changes on the four verbs that gained `audio_stream`,
  worth knowing if you call them tersely. The new argument sits before `run`, so
  **positional** calls that supplied `run` (or `parallel`) by position now bind
  it to the wrong argument — name your arguments, or move `run` along by one.
  And on `normalize_audio()` / `normalize_audio_batch()`, abbreviating
  `audio_codec` to `audio` no longer works: with `audio_stream` beside it, any
  prefix shorter than `audio_c` is ambiguous. Spell `audio_codec` out.

* `crop_video()` and `segment_video()` (and their `_batch` siblings) take a new
  `audio_stream` argument naming which audio track to carry, and now state that
  selection on every call. What that changes depends on which of them you use,
  because the two verbs did not start from the same place.

  `crop_video()` and `segment_video(reencode = FALSE)` mapped *every* stream
  before, so they already kept every audio track — that part is unchanged for
  them — but they dragged subtitle and data streams along and offered no way to
  pick a track. Subtitles are no longer carried: writing to `.mkv` used to pass
  one through and now passes none. That also fixes a real failure, since
  `crop_video()` writing a subtitle-bearing input to `.mp4` used to abort
  outright (FFmpeg has no default subtitle encoder for that container) and now
  succeeds.

  `segment_video()` with its **default** `reencode = TRUE` is the bigger change:
  it emitted no stream mapping at all, so FFmpeg picked for it — one stream of
  each type, preferring whichever audio track carries the container's "default"
  flag. On a three-track test file whose default flag sat on the second track,
  cutting a segment kept only that second track and discarded the other two in
  silence. That branch now keeps all three. If you cut segments from multi-track
  sources, your outputs will gain tracks they used to lose, and grow accordingly.

  `NULL`, the default, keeps every audio track, matching `standardize_video()`,
  `anonymize_video()` and `separate_audio_video()`. Note that `extract_audio()`
  and `convert_audio()` read `NULL` the other way — they take the first track —
  because their output *is* an audio stream and has to be one track. Each
  argument's documentation says which family it belongs to.

* `ffm_copy()` now **sets** the all-streams mapping rather than adding to it, so
  calling it twice no longer duplicates every output stream. Since the mapping
  builder began appending, `ffm_copy() |> ffm_copy()` compiled `-map 0` twice
  and a one-video/one-audio input came out with four streams;
  `ffm_concat() |> ffm_copy()` did the same, because concatenation copies
  internally. No pipeline built by a task verb was affected — this only reached
  you if you composed the builder yourself.

  If the pipeline already states a *different* mapping, `ffm_copy()` now stops
  with an error rather than discarding it silently. Pass `streams = FALSE` to
  keep the mapping you set, or call `ffm_copy()` first and narrow afterwards
  with `ffm_map(replace = TRUE)`. `ffm_map()` itself is unchanged and still
  appends.

* `standardize_video()` and `anonymize_video()` (and their `_batch` siblings)
  now keep **every** audio track from the input instead of letting FFmpeg pick
  one. Neither verb emitted a stream mapping before, so FFmpeg applied its own
  rules: one stream of each type, preferring whichever audio track carries the
  container's "default" flag. On a three-track file that meant two tracks were
  discarded in silence, and *which* one survived depended on the input's flags
  rather than on anything you wrote — on a test file whose default flag sat on
  the second track, the second track is what came out. Both verbs now state
  their selection on every call, and a new `audio_stream` argument names a
  single track when you want one.

  Two consequences worth knowing about. Output files from multi-track inputs
  will be larger, because they now carry tracks that were previously dropped.
  And subtitle streams are no longer carried: into a container that accepts
  them (`.mkv`) these verbs used to pass one subtitle through, and now pass
  none. Writing to `.mp4`, the common case, is unaffected — that container was
  already dropping them.

* `extract_audio()` now names the audio track it takes instead of leaving the
  choice to FFmpeg. It previously emitted no stream mapping at all, so FFmpeg
  picked a track by its own rules — which prefer whichever track carries the
  container's "default" flag. On a multi-track file that could be any track, and
  it could differ between FFmpeg versions on the same file, which is exactly the
  kind of invisible variation this package exists to remove. The verb now maps
  the input's **first** audio track unless you say otherwise with the new
  `audio_stream` argument.

  Single-track inputs are unaffected. On a multi-track input whose *second*
  track is flagged as the default, the extracted audio changes — you would have
  got that second track before and get the first one now. Pass
  `audio_stream = 1` to keep the old result on such a file.

  Extracting to a container that can hold subtitles (`.mkv`, say) also stops
  carrying a subtitle track through. The old command named no streams at all, so
  FFmpeg carried one stream of *each* type and `-vn` removed only the video;
  naming the audio stream takes audio alone. Extracting to an audio-only
  container such as `.aac`, `.m4a` or `.mka` is unaffected, because those never
  carried the subtitle track in the first place.

* `audio_stream` is inserted **before** `run` on `extract_audio()`,
  `convert_audio()`, `extract_audio_batch()` and `convert_audio_batch()`, so the
  arguments after it have all shifted one position: **calls that pass `run` (or
  `parallel` on the batch verbs) by position rather than by name must be
  updated.** `extract_audio(video, "audio.aac", "copy", FALSE)` now reads
  `FALSE` as the audio-stream index rather than as `run` — an error rather than
  a silent misread, since the index must be a whole number. In line with this
  package's pre-1.0 clean-break policy the argument is placed where it belongs
  rather than appended for compatibility; naming your arguments avoids the
  problem entirely.

* `audio_stream` is likewise inserted **before** `run` on
  `separate_audio_video()` and `separate_audio_video_batch()`, so `run` (and
  `parallel` on the batch verb) shifts one position there too: **calls passing
  them by position rather than by name must be updated.** As above, the argument
  is placed where it belongs rather than appended, in line with this package's
  pre-1.0 clean-break policy; naming your arguments avoids the problem.

* `ffm_map()` appends instead of overwriting. Calling it twice on the same
  pipeline used to discard the first mapping; it now keeps both, emitting one
  `-map` per mapping in the order given, which is what lets a pipeline keep the
  video and then name one audio track. `mapping` may now be a character vector
  for the same reason. Pass `replace = TRUE` to get the old
  discard-what-came-before behavior, which is how you narrow the all-streams
  mapping that `ffm_copy()` sets. No task verb's compiled command changes *as a
  result of this*: each sets its mapping once. Note that composing Layer-1
  builders that each set a mapping now accumulates them — `ffm_copy()` maps
  every stream, so calling it twice, or calling it after `ffm_concat()` (which
  calls it internally), emits `-map 0` twice and duplicates every stream in the
  output. That was a harmless no-op before; use `ffm_map(replace = TRUE)` to
  narrow instead.

* `convert_audio()` and `convert_audio_batch()` rename the `format` argument to
  `audio_codec`. The argument was always an audio codec — its own documentation
  said so, and its value has only ever been passed to FFmpeg's `-c:a` — so this
  brings the last of the codec arguments onto the package's `audio_codec` /
  `video_codec` naming, and every codec argument in the package is now spelled
  the same way.

  Only the name changes: `audio_codec = NULL` is still the default and still
  compiles `-q:a 0`, letting the output extension pick the codec at highest
  VBR quality, so existing default calls produce byte-identical commands.
  Note that `NULL` means something different here than on the other transform
  verbs, where it leaves the codec unset — on this verb it selects `-q:a 0`.

  `format` is removed rather than deprecated, in line with this package's
  pre-1.0 clean-break policy. Calls passing it to `convert_audio()` get R's
  usual `unused argument` error; `convert_audio_batch()`, whose `...` would
  otherwise ignore it in silence, aborts and names the replacement, as it does
  for a stale `format` column in a jobs table.

  In a jobs table, the per-row column is likewise now `audio_codec`, and it
  gains the ability to spell "unset": `NA` in a cell keeps that row on the
  `-q:a 0` default, which the old `format` column could not express.

* `separate_audio_video()` and `separate_audio_video_batch()` replace the
  `reencode` argument with per-stream `audio_codec` and `video_codec`
  arguments, so you can name the encoder for each output file instead of
  choosing between "copy everything" and "let the container decide everything".
  Both default to `"copy"`, which compiles exactly the commands
  `reencode = FALSE` compiled before; `audio_codec = NULL, video_codec = NULL`
  reproduces `reencode = TRUE`; and a codec name (`audio_codec = "libmp3lame"`)
  transcodes that stream alone. Each argument governs only its own output file.

  `reencode` is removed rather than deprecated, in line with this package's
  pre-1.0 clean-break policy. Calls passing it to `separate_audio_video()` get
  R's usual `unused argument` error; `separate_audio_video_batch()`, whose `...`
  would otherwise ignore it in silence, aborts and names the replacement.

  In a jobs table, `audio_codec` and `video_codec` may be per-row columns where
  `NA` means "leave that stream's codec unset". They replace the per-row
  `reencode` column. Because each input row fans out into an audio row and a
  video row, the returned table collapses the two into one `codec` column
  carrying each row's encoder for its own stream.

* `crop_video()`, `segment_video()`, `compare_videos()`, and
  `picture_in_picture()` (and their `_batch` siblings) no longer re-encode the
  audio they pass through. They now stream-copy it, matching what
  `standardize_video()` and `anonymize_video()` have always done: previously
  these four left the audio codec unset, so whatever encoder your FFmpeg build
  defaults to for the output container silently re-encoded the audio — a quality
  loss, and a result that depended on the machine. Their compiled commands
  therefore gain `-codec:a copy`.

  The new `audio_codec` argument controls this. `"copy"` is the default; name an
  encoder (e.g. `audio_codec = "aac"`) to transcode instead, or pass
  `audio_codec = NULL` for the old behavior of leaving the codec unset. Note
  that a stream copy fails if the output container cannot hold the source audio
  codec (FLAC in `.mp4`, say) — name an encoder in that case. In a jobs table,
  `audio_codec` may be a per-row column, where `NA` means "leave it unset".

  Cutting with `segment_video(reencode = FALSE)` copies every stream by
  definition, so any `audio_codec` other than `"copy"` is an error there, as is
  naming an audio encoder on a composite that carries no audio at all.

* `compare_videos_batch()` now rejects a wrongly typed `audio` column up front
  with a clear message instead of failing partway through the batch, and
  `picture_in_picture_batch()`'s equivalent check no longer accepts an all-`NA`
  column of the wrong type.

## Performance

* `hardware = "nvenc"` asks FFmpeg which encoders it has once per R session
  instead of once per call. Every such call previously started a separate
  FFmpeg process to re-read the same encoder list, so a 500-row nvenc batch
  paid 500 of them before encoding anything; now it pays one. The compiled
  commands are unchanged.

  The answer is remembered for the rest of the session, which matters if the
  build changes underneath you — a fresh FFmpeg install, a new GPU driver, a
  different binary. Two calls discard it: the new
  `refresh_ffmpeg_capabilities()`, and `set_program()` (or `set_ffmpeg()`),
  which discards it for you since it points tidymedia at a different binary.
  Setting `options(tidymedia.nvenc_encoders = )` still overrides the answer
  outright and is read before anything remembered, so it takes effect at once.

  `ffmpeg_encoders()` and `ffmpeg_codecs()` are never remembered: they query
  FFmpeg on every call and always report the build as it is now.

  What is remembered is per R process, so under `parallel = TRUE` each worker
  asks once rather than sharing the parent's answer. That is bounded by the
  worker count, not the row count.

* `probe_all()` and the `probe_*()` shortcuts take a new `parallel` argument
  (default `FALSE`). With `parallel = TRUE` the per-file probes are spread
  across workers with the optional **furrr** package, following whatever
  `future::plan()` is active — the same mechanism `ffm_batch()` already uses,
  so one plan configures both. The output is unchanged either way: the same
  tibbles, the same types, and rows in the order the input vector gave them.
  Files that cannot be probed still produce one warning at the end of the call
  naming all of them, not one per worker.

  Two things to know. `furrr` is looked for only when `parallel = TRUE`, so it
  stays an optional dependency for everyone else. And because the default
  `future` plan is sequential, `parallel = TRUE` on its own gives no speedup —
  it now says so with a warning rather than quietly doing nothing. Set a plan
  first, e.g. `future::plan(future::multisession)`.

* `probe_all()` and the `probe_*()` shortcuts now read each file with a **single**
  FFprobe process instead of one per stream plus one more for the container. A
  five-stream file needed six processes and needs one. The saving grows with
  stream count and with the number of files, so it is largest on exactly the
  batch work these functions exist for —
  locally, probing ten copies of a four-stream file went from 1.7 seconds to
  0.46. The returned tibbles keep the same columns, in the same order, with the
  same values and types — except for the invented columns described under Bug
  fixes below, which were never data in the first place.

## Bug fixes

* `normalize_audio()` and `normalize_audio_batch()` work again when the output
  is a FLAC (`.flac`) or Ogg Vorbis (`.oga`) file. On FFmpeg 9 these failed
  with "Could not open encoder before EOF" and left a zero-byte file: the
  loudness filter hands its output on in very long frames, which most encoders
  are re-framed out of but FLAC and Vorbis are not, and the frame was longer
  than FLAC will encode. Loudness normalization now re-chunks its output, so
  every audio container works. Commands built with `ffm_loudnorm()` carry the
  extra `asetnsamples` filter, which is visible in the compiled command string.

* A run that fails no longer leaves a broken output file behind. FFmpeg creates
  its output before it knows the command will work, so a refused encode left a
  zero-byte file sitting where a result should be — and if you were writing over
  an existing file, FFmpeg had already truncated that to zero on its way to
  failing. Every verb, and every row of a `_batch` verb, now deletes what the
  failed run wrote, and the error says so and names it.

  Only what the run wrote. Some failures — an unknown encoder, an unknown
  filter, a bad option value — are refused before FFmpeg opens the output at
  all, and a file already sitting at that path is then untouched. tidymedia
  checks the output's size and timestamp before the run and again after the
  failure, leaves such a file exactly as it was, and says that instead. A file
  whose name contains `*`, `?` or `[` is deleted as the name it is, never as a
  pattern, so a neighboring file is never taken with it.

  `overwrite = FALSE` against a file that was already there keeps its own
  guarantee: FFmpeg was told not to replace it, so neither will tidymedia. A
  failed run that created its output still has it cleaned up whatever
  `overwrite` says. If the file cannot be deleted — a read-only directory, say —
  the error tells you it is still there rather than claiming a cleanup that did
  not happen.

  `sample_frames()` writes a numbered image sequence from one command, and a
  failed run there deletes the frames that run wrote, in that directory, leaving
  an earlier run's frames alone.

  This does not reach `ffmpeg()`, the raw escape hatch, which runs a command
  string it cannot parse for an output path.

* A `_batch` verb that refuses a bad value carried in a `jobs` column now says
  which row carries it. The refusal message gains one final bullet — `First
  offending jobs row: 7.` — on the front-door value, vocabulary, codec-token
  and contradiction sweeps of the batch verbs, so a bad cell in a 50-row table
  no longer has to be found by hand. The rest of the message is unchanged
  byte-for-byte, and the same value passed as the verb's own argument (which
  applies to every row) still refuses without naming one. On
  `separate_audio_video_batch()`, whose jobs table is reshaped internally, the
  row named is the row of *your* table, not the reshaped one.

* A bad crop, scale, rate or pixel-format value is now refused by the function
  you called. `crop_video()`'s `width`, `height`, `x` and `y`,
  `standardize_video()`'s `width`, `height`, `fps` and `pixel_format`, and
  `sample_frames_batch()`'s per-row rate used to be reported against an
  internal builder the caller never called — `ffm_crop()`, `ffm_scale()`,
  `ffm_fps()`, `ffm_pixel_format()` — or, on the `_batch` verbs, against
  `purrr::pmap()` with an `In index:` prefix. Each `_batch` sibling refuses the
  value whether it is passed as the argument or carried in a `jobs` column, and
  before any row runs. One message changes: a malformed `pixel_format` used to be reported
  against `format`, an argument name these verbs do not have, and now names
  `pixel_format`. On the two `_batch` verbs gaining a sweep, a call that is
  also wrong about `hardware = "nvenc"` — the machine lacks the encoder — is
  now told about the bad value first, the same answer `crop_video_batch()`
  gives for its `width` and `height`.

* A bad region, inset-scale or loudness value is now refused by the function
  you called. `anonymize_video()`'s per-region `x`, `y`, `width` and `height`
  values, `picture_in_picture()`'s out-of-range `scale`, and
  `normalize_audio()`'s `target_loudness`, `true_peak` and `loudness_range`
  used to be reported against an internal builder the caller never called —
  `ffm_drawbox()`, `ffm_overlay()`, `ffm_loudnorm()` — or, on the `_batch`
  siblings, against `purrr::pmap()` with an `In index:` prefix. Each `_batch`
  sibling refuses the value whether it is passed as the argument or carried in
  a `jobs` column (`regions`, which exists only as a column on the batch verb,
  in its column form), and before any row runs. Under `two_pass = TRUE`, a bad
  loudness target is refused before the analysis pass measures the input,
  instead of after that measurement was already spent.
  `picture_in_picture()`'s existing complaint about a non-numeric `scale` is
  unchanged; the new refusal covers a numeric `scale` outside
  `0 < scale <= 1`. On the two `_batch` verbs that take `hardware`, a call
  also wrong about `hardware = "nvenc"` on a machine without the encoder is
  now told about the bad value first, the same answer the crop and
  standardize verbs give. The documented loudness ranges and the checks that
  enforce them now read from one shared definition per range, so the
  documentation and the refusal can no longer drift apart.

* An input file that does not exist is now reported against the verb you
  called. Every `_batch` verb used to accept a `jobs` table naming a missing
  path and only discover it once the batch was under way, so the error arrived
  as `In index: 3` against `purrr::pmap()`, naming a row number in a table you
  may have built programmatically rather than the file that was not there.
  `concatenate_videos()` and `compare_videos()` had no check of their own at all
  and reported `Error in ffm_files(infiles, outfile)`. All of them now refuse
  the call up front, name the function you called, and account for **every**
  missing path in one message rather than stopping at the first row — so one
  run tells you about all four typos in a fifty-row table instead of four runs.
  One path typed wrong the same way in twenty rows is one missing file, not
  twenty.

  The check reads the same way whichever shape carries the paths: the `input`
  column, the `inputs` list-column of the many-in/one-out verbs, and
  `picture_in_picture_batch()`'s `main`/`overlay` pair. A call that is wrong
  about a path *and* about something else — contradictory codec arguments, an
  unavailable hardware encoder, an out-of-range per-row value — is now told
  about the path first, on the reasoning that a path typed wrong is the more
  likely mistake and is the one you can act on without reading further.
  Malformed table shapes and wrong column types still report before it, since a
  column whose type has not been checked yet cannot usefully be swept for
  paths.

  A file that *exists* but cannot be opened for reading is refused the same
  way, and by the same test the pipeline has always applied: there is now one
  readability test, reached both from the verb you call and from the pipeline
  underneath it, so the two cannot disagree about which paths are acceptable.
  Such a file was previously refused only once the pipeline reached it,
  reported against `ffm_files()` or, from a `_batch` verb, against
  `purrr::pmap()` with an `In index:` prefix.

  Because one message now covers both cases, the wording changed. Where these
  verbs said `` `infile` does not exist: 'clip.mp4'. `` they now say
  `` `infile` can't be found or read: 'clip.mp4'. ``, and the many-path form
  reads `names 2 files that can't be found or read`. Which calls are refused
  is unchanged. `verify_media()` and `write_mediainfo_template()` keep the
  existence wording, their file arguments not being pipeline inputs.

* A malformed codec value — a string carrying whitespace or shell characters,
  such as `"aac -evil"` — is now reported against the argument and the function
  you called. Every verb whose `video_codec` or `audio_codec` argument *sets* a
  codec used to accept such a value at its front door and refuse it deeper in,
  reporting it against the pipeline's internal `audio` / `video` setting,
  against an internal helper, or — on the verbs that fan out, meaning every
  `_batch` sibling and the scalar `segment_video()` — against the fan-out,
  prefixed `In index:`. Non-string values were already reported this way.
  (`verify_media()` carries same-named arguments that are expected probe
  *values* rather than codec settings; it is unaffected.)

  A `_batch` verb reads the same value three ways, and all three now answer
  alike. A malformed value in the scalar argument used to be discarded in
  silence whenever the `jobs` table carried a column of the same name, since the
  column wins; it is now refused. A malformed value in the **column** used to be
  reported from inside the fan-out, naming an internal closure; it is now
  refused at the verb's own front door, before any row runs.

  Under `hardware = "nvenc"`, `standardize_video()` and
  `standardize_video_batch()` used to accept a malformed `video_codec`
  outright — the encoder name was rewritten to the nvenc equivalent before
  anything checked it, and the rewritten name is well-formed. They now refuse
  it, as `crop_video()` already did. One consequence for callers who pass both
  `hardware = "nvenc"` and bad dimensions: `standardize_video()` now reports the
  dimensions first, where it used to report the missing nvenc encoder first.

  No compiled command changes: every legal codec value compiles exactly the
  command it did before.

* When `hardware = "nvenc"` is requested on a machine whose FFmpeg does not
  list the encoder, every verb taking `hardware` now reports the error against
  the function you called rather than against an internal helper. This last
  covers the verbs that fan out over several commands — every `_batch` verb,
  and the scalar `segment_video()`, which fans out over its segments — which
  previously reported the error against `purrr::pmap()` with an internal row
  index, or against a `furrr` closure under `parallel = TRUE`.

  On those verbs the check now also runs before any row does, so a large jobs
  table fails immediately instead of after building the first row's command.
  Where a `video_codec` column names several codec families in one call, each
  family is checked: a build listing `h264_nvenc` but not `av1_nvenc` refuses
  the table rather than failing partway through it.

  Only the encoders a call actually needs are checked, so a row that copies
  rather than re-encodes is not held to an encoder it never asks for.

  Because the check now runs first, it reports ahead of anything still raised
  from inside the fan-out. It does not report ahead of the other checks that
  also moved to the front door in this development cycle. A call that names an
  unavailable encoder and is *also* wrong about a per-row value — a malformed
  `regions` table, an out-of-range `width`, `height`, `margin` or `audio`
  index, a misspelled `direction` or `position` — is told about the value, and
  a call whose *arguments* contradict each other is told about the
  contradiction. Both of those answers are the same on every machine, which is
  why they come first; see the two entries below. Such calls failed before and
  fail now; what changes is which of the errors you see.

  `fallback = TRUE` behaves exactly as before, and no call that used to succeed
  now fails.

* Arguments that contradict each other are now refused by the function you
  called. Six such contradictions used to be caught only while each command was
  being built, which on a verb that processes many files at once meant the error
  was reported against `purrr::pmap()` with an internal row index instead of
  against your call:

  - a video stream copy asked to encode on the GPU
    (`separate_audio_video_batch()`);
  - a `reencode = FALSE` cut that names a `video_codec` or `hardware`
    (`segment_video()`, `segment_video_batch()`);
  - a `reencode = FALSE` cut that names an `audio_codec` other than `"copy"`
    (same two verbs);
  - an `audio_codec` with no audio carried into the output
    (`compare_videos_batch()`, `picture_in_picture_batch()`);
  - `resize = TRUE` across other than two inputs (`compare_videos_batch()`).

  Where any of these values can arrive as a `jobs` column, the check is made
  per row: a table with one offending row is refused for that row, and a table
  with none compiles as before. Large tables now fail immediately rather than
  after building the first command.

  Exactly the same calls are refused as before, verified cell by cell across a
  grid of every combination of the arguments involved. What moves is which
  function the error names, and when.

  Because the check now runs before any row is built, it also reports before
  errors that used to surface from inside the fan-out. A call that is wrong in
  more than one way — a contradiction *plus* an out-of-range `audio` index, a
  misspelled `direction`, an out-of-range `margin`, or a bad `run`/`parallel`
  value — is now told about the contradiction. Such calls failed before and
  fail now; which of the errors you see is what changes.

  On a machine lacking an nvenc encoder, a call that both contradicts itself
  and asks for GPU encoding is told about the contradiction rather than about
  the encoder. A contradiction between two arguments is the same mistake on
  every machine, so it is not reported differently depending on which FFmpeg
  build you happen to have.

  The single-file verbs — `separate_audio_video()`, `compare_videos()`,
  `picture_in_picture()` — build one command each, so three of the four
  contradictions they can raise already named the verb and are unchanged. The
  fourth, `compare_videos()`'s two-input `resize` error, reported against an
  internal function name and now names `compare_videos()`.

* Six per-row value checks are now made by the function you called, before any
  row runs. Each was previously reached only while a row's command was being
  built, so on a verb that processes many files at once the error was reported
  against `purrr::pmap()` — or against a `furrr` closure under
  `parallel = TRUE` — with an internal row index instead of against your call:

  - a `width` or `height` that is neither a positive number nor an FFmpeg
    expression (`crop_video_batch()`);
  - a negative `margin` (`picture_in_picture_batch()`);
  - a `regions` table missing a required column, or carrying one of the wrong
    type (`anonymize_video_batch()`);
  - an `audio` index past the number of inputs in that row
    (`compare_videos_batch()`);
  - a `direction` outside `"horizontal"` and `"vertical"`
    (`compare_videos_batch()`);
  - a `position` outside the five inset positions
    (`picture_in_picture_batch()`).

  The last two were previously checked only for the *argument*. A `jobs` column
  of the same name had its type checked but never its values, so a misspelled
  cell reached the fan-out; both columns are now checked against the same list
  of values the argument is checked against.

  Where any of these values can arrive as a `jobs` column, the check is made
  per row: a table with one offending row is refused for that row, and a table
  with none compiles as before. Large tables now fail immediately rather than
  after building the first command.

  Exactly the same calls are refused as before, verified cell by cell across a
  grid that varies each value in and out of range, as an argument, as a column,
  and as a column whose rows disagree. What moves is which function the error
  names, and when.

  Because these checks now run before any row is built, they also report before
  errors that used to surface first. A call wrong in one of these ways *and*
  asking for an nvenc encoder this machine does not have is now told about the
  value — the reverse of the order shipped earlier in this development cycle,
  so that the diagnosis no longer depends on which FFmpeg build you happen to
  have. A call wrong in one of these ways *and* in an argument that
  `ffm_batch()` alone guards — `run`, `parallel`, `progress`, `manifest`,
  `checksums`, `verify` — is likewise told about the value. (The `jobs` table's
  own shape is not in that list: all four verbs check it themselves before
  reaching `ffm_batch()`, so nothing displaces it.)

  On `compare_videos_batch()` and `picture_in_picture_batch()`, a call can be
  wrong in **both** a per-row value and one of the contradictions above. A value
  error and a contradiction resolve the same way whether the value arrived as an
  argument or in a `jobs` column; the contradiction reports first. Four checks
  moved to make that true — `direction`, `position`, `margin`, and the `audio`
  index — so a call passing one of these as an **argument** alongside a
  contradiction is now told about the contradiction, where it used to be told
  about the value. If you match on the text of an error from such a call, that
  is the message that changed.

  Two consequences worth knowing if you match on error text. First, these four
  checks now also report **after** every argument check that runs before them,
  not only after the contradiction: a call wrong in both one of these values and
  in a malformed `video_codec` or `audio_codec` token, an unrecognized
  `hardware`, a `resize` that is not `TRUE` or `FALSE`
  (`compare_videos_batch()` only), a non-numeric `scale`
  (`picture_in_picture_batch()` only), or a `jobs` table of the wrong shape is
  now told about that other check. Second, the same reordering reaches the
  single-call `compare_videos()` and `picture_in_picture()`, which check
  `direction` and `position` inside the pipeline they share with the batch
  verbs — so `compare_videos(files, out, direction = "sideways", audio_codec =
  "aac")` now reports the `audio_codec` contradiction too. Exactly the same
  calls are refused as before in every case; only which error you are shown
  moves.

  `picture_in_picture_batch()` gains a front-door check on its `audio` index as
  part of this. An out-of-range index in a `jobs` `audio` column was previously
  caught only while a row's command was being built, so it was reported against
  `purrr::pmap()` and named an internal variable (`aud`); it now aborts naming
  the verb you called, before any row runs. Two errors that used to report ahead
  of it — an unavailable nvenc encoder, and `ffm_batch()`'s own argument checks
  — now report after it, matching the other value checks above.

  One class of `audio` value behaves differently from the rest, on both verbs.
  Passing `audio = NA` (or `NaN`) asks to drop the audio, so these are the
  `audio` arguments that *create* the "`audio_codec` needs an audio stream to
  encode" contradiction rather than removing it — `picture_in_picture_batch(jobs,
  audio = NA, audio_codec = "aac")` now reports that contradiction where it used
  to report the `audio` value. An index carries audio, so it never creates that
  contradiction: out of range it reports the `audio` value, and in range the
  call compiles.

  Two smaller corrections come with this. `compare_videos_batch()`'s
  out-of-range `audio` message named an internal variable (`aud`) rather than
  the argument, and now names `audio`. `compare_videos()` and
  `picture_in_picture()` reported a misspelled `direction` or `position`
  against their internal pipeline function, and now name themselves.

* Metadata values containing a newline no longer corrupt the probe output.
  `probe_all()` and the `probe_*()` shortcuts read FFprobe's per-stream output
  as one `key=value` pair per line, so a tag whose value spanned lines — a
  multi-line description or comment, most often — was truncated at the first
  line break and its remainder was read as further `key=value` pairs, adding
  invented columns to the `streams` tibble. Such a value now arrives whole, in
  one cell. Values containing `|` or a backslash are likewise returned
  unchanged. If you worked around this by dropping unexpected columns, that
  workaround is no longer needed.

  The commonest case in practice is a rotated video. FFprobe prints a stream's
  display matrix across four lines, so `streams` gained three columns named
  after the matrix's own rows while its `displaymatrix` cell sat empty. The
  matrix now arrives whole in that cell, and the `rotation` column beside it is
  unchanged.

* The compiled command string that every verb returns under `run = FALSE` — and
  that `ffm_compile()` produces — now wraps each stream map in double quotes:
  `-map "0:a:0"` where it used to print `-map 0:a:0`. Since the verbs began
  stating their stream selection explicitly, that string could carry a `?`
  (as in `-map 0:v?`, "this stream if the input has one"), and pasting it into a
  shell failed there rather than running: zsh reads a bare `?` as a filename
  pattern and answers `no matches found`. The command tidymedia itself runs is
  unchanged — it never goes through a shell — so this affects only what you
  read, log, and paste. If you compare compiled commands against saved strings,
  those strings need updating.

* `convert_audio()` and `convert_audio_batch()` no longer fail on an input with
  more than one audio track. They mapped *every* audio stream into the output,
  so a file carrying several tracks — a recording with separate per-speaker or
  per-language audio, say — handed several streams to a format that accepts only
  one. FFmpeg aborted (`Exactly one MP3 audio stream is required`) and left a
  zero-byte file behind. Both verbs now take the input's first audio track, which
  is what their documentation always described and what a single-track file
  always did. Single-track inputs are unaffected. To choose a track other than
  the first, see the new `audio_stream` argument below.

## New features

* `standardize_video()`, `anonymize_video()` and their `_batch` siblings gain an
  `audio_stream` argument: the 0-based index of the audio track to carry,
  counted among the input's audio streams, so `0` is the first audio track
  whatever its position among the file's streams. Leaving it unset keeps every
  track. The `_batch` verbs also accept an `audio_stream` column in `jobs` to
  choose per row, where `NA` in a cell keeps that row's tracks all.

  Note that `NULL` does not mean the same thing across the package:
  `extract_audio()` and `convert_audio()` take the **first** track when you
  leave `audio_stream` unset, because they write exactly one audio stream and
  have to pick. The verbs that pass audio through — `separate_audio_video()`
  and now these two — keep them all. Each function's documentation says which
  it does and names the ones that do the other.

  Naming a track the input does not have is an FFmpeg error rather than an R
  one, unchanged from the other verbs that take this argument. An input with no
  audio at all is fine, and so is a video-only or audio-only file.

* `extract_audio()` and `convert_audio()` (and their `_batch` siblings) now warn
  when the file they read carries audio tracks the file they write will not.
  Each of these verbs takes exactly one track, so feeding a three-track
  recording to `extract_audio()` without saying which track you want quietly
  discarded two of them. It now says so, tells you how many went, and points at
  `audio_stream` for choosing a different one. Name a track and the warning
  stops; suppress it by class with
  `suppressWarnings(classes = "tidymedia_dropped_audio")`.

  The batch verbs warn **once** for the whole table, naming every affected row,
  rather than once per row.

  The message also spells out a trap worth knowing about: `probe_audio()`'s
  `index` column counts *all* of a file's streams, while `audio_stream` counts
  only its audio streams. On a video file with three audio tracks those read
  `1, 2, 3` and `0, 1, 2` respectively, so reading a number off `probe_audio()`
  and passing it straight to `audio_stream` lands you one track off.

  Counting the tracks means running FFprobe, so the check is **best-effort**: it
  is made when FFprobe is available and the input can be probed, and skipped
  silently otherwise. It never runs under `run = FALSE` — compiling a command
  still touches no binary — and it never changes the command that gets compiled.

* `extract_audio()`, `convert_audio()` and their `_batch` siblings gain an
  `audio_stream` argument for choosing which audio track to take from a file
  that carries several — a recording with separate per-speaker or per-language
  tracks, say. It is a 0-based index counted among the input's audio streams, so
  `audio_stream = 1` takes the second audio track whatever its position among
  the file's streams; the default takes the first.

  In a jobs table, `audio_stream` may be a per-row column, which overrides the
  argument row by row. `NA` in a cell keeps that row on the first audio track,
  the per-row form of leaving the argument unset.

  Asking for a track the input does not have is an FFmpeg error, not an R one:
  the compiled command is still what you asked for, and FFmpeg reports that the
  stream map matches no streams.

* `separate_audio_video()` and `separate_audio_video_batch()` gain an
  `audio_stream` argument for writing one audio track instead of all of them.
  Like the argument of the same name on `extract_audio()` and `convert_audio()`,
  it is a 0-based index counted among the input's audio streams, so
  `audio_stream = 1` writes the second audio track whatever its position among
  the file's streams. Only the audio output is affected — the video file always
  takes the input's video.

  **The default is different on these two verbs, deliberately.** Leaving
  `audio_stream` unset keeps **every** audio track, which is what they have
  always done, rather than the first track `extract_audio()` and
  `convert_audio()` take. An audio container that holds several streams —
  Matroska (`.mka`) or `.m4a` — therefore still receives all of them, and no
  working call changes. The two families differ because they answer different
  questions: an extraction verb writes one track by construction, while a
  separation verb writes whatever your container can hold.

  In a jobs table, `audio_stream` may be a per-row column, which overrides the
  argument row by row; `NA` in a cell keeps that row on every audio track, the
  per-row form of leaving the argument unset.

* `separate_audio_video()` now explains itself when FFmpeg refuses your audio
  file because the input carries several audio tracks. Most audio containers
  (`.aac`, `.mp3`, `.wav`) hold exactly one stream, so separating a three-track
  recording into one of them failed with FFmpeg's own message and a zero-byte
  file — with nothing to say that the track count was the problem, or that there
  was any way around it. The error now states how many tracks the input carries
  and names both ways out: `audio_stream` to write one of them, or a container
  such as `.mka` or `.m4a` to keep them all.

  `separate_audio_video_batch()` cannot abort one row without abandoning the
  rest of the table, so it still records that row as `success = FALSE` and warns
  **once** when the batch finishes, naming every affected input row. Suppress it
  with `suppressWarnings(classes = "tidymedia_multitrack_separation")`.

  Counting the tracks means running FFprobe, so the explanation is
  **best-effort**: you get it when FFprobe is available and the input can be
  probed, and FFmpeg's own error otherwise. The probe runs only after FFmpeg has
  already failed, only on a real run (never under `run = FALSE`), and never
  changes the command that gets compiled. Naming a track skips it entirely —
  with one track mapped, a failure is something else and a track count would not
  explain it.

* `NULL` now means the same thing on every codec argument in the package, and
  `NA` means the same thing in every per-row codec column. `audio_codec = NULL`
  or `video_codec = NULL` emits no `-codec:a` / `-codec:v` at all, leaving the
  encoder to the output container; `NA` in a jobs-table codec column is the
  per-row form of that same `NULL`. Three places disagreed:

  - `anonymize_video()` and `anonymize_video_batch()` refused
    `video_codec = NULL`, while `standardize_video()` next door accepted it.
    Both now accept it — it is how you opt out of the `"libx264"` default when
    the output container is not an H.264 one, such as `.webm`.
  - `extract_audio()` refused `audio_codec = NULL`, while
    `extract_audio_batch()` has always accepted the same call. The scalar verb
    now accepts it too.
  - The `video_codec` columns of `standardize_video_batch()` and
    `anonymize_video_batch()`, and the `audio_codec` column of
    `extract_audio_batch()`, rejected `NA` — so a jobs table could not leave one
    row's codec unset the way every other codec column already could. All three
    now accept it, including in a mixed column where some rows name an encoder
    and others do not.

  No existing command changes. A call passing neither `NULL` nor a column `NA`
  compiles exactly what it compiled before; the calls that changed are ones that
  used to abort and now compile. A *scalar* `NA` is still an error everywhere:
  `NA` spells "unset" only as a column cell, where a per-row table has no other
  way to say it.

  `convert_audio()` and `convert_audio_batch()` stay the deliberate exception —
  `NULL` and a column `NA` there select `-q:a 0`, highest VBR quality, as they
  always have and as their documentation says. `pixel_format` and `color`
  columns still reject `NA`, having no unset state to spell.

  Three error messages changed along the way, all on calls that aborted before
  and still abort:

  - A non-character `video_codec` / `audio_codec` column now reports "must be
    character (`NA` to leave the codec unset)" instead of "must be character
    (no `NA`)", on `standardize_video_batch()`, `anonymize_video_batch()` and
    `extract_audio_batch()` — the message every other codec column already gave.
  - A bad `video_codec` value passed to `anonymize_video()`,
    `anonymize_video_batch()` or `extract_audio()` now says it "must be a single
    string or `NULL`", where it used to say only "a single string". `NULL` is
    legal on those arguments as of this release, so the old wording had become
    untrue.
  - On `standardize_video_batch()` and `anonymize_video_batch()`, a jobs table
    invalid in *both* its `video_codec` column and its `pixel_format` column now
    reports the `pixel_format` problem first; it reported `video_codec` first
    before. Only the reporting order changed — both columns are still rejected.
    (`anonymize_video_batch()`'s `color`-before-`video_codec` order is
    unchanged.)

* `standardize_video()` and `anonymize_video()` (and their `_batch` siblings)
  gain an `audio_codec` argument. Both verbs re-encode video and stream-copy
  audio, but the copy was fixed in place, so there was no way to say otherwise —
  which mattered because a stream copy fails outright when the output container
  cannot hold the source audio codec, and the documented remedy for that
  ("name an encoder") had no argument to name one. `audio_codec = "copy"` is
  the default and compiles exactly the commands these verbs compiled before, so
  calls that name their arguments (or take the defaults) produce identical
  output; `audio_codec = "aac"` transcodes the audio instead;
  `audio_codec = NULL` emits no audio codec at all and lets the output
  container choose.

  The new argument sits beside `video_codec` rather than at the end, so the
  arguments after it have all shifted one position: **calls that pass
  `pixel_format`, `hardware`, `fallback`, or `run` by position rather than by
  name must be updated.**
  `standardize_video(f, out, 1280, 720, 30, "libx264", "yuv420p")`
  now reads `"yuv420p"` as the audio codec, not the pixel format. In line with
  this package's pre-1.0 clean-break policy the argument is placed where it
  belongs rather than appended for compatibility; naming your arguments avoids
  the problem entirely.

  In a jobs table, `audio_codec` may be a per-row column where `NA` means
  "leave that row's codec unset". `hardware` remains batch-wide and applies to
  video only — audio is never hardware-accelerated.

* `separate_audio_video()` and `separate_audio_video_batch()` gain the
  `hardware` and `fallback` arguments the other re-encoding verbs already carry,
  so a video stream that is being re-encoded on the way out can be encoded on an
  NVIDIA GPU: `hardware = "nvenc"`. Only the video output is affected — nvenc
  encodes video, so the audio file is byte-for-byte what it would have been
  otherwise, whatever you pass.

  Because this verb copies the video by default, and a copy runs no encoder at
  all, `hardware = "nvenc"` on its own is an error rather than a silent switch
  from a lossless copy to a GPU re-encode. Pair it with `video_codec = NULL`,
  which assumes the H.264 family, or name a codec (`video_codec = "libx265"`)
  to pin a different one — a non-H.264 container such as `.webm` needs that
  explicit name. As on the other verbs, `hardware` applies to a
  whole batch rather than row by row, so a jobs table mixing copied and
  re-encoded video must be split into separate calls.

* `normalize_audio()` and `normalize_audio_batch()` gain an `audio_codec`
  argument naming the output audio encoder. Loudness normalization filters the
  audio, so it must be re-encoded — and until now it was re-encoded to whatever
  encoder your FFmpeg build defaults to for the output container, which made the
  result depend on the machine. `audio_codec = "aac"` (say) pins it. The default
  `NULL` leaves the codec unset, so existing calls compile exactly the commands
  they did before, and `"copy"` is an error, since a filtered stream cannot be
  copied. In a jobs table, `audio_codec` may be a per-row column (`NA` means
  "leave it unset"), and it applies to the two-pass path as well.

* `crop_video()`, `segment_video()`, `compare_videos()`, and
  `picture_in_picture()` (and their `_batch` siblings) gain a `video_codec`
  argument, alongside the `hardware`/`fallback` GPU toggle. The default
  `video_codec = NULL` leaves the codec unset, so these verbs compile exactly
  the commands they did before and each output keeps its container's default
  encoder. In a jobs table, `video_codec` may be a per-row column (`NA` means
  "leave it unset"); `hardware` and `fallback` apply to the whole batch. Naming
  a codec (or a hardware backend) while cutting with
  `segment_video(reencode = FALSE)` is an error — a stream copy runs no encoder.

* Opt-in NVIDIA GPU (nvenc) video encoding. `standardize_video()`,
  `format_for_web()`, and `anonymize_video()` (and their `_batch` siblings) gain
  a `hardware` argument:
  `hardware = "nvenc"` re-encodes on the GPU, choosing the nvenc encoder for the
  codec family (e.g. `h264_nvenc`). By default an unavailable GPU is an error
  (so output stays reproducible); `fallback = TRUE` re-encodes in software with
  a message instead. `has_nvenc()` reports whether an nvenc encoder is available
  in your FFmpeg build and `nvenc_encoder()` names it. Hardware *decoding* and
  GPU filter pipelines remain out of scope — use `ffmpeg()` for those.

* `sample_frames()` samples a video at a fixed rate (`fps`) or interval
  (`interval`, seconds between frames) into a numbered image sequence — the
  front door to per-frame coding and computer-vision feature pipelines.
  `sample_frames_batch()` does the same across many videos from a jobs table.

* Batch (`_batch`) siblings for the remaining single-input transform verbs:
  `extract_audio_batch()`, `convert_audio_batch()`, `crop_video_batch()`, and
  `format_for_web_batch()` process many files from one jobs table, each a thin
  wrapper over `ffm_batch()`. The audio verbs require an `output` column; the
  video verbs auto-name outputs (`_cropped`, `_web.mp4`) when it is absent, and
  all four reject two rows that resolve to the same output path.

## Standardized function and argument names

The public API was renamed to a single, predictable scheme. These are breaking
changes with no deprecation shims (the package is still pre-1.0 and soaking).

* **Batch verbs now use a `_batch` suffix** instead of a plural noun:
  `segment_videos()` → `segment_video_batch()`, `standardize_videos()` →
  `standardize_video_batch()`, `normalize_audios()` → `normalize_audio_batch()`,
  `anonymize_videos()` → `anonymize_video_batch()`, and `extract_frames()` →
  `extract_frame_batch()` (which also removes the confusion with grabbing "many
  frames" from one video).
* **FFmpeg capability queries moved out of the `get_*` namespace:**
  `get_codecs()` → `ffmpeg_codecs()` and `get_encoders()` → `ffmpeg_encoders()`.
  `get_*` is now reserved for per-file metadata getters.
* **`audio_as_mp3()` is now `convert_audio()`**, with a new `format` argument.
  The default (`format = NULL`) reproduces the old behavior exactly (the output
  format follows the file extension); pass `format` to pin the audio codec.
* **Metadata getters renamed** to match the argument vocabulary:
  `get_samplingrate()` → `get_sample_rate()` and `get_framerate()` →
  `get_frame_rate()`.
* **Codec and time-bound arguments harmonized:** `acodec`/`vcodec` (and the
  matching jobs-table columns) are now `audio_codec`/`video_codec`, and
  `segment_video()`'s `ts_start`/`ts_stop` are now `start`/`end` (matching the
  batch columns).
* **Removed unintended exports:** the unused tidy-eval reexports (`enquo()`,
  `enquos()`, `as_label()`, `as_name()`, `:=`) and two internal helpers
  (`pad_integers()`, `convert_fractions()`) are no longer exported. `.data`
  remains reexported.

## Documentation

* The package has a landing help topic: `?tidymedia` now resolves to an
  overview of the three layers and the vignettes, and the topic is listed by
  `help(package = "tidymedia")` and on the reference index. Previously neither
  reached anything.

* The batch vignette's account of `parallel = TRUE` now names the functions that
  actually take it — `ffm_batch()`, every `*_batch` verb, `segment_video()`, and
  the five `probe_*()` readers — and says that the scalar verbs do not. It
  previously said "the fan-out verbs", which read as covering
  `separate_audio_video()` (only its `_batch` sibling takes the argument) while
  omitting the metadata readers and `segment_video()` entirely. The metadata
  vignette's batching section, which was silent about the argument, now covers
  it too.

* Every verb taking `hardware` now says that asking for `"nvenc"` queries your
  FFmpeg build for the encoder while the command is being assembled, so a call
  that re-encodes the video runs the binary even with `run = FALSE`. Asking for
  `"nvenc"` alongside a stream copy is an error those pages already describe —
  `separate_audio_video()` at its default `video_codec = "copy"`,
  `segment_video(reencode = FALSE)`, and both `_batch` siblings — and it is
  caught first, so such a call aborts without probing. This was always true;
  only the documentation is new. `run = FALSE` promises you the command that
  would run, not a call that touches nothing.

* New `?audio_stream` help page explains the two 0-based audio arguments the
  package exposes and how they differ: `audio_stream` counts one input's audio
  tracks, while `audio` on `compare_videos()` and `picture_in_picture()` counts
  the verb's inputs, so neither index can be read off the other. It also covers
  what leaving each unset means (the extraction verbs take the first track, the
  pass-through verbs keep every track, and an unset `audio` drops audio
  altogether), what an `NA` cell means in a `_batch` jobs column, and the two
  unrelated things `audio` names on `ffm_codec()` and `ffm_copy()`. Every verb
  taking either argument now links to it, and the getting-started vignette
  gains a section on choosing an audio track.
* Corrected several `audio_stream` help pages whose descriptions still listed
  only some of the verbs that keep every audio track, omitting ones added
  later. Those lists are now generated from a single source, so they cannot
  fall behind the code again.
* Help pages now cross-reference each other: every task verb links to the
  `ffm_*` pipeline builders it is built on (and each builder back to the verbs
  that use it), and the three metadata reader families (`probe_*()`,
  `mediainfo_*()`, `get_*()`) link to one another so you can find the
  alternative backend.
* Each metadata help page now states its backend (FFprobe or MediaInfo) and
  what it returns (a tibble, a value, or a single scalar per file), and the
  "Media metadata as tibbles" vignette gains a table comparing the reader
  families at a glance.
* New "A research preprocessing workflow" vignette walks an end-to-end pipeline
  — standardizing recordings, normalizing and extracting audio, sampling frames,
  de-identifying, and packaging for sharing — demonstrating the task verbs on a
  realistic dyadic-interaction study. The "Get started" vignette now leads with
  the task verbs (the front door most users need) before descending to the
  builder, and every vignette cross-links to the others.

## Fixed-region anonymization

* New `anonymize_video()` covers one or more fixed rectangular regions of a
  video with opaque filled boxes — for redacting a face, a name badge, or any
  area that stays in one place for the whole clip (there is no motion
  tracking). Regions are given as a data frame of `x`, `y`, `width`, `height`
  (numbers or FFmpeg expressions), with an optional per-row `color`. The video
  is re-encoded reproducibly (H.264 / `yuv420p` by default) and audio is
  stream-copied unchanged.
* New `anonymize_videos()` applies the same box-fill redaction across many
  videos from one jobs tibble — each row names an `input` and carries its own
  `regions` (a list-column of boxes data frames), with optional per-row
  `output`, `color`, `vcodec`, and `pixel_format` columns. Like the other
  table-driven verbs it is a thin wrapper over `ffm_batch()`, returning one
  reproducible command per input and supporting `verify`, `manifest`, and
  parallel execution.

## Graceful handling of silent input in two-pass loudnorm

* Two-pass normalization now handles digitally silent input honestly. Silence
  measures as `-inf` loudness, which cannot be normalized to a target. Previously
  this surfaced as a misleading "could not parse the loudnorm measurement" error.
  Now `normalize_audio(two_pass = TRUE)` aborts with a clear message that names
  silence as the cause, and `normalize_audios(two_pass = TRUE)` no longer lets
  one silent row abort the whole batch: the non-silent rows are normalized, the
  silent rows are marked in a new logical `silent` column (with `success = FALSE`
  and no output written), and a warning names them. Genuine analysis failures
  still abort fail-fast. (Near-silent but non-empty audio is unaffected.)
* The two-pass batch's result schema no longer depends on how many rows are
  silent: when `verify` or `manifest` is requested, the `verified` column and
  the provenance manifest are now returned even when *every* row is silent
  (silent rows carry `NA` for those outputs), matching a batch with some
  non-silent rows.

## Accurate two-pass loudness normalization

* `normalize_audio()` gained `two_pass`. With `two_pass = TRUE` it runs an
  analysis pass to measure the input's loudness, then a linear correction pass
  that feeds those measurements back, hitting the EBU R128 target far more
  precisely than the single-pass default on material with a wide loudness range.
  Because it must measure the input, two-pass always calls FFmpeg — even under
  `run = FALSE`, where the analysis still runs and the returned value is the
  exact correction command, left unexecuted. The single-pass default is
  unchanged and stays binary-free under `run = FALSE`.
* `normalize_audios()` gained `two_pass` too, applying the same accurate
  measured/linear normalization across a whole jobs table. With
  `two_pass = TRUE` it measures every input (honoring `parallel` and each row's
  targets), then builds and runs one linear correction per row, surfacing the
  five measured values as `measured_I`/`measured_TP`/`measured_LRA`/
  `measured_thresh`/`offset` columns. As with the scalar verb the analysis pass
  always runs — even under `run = FALSE`, which then gates only the correction
  pass — and a row whose analysis yields no usable measurement aborts the call,
  naming the offending row. `two_pass` is a whole-table switch, not a per-row
  column. The single-pass default is unchanged.

## Audio loudness normalization

* Added `normalize_audio()`, a task verb that normalizes a file's perceived
  loudness to an EBU R128 target using FFmpeg's single-pass `loudnorm` filter.
  By default it targets -23 LUFS integrated loudness with a -1 dBTP true-peak
  ceiling (EBU R128, measured per ITU-R BS.1770-4), copies the video stream
  unchanged, and preserves the source channel layout, so the same input always
  yields one reproducible command. Supply `target_loudness`, `true_peak`, and
  `loudness_range` to retarget, and `channels`/`sample_rate` to downmix or
  resample the audio. Note that single-pass `loudnorm` resamples its output (up
  to 192 kHz, encoder-capped), so set `sample_rate` to pin the output rate.
* Added `ffm_loudnorm()`, a builder that appends FFmpeg's EBU R128 `loudnorm`
  audio filter to a pipeline — the first builder to write the audio filter
  chain (`-af`).

## Batch audio normalization across files

* Added `normalize_audios()`, a table-driven companion to `normalize_audio()`.
  Pass a jobs tibble with one row per input (only an `input` column is required)
  to loudness-normalize many files in one call, each to an EBU R128 target. It
  returns the tibble plus one reproducible `command` per row. The five loudness
  knobs — `target_loudness`, `true_peak`, `loudness_range`, `channels`, and
  `sample_rate` — may each appear as a column to vary per row, and outputs are
  auto-named `<base>_normalized.<ext>` when no `output` column is given. It is a
  thin wrapper over `ffm_batch()`, so `...` forwards batch options such as
  `verify`, `manifest`, `checksums`, `progress`, and `parallel`.

## Video standardization

* Added `standardize_video()`, a task verb that re-encodes a video to a
  reproducible, analysis-friendly format in one call. By default it produces
  H.264 video with `yuv420p` and `+faststart`, stream-copies the audio
  unchanged, and keeps the source resolution and frame rate (rounding odd
  dimensions down to the nearest even value so the codec can encode), so the
  same input always yields a byte-identical command. Supply `width`/`height` to
  set the output size (giving only one preserves the aspect ratio with an even
  output dimension), `fps` to resample the frame rate, and
  `vcodec`/`pixel_format` to override the codec or pixel format.
* Added `ffm_fps()`, a builder that appends an `fps` filter to a pipeline,
  accepting either a number of frames per second or an FFmpeg framerate
  expression such as `"30000/1001"`.

## Batch standardization across files

* Added `standardize_videos()`, a table-driven companion to
  `standardize_video()`. Pass a jobs tibble with an `input` column — one row per
  video — to re-encode many files to a reproducible format in one call. It is a
  thin wrapper over `ffm_batch()`, so `...` forwards batch options such as
  `verify`, `manifest`, `checksums`, and `progress`, and each row compiles to a
  command byte-identical to the equivalent `standardize_video()` call. Any of
  `width`, `height`, `fps`, `vcodec`, and `pixel_format` may appear as a column
  to override that setting per row, otherwise the function argument applies to
  every row. The `output` column is optional: when absent, names are derived per
  input as `<basename>_standardized.<ext>` (keeping the source extension), and a
  duplicated `input` with no `output` column is rejected rather than silently
  overwritten.

## Frame extraction across files

* Added `extract_frames()`, a table-driven companion to `extract_frame()`. Pass
  a jobs tibble with an `input` column and exactly one of a `timestamp` or
  `frame` column — one row per frame — to grab still images spanning many input
  files in one call. It is a thin wrapper over `ffm_batch()`, so `...` forwards
  batch options such as `verify`, `manifest`, `checksums`, and `progress`. The
  `output` column is optional: when absent, names are derived per input file as
  `<basename>_<n>.<format>` (default `format = "png"`), the frame number
  restarting for each input.

## Bug fixes

* `normalize_audio_batch(audio_codec = NA)` now aborts instead of quietly
  compiling the default command. A scalar `NA` was resolved the same way as an
  `NA` cell in a jobs-table column — where it legitimately means "leave this
  row's codec unset" — so an accidental `NA` argument produced a command with
  no `-codec:a` and no indication that anything had been ignored.
* Every `video_codec` and `audio_codec` argument now reports a bad value
  against the argument and the verb you actually called. Several previously
  blamed an internal helper, named FFmpeg's own `video` / `audio` parameter
  instead of the argument you passed, or — on the `_batch` verbs — surfaced the
  complaint from inside the row loop with an `In index: 1` prefix, as though one
  row's data were at fault rather than a whole-table argument. Affected
  `standardize_video()`, `standardize_video_batch()`, `anonymize_video_batch()`,
  `extract_audio_batch()`, `convert_audio()`, and `normalize_audio()`.
* A bad `video_codec` / `audio_codec` **argument** on a `_batch` verb is now
  refused even when `jobs` carries a column of the same name. The column
  takes precedence over the argument, so a non-string value passed as the
  argument used to be discarded in silence; `standardize_video_batch()`,
  `anonymize_video_batch()`, `extract_audio_batch()` and
  `normalize_audio_batch()` now report it, matching
  `separate_audio_video_batch()`, which already refused it. Values these verbs *accept*
  are unchanged — a codec string, and `NULL` where it was already legal, behave
  exactly as before.
* One knock-on for `standardize_video()`: a call that passes both a bad
  `video_codec` and an invalid `width` / `height` / `fps` now reports the
  codec problem first, where it previously reported the dimension problem.
  Both complaints are real and fixing the codec argument reveals the other;
  no value that was accepted before is refused now. The other verbs keep
  their previous ordering.

* `ffm_batch()` (and the `parallel = TRUE` path of `segment_video()` /
  `segment_videos()`) now warns when parallel processing is requested but no
  parallel `future::plan()` is active. Previously such calls ran one job at a
  time with no speedup and no indication; the warning points to
  `future::plan(future::multisession)`.

## Batch segmentation across files

* Added `segment_videos()`, a table-driven companion to `segment_video()`. Pass
  a jobs tibble with `input`, `output`, `start`, and `end` columns — one row per
  segment — to cut segments spanning many input files in one call. It is a thin
  wrapper over `ffm_batch()`, so `...` forwards batch options such as `verify`,
  `manifest`, `checksums`, and `progress`; `reencode` selects accurate
  re-encoding (default) or the fast keyframe-snapping copy path, as in
  `segment_video()`.
* `segment_videos()` now reaches full parity with `segment_video()`: the
  `output` column is optional (when absent, names are derived per input file as
  `<basename>_<n>.<ext>`, numbering restarting for each input), a per-row
  `reencode` column overrides the scalar `reencode` argument, and
  non-numeric/character `start`/`end` (or a non-logical `reencode`) columns are
  rejected with a clear error instead of an opaque FFmpeg failure.

## Verification & provenance

* Added `verify_media()`, a probe-backed checker that confirms an output really
  has the properties you asked for. It returns a tidy tibble with one row per
  check (`file`, `check`, `expected`, `actual`, `pass`) covering `duration`,
  `width`, `height`, `video_codec`, `audio_codec`, and `sample_rate`, plus any
  other FFprobe field passed by name through `...`. Numeric checks use an
  absolute `tolerance` (default `0.1`, so integer dimensions match exactly while
  duration gets a little slack); codec checks match exactly.
* Verification is wired into execution. `ffm_run(verify = <named list>)` probes
  the output after a successful run and aborts, listing the failed checks, if
  any assertion fails. `ffm_batch(verify = <list or function>)` instead records
  the outcome in a logical `verified` column (one spec for all jobs, or a
  `pmap`-style function of the job columns) without aborting.
* Added a batch provenance manifest. `ffm_batch(manifest = TRUE)` attaches a
  per-job record — command, FFmpeg/FFprobe versions, timestamp, and output size
  — read back with `ffm_manifest()`, which can also write it to CSV via `path =`.
  `checksums = TRUE` additionally records input/output md5 checksums.
* `ffm_batch(progress = TRUE)` shows a `cli` progress bar as the jobs run
  (following the `future` plan on the parallel path).

## Multi-input verbs

* Completed the blessed multi-input builder set with `ffm_vstack()` (stack
  videos top to bottom, the vertical companion to `ffm_hstack()`) and
  `ffm_overlay()` (composite one video over another at an `x`/`y` position given
  as pixels or an FFmpeg expression). `ffm_overlay()` also takes an optional
  `scale` to resize the overlay to a fraction of the main video's width.
* Added two research task verbs built on these: `compare_videos()` for a
  side-by-side or stacked comparison video, and `picture_in_picture()` for an
  inset overlay (corner or center `position`, `scale`, `margin`). Both drop
  audio by default; pass `audio =` an input index to carry that track.

## Safe execution

* Pipelines are now executed as argument vectors (via `system2()`), never
  through a shell string, so input and output paths containing spaces,
  quotes, `$`, or backticks are handled correctly. This applies to
  `ffm_run()`, `ffm_batch()`, and every task verb; `ffm_compile()` still
  returns the same reproducible command string. The Layer 0 escape hatches
  (`ffmpeg()`, `ffprobe()`, `mediainfo()`) keep their raw-string interface.
* Raw output options added with `ffm_output_options()` are tokenized on
  whitespace at execution time; option values themselves must not contain
  spaces (they never worked reliably before).

## Breaking changes

* `separate_audio_video()` now stream-copies by default — separation is
  lossless and fast, but each output container must support the source codec.
  Use the new `reencode = TRUE` argument for the previous re-encoding
  behavior.
* `ffm_codec()` and `ffm_pixel_format()` now reject values that are not a
  single clean token (no whitespace or shell metacharacters, and starting
  with a letter or digit).
* `ffm_run()` — and every task verb built on it — now aborts with FFmpeg's
  exit status when an encode fails, instead of returning silently (the old
  shell path only emitted a warning). `ffm_batch()` still records failures
  in its `success` column without aborting.
* `ffm_output_options()` now rejects option groups containing quote
  characters: options are split on whitespace into arguments at execution,
  so quoting cannot group tokens (previously such commands executed with a
  different meaning than printed).

## Bug fixes

* An explicit `ffm_map()` on a multi-input pipeline (e.g. `ffm_hstack()`) is
  now emitted alongside the automatic `-map "[vout]"` instead of being
  silently ignored, so e.g. `ffm_map(p, "0:a")` keeps the first input's audio
  next to the stacked video.
* Test coverage is measured again: an empty `R/zzz.R` triggered a `covr` bug
  that silently reported 0% package coverage.

# tidymedia 0.1.0

First tagged release, bringing the metadata, builder, and task-verb work of the
0.0.0.900x development series to a documented, release-ready state.

## Documentation

* Every exported function now carries a worked example and an architecture-layer
  `@family` tag, and there is a [pkgdown site](https://jmgirard.github.io/tidymedia/)
  whose reference index is grouped by the three layers (escape hatch, builder,
  task verbs).
* Added three vignettes: *Get started* (building pipelines, `vignette("tidymedia")`),
  *Media metadata as tibbles* (`vignette("metadata")`), and *Batch processing*
  (`vignette("batch")`).
* A small sample clip now ships in `inst/extdata/sample.mp4` so examples and
  vignettes are runnable.

## Metadata layer

* The MediaInfo and FFprobe readers now **accept a vector of files** and return
  one stacked tibble keyed by a leading `file` column, so metadata for a whole
  batch is ready for `dplyr` joins and filters. This covers `probe_all()`, the
  `probe_*()` shortcuts, `mediainfo_query()`, `mediainfo_template()`,
  `mediainfo_parameter()`, and the `get_*()` convenience helpers.
* **Typed output is now the default.** Every reader gains a `typed` argument
  (default `TRUE`) that converts numeric columns to integers/doubles and turns
  missing markers (FFprobe's `"N/A"`, MediaInfo's empty values) into `NA`;
  fractions, ratios, hex identifiers, and text stay as strings. Pass
  `typed = FALSE` for the previous all-character behavior. This replaces
  `probe_all()`'s `convert` argument.
* Readers are **resilient to unreadable files**: a missing file, or one that
  cannot be probed, among several yields an all-`NA` row (or `NA` value) plus a
  warning, instead of aborting the whole call. Malformed *arguments* still
  abort.
* Arguments are now passed to the CLIs through argument vectors (`system2()`)
  rather than interpolated into a shell string, so file paths and MediaInfo
  `--Inform` templates containing spaces, quotes, `;`, `%`, or `$` work
  correctly. The Layer 0 escape hatches `mediainfo()` / `ffprobe()` keep their
  raw-string signatures.
* Output column schemas are unified: readers lead with a `file` column and the
  two built-in MediaInfo templates now emit snake_case column names.
  User-supplied names (`mediainfo_query(names =)`, custom template headers) are
  kept verbatim.

## Bug fixes

* `ffmpeg()` no longer lets FFmpeg read the calling process's standard input, so
  running a pipeline (e.g. via `ffm_run()`) inside a script that is itself fed
  through `stdin` no longer swallows the rest of that input. Equivalent to
  FFmpeg's `-nostdin`.
* `probe_container()`, `probe_streams()`, `probe_video()`, and `probe_audio()`
  now return the requested tibble when called with `infile =`; they previously
  returned `NULL`.
* `convert_fractions()` parses fractions directly instead of via
  `eval(parse())`, passes `NA` through, and errors on values that are neither a
  number nor a fraction.
* FFprobe's `key=value` output is split on the first `=` only, so values that
  contain `=` are no longer truncated; the superseded `tidyr::separate()` call
  is gone. Files with zero streams no longer trip the stream loop.

# tidymedia 0.0.0.9002

## Batch processing

* Added `ffm_batch()`, the tidymedia batch entry point: it maps a
  pipeline-building function over every row of a jobs data frame (columns are
  passed to the function by name, `purrr::pmap()`-style), compiles one
  reproducible command per job, and optionally runs them. It returns the jobs
  as a tibble with an added `command` column and, when run, a `success` column.
  Set `parallel = TRUE` to map with `furrr` following the active `future` plan.

## Task verbs rebuilt on the builder

* Every task verb is now a thin wrapper over the Layer 1 `ffm_*` builder and no
  longer assembles its own FFmpeg command string. Each gains a `run` argument
  and returns its compiled, reproducible command (invisibly when run):
  `extract_audio()`, `audio_as_mp3()`, `crop_video()`, `format_for_web()`,
  `extract_frame()`, `separate_audio_video()`, `segment_video()`, and
  `concatenate_videos()`.
* `segment_video()` is now built on `ffm_batch()`: it returns the job tibble
  (one row per segment with its command and run status) and gains
  `reencode` and `parallel` arguments.
* **Cutting is frame-accurate by default.** `segment_video()` and the new
  `ffm_seek()` default to `reencode = TRUE`, which re-encodes so cuts land on
  the exact requested frames. `reencode = FALSE` selects a fast, lossless copy
  that snaps to the nearest keyframes (so the output duration may differ by up
  to one group-of-pictures). The previous copy-based `segment_video()` cut at
  the wrong point and shifted timestamps; that behavior is gone.
* Breaking: `extract_audio()`'s free-form `options` string is replaced by an
  `acodec` argument; `crop_video()` drops its `arg` argument and now centers
  the crop by default; `separate_audio_video()` returns a named vector of two
  commands; `segment_video()` returns a tibble.

## Pipeline engine

* Added `ffm_seek()`, a seek-based cut using `-ss`/`-to` (distinct from the
  `trim` filter of `ffm_trim()`), so cuts can stream-copy. Accurate seeks
  output-seek and re-encode; fast copy seeks input-seek and add
  `-avoid_negative_ts`.
* Added `ffm_concat()`, a blessed multi-input verb that concatenates the
  pipeline's inputs via FFmpeg's concat demuxer (fast, lossless, same-format).
* Added `ffm_output_options()`, a controlled passthrough for raw output options
  that `ffm_compile()` still positions and quotes.

# tidymedia 0.0.0.9001

## Pipeline engine

* Reworked the Layer 1 `ffm_*` builder onto a structured command model:
  `ffm_compile()` is now the single place that assembles, positions, and quotes
  every option. Single-input filter chains compile to `-vf`/`-af`; multi-input
  stacking compiles to a valid `-filter_complex` graph with explicit stream
  labels and an automatic `-map`.
* Fixed four builder bugs: `ffm_trim(setpts = FALSE)` no longer forces a
  `setpts` filter; `ffm_drop()` flags are now output options placed after the
  input (not before `-i`); `ffm_pixel_format()` no longer runs into the output
  filename; and the previously invalid `-filter_complex:v` output is gone.
* `ffm_compile()` now errors early when a stream is set to codec `copy` while a
  filter targets that same stream, instead of failing cryptically in ffmpeg.
* `ffm_hstack()` must be applied before other video filters and now produces a
  runnable command (verified end-to-end against ffmpeg).

## Infrastructure

* Added a testthat (3rd edition) test suite covering the `ffm_*` pipeline
  builder and `ffm_compile()` output, plus binary-gated tests for the
  ffmpeg/ffprobe/mediainfo task functions.
* Added GitHub Actions workflows for `R CMD check` (macOS, Windows, Linux) and
  test coverage; the Linux jobs install ffmpeg and mediainfo so execution
  tests run in CI.
* All input validation and user-facing messages now use rlang's `check_*`
  helpers and cli (`cli::cli_abort()` / `cli::cli_warn()`); the assertthat
  dependency has been removed. Added `dplyr`, `tidyr`, `purrr`, and `cli` to
  Imports (the first three were already used but undeclared).
* Enumerated arguments (e.g. `units`, `unit`, `section`, `template`,
  `program`) are now matched exactly via `rlang::arg_match()` instead of the
  partial matching of `match.arg()`; pass the full value.

## Bug fixes

* `mediainfo_parameter()` (and the helpers built on it: `get_duration()`,
  `get_framerate()`, `get_width()`, `get_height()`, `get_samplingrate()`) now
  shell-quote the `--Inform` argument, so they work on POSIX shells where the
  `;` was previously parsed as a command separator.

# tidymedia 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
