# tidymedia 0.2.0

## Breaking changes

The package is pre-1.0 and still soaking, so old names are removed rather than
deprecated: there are no `lifecycle` shims, and a call using an old name gets
R's usual `could not find function` or unused-argument error.

* **Exports renamed or removed.** `get_codecs()` and `get_encoders()` are now
  `ffmpeg_codecs()` and `ffmpeg_encoders()`, since `get_*` is reserved for
  per-file metadata getters. `get_samplingrate()` and `get_framerate()` are now
  `get_sample_rate()` and `get_frame_rate()`. `audio_as_mp3()` is now
  `convert_audio()`, which names a codec instead of hard-coding one. `ffm()` and
  `mediainfo_summary()` are gone — each was a second name for a function that
  already had one (`ffm_files()` and `mediainfo_template()`). The reexported
  tidy-eval helpers `enquo()`, `enquos()`, `as_label()`, `as_name()` and `:=`
  are no longer exported; `.data` still is. `pad_integers()` and
  `convert_fractions()`, exported by accident, are internal again.

* **Arguments renamed.** `extract_audio()`'s `acodec` is now `audio_codec`, and
  `segment_video()`'s `ts_start` and `ts_stop` are now `start` and `end`.
  Neither verb takes `...`, so an old name is an error rather than a silently
  ignored argument.

* **Argument positions have moved, so calls that pass later arguments by
  position must be updated.** New arguments are placed beside the argument they
  belong with rather than appended for compatibility. Naming your arguments
  avoids the problem entirely.

  | verb | `run` was at position | `run` is now at position |
  |---|---|---|
  | `extract_audio()` | 4 | 5 |
  | `format_for_web()` | 3 | 7 |
  | `separate_audio_video()` | 4 | 10 |
  | `segment_video()` | 6 | 12 |
  | `crop_video()` | 7 | 13 |

  `segment_video()`'s `parallel` moves from position 7 to 13. The old slots now
  hold other arguments, so a positional value lands on one of those instead:
  `extract_audio(video, "audio.aac", "copy", FALSE)` reads `FALSE` as the
  audio-stream index, and a positional `TRUE` in `crop_video()`'s or
  `segment_video()`'s old `run` slot stops with `` `video_codec` must be a
  single string or `NULL` ``.

* **Every verb that carries audio now states which audio tracks it takes**,
  instead of leaving the choice to FFmpeg. A verb that emitted no stream mapping
  got FFmpeg's own rules — one stream of each type, preferring whichever track
  carries the container's "default" flag — so the surviving track depended on
  the input's flags and could differ between FFmpeg versions on the same file.
  The new `audio_stream` argument names a single track (see *New features*).
  What changes depends on the verb:

  - **Now keep every track:** `standardize_video()`, `anonymize_video()`,
    `segment_video()` at its default `reencode = TRUE`, and `format_for_web()`.
    Outputs from multi-track sources will gain tracks they used to lose, and
    grow accordingly. `crop_video()` and `segment_video(reencode = FALSE)`
    mapped every stream already.
  - **Now take the first track:** `extract_audio()` and `convert_audio()`, which
    write exactly one audio stream. On an input whose *second* track is flagged
    as the default you used to get that track and now get the first; pass
    `audio_stream = 1` for the old result. `convert_audio()` and
    `convert_audio_batch()` also stop failing outright on multi-track input,
    where they used to hand several streams to a format that accepts one.
  - **Subtitle and data streams are no longer carried** by `crop_video()`,
    `segment_video(reencode = FALSE)`, `standardize_video()`,
    `anonymize_video()` and `extract_audio()`. Writing to `.mkv` passed one
    subtitle through and now passes none. Writing to `.mp4` is unaffected, and
    this also fixes `crop_video()` aborting on a subtitle-bearing input written
    to `.mp4`.
  - **`normalize_audio()` is now an audio-producing verb**: one audio stream and
    no video, whatever container you name, keeping the first track. Measuring
    loudness produces one measurement per track while the correction applies a
    single set of values, so normalizing several at once would silently apply
    the first track's measurements to all of them. Two consequences: normalizing
    a recording's loudness while keeping its picture is no longer possible in one
    call (normalize to an audio file and mux it back with `ffmpeg()`), and an
    input with no audio is now an error rather than a silent copy of the video.
    In exchange, the output container no longer decides whether the call works —
    `.wav`, `.mp3`, `.aac`, `.flac`, `.opus`, `.m4a`, `.mka`, `.oga`, `.w64` and
    the video containers all behave the same way.

  Naming a track the input does not have remains an FFmpeg error rather than an
  R one, on every verb.

* **Pass-through audio is stream-copied instead of silently re-encoded.**
  `crop_video()`, `segment_video()`, `compare_videos()` and
  `picture_in_picture()` (and their `_batch` siblings) left the audio codec
  unset, so whatever encoder your FFmpeg build defaults to for the output
  container re-encoded it — a quality loss, and a result that depended on the
  machine. Their commands now carry `-codec:a copy` wherever they map audio. The
  new `audio_codec` argument controls this: `"copy"` is the default, an encoder
  name transcodes, and `NULL` restores the old unset behavior. A stream copy
  fails if the output container cannot hold the source codec (FLAC in `.mp4`,
  say) — name an encoder there. Under `segment_video(reencode = FALSE)` any
  `audio_codec` other than `"copy"` is an error.

* **`separate_audio_video()` and `separate_audio_video_batch()` stream-copy by
  default**, and name an encoder per output file. Separation is lossless and
  fast this way, but each output container must support the source codec. The
  new `audio_codec` and `video_codec` each govern only their own output file,
  both defaulting to `"copy"`. In a jobs table both may be per-row columns where
  `NA` means "leave that stream's codec unset"; because each input row fans out
  into an audio row and a video row, the returned table collapses the two into
  one `codec` column.

* **`hardware_encoder()` and `has_hardware_encoder()` take a second argument
  naming the backend, and it has no default.** With two backends, a helper that
  silently answered for one reports on a machine you did not ask about — on a
  Mac, `hardware_encoder("h264")` would have declared the NVIDIA encoder
  available. The argument accepts `"nvenc"` and `"videotoolbox"` only.

* **`ffm_map()` appends instead of overwriting**, emitting one `-map` per
  mapping in the order given, which is what lets a pipeline keep the video and
  then name one audio track; `mapping` may now be a character vector. Pass
  `replace = TRUE` for the old discard-what-came-before behavior.
  `ffm_copy()` **sets** the all-streams mapping rather than adding to it, so
  `ffm_copy() |> ffm_copy()` no longer duplicates every output stream. Where the
  pipeline already states a different mapping, `ffm_copy()` now stops rather
  than discarding it silently; pass `streams = FALSE` to keep yours. No pipeline
  built by a task verb was affected.

* **Three Layer 1 builders refuse values they used to accept.** `ffm_codec()`
  and `ffm_pixel_format()` reject anything that is not a single clean token.
  `ffm_output_options()` rejects option groups containing quote characters:
  options are split on whitespace at execution, so quoting cannot group tokens,
  and such commands used to execute with a different meaning than the one
  printed.

* **`ffm_run()`, and every task verb built on it, aborts with FFmpeg's exit
  status when an encode fails**, instead of returning silently with a warning.
  `ffm_batch()` still records failures in its `success` column without aborting.

* **The compiled command string wraps each stream map in double quotes** —
  `-map "0:a:0"` where it used to print `-map 0:a:0`. That string can now carry
  a `?`, and pasting it into zsh failed there with `no matches found`. The
  command tidymedia itself runs is unchanged, since it never goes through a
  shell; this affects only what you read, log and paste.

## New features

* **Six new task verbs.**

  - `standardize_video()` re-encodes to a reproducible, analysis-friendly format
    in one call: H.264 with `yuv420p` and `+faststart` by default, audio
    stream-copied, source resolution and frame rate kept (odd dimensions rounded
    down to even). `width`/`height`, `fps`, `video_codec` and `pixel_format`
    override.
  - `normalize_audio()` normalizes perceived loudness to an EBU R128 target with
    FFmpeg's `loudnorm` filter, defaulting to -23 LUFS integrated with a -1 dBTP
    true-peak ceiling. `target_loudness`, `true_peak`, `loudness_range`,
    `channels` and `sample_rate` retarget it. Single-pass `loudnorm` resamples
    its output, so set `sample_rate` to pin the output rate.
  - `anonymize_video()` covers fixed rectangular regions with opaque filled
    boxes, for redacting a face or a name badge that stays in one place (there
    is no motion tracking). Regions are a data frame of `x`, `y`, `width`,
    `height` with an optional per-row `color`.
  - `strip_metadata()` removes container and global metadata tags — creation
    time, GPS, device make and model, title, comment — together with any
    chapters. The streams are stream-copied, so the picture and sound are
    bit-for-bit unchanged, and the output is muxed bit-exactly so FFmpeg does not
    re-stamp a fresh `creation_time`. Identifiers inside the encoded bitstream,
    and per-stream tags such as `handler_name`, are not removed.
  - `sample_frames()` samples a video at a fixed rate (`fps`) or interval
    (`interval`) into a numbered image sequence.
  - `convert_audio()` transcodes audio to the codec you name, or lets the output
    extension pick one at highest VBR quality.

  `compare_videos()` and `picture_in_picture()` join them as the two fan-in
  verbs: a side-by-side or stacked comparison, and an inset overlay with a
  `position`, `scale` and `margin`. Both drop audio by default; `audio_input` is
  the 0-based index of the *input* whose audio to keep, which counts the verb's
  inputs rather than one input's audio streams.

* **Batch siblings for every transform verb.** Each takes a jobs tibble with one
  row per unit of work and is a thin wrapper over `ffm_batch()`, so `...`
  forwards `verify`, `manifest`, `checksums`, `progress` and `parallel`, and each
  row compiles to a command byte-identical to the equivalent scalar call. Where a
  scalar argument can sensibly vary per row it may also be a `jobs` column, which
  overrides the argument row by row. The fan-in verbs
  (`concatenate_videos_batch()`, `compare_videos_batch()`) carry an `inputs`
  list-column, and `picture_in_picture_batch()` fixed `main` and `overlay`
  columns.

  The `output` column is optional on the verbs that can derive a name. Six
  auto-name one output per input by suffixing its basename — `_standardized`,
  `_normalized`, `_anonymized`, `_stripped`, `_cropped`, `_web.mp4`.
  `segment_video_batch()` and `extract_frame_batch()` append a zero-padded
  `<basename>_<n>` restarting at every input, and `sample_frames_batch()` writes
  into a `<basename>_frames` directory. Every batch verb refuses, before any row
  runs, a table in which two rows name the same output path.

* **`ffm_jobs()` turns a directory into a batch jobs table.** It lists the files
  in a directory whose names do not start with a dot and that carry one of the
  extensions it knows for a media type, returning the tibble `ffm_batch()` takes:
  one row per file with its full path in an `input` column. `type` — `"video"`,
  `"audio"` or `"image"` — has no default, `extension` narrows within a type, and
  `recursive = TRUE` descends into subdirectories. The function reads names
  rather than file contents, so a folder of TypeScript sources comes back as
  video rows under `type = "video"`, `.ts` being a video extension here. Six of
  the fifteen `*_batch()` verbs take the table unaltered; the other nine refuse
  it until you add the columns their own task needs.

* **A `quality` argument on the re-encoding verbs.** `standardize_video()`,
  `format_for_web()`, `anonymize_video()`, `crop_video()`, `segment_video()`,
  `separate_audio_video()`, `compare_videos()` and `picture_in_picture()` take
  `quality = NULL`. It is the encoder's own rate-control value, passed through
  unchanged: `libx264` and `libx265` read it as `-crf` (0 to 51), the nvenc
  encoders as `-cq` (0 to 51), and the videotoolbox encoders as `-q:v` (1 to
  100). No cross-encoder scale exists, so the same number means something
  different on each. A call is refused before FFmpeg runs when the value is not
  one finite number, is outside the encoder's range, names an encoder outside
  those seven, or is set alongside a stream copy.

  The eight `_batch` siblings take it too, after `fallback`, applying to every
  row unless `jobs` carries a numeric `quality` column, where `NA` leaves that
  row's encoder default in place. Each cell is checked against the encoder its
  own row resolves to, and a wrong cell is refused before any row runs, naming
  the function and the row.

* **`audio_stream`: naming which audio track to work on.** Nine scalar verbs and
  their `_batch` siblings take it — a 0-based index counted among the input's
  audio streams, so `audio_stream = 1` is the second audio track whatever its
  position among the file's streams. In a jobs table it may be a per-row column
  where `NA` means "unset". What unset means depends on the verb, as described
  under *Breaking changes* and on the new `?audio_stream` page. One trap:
  `probe_audio()`'s `index` column counts *all* of a file's streams while
  `audio_stream` counts only its audio streams, so reading a number off one and
  passing it to the other lands you a track off.

* **A warning when tracks are being dropped.** `extract_audio()`,
  `convert_audio()`, `normalize_audio()` and their `_batch` siblings warn when
  the input carries audio tracks the output will not, say how many went, and
  point at `audio_stream`. Naming a track stops it, and it can be suppressed by
  class with `suppressWarnings(classes = "tidymedia_dropped_audio")`. The batch
  verbs warn once for the whole table, naming every affected row. Counting the
  tracks means running FFprobe, so the check is best-effort, never runs under
  `run = FALSE`, and never changes the compiled command.
  `options(tidymedia.check_tracks = FALSE)` switches it off for the rest of the
  session; it defaults to TRUE, so nothing changes until you set it. Its whole
  cost is one FFprobe call per distinct input, run before the work starts and,
  on the `_batch` verbs, serially at the front door before the fan-out. A row
  that names an `audio_stream` is never probed.

* **Two-pass loudness normalization.** `normalize_audio(two_pass = TRUE)` runs an
  analysis pass to measure the input, then a linear correction pass that feeds
  those measurements back, hitting the EBU R128 target far more precisely than
  the single-pass default on material with a wide loudness range. Because it must
  measure, two-pass always calls FFmpeg — even under `run = FALSE`, where the
  returned value is the exact correction command, left unexecuted.
  `normalize_audio_batch(two_pass = TRUE)` does the same across a jobs table,
  surfacing the five measured values as
  `measured_I`/`measured_TP`/`measured_LRA`/`measured_thresh`/`offset` columns.

  Digitally silent input measures as `-inf` loudness, which cannot be normalized.
  The scalar verb aborts naming silence as the cause; the batch verb normalizes
  the non-silent rows, marks the silent ones in a logical `silent` column with
  `success = FALSE`, and warns naming them.

* **Checking a result, and recording how it was made.** `verify_media()` is a
  probe-backed checker that confirms an output really has the properties you
  asked for, returning a tibble with one row per check (`file`, `check`,
  `expected`, `actual`, `pass`) covering `duration`, `width`, `height`,
  `video_codec`, `audio_codec` and `sample_rate`, plus any other FFprobe field
  passed through `...`. Numeric checks use an absolute `tolerance` (default
  `0.1`); codec checks match exactly.

  It is wired into execution: `ffm_run(verify = <named list>)` probes the output
  after a successful run and aborts listing the failed checks, while
  `ffm_batch(verify = )` records the outcome in a logical `verified` column
  without aborting. `ffm_batch(manifest = TRUE)` attaches a per-job provenance
  record — command, FFmpeg/FFprobe versions, timestamp, output size — read back
  with `ffm_manifest()`, and `checksums = TRUE` adds input and output md5 sums.
  `ffm_batch(progress = TRUE)` shows a `cli` progress bar.

* **Opt-in hardware video encoding, as a vocabulary of backends rather than one
  vendor.** Sixteen verbs take a `hardware` argument. `"nvenc"` encodes on an
  NVIDIA GPU and `"videotoolbox"` on Apple silicon; `"none"` is the default, so a
  call that does not ask for hardware is unchanged. Each backend covers the codec
  families it has encoders for — nvenc h264, hevc and av1; videotoolbox h264 and
  hevc — and the encoder is named from the family and the backend, so
  `video_codec = "libx264"` resolves to `h264_nvenc` under one and
  `h264_videotoolbox` under the other. Asking a backend for a family it has no
  encoder for is an error whatever `fallback` says. What `fallback` covers is an
  encoder the backend has but *your* FFmpeg build does not list: by default that
  is an error too, so output stays reproducible, and `fallback = TRUE` re-encodes
  in software with a message instead. `has_hardware_encoder()` reports whether a
  backend's encoder is available and `hardware_encoder()` names it;
  `options(tidymedia.hardware_encoders = )` overrides detection outright.
  Hardware *decoding* and GPU filter pipelines remain out of scope.

* **Codec arguments on every transform verb, spelled the same way.**
  `standardize_video()` and `anonymize_video()` gain `audio_codec`;
  `crop_video()`, `segment_video()`, `compare_videos()` and
  `picture_in_picture()` gain `video_codec`; `normalize_audio()` gains
  `audio_codec` naming the output audio encoder, since loudness normalization
  must re-encode. Each is available as a per-row `jobs` column too.

  `NULL` now means the same thing on every codec argument, and `NA` the same
  thing in every per-row codec column: emit no `-codec:a` / `-codec:v` at all,
  leaving the encoder to the output container. Three places disagreed —
  `anonymize_video()` refused `video_codec = NULL` while `standardize_video()`
  accepted it, `extract_audio()` refused `audio_codec = NULL` while its `_batch`
  sibling accepted it, and three codec columns rejected `NA`. All now accept it.
  No existing command changes; the calls that changed are ones that used to
  abort and now compile. A *scalar* `NA` is still an error everywhere.
  `convert_audio()` stays the deliberate exception, where `NULL` selects `-q:a
  0`.

* **A wall-clock limit on the programs tidymedia starts.**
  `options(tidymedia.timeout = 600)` gives every FFmpeg, FFprobe and MediaInfo
  process a limit in whole seconds. The default is `0`, meaning no limit. The
  limit applies to each spawned program rather than to a batch as a whole, and
  the `parallel = TRUE` paths are bounded by the same limit as the sequential
  ones.

  A reached limit is never silent. The task verbs, `ffm_run()`, `verify_media()`
  and the raw `ffmpeg()`/`ffprobe()`/`mediainfo()` hatches abort, naming the
  program and the limit. Where one hung file must not discard the rest of the
  work they warn instead: the metadata readers give an `NA` row and one warning
  saying how many files timed out, and the batch verbs mark the row
  `success = FALSE` and warn once at the end. Those two lists are not written
  from memory: a test derives the calls that can start one of these programs
  from the package's own call graph and drives a timeout through each of them.

  `with_timeout(expr, seconds)` puts the limit on one call and restores whatever
  the session had, by any exit route, and `local_timeout(seconds)` is the
  statement form bounding the rest of the function you call it from. Both refuse
  a value the underlying limit could not use before `expr` runs.

  The limit says how long tidymedia waits for a program, not how long that
  program may run. When it is reached R asks the program to stop, insists 20
  seconds later, and kills it 20 seconds after that, so a program that answers
  none of the three is waited for up to 40 seconds longer than you asked. Under
  a 2-second limit, an FFmpeg blocked reading a pipe nobody writes to returned
  at 42.0 seconds on Linux, and a shell child that ignores both signals returned
  at 42.0 seconds on Linux and macOS alike. Plan for it: a 1-second limit across
  five hung files is three and a half minutes of waiting, not five seconds. How
  much of the lag you see depends on your FFmpeg, since the same blocked input
  took 42.0 seconds against FFmpeg 6.1.1 and 2.0 seconds against 9.0.1.

* **A `parallel = TRUE` call runs its workers under the tidymedia settings you
  set in your own session.** Each worker previously started from an empty option
  list, so `options(tidymedia.timeout = )` bounded a sequential batch and left
  the parallel one unbounded. All three tidymedia options are now carried into
  each worker for the duration of the call and put back afterwards, including
  when the call fails. What is not carried is the remembered answer about your
  FFmpeg build: a worker with no override still asks its own binary once.

* **A failed FFmpeg run is something you can catch.** `ffm_run()` aborts with a
  condition of class `tidymedia_ffmpeg_exit`, carrying the exit status in its
  `tm_status` field:

  ```r
  tryCatch(
    ffm_run(pipeline),
    tidymedia_ffmpeg_exit = function(cnd) cnd$tm_status
  )
  ```

  The `loudnorm` analysis pass behind `normalize_audio(two_pass = TRUE)` raises
  `tidymedia_loudnorm_no_measurement` instead, meaning the analysis yielded no
  usable measurement, and both the scalar and batch verbs raise the same class.
  The `ffm_batch()` family records `success = FALSE` for a failed row rather than
  raising either class. Batch diagnostics that cannot report one exit status carry the
  numbers they do have: `tm_rows`, the 1-indexed offending rows, and
  `tm_row_status`, their exit statuses aligned to it.

* **`install_on_win()` checks what it downloaded before it changes anything.** On
  the build tidymedia fetches by default it downloads the SHA-256 digest
  gyan.dev publishes beside the archive — before the archive itself, so a source
  that cannot produce one refuses in a second rather than after a long download —
  and refuses to unpack anything whose digest does not match. For a build you
  name yourself, pass its digest as the new `archive_checksum`; without one the
  call installs as before but says the archive was not verified. The digest
  travels from the same host over the same connection as the archive, so this
  catches a corrupted or truncated download, not a substituted one. Its failures
  now carry conditions you can catch by class, and the temporary download is
  removed whether the install succeeds or fails.

* **`program_status()` reports all four programs in one table** — `ffmpeg`,
  `ffprobe`, `ffplay` and `mediainfo` — with where each resolved to and what
  version it reported, `NA` in both columns for one that could not be found. A
  program it cannot find is reported rather than warned about, so checking a
  fresh setup is one call and one table.

* **`unset_program()` forgets a location `set_program()` remembered**, so lookups
  go back to the `PATH`. It clears both the current configuration file and one
  written by an earlier version of tidymedia, and discards what tidymedia
  remembers about your FFmpeg build whenever a removal took. The program must be
  named, since the call deletes a file. With nothing remembered it warns and
  returns `FALSE` rather than failing.

* **Four new Layer 1 builders.** `ffm_fps()` appends an `fps` filter, accepting a
  number or an FFmpeg framerate expression such as `"30000/1001"`.
  `ffm_loudnorm()` appends the EBU R128 `loudnorm` filter — the first builder to
  write the audio filter chain. `ffm_vstack()` stacks videos top to bottom, the
  companion to `ffm_hstack()`, alongside `ffm_overlay()`, which composites one
  video over another at an `x`/`y` position with an optional `scale`.

## Bug fixes

* **Two jobs can no longer be given the same output path.** Every batch verb,
  `segment_video()` and `ffm_batch()` refuse such a call before any job runs, and
  under `run = FALSE` as well. Six let some through, whether by a repeated
  `output` column, by two inputs deriving the same name, or by `outfiles`
  repeating a name. `ffm_batch()` lets an output that writes no file repeat.

* **A wrong argument is now reported against the function you called, before any
  row runs.** Most checks used to be reached only while a command was being
  built, so on a batch verb the error arrived as ``Error in
  `purrr::pmap(jobs, .f, ...)` `` with an `In index: 1` line, and others named a
  Layer 1 builder the verb had called on its way down, or an internal variable.
  Input paths, codec tokens, dimension and position values, region tables,
  enumerated values, hardware backends and argument contradictions are now
  refused at the verb's own front door and name it, whether the value arrives as
  an argument or in a `jobs` column of the same name.

  **Exactly the same calls are refused as before**, and no legal value compiles a
  different command. What moves is which function the error names and, when a
  call is wrong in more than one way, which error you see: a path typed wrong is
  reported first, an argument contradiction ahead of a per-row value error, and
  both ahead of an unavailable hardware encoder, so the diagnosis no longer
  depends on which FFmpeg build you happen to have. On
  `compare_videos_batch()` and `picture_in_picture_batch()`, where a call can be
  wrong in both at once: A value error and a contradiction resolve the same way
  whether the value arrived as an argument or in a `jobs` column; the
  contradiction reports first. Some wording changed on calls
  that aborted before and still abort: `` `infile` does not exist `` is now
  `` `infile` can't be found or read ``. One message is missing rather than
  moved: a refusal that now happens before any row is built no longer carries the
  `In index:` line — except where the next entry supplies it.

* **A `_batch` verb that refuses a bad value carried in a `jobs` column now says
  which row carries it.** The message gains a final `First offending jobs row:
  7.` bullet on the front-door sweeps, so a bad cell in a 50-row table no longer
  has to be found by hand. The rest of the message is unchanged, and the same
  value passed as the verb's own argument still refuses without naming a row.

* **A failed audio output no longer costs you the video in
  `separate_audio_video()`.** The verb runs the audio command first, and a
  failure there used to abort before the video command ran at all. The video
  command now runs either way; the audio failure is still what aborts the call,
  and its error gains a line naming the video file when that run actually wrote
  it. The video command's own condition rides on the error's `tm_video_error`
  field.

* **The advice `separate_audio_video()` gives when an audio output fails no
  longer arrives when you are already following it.** The multi-track report was
  attached to any failed audio command on a multi-track input, including one
  already writing to `.mka`, `.m4a`, `.mp4`, `.mov`, `.mkv`, `.webm`, `.ogg`,
  `.opus` or `.ts` — every one of which holds three audio tracks. Writing to one
  of those nine you now get the error the run itself raised. The batch sibling
  drops such a row from its post-fan-out warning rather than listing it.

* **A run that fails no longer leaves a broken output file behind.** FFmpeg
  creates its output before it knows the command will work, so a refused encode
  left a zero-byte file where a result should be — and an existing file had
  already been truncated to zero. Every verb, and every row of a `_batch` verb,
  now deletes what the failed run wrote and says so. Only what the run wrote:
  tidymedia checks the output's size and timestamp before the run and again after
  the failure, so a file a refused-before-opening command never touched is left
  exactly as it was. `overwrite = FALSE` keeps its own guarantee, and if the file
  cannot be deleted the error says it is still there.

* **Pipelines are executed as argument vectors (via `system2()`), never through a
  shell string**, so paths containing spaces, quotes, `$` or backticks are
  handled correctly. This applies to `ffm_run()`, `ffm_batch()` and every task
  verb; the Layer 0 escape hatches keep their raw-string interface.

* **`normalize_audio()` works when the output is FLAC (`.flac`) or Ogg Vorbis
  (`.oga`).** On FFmpeg 9 these failed with "Could not open encoder before EOF"
  and left a zero-byte file: the loudness filter hands its output on in very long
  frames, longer than FLAC will encode. Normalization now re-chunks its output,
  so every audio container works. Commands built with `ffm_loudnorm()` carry the
  extra `asetnsamples` filter, visible in the compiled command string.

* **Metadata values containing a newline no longer corrupt the probe output.**
  `probe_all()` and the `probe_*()` shortcuts read FFprobe's output as one
  `key=value` pair per line, so a multi-line tag was truncated at the first break
  and its remainder read as further pairs, adding invented columns to the
  `streams` tibble. The commonest case is a rotated video, whose display matrix
  prints across four lines. Such a value now arrives whole, in one cell.

* **A missing value where a number belongs is refused instead of reaching
  FFmpeg.** `crop_video(f, o, width = NA_real_)` used to fail with R's own
  `missing value where TRUE/FALSE needed`, and `width = NA_character_` was worse,
  compiling `crop=w=NA:h=100` into the command. Both now abort naming the
  argument and the verb. The same covers the size and position arguments of
  `crop_video()` and `standardize_video()`, `anonymize_video()`'s region values,
  and the `ffm_crop()` / `ffm_scale()` / `ffm_fps()` / `ffm_overlay()` /
  `ffm_drawbox()` builders. `normalize_audio_batch(audio_codec = NA)` likewise
  aborts instead of quietly compiling the default command.

* **`ffmpeg_codecs(sort_by_type = )` refuses a value that is not `TRUE` or
  `FALSE`, without running FFmpeg first.** A string or `NA` used to run the
  binary, parse the whole codec list, and only then fail on an internal `if`; a
  number never failed at all, so `sort_by_type = 1` returned the sorted table.
  That call is now an error, matching `ffmpeg_encoders()`.

* **A configuration file that holds nothing, more than one line, or one empty
  line no longer stops the call or answers about the wrong thing.** The first two
  made `find_ffmpeg()` and every call above it fail with an R error naming
  neither the program nor the file; the third warned that the binary had gone
  missing, which was not what was wrong. All three now warn with a
  `tidymedia_location_unreadable` condition you can catch by class, and return
  `NULL`.

* **The warnings about a program tidymedia cannot use now name the recovery your
  machine actually has, and can be caught.** A remembered location whose binary
  has gone offers `unset_program()` beside `set_program()` and carries
  `tidymedia_location_gone`; the not-found warning offers `install_on_win()` on
  Windows, for the three programs that installer registers, and not off Windows
  or for `mediainfo`. `program_status()` no longer swallows these: a remembered
  location that cannot be used would read exactly like a program you never had.

* **`install_on_win()` refuses on a platform it cannot install for**, before it
  downloads, writes or asks anything. It always installed a Windows build and
  only looked for `.exe` files in it; called on macOS or Linux it used to ask for
  consent and then download and unpack that build anyway. It now aborts with
  `tidymedia_wrong_platform`, naming where FFmpeg comes from there instead.

* **`install_on_win()` registers every program the archive produced, or none of
  them.** It used to register them one at a time, so a truncated `ffprobe.exe`
  was remembered as working and a build missing it altogether registered `ffmpeg`
  and then failed — overwriting, in both cases, whatever an earlier install had
  left. It now looks at every produced program first, refuses without changing a
  single remembered location where a required one cannot be used, and completes
  while naming what it skipped where an optional one cannot. A refusal also
  leaves the install directory as it found it: files a failed extraction wrote
  are removed and a directory the call created is removed again. On Windows a
  partly-written file cannot be removed while the extraction library holds it
  open, so the error names the leftovers by full path instead.

* **Remembered locations and installed builds now live where CRAN policy says
  they should.** A location set with `set_ffmpeg()` and its siblings lives under
  `tools::R_user_dir("tidymedia", "config")`, and `install_on_win()` installs
  under `tools::R_user_dir("tidymedia", "data")`. A location set before this
  change is still found: the lookups read the new directory first and the old one
  only when no file exists there. Nothing is moved or copied, and an FFmpeg
  installed by an earlier version keeps working.

* **Errors keep naming the function you called.** `set_ffmpeg("nope")` says
  `set_ffmpeg()` whether called at the console or from inside your own function,
  and the same holds for `set_program()`, the other `set_*()` functions and
  `hardware_encoder()`. A wrong argument to `has_hardware_encoder()` used to be
  reported as coming from `hardware_encoder()`, which you never called.

* `ffm_batch()`, and the `parallel = TRUE` path of `segment_video()` and
  `segment_video_batch()`, warns when parallel processing is requested but no
  parallel `future::plan()` is active. Such calls previously ran one job at a
  time with no speedup and no indication.

* An explicit `ffm_map()` on a multi-input pipeline is now emitted alongside the
  automatic `-map "[vout]"` instead of being silently ignored, so
  `ffm_map(p, "0:a")` keeps the first input's audio next to the stacked video.

* Test coverage is measured again: an empty `R/zzz.R` triggered a `covr` bug that
  silently reported 0% package coverage.

## Performance

* **Asking for hardware encoding queries FFmpeg for its encoder list once per R
  session instead of once per call.** A 500-row GPU batch previously started 500
  separate FFmpeg processes to re-read the same list before encoding anything;
  now it starts one. The compiled commands are unchanged. Two calls discard the
  remembered answer: the new `refresh_ffmpeg_capabilities()`, and `set_program()`
  (or `set_ffmpeg()`), which points tidymedia at a different binary.
  `ffmpeg_encoders()` and `ffmpeg_codecs()` are never remembered.

* **`probe_all()` and the `probe_*()` shortcuts read each file with a single
  FFprobe process** instead of one per stream plus one for the container. A
  five-stream file needed six processes and needs one; locally, probing ten
  copies of a four-stream file went from 1.7 seconds to 0.46. The returned
  tibbles keep the same columns, order, values and types.

* **`probe_all()` and the `probe_*()` shortcuts take a `parallel` argument**
  (default `FALSE`). With `parallel = TRUE` the per-file probes are spread across
  workers with the optional **furrr** package, following the active
  `future::plan()` — the same mechanism `ffm_batch()` uses, so one plan
  configures both. The output is unchanged either way, and files that cannot be
  probed still produce one warning at the end naming all of them.

## Documentation

* **The package has a landing help topic.** `?tidymedia` gives an overview of the
  three layers and the vignettes, is listed by `help(package = "tidymedia")` and
  appears on the reference index; previously neither reached anything. It carries
  a *Session options* section covering all three session options in one place.

* **A new `?audio_stream` help page** explains the two 0-based audio arguments
  and how they differ: `audio_stream` counts one input's audio tracks while
  `audio_input` counts the verb's inputs, so neither index can be read off the
  other. Every verb taking either argument links to it, and the verb lists inside
  those descriptions are generated from a single source so they cannot fall
  behind the code.

* **Two new vignettes and a reordered third.** `vignette("verification")` covers
  checking a processed file with `verify_media()` and `verify =`, recording how a
  run was made with `manifest = TRUE` and `ffm_manifest()`, and bounding a run
  that hangs. "A research preprocessing workflow" walks an end-to-end pipeline on
  a realistic dyadic-interaction study. "Get started" now leads with the task
  verbs before descending to the builder, and gains a section on choosing an
  audio track; every vignette cross-links to the others.

* **Help pages cross-reference each other**: every task verb links to the `ffm_*`
  builders it is built on and each builder back to the verbs that use it, and the
  three metadata reader families link to one another. Each metadata page states
  its backend and what it returns, and the metadata vignette gains a table
  comparing the reader families.

* **The install instructions in the README end at a check.** Each platform's route,
  under MediaInfo and under FFmpeg, finishes with a `program_status()` call, what
  a found and a not-found answer look like, and the call to make when the program
  was not found. The macOS manual FFmpeg route used to stop after dragging the
  program into the Applications folder, which is not on the `PATH`. README chunks
  and help-page examples that start a program now run only when it is installed.

* `citation("tidymedia")` returns a citation written for the package, carrying
  the package website, instead of the entry R generates from `DESCRIPTION`.

## Requirements

* **tidymedia states the R version it needs: `R (>= 4.1.0)`.** An installer on an
  older R refuses the package and says why, rather than installing something
  whose examples will not run — they use the native pipe `|>`, which R gained in
  4.1.0.

* **The declared dependency versions are measured rather than assumed**: the test
  suite has been run against the exact version each `Imports` entry names. One
  was wrong. `rlang` is now `(>= 1.2.0)`, up from `1.1.0`: tidymedia checks its
  arguments with `rlang::check_string()` and its siblings in well over a hundred
  places, and rlang first exports those in 1.2.0, so on an earlier rlang the
  verbs failed at their own front doors. The other nine floors stand unchanged.

* **Two new imports.** **digest** computes the SHA-256 of a downloaded FFmpeg
  archive; base R gained `tools::sha256sum()` only in 4.5.0, four releases above
  the R this package declares. **withr**, already a suggested package, is what
  `local_timeout()` uses to register its undo on the calling frame; it depends on
  nothing outside base R, and its declared minimum of 2.5.0 is the version it was
  tested against.

* **`DESCRIPTION` names the external tools the package interfaces.** FFmpeg and
  MediaInfo appear in `SystemRequirements`, each with its project URL, so the
  tools the package shells out to are visible to anyone reading its metadata.

* **The `Title` and `Description` say what tidymedia does** rather than restating
  its own name, naming the two batch jobs the package is built around and the two
  programs it drives. This is what an installer and a package index show before
  anyone opens the help pages.

* The built package no longer carries three paths it has no use for: two
  `inst/extdata/*_location.rds` files, which nothing has read since remembered
  locations moved to the user's configuration directory, and a
  `tests/testthat/_problems/` scratch directory left over from a local test run.

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
