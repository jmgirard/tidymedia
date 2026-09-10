# Changelog

## tidymedia (development version)

### Breaking changes

- Some exports have been renamed, and some removed. The package is
  pre-1.0 and still soaking, so old names are removed rather than
  deprecated: there are no `lifecycle` shims, and a call using an old
  name gets R’s usual `could not find function` error.

  - FFmpeg capability queries have moved out of the `get_*` namespace:
    `get_codecs()` is now
    [`ffmpeg_codecs()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_codecs.md)
    and `get_encoders()` is now
    [`ffmpeg_encoders()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md).
    `get_*` is reserved for per-file metadata getters.
  - The metadata getters have moved onto the package’s full-word
    vocabulary: `get_samplingrate()` is now
    [`get_sample_rate()`](https://jmgirard.github.io/tidymedia/reference/get_sample_rate.md)
    and `get_framerate()` is now
    [`get_frame_rate()`](https://jmgirard.github.io/tidymedia/reference/get_frame_rate.md).
  - `audio_as_mp3()` is now
    [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
    which names an audio codec instead of hard-coding one. The default
    (`audio_codec = NULL`) compiles `-q:a 0`, letting the output
    extension pick the codec at highest VBR quality; pass `audio_codec`
    to pin one.
  - `ffm()` and `mediainfo_summary()` are gone. Each was a second
    exported name for a function that already had one: use
    [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md)
    and
    [`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md),
    which are unchanged. Renaming the calls is the whole migration.
  - Five reexported tidy-eval quoting helpers — `enquo()`, `enquos()`,
    `as_label()`, `as_name()` and `:=` — are no longer exported; nothing
    in the package’s own interface needed them. `.data` remains
    reexported. Two internal helpers that were exported by accident,
    `pad_integers()` and `convert_fractions()`, are internal again.

- Three arguments have been renamed.
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)’s
  codec argument `acodec` is now `audio_codec`, and
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)’s
  time bounds `ts_start` and `ts_stop` are now `start` and `end` — in
  each case the name the rest of the package uses for the same thing.
  Rename them at your call sites: no alias is kept, and neither verb
  takes `...`, so an old name is an unused-argument error rather than a
  silently ignored one.

- [`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  and its wrappers
  [`set_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/set_program.md),
  [`set_ffprobe()`](https://jmgirard.github.io/tidymedia/reference/set_program.md),
  [`set_ffplay()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  and
  [`set_mediainfo()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  now ask before they write. Each takes a new `confirm` argument, `TRUE`
  by default. The prompt names the location as you typed it – which is
  the string that gets written – and the full path of the
  `<program>_location.txt` file that would record it; declining leaves
  the config directory exactly as it was, creating nothing. In a session
  with no one to ask, the call now aborts with
  `tidymedia_confirmation_unavailable` rather than assume consent, which
  is the same contract
  [`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
  has. This breaks unattended scripts that call these functions: pass
  `confirm = FALSE` to write without being asked.
  [`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
  passes it internally, so an approved install still asks exactly once.
  A call in `.Rprofile` is a third case: the session counts as
  interactive there, so the call now prompts while R starts up rather
  than refusing – pass `confirm = FALSE` there too.

  These five functions now return `TRUE` or `FALSE`, invisibly, saying
  whether the location was written. They previously documented a logical
  and returned whatever clearing the capability memo returned, which was
  `NULL`. A location with no executable at it now aborts with
  `tidymedia_program_not_found`, and the error names the function you
  called rather than being unclassed.

- [`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
  asks before it downloads or installs anything. The prompt names the
  archive it will fetch, the directory it will unpack into, and the
  remembered program locations it may overwrite; declining returns
  `FALSE` and leaves every one of them untouched — no directory is
  created, nothing is downloaded, and no remembered location changes. In
  a session with no one to ask, the call now aborts rather than proceed
  as if consent had been given, and the abort names the same archive,
  directory and locations the prompt would have. This breaks unattended
  scripts: pass the new `confirm = FALSE` to install without being
  asked.

- **Every verb that carries audio now states which audio tracks it
  takes, instead of leaving the choice to FFmpeg.** A verb that emitted
  no stream mapping got FFmpeg’s own rules — one stream of each type,
  preferring whichever audio track carries the container’s “default”
  flag — so on a multi-track input the surviving track depended on the
  input’s flags and could differ between FFmpeg versions on the same
  file, which is exactly the kind of invisible variation this package
  exists to remove. Every affected verb now maps its audio explicitly,
  and a new `audio_stream` argument names a single track when you want
  one (see *New features*). What changes for you depends on the verb,
  because they did not all start from the same place.

  **The verbs that now keep every track.**
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  at its default `reencode = TRUE`, and
  [`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md)
  emitted no mapping and so kept one track; they now keep all of them.
  Which track survived was the input’s business rather than yours: on a
  three-track test file flagged on its second track,
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  and
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  each kept that second track and discarded the other two in silence,
  while
  [`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md),
  measured on a three-track file flagged on its third, kept only the
  third. All three tracks now survive in either case. If you process
  multi-track sources, your outputs will gain tracks they used to lose,
  and grow accordingly.
  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md)
  and `segment_video(reencode = FALSE)` mapped every stream already, so
  that part is unchanged for them.

  **The verbs that now take the first track.**
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  and
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  write exactly one audio stream and have to pick one; they now map the
  input’s **first** audio track. On a multi-track input whose *second*
  track is flagged as the default, the extracted audio changes — you
  would have got that second track before and get the first one now.
  Pass `audio_stream = 1` to keep the old result on such a file.
  Single-track inputs are unaffected.
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and
  [`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md)
  also stop failing outright on multi-track input: they mapped *every*
  audio stream, so a file carrying several handed several streams to a
  format that accepts only one, FFmpeg aborted
  (`Exactly one MP3 audio stream is required`) and a zero-byte file was
  left behind.

  **Subtitle and data streams are no longer carried.**
  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  `segment_video(reencode = FALSE)`,
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  and
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  used to drag them along wherever the container accepted them: writing
  to `.mkv` passed one subtitle through and now passes none. Writing to
  `.mp4`, the common case, is unaffected — that container was already
  dropping them — and this also fixes a real failure, since
  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md)
  writing a subtitle-bearing input to `.mp4` used to abort outright
  (FFmpeg has no default subtitle encoder for that container) and now
  succeeds. Extracting to an audio-only container such as `.aac`, `.m4a`
  or `.mka` is unaffected, because those never carried a subtitle track
  in the first place.

  **[`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  has become an audio-producing verb.** It now writes **one audio stream
  and no video**, whatever container you name for the output, keeping
  the **first** audio track — matching
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  and
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  rather than the pass-through verbs. That is not a narrowing of which
  track: the verb already produced a single audio track, just an
  unpredictable one. It reads an unset `audio_stream` this way because
  measuring loudness produces one measurement per audio track while the
  correction applies a single set of values, so normalizing several
  tracks at once would silently apply the first track’s measurements to
  all of them; under `two_pass = TRUE` the measurement pass now measures
  exactly the track the correction pass normalizes. Normalizing every
  track independently would need per-track filter settings the pipeline
  builder does not have, and is not offered. Two consequences worth
  reading before you upgrade:

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

  Naming a track the input does not have remains an FFmpeg error rather
  than an R one, on every verb. Each argument’s documentation says which
  family it belongs to.

- [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
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
  `-codec:a copy` wherever they map audio at all — on
  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md)
  and
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  always, and on
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  and
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
  once `audio_input` names an input to take it from, since those two map
  no audio at their default `audio_input = NULL`. The new `audio_codec`
  argument controls this: `"copy"` is the default, an encoder name
  (`audio_codec = "aac"`) transcodes instead, and `audio_codec = NULL`
  restores the old behavior of leaving the codec unset. Note that a
  stream copy fails if the output container cannot hold the source audio
  codec (FLAC in `.mp4`, say) — name an encoder in that case. Cutting
  with `segment_video(reencode = FALSE)` stream-copies the video and
  audio it carries, so any `audio_codec` other than `"copy"` is an error
  there, as is naming an audio encoder on a composite that carries no
  audio at all.

- [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  and
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md)
  now stream-copy by default, and name an encoder per output file.
  Separation is lossless and fast this way, but each output container
  must support the source codec. The new `audio_codec` and `video_codec`
  arguments each govern only their own output file: both default to
  `"copy"`; `audio_codec = NULL, video_codec = NULL` re-encodes both;
  and a codec name (`audio_codec = "libmp3lame"`) transcodes that stream
  alone. In a jobs table both may be per-row columns where `NA` means
  “leave that stream’s codec unset”. Because each input row fans out
  into an audio row and a video row, the returned table collapses the
  two into one `codec` column carrying each row’s encoder for its own
  stream.

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
  position, and neither helper has an answer for it.

- Several new arguments are placed where they belong rather than
  appended for compatibility, in line with the package’s pre-1.0
  clean-break policy, so **calls that pass later arguments by position
  rather than by name must be updated**. Naming your arguments avoids
  the problem entirely.

  The inserted arguments are drawn from `video_codec`, `audio_codec`,
  `hardware`, `fallback` and `audio_stream`, each placed beside the
  argument it belongs with. `run` has moved on all five verbs that
  carried it before, and by more than one position on four of them:

  | verb | `run` was at position | `run` is now at position |
  |----|----|----|
  | [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md) | 4 | 5 |
  | [`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md) | 3 | 6 |
  | [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md) | 4 | 9 |
  | [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md) | 6 | 11 |
  | [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md) | 7 | 12 |

  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)’s
  `parallel` moves with it, from position 7 to position 12.
  `extract_audio(video, "audio.aac", "copy", FALSE)` now reads `FALSE`
  as the audio-stream index rather than as `run` — an error rather than
  a silent misread, since the index must be a whole number. On
  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md)
  and
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  the old `run` slot now holds `video_codec`, so a positional `TRUE`
  there stops with
  `` `video_codec` must be a single string or `NULL`, not `TRUE`. ``
  rather than being misread.

- [`ffm_map()`](https://jmgirard.github.io/tidymedia/reference/ffm_map.md)
  appends instead of overwriting. Calling it twice on the same pipeline
  used to discard the first mapping; it now keeps both, emitting one
  `-map` per mapping in the order given, which is what lets a pipeline
  keep the video and then name one audio track. `mapping` may now be a
  character vector for the same reason. Pass `replace = TRUE` to get the
  old discard-what-came-before behavior, which is how you narrow the
  all-streams mapping that
  [`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
  sets.

  [`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
  **sets** the all-streams mapping rather than adding to it, so calling
  it twice no longer duplicates every output stream. Since the mapping
  builder began appending, `ffm_copy() |> ffm_copy()` compiled `-map 0`
  twice and a one-video/one-audio input came out with four streams;
  `ffm_concat() |> ffm_copy()` did the same, because concatenation
  copies internally. No pipeline built by a task verb was affected —
  this only reached you if you composed the builder yourself. If the
  pipeline already states a *different* mapping,
  [`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
  now stops with an error rather than discarding it silently. Pass
  `streams = FALSE` to keep the mapping you set, or call
  [`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
  first and narrow afterwards with `ffm_map(replace = TRUE)`.

- Three Layer 1 builders refuse values they used to accept.
  [`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md)
  and
  [`ffm_pixel_format()`](https://jmgirard.github.io/tidymedia/reference/ffm_pixel_format.md)
  reject anything that is not a single clean token — no whitespace or
  shell metacharacters, and starting with a letter or digit.
  [`ffm_output_options()`](https://jmgirard.github.io/tidymedia/reference/ffm_output_options.md)
  rejects option groups containing quote characters: options are split
  on whitespace into arguments at execution, so quoting cannot group
  tokens, and such commands previously executed with a different meaning
  than the one printed. Option values themselves must not contain
  spaces.

- [`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md)
  — and every task verb built on it — now aborts with FFmpeg’s exit
  status when an encode fails, instead of returning silently; the old
  shell path only emitted a warning.
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  still records failures in its `success` column without aborting.

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

### New features

- **[`ffm_jobs()`](https://jmgirard.github.io/tidymedia/reference/ffm_jobs.md)
  turns a directory into a batch jobs table.** It lists the files in a
  directory whose names do not start with a dot and that carry one of
  the extensions it knows for a given media type, and returns them as
  the tibble
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  takes: one row per file, with the file’s full path in an `input`
  column, and every row a path that exists and is not a directory —
  neither a subdirectory whose name ends in a matching extension nor, on
  macOS and Linux, a symbolic link whose target is gone (Windows reports
  such a link as existing, so there it can still be a row). `type` names
  the category to list — `"video"`, `"audio"` or `"image"` — and has no
  default. `extension` narrows within that type (`"mp4"` and `".mp4"`
  both work, matched case-insensitively), and `recursive = TRUE`
  descends into subdirectories, including through a symbolic link to a
  directory, so a row can then name a file outside the one given. A
  directory that does not exist, a type outside the three, and a call
  that matches no file are each an error naming
  [`ffm_jobs()`](https://jmgirard.github.io/tidymedia/reference/ffm_jobs.md).
  The returned table carries `input` and nothing else, because
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  passes every column of the jobs table to `.f` by name. Six of the
  fifteen `*_batch()` task verbs take that table unaltered:
  [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
  [`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
  [`format_for_web_batch()`](https://jmgirard.github.io/tidymedia/reference/format_for_web_batch.md)
  and
  [`strip_metadata_batch()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata_batch.md)
  derive their own output from `input`, and
  [`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md)
  and
  [`sample_frames_batch()`](https://jmgirard.github.io/tidymedia/reference/sample_frames_batch.md)
  do too once you pass their arguments. The other nine refuse it until
  you add the columns their own task needs:
  [`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md)
  and
  [`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md)
  ask for `output`,
  [`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md)
  for `main`, `overlay` and `output`, and the rest name what they work
  on, such as `start` and `end`, `regions`, `inputs`, `timestamp`, or
  `audiofile` and `videofile`.

- **Six new task verbs.**

  - [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
    re-encodes a video to a reproducible, analysis-friendly format in
    one call. By default it produces H.264 video with `yuv420p` and
    `+faststart`, stream-copies the audio unchanged, and keeps the
    source resolution and frame rate (rounding odd dimensions down to
    the nearest even value so the codec can encode), so the same input
    always yields a byte-identical command. Supply `width`/`height` to
    set the output size (giving only one preserves the aspect ratio with
    an even output dimension), `fps` to resample the frame rate, and
    `video_codec`/`pixel_format` to override the codec or pixel format.
  - [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
    normalizes a file’s perceived loudness to an EBU R128 target using
    FFmpeg’s `loudnorm` filter. By default it targets -23 LUFS
    integrated loudness with a -1 dBTP true-peak ceiling (EBU R128,
    measured per ITU-R BS.1770-4) and preserves the source channel
    layout, so the same input always yields one reproducible command.
    Supply `target_loudness`, `true_peak` and `loudness_range` to
    retarget, and `channels`/`sample_rate` to downmix or resample. Note
    that single-pass `loudnorm` resamples its output (up to 192 kHz,
    encoder-capped), so set `sample_rate` to pin the output rate.
  - [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
    covers one or more fixed rectangular regions of a video with opaque
    filled boxes — for redacting a face, a name badge, or any area that
    stays in one place for the whole clip (there is no motion tracking).
    Regions are given as a data frame of `x`, `y`, `width`, `height`
    (numbers or FFmpeg expressions), with an optional per-row `color`.
    The video is re-encoded reproducibly (H.264 / `yuv420p` by default)
    and audio is stream-copied unchanged.
  - [`strip_metadata()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata.md)
    removes a file’s container and global metadata tags — creation time,
    GPS and location, device make and model, title, comment and the like
    — together with any chapters, writing a de-identified copy. It is
    the front door for IRB de-identification of research recordings, and
    the metadata sibling of
    [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
    which redacts the picture. The streams are **stream-copied**, so the
    operation is lossless and fast and the picture and sound are
    bit-for-bit unchanged, including any rotation display matrix, which
    is stream side data rather than a metadata tag. The output is muxed
    bit-exactly (`-fflags +bitexact`) so FFmpeg does not re-stamp the
    container with a fresh `creation_time` or an `encoder` tag naming
    its own version, either of which would defeat de-identification.
    Because the streams are copied, identifiers embedded *inside* the
    encoded bitstream, and per-stream metadata such as `handler_name` or
    `language`, are not removed; removing those would need a re-encode
    (use the
    [`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
    escape hatch).
  - [`sample_frames()`](https://jmgirard.github.io/tidymedia/reference/sample_frames.md)
    samples a video at a fixed rate (`fps`) or interval (`interval`,
    seconds between frames) into a numbered image sequence — the front
    door to per-frame coding and computer-vision feature pipelines.
  - [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
    transcodes an audio stream to the codec you name, or lets the output
    extension pick one at highest VBR quality. It replaces
    `audio_as_mp3()` (see *Breaking changes*).

  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  and
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
  join them as the two fan-in task verbs: a side-by-side or stacked
  comparison video, and an inset overlay (a `position` naming one of the
  four corners or `"center"`, plus `scale` and `margin`). Both drop
  audio by default: `audio_input` is the 0-based index of the *input*
  whose audio to keep — `0` the first file passed in, `1` the second —
  and counts the verb’s inputs, not one input’s audio streams, so it is
  a different index from `audio_stream` on the single-input verbs.
  `NULL`, the default, maps no audio at all; naming an input the call
  does not have is an R error raised before FFmpeg runs. On the `_batch`
  siblings an `audio_input` column overrides the argument row by row, an
  `NA` cell meaning what `NULL` means to the scalar.

- **Two-pass loudness normalization.**
  `normalize_audio(two_pass = TRUE)` runs an analysis pass to measure
  the input’s loudness, then a linear correction pass that feeds those
  measurements back, hitting the EBU R128 target far more precisely than
  the single-pass default on material with a wide loudness range.
  Because it must measure the input, two-pass always calls FFmpeg — even
  under `run = FALSE`, where the analysis still runs and the returned
  value is the exact correction command, left unexecuted. The
  single-pass default is unchanged and stays binary-free under
  `run = FALSE`.

  `normalize_audio_batch(two_pass = TRUE)` applies the same
  measured/linear normalization across a whole jobs table. It measures
  every input (honoring `parallel` and each row’s targets), then builds
  and runs one linear correction per row, surfacing the five measured
  values as
  `measured_I`/`measured_TP`/`measured_LRA`/`measured_thresh`/`offset`
  columns. As with the scalar verb the analysis pass always runs, and
  `two_pass` is a whole-table switch rather than a per-row column.

  Digitally silent input is handled honestly. Silence measures as `-inf`
  loudness, which cannot be normalized to a target, so
  `normalize_audio(two_pass = TRUE)` aborts with a message naming
  silence as the cause. `normalize_audio_batch(two_pass = TRUE)` does
  not let one silent row abort the whole batch: the non-silent rows are
  normalized, the silent rows are marked in a logical `silent` column
  (with `success = FALSE` and no output written), and a warning names
  them. Genuine analysis failures still abort fail-fast, and near-silent
  but non-empty audio is unaffected. The batch’s result schema does not
  depend on how many rows are silent: when `verify` or `manifest` is
  requested, the `verified` column and the provenance manifest are
  returned even when *every* row is silent, silent rows carrying `NA`
  for those outputs.

- **Batch siblings for every transform verb.** Each takes a jobs tibble
  with one row per unit of work and is a thin wrapper over
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md),
  so `...` forwards batch options such as `verify`, `manifest`,
  `checksums`, `progress` and `parallel`, and each row compiles to a
  command byte-identical to the equivalent scalar call. Where a scalar
  argument can sensibly vary per row it may also appear as a `jobs`
  column, which overrides the argument row by row.

  - Single-input transforms:
    [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
    [`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md),
    [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
    [`strip_metadata_batch()`](https://jmgirard.github.io/tidymedia/reference/strip_metadata_batch.md),
    [`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md),
    [`crop_video_batch()`](https://jmgirard.github.io/tidymedia/reference/crop_video_batch.md),
    [`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md),
    [`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md),
    [`format_for_web_batch()`](https://jmgirard.github.io/tidymedia/reference/format_for_web_batch.md),
    [`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md),
    [`sample_frames_batch()`](https://jmgirard.github.io/tidymedia/reference/sample_frames_batch.md)
    and
    [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md).
  - Fan-in transforms, whose rows name **many** inputs each and so carry
    an `inputs` list-column plus a required `output` column:
    [`concatenate_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos_batch.md)
    and
    [`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md).
  - [`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md),
    whose two inputs have distinct roles and so carries fixed `main` and
    `overlay` columns rather than a list-column, plus a required
    `output` column.

  The `output` column is optional on the verbs that can derive a name.
  Six auto-name one output per input by suffixing its basename —
  `_standardized`, `_normalized`, `_anonymized`, `_stripped`, `_cropped`
  and `_web.mp4` — keeping the source extension, except the web
  re-encode, which always writes `.mp4`.
  [`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md)
  and
  [`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md)
  write many files per input instead, so each appends a zero-padded
  `<basename>_<n>` that restarts at every input file: the source
  extension for a segment, the image `format` for a frame.
  [`sample_frames_batch()`](https://jmgirard.github.io/tidymedia/reference/sample_frames_batch.md)
  names a directory rather than a file — given neither an `outdir`
  column nor the argument, it writes each input’s numbered sequence into
  a `<basename>_frames` directory beside that input. Ten of them reject
  two rows that resolve to the same output path, so one file cannot
  silently overwrite another. Three —
  [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md),
  [`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md)
  and
  [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md)
  — instead refuse two rows naming the same input when no `output`
  column is given, and do not check an `output` column that repeats a
  path.
  [`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md)
  and
  [`extract_frame_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_frame_batch.md)
  check neither: they number their outputs per input, so derived names
  cannot collide, but a repeated `output` column is accepted and the
  second row overwrites the first. The two audio verbs, the fan-in verbs
  and
  [`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md)
  require an `output` column and derive nothing: an audio destination’s
  extension picks the output format, and a row naming many inputs has no
  single basename to build from.
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md)
  requires two destination columns, `audiofile` and `videofile`, and
  derives neither, because a copied stream’s container extension has to
  match the codec it carries.

- **Checking a result, and recording how it was made.**
  [`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md)
  is a probe-backed checker that confirms an output really has the
  properties you asked for. It returns a tidy tibble with one row per
  check (`file`, `check`, `expected`, `actual`, `pass`) covering
  `duration`, `width`, `height`, `video_codec`, `audio_codec` and
  `sample_rate`, plus any other FFprobe field passed by name through
  `...`. Numeric checks use an absolute `tolerance` (default `0.1`, so
  integer dimensions match exactly while duration gets a little slack);
  codec checks match exactly.

  Verification is wired into execution. `ffm_run(verify = <named list>)`
  probes the output after a successful run and aborts, listing the
  failed checks, if any assertion fails.
  `ffm_batch(verify = <list or function>)` instead records the outcome
  in a logical `verified` column (one spec for all jobs, or a
  `pmap`-style function of the job columns) without aborting.

  `ffm_batch(manifest = TRUE)` attaches a per-job provenance record —
  command, FFmpeg/FFprobe versions, timestamp and output size — read
  back with
  [`ffm_manifest()`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md),
  which can also write it to CSV via `path =`. `checksums = TRUE`
  additionally records input and output md5 checksums. And
  `ffm_batch(progress = TRUE)` shows a `cli` progress bar as the jobs
  run, following the `future` plan on the parallel path.

- **`audio_stream`: naming which audio track to work on.** Nine scalar
  verbs and their `_batch` siblings take this argument — a 0-based index
  counted among the input’s audio streams, so `audio_stream = 1` is the
  second audio track whatever its position among the file’s streams. In
  a jobs table it may be a per-row column, where `NA` in a cell is the
  per-row form of leaving the argument unset.

  What leaving it unset means depends on what the verb writes, and the
  difference is deliberate.
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and
  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  write exactly one audio stream and take the **first** track. The verbs
  that pass audio through —
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md),
  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
  [`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md)
  and
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  — keep **every** track. Each function’s documentation says which it
  does and names the ones that do the other; see
  [`?audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md).

  One trap is worth knowing about:
  [`probe_audio()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)’s
  `index` column counts *all* of a file’s streams, while `audio_stream`
  counts only its audio streams. On a video file with three audio tracks
  those read `1, 2, 3` and `0, 1, 2` respectively, so reading a number
  off
  [`probe_audio()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)
  and passing it straight to `audio_stream` lands you one track off.

- **A warning when tracks are being dropped.**
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md),
  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  and their `_batch` siblings warn when the file they read carries audio
  tracks the file they write will not. Each of these verbs takes exactly
  one track, so feeding a three-track recording to
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  without saying which track you want quietly discarded two of them. The
  warning says so, tells you how many went, and points at `audio_stream`
  for choosing a different one; naming a track stops it, and it can be
  suppressed by class with
  `suppressWarnings(classes = "tidymedia_dropped_audio")`. The batch
  verbs warn **once** for the whole table, naming every affected row. On
  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  the check lands before the two-pass analysis pass, so on a multi-track
  input the warning arrives while adding `audio_stream` can still save
  that pass.

  Counting the tracks means running FFprobe, so the check is
  **best-effort**: it is made when FFprobe is available and the input
  can be probed, and skipped silently otherwise. It never runs under
  `run = FALSE` — compiling a command still touches no binary — and it
  never changes the command that gets compiled.

  `options(tidymedia.check_tracks = FALSE)` stops the check for the rest
  of the session; it defaults to TRUE, so nothing changes until you set
  it. What you get back is the check’s only cost: one FFprobe call per
  distinct input, run before the work starts and, on the `_batch` verbs,
  serially at the front door before the fan-out. That is worth declining
  on a large batch whose inputs you already know the tracks of, where
  the warning has nothing to tell you; a row that names an
  `audio_stream` is never probed, so a table whose rows all name one
  costs nothing either way. Use
  `withr::local_options(tidymedia.check_tracks = FALSE)` to switch it
  off for the rest of one function instead of the session. The option is
  carried into `parallel = TRUE` workers, and a value that is not `TRUE`
  or `FALSE` is refused, naming the option, rather than read as one or
  the other.

- **[`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  explains itself when the audio container is the problem.** Most audio
  containers (`.aac`, `.mp3`, `.wav`) hold exactly one stream, so
  separating a three-track recording into one of them failed with
  FFmpeg’s own message and a zero-byte file — with nothing to say that
  the track count was the problem, or that there was any way around it.
  The error now states how many tracks the input carries and names both
  ways out: `audio_stream` to write one of them, or a container such as
  `.mka` or `.m4a` to keep them all.
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md)
  cannot abort one row without abandoning the rest of the table, so it
  records that row as `success = FALSE` and warns **once** when the
  batch finishes, naming every affected input row; suppress it with
  `suppressWarnings(classes = "tidymedia_multitrack_separation")`. The
  count comes from FFprobe, so the explanation is best-effort, runs only
  after FFmpeg has already failed and only on a real run, and never
  changes the compiled command. Naming a track skips it entirely — with
  one track mapped, a failure is something else and a track count would
  not explain it.

- **Codec arguments on every transform verb, spelled the same way.**
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
  and
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  gain `audio_codec`;
  [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md),
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  and
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
  gain `video_codec` alongside the `hardware`/`fallback` toggle;
  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  gains `audio_codec` naming the output audio encoder, since loudness
  normalization filters the audio and so must re-encode it — until now
  to whatever encoder your FFmpeg build defaults to for the output
  container, which made the result depend on the machine. On
  [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  the default `NULL` leaves the codec unset and `"copy"` is an error,
  since a filtered stream cannot be copied. Each of these is available
  as a per-row `jobs` column as well as an argument.

  `NULL` now means the same thing on every codec argument in the
  package, and `NA` the same thing in every per-row codec column.
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

  No existing command changes: a call passing neither `NULL` nor a
  column `NA` compiles exactly what it compiled before, and the calls
  that changed are ones that used to abort and now compile. A *scalar*
  `NA` is still an error everywhere: `NA` spells “unset” only as a
  column cell, where a per-row table has no other way to say it.
  [`convert_audio()`](https://jmgirard.github.io/tidymedia/reference/convert_audio.md)
  and
  [`convert_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/convert_audio_batch.md)
  stay the deliberate exception — `NULL` and a column `NA` there select
  `-q:a 0`, highest VBR quality, as their documentation says.
  `pixel_format` and `color` columns still reject `NA`, having no unset
  state to spell.

- **Opt-in hardware video encoding, as a vocabulary of backends rather
  than one vendor.** Sixteen verbs take a `hardware` argument.
  `hardware = "nvenc"` encodes on an NVIDIA GPU and
  `hardware = "videotoolbox"` on Apple silicon, so a Mac encodes on the
  hardware it has. Each backend covers the codec families it has
  encoders for: nvenc covers h264, hevc and av1; videotoolbox covers
  h264 and hevc. The encoder is named from the family and the backend,
  so `video_codec = "libx264"` resolves to `h264_nvenc` under one and
  `h264_videotoolbox` under the other. Asking a backend for a family it
  has no encoder for — nvenc has no `prores`, videotoolbox has neither
  `prores` nor `av1` — is an error naming the backend and the family,
  and is refused whatever `fallback` is set to. That is a wrong argument
  rather than a machine missing something, which is what `fallback` is
  about, so no setting of it opens a path the table does not hold.
  `hardware = "none"` is the default, so a call that does not ask for
  hardware is unchanged. The separate case `fallback` does cover is an
  encoder this backend has but *your* FFmpeg build does not list: by
  default that is an error too, so output stays reproducible, and
  `fallback = TRUE` re-encodes in software with a message instead,
  saying which backend it fell back from.

  [`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
  reports whether a backend’s encoder for a codec family is available in
  your FFmpeg build, and
  [`hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
  names it; both take the backend as a second argument (see *Breaking
  changes*). `options(tidymedia.hardware_encoders = )` overrides
  detection outright. Hardware *decoding* and GPU filter pipelines
  remain out of scope — use
  [`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
  for those.

  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  and
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md)
  gain `hardware` and `fallback` too, so a video stream being re-encoded
  on the way out can go to the GPU. Only the video output is affected —
  the audio file is byte-for-byte what it would have been otherwise.
  Because this verb copies the video by default, and a copy runs no
  encoder at all, `hardware = "nvenc"` on its own is an error rather
  than a silent switch from a lossless copy to a GPU re-encode: pair it
  with `video_codec = NULL`, which assumes the H.264 family, or name a
  codec (`video_codec = "libx265"`) to pin a different one — a non-H.264
  container such as `.webm` needs that explicit name. As on the other
  verbs `hardware` applies to a whole batch rather than row by row, so a
  jobs table mixing copied and re-encoded video must be split into
  separate calls.

- **A wall-clock limit on the programs tidymedia starts.** Setting
  `options(tidymedia.timeout = 600)` gives every FFmpeg, FFprobe and
  MediaInfo process tidymedia starts a limit in whole seconds. The
  default is `0`, meaning no limit, so existing code is unaffected and a
  legitimate multi-hour encode still runs to completion. The limit
  applies to each spawned program rather than to a batch as a whole, and
  tidymedia’s own `parallel = TRUE` paths are bounded by the same limit
  as their sequential ones.

  A reached limit is never silent: every call that can start one of
  those programs either aborts or warns. The task verbs,
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
  jobs the limit gave up waiting for. The dropped-track check warns that
  it could not check, and the provenance manifest warns that it could
  not read a version. Those two lists are not written from memory: a
  test derives the calls that can start one of these programs from the
  package’s own call graph and drives a timeout through each of them.
  Where the call knows its own output — the task verbs and
  [`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md)
  — any partial file the killed run had written is removed just as it is
  after any other failed run; the raw
  [`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
  escape hatch does not parse the argument string it is given, so it
  leaves the partial file in place.

  `with_timeout(expr, seconds)` puts the limit on one call without
  changing the limit the rest of your session runs under. Every program
  started while `expr` is being evaluated is waited for at most
  `seconds`, and when the call ends — by any route, a failure or a
  reached limit included — whatever the session had set before is back,
  an unset option included. It reaches a `parallel = TRUE` fan-out too,
  because the worker is handed the limit in force when the fan-out
  starts. `0` means no limit, so `with_timeout(expr, 0)` lifts a
  session-wide limit for one call.

  `local_timeout(seconds)` is the statement form of the same limit: it
  bounds the rest of the function you call it from, rather than an
  expression you wrap. When that function ends — by any route — the
  caller’s value is back, unless the function discards the undo by
  writing an [`on.exit()`](https://rdrr.io/r/base/on.exit.html) of its
  own without `add = TRUE`. Two calls in one function stack the way any
  pair of `local_*()` calls does. Reach for it when the thing to bound
  is the rest of a function body, or several calls that would be awkward
  to wrap together.

  Both refuse a value the underlying limit could not use — a fraction of
  a second, a negative number, `NA`, a string — before `expr` runs,
  naming `seconds`, and
  [`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
  refuses an omitted `expr` itself, saying which argument is missing,
  rather than letting R report a missing parameter of the function’s own
  definition. Neither refusal disturbs the session-wide limit.

  **A limit can be exceeded, and by how much is measured.** The limit
  says how long tidymedia waits for a media program, not how long that
  program is allowed to run. When it is reached R asks the program to
  stop, insists 20 seconds later, and kills it 20 seconds after that, so
  a program that answers none of the three is waited for up to 40
  seconds longer than you asked. Under a 2-second limit, an FFmpeg
  blocked reading a pipe nobody writes to returned at 42.0 seconds on
  Linux, and a shell child that ignores both signals returned at 42.0
  seconds on Linux and macOS alike. Plan for it when you pick a limit: a
  1-second limit across five hung files is three and a half minutes of
  waiting, not five seconds. How much of the lag you see depends on your
  FFmpeg — the same blocked input took 42.0 seconds against FFmpeg 6.1.1
  and 2.0 seconds against 9.0.1, which answers the first signal — and R
  does not promise the program dies at all: one can be written to
  survive every signal R sends.

- **A `parallel = TRUE` call runs its workers under the tidymedia
  settings you set in your own session.** Previously each worker started
  from its own empty option list, so `options(tidymedia.timeout = )`
  bounded a sequential batch and left the parallel one unbounded, and
  `options(tidymedia.hardware_encoders = )` steered a sequential build
  while each worker ignored it and asked FFmpeg for its own encoder
  list. All three tidymedia options are now carried into each worker for
  the duration of the call, and whatever that worker had set for itself
  is put back afterwards — including when the call fails. What is still
  not carried is the remembered answer about your FFmpeg build itself: a
  worker with no `tidymedia.hardware_encoders` override still asks its
  own binary once. See
  [`?tidymedia`](https://jmgirard.github.io/tidymedia/reference/tidymedia-package.md)
  and
  [`?refresh_ffmpeg_capabilities`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md).

- **A failed FFmpeg run is something you can catch.** When FFmpeg exits
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

  The status is whatever
  [`system2()`](https://rdrr.io/r/base/system2.html) reported: for a
  signal-terminated FFmpeg that is the shell’s 128-plus-signal number,
  passed through unchanged, rather than a value FFmpeg chose.

  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)’s
  multi-track diagnostic carries the class too, as well as its own
  `tidymedia_multitrack_separation`, so an exit-status handler catches
  it like any other refused run while a handler written for the
  multi-track case still catches only that. Two paths deliberately do
  not signal it. The
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  family records `success = FALSE` for a failed row instead of aborting.
  And the `loudnorm` analysis pass behind
  `normalize_audio(two_pass = TRUE)` raises
  `tidymedia_loudnorm_no_measurement` — meaning the analysis yielded no
  usable measurement, so no correction could be built:

  ``` r

  tryCatch(
    normalize_audio("input.wav", "out.m4a", two_pass = TRUE),
    tidymedia_loudnorm_no_measurement = function(cnd) NA_character_
  )
  ```

  `normalize_audio_batch(two_pass = TRUE)` raises the same class, so a
  handler written from either help page fires on the other, and one path
  could not be caught by any name before: a scalar analysis pass that
  exits zero and prints no parseable measurement block. The class rides
  alongside `tidymedia_ffmpeg_exit` where FFmpeg exited non-zero and
  alone where it did not, so an exit-status handler still sees exactly
  the runs FFmpeg refused. A silent input is deliberately not this event
  — it was measured, at `-inf` — and keeps its own abort.

  Where a batch diagnostic cannot report one exit status, it now reports
  the numbers it does have. `normalize_audio_batch(two_pass = TRUE)`’s
  condition carries `tm_rows`, the 1-indexed offending rows the message
  names, and `tm_row_status`, their exit statuses aligned to it, `NA`
  where the row exited zero; those numbers used to be discarded, so the
  only account of why a row failed was the prose. Both `_batch` verbs
  say why they carry no single status:
  [`?normalize_audio_batch`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md)
  explains that its abort fires for rows that exited zero too, and
  [`?separate_audio_video_batch`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md)
  that the batch runner records whether a row succeeded, not how FFmpeg
  exited, so the number is gone by the time the warning is assembled.

- **[`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
  checks what it downloaded before it changes anything**, and remembers
  a program’s location only if the archive actually contained that
  program. On the build tidymedia fetches by default, it downloads the
  SHA-256 digest gyan.dev publishes beside the archive — before the
  archive itself, so a source that cannot produce one refuses in a
  second rather than after a long download — and refuses to unpack
  anything whose digest does not match. For a build you name yourself,
  pass its digest as the new `archive_checksum`; without one, the call
  installs as before but says the archive was not verified. Note that
  the digest travels from the same host over the same connection as the
  archive, so this catches a corrupted or truncated download, not a
  substituted one.

  Its failures carry conditions you can catch by class, where they used
  to escape as base R and libarchive text: a download that did not
  deliver, a digest that could not be fetched or read, a digest that did
  not match, an archive that could not be unpacked, and a required
  program the archive did not contain. `ffmpeg` and `ffprobe` are
  required — a build missing either leaves every remembered location
  untouched — while `ffplay` is optional, and an install without it
  succeeds and says so. The temporary download is removed whether the
  install succeeds or fails.

- [`program_status()`](https://jmgirard.github.io/tidymedia/reference/program_status.md)
  reports all four programs tidymedia knows about – `ffmpeg`, `ffprobe`,
  `ffplay` and `mediainfo` – in one table: where each one resolved to
  and what version it reported, with `NA` in both columns for a program
  that could not be found. A program it cannot find is reported rather
  than warned about, so checking a fresh setup is one call and one table
  rather than four calls and a pile of messages.

- [`unset_program()`](https://jmgirard.github.io/tidymedia/reference/unset_program.md)
  forgets a location
  [`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  remembered, so lookups go back to answering from the `PATH`. It clears
  both places a location can live: the current configuration file and
  one written by an earlier version of tidymedia, so a location
  remembered before the upgrade is forgotten too. The program must be
  named – the call deletes a file, so there is no default. Called for a
  program with nothing remembered, it warns and returns `FALSE` rather
  than failing. It also discards what tidymedia remembers about your
  FFmpeg build whenever a removal took, including a removal that cleared
  one of the two configuration files and then failed on the other; that
  case used to leave the remembered capabilities describing a binary the
  lookups had already stopped resolving to.

- **Four new Layer 1 builders.**
  [`ffm_fps()`](https://jmgirard.github.io/tidymedia/reference/ffm_fps.md)
  appends an `fps` filter, accepting either a number of frames per
  second or an FFmpeg framerate expression such as `"30000/1001"`.
  [`ffm_loudnorm()`](https://jmgirard.github.io/tidymedia/reference/ffm_loudnorm.md)
  appends FFmpeg’s EBU R128 `loudnorm` audio filter — the first builder
  to write the audio filter chain (`-af`).
  [`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md)
  stacks videos top to bottom, the vertical companion to
  [`ffm_hstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md),
  completing the blessed multi-input set alongside
  [`ffm_overlay()`](https://jmgirard.github.io/tidymedia/reference/ffm_overlay.md),
  which composites one video over another at an `x`/`y` position given
  as pixels or an FFmpeg expression and takes an optional `scale` to
  resize the overlay to a fraction of the main video’s width.

### Bug fixes

- **A wrong argument is now reported against the function you called,
  before any row runs.** Most of the package’s checks used to be reached
  only while a command was being built, which on a verb that processes
  many files at once meant the error arrived as
  `` Error in `purrr::pmap(jobs, .f, ...)` `` with an `In index: 1` line
  beneath it — a dependency’s name and an internal row number in place
  of the function you typed — or, under `parallel = TRUE`, against a
  `furrr` closure. Others named the Layer 1 builder the verb had called
  on its way down
  ([`ffm_crop()`](https://jmgirard.github.io/tidymedia/reference/ffm_crop.md),
  [`ffm_scale()`](https://jmgirard.github.io/tidymedia/reference/ffm_scale.md),
  [`ffm_fps()`](https://jmgirard.github.io/tidymedia/reference/ffm_fps.md),
  [`ffm_pixel_format()`](https://jmgirard.github.io/tidymedia/reference/ffm_pixel_format.md),
  [`ffm_drawbox()`](https://jmgirard.github.io/tidymedia/reference/ffm_drawbox.md),
  [`ffm_overlay()`](https://jmgirard.github.io/tidymedia/reference/ffm_overlay.md),
  [`ffm_loudnorm()`](https://jmgirard.github.io/tidymedia/reference/ffm_loudnorm.md),
  [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md))
  — public functions, but not the one you typed — or an internal
  variable the caller had never heard of. These now refuse at the verb’s
  own front door and name it:

  - an input file that does not exist, or exists and cannot be opened
    for reading, on every verb — including
    [`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md)
    and
    [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md),
    which had no check of their own at all;
  - a malformed `video_codec` or `audio_codec` token — a string carrying
    whitespace or shell characters, such as `"aac -evil"`;
  - a dimension, position or format value that is neither a positive
    number nor an FFmpeg expression — `width`, `height`, `x` and `y` on
    [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md),
    `width`, `height`, `fps` and `pixel_format` on
    [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
    and
    [`sample_frames_batch()`](https://jmgirard.github.io/tidymedia/reference/sample_frames_batch.md)’s
    per-row rate;
  - [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)’s
    per-region `x`, `y`, `width` and `height` values,
    [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)’s
    out-of-range `scale`, a negative `margin`, and
    [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)’s
    `target_loudness`, `true_peak` and `loudness_range`;
  - a `regions` table missing a required column or carrying one of the
    wrong type, a `direction` outside `"horizontal"` and `"vertical"`, a
    `position` outside the five inset positions, and an `audio_input`
    index past the number of inputs in that row;
  - a `hardware` backend whose encoder this FFmpeg build does not list,
    and a `video_codec` matching no codec family at all;
  - six ways for arguments to contradict each other: a video stream copy
    asked to encode on the GPU
    ([`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md));
    a `reencode = FALSE` cut naming a `video_codec` or `hardware`, or an
    `audio_codec` other than `"copy"`
    ([`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
    and its `_batch` sibling); an `audio_codec` with no audio carried
    into the output
    ([`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md),
    [`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md));
    and `resize = TRUE` across other than two inputs
    ([`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md));
  - an `outfiles` value
    [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
    cannot use — a number, `NA`, a list holding one, or a character
    vector with a missing value in it;
  - a `tidymedia.timeout` the underlying limit could not use — a
    fraction of a second, a negative number, `NA`, a string, more than
    one number — which used to be refused by whatever read the option
    first, so
    [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
    reported it as
    [`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
    [`extract_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/extract_audio_batch.md)
    as
    [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
    followed by the whole deparsed builder it had been handed, and
    [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
    as `purrr::map(infile, probe_one)`.

  Each `_batch` sibling refuses such a value whether it arrives as the
  verb’s own argument or in a `jobs` column of the same name, and
  refuses it before any row runs, so a large table fails immediately
  rather than after building the first row’s command. Under
  `two_pass = TRUE`, a bad loudness target is refused before the
  analysis pass measures the input, instead of after that measurement
  was already spent. Two shapes that used to slip through are now
  caught: a malformed value in the scalar argument was discarded in
  silence whenever the table carried a column of the same name, since
  the column wins; and a `direction` or `position` column had its type
  checked but never its values, so a misspelled cell reached the
  fan-out. Under `hardware = "nvenc"`,
  [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
  and
  [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
  used to accept a malformed `video_codec` outright, because the encoder
  name was rewritten to the nvenc equivalent before anything checked it
  and the rewritten name is well-formed.

  **Exactly the same calls are refused as before** — verified cell by
  cell across grids that vary each value in and out of range, as an
  argument, as a column, and as a column whose rows disagree — and no
  legal value compiles a different command. What moves is which function
  the error names, and, when a call is wrong in more than one way, which
  error you see. If you match on error text, this is the paragraph to
  read:

  - A path typed wrong is reported first, on the reasoning that it is
    the more likely mistake and the one you can act on without reading
    further. Malformed table shapes and wrong column types still report
    before it, since a column whose type has not been checked yet cannot
    usefully be swept for paths.
  - An argument contradiction is reported ahead of a per-row value
    error, and both are reported ahead of an unavailable hardware
    encoder. A contradiction and a bad value are the same mistake on
    every machine, so the diagnosis no longer depends on which FFmpeg
    build you happen to have. This reverses an order shipped earlier in
    this development cycle. On
    [`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md)
    and
    [`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md),
    where a call can be wrong in both at once: A value error and a
    contradiction resolve the same way whether the value arrived as an
    argument or in a `jobs` column; the contradiction reports first.
    Four checks moved to make that true — `direction`, `position`,
    `margin` and the `audio_input` index — so a call passing one of
    these as an **argument** alongside a contradiction is now told about
    the contradiction, where it used to be told about the value.
  - A value error also reports ahead of
    [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)’s
    own argument checks (`run`, `parallel`, `progress`, `manifest`,
    `checksums`, `verify`), though not ahead of the `jobs` table’s
    shape, which all four verbs check themselves before reaching
    [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md).
  - A limit and the hardware-encoder question are both asked after the
    verb’s own guards, so a bad `regions`, `pixel_format`, `audio_codec`
    or `audio_stream` reports as itself whether or not a limit is set
    and whether or not the build has the encoder. `fallback` is checked
    where the encoder question is asked and so moved down with it: a
    call wrong about both `fallback` and `pixel_format` now hears about
    the pixel format. Values validated inside the per-row fan-out — a
    `jobs` table’s `output` column, and
    [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md)’s
    `pixel_format` and `color` — still lose to both.
  - Where a verb’s checks on its own *arguments* fall relative to the
    missing-file sweep is not uniform and is not a promise:
    [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
    reports a bad `video_codec` before the sweep and a bad `width` after
    it. The refusal of duplicated inputs reports after it.
  - On
    [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md),
    the dimension wins twice over: a call passing both a bad
    `video_codec` and an invalid `width`/`height`/`fps` reports the
    dimension, and so does a call passing both `hardware = "nvenc"` and
    bad dimensions.
  - `picture_in_picture_batch(jobs, audio_input = NA, audio_codec = "aac")`
    reports the `audio_codec` contradiction rather than the
    `audio_input` value: `NA` (or `NaN`) asks to drop the audio, so it
    *creates* the “needs an audio stream to encode” contradiction rather
    than removing it. An index carries audio, so out of range it reports
    the value and in range the call compiles.
  - `compare_videos(files, out, direction = "sideways", audio_codec = "aac")`
    now reports the `audio_codec` contradiction too, because the
    single-call verbs check `direction` and `position` inside the
    pipeline they share with the batch verbs.

  Some wording changed with it, all on calls that aborted before and
  still abort. Where these verbs said
  `` `infile` does not exist: 'clip.mp4'. `` they now say
  `` `infile` can't be found or read: 'clip.mp4'. ``, and the many-path
  form reads `names 2 files that can't be found or read` — one
  readability test is now reached both from the verb you call and from
  the pipeline underneath it, so the two cannot disagree about which
  paths are acceptable.
  ([`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md)’s
  `file` and
  [`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md)’s
  `templatefile` keep the existence wording, not being pipeline inputs.)
  A malformed `pixel_format` used to be reported against `format`, an
  argument name these verbs do not have.
  [`compare_videos_batch()`](https://jmgirard.github.io/tidymedia/reference/compare_videos_batch.md)’s
  out-of-range audio message named an internal variable (`aud`) rather
  than the argument. A non-character codec column now says “must be
  character (`NA` to leave the codec unset)” instead of “must be
  character (no `NA`)”, and a bad codec value now says it “must be a
  single string or `NULL`”, `NULL` having become legal on those
  arguments — the `video_codec` of
  [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  and
  [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
  and the `audio_codec` of
  [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md),
  which has no `video_codec`. On
  [`standardize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/standardize_video_batch.md)
  and
  [`anonymize_video_batch()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video_batch.md),
  a jobs table invalid in both its `video_codec` and its `pixel_format`
  column now reports `pixel_format` first.

  One message is missing rather than moved: a refusal that now happens
  before any row is built no longer carries the `In index:` line, so a
  batch of many rows says which value is wrong but not which row named
  it — except where the next entry supplies it.

- **A `_batch` verb that refuses a bad value carried in a `jobs` column
  now says which row carries it.** The refusal message gains one final
  bullet — `First offending jobs row: 7.` — on the front-door value,
  vocabulary, codec-token and contradiction sweeps, so a bad cell in a
  50-row table no longer has to be found by hand. The rest of the
  message is unchanged byte-for-byte, and the same value passed as the
  verb’s own argument (which applies to every row) still refuses without
  naming one. On
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md),
  whose jobs table is reshaped internally, the row named is the row of
  *your* table, not the reshaped one.

- **A failed audio output no longer costs you the video in
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md).**
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
  The field is `NULL` when the video command succeeded. A wall-clock
  limit bounds each spawned program, so an audio half that reaches the
  limit still lets the video command run on a fresh limit of its own,
  and such a call can wait up to two limits rather than one.
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md)
  is unchanged: it already ran both rows.

- **The advice
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  gives when an audio output fails no longer arrives when you are
  already following it.** The multi-track report was attached to any
  audio command FFmpeg ended at a non-zero exit status on a multi-track
  input — including one whose output was already `.mka`, `.m4a`, `.mp4`,
  `.mov`, `.mkv`, `.webm`, `.ogg`, `.opus` or `.ts`, every one of which
  holds three audio tracks (`.webm`, `.ogg` and `.opus` under an encoder
  they accept, such as `audio_codec = "libopus"`; none has room for
  AAC). On those the container is not what FFmpeg objected to, so the
  report named a cause that was not the cause while telling you to do
  the thing you had already done. Writing to one of those nine, the
  error you get is now the one the run itself raised — same class, same
  exit status, same message, but for the line saying the video output
  was written. Those nine are the containers the package knows about,
  not every one FFmpeg can write several audio streams into (`.avi` and
  `.nut` take three too), so on an output outside the list the report
  still appears.
  [`separate_audio_video_batch()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video_batch.md)
  does the same: such a row is dropped from the post-fan-out warning
  rather than listed in it, the headline count follows the rows actually
  listed, and a batch whose failed audio rows all write to those
  containers warns not at all. The extension is read without regard to
  case, so `OUT.MKA` counts. What the report says when it does appear is
  unchanged, and it still tells you what the call did rather than why
  FFmpeg refused — a stream copy into a container that will not hold the
  source codec still looks the same from there.

- **A run that fails no longer leaves a broken output file behind.**
  FFmpeg creates its output before it knows the command will work, so a
  refused encode left a zero-byte file sitting where a result should be
  — and if you were writing over an existing file, FFmpeg had already
  truncated that to zero on its way to failing. Every verb, and every
  row of a `_batch` verb, now deletes what the failed run wrote, and the
  error says so and names it.

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
  earlier run’s frames alone. This does not reach
  [`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md),
  the raw escape hatch, which runs a command string it cannot parse for
  an output path.

- **Pipelines are executed as argument vectors (via
  [`system2()`](https://rdrr.io/r/base/system2.html)), never through a
  shell string**, so input and output paths containing spaces, quotes,
  `$` or backticks are handled correctly. This applies to
  [`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md),
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  and every task verb;
  [`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
  still returns the same reproducible command string. The Layer 0 escape
  hatches
  ([`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md),
  [`ffprobe()`](https://jmgirard.github.io/tidymedia/reference/ffprobe.md),
  [`mediainfo()`](https://jmgirard.github.io/tidymedia/reference/mediainfo.md))
  keep their raw-string interface.

- [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  and
  [`normalize_audio_batch()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio_batch.md)
  work when the output is a FLAC (`.flac`) or Ogg Vorbis (`.oga`) file.
  On FFmpeg 9 these failed with “Could not open encoder before EOF” and
  left a zero-byte file: the loudness filter hands its output on in very
  long frames, which most encoders are re-framed out of but FLAC and
  Vorbis are not, and the frame was longer than FLAC will encode.
  Loudness normalization now re-chunks its output, so every audio
  container works. Commands built with
  [`ffm_loudnorm()`](https://jmgirard.github.io/tidymedia/reference/ffm_loudnorm.md)
  carry the extra `asetnsamples` filter, which is visible in the
  compiled command string.

- **Metadata values containing a newline no longer corrupt the probe
  output.**
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
  report the duplication.

- [`picture_in_picture_batch()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture_batch.md)
  names only the column that actually holds a bad path. A row whose
  `main` is fine and whose `overlay` is missing used to read
  `` `jobs$main` and `jobs$overlay` name 1 file that can't be found or read. ``,
  sending you to a column with nothing wrong in it. It now reads
  `` `jobs$overlay` names 1 file that can't be found or read. ``, and
  still names both when both are bad.

- **A missing value where a number belongs is refused instead of
  reaching FFmpeg.** `crop_video(f, o, width = NA_real_, height = 100)`
  used to fail with R’s own `missing value where TRUE/FALSE needed`,
  which names neither the argument nor the function you called;
  `width = NA_character_` was worse, because it was accepted and
  compiled `crop=w=NA:h=100` into the command, so a `run = FALSE` call
  returned a command string FFmpeg would have rejected later. Both now
  abort with `` `width` must be a single FFmpeg expression or number. ``
  against the verb you called. The same refusal covers the size and
  position arguments of
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

  In the same vein, `normalize_audio_batch(audio_codec = NA)` aborts
  instead of quietly compiling the default command. A scalar `NA` was
  resolved the same way as an `NA` cell in a jobs-table column — where
  it legitimately means “leave this row’s codec unset” — so an
  accidental `NA` argument produced a command with no `-codec:a` and no
  indication that anything had been ignored.

- `ffmpeg_codecs(sort_by_type = )` refuses a value that is not `TRUE` or
  `FALSE`, without running FFmpeg first. What it did before depended on
  the value. A string, `NA`, or more than one value ran the binary,
  parsed the whole codec list, and only then failed on the internal `if`
  that does the sorting — so a call it could have refused outright cost
  a process, and the failure named no argument. A number, though, never
  failed at all: `if (123)` is `TRUE` in R, so
  `ffmpeg_codecs(sort_by_type = 1)` returned the sorted table. **That
  call is now an error**, matching what
  [`ffmpeg_encoders()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)
  has always done with it.

- **A configuration file that holds nothing, holds more than one line,
  or holds one empty line no longer stops the call, or answers about the
  wrong thing.** The first two made
  [`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_program.md)
  – and every call above it – fail with an R error naming neither the
  program nor the file; the third warned instead that the binary had
  gone missing, which was not what was wrong with it. All three now warn
  with a condition you can catch by class –
  `tidymedia_location_unreadable`, carrying the program and the file –
  and return `NULL`. A line holding only spaces is not one of these: it
  is still read as a location, and still warns that the binary is
  missing.
  [`unset_program()`](https://jmgirard.github.io/tidymedia/reference/unset_program.md)
  clears the file;
  [`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  replaces it.

- **The warnings about a program tidymedia cannot use now name the
  recovery your machine actually has, and can be caught.** The warning
  for a remembered location whose binary has gone offers
  [`unset_program()`](https://jmgirard.github.io/tidymedia/reference/unset_program.md)
  beside
  [`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md),
  since forgetting the location is the other repair, and on Windows it
  offers
  [`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
  on the same terms the not-found warning does; it carries the class
  `tidymedia_location_gone`, with the program and the location. The
  warning
  [`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_program.md)
  and its siblings give when they cannot find a program at all advised
  `set_<program>()` and nothing else; on Windows it now also offers
  [`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md),
  for the three programs that installer registers. It is not offered for
  `mediainfo`, or off Windows, where that call would refuse you. And the
  warning raised when a version probe runs out of time names each
  program the way
  [`program_status()`](https://jmgirard.github.io/tidymedia/reference/program_status.md)’s
  `program` column does – `ffmpeg` rather than `FFmpeg` – and no longer
  says the `NA` lands in a manifest, since the same warning is raised
  from
  [`program_status()`](https://jmgirard.github.io/tidymedia/reference/program_status.md),
  whose `NA` lands in a returned table.

- [`program_status()`](https://jmgirard.github.io/tidymedia/reference/program_status.md)
  no longer swallows those two warnings. A program that was never
  configured and is not installed still gets `NA` in both columns and
  says nothing about it – there, `NA` is the whole answer. A remembered
  location that cannot be used is not: `NA` would read exactly like a
  program you never had, while the real state is a file on your machine
  you can clear in one call, so the warning naming it comes through.

- **[`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
  refuses on a platform it cannot install for**, before it downloads,
  writes, or asks anything. It always installed a Windows build and only
  ever looked for `.exe` files in it; called on macOS or Linux it used
  to ask for consent and then download and unpack that build anyway. It
  now aborts with `tidymedia_wrong_platform`, naming the platform it
  found and where FFmpeg comes from there – `brew install ffmpeg` on
  macOS, `sudo apt-get install ffmpeg` on Linux – and
  [`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  on any platform, for a build that is already installed.

- **[`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
  registers every program the archive produced, or none of them.** It
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

  It also decides what an archive produced by looking at the install
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

- **A refused
  [`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
  leaves the install directory as it found it.** Files a failed
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

- **Remembered locations and installed builds now live where CRAN policy
  says they should.** A binary location remembered with
  [`set_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/set_program.md),
  [`set_ffprobe()`](https://jmgirard.github.io/tidymedia/reference/set_program.md),
  [`set_ffplay()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  or
  [`set_mediainfo()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  lives under `tools::R_user_dir("tidymedia", "config")`, in a file
  named `<program>_location.txt`. A location set before this change is
  still found: the lookups read the new directory first and, only when
  no file exists there, the old one; nothing is moved or copied. Calling
  [`set_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  again writes the new file, after which the old one is no longer read,
  even if the new file names a binary that has since gone.
  [`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
  likewise installs FFmpeg under
  `tools::R_user_dir("tidymedia", "data")` in an `ffmpeg` subdirectory,
  replacing the old `rappdirs` location. That is the default only: an
  `install_dir` you pass yourself is used as before. An FFmpeg installed
  by an earlier version keeps working and is not moved, its location
  having been recorded when it was installed; running
  [`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
  again does install a second copy in the new location and leaves the
  old one on disk, which is yours to delete once nothing points at it.

- **Errors keep naming the function you called, and one of them names it
  for the first time.** `set_ffmpeg("nope")` says
  [`set_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/set_program.md),
  whether you call it at the console or from inside your own function,
  and the same holds for
  [`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md),
  the other three `set_*()` functions and
  [`hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
  — for a wrong argument and for a location or codec the package cannot
  use. A wrong `codec` or `hardware` passed to
  [`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md)
  used to be reported as coming from
  [`hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md),
  which you never called; it now names
  [`has_hardware_encoder()`](https://jmgirard.github.io/tidymedia/reference/hardware_encoder.md).

- [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  — and the `parallel = TRUE` path of
  [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  and
  [`segment_video_batch()`](https://jmgirard.github.io/tidymedia/reference/segment_video_batch.md)
  — warns when parallel processing is requested but no parallel
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  is active. Previously such calls ran one job at a time with no speedup
  and no indication; the warning points to
  `future::plan(future::multisession)`.

- An explicit
  [`ffm_map()`](https://jmgirard.github.io/tidymedia/reference/ffm_map.md)
  on a multi-input pipeline
  (e.g. [`ffm_hstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md))
  is now emitted alongside the automatic `-map "[vout]"` instead of
  being silently ignored, so `ffm_map(p, "0:a")` keeps the first input’s
  audio next to the stacked video.

- Test coverage is measured again: an empty `R/zzz.R` triggered a `covr`
  bug that silently reported 0% package coverage.

### Performance

- Asking for hardware encoding queries FFmpeg for its encoder list once
  per R session instead of once per call. Every such call previously
  started a separate FFmpeg process to re-read the same list, so a
  500-row GPU batch paid 500 of them before encoding anything; now it
  pays one. The compiled commands are unchanged.

  The answer is remembered for the rest of the session, which matters if
  the build changes underneath you — a fresh FFmpeg install, a new GPU
  driver, a different binary. Two calls discard it: the new
  [`refresh_ffmpeg_capabilities()`](https://jmgirard.github.io/tidymedia/reference/refresh_ffmpeg_capabilities.md),
  and
  [`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  (or
  [`set_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)),
  which discards it for you since it points tidymedia at a different
  binary. Setting `options(tidymedia.hardware_encoders = )` still
  overrides the answer outright and is read before anything remembered,
  so it takes effect at once.
  [`ffmpeg_encoders()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_encoders.md)
  and
  [`ffmpeg_codecs()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg_codecs.md)
  are never remembered: they query FFmpeg on every call and always
  report the build as it is now. What is remembered is per R process, so
  under `parallel = TRUE` each worker asks once rather than sharing the
  parent’s answer — bounded by the worker count, not the row count.

- [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
  and the `probe_*()` shortcuts read each file with a **single** FFprobe
  process instead of one per stream plus one more for the container. A
  five-stream file needed six processes and needs one. The saving grows
  with stream count and with the number of files, so it is largest on
  exactly the batch work these functions exist for — locally, probing
  ten copies of a four-stream file went from 1.7 seconds to 0.46. The
  returned tibbles keep the same columns, in the same order, with the
  same values and types — except for the invented columns described
  under *Bug fixes*, which were never data in the first place.

- [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
  and the `probe_*()` shortcuts take a `parallel` argument (default
  `FALSE`). With `parallel = TRUE` the per-file probes are spread across
  workers with the optional **furrr** package, following whatever
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  is active — the same mechanism
  [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  already uses, so one plan configures both. The output is unchanged
  either way: the same tibbles, the same types, and rows in the order
  the input vector gave them. Files that cannot be probed still produce
  one warning at the end of the call naming all of them, not one per
  worker. Two things to know: `furrr` is looked for only when
  `parallel = TRUE`, so it stays an optional dependency for everyone
  else; and because the default `future` plan is sequential,
  `parallel = TRUE` on its own gives no speedup — it now says so with a
  warning rather than quietly doing nothing. Set a plan first, e.g.
  `future::plan(future::multisession)`.

### Documentation

- The package has a landing help topic:
  [`?tidymedia`](https://jmgirard.github.io/tidymedia/reference/tidymedia-package.md)
  resolves to an overview of the three layers and the vignettes, and the
  topic is listed by
  [`help(package = "tidymedia")`](https://jmgirard.github.io/tidymedia/reference)
  and on the reference index. Previously neither reached anything. It
  gained a *Session options* section covering all three session options
  in one place, and it is where the wall-clock limit’s escalation lag is
  described.

- A new
  [`?audio_stream`](https://jmgirard.github.io/tidymedia/reference/audio_stream.md)
  help page explains the two 0-based audio arguments the package exposes
  and how they differ: `audio_stream` counts one input’s audio tracks,
  while `audio_input` on
  [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  and
  [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
  counts the verb’s inputs, so neither index can be read off the other.
  It also covers what leaving each unset means (the extraction verbs
  take the first track, the pass-through verbs keep every track, and an
  unset `audio_input` drops audio altogether), what an `NA` cell means
  in a `_batch` jobs column, and the two unrelated things `audio` names
  on
  [`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md)
  and
  [`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md).
  Every verb taking either argument links to it. The verb lists inside
  those descriptions are generated from a single source, so they cannot
  fall behind the code the way they had — several pages still listed
  only some of the verbs that keep every audio track, omitting ones
  added later.

- Two new vignettes and a reordered third.
  [`vignette("verification")`](https://jmgirard.github.io/tidymedia/articles/verification.md),
  “Checking results and bounding runs”, covers three things that had
  reference pages but no narrative: checking a processed file against
  what you asked for with
  [`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md)
  and the batch runner’s `verify =` argument; recording how a run was
  actually made with `ffm_batch(manifest = TRUE)`, `checksums = TRUE`
  and
  [`ffm_manifest()`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md);
  and bounding a run that hangs with `options(tidymedia.timeout = )`,
  [`with_timeout()`](https://jmgirard.github.io/tidymedia/reference/with_timeout.md)
  and
  [`local_timeout()`](https://jmgirard.github.io/tidymedia/reference/local_timeout.md).
  It states what the wall-clock limit really bounds — how long R waits,
  which runs up to 40 seconds past the limit you set when a program
  ignores the first two signals — rather than promising the limit. “A
  research preprocessing workflow” walks an end-to-end pipeline —
  standardizing recordings, normalizing and extracting audio, sampling
  frames, de-identifying, and packaging for sharing — on a realistic
  dyadic-interaction study, and points at the verification vignette from
  its reproducibility section. The “Get started” vignette now leads with
  the task verbs, the front door most users need, before descending to
  the builder, and gains a section on choosing an audio track; every
  vignette cross-links to the others.

- The batch vignette’s account of `parallel = TRUE` names the functions
  that actually take it —
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

- Help pages cross-reference each other: every task verb links to the
  `ffm_*` pipeline builders it is built on (and each builder back to the
  verbs that use it), and the three metadata reader families
  (`probe_*()`, `mediainfo_*()`, `get_*()`) link to one another so you
  can find the alternative backend. Each metadata help page states its
  backend (FFprobe or MediaInfo) and what it returns — a tibble, a
  value, or a single scalar per file — and the “Media metadata as
  tibbles” vignette gains a table comparing the reader families at a
  glance.

- Every verb taking `hardware` says that asking for a backend queries
  your FFmpeg build for the encoder while the command is being
  assembled, so a call that re-encodes the video runs the binary even
  with `run = FALSE`. Asking for a backend alongside a stream copy is an
  error those pages already describe —
  [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  at its default `video_codec = "copy"`,
  `segment_video(reencode = FALSE)`, and both `_batch` siblings — and it
  is caught first, so such a call aborts without probing. This was
  always true; only the documentation is new. `run = FALSE` promises you
  the command that would run, not a call that touches nothing.

- The install instructions in the README end at a check. Each platform’s
  route, under MediaInfo and under FFmpeg, finishes with a
  [`program_status()`](https://jmgirard.github.io/tidymedia/reference/program_status.md)
  call, what a found and a not-found answer look like, and the call to
  make when the program was not found. The macOS manual FFmpeg route
  used to stop after dragging the program into the Applications folder,
  which is not on the `PATH`, so nothing found it there; it now names
  the separate `ffprobe` download and the
  [`set_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  /
  [`set_ffprobe()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  step that makes both usable.

- The examples no longer run a program that is not there. Each README
  chunk that starts FFmpeg, FFprobe or MediaInfo is evaluated only when
  that program is installed, and the help-page example for finding a
  program no longer warns on a machine with no binaries.

- The reference page for finding a program documents only the four
  functions you can call —
  [`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_program.md),
  [`find_ffprobe()`](https://jmgirard.github.io/tidymedia/reference/find_program.md),
  [`find_ffplay()`](https://jmgirard.github.io/tidymedia/reference/find_program.md)
  and
  [`find_mediainfo()`](https://jmgirard.github.io/tidymedia/reference/find_program.md).
  The internal
  [`find_program()`](https://jmgirard.github.io/tidymedia/reference/find_program.md)
  behind them is no longer shown as though it were part of the
  interface.

- `citation("tidymedia")` returns a citation written for the package,
  carrying the package website, instead of the entry R generates
  automatically from `DESCRIPTION`.

- The get-started vignette said a task verb returns the path it wrote.
  It returns the compiled FFmpeg command, invisibly when it runs the
  command and visibly under `run = FALSE`; the vignette now says so.

### Requirements

- tidymedia now states the R version it needs: `R (>= 4.1.0)`. An
  installer on an older R refuses the package and says why, instead of
  installing something whose help-page examples will not run. That
  version is what those examples require — they use the native pipe
  `|>`, a form R gained in 4.1.0 — and it sits above the highest R
  version any declared dependency floor asks for.

- The dependency versions tidymedia declares are now measured rather
  than assumed: the package’s test suite has been run against the exact
  version of each package `Imports` names. One of them was wrong.
  `rlang` is now `(>= 1.2.0)`, up from `1.1.0`: tidymedia checks its
  arguments with
  [`rlang::check_string()`](https://rlang.r-lib.org/reference/check_type_scalar.html),
  `check_bool()` and their siblings in well over a hundred places, and
  rlang first exports those functions in 1.2.0 — so on an earlier rlang
  the package’s verbs failed at their own front doors. The other nine
  declared floors were exercised at the version they name and stand
  unchanged.

- tidymedia now imports **digest**, which is what computes the SHA-256
  of a downloaded FFmpeg archive. Base R gained
  [`tools::sha256sum()`](https://rdrr.io/r/tools/sha256sum.html) in
  4.5.0, four releases above the `R (>= 4.1.0)` this package declares;
  taking a small, pure-C dependency rather than asking everyone below
  4.5.0 to give up the package was the trade made.

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

- tidymedia now declares the external tools it interfaces. `DESCRIPTION`
  names FFmpeg and MediaInfo in `SystemRequirements`, each with its
  project URL, so the tools the package shells out to are visible to
  anyone reading the package’s metadata rather than only to someone who
  runs it and gets an error. Nothing about how the package finds those
  tools has changed.

- The package’s `Title` and `Description` now say what tidymedia does
  rather than restating its own name. The title reads “Media File
  Preprocessing and Metadata for the ‘tidyverse’”, and the description
  names the two batch jobs the package is built around — transforming
  files and reading their metadata back as tibbles — along with the two
  programs it drives. This is what an installer and a package index show
  before anyone opens the help pages.

- The built package no longer carries three paths it has no use for: two
  `inst/extdata/*_location.rds` files, which held a remembered binary
  location until that moved to the user’s own configuration directory
  and which nothing has read since, and a `tests/testthat/_problems/`
  scratch directory left over from a local test run. Both kinds of
  leftover a failing test run can produce — that directory and
  testthat’s own `testthat-problems.rds` — are now ignored at build
  time, so a later failing run cannot put either back.

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
