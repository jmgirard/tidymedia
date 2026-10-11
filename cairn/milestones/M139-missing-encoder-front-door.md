# M139: A failed FFmpeg run names the encoder this build lacks

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M138
- **Driving RR:** —
- **Principles touched:** GP1
- **Resolves:** —
- **Surface tier:** user-facing. It adds a condition class, a batch warning and help text a caller sees.
- **Branch/PR:** m139-missing-encoder-diagnosis

## Goal

FFmpeg can fail on a run whose codec this FFmpeg build cannot encode. Then the
error the caller gets names the missing encoder and the function the caller
typed, and it carries a class a caller can catch. Today only FFmpeg's own
console message names the encoder.

## Scope

**In:** a diagnosis in `ffm_run()` that runs only after FFmpeg exits with a
non-zero status, under D024's diagnostic licence, in the shape
`run_separation_audio()` already uses. One condition class covers the event,
and `check_hardware_available()`'s availability abort takes it too. A
single-file call names the function the caller typed. A batch warns once with
the rows. The help text and Known issues that the diagnosis needs.

**Out:** nothing checks the encoder before FFmpeg starts, so a `run = FALSE`
call still previews a command this build cannot run. A check before the start
refuses calls that run today (plan gate, 2026-10-10, second round). Names the
hardware backend table holds keep FFmpeg's error on two routes: `ffm_codec()`
in Layer 1, and a Layer 2 `video_codec` with `hardware = "none"`. Encoders
passed through `ffm_output_options()`, and the encoder FFmpeg picks for a
container that gets no codec. `verify_media()`'s codec arguments, which are
expectations about a file. Muxers, filters and decoders. Layer 0 `ffmpeg()`
(D002).

## Acceptance criteria

- [x] AC1: FFmpeg can exit with a non-zero status from `ffm_run()` while the pipeline's
      video or audio codec names something the session's remembered listing (D044) does
      not hold as encodable. Then the condition `ffm_run()` raises carries the class
      `tidymedia_encoder_unavailable` beside `tidymedia_ffmpeg_exit` and its `tm_status`.
      It carries the fields `tm_encoder` and `tm_stream` (`"video"` or `"audio"`), one
      element per such codec. Its message states that the build lacks each encoder,
      beside the bullets the merge base prints. It does not state that the missing
      encoder caused the exit. If `ffmpeg_encoders()` lists a name, or `ffmpeg_codecs()`
      lists it with `encoding = TRUE`, the name is encodable. So `"h264"` and `"mp3"`
      never trigger the diagnosis on a build that encodes those codecs. The diagnosis
      reads `codec_video` and `codec_audio` off the pipeline `ffm_run()` receives. It
      excludes `"copy"` and a codec the pipeline leaves unset (`NULL`, or a column `NA`,
      D022). It also excludes any name `tm_hardware_encoder()` can return, and anything
      passed through `ffm_output_options()`. `check_hardware_available()`'s
      availability abort carries the same class, with `tm_encoder` and
      `tm_stream = "video"` (D062, D063).
- [ ] AC2: A run that FFmpeg completes, and every `run = FALSE` call, compiles the same
      command as at the merge base and, from the same remembered encoder listing (D044),
      starts the same programs as many times, because the diagnosis is reached only after
      a non-zero exit inside `ffm_run()`. The remembered codec reader has one caller in `tm_call_graph()`, the
      diagnosis, and `ffmpeg_codecs()` stays uncached (D044). The codec listing is
      remembered beside the encoder listing once it is read. A failed read is not
      remembered, and each of D089's four routes discards both listings. If the listing
      cannot be read, the condition is the merge base's condition, unchanged (D024's
      fail-open consequence). If the read reaches the limit, one
      `tidymedia_probe_timeout` warning also reports it, in the single-file and the batch
      forms (D049).
- [x] AC3: A single-file call names the function the caller typed. The domain is the
      exports that reach `ffm_run()` and not `ffm_batch()` in the namespace call graph
      (`tm_reaches()` in `tests/testthat/helper-input-paths.R`), less `ffm_batch()`, plus
      `ffm_run()`. Measured 2026-10-10, they are `anonymize_video`, `compare_videos`,
      `concatenate_videos`, `convert_audio`, `crop_video`, `extract_audio`,
      `extract_frame`, `format_for_web`, `normalize_audio`, `picture_in_picture`,
      `sample_frames`, `separate_audio_video`, `standardize_video`, `strip_metadata` and
      `ffm_run`. Take each one whose pipeline can carry a codec other than `"copy"`. A
      `run = TRUE` call whose run fails with an encoder the build lacks raises AC1's
      condition with that export as its `conditionCall()`. For `separate_audio_video()`,
      the promise covers a single-track input whose other half succeeds. Its track-count
      diagnostic and D068's held video error keep their merge-base behavior. Where AC1's
      diagnosis does not fire, `ffm_run()`'s exit, timeout and verify-failure conditions
      keep their class, fields and message. Their call still names `ffm_run`.
- [x] AC4: A batch run starts and records every row as it does at the merge base. Some
      rows can raise AC1's condition. If so, after the rows run, `ffm_batch()` raises one
      warning of class `tidymedia_encoder_unavailable` with the fields `tm_encoder`,
      `tm_stream` and `tm_row`. It names each such encoder and the numbers of those rows
      in the jobs table `ffm_batch()` received. This holds under `parallel = FALSE`,
      `parallel = TRUE` and `progress = TRUE`. `segment_video()` and the `_batch` exports
      run their codec-carrying rows through `ffm_batch()`. A `run = FALSE` batch is
      unchanged.
- [ ] AC5: The profile's `verify` and consistency-gate slots in `cairn/PROFILE.md` are
      clean, the universal cairn-file checks included. `NEWS.md` carries an entry for the
      class and the batch warning. `?ffm_run` states that the diagnosis can silently not
      run (D024). `?refresh_ffmpeg_capabilities` states that a failed run can fill the
      remembered listings. The class list in `?tidymedia` gains the class.
      `cairn/DESIGN.md`'s Known issues states five things. A dry run can preview a
      command this build cannot run. The two hardware-name routes keep FFmpeg's error. A
      stale remembered listing, or a parallel worker's own listing, can misreport an
      encoder. `separate_audio_video_batch()` numbers its rows in a table with two rows
      per caller row. One of its rows can draw both its own separation warning and the
      new one. `normalize_audio_batch(two_pass = TRUE)` numbers its rows without its
      silent rows.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T5, T7
- AC3 → T3
- AC4 → T4
- AC5 → T6

## Tasks

- [x] T1: Add the remembered codec listing beside the encoder listing, the encodable
      test, and the diagnosis after the exit in `ffm_run()`. Test with a mocked exit,
      `cached_encoder_names()` and codec listing, which are the CI-portable cells. Cover
      `"h264"`, `"mp3"`, `"copy"`, unset and backend-table names. Also cover a listing
      error, a reached limit during the listing, and both codecs missing at once. Add
      cells with M138's `TIDYMEDIA_HIDE_ENCODERS` wrapper, which run only where a real
      FFmpeg is present.
- [x] T2: Give `check_hardware_available()`'s availability abort the class and fields,
      tested with the `tidymedia.hardware_encoders` option seam.
- [x] T3: Carry the caller's frame from `ffm_finish()` and the four direct `ffm_run()`
      sites to the diagnosis, through an internal implementation, because
      `test-exported-call-formal.R` refuses a `call` formal on an export (D087). Write
      the call-graph sweep. It lists each export whose pipeline carries no codec but
      `"copy"`, with its reason. If the tested and listed exports differ from the
      computed domain, it fails.
- [x] T4: Carry the diagnosed encoder back from each row in `ffm_batch()`, workers
      included, and raise the one warning. The parallel cells are wrapper cells under a
      multisession plan, because mocks do not reach workers.
- [x] T5: Run the purity tests, the timeout sweeps and the recorded spawn baselines
      unchanged, with no re-recording. Plant a listing read on the success path and see
      a spawn baseline redden.
- [x] T6: Write the records: `NEWS.md`, the three help pages, the `cairn/DESIGN.md`
      Known issues text, and the trim of the ROADMAP hardware-surface row's sentence
      naming this milestone.
- [x] T7: Review return 1. Give one `tidymedia_probe_timeout` warning per call in
      `ffm_batch()` and `separate_audio_video()`, and skip the listing read for the
      call's later runs after one reaches the limit. Add a batch reached-limit cell.
      Take the Review section's fix-now findings. Add listing-read counts for a
      successful batch, a successful separation and a `run = FALSE` verb.

## Work log

- 2026-10-10: created by /milestone-plan, from the user's answer at M138's question set asking for the refusal everywhere a caller names an encoder. Split out of M138 so the CRAN deadline of 2026-10-31 is not held by it.
- 2026-10-10: plan gate chose the spawn gate over the verb's front door as the refusal's site. A front-door probe spawns `ffmpeg -encoders` on a `run = FALSE` call. That runs against D024's pure-compilation surface, the `cairn/DESIGN.md` Conventions bullet, D034's two-member census, and five committed purity tests asserting zero spawns. The cost is that a dry run still previews a command this build cannot run. AC1 pins the purity, and Scope Out discloses the cost. Falsified by a caller harmed by previewing a command that cannot run.
- 2026-10-10: plan gate chose to ask about `intended_encoder(video_codec, hardware)` rather than the token the caller typed. Asking about the token refuses `video_codec = "libx264", hardware = "videotoolbox"`, which works today, against D035's no-new-refusal condition. Falsified by a caller who needs the refusal to name what they typed.
- 2026-10-10: criteria audit ran in full mode, in a fresh Opus reader, over a first draft that sited the refusal at the verb's front door. It found no criterion passing all six questions, and two of them mutually unsatisfiable: the front-door siting and the promise that no `run = FALSE` call spawns anything new. All findings were taken, and the siting choice above is what resolves the pair. Also taken: `verify_media()` excluded from the sweep by name and reason, the `"copy"`, `NULL` and `NA` cells carved out, the hardware axis routed through `intended_encoder()`, the class named and the hardware sibling's unclassed abort brought with it, the D075 wording restored to its own rule, the timeout displacement at the four D074 verbs closed, the crossings generated rather than listed per D039's precedent, the batch-column disposition given its superseding entry, the no-location silence conditioned on location resolution rather than on FALSE, and the per-cell instruments named because this build's missing encoder is not observable on this machine.
- 2026-10-10: THE CRITERIA BELOW NEED A RE-CUT THROUGH /milestone-plan BEFORE THIS MILESTONE IS IMPLEMENTED. A second audit round ran in a fresh Opus reader over the redrafted criteria. It found no criterion passing all six questions, and a new unsatisfiable pair. The findings are recorded in the lines below, so the re-cut starts from them rather than from a third blank draft. Nothing here is dropped.
- 2026-10-10: re-audit: the pair. AC2 and AC3 promise that the refusal names the verb the caller typed and moves out of the per-row fan-out. AC4 forces it below the builder's checks at the four verbs that keep no front-door guard for `video_codec`, `pixel_format` or `regions` (D074 property 1). The only window left is inside `ffm_finish()` or `ffm_run()`, where neither the verb's frame nor the argument's name is in scope and the fan-out frame is `purrr::pmap()`. AC4 is the side that stands, because D075 is explicit and names this exact cost. So AC2's blame clause narrows to what that window can carry, and AC3's superseding-entry and Known-issues trim clauses are deleted, because nothing moves out of the fan-out and there is no disposition to supersede.
- 2026-10-10: re-audit: restrict the refusal to `hardware = "none"` and leave every backend encoder to `check_hardware_available()`, which already answers it on both `fallback` arms (D085). `intended_encoder()` leaves the machine out of its answer, so asking about it refuses a `fallback = TRUE` call that works today, against D035's no-new-refusal condition. It also splits one availability question across two seams, because the hardware path reads the `tidymedia.hardware_encoders` option and `has_encoder()` deliberately does not. With the restriction, AC2's worked example holds for free and `intended_encoder()` drops out of the criterion.
- 2026-10-10: re-audit: a codec name is not an encoder name. `-c:v h264` and `-c:a mp3` run on a full build, while `ffmpeg -encoders` lists neither name, so `has_encoder()` answers FALSE for both and the refusal as drafted would refuse working calls. The package's own help uses `video_codec = "h264"`. The repair consults `ffmpeg_codecs()` as well, with planted cases for `"h264"` and `"mp3"`. And `audio_codec` is asked about as itself, with no hardware mapping, which no criterion said.
- 2026-10-10: re-audit: the formals sweep is a proxy for "reaches the codec emitters" and misses the Layer-1 route, `ffm_codec(video = "libx264")` into `ffm_run()` or `ffm_batch()`, which reaches the same spawn gate. Layer 0 `ffmpeg()` stays out by D002. Name the reachability reading rather than resting on formals.
- 2026-10-10: re-audit: AC4's clause that the probe "reads the session memo without re-reading `tidymedia.timeout`" is false against M138's deliverable, whose `has_encoder()` reads the limit first. What delivers the promise is ordering: the verb's own re-call is machine-independent and sits above the probe. The grid's axes also miss the limit error the criterion is about, the no-location state, and the carve-out cells, and D039's record asks for a completeness reader plus controls compared at crossing grain.
- 2026-10-10: re-audit: three smaller repairs. AC1's planted refusal must run with the memo discarded, or assert the refusal rather than the spawn count, because a plant reading a warm memo spawns nothing and the control stays green. AC5 adds that the remembered-location warning reaches the caller exactly once. AC6 must say which cells are wrapper cells, which run only where a real FFmpeg is on `PATH`, and which are mock cells, which are the CI-portable ones. The `cairn/DESIGN.md` Known issues entry gains a member rather than losing one.
- 2026-10-10: re-cut by /milestone-plan. The plan-owned sections above are replaced, and the title in ROADMAP is changed to match. The criteria audit ran after the first question round instead of before it, so a second round asked the one question its finding raised.
- 2026-10-10: question set: in a batch, what happens to a row whose encoder this build lacks. Answer: skip the row, run the rest, and warn once with the encoder and the rows. The second round below removed the skip, because rows now start as at the merge base. The one warning stays.
- 2026-10-10: question set: which function a single-file error names. Answer: the function the caller typed, through an internal implementation (D087).
- 2026-10-10: question set, second round: refuse before FFmpeg starts, or explain after FFmpeg fails. Answer: explain after FFmpeg fails. This supersedes the first plan-gate line's spawn-gate siting.
- 2026-10-10: approach: rejected a check before FFmpeg starts. FFmpeg does not look up an encoder for a stream kind the output lacks. The first audit reader measured `-map 0:v?` on an audio-only input and `-an`, and both exit 0 with an unknown encoder. So such a check refuses calls that run today. An example is `standardize_video()` on an audio-only file on a build without libx264, because its default `video_codec` is `"libx264"`. Falsified by a caller harmed because FFmpeg starts before the error, for example a failed run that changes a file before the encoder lookup.
- 2026-10-10: approach: rejected naming `ffm_run()` in the single-file condition, as FFmpeg's exit error does today. It needs no threading, but it names a function that the caller did not always type. Falsified by a caller who catches the condition by its call and expects `ffm_run`.
- 2026-10-10: criteria audit (full mode), round 1, fresh Opus reader, over a draft that refused before the spawn. Findings taken, nine. The false claim that the gate refuses only runs FFmpeg refuses, which raised the second question. `ffm_batch` inside and `ffm_run` outside the blame domain, and `segment_video` in the batch group. The once-per-session count, which a failed listing breaks. Nonexistent `pixel_format` and `regions` pairs at `crop_video()` and `format_for_web()`. A dry-run claim wider than its tests. Row numbers with no referent. Parallel cells that mocks cannot reach. A second hardware-name route. A review act written as a criterion.
- 2026-10-10: criteria audit (full mode), round 2, a second fresh Opus reader, over the diagnosis draft. Ten findings, all taken by narrowing. A limit reached during the listing now also warns (D049). The conditions that keep their call are named. The blame domain is written out, and `separate_audio_video()` is limited to a single-track input. Batch row numbers are those of `ffm_batch()`'s jobs table, with the two-pass silent-row offset disclosed. "This build" reads the remembered listing (D044), with staleness disclosed. `tm_encoder` and `tm_stream` are at every site of the class. The grep is replaced by one caller in the call graph. Three help pages join the records. Reading codecs off the pipeline replaces the column-override exemplar. The message does not claim the encoder caused the exit.
- 2026-10-10: remainder ledger: the refusal everywhere a caller names an encoder (M138's question set) ships as a classed diagnosis after FFmpeg fails, at the user's second-round answer. A check before the start is not planned and not a candidate, because it refuses calls that run today. Hardware-table names and `ffm_output_options()` encoders keep FFmpeg's error, disclosed in Known issues by AC5.
- 2026-10-10: implement started on branch `m139-missing-encoder-diagnosis`, cut from master at `db99245a`. The uncommitted `CRAN-SUBMISSION` change is unrelated and stays unstaged.
- 2026-10-10: T1 done. `cached_encodable_codecs()` in `R/cache.R`, `hardware_encoder_names()` and `diagnose_missing_encoders()` in `R/ffmpeg.R`, and the diagnosed abort in `ffm_run()`'s exit branch. The encoder list is read first, and the codec list only for a name the encoder list lacks. Warnings from the listing's own program lookup are muffled, because the failed run already resolved the same program. `tests/testthat/test-encoder-diagnosis.R` has 11 tests, the 2 wrapper cells included. The full suite ran 1802 tests with 1 failure: `test-timeout-silence.R`'s absorber list, which `diagnose_missing_encoders()` joins by design. That list now names it, with its guard.
- 2026-10-10: implementation choice: the frame a diagnosis names rides on the pipeline as attribute `tm_call`, set by the site that calls `ffm_run()`. This keeps every existing `ffm_run()` condition's frame as it is at the merge base, which an internal implementation function would change. No export gains a `call` formal (D087). T3's wording "through an internal implementation" is refined to this mechanism.
- 2026-10-10: T2 done. `check_hardware_available()`'s availability abort carries class `tidymedia_encoder_unavailable`, `tm_encoder` and `tm_stream = "video"`, tested through the option seam at `standardize_video()` and `standardize_video_batch()`, with a control where the machine has the encoder. Full suite: 1803 tests, 0 failed.
- 2026-10-10: T3 done. `ffm_finish()` takes `call` with no default and sets it as the pipeline's `tm_call`. The 13 verbs that call it pass their own frame, and `run_separation_audio()` and `separate_audio_video()` set it on their own runs. The sweep computes the 15-export domain from the call graph. It calls 11 of them, and it lists 4 with the reason that no call of theirs sets a codec other than `"copy"`, which a second test reads off their compiled commands. A planted `ffm_finish()` that drops the frame reddens the sweep for the 9 verbs that run through it. Full suite: 1806 tests, 0 failed.
- 2026-10-10: T4 done. `ffm_batch()`'s `run_one()` returns the diagnosed encoders and streams, and one warning after the rows carries `tm_encoder`, `tm_stream` and `tm_row`, one element per row and codec. Cells cover `progress = FALSE` and `TRUE`, a row failing for another reason, one encoder over several rows, `run = FALSE`, and a `standardize_video_batch()` column override. The parallel cell runs real FFmpeg through the wrapper in two PSOCK workers. It ran green with the branch installed in a scratch library on `R_LIBS`, and it skips when workers load an older installed copy (M53). Four `test-separate-av-multitrack.R` tests forced a failure with `"notanencoder"` and caught the first warning, which is now the encoder warning. They set it aside through one helper, and one of them asserts that both warnings arrive, encoder first, with rows 1 and 3 of the two-rows-per-caller-row table. Full suite: 1811 tests, 0 failed.
- 2026-10-10: T5 done, with no code change. Since the plan commit `db99245a`, the branch changes three test files: the new file, the absorber list in `test-timeout-silence.R`, and the four separation tests. No purity test, timeout sweep or recorded spawn baseline was edited or re-recorded, and all pass. Plant 1 put a listing read before the spawn in `ffm_run()` through `output_snapshot()`. `test-timeout-refusal-blame.R`'s byte-for-byte baseline test went red with 64 failures. Plant 2 put a listing read in `ffm_compile()`, on the dry-run path. Four of the five purity tests went red. `test-audio-track-drop.R:63` stayed green, because it watches only `run_program()` and the listing spawns through `ffmpeg()`.
- 2026-10-10: T6 checkpoint, not yet checked off: the records are written and `devtools::document()` regenerated four Rd files, and the full suite over them is still running. Written: a `NEWS.md` entry, a section "When the build lacks an encoder" on `?ffm_run` and on `?ffm_batch`, the record paragraph on `?refresh_ffmpeg_capabilities`, the class in `?tidymedia`, the five-part Known issues entry in `cairn/DESIGN.md`, and the trim of the ROADMAP hardware-surface row. The help text's claim that FFmpeg uses no video encoder for an input without video was run here: `-map 0:v? -c:v no_such_enc` exits 0 on an audio-only input and 8 on a video input.
- 2026-10-10: T6 done. The full suite over the records ran 1811 tests with 0 failures.
- 2026-10-10: claim audit: 58 claims read, 7 corrected — NEWS.md, R/cache.R, R/ffmpeg.R (two comments), tests/testthat/test-separate-av-multitrack.R (two claims in one comment), tests/testthat/test-encoder-diagnosis.R. One optional wording fix in the same test file was also taken. The NEWS correction names the case where `separate_audio_video()` keeps its several-tracks error. `cairn/DESIGN.md`'s Known issues gains it as a sixth limit. The same reader's re-read of the corrected claims is pending.
- 2026-10-10: claim audit re-read: of 8 corrected claims, 6 true and 2 still wider. The separation exception holds only for an `audiofile` whose container holds one audio stream: into `.mka` the diagnosed condition arrives, measured by the reader with real FFmpeg. NEWS.md and the Known issues sentence now carry that narrower wording. One re-read only, so no further pass.
- 2026-10-10: implement complete. `devtools::document()` gives no diff, and the final full suite ran 1811 tests with 0 failed and 7 skipped. Status set to review. Open concern for the merge question: the ROADMAP's 0.2.2 window ships M138 alone, and `NEWS.md` puts this milestone under the development heading. So if M139 merges before 0.2.2 is cut, it ships in 0.2.2.
- 2026-10-10: review started. The plan's remainder-ledger line sat under `## Review`, because the re-cut edit wrote the section headings above it. It is moved, unchanged, to its place after the round-2 audit line. No PR exists, master has not moved since the branch was cut, and the Copilot round is off.
- 2026-10-10: review return 1: AC2 fails. A limit reached while the diagnosis reads the listings gives one `tidymedia_probe_timeout` warning per failed row in a batch, and one per half in `separate_audio_video()`, against D049's one warning per call. Fix both, add a batch reached-limit cell, and take every fix-now finding in the Review section.
- 2026-10-10: amendment routed: AC2 — "every `run = FALSE` call behaves as at the merge base" is falsified by AC1's own required class on `check_hardware_available()`'s abort, which fires at `run = FALSE`.
- 2026-10-10: follow-ups filed as candidate rows: diff-bug #8 (unquoted path in `ffmpeg()`) and blame-history #2 (the track diagnostic over a diagnosed missing encoder). Status back to in-progress.
- 2026-10-10: the two follow-ups put `ROADMAP.md` at 61 lines against its cap of under 60, so both are absorbed into existing rows instead: diff-bug #8 into the row on the compiled command's bare token classes, and blame-history #2 as item (d) of the row "The multi-track separation abort is still blind to WHY FFmpeg failed". The Review section lines that name new rows are superseded by this line.
- 2026-10-10: re-audit: AC2 (full) — over the first proposed amendment ("compiles the same command and starts the same programs as at the merge base"), two findings. (1) A failed run's diagnosis fills the remembered encoder listing (D044), so a later hardware dry run starts no program where the merge base starts one. Measured by the reader: 1 listing read before, 0 after a diagnosed failure. Taken: the clause now reads "from the same remembered listings (D044)". (2) "one `tidymedia_probe_timeout` warning also reports it" binds per read, not per call. Held, not taken: writing the per-call grain into AC2 widens it, which D-118 rules out after a defect return. D049 binds the per-call grain on its own, and review return 1 fixes it as a defect, which also satisfies the per-read wording.
- 2026-10-10: re-audit: AC2 (full) — second fresh reader, over the repaired clause ("compiles the same command as at the merge base and, from the same remembered listings (D044), starts the same programs"). It passes satisfiability, reachability, instrument, proportionality and narrowing, with three findings. (1) "starts the same programs" is ambiguous once diff-bug #2's `-hide_banner` lands in the shared listing reader, because a cold hardware dry run then starts `ffmpeg -hide_banner -encoders`. Repair: "starts the same programs as many times". (2) No plant or read count covers `run_one()`, the separation run block or `ffm_finish()`'s `run = FALSE` branch. This is evidence, not wording, and becomes a T7 item. (3) The merge base has no codec listing. Repair: "the same remembered encoder listing". This is the second re-audit line for AC2, so the wording goes to the user.
- 2026-10-10: amendment return: AC2 — "A run that FFmpeg completes, and every `run = FALSE` call, compiles the same command as at the merge base and, from the same remembered encoder listing (D044), starts the same programs as many times, because the diagnosis is reached only after a non-zero exit inside `ffm_run()`."
- 2026-10-10: the user accepted that wording as final with no third reader, and chose `-hide_banner` in the shared listing reads, disclosed in NEWS. T7 added for review return 1, and Coverage maps AC2 to it.
- 2026-10-10: T7 checkpoint, not yet checked off, full suite running. One probe-timeout warning per call: `ffm_batch()`'s `run_one()` holds the row's warning, skips the diagnosis for its later rows (`tm_no_diagnosis`), and the batch warns once with `tm_limit`. `separate_audio_video()` sets that attribute on the video run when the audio half's warning passes. Fix-now findings taken: `-hide_banner` in both listing reads, the location warnings muffled by class only, a video-half sweep cell, the reached-limit test rewritten to one run, a batch and a separation reached-limit cell, a read-count cell for a successful batch, a successful separation and a `run = FALSE` verb, the separation tests renamed with the all-listed case asserting the encoder warning, and help and NEWS corrections (`?ffm_run`, `?ffm_batch`, the `tidymedia.timeout` page). The four affected test files pass: 23, 58, 38 and 8 tests, 0 failed, 0 skipped, with the branch installed for the workers.
- 2026-10-10: T7 done. Full suite: 1814 tests, 0 failed, 7 skipped.

## Decisions

## Review

Evidence run 2026-10-10 at branch head `8ffc503c`, with `NOT_CRAN=true` and the branch installed in a scratch library on `R_LIBS` so parallel workers load it. `test-encoder-diagnosis.R`: 20 tests, 0 failed, 0 skipped, the 3 wrapper cells included. `test-separate-av-multitrack.R`: 58 tests, 0 failed. `test-timeout-silence.R`: 38 tests, 0 failed.

- AC1 evidence: "an encoder the build does not list is named on the exit condition" asserts the class vector exactly (`tidymedia_encoder_unavailable`, `tidymedia_ffmpeg_exit`, …), `tm_status`, `tm_encoder`, `tm_stream`, the message naming encoder and stream beside the merge-base bullets, and no causal wording. "a codec name the build encodes is not diagnosed (h264, mp3)" holds with an unlisted control. "both codecs missing are both named, video first". "copy, an unset codec and a hardware-table name are never asked about" shows zero listing reads for four cells and pins `hardware_encoder_names()` to the table's five names. `ffm_output_options()` encoders are outside by construction: the diagnosis reads only `codec_video` and `codec_audio`, which only `ffm_codec()` sets (R/ffm.R). "the hardware availability abort carries the same class and fields" checks `tm_encoder` and `tm_stream = "video"` at `standardize_video()` and `standardize_video_batch()`, with a control. The wrapper cell "a real run naming a hidden encoder is diagnosed" shows the class on real FFmpeg with libx264 hidden, and its control succeeds.
- AC3 evidence: "a single-file call names the function the caller typed" computes the domain with `tm_reaches()` at run time. It equals the 15 exports the criterion names, and the test fails unless its 11 calls and 4 listed exceptions together equal that domain. Each of the 11 calls (`separate_audio_video()` on a single-track input whose video half succeeds) raises the class with that export as `conditionCall()`. "the listed exceptions compile no codec but copy" reads the four exceptions' compiled commands. "a condition the diagnosis does not touch keeps naming ffm_run()" shows the merge-base exit class exactly and the call `ffm_run` after a verb set `tm_call`. The timeout and verify-failure conditions are raised by code this branch did not change in `ffm_run()`, and `test-timeout-refusal-blame.R` and the verify tests pass in the `check()` run below. At implement, a planted `ffm_finish()` that drops the frame turned the sweep red for 9 verbs.
- AC4 evidence: "a batch runs every row and warns once with the diagnosed rows" covers 5 rows under `progress = FALSE` and `TRUE`. The `success` column is unchanged, a row failing for another reason is not named, and one warning carries `tm_row` 2 and 3 with `tm_encoder` and `tm_stream`. "one encoder missing on several rows is one bullet naming them all". "a batch with no diagnosed row, and a dry-run batch, do not warn". "a batch verb's codec column reaches the warning by its row" uses `standardize_video_batch()`. "parallel workers carry the diagnosed rows back (wrapper)" ran in two PSOCK workers with real FFmpeg and gave `success` FALSE then TRUE and `tm_row` 1. The call graph at review shows 16 exports reaching `ffm_batch()`: the 15 `_batch` exports and `segment_video()`. A grep shows `ffm_batch()`'s one `ffm_run()` call inside its `if (run)` branch (R/ffm_batch.R:184).
- `devtools::check()` at `cc1ed806` (code identical to `8ffc503c`): 0 errors, 0 warnings, 0 notes. Its test run includes `test-timeout-refusal-blame.R`, the purity tests and the recorded spawn baselines.
- AC3 evidence, completed: the timeout and verify-failure conditions of `ffm_run()` pass their existing tests unchanged in that `check()` run, which closes the last clause of AC3.
- AC2 not ticked: diff-bug #1 below shows it failing (review return 1), and diff-bug #5 shows its run = FALSE clause wrong as worded (amendment routed).

Review pass 1, 2026-10-10.
- spawned: diff-bug, blame-history, prior-review
- diff-bug #1: a batch gives one `tidymedia_probe_timeout` warning per failed row whose listing read reaches the limit (3 rows, 3 warnings, measured), against D049's one-per-call grain and AC2, and each later row waits on the listing again. `?ffm_run` says the batch gives one warning — floor return (AC2 failing): review return 1.
- diff-bug #2: the listing reads print two FFmpeg version banners between FFmpeg's error output and the R error — fix now, in the return (read the listings with `-hide_banner`).
- diff-bug #3: no test fails the video half of `separate_audio_video()`, so deleting its `tm_call` line stays green — fix now, in the return (a second sweep call).
- diff-bug #4: the reached-limit test cannot see a wrong warning count, call or ordering — fix now, in the return.
- diff-bug #5: NEWS says a `run = FALSE` call behaves as before, while AC1's hardware abort, which fires at `run = FALSE`, gains a class; AC2's "every `run = FALSE` call behaves as at the merge base" is falsified by AC1 itself — amendment routed for AC2's wording; NEWS fixed in the return.
- diff-bug #6: one class used as an error and as a batch warning lets a `tryCatch()` for the hardware refusal swallow a batch's results — fix now, in the return (one sentence on `?ffm_batch` advising `withCallingHandlers()` for the warning).
- diff-bug #7: NEWS does not say the class is not evidence of the cause — fix now, in the return.
- diff-bug #8: `ffmpeg()` builds its command with an unquoted program path, so on a path with spaces every listing read fails and the diagnosis stays silent — follow-up: pre-existing in master, candidate row "`ffmpeg()` and the two listing functions break on an FFmpeg path with spaces".
- blame-history #1: the batch per-row probe-timeout warnings — same defect as diff-bug #1, review return 1.
- blame-history #2: on a multi-track input into a one-track container, `run_separation_audio()`'s track diagnostic replaces a diagnosed missing encoder with advice to pick one track — follow-up: AC3 holds that diagnostic's merge-base behavior, so changing it is outside this milestone; candidate row naming "The multi-track separation abort is still blind to WHY FFmpeg failed".
- blame-history #3: the separation test named "warns not at all" now holds only for non-encoder warnings, and the all-listed case does not assert the encoder warning — fix now, in the return (rename, and assert that warning).
- blame-history #4: `separate_audio_video()` can give two probe-timeout warnings in one call, one per half — part of review return 1 (D049's per-call grain).
- blame-history #5: the absorber list's guard covers only the single-file reached-limit case — fix now, in the return (a batch reached-limit cell).
- blame-history #6: a stale remembered listing can name an encoder the running binary has — reject, planned change: AC1 reads the session's remembered listing (D044) and AC5's Known issues discloses it.
- blame-history #7: `tm_call` is a new mechanism, an attribute holding a frame — reject, planned change: the implement work log records the choice and why, and D087 governs exported formals only.
- prior-review #1: batch row numbers are reshaped-table numbers for `separate_audio_video_batch()` and two-pass `normalize_audio_batch()` — the numbering is a planned change (AC4, Known issues); the NEWS wording "the job rows" is fixed in the return to say whose rows they are.
- prior-review #2: the `tidymedia.timeout` help page lists every source of `tidymedia_probe_timeout` and misses the new one — fix now, in the return.
- prior-review #3: `?ffm_run`'s "one warning with this class for all its rows" reads as the probe-timeout class — fix now, in the return (with diff-bug #1).
- prior-review #4: separation tests trigger failure with `"notanencoder"` and mute the new warning — reject, planned change: AC5's Known issues discloses the double warning, and one test asserts both arrive.
- prior-review #5: the listing read muffles every warning, not only the program lookup's — fix now, in the return (muffle by class).
- prior-review #6: the parallel cell can skip under `load_all()` — reject, false: the evidence run above ran it on an installed copy, not skipped.

