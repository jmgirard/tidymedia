# Package index

## Layer 0: escape hatches

Pass a raw argument string straight to the command-line tools. Use these
when you need something tidymedia does not wrap.

- [`ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/ffmpeg.md)
  : Run a raw FFmpeg command
- [`ffprobe()`](https://jmgirard.github.io/tidymedia/reference/ffprobe.md)
  : Send a command to the FFprobe program
- [`mediainfo()`](https://jmgirard.github.io/tidymedia/reference/mediainfo.md)
  : Run MediaInfo CLI

## Layer 1: the pipeline builder

Assemble a reproducible FFmpeg command step by step, then compile or run
it.

- [`ffm()`](https://jmgirard.github.io/tidymedia/reference/ffm.md) :
  Specify Files in an FFmpeg Pipeline
- [`ffm_files()`](https://jmgirard.github.io/tidymedia/reference/ffm_files.md)
  : Specify Files in an FFmpeg Pipeline
- [`ffm_trim()`](https://jmgirard.github.io/tidymedia/reference/ffm_trim.md)
  : Trim the Duration of the FFmpeg Pipeline
- [`ffm_seek()`](https://jmgirard.github.io/tidymedia/reference/ffm_seek.md)
  : Cut a Continuous Section from an FFmpeg Pipeline by Seeking
- [`ffm_drop()`](https://jmgirard.github.io/tidymedia/reference/ffm_drop.md)
  : Drop Steams from an FFmpeg Pipeline
- [`ffm_crop()`](https://jmgirard.github.io/tidymedia/reference/ffm_crop.md)
  : Crop Frames in an FFmpeg Pipeline
- [`ffm_scale()`](https://jmgirard.github.io/tidymedia/reference/ffm_scale.md)
  : Scale (Resize) Frames in a FFmpeg Pipeline
- [`ffm_fps()`](https://jmgirard.github.io/tidymedia/reference/ffm_fps.md)
  : Set the Frame Rate in an FFmpeg Pipeline
- [`ffm_codec()`](https://jmgirard.github.io/tidymedia/reference/ffm_codec.md)
  : Set Codecs in an FFmpeg Pipeline
- [`ffm_map()`](https://jmgirard.github.io/tidymedia/reference/ffm_map.md)
  : Set the Stream Mapping in an FFmpeg Pipeline
- [`ffm_copy()`](https://jmgirard.github.io/tidymedia/reference/ffm_copy.md)
  : Copy the codecs and map all streams
- [`ffm_pixel_format()`](https://jmgirard.github.io/tidymedia/reference/ffm_pixel_format.md)
  : Set the Pixel Format in an FFmpeg Pipeline
- [`ffm_hstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_hstack.md)
  : Horizontally Stack Multiple Videos in an FFmpeg Pipeline
- [`ffm_vstack()`](https://jmgirard.github.io/tidymedia/reference/ffm_vstack.md)
  : Vertically Stack Multiple Videos in an FFmpeg Pipeline
- [`ffm_overlay()`](https://jmgirard.github.io/tidymedia/reference/ffm_overlay.md)
  : Overlay One Video on Another in an FFmpeg Pipeline
- [`ffm_concat()`](https://jmgirard.github.io/tidymedia/reference/ffm_concat.md)
  : Concatenate Multiple Inputs in an FFmpeg Pipeline
- [`ffm_drawbox()`](https://jmgirard.github.io/tidymedia/reference/ffm_drawbox.md)
  : Draw a Colored Box on the Videos in an FFmpeg Pipeline
- [`ffm_loudnorm()`](https://jmgirard.github.io/tidymedia/reference/ffm_loudnorm.md)
  : Normalize Loudness in an FFmpeg Pipeline
- [`ffm_output_options()`](https://jmgirard.github.io/tidymedia/reference/ffm_output_options.md)
  : Add Raw Output Options to an FFmpeg Pipeline
- [`ffm_compile()`](https://jmgirard.github.io/tidymedia/reference/ffm_compile.md)
  : Compile the tidymedia pipeline into FFmpeg command
- [`ffm_run()`](https://jmgirard.github.io/tidymedia/reference/ffm_run.md)
  : Run the FFmpeg Pipeline
- [`ffm_batch()`](https://jmgirard.github.io/tidymedia/reference/ffm_batch.md)
  : Run an FFmpeg Pipeline Over Many Files
- [`print(`*`<tidymedia_ffm>`*`)`](https://jmgirard.github.io/tidymedia/reference/print.tidymedia_ffm.md)
  : Print an FFmpeg pipeline

## Layer 2: task verbs

Thin wrappers over the builder for common preprocessing jobs.

- [`extract_frame()`](https://jmgirard.github.io/tidymedia/reference/extract_frame.md)
  : Extract a single frame from a video
- [`extract_frames()`](https://jmgirard.github.io/tidymedia/reference/extract_frames.md)
  : Extract Still Frames From Many Videos From a Jobs Table
- [`extract_audio()`](https://jmgirard.github.io/tidymedia/reference/extract_audio.md)
  : Extract the audio stream from a media file
- [`separate_audio_video()`](https://jmgirard.github.io/tidymedia/reference/separate_audio_video.md)
  : Split a media file into separate audio and video files
- [`audio_as_mp3()`](https://jmgirard.github.io/tidymedia/reference/audio_as_mp3.md)
  : Extract a media file's audio as an MP3
- [`normalize_audio()`](https://jmgirard.github.io/tidymedia/reference/normalize_audio.md)
  : Normalize a file's audio loudness (EBU R128)
- [`normalize_audios()`](https://jmgirard.github.io/tidymedia/reference/normalize_audios.md)
  : Normalize Many Files' Audio Loudness From a Jobs Table
- [`crop_video()`](https://jmgirard.github.io/tidymedia/reference/crop_video.md)
  : Crop a video to a rectangular region
- [`anonymize_video()`](https://jmgirard.github.io/tidymedia/reference/anonymize_video.md)
  : Cover fixed regions of a video with opaque boxes
- [`anonymize_videos()`](https://jmgirard.github.io/tidymedia/reference/anonymize_videos.md)
  : Anonymize Many Videos From a Jobs Table
- [`format_for_web()`](https://jmgirard.github.io/tidymedia/reference/format_for_web.md)
  : Re-encode a video for web playback
- [`standardize_video()`](https://jmgirard.github.io/tidymedia/reference/standardize_video.md)
  : Standardize a video to a reproducible format
- [`standardize_videos()`](https://jmgirard.github.io/tidymedia/reference/standardize_videos.md)
  : Standardize Many Videos From a Jobs Table
- [`segment_video()`](https://jmgirard.github.io/tidymedia/reference/segment_video.md)
  : Segment Video
- [`segment_videos()`](https://jmgirard.github.io/tidymedia/reference/segment_videos.md)
  : Segment Many Videos From a Jobs Table
- [`concatenate_videos()`](https://jmgirard.github.io/tidymedia/reference/concatenate_videos.md)
  : Combine video files using the concat demuxer
- [`compare_videos()`](https://jmgirard.github.io/tidymedia/reference/compare_videos.md)
  : Build a side-by-side comparison video
- [`picture_in_picture()`](https://jmgirard.github.io/tidymedia/reference/picture_in_picture.md)
  : Inset one video over another (picture-in-picture)

## Verification & provenance

Check that an output really has the properties you asked for, and record
a reproducibility manifest for a batch run.

- [`verify_media()`](https://jmgirard.github.io/tidymedia/reference/verify_media.md)
  : Verify a Media File Against Expected Properties
- [`ffm_manifest()`](https://jmgirard.github.io/tidymedia/reference/ffm_manifest.md)
  : Read a Batch Provenance Manifest

## Media metadata

Read container and stream metadata as tibbles, ready for the tidyverse.

- [`probe_all()`](https://jmgirard.github.io/tidymedia/reference/probe_all.md)
  : Look up information about media files using FFprobe
- [`probe_container()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)
  [`probe_streams()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)
  [`probe_video()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)
  [`probe_audio()`](https://jmgirard.github.io/tidymedia/reference/probe_container.md)
  : Shortcut functions for probing specific information
- [`mediainfo_parameter()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_parameter.md)
  : Query a single parameter from a single MediaInfo section
- [`mediainfo_query()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_query.md)
  : Query multiple parameters from a single MediaInfo section
- [`mediainfo_template()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_template.md)
  : Describe media files by applying a MediaInfo template
- [`mediainfo_summary()`](https://jmgirard.github.io/tidymedia/reference/mediainfo_summary.md)
  : Describe media files by applying a MediaInfo template
- [`get_duration()`](https://jmgirard.github.io/tidymedia/reference/get_duration.md)
  : Get the duration of a media file
- [`get_framerate()`](https://jmgirard.github.io/tidymedia/reference/get_framerate.md)
  : Get the video frame rate of a media file
- [`get_width()`](https://jmgirard.github.io/tidymedia/reference/get_width.md)
  : Get the video width of a media file
- [`get_height()`](https://jmgirard.github.io/tidymedia/reference/get_height.md)
  : Get the video height of a media file
- [`get_samplingrate()`](https://jmgirard.github.io/tidymedia/reference/get_samplingrate.md)
  : Get the audio sampling rate of a media file
- [`convert_fractions()`](https://jmgirard.github.io/tidymedia/reference/convert_fractions.md)
  : Convert string fractions to doubles

## FFmpeg capabilities

Discover the codecs and encoders available in your FFmpeg build.

- [`get_codecs()`](https://jmgirard.github.io/tidymedia/reference/get_codecs.md)
  : Get a data frame of all installed codecs
- [`get_encoders()`](https://jmgirard.github.io/tidymedia/reference/get_encoders.md)
  : Get a data frame of all installed encoders

## Program management

Locate, configure, and install the FFmpeg and MediaInfo binaries.

- [`find_program()`](https://jmgirard.github.io/tidymedia/reference/find_program.md)
  [`find_mediainfo()`](https://jmgirard.github.io/tidymedia/reference/find_program.md)
  [`find_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/find_program.md)
  [`find_ffprobe()`](https://jmgirard.github.io/tidymedia/reference/find_program.md)
  [`find_ffplay()`](https://jmgirard.github.io/tidymedia/reference/find_program.md)
  : Find the location of a dependency program
- [`set_program()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  [`set_mediainfo()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  [`set_ffmpeg()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  [`set_ffprobe()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  [`set_ffplay()`](https://jmgirard.github.io/tidymedia/reference/set_program.md)
  : Set the location of a dependency program
- [`install_on_win()`](https://jmgirard.github.io/tidymedia/reference/install_on_win.md)
  : Install FFmpeg on Windows

## Utilities

- [`pad_integers()`](https://jmgirard.github.io/tidymedia/reference/pad_integers.md)
  : Pad Integers
