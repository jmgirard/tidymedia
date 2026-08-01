# the executed argument vector is pinned for every enumerated pipeline

    Code
      cat(paste0(names(rendered), ": ", rendered), sep = "\n")
    Output
      extract_audio: -y | -i | <input> | -codec:a | copy | -vn | -map | 0:a:0 | out.aac
      convert_audio: -y | -i | <input> | -q:a | 0 | -map | 0:a:0 | out.mp3
      strip_metadata: -y | -i | <input> | -codec:v | copy | -codec:a | copy | -map_metadata | -1 | -map_chapters | -1 | -fflags | +bitexact | -map | 0 | out.mp4
      concatenate_videos: -y | -f | concat | -safe | 0 | -i | <concat-list> | -codec:v | copy | -codec:a | copy | -map | 0 | out.mp4
      separate_audio_video(audio): -y | -i | <input> | -codec:a | copy | -map | 0:a | out.m4a
      separate_audio_video(video): -y | -i | <input> | -codec:v | copy | -map | 0:v | out.mp4
      crop_video: -y | -i | <input> | -vf | crop=w=32:h=32:x=(in_w-out_w)/2:y=(in_h-out_h)/2 | -codec:a | copy | -map | 0:v? | -map | 0:a? | out.mp4
      segment_video(reencode = TRUE): -y | -i | <input> | -codec:a | copy | -ss | 0 | -to | 1 | -map | 0:v? | -map | 0:a? | seg.mp4
      segment_video(reencode = FALSE): -y | -ss | 0 | -to | 1 | -i | <input> | -codec:v | copy | -codec:a | copy | -avoid_negative_ts | make_zero | -map | 0:v? | -map | 0:a? | seg.mp4
      standardize_video: -y | -i | <input> | -vf | crop=w=floor(in_w/2)*2:h=floor(in_h/2)*2:x=(in_w-out_w)/2:y=(in_h-out_h)/2 | -codec:v | libx264 | -codec:a | copy | -pix_fmt | yuv420p | -movflags | +faststart | -map | 0:v? | -map | 0:a? | out.mp4
      anonymize_video: -y | -i | <input> | -vf | crop=w=floor(in_w/2)*2:h=floor(in_h/2)*2:x=(in_w-out_w)/2:y=(in_h-out_h)/2,drawbox=x=0:y=0:w=10:h=10:c=black:t=fill | -codec:v | libx264 | -codec:a | copy | -pix_fmt | yuv420p | -map | 0:v? | -map | 0:a? | out.mp4
      format_for_web: -y | -i | <input> | -vf | crop=w=floor(in_w/2)*2:h=floor(in_h/2)*2:x=(in_w-out_w)/2:y=(in_h-out_h)/2 | -codec:v | libx264 | -codec:a | aac | -pix_fmt | yuv420p | -movflags | +faststart | -map | 0:v? | -map | 0:a? | out.mp4
      normalize_audio(correction): -y | -i | <input> | -af | loudnorm=I=-23:TP=-1:LRA=7 | -map | 0:a:0 | out.mp4
      normalize_audio(analysis): -y | -i | <input> | -af | loudnorm=I=-23:TP=-1:LRA=7:print_format=json | -f | null | -map | 0:a:0 | -

# compiled commands match snapshots

    Code
      writeLines(compile_scrubbed(ffm_files(f1, "out.mp4")))
    Output
      -y -i "<in1>" "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_files(f1, "out.mp4", overwrite = FALSE)))
    Output
      -n -i "<in1>" "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_trim(ffm_files(f1, "out.mp4"), start = 1, end = 2)))
    Output
      -y -i "<in1>" -vf "trim=start=1:end=2,setpts=PTS-STARTPTS" "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_drop(ffm_files(f1, "out.mp4"), c("audio",
        "subtitles"))))
    Output
      -y -i "<in1>" -an -sn "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_pixel_format(ffm_files(f1, "out.mp4"),
      "yuv420p")))
    Output
      -y -i "<in1>" -pix_fmt yuv420p "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_codec(ffm_files(f1, "out.mp4"), audio = "aac",
      video = "libx264")))
    Output
      -y -i "<in1>" -codec:v libx264 -codec:a aac "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_copy(ffm_files(f1, "out.mp4"))))
    Output
      -y -i "<in1>" -codec:v copy -codec:a copy -map 0 "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_crop(ffm_scale(ffm_files(f1, "out.mp4"), 640,
      480), width = 100, height = 50)))
    Output
      -y -i "<in1>" -vf "scale=w=640:h=480,crop=w=100:h=50:x=(in_w-out_w)/2:y=(in_h-out_h)/2" "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_drawbox(ffm_files(f1, "out.mp4"), color = "red")))
    Output
      -y -i "<in1>" -vf "drawbox=x=0:y=0:w=in_w:h=in_h:c=red:t=fill" "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_hstack(ffm_files(c(f1, f2), "out.mp4"))))
    Output
      -y -i "<in1>" -i "<in2>" -filter_complex "[0:v][1:v]hstack=inputs=2:shortest=0[vout]" -map "[vout]" "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_crop(ffm_hstack(ffm_files(c(f1, f2), "out.mp4")),
      width = 100, height = 50)))
    Output
      -y -i "<in1>" -i "<in2>" -filter_complex "[0:v][1:v]hstack=inputs=2:shortest=0,crop=w=100:h=50:x=(in_w-out_w)/2:y=(in_h-out_h)/2[vout]" -map "[vout]" "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_hstack(ffm_files(c(f1, f2), "out.mp4"), resize = TRUE)))
    Output
      -y -i "<in1>" -i "<in2>" -filter_complex "[0:v][1:v]scale2ref='oh*mdar':'if(lt(main_h,ih),ih,main_h)'[0s][1s];[1s][0s]scale2ref='oh*mdar':'if(lt(main_h,ih),ih,main_h)'[1s][0s];[0s][1s]hstack,setsar=1[vout]" -map "[vout]" "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_vstack(ffm_files(c(f1, f2), "out.mp4"))))
    Output
      -y -i "<in1>" -i "<in2>" -filter_complex "[0:v][1:v]vstack=inputs=2:shortest=0[vout]" -map "[vout]" "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_vstack(ffm_files(c(f1, f2), "out.mp4"), resize = TRUE)))
    Output
      -y -i "<in1>" -i "<in2>" -filter_complex "[0:v][1:v]scale2ref='if(lt(main_w,iw),iw,main_w)':'ow/mdar'[0s][1s];[1s][0s]scale2ref='if(lt(main_w,iw),iw,main_w)':'ow/mdar'[1s][0s];[0s][1s]vstack,setsar=1[vout]" -map "[vout]" "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_overlay(ffm_files(c(f1, f2), "out.mp4"), x = "main_w-overlay_w-16",
      y = 16)))
    Output
      -y -i "<in1>" -i "<in2>" -filter_complex "[0:v][1:v]overlay=x=main_w-overlay_w-16:y=16:shortest=0[vout]" -map "[vout]" "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_overlay(ffm_files(c(f1, f2), "out.mp4"), x = "main_w-overlay_w-16",
      y = 16, scale = 0.25)))
    Output
      -y -i "<in1>" -i "<in2>" -filter_complex "[1:v][0:v]scale2ref=w='main_w*0.25':h='main_w*0.25*ih/iw'[pip][bg];[bg][pip]overlay=x=main_w-overlay_w-16:y=16:shortest=0[vout]" -map "[vout]" "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_seek(ffm_files(f1, "out.mp4"), start = 3, end = 7)))
    Output
      -y -i "<in1>" -ss 3 -to 7 "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_copy(ffm_seek(ffm_files(f1, "out.mp4"), start = 3,
      end = 7, reencode = FALSE))))
    Output
      -y -ss 3 -to 7 -i "<in1>" -codec:v copy -codec:a copy -avoid_negative_ts make_zero -map 0 "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_output_options(ffm_files(f1, "out.mp4"),
      "-q:v 1", "-frames:v 1")))
    Output
      -y -i "<in1>" -q:v 1 -frames:v 1 "out.mp4"
    Code
      writeLines(compile_scrubbed(ffm_concat(ffm_files(c(f1, f2), "out.mp4"))))
    Output
      -y -f concat -safe 0 -i "<concatlist>" -codec:v copy -codec:a copy -map 0 "out.mp4"

