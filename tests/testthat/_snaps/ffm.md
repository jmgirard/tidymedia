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

