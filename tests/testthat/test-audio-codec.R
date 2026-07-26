# audio_codec on the four re-encode verbs and their _batch siblings (M35, D017).
#
# Unlike video_codec's NULL sentinel (M34/D016), audio_codec defaults to "copy":
# these verbs never need to touch the audio, so re-encoding it to the local
# build's container default was pure loss. The compiled commands therefore DO
# change — every default gains -codec:a copy — and the tests below pin the new
# literals so the change stays exactly that one token. NULL is retained as the
# "emit no -codec:a" escape hatch. All compile tests here are binary-free.

# crop_video() ----------------------------------------------------------------

test_that("crop_video() stream-copies audio by default", {
  f <- make_input()
  cmd <- crop_video(f, "out.mp4", width = 100, height = 50, x = 0, y = 0,
                    run = FALSE)
  # The pre-M35 literal, plus -codec:a copy and nothing else.
  expect_equal(
    as.character(cmd),
    paste0('-y -i "', f, '" -vf "crop=w=100:h=50:x=0:y=0" ',
           '-codec:a copy -map 0 "out.mp4"')
  )
  expect_no_match(as.character(cmd), "-codec:v", fixed = TRUE)
})

test_that("crop_video(audio_codec = ) names the audio encoder", {
  f <- make_input()
  cmd <- crop_video(f, "out.mp4", width = 100, height = 50,
                    audio_codec = "aac", run = FALSE)
  expect_match(as.character(cmd), "-codec:a aac", fixed = TRUE)
  expect_no_match(as.character(cmd), "-codec:a copy", fixed = TRUE)
})

test_that("crop_video(audio_codec = NULL) emits no -codec:a", {
  f <- make_input()
  cmd <- crop_video(f, "out.mp4", width = 100, height = 50,
                    audio_codec = NULL, run = FALSE)
  expect_no_match(as.character(cmd), "-codec:a", fixed = TRUE)
  # The rest of the command is untouched by the escape hatch.
  expect_equal(
    as.character(cmd),
    paste0('-y -i "', f, '" -vf "crop=w=100:h=50:x=',
           '(in_w-out_w)/2:y=(in_h-out_h)/2" -map 0 "out.mp4"')
  )
})

test_that("crop_video() rejects a non-token audio_codec", {
  f <- make_input()
  expect_error(
    crop_video(f, "out.mp4", width = 100, height = 50,
               audio_codec = "aac -evil", run = FALSE),
    "clean token"
  )
  expect_error(
    crop_video(f, "out.mp4", width = 100, height = 50,
               audio_codec = 1, run = FALSE),
    class = "rlang_error"
  )
})

test_that("crop_video() sets both codecs when both are named", {
  f <- make_input()
  cmd <- crop_video(f, "out.mp4", width = 100, height = 50,
                    video_codec = "libx265", audio_codec = "aac", run = FALSE)
  expect_match(as.character(cmd), "-codec:v libx265", fixed = TRUE)
  expect_match(as.character(cmd), "-codec:a aac", fixed = TRUE)
})
