# M145: The `ffm_*()` pipeline help pages state what the code does

**Status:** done (2026-10-18, PR #149 https://github.com/jmgirard/tidymedia/pull/149)

**Goal:** Thirteen `ffm_*()` help-page claims that the M128 and M131 claim audits found false or could not settle now say what the code does.

feature series: M144 -> M145 -> M146

**Outcome:** Roxygen and three code comments corrected, no code behavior changed. `?ffm_map` names the appending functions (the filter functions, `ffm_output_options()`) and the replacing ones, says `ffm_copy()` with `streams = TRUE` refuses a mapping other than "0", and limits the `[vout]` map to `ffm_hstack()`, `ffm_vstack()` and `ffm_overlay()`. `?ffm_run`: a non-zero exit gives an error; each argument is `shQuote()`d before `system2()`, which uses `sh` on Unix; `tm_status` for a killed FFmpeg is 255 for SIGTERM and 137 for SIGKILL (run, macOS, FFmpeg 9.0.2). `?ffm_seek`: `reencode = FALSE` is a copy only with `ffm_copy()`; a copy cut starts at the keyframe at or before `start`, and its end fell at 7 s without B-frames and 2 frames later with them (runs). `?ffm_drop`: streams leave the output only. Crop, scale, fps, drawbox and overlay: a double or a string, an integer or `NA` refused, any other string unchecked; `thickness` included. `?ffm_hstack`/`?ffm_vstack`: `shortest` does nothing with `resize = TRUE`. `?ffm_loudnorm`: no audio filter with a stack or overlay in either order; FFmpeg's `linear` default is true. The vignette's fast-cut sentence and the `ffm_batch()` comment match. `NEWS.md`'s series bullet names the twelve pages.

**Decisions:** none.

**Review:** One pass with the three-lens fan-out after an implement claim audit (35 claims read, 2 corrected). 25 findings: 7 fixed (among them the `ffm_batch()` comment, the vignette's "nearest keyframe", the B-frame end cut, `NA` and `thickness`), 5 to candidate rows, 13 rejected (among them a reading of D093 as barring new claims, rejected because this milestone is the correction D093 sends false claims to). A fresh reader checked the fixes: 15 claims, 2 refined. CI: all 11 checks green. Hygiene narrowed the "Six help-text claims on the `ffm_*()` input, output and run pages" row to its M130 part for M146, and the run added the rows "`ffm_run()` signal to the shell" and "Keyframe wording left after M145". No lesson added: the B-frame finding is LESSONS line 8's over-generalizing case.
