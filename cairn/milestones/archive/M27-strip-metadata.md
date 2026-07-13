# M27: Metadata scrubbing for de-identification (`strip_metadata` + `_batch`) — done

**Goal:** De-identification front door — task verbs that strip identifying
container/global metadata and chapters via lossless stream copy (M25 survey K2).

**Outcome:** Shipped `strip_metadata(infile, outfile)` and
`strip_metadata_batch(jobs, …)` — thin Layer-2 wrappers (IP1) over
`ffm_files`→`ffm_copy`→`ffm_output_options("-map_metadata -1","-map_chapters -1","-fflags +bitexact")`.
The bitexact flag stops FFmpeg re-stamping its own `creation_time`/`encoder`
tag. Streams and the rotation display matrix are preserved bit-for-bit. Shared
`strip_metadata_pipeline()` gives the batch per-row parity (M13); batch guards
duplicated *resolved* outputs (M26). Documented boundary: per-stream tags
(`handler_name`, `language`) and codec-embedded identifiers survive a copy.
Plan-gate calls: global+chapters+bitexact scrub, batch sibling in, zero-config
surface; per-stream stripping ruled out to keep compile binary-free.

**Review:** all 6 ACs passed on fresh evidence; suite 979 pass / 0 fail; check
0/0/0; CI green (7 jobs). Independent 3-lens review: blame-history + prior-PR
clean. Diff-bug **F1** (score 88) fixed — the AC2 execution test checked only
container `format_tags`, not stream tags; added a `probe_stream_tags()` helper
and output-side stream-tag assertions (later made cross-version robust after CI
flagged an ffmpeg-version-fragile fixture sanity check). **F2** (rotation exact
`== 90`, score 35) logged, not actioned.

**PR:** https://github.com/jmgirard/tidymedia/pull/29
