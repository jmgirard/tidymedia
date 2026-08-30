# M093: The separation help pages render their container list from the vector

**Status:** done (2026-08-30, PR #97 https://github.com/jmgirard/tidymedia/pull/97)

**Goal:** Both separation help pages take their nine-container enumeration and
its count word from `multi_audio_extensions` itself, so the prose cannot
disagree with the vector.

**Outcome:** Two renderers beside the vector in `R/ffmpeg.R` —
`multi_audio_rd_list()` (dotted extensions, comma-joined, one `and` last) and
`multi_audio_rd_count()` (length as an English word, from the
`multi_audio_count_words` lookup for two through twelve). Both default to the
vector, refuse under 2 members as `rd_verb_list()` does, and abort rather than
render `NA` past the vocabulary. Both help pages paste them in through inline
`` `r ` `` roxygen calls at all six hand-written sites; the `R/ffmpeg.R:649`
comment is reworded off its count. New guard `test-separation-container-docs.R`
holds every Rd topic naming `\code{.opus}` to the renderer's string, plus the
converse over the marker clause. Rendered wording unchanged, so no NEWS entry.

**Decisions:** none cross-cutting; four plan-gate choices in the work log.

**Review:** three-lens fan-out, no correctness defects; two lenses returned
nothing. Six minor diff-lens findings: the count vocabulary's implicit offset
(fixed), the `.avi`/`.nut` counter-examples (follow-up onto the exemplar row),
T2's re-wrap superseded by a wrap-independent guard (logged), three rejected.
