# M09: Research-workflow task verbs

- **Status:** idea <!-- stub; full plan written when this milestone is reached -->
- **Created:** 2026-07-10

## Idea

New Layer 2 verbs for common research preprocessing (all four families
confirmed by user 2026-07-10; grouping/order flexible): standardization
presets (`standardize_media()`: resolution/fps/sample-rate/codec across a
corpus); audio normalization (EBU R128 `loudnorm`, mono downmix, resampling);
batch frame extraction at intervals/timestamps (tibble-driven, for
annotation/CV); fixed-region anonymization (box/blur via drawbox/boxblur —
explicitly no face tracking). All thin wrappers over Layer 1 (D002).
