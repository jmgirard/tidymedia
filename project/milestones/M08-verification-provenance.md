# M08: Verification & provenance

- **Status:** idea <!-- stub; full plan written when this milestone is reached -->
- **Created:** 2026-07-10

## Idea

Make "reproducible pipelines" (D001) auditable, using the M04 metadata layer:
post-run output assertions (duration/dimensions/codec/sample rate within
tolerance, probe-backed) for `ffm_run()` and `ffm_batch()`; a batch run
manifest capturing commands, tool versions, and file checksums; progress
reporting for long batch runs. Follows M06 so verification sits on the safe
execution path.
