---
id: TASK-0008
title: Replace /dev/stdin text reads with posix read 0 descriptor I/O
status: completed
created: 2026-10-21
completed: 2026-10-21
requirements: [ADR-0001, FR-FILTER-001, FR-FILTER-002, FR-FILTER-003, FR-FS-003, FR-PROC-002]
tests: [TS-FILTER-001, TS-FILTER-002, TS-FILTER-003, TS-FS-003, TS-PROC-002]
security: []
context_files: [src/main/ridgun/lib/filter_lib.rg, src/main/ridgun/rs/main.rg, src/main/ridgun/lam/main.rg, src/main/ridgun/unvis/main.rg, src/main/ridgun/mdrr/main.rg, src/main/ridgun/mtree/main.rg, src/main/ridgun/sh/main.rg, src/test/ridgun/test_filter.rg]
scope: [src/main/ridgun/**, src/test/ridgun/test_filter.rg]
owner: agent-001
claimed: 2026-10-21T20:08:40Z
claimed_by: agent gemini@solarfall
---
# TASK-0008 — Replace `/dev/stdin` text reads with `posix read 0` descriptor I/O

## Goal (one sentence)
Replace all `read file "/dev/stdin"` calls across `rs`, `lam` (`filter_lib.rg`), `unvis`, `mdrr`, `mtree`, and `sh` with loop-buffered `posix read 0 65536` descriptor reads so standard input text streams never depend on `/dev/stdin`.

## Plan
- [x] Add `read stdin all` (looping `posix read 0 65536` until EOF) to `filter_lib.rg` and use it in `parse_lam_file`, `rs/main.rg`, and `unvis/main.rg` (plus `import posix` in `rs/main.rg`, `lam/main.rg`, `unvis/main.rg`, `test_filter.rg`).
- [x] Replace `read file "/dev/stdin"` in `mdrr/main.rg`, `mtree/main.rg`, and `sh/main.rg` with `posix read 0 65536` loop reads.
- [x] Rebuild modified binaries (`rs`, `lam`, `unvis`, `mdrr`, `mtree`, `sh`), run `stable check` and test suites, and close `TASK-0008`.

## Progress log (append-only, newest last)
- 2026-10-21: Created and started `TASK-0008`; replaced all `read file "/dev/stdin"` calls with loop-buffered `posix read 0 65536` reads.
- 2026-10-21: Rebuilt `rs`, `lam`, `unvis`, `mdrr`, `mtree`, `sh` and verified all 4 affected test suites (`test_filter.rg`, `test_md.rg`, `test_fs.rg`, `test_proc.rg` — 100% pass).

## Files changed (keep current)
- `src/main/ridgun/lib/filter_lib.rg`
- `src/main/ridgun/rs/main.rg`
- `src/main/ridgun/lam/main.rg`
- `src/main/ridgun/unvis/main.rg`
- `src/main/ridgun/mdrr/main.rg`
- `src/main/ridgun/mtree/main.rg`
- `src/main/ridgun/sh/main.rg`
- `src/test/ridgun/test_filter.rg`

## Verification performed (command → result, newest last)
- `stable format` → `Formatted 3/8 file(s)`
- `ridgunc build` (`rs`, `lam`, `unvis`, `mdrr`, `mtree`, `sh`) → `6/6 native ELF64 binaries built`
- `ridgunc run src/test/ridgun/test_{filter,md,fs,proc}.rg` → `202/202 assertions passed`

## Exact next actions
1. Complete `TASK-0008` and start `TASK-0009`.

## Decisions / open questions
- None.

## Definition of done checklist
- [x] requirements referenced are `implemented`  - [x] tests automated & green
- [x] security entries updated  - [x] docs INDEX updated  - [x] coverage matrix regenerated
