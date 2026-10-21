---
id: TASK-0007
title: Adopt Ridgun v0.9.5 PAL and ridgun-libs (posix.rglib, io.rglib)
status: completed
created: 2026-10-21
completed: 2026-10-21
requirements: [ADR-0001, FR-FILTER-001, FR-FILTER-002, FR-FILTER-003, FR-FS-001, FR-FS-002, FR-FS-003, FR-PROC-001, FR-PROC-002, FR-SEC-001]
tests: [TS-FILTER-001, TS-FILTER-002, TS-FILTER-003, TS-FS-001, TS-FS-002, TS-FS-003, TS-PROC-001, TS-PROC-002, TS-SEC-002]
security: []
context_files: [build.rg, src/main/ridgun/lib/proc_lib.rg, src/main/ridgun/lib/sec_lib.rg, src/main/ridgun/jot/main.rg, src/main/ridgun/vis/main.rg, src/main/ridgun/mdrr/main.rg, src/main/ridgun/ls/main.rg, src/main/ridgun/find/main.rg, src/main/ridgun/mtree/main.rg, src/main/ridgun/sh/main.rg, src/main/ridgun/doas/main.rg, src/test/ridgun/test_proc.rg]
scope: [build.rg, src/main/ridgun/**, src/test/ridgun/**]
owner: agent-001
claimed: 2026-10-21T19:51:12Z
claimed_by: agent gemini@solarfall
---
# TASK-0007 — Adopt Ridgun v0.9.5 PAL and ridgun-libs (`posix.rglib`, `io.rglib`)

## Goal (one sentence)
Adopt the Ridgun v0.9.5 Platform Abstraction Layer (PAL) and compile/consume `posix.rglib` and `io.rglib` from `/home/boogie/ridgun-libs` (read-only) across `unix-tools`, replacing `/dev/stdout` and `/dev/stderr` file writes with `posix write` descriptor I/O and upgrading `proc_lib.rg` (`sh`) to use `append file`, `is file`, `is dir`, `file exists`, `sys getpid`, and `sys unlink`.

## Plan
- [x] Configure `build.rg` and build `posix.rglib` and `io.rglib` into `.stable/deps/` and `target/` from `/home/boogie/ridgun-libs/src/main/ridgun/lib/{posix,io}.rg` (strictly read-only on `/home/boogie/ridgun-libs`).
- [x] Update utility entrypoints (`jot`, `vis`, `mdrr`, `ls`, `find`, `mtree`, `sh`, `doas`) and `sec_lib.rg` to import `posix` and write stdout/stderr via `posix write 1` and `posix write 2` instead of `write file "/dev/stdout"` / `write file "/dev/stderr"`.
- [x] Update `proc_lib.rg` to import `posix` and `io`, replacing manual `RedirAppend` (`read_file` + `write_file`) with `append file`, using `file exists` / `is file` / `is dir` in `test`/`[`, and using `sys getpid` + `sys unlink` for collision-free command capture in `run_external_capture`.
- [x] Extend `src/test/ridgun/test_proc.rg` with PAL & `posix`/`io` library assertions (`null device`, `path separator`, `process exists`, `io read with timeout`, `append file`) and verify all 5 test suites (`stable check`, `stable built`, `stable test`, `aix docs validate`).

## Progress log (append-only, newest last)
- 2026-10-21: Created task `TASK-0007` after inspecting `/tmp/ridgun_posix_io_library_spec.md` and `/home/boogie/ridgun-libs/src/main/ridgun/lib/{posix,io}.rg`.
- 2026-10-21: Built `.stable/deps/{posix,io}.rglib` and `target/{posix,io}.rglib` from `/home/boogie/ridgun-libs` (read-only), updated `build.rg`, `.gitignore`, `ADR-0001`, `TS-PROC-002`, `proc_lib.rg`, `sec_lib.rg`, utility entrypoints, and `test_proc.rg` / `test_sec.rg`.
- 2026-10-21: Verified `stable check` (`19 modules, 0 errors`), rebuilt all native ELF64 binaries in `target/`, and ran all 5 test suites (`223/223` assertions passing).

## Files changed (keep current)
- `.gitignore`
- `build.rg`
- `docs/requirements/decisions/ADR-0001-adopt-aix-and-choose-stack.md`
- `docs/tests/functional/proc/TS-PROC-002-bsd-sh.md`
- `src/main/ridgun/apply/main.rg`
- `src/main/ridgun/doas/main.rg`
- `src/main/ridgun/find/main.rg`
- `src/main/ridgun/jot/main.rg`
- `src/main/ridgun/lib/proc_lib.rg`
- `src/main/ridgun/lib/sec_lib.rg`
- `src/main/ridgun/ls/main.rg`
- `src/main/ridgun/make/main.rg`
- `src/main/ridgun/mdrr/main.rg`
- `src/main/ridgun/mtree/main.rg`
- `src/main/ridgun/sh/main.rg`
- `src/main/ridgun/vis/main.rg`
- `src/test/ridgun/test_proc.rg`
- `src/test/ridgun/test_sec.rg`

## Verification performed (command → result, newest last)
- `aix docs validate` → `0 errors, 0 warnings`
- `aix docs coverage` → `wrote docs/tests/coverage-matrix.md (12 requirements, 11 test specs)`
- `stable format` → `0/12` & `0/6` files needed reformatting (all canonical)
- `stable check` → `Checked 19 module(s) — 0 errors`
- `ridgunc run src/test/ridgun/test_{filter,md,fs,proc,sec}.rg` → `5/5 test suites passed (223/223 assertions, 0 failures)`

## Exact next actions
1. None — task completed.

## Decisions / open questions
- `/home/boogie/ridgun-libs` is strictly read-only; all compiled `.rglib` bundles are emitted inside `/home/boogie/unix-tools/.stable/deps/` and `/home/boogie/unix-tools/target/`.

## Definition of done checklist
- [x] requirements referenced are `implemented`  - [x] tests automated & green
- [x] security entries updated  - [x] docs INDEX updated  - [x] coverage matrix regenerated
