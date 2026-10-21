---
id: TASK-0009
title: Implement BSD mktemp, timeout, nc, and sha256 utilities using ridgun-libs
status: completed
created: 2026-10-21
completed: 2026-10-21
requirements: [FR-FS-004, FR-PROC-003, FR-SEC-002]
tests: [TS-FS-004, TS-PROC-003, TS-SEC-003]
security: [VUL-INJ-001, VUL-INPUT-001, VUL-AUTHZ-001]
context_files:
  - build.rg
  - docs/requirements/functional/fs/FR-FS-004-bsd-mktemp.md
  - docs/requirements/functional/proc/FR-PROC-003-bsd-timeout-and-nc.md
  - docs/requirements/functional/sec/FR-SEC-002-bsd-sha256.md
  - docs/tests/functional/fs/TS-FS-004-bsd-mktemp.md
  - docs/tests/functional/proc/TS-PROC-003-bsd-timeout-and-nc.md
  - docs/tests/functional/sec/TS-SEC-003-bsd-sha256.md
  - src/main/ridgun/lib/fs_lib.rg
  - src/main/ridgun/lib/proc_lib.rg
  - src/main/ridgun/lib/sec_lib.rg
  - src/main/ridgun/lib/test.rg
  - src/main/ridgun/mktemp/main.rg
  - src/main/ridgun/timeout/main.rg
  - src/main/ridgun/nc/main.rg
  - src/main/ridgun/sha256/main.rg
  - src/test/ridgun/test_filter.rg
  - src/test/ridgun/test_md.rg
  - src/test/ridgun/test_fs.rg
  - src/test/ridgun/test_proc.rg
  - src/test/ridgun/test_sec.rg
scope:
  - "build.rg"
  - "docs/requirements/**"
  - "docs/tests/**"
  - "src/main/ridgun/**"
  - "src/test/ridgun/**"
owner: agent-001
claimed: 2026-10-21T20:13:52Z
claimed_by: agent gemini@solarfall
---
# TASK-0009 — Implement BSD mktemp, timeout, nc, and sha256 utilities using ridgun-libs

## Goal (one sentence)
Implement four pure-Ridgun BSD utilities (`mktemp`, `timeout`, `nc`, and `sha256`) powered by `posix.rglib`, `io.rglib`, `socket.rglib`, and `crypto.rglib` from `/home/boogie/ridgun-libs` without external subprocesses.

## Plan
- [x] Compile `crypto.rglib` and `socket.rglib` from `/home/boogie/ridgun-libs/src/main/ridgun/lib/` into `.stable/deps/` and `target/`.
- [x] Author and index requirements (`FR-FS-004`, `FR-PROC-003`, `FR-SEC-002`) and test specs (`TS-FS-004`, `TS-PROC-003`, `TS-SEC-003`).
- [x] Implement `run_mktemp` in `src/main/ridgun/lib/fs_lib.rg`, `run_timeout` and `run_nc` in `src/main/ridgun/lib/proc_lib.rg`, and `run_sha256` in `src/main/ridgun/lib/sec_lib.rg`.
- [x] Create CLI entrypoints `src/main/ridgun/{mktemp,timeout,nc,sha256}/main.rg` and register targets in `build.rg`.
- [x] Add automated unit & integration tests in `src/test/ridgun/test_{fs,proc,sec}.rg` and verify compilation and test runs.

## Progress log (append-only, newest last)
- 2026-10-21: Built `crypto.rglib` and `socket.rglib` into `.stable/deps/` and `target/`, authored `FR-FS-004`, `FR-PROC-003`, `FR-SEC-002` and `TS-FS-004`, `TS-PROC-003`, `TS-SEC-003`, implemented `mktemp`, `timeout`, `nc`, and `sha256`, and added automated test suites.

## Files changed (keep current)
- `build.rg`
- `docs/requirements/product/glossary.md`
- `docs/requirements/INDEX.md`
- `docs/requirements/functional/fs/INDEX.md`
- `docs/requirements/functional/fs/FR-FS-004-bsd-mktemp.md`
- `docs/requirements/functional/proc/INDEX.md`
- `docs/requirements/functional/proc/FR-PROC-003-bsd-timeout-and-nc.md`
- `docs/requirements/functional/sec/INDEX.md`
- `docs/requirements/functional/sec/FR-SEC-002-bsd-sha256.md`
- `docs/tests/INDEX.md`
- `docs/tests/functional/fs/INDEX.md`
- `docs/tests/functional/fs/TS-FS-004-bsd-mktemp.md`
- `docs/tests/functional/proc/INDEX.md`
- `docs/tests/functional/proc/TS-PROC-003-bsd-timeout-and-nc.md`
- `docs/tests/functional/sec/INDEX.md`
- `docs/tests/functional/sec/TS-SEC-003-bsd-sha256.md`
- `src/main/ridgun/lib/fs_lib.rg`
- `src/main/ridgun/lib/proc_lib.rg`
- `src/main/ridgun/lib/sec_lib.rg`
- `src/main/ridgun/lib/test.rg`
- `src/main/ridgun/mktemp/main.rg`
- `src/main/ridgun/timeout/main.rg`
- `src/main/ridgun/nc/main.rg`
- `src/main/ridgun/sha256/main.rg`
- `src/main/ridgun/apply/main.rg`
- `src/main/ridgun/make/main.rg`
- `src/main/ridgun/sh/main.rg`
- `src/main/ridgun/doas/main.rg`
- `src/test/ridgun/test_filter.rg`
- `src/test/ridgun/test_md.rg`
- `src/test/ridgun/test_fs.rg`
- `src/test/ridgun/test_proc.rg`
- `src/test/ridgun/test_sec.rg`

## Verification performed (command → result, newest last)
- `for t in mktemp timeout nc sha256 ls find mtree apply make sh doas; do ridgunc build src/main/ridgun/$t/main.rg -o target/$t; done` → all 11 ELF binaries built cleanly (0 errors).
- `ridgunc run src/test/ridgun/test_filter.rg` → 83/83 assertions passed.
- `ridgunc run src/test/ridgun/test_md.rg` → 31/31 assertions passed.
- `ridgunc run src/test/ridgun/test_fs.rg` → 68/68 assertions passed (including `TS-FS-004` for `mktemp`).
- `ridgunc run src/test/ridgun/test_proc.rg` → 63/63 assertions passed (including `TS-PROC-003` for `timeout` and `nc`).
- `ridgunc run src/test/ridgun/test_sec.rg` → 38/38 assertions passed (including `TS-SEC-003` for `sha256`).
- `stable check` → 23/23 modules checked, 0 errors.

## Exact next actions
1. Verify test runs and `stable check`, then complete `TASK-0009` and commit.

## Decisions / open questions
- None.

## Definition of done checklist
- [x] requirements referenced are `implemented`  - [x] tests automated & green
- [x] security entries updated  - [x] docs INDEX updated  - [x] coverage matrix regenerated
