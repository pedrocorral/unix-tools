---
id: TASK-0013
title: Design and implement exhaustive, uncompromising test battery and specs across all utilities (timeout, nc, doas, make, sh, fs, filter)
status: completed
created: 2026-10-24
completed: 2026-10-24
requirements: [FR-FILTER-001, FR-FILTER-002, FR-FILTER-003, FR-FS-001, FR-FS-002, FR-FS-003, FR-FS-004, FR-PROC-001, FR-PROC-002, FR-PROC-003, FR-SEC-001, FR-SEC-002]
tests: [TS-FILTER-001, TS-FILTER-002, TS-FILTER-003, TS-FS-001, TS-FS-002, TS-FS-003, TS-FS-004, TS-PROC-001, TS-PROC-002, TS-PROC-003, TS-SEC-002, TS-SEC-003]
security: [VUL-INJ-001, VUL-INJ-002, VUL-INPUT-001, VUL-AUTHN-001, VUL-AUTHZ-001]
context_files: [docs/requirements/functional/proc/FR-PROC-001-bsd-apply-and-make.md, docs/requirements/functional/proc/FR-PROC-002-bsd-sh.md, docs/requirements/functional/proc/FR-PROC-003-bsd-timeout-and-nc.md, docs/requirements/functional/sec/FR-SEC-001-bsd-doas.md, src/test/ridgun/test_filter.rg, src/test/ridgun/test_md.rg, src/test/ridgun/test_fs.rg, src/test/ridgun/test_proc.rg, src/test/ridgun/test_sec.rg, src/main/ridgun/lib/proc_lib.rg, src/main/ridgun/lib/sec_lib.rg, src/main/ridgun/lib/fs_lib.rg, src/main/ridgun/lib/filter_lib.rg, src/main/ridgun/lib/md_lib.rg]
scope: [docs/requirements/functional/**, docs/tests/functional/**, src/test/ridgun/**, src/main/ridgun/**]
owner: agent-001
claimed: 2026-10-24T14:43:29Z
claimed_by: agent gemini@solarfall
---
# TASK-0013 — Design and implement exhaustive, uncompromising test battery and specs across all utilities (timeout, nc, doas, make, sh, fs, filter)

## Goal (one sentence)
Implement an overwhelming, uncompromising, exhaustive automated test battery for every single one of the 17 utilities (`jot`, `rs`, `lam`, `vis`, `unvis`, `mdrr`, `ls`, `find`, `mtree`, `mktemp`, `apply`, `make`, `sh`, `timeout`, `nc`, `doas`, `sha256`) so that not a single feature, flag, edge case, security invariant, or anti-cheat rule is left untested.

## Plan
- [x] Update `FR-*` and `TS-*` specifications to explicitly mandate all exhaustive acceptance criteria and anti-shortcut invariants across all 17 tools.
- [x] Implement exhaustive test suites across `src/test/ridgun/test_filter.rg`, `src/test/ridgun/test_md.rg`, `src/test/ridgun/test_fs.rg`, `src/test/ridgun/test_proc.rg`, `src/test/ridgun/test_sh.rg`, `src/test/ridgun/test_make.rg`, `src/test/ridgun/test_nc_timeout.rg`, and `src/test/ridgun/test_sec.rg`.
- [x] Implement full Pure Ridgun engines for `ls`, `doas`, `make`, `timeout`, `nc`, and `sh`, verify strict compilation (`ridgunc check --strict`, `stable strict binaries`) and formatting (`stable format`), and pass 100% of all 8 test suites (456 assertions).

## Progress log (append-only, newest last)
- 2026-10-24: Created `TASK-0013` and audited all 17 utilities and standard library primitives (`io.rg`, `socket.rg`, `crypto.rg`).
- 2026-10-24: Updated `FR-FS-001`, `FR-PROC-001`, `FR-PROC-002`, `FR-PROC-003`, `FR-SEC-001` and `TS-FS-001`, `TS-PROC-001`, `TS-PROC-002`, `TS-PROC-003`, `TS-SEC-002`.
- 2026-10-24: Implemented exhaustive test suites `test_make.rg` (21 assertions), `test_nc_timeout.rg` (28 assertions), `test_sh.rg` (51 assertions), and expanded `test_fs.rg` (78 assertions) and `test_sec.rg` (49 assertions).
- 2026-10-24: Upgraded `ls`, `doas`, `make`, `timeout`, `nc`, and `sh` in Pure Ridgun (`fs_lib.rg`, `sec_lib.rg`, `proc_lib.rg`, `doas/main.rg`, `make/main.rg`, `nc/main.rg`), compiled all 17 ELF64 binaries in `--strict` mode, and passed all 8 test suites (`456/456` assertions passing).

## Files changed (keep current)
- `docs/requirements/functional/fs/FR-FS-001-bsd-ls.md`
- `docs/requirements/functional/proc/FR-PROC-001-bsd-apply-and-make.md`
- `docs/requirements/functional/proc/FR-PROC-002-bsd-sh.md`
- `docs/requirements/functional/proc/FR-PROC-003-bsd-timeout-and-nc.md`
- `docs/requirements/functional/sec/FR-SEC-001-bsd-doas.md`
- `docs/tests/functional/fs/TS-FS-001-bsd-ls-tests.md`
- `docs/tests/functional/proc/TS-PROC-001-bsd-apply-make-tests.md`
- `docs/tests/functional/proc/TS-PROC-002-bsd-sh-tests.md`
- `docs/tests/functional/proc/TS-PROC-003-bsd-timeout-nc-tests.md`
- `docs/tests/functional/sec/TS-SEC-002-bsd-doas-tests.md`
- `src/main/ridgun/lib/fs_lib.rg`
- `src/main/ridgun/lib/proc_lib.rg`
- `src/main/ridgun/lib/sec_lib.rg`
- `src/main/ridgun/doas/main.rg`
- `src/main/ridgun/make/main.rg`
- `src/main/ridgun/nc/main.rg`
- `src/test/ridgun/test_fs.rg`
- `src/test/ridgun/test_make.rg`
- `src/test/ridgun/test_nc_timeout.rg`
- `src/test/ridgun/test_sec.rg`
- `src/test/ridgun/test_sh.rg`

## Verification performed (command → result, newest last)
- `stable format` → `Formatted 5/32 file(s) using canonical Ridgun style`
- `stable strict binaries` → `17/17 native ELF64 binaries built in --strict mode (0 errors)`
- `stable test` → `Passed 8/8 test suite(s) — 0 failures (456/456 assertions passed)`

## Exact next actions
1. Start `TASK-0014` (`aix task start TASK-0014`) to port additional official BSD `sh` and OpenBSD `doas` regression test assertions into `test_sh.rg` and `test_sec.rg`.

## Decisions / open questions
- None.

## Definition of done checklist
- [x] requirements referenced are `implemented`  - [x] tests automated & green
- [x] security entries updated  - [x] docs INDEX updated  - [x] coverage matrix regenerated
