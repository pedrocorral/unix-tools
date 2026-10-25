---
id: TASK-0016
title: Document, test, and implement all remaining BSD parity gaps across all 17 utilities
status: completed
created: 2026-10-24
completed: 2026-10-25
requirements:
  - FR-FILTER-001
  - FR-FILTER-002
  - FR-FS-001
  - FR-FS-002
  - FR-FS-003
  - FR-FS-004
  - FR-PROC-001
  - FR-PROC-002
  - FR-PROC-003
  - FR-SEC-001
  - FR-SEC-002
tests:
  - TS-FILTER-001
  - TS-FILTER-002
  - TS-FS-001
  - TS-FS-002
  - TS-FS-003
  - TS-FS-004
  - TS-PROC-001
  - TS-PROC-002
  - TS-PROC-003
  - TS-SEC-002
  - TS-SEC-003
security:
  - VUL-AUTHN-001
  - VUL-AUTHZ-001
  - VUL-INJ-001
context_files:
  - docs/requirements/decisions/ADR-0002-full-bsd-parity-specification.md
  - src/main/ridgun/lib/filter_lib.rg
  - src/main/ridgun/lib/fs_lib.rg
  - src/main/ridgun/lib/proc_lib.rg
  - src/main/ridgun/lib/sec_lib.rg
  - src/test/ridgun/test_filter.rg
  - src/test/ridgun/test_fs.rg
  - src/test/ridgun/test_make.rg
  - src/test/ridgun/test_nc_timeout.rg
  - src/test/ridgun/test_proc.rg
  - src/test/ridgun/test_sec.rg
  - src/test/ridgun/test_sh.rg
scope:
  - docs/requirements/
  - docs/tests/
  - src/main/ridgun/
  - src/test/ridgun/
owner: agent-001
claimed: 2026-10-24T21:33:45Z
claimed_by: agent gemini@solarfall
---
# TASK-0016 — Document, test, and implement all remaining BSD parity gaps across all 17 utilities

## Goal (one sentence)
Document in full detail (`ADR-0002`, `FR-*`, `TS-*`), write multi-case and combinational tests for, and implement every remaining BSD/POSIX parity gap across all 17 utilities with zero in-memory shortcuts.

## Plan
- [x] Phase 1: Document all 9 categories of BSD/POSIX parity gaps in `ADR-0002`, `FR-*`, and `TS-*`
- [x] Phase 2: Add multi-case unit, E2E, and combinational tests for every requirement across `test_filter.rg`, `test_fs.rg`, `test_make.rg`, `test_nc_timeout.rg`, `test_sec.rg`, and `test_sh.rg`
- [x] Phase 3: Implement all requirements in Pure Ridgun (`filter_lib.rg`, `fs_lib.rg`, `proc_lib.rg`, `sec_lib.rg`, and CLI `main.rg` entry points), rebuild strict binaries, and pass 100% of test suites

## Progress log (append-only, newest last)
- 2026-10-24: Completed Phase 1 — Recorded `ADR-0002` and updated `FR-FILTER-001`, `FR-FILTER-002`, `FR-FS-001`, `FR-FS-002`, `FR-FS-003`, `FR-FS-004`, `FR-PROC-001`, `FR-PROC-002`, `FR-PROC-003`, `FR-SEC-001`, `FR-SEC-002` and all corresponding `TS-*` specifications.
- 2026-10-24: Completed Phase 2 — Added exhaustive multi-case unit, E2E, and combinational tests across `test_filter.rg`, `test_fs.rg`, `test_make.rg`, `test_nc_timeout.rg`, `test_sec.rg`, and `test_sh.rg`.
- 2026-10-25: Completed Phase 3 — Implemented all `ADR-0002` BSD/POSIX parity features across `filter_lib.rg`, `fs_lib.rg`, `proc_lib.rg`, `sec_lib.rg`, `vis/main.rg`, `unvis/main.rg`, `mtree/main.rg`, `doas/main.rg`, and `sha256/main.rg`. Rebuilt all 17 strict ELF64 binaries and verified 100% of test suites (624/624 assertions passing).

## Files changed (keep current)
- `docs/requirements/decisions/ADR-0002-full-bsd-parity-specification.md`
- `docs/requirements/decisions/INDEX.md`
- `docs/requirements/functional/filter/FR-FILTER-001-bsd-jot-and-rs.md`
- `docs/requirements/functional/filter/FR-FILTER-002-bsd-lam-and-vis.md`
- `docs/requirements/functional/fs/FR-FS-001-bsd-ls.md`
- `docs/requirements/functional/fs/FR-FS-002-bsd-find.md`
- `docs/requirements/functional/fs/FR-FS-003-bsd-mtree.md`
- `docs/requirements/functional/fs/FR-FS-004-bsd-mktemp.md`
- `docs/requirements/functional/proc/FR-PROC-001-bsd-apply-and-make.md`
- `docs/requirements/functional/proc/FR-PROC-002-bsd-sh.md`
- `docs/requirements/functional/proc/FR-PROC-003-bsd-timeout-and-nc.md`
- `docs/requirements/functional/sec/FR-SEC-001-bsd-doas.md`
- `docs/requirements/functional/sec/FR-SEC-002-bsd-sha256.md`
- `docs/tests/functional/filter/TS-FILTER-001-jot-and-rs.md`
- `docs/tests/functional/filter/TS-FILTER-002-lam-and-vis.md`
- `docs/tests/functional/fs/TS-FS-001-bsd-ls.md`
- `docs/tests/functional/fs/TS-FS-002-bsd-find.md`
- `docs/tests/functional/fs/TS-FS-003-bsd-mtree.md`
- `docs/tests/functional/fs/TS-FS-004-bsd-mktemp.md`
- `docs/tests/functional/proc/TS-PROC-001-bsd-apply-and-make.md`
- `docs/tests/functional/proc/TS-PROC-002-bsd-sh.md`
- `docs/tests/functional/proc/TS-PROC-003-bsd-timeout-and-nc.md`
- `docs/tests/functional/sec/TS-SEC-002-bsd-doas.md`
- `docs/tests/functional/sec/TS-SEC-003-bsd-sha256.md`
- `src/main/ridgun/lib/filter_lib.rg`
- `src/main/ridgun/lib/fs_lib.rg`
- `src/main/ridgun/lib/proc_lib.rg`
- `src/main/ridgun/lib/sec_lib.rg`
- `src/main/ridgun/vis/main.rg`
- `src/main/ridgun/unvis/main.rg`
- `src/main/ridgun/mtree/main.rg`
- `src/main/ridgun/doas/main.rg`
- `src/main/ridgun/sha256/main.rg`
- `src/test/ridgun/test_filter.rg`
- `src/test/ridgun/test_fs.rg`
- `src/test/ridgun/test_make.rg`
- `src/test/ridgun/test_nc_timeout.rg`
- `src/test/ridgun/test_sec.rg`
- `src/test/ridgun/test_sh.rg`

## Verification performed (command → result, newest last)
- `ridgunc run src/test/ridgun/test_filter.rg` → 124/124 passed
- `ridgunc run src/test/ridgun/test_fs.rg` → 104/104 passed
- `ridgunc run src/test/ridgun/test_sec.rg` → 101/101 passed
- `ridgunc run src/test/ridgun/test_make.rg` → 27/27 passed
- `ridgunc run src/test/ridgun/test_nc_timeout.rg` → 34/34 passed
- `ridgunc run src/test/ridgun/test_sh.rg` → 116/116 passed
- `ridgunc run src/test/ridgun/test_proc.rg` → 77/77 passed
- `ridgunc run src/test/ridgun/test_md.rg` → 41/41 passed
- `stable format` → 32/32 files formatted cleanly

## Exact next actions
1. None (`TASK-0016` complete).

## Decisions / open questions
- See `ADR-0002`.

## Definition of done checklist
- [x] requirements referenced are `implemented`  - [x] tests automated & green
- [x] security entries updated  - [x] docs INDEX updated  - [x] coverage matrix regenerated
