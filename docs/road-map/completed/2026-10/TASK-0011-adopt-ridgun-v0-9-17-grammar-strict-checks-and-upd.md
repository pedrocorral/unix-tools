---
id: TASK-0011
title: Adopt Ridgun v0.9.17 grammar, strict checks, and updated /home/boogie/ridgun/libs
status: completed
created: 2026-10-24
completed: 2026-10-24
requirements: [ADR-0001, FR-FILTER-001, FR-FILTER-002, FR-FILTER-003, FR-FS-001, FR-FS-002, FR-FS-003, FR-FS-004, FR-PROC-001, FR-PROC-002, FR-PROC-003, FR-SEC-001, FR-SEC-002]
tests: [TS-FILTER-001, TS-FILTER-002, TS-FILTER-003, TS-FS-001, TS-FS-002, TS-FS-003, TS-FS-004, TS-PROC-001, TS-PROC-002, TS-PROC-003, TS-SEC-002, TS-SEC-003]
security: []
context_files: [build.rg, README.md, docs/requirements/decisions/ADR-0001-adopt-aix-and-choose-stack.md, src/main/ridgun/lib/filter_lib.rg, src/main/ridgun/lib/fs_lib.rg, src/main/ridgun/lib/md_lib.rg, src/main/ridgun/lib/proc_lib.rg, src/main/ridgun/lib/sec_lib.rg, src/test/ridgun/test_filter.rg, src/test/ridgun/test_fs.rg, src/test/ridgun/test_md.rg, src/test/ridgun/test_proc.rg, src/test/ridgun/test_sec.rg]
scope: [build.rg, README.md, docs/requirements/decisions/ADR-0001-adopt-aix-and-choose-stack.md, src/main/ridgun/**, src/test/ridgun/**]
owner: agent-002
claimed: 2026-10-24T12:58:40Z
claimed_by: agent gemini@solarfall
---
# TASK-0011 — Adopt Ridgun v0.9.17 grammar, strict checks, and updated `/home/boogie/ridgun/libs`

## Goal (one sentence)
Upgrade `unix-tools` (`build.rg`, shared libraries, 17 utility entrypoints, test suites, and documentation) to Ridgun `v0.9.17` (`with field = value` record updates, single-declaration functions, space-separated sequence types, `--strict` check compliance, and updated `.rglib` dependencies from `/home/boogie/ridgun/libs`).

## Plan
- [x] Update `ADR-0001`, `README.md`, and `build.rg` to point to `/home/boogie/ridgun/libs/src/main/ridgun/lib/` and Ridgun `v0.9.17`, and re-sync `.stable/deps/*.rglib` and `target/*.rglib`.
- [x] Migrate all product record updates across `filter_lib.rg`, `fs_lib.rg`, `md_lib.rg`, `proc_lib.rg`, `sec_lib.rg`, and `test_sec.rg` from `with field: value` to `with field = value`.
- [x] Resolve duplicate function definitions in `md_lib.rg` (`alert color and title`) and transitive `posix` duplicate definitions across `io.rglib` / `socket.rglib` imports in `proc_lib.rg`, `apply`, `make`, `sh`, `timeout`, `nc`, and `test_proc.rg`.
- [x] Modernize tuple signatures to space-separated sequence types (`FR-COMP-008`), format with `stable format`, verify `stable check` and `ridgunc check --strict`, rebuild all 17 ELF64 binaries with `ridgunc build --strict`, and run all 5 test suites.

## Progress log (append-only, newest last)
- 2026-10-24: Created `TASK-0011` to upgrade `unix-tools` for Ridgun `v0.9.17` and the relocated `/home/boogie/ridgun/libs` standard library repository.
- 2026-10-24: Updated `build.rg`, `ADR-0001`, and `README.md` to reference `/home/boogie/ridgun/libs` and Ridgun `v0.9.17`, added `task sync libs` and `task strict binaries` (`ridgunc build --strict`), and synced `posix.rglib`, `io.rglib`, `crypto.rglib`, and `socket.rglib`.
- 2026-10-24: Migrated all 29 `.rg` files across `filter_lib`, `fs_lib`, `md_lib`, `proc_lib`, `sec_lib`, 17 utility `main.rg` entrypoints, and 5 test suites to Ridgun `v0.9.17` (`with field = value` record updates, space-separated `fn ::` / `op ::` headers with zero underscores, space-separated sequence return types `=> A B`, and deduplicated transitive library imports).
- 2026-10-24: Verified `ridgunc check --strict` across all 29 `.rg` files (0 errors), ran `stable format` and `stable check` (23/23 modules OK), compiled all 17 native ELF64 binaries via `stable strict binaries` (`ridgunc build --strict`), and passed all 5 test suites (`283/283` assertions).

## Files changed (keep current)
- `build.rg`
- `README.md`
- `docs/requirements/decisions/ADR-0001-adopt-aix-and-choose-stack.md`
- `src/main/ridgun/lib/filter_lib.rg`
- `src/main/ridgun/lib/fs_lib.rg`
- `src/main/ridgun/lib/md_lib.rg`
- `src/main/ridgun/lib/proc_lib.rg`
- `src/main/ridgun/lib/sec_lib.rg`
- `src/main/ridgun/apply/main.rg`
- `src/main/ridgun/doas/main.rg`
- `src/main/ridgun/find/main.rg`
- `src/main/ridgun/jot/main.rg`
- `src/main/ridgun/lam/main.rg`
- `src/main/ridgun/ls/main.rg`
- `src/main/ridgun/make/main.rg`
- `src/main/ridgun/mdrr/main.rg`
- `src/main/ridgun/mktemp/main.rg`
- `src/main/ridgun/mtree/main.rg`
- `src/main/ridgun/nc/main.rg`
- `src/main/ridgun/rs/main.rg`
- `src/main/ridgun/sh/main.rg`
- `src/main/ridgun/sha256/main.rg`
- `src/main/ridgun/timeout/main.rg`
- `src/main/ridgun/unvis/main.rg`
- `src/main/ridgun/vis/main.rg`
- `src/test/ridgun/test_filter.rg`
- `src/test/ridgun/test_fs.rg`
- `src/test/ridgun/test_md.rg`
- `src/test/ridgun/test_proc.rg`
- `src/test/ridgun/test_sec.rg`

## Verification performed (command → result, newest last)
- `for f in build.rg src/main/ridgun/lib/*.rg src/main/ridgun/*/main.rg src/test/ridgun/*.rg; do ridgunc check --strict "$f"; done` → 29/29 files passed `--strict` with 0 errors.
- `stable format && stable check` → 29/29 files canonically formatted; 23/23 modules checked with 0 errors.
- `stable sync libs && stable strict binaries` → 4 `.rglib` bundles and all 17 native Linux `x86_64` ELF64 binaries (`target/jot` .. `target/sha256`) compiled with `ridgunc build --strict` (0 errors).
- `stable test` → 5/5 test suites passed (`test_filter.rg` 100, `test_md.rg` 32, `test_fs.rg` 50, `test_proc.rg` 63, `test_sec.rg` 38 = 283/283 assertions green).

## Exact next actions
1. Close `TASK-0011` and update `STATE.md`.

## Decisions / open questions
- `/home/boogie/ridgun` and `/home/boogie/ridgun/libs` remain strictly read-only; all `.rglib` bundles are compiled into `.stable/deps/` and `target/` inside `unix-tools`.

## Definition of done checklist
- [x] requirements referenced are `implemented`  - [x] tests automated & green
- [x] security entries updated  - [x] docs INDEX updated  - [x] coverage matrix regenerated
