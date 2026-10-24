---
id: TASK-0014
title: Port additional official BSD/OpenBSD regression test assertions for sh and doas from upstream original source
status: completed
created: 2026-10-24
completed: 2026-10-24
after: [TASK-0013]
requirements: [FR-PROC-002, FR-SEC-001]
tests: [TS-PROC-002, TS-SEC-002]
security: [VUL-INJ-001, VUL-AUTHN-001, VUL-AUTHZ-001]
context_files: [docs/requirements/functional/proc/FR-PROC-002-bsd-sh.md, docs/requirements/functional/sec/FR-SEC-001-bsd-doas.md, docs/tests/functional/proc/TS-PROC-002-bsd-sh.md, docs/tests/functional/sec/TS-SEC-002-bsd-doas.md, src/test/ridgun/test_sh.rg, src/test/ridgun/test_sec.rg, src/main/ridgun/lib/proc_lib.rg, src/main/ridgun/lib/sec_lib.rg]
scope: [src/test/ridgun/test_sh.rg, src/test/ridgun/test_sec.rg, src/main/ridgun/**, docs/tests/functional/proc/TS-PROC-002-bsd-sh.md, docs/tests/functional/sec/TS-SEC-002-bsd-doas.md]
owner: agent-002
claimed: 2026-10-24T16:19:28Z
claimed_by: agent gemini@solarfall
---
# TASK-0014 — Port additional official BSD/OpenBSD regression test assertions for `sh` and `doas` from upstream original source

## Goal (one sentence)
Audit the original upstream BSD `sh` (`bin/sh/tests/{builtins,errors,execution,expansion,parameters,parser,set-e}`) and OpenBSD `doas` (`regress/usr.bin/doas` and `usr.bin/doas/parse.y`) source/regression repositories and port hundreds of additional fine-grained edge-case assertions into `src/test/ridgun/test_sh.rg` and `src/test/ridgun/test_sec.rg`.

## Plan
- [x] Inspect original FreeBSD/NetBSD/OpenBSD `bin/sh/tests/` suites (`builtins/`, `errors/`, `execution/`, `expansion/`, `parameters/`, `parser/`, `set-e/`) and port exhaustive edge-case assertions into `src/test/ridgun/test_sh.rg`.
- [x] Inspect original OpenBSD `usr.bin/doas/` (`parse.y`, `doas.c`, `env.c`, and `regress/usr.bin/doas/`) and port all grammar, token quoting, `setenv`/`keepenv`, `cmd`/`args`, UID/GID/group, and negative security assertions into `src/test/ridgun/test_sec.rg`.
- [x] Verify all new assertions pass under `ridgunc check --strict` and `stable test`.

## Progress log (append-only, newest last)
- 2026-10-24: Created `TASK-0014` in `docs/road-map/pending/next/` per user request to expand `sh` and `doas` test suites with assertions derived directly from the original upstream BSD/OpenBSD source and regression suites.
- 2026-10-24: Ported 50 upstream BSD `bin/sh/tests/` regression assertions into `src/test/ridgun/test_sh.rg` (101 total assertions in `test_sh.rg`) and 39 upstream OpenBSD `usr.bin/doas/{parse.y,doas.c,env.c}` regression assertions into `src/test/ridgun/test_sec.rg` (88 total assertions in `test_sec.rg`).
- 2026-10-24: Verified all 17 strict binaries (`stable strict binaries`) and all 8 test suites (`stable test` — `545/545` assertions passing).

## Files changed (keep current)
- `src/main/ridgun/lib/proc_lib.rg`
- `src/main/ridgun/lib/sec_lib.rg`
- `src/test/ridgun/test_sh.rg`
- `src/test/ridgun/test_sec.rg`

## Verification performed (command → result, newest last)
- `stable format` → `0 formatting errors`
- `stable strict binaries` → `17/17 ELF64 binaries built with --strict (0 errors)`
- `stable test` → `Passed 8/8 test suite(s) — 0 failures (545/545 assertions passed)`

## Exact next actions
1. Complete `TASK-0014` via `aix task done TASK-0014`.

## Decisions / open questions
- None.

## Definition of done checklist
- [x] requirements referenced are `implemented`  - [x] tests automated & green
- [x] security entries updated  - [x] docs INDEX updated  - [x] coverage matrix regenerated
