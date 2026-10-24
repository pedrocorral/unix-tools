---
id: TASK-0014
title: Port additional official BSD/OpenBSD regression test assertions for sh and doas from upstream original source
status: going-on
created: 2026-10-24
completed:
after: [TASK-0013]
requirements: [FR-PROC-002, FR-SEC-001]
tests: [TS-PROC-002, TS-SEC-002]
security: [VUL-INJ-001, VUL-AUTHN-001, VUL-AUTHZ-001]
context_files: [docs/requirements/functional/proc/FR-PROC-002-bsd-sh.md, docs/requirements/functional/sec/FR-SEC-001-bsd-doas.md, docs/tests/functional/proc/TS-PROC-002-bsd-sh.md, docs/tests/functional/sec/TS-SEC-002-bsd-doas.md, src/test/ridgun/test_sh.rg, src/test/ridgun/test_sec.rg]
scope: [src/test/ridgun/test_sh.rg, src/test/ridgun/test_sec.rg, docs/tests/functional/proc/TS-PROC-002-bsd-sh.md, docs/tests/functional/sec/TS-SEC-002-bsd-doas.md]
owner: agent-002
claimed: 2026-10-24T16:19:28Z
claimed_by: agent gemini@solarfall
---
# TASK-0014 — Port additional official BSD/OpenBSD regression test assertions for `sh` and `doas` from upstream original source

## Goal (one sentence)
Audit the original upstream BSD `sh` (`bin/sh/tests/{builtins,errors,execution,expansion,parameters,parser,set-e}`) and OpenBSD `doas` (`regress/usr.bin/doas` and `usr.bin/doas/parse.y`) source/regression repositories and port hundreds of additional fine-grained edge-case assertions into `src/test/ridgun/test_sh.rg` and `src/test/ridgun/test_sec.rg`.

## Plan
- [ ] Inspect original FreeBSD/NetBSD/OpenBSD `bin/sh/tests/` suites (`builtins/`, `errors/`, `execution/`, `expansion/`, `parameters/`, `parser/`, `set-e/`) and port exhaustive edge-case assertions into `src/test/ridgun/test_sh.rg`.
- [ ] Inspect original OpenBSD `usr.bin/doas/` (`parse.y`, `doas.c`, `env.c`, and `regress/usr.bin/doas/`) and port all grammar, token quoting, `setenv`/`keepenv`, `cmd`/`args`, UID/GID/group, and negative security assertions into `src/test/ridgun/test_sec.rg`.
- [ ] Verify all new assertions pass under `ridgunc check --strict` and `stable test`.

## Progress log (append-only, newest last)
- 2026-10-24: Created `TASK-0014` in `docs/road-map/pending/next/` per user request to expand `sh` and `doas` test suites with assertions derived directly from the original upstream BSD/OpenBSD source and regression suites.

## Files changed (keep current)
- `docs/road-map/pending/next/TASK-0014-port-additional-official-bsd-openbsd-regression-te.md`

## Verification performed (command → result, newest last)
- Pending

## Exact next actions
1. Start `TASK-0014` via `aix task start TASK-0014` once `TASK-0013` completes.
2. Port upstream BSD `sh` and OpenBSD `doas` regression test cases into `test_sh.rg` and `test_sec.rg`.

## Decisions / open questions
- None.

## Definition of done checklist
- [ ] requirements referenced are `implemented`  - [ ] tests automated & green
- [ ] security entries updated  - [ ] docs INDEX updated  - [ ] coverage matrix regenerated
