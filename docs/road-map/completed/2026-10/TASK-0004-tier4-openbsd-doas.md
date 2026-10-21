---
id: TASK-0004
title: Implement Tier 4 OpenBSD doas & doas.conf (-C audit + direct POSIX execution)
status: completed
priority: medium
created: 2026-10-17
updated: 2026-10-17
implements: [FR-SEC-001]
tests: []
mitigates: []
depends_on: [TASK-0003]
scope: [src/main/ridgun, src/test/ridgun]
context_files:
  - docs/requirements/decisions/ADR-0001-adopt-aix-and-choose-stack.md
  - docs/requirements/functional/sec/FR-SEC-001-bsd-doas.md
owner: agent-001
claimed: 2026-10-18T14:10:40Z
claimed_by: agent gemini@solarfall
---
# TASK-0004 — Implement Tier 4 OpenBSD `doas` & `doas.conf` (`-C` Audit + Direct POSIX Execution)

## Goal
Implement OpenBSD `doas(1)` and `doas.conf(5)` in 100% pure Ridgun (`@implements FR-SEC-001`).

## Steps
1. Phase A: Implement the `doas.conf(5)` lexer/parser, *"last matching rule wins"* policy evaluator, environment sanitizer (`keepenv` / `setenv`), and `doas -C <config> [cmd args...]` mode in `src/main/ridgun/doas/main.rg`.
2. Phase B: Add direct POSIX syscalls (`sys_getuid`, `sys_setresgid`, `sys_setresuid`, `sys_execve`, `termios` TTY prompt) in `/home/boogie/ridgun` (never using `/bin/sh`) and complete privileged execution and security audit (`docs/security/`).
