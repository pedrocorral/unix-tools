---
id: FR-SEC-001
title: OpenBSD doas(1) and doas.conf(5) in Pure Ridgun
status: implemented
priority: could
created: 2026-10-17
updated: 2026-10-18
depends_on: [ADR-0001]
verified_by: [TS-SEC-002]
---
# FR-SEC-001 — OpenBSD `doas(1)` and `doas.conf(5)` in Pure Ridgun

## Statement
The project SHALL provide a pure Ridgun implementation of OpenBSD `doas(1)` (`src/main/ridgun/doas/main.rg`) implementing the full `doas.conf(5)` grammar, the *"last matching rule wins"* policy engine, the `-C config` configuration audit mode, and secure privilege execution.

## Functional Specification

### 1. `doas.conf(5)` Grammar & Policy Engine
- **Rule Syntax**:
  ```text
  permit|deny [nopass] [nolog] [persist] [keepenv] [setenv { [VAR ...] [VAR=val ...] [-VAR ...] }] <identity> [as <target>] [cmd <command> [args ...]]
  ```
  - `<identity>`: Username (e.g. `pedro`) or colon-prefixed group name (e.g. `:wheel`).
  - `as <target>`: Target user (default `root`).
  - `cmd <command>`: Exact command path; if `args ...` is specified, the invoked arguments must match `args ...` exactly (`args` with no tokens means the command may only be run with zero arguments).
- **Evaluation Semantics**:
  - All rules in `doas.conf` are evaluated in order; the **last matching rule** determines whether execution is permitted or denied.
  - Comments start with `#`; blank lines are ignored; syntax errors cause immediate rejection of the configuration file.

### 2. Configuration Audit Mode (`doas -C config [cmd [args ...]]`)
- **With `-C config` only**: Parses and validates `config`; exits `0` if valid, or prints syntax error line numbers and exits non-zero.
- **With `-C config cmd [args ...]`**: Evaluates the rules for the calling user, target user (`-u user`, default `root`), `cmd`, and `args...`, printing:
  - `permit` (when permitted and password required)
  - `permit nopass` (when permitted without password)
  - `deny` (when no rule matches or last matching rule is `deny`)

### 3. Security & Runtime Constraint (No `/bin/sh` for Privileged Execution)
- **CRITICAL SECURITY REQUIREMENT**: Privileged command execution (`Phase B`) MUST NEVER invoke `/bin/sh -c` (`shell_exec`). It MUST use direct OS syscalls exposed by `ridgunc` (`sys_getuid`, `sys_setresgid`, `sys_setresuid`, `sys_execve` with sanitized environment, and `termios` no-echo `/dev/tty` authentication).

## Acceptance Criteria
- AC1: `doas -C doas.conf` validates rule syntax and reports line numbers on malformed rules.
- AC2: `doas -C doas.conf -u root /sbin/reboot` prints `permit`, `permit nopass`, or `deny` according to OpenBSD *"last matching rule wins"* semantics.
