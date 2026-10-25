---
id: FR-SEC-001
title: OpenBSD doas(1) and doas.conf(5) in Pure Ridgun
status: implemented
priority: could
created: 2026-10-17
updated: 2026-10-24
depends_on: [ADR-0001, ADR-0002]
verified_by: [TS-SEC-002]
---
# FR-SEC-001 — OpenBSD `doas(1)` and `doas.conf(5)` in Pure Ridgun

## Statement
The project SHALL provide a pure Ridgun implementation of OpenBSD `doas(1)` (`src/main/ridgun/doas/main.rg` and `src/main/ridgun/lib/sec_lib.rg`) implementing the full `doas.conf(5)` grammar, the *"last matching rule wins"* policy engine, the `-C config` configuration audit mode, cryptographic password verification (`crypto.rglib`), real `persist` session ticket caching and `-L` ticket clearing, audit logging with `nolog` suppression, `PATH` command resolution, and direct `posix execve` privilege execution.

## Functional Specification

### 1. `doas.conf(5)` Grammar & Policy Engine
- **Rule Syntax**:
  ```text
  permit|deny [nopass] [nolog] [persist] [keepenv] [setenv { [VAR ...] [VAR=val ...] [-VAR ...] [$VAR ...] }] <identity> [as <target>] [cmd <command> [args ...]]
  ```
- **Evaluation Semantics**:
  - All rules in `doas.conf` are evaluated in order; the **last matching rule** determines whether execution is permitted or denied.
  - Comments start with `#`; blank lines and backslash-continued lines (`\` + `\n`) are supported; syntax errors cause immediate rejection of the configuration file with exact 1-based line numbers.

### 2. CLI Flags & Modes (`doas [-Lns] [-C config] [-u user] [cmd [args ...]]`)
- **`-C config` (Configuration Audit Mode)**:
  - With `-C config` only: Parses and validates `config`; exits `0` if valid, or prints `doas.conf:<line>: syntax error: <reason>` and exits `1`.
  - With `-C config [-u user] cmd [args ...]`: Prints `permit`, `permit nopass`, or `deny`.
- **`-n` (Non-Interactive Mode)**:
  - Fails immediately (`doas: Authentication required`) if the matching rule requires a password and no valid `persist` ticket is active.
- **`-s` (Shell Mode)**:
  - Executes the shell from `SHELL` or `/bin/sh` when `-s` is passed instead of an explicit command.
- **`persist` Session Ticket & `-L` (Clear Persist Token)**:
  - When a matching `permit` rule includes `persist` and password authentication succeeds, `doas` writes a timestamped session ticket (`/tmp/.doas_persist_<uid>`) valid for 300 seconds. Subsequent `doas` invocations matching a `persist` rule within 300 seconds succeed without password re-entry (even under `-n`).
  - `doas -L` unlinks `/tmp/.doas_persist_<uid>` immediately so subsequent calls require password authentication again.
- **Audit Logging & `nolog` Suppression**:
  - When a permitted command is executed and the matching rule does NOT specify `nolog`, `doas` writes an OpenBSD-formatted audit record (`doas: <caller> ran command <cmd> <args> as <target> from <cwd>`) to the audit log (`DOAS_LOG_FILE`). When `nolog` is present, no audit entry is written.
- **`PATH` Resolution**:
  - If `<cmd>` does not contain `/`, `doas` resolves `<cmd>` across the sanitized `PATH` before invoking `posix execve`.

## Acceptance Criteria
- AC1: `doas -C doas.conf` validates rule syntax (including quoted arguments, line continuations, and `setenv` blocks) and reports exact line numbers on malformed rules.
- AC2: `doas -C doas.conf -u root /sbin/reboot` prints `permit`, `permit nopass`, or `deny` according to OpenBSD *"last matching rule wins"* semantics, and `doas` enforces `-n` non-interactive rejection, cryptographic password verification, real `persist` ticket caching and `-L` ticket removal, audit log recording vs `nolog` suppression, `PATH` command resolution, caller identity anti-spoofing in live mode, and environment sanitization (`LD_PRELOAD` stripping).
